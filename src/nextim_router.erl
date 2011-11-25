%%%----------------------------------------------------------------------
%%% File    : nextim_router.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : message and presence router.
%%% Created : 12 Apil 2008
%%% Updated : 01 Sept. 2009
%%% License : http://www.webim20.cn/
%%%
%%% Copyright www.webim20.cn 2010
%%%----------------------------------------------------------------------
-module(nextim_router).

-author('ery.lee@gmail.com').

-include("elog.hrl").

-include("nextim.hrl").

-export([start_link/0, 
		stop/0, 
		get_route/1, 
		get_routes/1, 
        update_route/1,
		register_route/1, 
		unregister_route/1, 
        broadcast/3,
        route/3]).

-behaviour(gen_server).

%Callbacks of gen_server
-export([init/1, 
		handle_call/3, 
		handle_cast/2, 
		terminate/2, 
		handle_info/2, 
		code_change/3]).

start_link() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call(?MODULE, stop).

get_route(Jid) ->
	mnesia:dirty_read({route, Jid}).

get_routes(Jids) ->
	lists:flatten([nextim_router:get_route(Jid) || Jid <- Jids]).

update_route(Route) ->
    gen_server:call(?MODULE, {update_route, Route}).

register_route(Route) ->
	gen_server:call(?MODULE, {register_route, Route}).

unregister_route(Jid) ->
	gen_server:call(?MODULE, {unregister_route, Jid}).

broadcast(From, Domain, Message) ->
	Routes = mnesia:dirty_index_read(route, Domain, #route.domain),
	lists:foreach(fun(Route) -> 
        Message1 = Message#message{to = Route#route.jid},
		route(From, Route#route.jid, Message1)
	end, Routes).

route(From, To, Packet) ->
    gen_server:cast(?MODULE, {route, From, To, Packet}).

init([]) ->
	process_flag(trap_exit, true),
	mnesia:create_table(route,
		[{ram_copies, [node()]},
		 {attributes, record_info(fields, route)},
		 {index, [mon, domain]}]),
	mnesia:add_table_copy(route, node(), ram_copies),
    {ok, state}.

handle_call({update_route, Route}, _From, State) ->
	case get_route(Route#route.jid) of
    [_] ->
        mnesia:dirty_write(Route);
    [] ->
        ok
    end,
    {reply, ok, State};

handle_call({register_route, Route}, _From, State) ->
    Mon = erlang:monitor(process, Route#route.pid),
    mnesia:dirty_write(Route#route{mon = Mon}),
    nextim_meter:incr(route, Route#route.domain),
    {reply, ok, State};

handle_call({unregister_route, Jid}, _From, State) ->
	case get_route(Jid) of
    [#route{mon = Mon, domain = Domain}] -> 
        demonitor(Mon),
        mnesia:dirty_delete({route, Jid}),
        nextim_meter:decr(route, Domain);
    [] -> 
        ok 
	end,
	{reply, ok, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(Req, _From, State) ->
    ?ERROR("unexpected request: ~p", [Req]),
	{reply, {error, unexpected_request}, State}.

handle_cast({route, From, To, Packet}, State) ->
	?INFO("~p -> ~p : ~n~p", [jid:to_list(From), jid:to_list(To), Packet]),
    [Route#route.pid ! {packet, Packet} || Route <- get_route(To)],
	{noreply, State};

handle_cast(Msg, State) ->
    ?ERROR("unexpected msg: ~p", [Msg]),
    {noreply, State}.

handle_info({'DOWN', Mon, _Type, _Object, _Info}, State) ->
    [begin 
        mnesia:dirty_delete(route, Route#route.jid),
        nextim_meter:decr(route, Route#route.domain)
     end || Route <- mnesia:dirty_index_read(route, Mon, #route.mon)],
    {noreply, State};

handle_info(Info, State) ->
    ?ERROR("unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _Channels) ->
    ok.
    
%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

demonitor(undefined) ->
    ok;
demonitor(Mon) ->
    erlang:demonitor(Mon).

