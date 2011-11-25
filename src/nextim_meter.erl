%%%----------------------------------------------------------------------
%%% File    : nextim_meter.erl
%%% Author  : Feng Lee <feng.lee@webim20.cn>
%%% Purpose : bind,unbind,message,status meter 
%%% Created : 18 Feb 2008
%%% Updated : 01 Sept. 2009
%%% License : http://www.webim20.cn
%%%
%%% Copyright www.webim20.cn 2010
%%%----------------------------------------------------------------------
-module(nextim_meter).

-author('ery.lee@gmail.com').

-include("elog.hrl").

-include("nextim.hrl").

-export([start_link/1, 
		 stop/0,
		 incr/2, 
		 decr/2]).

-behavior(gen_server).

-export([init/1,
		handle_call/3,
		handle_cast/2,
		handle_info/2,
		terminate/2,
		code_change/3]).

-record(state, {fd, file}).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Opts], []).

stop() ->
	gen_server:call(?MODULE, stop).

incr(Type, Domain) ->
	gen_server:cast(?MODULE, {incr, Type, Domain}).

decr(Type, Domain) ->
	gen_server:cast(?MODULE, {decr, Type, Domain}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Opts]) ->
	File = proplists:get_value(file, Opts),
	case file:open(File, [append, raw]) of
    {ok, Fd} ->
		mnesia:create_table(domain_meter,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, domain_meter)}]),
		mnesia:add_table_copy(domain_meter, node(), ram_copies),
		mnesia:dirty_write(#domain_meter{domain = <<"global">>}),
		erlang:send_after(300 * 1000, self(), write_meter),
        {ok, #state{fd = Fd, file = File}};
    Error ->
        Error
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
	{stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unexpected_req}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({incr, Type, Domain}, State) ->
	update_meter(Type, Domain, 1),
    {noreply, State};

handle_cast({decr, Type, Domain}, State) ->
	update_meter(Type, Domain, -1),
    {noreply, State};
	
handle_cast(Msg, State) ->
	?ERROR("Unexpected message: ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(write_meter, #state{fd = Fd} = State) ->
    spawn(fun() -> 
        write_meter(Fd, strnow(), mnesia:sync_dirty(fun() -> 
            mnesia:all_keys(domain_meter)
        end))
    end),
	erlang:send_after(30*60*1000, self(), write_meter),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{fd = Fd} = _State) ->
	file:close(Fd),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
update_meter(Type, Domain, Count) ->
	[Global] = mnesia:dirty_read({domain_meter, <<"global">>}),
	mnesia:dirty_write(new_meter(Type, Count, Global)),
	case mnesia:dirty_read({domain_meter, Domain}) of
	[] ->
		mnesia:dirty_write(#domain_meter{domain = Domain});
	[Meter] ->
		mnesia:sync_dirty(new_meter(Type, Count, Meter))
	end.

new_meter(route, Count, Meter) ->
	Meter#domain_meter{counter = Meter#domain_meter.counter + Count};

new_meter(bind, Count, Meter) ->
	Meter#domain_meter{binds = Meter#domain_meter.binds + Count};

new_meter(unbind, Count, Meter) ->
	Meter#domain_meter{unbinds = Meter#domain_meter.unbinds + Count};

new_meter(message, Count, Meter) ->
	Meter#domain_meter{messages = Meter#domain_meter.messages + Count};

new_meter(status, Count, Meter) ->
	Meter#domain_meter{statuses = Meter#domain_meter.statuses + Count};

new_meter(Unknown, _Count, Meter) ->
	?WARNING("unknown meter: ~p ", [Unknown]),
	Meter.

write_meter(_Fd, _Time, []) ->
	ok;

write_meter(Fd, Time, [Domain|T]) ->
	case mnesia:dirty_read({domain_meter, Domain}) of
	[] -> 
		pass;
	[Meter] -> 
       file:write(Fd, io_lib:format("~s(~s): ~p~n", [binary_to_list(Domain), Time, Meter#domain_meter.counter]))
	end,
	write_meter(Fd, Time, T).

strnow() ->
	{{Y, M, D}, {H, MM, S}} = erlang:localtime(),
	Date = string:join([integer_to_list(I) || I <- [Y, M, D]], "-"),
	Time = string:join([integer_to_list(I) || I <- [H, MM, S]], ":"),
	Date ++ " " ++ Time.
