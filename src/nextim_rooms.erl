%%%----------------------------------------------------------------------
%%% File    : nextim_rooms.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : rooms for group chat
%%% Created : 20 Jan. 2009
%%% Updated : 01 Sept. 2009
%%% License : http://www.webim20.cn
%%% Copyright www.webim20.cn 2010 
%%%----------------------------------------------------------------------
-module(nextim_rooms).

-author('ery.lee@gmail.com').

-include("elog.hrl").

-include("nextim.hrl").

-export([start_link/0]).

-export([total/1,
		members/1, 
		register/3, 
		unregister/1, 
        join/3,
        leave/2,
		route/3]).

-behavior(gen_server).

-export([init/1, 
		handle_call/3, 
		handle_cast/2, 
		handle_info/2, 
		terminate/2, 
		code_change/3]).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

total(Rid) ->
	case mnesia:dirty_read(room, Rid) of
	[] -> 0;
	[#room{total = Total}] -> Total
	end.

members(Rid) ->
    mnesia:dirty_index_read(in_room, Rid, #in_room.rid).

register(Jid, Nick, Rids) ->
    gen_server:cast(?MODULE, {register, Jid, Nick, Rids}).

unregister(Jid) ->
    gen_server:cast(?MODULE, {unregister, Jid}).

join(Jid, Nick, Rid) ->
    gen_server:cast(?MODULE, {join, Jid, Nick, Rid}).

leave(Jid, Rid) ->
    gen_server:cast(?MODULE, {leave, Jid, Rid}).

route(From, To, Message) ->
    [nextim_router:route(From, R#in_room.jid, Message) || 
        R <- members(To), R#in_room.jid =/= From].

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
init([]) ->
    mnesia:create_table(room,
        [{ram_copies, [node()]}, 
         {attributes, record_info(fields, room)}]),
    mnesia:add_table_copy(room, node(), ram_copies),
    mnesia:create_table(in_room,
        [{ram_copies, [node()]}, 
         {type, bag}, 
         {index, [rid]},
         {attributes, record_info(fields, in_room)}]),
    mnesia:add_table_copy(in_room, node(), ram_copies),
    {ok, state}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(Req, _From, State) ->
	?ERROR("unexpected request: ~p", [Req]),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({register, Jid, Nick, Rids}, State) ->
	mnesia:sync_dirty(fun() -> 
        mnesia:delete({in_room, Jid}),
        [mnesia:write(#in_room{jid = Jid, nick=Nick, rid = Rid}) || Rid <- Rids], 
		[begin
			case mnesia:read(room, Rid, sticky_write) of
			[] -> 
				mnesia:write(#room{rid = Rid, total = 1});
			[#room{total = Total} = R] ->
				mnesia:write(R#room{total = Total + 1})
			end 
		 end || Rid <- Rids]
	end),
	{noreply, State};

handle_cast({unregister, Jid}, State) ->
    InRooms = mnesia:dirty_read({in_room, Jid}),
	mnesia:sync_dirty(fun() -> 
		[begin
			case mnesia:read(room, Rid, sticky_write) of
			[] -> 
				ok;
			[#room{total = Total} = R] ->
				mnesia:write(R#room{total = Total - 1})
			end 
		 end || #in_room{rid = Rid} <- InRooms]
	end),
    mnesia:dirty_delete({in_room, Jid}),
	{noreply, State};

handle_cast({join, Jid, Nick, Rid}, State) ->
	mnesia:sync_dirty(fun() -> 
        mnesia:write(#in_room{jid = Jid, nick = Nick, rid = Rid}), 
        Members = mnesia:dirty_index_read(in_room, Rid, #in_room.rid),
        mnesia:write(#room{rid = Rid, total = length(Members)})
	end),
	{noreply, State};

handle_cast({leave, Jid, Rid}, State) ->
	mnesia:sync_dirty(fun() -> 
        InRooms = mnesia:read({in_room, Jid}),
        case find_inroom(Rid, InRooms) of
        {ok, InRoom} ->
            mnesia:delete_object(InRoom),
            case mnesia:read(room, Rid, sticky_write) of
            [#room{total = Total} = R] ->
                mnesia:write(R#room{total = Total - 1});
            [] -> 
                ok
            end;
        false ->
            ok
        end
	end),
    {noreply, State};

handle_cast(Msg, State) ->
	?ERROR("unexpected message: ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages 
%%%-------------------------------------------------------------------- handle_info(Info, State) ->
handle_info(Info,State) ->
    ?ERROR("unexpected info: ~p", [Info]),
    {noreply, State}.
%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

find_inroom(_Rid, []) ->
    false;
find_inroom(Rid, [#in_room{rid = Rid} = InRoom | _])  ->
    {ok, InRoom};
find_inroom(Rid, [_H|InRooms]) ->
    find_inroom(Rid, InRooms).
    
