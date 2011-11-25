%%%----------------------------------------------------------------------
%%% File    : nextim_client.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : client: maybe a buddy, service or channel
%%% Created : 20 Jan. 2009
%%% Updated : 01 Sept. 2009
%%% License : http://www.webim20.cn

%%% Copyright www.webim20.cn 2010
%%%----------------------------------------------------------------------
-module(nextim_client).

-author('ery.lee@gmail.com').

-include("elog.hrl").

-include("nextim.hrl").

-import(erlang, [send_after/3]).

-export([start/1,
        start_link/1,
        info/1,
        update/2,
        bind/2, 
		unbind/2, 
		subscribe/3, 
		unsubscribe/3, 
		message/4, 
		status/4, 
		presence/2]).

-behavior(gen_server).

-export([init/1, 
		handle_call/3, 
		handle_cast/2, 
		handle_info/2, 
		terminate/2, 
		code_change/3]).

-record(state, {jid, client, ref, subscribers = [], buddies = []}).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start({Client, Buddies, Rooms}) ->
    supervisor:start_child(nextim_client_sup, [{Client, Buddies, Rooms}]).

start_link({Client, Buddies, Rooms}) ->
    gen_server:start_link(?MODULE, [{Client, Buddies, Rooms}], []).

info(CPid) ->
    gen_server:call(CPid, info).

update(CPid, Attrs) ->
    gen_server:call(CPid, {update, Attrs}).

% @spec bind(CPid, Sid) -> ok
%%  CPid = pid()
%%  Sid = iolist()
%% @doc bind a session to this client, a session is a browser tab or window.
bind(CPid, Sid) ->
	gen_server:call(CPid, {bind, Sid}).
    
subscribe(CPid, Sid, SPid) ->
	gen_server:cast(CPid, {subscribe, Sid, SPid}).
    
unsubscribe(CPid, Sid, SPid) ->
	gen_server:call(CPid, {unsubscribe, Sid, SPid}).

%% @spec unbind(CPid, Sid) -> ok
%%  CPid = pid()
%%  Sid = iolist()
%% @doc a session unbind this client, a browser tab or window is closed.
unbind(CPid, Sid) ->
	gen_server:call(CPid, {unbind, Sid}).

status(CPid, Sid, To, Status) when is_record(Status, status) ->
	gen_server:cast(CPid, {status, Sid, To, Status}).

message(CPid, Sid, To, Message) when is_record(Message, message) ->
	gen_server:cast(CPid, {message, Sid, To, Message}).

presence(CPid, Presence) when is_record(Presence, presence) ->
	gen_server:cast(CPid, {presence, Presence}).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([{#client{jid = Jid} = Client, Buddies, Rooms}]) ->
    process_flag(trap_exit, true),
	add_buddies(Client, Buddies),
	add_rooms(Client, Rooms),
	register_route(Client),
	send_available(Client),
	Ref = send_after(?IDLE_TIMEOUT, self(), idle_timeout),
    {ok, #state{jid = Jid, client = Client, ref = Ref}}.

add_buddies(#client{jid = Jid}, Buddies) ->
    Onlines = [J || #route{jid = J} <- nextim_router:get_routes(Buddies)],
	nextim_roster:add(Jid, Onlines).

add_rooms(#client{jid = Jid, nick = Nick}, Rooms) ->
	nextim_rooms:register(Jid, Nick, Rooms).

register_route(#client{jid = Jid, domain = Domain}) ->
	Route = #route{jid = Jid, pid = self(), domain = Domain},
    nextim_router:register_route(Route).

send_available(#client{jid = Jid, nick = Nick, show = Show, status = Status}) ->
    Presence = #presence{type = <<"online">>, nick = Nick, 
        from = Jid, show = Show, status = Status},
	presence(self(), Presence).

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(info, _From, #state{client = Client} = State) ->
    {reply, {ok, Client}, State};

handle_call({update, Attrs}, _From, #state{client = Client} = State) ->
    #client{nick=N, show=Sh, status=S} = Client,
    Nick = proplists:get_value(nick, Attrs, N),
    Show = proplists:get_value(show, Attrs, Sh),
    Status = proplists:get_value(status, Attrs, S),
    Client1 = Client#client{nick=Nick, show=Show, status=Status},
    {reply, ok, State#state{client = Client1}};

handle_call({bind, Sid}, _From, #state{ref = IdleTimer, subscribers = Subscribers} = State) ->
	?INFO("bind: ~p", [Sid]),
	cancel_timer(IdleTimer),
	undefined = get(Sid),
	Ref = send_after(?IDLE_TIMEOUT, self(), {idle_timeout, Sid}),
	Sub = #subscriber{sid = Sid, ref = Ref},
	put(Sid, Sub),
    {reply, ok, State#state{ref = undefined, subscribers = [Sid|Subscribers]}};

handle_call({unbind, Sid}, _From, #state{subscribers = Subscribers} = State) ->
	?INFO("unbind: ~p", [Sid]),
	NewState = 
	case get(Sid) of
    Sub when is_record(Sub, subscriber) ->
        demonitor(Sub#subscriber.mon),
		cancel_timer(Sub#subscriber.ref),
		case Sub#subscriber.spid of
		undefined -> 
			ok;
		SPid ->
			case is_process_alive(SPid) of
			true -> SPid ! stop;
			false -> ok
			end
		end,
		erase(Sid),
		Subscribers1 = lists:delete(Sid, Subscribers),
		case length(Subscribers1) of
		0 ->
			Ref = send_after(?IDLE_TIMEOUT, self(), idle_timeout),
			State#state{ref = Ref, subscribers = Subscribers1};
		_ ->
			State#state{subscribers = Subscribers1}	
		end;
	undefined ->
        ?WARNING("unbind sid is not existed: ~p", [Sid]),
		State
	end,
	{reply, ok, NewState};

handle_call({unsubscribe, Sid, SPid}, _From, State) ->
    case get(Sid) of
    Sub when is_record(Sub, subscriber)->
		if
		(Sub#subscriber.spid == undefined) or (Sub#subscriber.spid == SPid) ->
			?INFO("unsubscribe ~p ~p", [Sid, SPid]),
            demonitor(Sub#subscriber.mon),
			cancel_timer(Sub#subscriber.ref),
			Ref = send_after(?IDLE_TIMEOUT, self(), {idle_timeout, Sid}),
            NewSub = Sub#subscriber{spid = undefined, ref = Ref, mon = undefined},
			put(Sid, NewSub);
        true ->
            ?ERROR("~p cannot not unsubscribe ~p", [SPid, Sub#subscriber.spid])
        end;
    undefined ->
		?ERROR("unsubscribed sid is not existed: ~p", [Sid])
    end,
	{reply, ok, State};

handle_call(Req, _From, State) ->
	?ERROR("unexpected request: ~p", [Req]),
    {reply, {error, unexpected_req}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({subscribe, Sid, SPid}, State) ->
	case get(Sid) of
	Sub when is_record(Sub, subscriber) ->
		?INFO("subscribe ~p, ~p", [Sid, SPid]),
		cancel_timer(Sub#subscriber.ref),
        demonitor(Sub#subscriber.mon),
		case Sub#subscriber.spid of
		undefined -> 
			ok;
        SPid ->
            ?ERROR("assert failure: subscribed by the same pid:~p", [SPid]);
		OldSPid ->
			case is_process_alive(OldSPid) of
			true -> OldSPid ! stop;
			false -> ok
			end
		end,
		Packets = Sub#subscriber.packets,
		case length(Packets) of
		0 -> 
			Mon = erlang:monitor(process, SPid),
			NewSub = Sub#subscriber{spid = SPid, ref = undefined, mon = Mon},
			put(Sid, NewSub);
		_ -> 
			NewSub = Sub#subscriber{spid = undefined, ref = undefined, mon = undefined, packets = []},
			put(Sid, NewSub),
			SPid ! {ok, Packets}
		end;
	undefined ->
		?WARNING("illegal subscriber: ~p", [Sid]),
		SPid ! stop
	end,
	{noreply, State};

handle_cast({status, _Sid, To, Status}, #state{jid = Jid} = State) ->
	case nextim_router:get_route(To) of
	[_Route] ->
		nextim_router:route(Jid, To, Status#status{from = Jid});
	[] ->
		ok
	end,
	{noreply, State};

handle_cast({message, FromSid, To, Message}, #state{jid = Jid, 
    subscribers = Subscribers} = State) ->
    %% users ---- pid
    %% sync all browsers of sender
	Message1 = Message#message{from = Jid},
	lists:foreach(fun(Sid) -> 
		case FromSid == Sid of
		true ->
			pass;
		false ->
			case get(Sid) of
			Sub when is_record(Sub, subscriber)->
				Packets = Sub#subscriber.packets,
				case Sub#subscriber.spid of
					undefined ->
						put(Sid, Sub#subscriber{packets = [Message1|Packets]});
					Pid ->
						demonitor(Sub#subscriber.mon),
						Pid ! {ok, [Message1|Packets]},
						put(Sid, Sub#subscriber{spid=undefined, mon=undefined, packets=[]})
				end;
			undefined ->
				?ERROR("no subscriber found: ~p", [Sid])
			end
		end
	end, Subscribers),
	%%------------------------------------------------------------------------------------
    %% Send Message 
    %% nextim_router:route -> nextim_endpoint:dispatch =========
    %%                                                     ||
    %%                                                     \/
    %% ReceiverClient <---LongPoll---> nextim_jsonp  <- nextim_client:handle_info(packet)
	%%------------------------------------------------------------------------------------
    case Message1#message.type of
    <<"unicast">> -> %recver is single
        nextim_router:route(Jid, To, Message1);
    <<"multicast">> ->
        nextim_rooms:route(Jid, To, Message1);
    <<"broadcast">> ->
        nextim_router:broadcast(Jid, To, Message1);
    Other ->
        ?ERROR("unsupported message type: ~p", [Other])
    end,
	{noreply, State};

handle_cast({presence, Presence}, #state{jid = Jid} = State) ->
	[nextim_router:route(Jid, Buddy#buddy.fid, Presence) 
        || Buddy <- nextim_roster:buddies(Jid)],
	{noreply, State};

handle_cast(Msg, State) ->
	?ERROR("unexpected message: ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({packet, Packet}, #state{subscribers = Subscribers} = State) ->
	dispatch(Packet, Subscribers),
    {noreply, State};

handle_info({'DOWN', Mon, _Type, _Object, _Info}, #state{subscribers = Subscribers} = State) ->
	NewState = 
	case find_subscriber(Subscribers, Mon) of
	{ok, #subscriber{sid = Sid}} ->
		?INFO("down: ~p, ~p",[Sid, Mon]),
		erase(Sid),
		Subscribers1 = lists:delete(Sid, Subscribers),
		case length(Subscribers1) of
		0 ->
			Ref = send_after(?IDLE_TIMEOUT, self(), idle_timeout),
			State#state{ref = Ref, subscribers = Subscribers1};
		_ ->
			State#state{subscribers = Subscribers1}	
		end;
	false ->
		?ERROR("cannot find down session: ~p", [Mon]),
		State
	end,
	{noreply, NewState};

handle_info({idle_timeout, Sid}, #state{subscribers = Subscribers} = State) ->
	?INFO("idle_timeout: ~p", [Sid]),
    case get(Sid) of
    undefined ->
        ?ERROR("assert failure: cannot idle_timeout sid: ~p", [Sid]);
    Sub ->
        %debug
        if
        Sub#subscriber.mon =/= undefined ->
            ?ERROR("assert failure, subscriber mon: ~p", [Sub#subscriber.mon]);
        true ->
            ok
        end,
        %debug
        if
        Sub#subscriber.spid =/= undefined ->
            ?ERROR("assert failure, subscriber spid: ~p", [Sub#subscriber.spid]);
        true ->
            ok
        end
    end,
	erase(Sid),
	Subscribers1 = lists:delete(Sid, Subscribers),
	NewState = 
	case length(Subscribers1) of
	0 ->
		Ref = send_after(?IDLE_TIMEOUT, self(), idle_timeout),
		State#state{ref = Ref, subscribers = Subscribers1};
	_ ->
		State#state{subscribers = Subscribers1}	
	end,
	{noreply, NewState};

handle_info(idle_timeout, #state{subscribers = Subscribers} = State) ->
	case length(Subscribers) of
	0 -> ok;
	I -> ?ERROR("idle_timeout when ~p subscribers left", [I])
	end,
	{stop, normal, State};

handle_info(stop, State) ->
	?INFO("client received stop info", []),
	{stop, normal, State};

handle_info(Info, State) ->
	?ERROR("unexpected info: ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{jid = Jid, client = Client}) ->
	Presence = #presence{type = <<"offline">>, 
						 from = Jid, 
						 nick = Client#client.nick, 
					     show = <<"unavailable">>, 
						 status = Client#client.status},
	[nextim_router:route(Jid, Buddy#buddy.fid, Presence) || Buddy <- nextim_roster:buddies(Jid)],
	nextim_roster:remove(Jid),
	nextim_rooms:unregister(Jid),
    nextim_router:unregister_route(Jid),
    ?INFO("client is terminated: ~p",[Jid]),
    ok.
%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
	
dispatch(Packet, Subscribers) ->
    %% send Message to all the browsers of the receiver.
    %% nextim_endpoint:dispatch -> nextim_client:handle_info(packet)
    %%                                              ||
    %%                                              ||
    %% ReceiverClient <---LongPoll---> nextim_jsonp <--
	lists:foreach(fun(Sid) -> 
		case get(Sid) of
		Sub when is_record(Sub, subscriber) ->
			Packets = Sub#subscriber.packets,
			case Sub#subscriber.spid of
			undefined ->
				put(Sid, Sub#subscriber{packets = [Packet|Packets]});
			Pid ->
				demonitor(Sub#subscriber.mon),
				Pid ! {ok, [Packet|Packets]},
				put(Sid, Sub#subscriber{spid=undefined, mon=undefined, packets=[]})
			end;
		undefined ->
			?ERROR("undefined subscriber in dict: ~p", [Sid])
		end
	end, Subscribers).

find_subscriber([], _Mon) ->
	false;

find_subscriber([Sid|Subscribers], Mon) ->
	#subscriber{mon = M} = Sub = get(Sid),
	if 
	M == Mon -> {ok, Sub};
	true -> find_subscriber(Subscribers, Mon)
	end.

demonitor(undefined) ->
    ok;

demonitor(Mon) ->
    erlang:demonitor(Mon).

cancel_timer(undefined) ->
	ok;

cancel_timer(Ref) ->
	erlang:cancel_timer(Ref).
	
