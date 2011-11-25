%%%----------------------------------------------------------------------
%%% File    : nextim_httpd.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : nextim http post deamon.
%%% Created : 12 Apil 2008
%%% Updated : 17 Sept. 2010
%%% License : http://www.webim20.cn/
%%%
%%% Copyright www.webim20.cn 2010
%%%----------------------------------------------------------------------
-module(nextim_httpd).

-author('ery.lee@gmail.com').

-include("elog.hrl").

-include("nextim.hrl").

-import(extbif, [atom_to_binary/1, 
        binary_to_atom/1,
        binary_split/2]).

-export([start_link/1, 
		loop/2,
		stop/0]).

-define(AUTH_FILTERS, [{'POST', "/presences/online"},
        {'POST', "/presences/offline"},
        {'POST', "/presences/show"},
        {'POST', "/statuses"},
        {'POST', "/messages"},
        {'POST', "/room/join"},
        {'POST', "/room/leave"},
        {'GET', "/room/members"},
        {'GET', "/onlines"}]).

%% External API
start_link(Options) ->
    {ok, Auth} = application:get_env(webauth),
    Domain = list_to_binary(proplists:get_value(domain, Auth)),
    ApiKey = list_to_binary(proplists:get_value(apikey, Auth)),
    Loop = fun(Req) -> loop(Req, {Domain, ApiKey}) end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, Auth) ->
    case Req:get(body_length) of
    Length when is_integer(Length) and (Length > 40000) -> 
        ?ERROR("reqest length: ~p", [Length]),
        Req:respond({400, [], <<"body is too long.">>});
    _Length ->
        do_loop(Req, Auth)
    end.

do_loop(Req, Auth) ->
    Method = Req:get(method),
    Params =
	case Method of
    'GET' ->
        [{list_to_atom(K), list_to_binary(V)} || {K, V} <- Req:parse_qs()];
    'POST' -> 
        [{list_to_atom(K), list_to_binary(V)} || {K, V} <- Req:parse_post()];
     Method -> 
        []
	end,
    case auth(Auth, Method, Req:get(path), Params) of
    true ->
        Path = list_to_tuple(string:tokens(Req:get(path), "/")),
        handle(Req, Method, Path, Params);
    false ->
        Req:respond({401, [], <<"authentication failed.">>})
    end.

auth({Domain, ApiKey}, Method, Path, Params) ->
    case lists:member({Method, Path}, ?AUTH_FILTERS) of
    true -> %%simple auth
        {value, Domain0} = getparam(domain, Params),
        {value, ApiKey0} = getparam(apikey, Params),
        (Domain == Domain0) and (ApiKey == ApiKey0); 
    false -> %%not need to auth
        true
    end.

handle(Req, 'POST', {"presences", "online"}, Params) ->
    {value, Nick} = getparam(nick, Params),
    {value, Name} = getparam(name, Params),
    {value, Domain} = getparam(domain, Params),
    {value, Vsn} = getparam(version, Params, <<"">>),
    {value, Rooms} = getparam(rooms, Params, <<"">>),
    {value, Buddies} = getparam(buddies, Params, <<"">>),
    %%added in vsn 3.2
    {value, Show} = getparam(show, Params, <<"available">>),
    {value, Status} = getparam(status, Params, <<"">>),
    Jid = jid:make(Name, Domain),
    Rooms1 = [jid:make(Room, Domain) || Room <- binary_split(Rooms, $,)],
    Buddies1 = [jid:make(Buddy, Domain) || Buddy <- binary_split(Buddies, $,)],
    {ok, CPid} =
    case nextim_router:get_route(Jid) of
    [] ->
        Client = #client{jid = Jid, name = Name, nick = Nick, 
            domain = Domain, show = binary_to_atom(Show), status = Status},
        nextim_client:start({Client, Buddies1, Rooms1});
    [Route] ->
        {ok, Route#route.pid}
    end,
    Sid = session_id(),
    nextim_client:bind(CPid, Sid),
    nextim_meter:incr(bind, Domain), 
    Ticket = list_to_binary([Sid, "|", Name]),
    Onlines = onlines(nextim_router:get_routes(Buddies1)),
    Response =
    if 
    Vsn == <<"3">> ->
        Onlines1 = [ [{name, jid:node(J)}, {nick, N}, {show, Sh}, {status, Sts}] 
            || #client{jid = J, nick = N, show = Sh, status = Sts} <- Onlines],
        Totals = [ [{name, jid:node(Room)}, {total, nextim_rooms:total(Room)}] 
            || Room <- Rooms1],
        json([{ticket, Ticket}, {rooms, Totals}, {buddies, Onlines1}]);
    true ->
        Onlines1 = [{jid:node(J), P} || #client{jid = J, show = P} <- Onlines],
        Totals = [{jid:node(Room), nextim_rooms:total(Room)} || Room <- Rooms1],
        json([{ticket, Ticket}, {roominfo, Totals}, {buddies, Onlines1}])
    end,
    Req:ok({"text/plain", Response});

handle(Req, 'POST', {"presences", "offline"}, Params) ->
    {value, Ticket} = getparam(ticket, Params),
    {value, Domain} = getparam(domain, Params),  
    case session(Ticket) of
    {Sid, Name, _} ->                                            
        case nextim_router:get_route(jid:make(Name, Domain)) of
        [Route] ->
            nextim_client:unbind(Route#route.pid, Sid),
            nextim_meter:incr(unbind, Domain),    
            Req:ok({"text/plain", <<"ok">>});
        [] ->
            Req:respond({404, [], <<"no client found.">>})
        end;
    _ -> 
        Req:respond({404, [], <<"invalid ticket.">>})
    end;

handle(Req, 'POST', {"presences", "show"}, Params) ->                                  
    {value, Ticket} = getparam(ticket, Params),
    {value, Domain} = getparam(domain, Params),
    case session(Ticket) of
    {_Sid, Name, _} ->
        Jid = jid:make(Name, Domain),
        case nextim_router:get_route(Jid) of
        [Route] ->
            {value, Nick} = getparam(nick, Params),
            {value, Domain} = getparam(domain, Params),
            {value, Show} = getparam(show, Params, <<"available">>),
            {value, Status} = getparam(status, Params, <<"">>),
            nextim_client:update(Route#route.pid, [{show, Show},{status, Status}]),
            Presence = #presence{type = <<"show">>,
                                 from = Jid,
                                 nick = Nick,
                                 show = Show,
                                 status = Status},
            nextim_client:presence(Route#route.pid, Presence),
            Req:ok({"text/plain", <<"ok">>});
        [] ->
            Req:respond({404, [], <<"no client found.">>})
        end;
    _ ->
        Req:respond({404, [], <<"invalid ticket.">>})
    end;

handle(Req, 'POST', {"statuses"}, Params) ->
    {value, Ticket} = getparam(ticket, Params),
    {value, Domain} = getparam(domain, Params),
    case session(Ticket) of 
    {Sid, Name, _} ->                       
        Jid = jid:make(Name, Domain),
        case nextim_router:get_route(Jid) of
        [Route] ->
            {value, To} = getparam(to, Params),
            {value, Nick} = getparam(nick, Params),
            {value, Show} = getparam(show, Params),
            ToJid  = jid:make(To, Domain),
            Status = #status{from = Jid,
                             nick = Nick,
                             to = ToJid,
                             show = Show},
            nextim_client:status(Route#route.pid, Sid, ToJid, Status),
            nextim_meter:incr(status, Domain),
            Req:ok({"text/plain", <<"ok">>});
        [] ->
            Req:respond({404, [], <<"no client found.">>})
        end;
    _ -> 
        Req:respond({404, [], <<"invalid ticket.">>})
    end;

handle(Req, 'POST', {"messages"}, Params) ->
    {value, Ticket} = getparam(ticket, Params),
    {value, Domain} = getparam(domain, Params),
    case session(Ticket) of 
    {Sid, Name, _} ->                       
        case nextim_router:get_route(jid:make(Name, Domain)) of
        [Route] ->
            {value, To} = getparam(to, Params),
            {value, Body} = getparam(body, Params),
            {value, Nick} = getparam(nick, Params, <<"">>), 
            {value, Style} = getparam(style, Params, <<"">>), 
            {value, Timestamp} = getparam(timestamp, Params),
            {value, Type} = getparam(type, Params, <<"unicast">>),
            ToJid = jid:make(To, Domain),             
            Message = #message{nick=Nick,
                               to=ToJid,
                               type = Type,
                               body=Body,
                               style=Style,
                               timestamp=Timestamp},
            nextim_client:message(Route#route.pid, Sid, ToJid, Message),
            nextim_meter:incr(message, Domain),
            Req:ok({"text/plain", <<"ok">>});
        [] ->
            Req:respond({404, [], <<"no client found.">>})
        end;
    _ -> 
        Req:respond({404, [], <<"invalid ticket.">>})
    end;

handle(Req, 'GET', {"onlines"}, Params) ->
    {value, Domain} = getparam(domain, Params),
    {value, Ticket} = getparam(ticket, Params),
    {value, Vsn} = getparam(version, Params, <<"">>),
    case session(Ticket) of
    {_Sid, Name, _} ->
        case nextim_router:get_route(jid:make(Name, Domain)) of
        [_Route] ->
            {value, Names} = getparam(names, Params),
            Jids = [jid:make(N, Domain) || N <- binary_split(Names, $,)],
            Onlines = onlines(nextim_router:get_routes(Jids)),
            Response = 
            if
            Vsn == <<"3">> ->
                json([[{name, jid:node(J)}, {nick, N}, {show, Show}, {status, Status}] 
                    || #client{jid = J, nick = N, show = Show, status = Status} <- Onlines]);
            true ->
                json([{jid:node(J), Show} || #client{jid = J, show = Show} <- Onlines])
            end,
            Req:ok({"text/plain", Response});
        [] ->
            Req:respond({404, [], <<"no client found.">>})
        end;
    _ ->
        Req:respond({404, [], <<"invalid ticket.">>})
    end;

handle(Req, 'POST', {"room", "join"}, Params) ->
    {value, Domain} = getparam(domain, Params),
    {value, Ticket} = getparam(ticket, Params),
    case session(Ticket) of
    {_Sid, Name, _} ->
        case nextim_router:get_route(jid:make(Name, Domain)) of
        [#route{jid = Jid}] ->
            {value, Room} = getparam(room, Params),
            {value, Nick} = getparam(nick, Params),
            Rid = jid:make(Room, Domain),
            nextim_rooms:join(Jid, Nick, Rid),
            Members = nextim_rooms:members(Rid),
            Presence = #presence{type = <<"join">>, from = Jid, nick = Nick, show = <<"available">>, status = Room},
            [nextim_router:route(Jid, To, Presence) || #in_room{jid = To} <- Members],
            Req:ok({"text/plain", json([{Room, length(Members)}])});
        [] ->
            Req:respond({404, [], <<"no client found.">>})
        end;
    _ ->
        Req:respond({404, [], <<"invalid ticket.">>})
    end;

handle(Req, 'POST', {"room", "leave"}, Params) ->
    ?INFO("leave: ~p", [Params]),
    {value, Domain} = getparam(domain, Params),
    {value, Ticket} = getparam(ticket, Params),
    case session(Ticket) of
    {_Sid, Name, _} ->
        case nextim_router:get_route(jid:make(Name, Domain)) of
        [#route{jid = Jid}] ->
            {value, Room} = getparam(room, Params),
            {value, Nick} = getparam(nick, Params),
            Rid = jid:make(Room, Domain),
            nextim_rooms:leave(Jid, Rid),
            Members = nextim_rooms:members(Rid),
            Presence = #presence{type = <<"leave">>, from = Jid, nick = Nick, show = <<"available">>, status = Room},
            [nextim_router:route(Jid, To, Presence) || #in_room{jid = To} <- Members],
            Req:ok({"text/plain", "ok"});
        [] ->
            Req:respond({404, [], <<"no client found.">>})
        end;
    _ ->
        Req:respond({404, [], <<"invalid ticket.">>})
    end;

handle(Req, 'GET', {"room", "members"}, Params) ->
    {value,Ticket} = getparam(ticket, Params),
    {value,Domain} = getparam(domain, Params),
    case session(Ticket) of 
    {_Sid, Name, _} ->                       
        case nextim_router:get_route(jid:make(Name, Domain)) of
        [_Route] ->
            {value, Room} = getparam(room, Params),
            Members = nextim_rooms:members(jid:make(Room, Domain)),
            Members1 = [ [{id, jid:node(Jid)}, {nick, Nick}] || 
                       #in_room{jid = Jid, nick = Nick} <- Members],
            Req:ok({"text/plain", json([{Room, Members1}])});
        [] ->
            Req:respond({404, [], <<"no client found.">>})
        end;
    _ -> 
        Req:respond({404, [], <<"invalid ticket.">>})
    end;

handle(Req, 'GET', {"packets"}, Params) ->
    {value, Domain} = getparam(domain, Params),
    {value, Ticket} = getparam(ticket, Params),
    {value, Callback} = getparam(callback, Params, <<"">>), 
    {Sid, Name, _} = session(Ticket),
	case nextim_router:get_route(jid:make(Name, Domain)) of
	[Route] ->
		nextim_client:subscribe(Route#route.pid, Sid, self()),
		Result = 
		receive 
			{ok, Packets} -> 
				{ok, Packets};
			stop ->
				stop
		after
			?POLL_TIMEOUT -> 
				{ok, []}
		end,
		case Result of
		{ok, Packets1} ->
            case is_process_alive(Route#route.pid) of
            true ->
                nextim_client:unsubscribe(Route#route.pid, Sid, self());
            false ->
                ok
            end,
			JSON = pack(lists:reverse(Packets1)),
            case Callback of
            <<>> ->
                Req:ok({"application/javascript", JSON});
            Callback ->
                Req:ok({"application/javascript", list_to_binary([Callback, "(", JSON, ")"])})
            end;
		stop ->
			Req:ok({"application/javascript", list_to_binary([Callback, <<"({\"status\": \"stopped\"})">>])}),
			erlang:exit(normal)
		end;
	[] ->
		Req:respond({404, [], <<"no client found.">>})
	end;

handle(Req, Method, Path, Params) ->
    ?ERROR("unexpected http request from ~p: ~p ~p: ~p", 
        [Req:get(peer), Method, Path, Params]),
    Req:respond({404, [], <<"fuck out.">>}).

onlines(Routes) ->
    onlines(Routes, []).

onlines([], Acc) ->
    Acc;
onlines([#route{pid = Pid} | Routes], Acc) ->
    case catch nextim_client:info(Pid) of
    {ok, Client} -> onlines(Routes, [Client|Acc]);
    _ -> onlines(Routes, Acc)
    end.

pack(Packets) ->
    pack(Packets, [], [], []).

pack([], MsgAcc, PresAcc, StatAcc) ->
    json([{status, <<"ok">>}, 
         {messages, lists:reverse(MsgAcc)}, 
         {presences, lists:reverse(PresAcc)}, 
         {statuses, lists:reverse(StatAcc)}]);

pack([#message{from=FromJid, nick=Nick, to=ToJid, timestamp=Timestamp, 
    type=Type ,body=Body, style=Style} | T], MsgAcc, PresAcc, StatusAcc) ->
    if
    (FromJid == undefined) or (ToJid == undefined) ->
        ?ERROR("error message: from = ~p, to = ~p", [FromJid, ToJid]);
    true ->
        ok
    end,
    MsgObj = [{from, jid:node(FromJid)}, {nick, Nick}, 
              {to, jid:node(ToJid)}, {timestamp, Timestamp}, 
              {type, Type}, {body, Body}, {style, Style}],
    pack(T, [MsgObj|MsgAcc], PresAcc, StatusAcc);

pack([#presence{type=Type, from=FromJid, nick=Nick, show=Show, 
    status=Status} | T], MsgAcc, PresAcc, StatusAcc) ->
    if
    (FromJid == undefined) ->
        ?ERROR("error presence: nick = ~p", [Nick]);
    true ->
        ok
    end,
    PresObj = [{from, jid:node(FromJid)}, {nick, Nick},
               {type, Type}, {show, Show}, {status, Status}],
    pack(T, MsgAcc, [PresObj|PresAcc], StatusAcc);

pack([#status{from= FromJid, to=ToJid, nick = Nick, show=Show} | T], 
    MsgAcc, PresAcc, StatusAcc) ->
    if
    (FromJid == undefined) or (ToJid == undefined) ->
        ?ERROR("error message: from = ~p, to = ~p", [FromJid, ToJid]);
    true ->
        ok
    end,
    StatusObj = [{from, jid:node(FromJid)}, {nick, Nick},
                 {to, jid:node(ToJid)}, {show, Show}],
    pack(T, MsgAcc, PresAcc, [StatusObj|StatusAcc]).

getparam(Key, Opts) ->
	dataset:get_value(Key, Opts).

getparam(Key, Opts, Def) ->
	dataset:get_value(Key, Opts, Def).

session_id() ->
	random:seed(now()),
    I1 = random:uniform(round(math:pow(2, 48))) - 1, 
	I2 = random:uniform(round(math:pow(2, 32))) - 1, 
	L = lists:flatten(io_lib:format("~12.16.0b~8.16.0b", [I1, I2])),
	list_to_binary(L).

session(Ticket) ->
	case binary_split(Ticket, $|) of
    [Sid, Name] -> {Sid, Name, []};
    [Sid, Name | PropsBin] ->
        Props = 
        [begin 
            case binary_split(Prop, $=) of
            [N, V] -> {binary_to_atom(N), V};
            [N] -> {binary_to_atom(N), true}
            end
        end || Prop <- PropsBin],
        {Sid, Name, Props}
    end.

json(Term) ->
    Encoder = mochijson2:encoder([]),
    list_to_binary(Encoder(Term)).
