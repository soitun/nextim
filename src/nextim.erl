%%%----------------------------------------------------------------------
%%% File    : nextim.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : NextIM startup
%%% Created : Jul 20 2010
%%% Updated : 01 Sept. 2009
%%% License : http://www.webim20.cn
%%%
%%% Copyright www.webim20.cn 2010. 
%%%----------------------------------------------------------------------
-module(nextim).

-author('ery.lee@gmail.com').

-export([start/0, cluster/1, stop/0]).

start() ->
    [start(App) || App <- [sasl, crypto, mnesia, elog, nextim]],
	io:format("~nfinished.~n").

cluster(Node) ->
    case net_adm:ping(Node) of
    pong -> {ok, nodes()};
    pang -> {error, "ping node failure."}
    end.
    
stop() ->
    [stop(App) || App <- [nextim, mnesia, crypto, sasl]].

start(elog) ->
    {ok, [[LogPath]]} = init:get_argument(log_path),
    {ok, [[LogLevel]]} = init:get_argument(log_level),
    elog:init(list_to_integer(LogLevel), LogPath);

start(mnesia) ->
    case mnesia:system_info(extra_db_nodes) of
    [] ->
        mnesia:delete_schema([node()]),
        mnesia:create_schema([node()]);
    _ ->
		ok
    end,
	mnesia:start(),
    mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity);

start(App) ->
    application:start(App).

stop(mnesia) ->
	mnesia:stop();

stop(App) ->
    application:stop(App).

