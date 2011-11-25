%%%----------------------------------------------------------------------
%%% File    : nextim_app.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : NextIM startup
%%% Created : Jul 20 2010
%%% License : http://www.webim20.cn
%%%
%%% Copyright www.webim20.cn 2010. 
%%%----------------------------------------------------------------------
-module(nextim_app).

-author('ery.lee@gmail.com').

-include("elog.hrl").

-behavior(application).

-export([start/2, stop/1]).

start(normal, _Args) ->
	{ok, Sup} = nextim_sup:start_link(),
    {ok, HttpdOpts} = application:get_env(httpd),
	{ok, MeterLogger} = application:get_env(meter_logger),
	lists:foreach(
        fun({Name, F}) when is_function(F) ->
			io:format("~n~s: ~s is starting...", [node(), Name]),
            F(),
			io:format("[done]~n");
		   ({Name, Server}) when is_atom(Server) ->
			io:format("~n~s: ~s is starting...", [node(), Name]),
			start_child(Sup, Server),
			io:format("[done]~n");
           ({Name, Server, Opts}) when is_atom(Server) ->
			io:format("~n~s: ~s is starting...", [node(), Name]),
			start_child(Sup, Server, Opts),
			io:format("[done]~n")
		end,
	 	[{"nextim client supervisor", fun() -> 
            Name = nextim_client_sup,
            supervisor:start_child(Sup, 
                {Name, {Name, start_link, []}, 
                    permanent, 10, supervisor, [Name]})
         end},
         {"nextim router", nextim_router},
		 {"nextim roster", nextim_roster},
		 {"nextim rooms", nextim_rooms},
		 {"nextim meter", nextim_meter, MeterLogger},
		 {"nextim httpd", nextim_httpd, HttpdOpts}
		]),
	{ok, Sup}.	

stop(_) ->
	ok.

%%Internal functions
start_child(Sup, Name) ->
    {ok, _ChiId} = supervisor:start_child(Sup, worker_spec(Name)).
start_child(Sup, Name, Opts) ->
    {ok, _ChiId} = supervisor:start_child(Sup, worker_spec(Name, Opts)).

worker_spec(Name) ->
    {Name, {Name, start_link, []}, 
        permanent, 10, worker, [Name]}.
worker_spec(Name, Opts) ->
    {Name, {Name, start_link, [Opts]}, 
        permanent, 10, worker, [Name]}.

