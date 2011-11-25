%%%----------------------------------------------------------------------
%%% File    : nextim_client_sup.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : NextIM client supervisor
%%% Created : Arg 30 2010
%%% Updated : 01 Sept. 2009
%%% License : http://www.webim20.cn
%%%
%%% Copyright www.webim20.cn 2010. 
%%%----------------------------------------------------------------------
-module(nextim_client_sup).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 3, 1},
          [{undefined, {nextim_client, start_link, []},
              transient, 10, worker, [nextim_client]}]}}.

