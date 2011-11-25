%%%----------------------------------------------------------------------
%%% File    : nextim_sup.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : nextim supervisor.
%%% Created : 19 Jan. 2009
%%% Updated : 01 Sept. 2009
%%% License : http://www.webim20.com/
%%%
%%% Copyright www.webim20.cn 2010 
%%%----------------------------------------------------------------------
-module(nextim_sup).

-author('ery.lee@gmail.com').

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, nextim_sup}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 10, 10}, []}}.

