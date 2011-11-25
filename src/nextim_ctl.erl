%%%----------------------------------------------------------------------
%%% File    : nextim_ctl.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : NextIM Server Controller.
%%% Created : 1 May. 2009
%%% Updated : 01 Sept. 2009
%%% License : http://www.webim20.cn
%%%
%%% Copyright www.webim20.cn 2010
%%%----------------------------------------------------------------------
-module(nextim_ctl).

-author('ery.lee@gmail.com').

-include("elog.hrl").

-export([start/0, init/0, process/1]).  

-define(STATUS_SUCCESS, 0).

-define(STATUS_ERROR,   1).

-define(STATUS_USAGE,   2).

-define(STATUS_BADRPC,  3).

start() ->
    case init:get_plain_arguments() of
	[SNode | Args] ->
	    SNode1 = case string:tokens(SNode, "@") of
		[_Node, _Server] ->
		    SNode;
		_ ->
		    case net_kernel:longnames() of
			 true ->
			     SNode ++ "@" ++ inet_db:gethostname() ++
				      "." ++ inet_db:res_option(domain);
			 false ->
			     SNode ++ "@" ++ inet_db:gethostname();
			 _ ->
			     SNode
		     end
	    end,
	    Node = list_to_atom(SNode1),
	    Status = case rpc:call(Node, ?MODULE, process, [Args]) of
			 {badrpc, Reason} ->
			     ?PRINT("RPC failed on the node ~p: ~p~n",
				       [Node, Reason]),
			     ?STATUS_BADRPC;
			 S ->
			     S
		     end,
	    halt(Status);
	_ ->
        ?PRINT("~p~n", [init:get_plain_arguments()]),
	    halt(?STATUS_USAGE)
    end.

node_name(SNode) ->
    SNode1 = 
    case string:tokens(SNode, "@") of
    [_Node, _Server] ->
        SNode;
    _ ->
        case net_kernel:longnames() of
         true ->
             SNode ++ "@" ++ inet_db:gethostname() ++
                  "." ++ inet_db:res_option(domain);
         false ->
             SNode ++ "@" ++ inet_db:gethostname();
         _ ->
             SNode
         end
    end,
    list_to_atom(SNode1).

init() ->
	ok.

process(["status" | _Args]) ->
    {InternalStatus, ProvidedStatus} = init:get_status(),
    ?PRINT("Status: ~p.~nNode ~p is ~p.~n",
              [ProvidedStatus, node(), InternalStatus]),
    %?PRINT("Running applications: ~p ~n", [application:which_applications()]),
    ?STATUS_SUCCESS;

process(["cluster", SNode]) ->
    case nextim:cluster(node_name(SNode)) of
    {error, Reason}-> 
        ?PRINT("failed to cluster with ~p, reason: ~p", [SNode, Reason]),
        ?STATUS_ERROR;
    {ok, Nodes} ->
        ?PRINT("cluster success, nodes: ~p~n", [Nodes]),
        ?STATUS_SUCCESS
    end;

process(["stop" | _Args]) ->
    init:stop(),
    ?PRINT("stopped.~n", []),
    ?STATUS_SUCCESS;

process(["restart" | _Args]) ->
    init:restart(),
    ?STATUS_SUCCESS;

process(["mnesia"| Arg]) when is_list(Arg) ->
    ?PRINT("~p ~n", [mnesia:info()]), 
    case catch mnesia:system_info(list_to_atom(Arg)) of
	{'EXIT', Error} -> ?PRINT("Error: ~p~n", [Error]);
	Return -> ?PRINT("~p~n", [Return])
    end,
    ?STATUS_SUCCESS.

