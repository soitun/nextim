%%%----------------------------------------------------------------------
%%% File    : jid.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : replace exmpp_jid
%%% Created : Jul 20 2010
%%% License : http://www.webim20.cn
%%%
%%% Copyright (C) 2007-2010, www.webim20.cn
%%%----------------------------------------------------------------------
-module(jid).

-include("nextim.hrl").

-export([node/1,
         make/2,
         to_list/1,
         to_binary/1]).

node(Jid) when is_record(Jid, jid) -> 
    Jid#jid.node.

make(Node, Domain) when is_binary(Node) and is_binary(Domain) -> 
    #jid{node = Node, domain = Domain}.

to_list(Jid) when is_record(Jid, jid) ->
    binary_to_list(to_binary(Jid)).

to_binary(#jid{node = undefined, domain = Domain}) ->
    Domain;
to_binary(#jid{node = Node, domain = Domain, resource = undefined}) ->
    <<Node/binary, "@", Domain/binary>>;
to_binary(#jid{node = Node, domain = Domain, resource = Res}) ->
    <<Node/binary, "@", Domain/binary, "/", Res/binary>>.
