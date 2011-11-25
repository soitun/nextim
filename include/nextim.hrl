%%%----------------------------------------------------------------------
%%% File    : nextim.hrl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : nextim header file
%%% Created : 28 Jan 2009 by Ery Lee
%%% License : http://www.webim20.cn/license
%%%
%%% Copyright  www.webim20.cn 2010
%%%----------------------------------------------------------------------
-define(VERSION, "3.2").

-define(IDLE_TIMEOUT, 8000). 

-define(POLL_TIMEOUT, 28000).

-record(jid, {node, domain, resource}).

-record(subscriber, {sid, spid, ref, mon, packets=[]}).

%show: ['available', 'away', 'chat', 'dnd', 'invisible']
-record(client, {jid, name, nick, domain, show = available, status = <<"">>}).

-record(buddy, {jid, fid}).

-record(room, {rid, total = 0}).

-record(in_room, {jid, nick, rid}).

-record(route, {jid, pid, mon, domain}).

-record(site, {domain, apikey, status = pending, max_users = 0, expired = 0}).

-record(domain_meter, {domain, counter=0, binds= 0, unbinds= 0, messages = 0, statuses = 0}).

-record(status, {from, nick, to, show}).

-record(presence, {type, from, to, nick, show, status}).

-record(message, {from, nick, to, timestamp, type = <<"unicast">>, body = <<"">>, style = <<"">>}).
