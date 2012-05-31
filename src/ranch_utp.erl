%% Copyright (c) 2012 Basho Technologies, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License, Version 2.0 (the
%% "License"); you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at:
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
%% License for the specific language governing permissions and limitations
%% under the License.

%% @doc uTP transport API.
%%
%% Wrapper around <em>gen_utp</em> implementing the Ranch transport API.
%%
%% @see gen_utp
-module(ranch_utp).
-author("Greg Burd <greg@basho.com>").

-export([name/0, messages/0, listen/1, accept/2, recv/3, send/2, setopts/2,
        controlling_process/2, peername/1, close/1, sockname/1]).

%% @doc Name of this transport API, <em>utp</em>.
-spec name() -> utp.
name() -> utp.

%% @doc Atoms used in the process messages sent by this API.
%%
%% They identify incoming data, closed connection and errors when receiving
%% data in active mode.
-spec messages() -> {utp, utp_closed, utp_error}.
messages() -> {utp, utp_closed, utp_error}.

%% @doc Setup a socket to listen on the given port on the local host.
%%
%% The available options are:
%% <dl>
%%  <dt>port</dt><dd>Mandatory. UTP port number to open.</dd>
%%  <dt>backlog</dt><dd>Maximum length of the pending connections queue.
%%   Defaults to 1024.</dd>
%%  <dt>ip</dt><dd>Interface to listen on. Listen on all interfaces
%%   by default.</dd>
%% </dl>
%%
%% @see gen_utp:listen/2
-spec listen([{port, inet:port_number()} | {ip, inet:ip_address()}])
        -> {ok, inet:socket()} | {error, atom()}.
listen(Opts) ->
        {port, Port} = lists:keyfind(port, 1, Opts),
        Backlog = proplists:get_value(backlog, Opts, 1024),
        ListenOpts0 = [binary, {active, false},
                {backlog, Backlog}, {packet, raw}, {reuseaddr, true}],
        ListenOpts =
                case lists:keyfind(ip, 1, Opts) of
                        false -> ListenOpts0;
                        Ip -> [Ip|ListenOpts0]
                end,
        gen_utp:start_link(Port), % TODO: supervised?
        gen_utp:listen(ListenOpts). % TODO: why isn't this (Port, ListenOpts)?

%% @doc Accept an incoming connection on a listen socket.
%% @see gen_utp:accept/2
-spec accept(inet:socket(), timeout())
        -> {ok, inet:socket()} | {error, closed | timeout | atom()}.
accept(LSocket, Timeout) ->
        gen_utp:accept(LSocket, Timeout).

%% @doc Receive a packet from a socket in passive mode.
%% @see gen_utp:recv/3
-spec recv(inet:socket(), non_neg_integer(), timeout())
        -> {ok, any()} | {error, closed | atom()}.
recv(Socket, Length, Timeout) ->
        gen_utp:recv(Socket, Length, Timeout).

%% @doc Send a packet on a socket.
%% @see gen_utp:send/2
-spec send(inet:socket(), iolist()) -> ok | {error, atom()}.
send(Socket, Packet) ->
        gen_utp:send(Socket, Packet).

%% @doc Set one or more options for a socket.
%% @see inet:setopts/2
-spec setopts(inet:socket(), list()) -> ok | {error, atom()}.
setopts(Socket, Opts) ->
        inet:setopts(Socket, Opts).

%% @doc Assign a new controlling process <em>Pid</em> to <em>Socket</em>.
%% @see gen_utp:controlling_process/2
-spec controlling_process(inet:socket(), pid())
        -> ok | {error, closed | not_owner | atom()}.
controlling_process(Socket, Pid) ->
        gen_utp:controlling_process(Socket, Pid).

%% @doc Return the address and port for the other end of a connection.
%% @see inet:peername/1
-spec peername(inet:socket())
        -> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
peername(Socket) ->
        inet:peername(Socket).

%% @doc Close a UTP socket.
%% @see gen_utp:close/1
-spec close(inet:socket()) -> ok.
close(Socket) ->
        gen_utp:close(Socket).

%% @doc Get the local address and port of a socket
%% @see inet:sockname/1
-spec sockname(inet:socket())
        -> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
sockname(Socket) ->
        inet:sockname(Socket).
