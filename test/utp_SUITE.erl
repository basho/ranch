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

-module(utp_SUITE).
-author("Greg Burd <greg@basho.com>").

-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]). %% ct.
-export([utp_echo/1]). %% utp.

%% ct.

all() ->
        [{group, utp}].

groups() ->
        Tests = [utp_echo],
        [{utp, Tests}].

init_per_suite(Config) ->
        application:start(ranch),
        Config.

end_per_suite(_) ->
        application:stop(ranch),
        ok.

%% utp.

utp_echo(_) ->
        %% @todo Don't use a fixed port. start_listener should return the port used?
        {ok, _} = ranch:start_listener(utp_echo, 1,
                ranch_utp, [{port, 33333}],
                echo_protocol, []),
        {ok, Socket} = gen_utp:connect("localhost", 33333, []),
        ok = gen_utp:send(Socket, <<"Ranch uTP is working!">>),
        {ok, <<"Ranch uTP is working!">>} = gen_utp:recv(Socket, 0, 1000),
        ok = ranch:stop_listener(utp_echo),
        {error, closed} = gen_utp:recv(Socket, 0, 1000),
        ok.
