%%--------------------------------------------------------------------
%% Copyright (c) 2015-2016 Feng Lee <feng@emqtt.io>.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

%% @doc emqttd pgsql pool client
-module(emqttd_pgsql_client).

-behaviour(ecpool_worker).

-import(proplists, [get_value/2]).

-export([connect/1, squery/1, equery/2]).

-define(POOL, pgsql_pool).

connect(Opts) ->
    Host     = get_value(host, Opts),
    Username = get_value(username, Opts),
    Password = get_value(password, Opts),
    epgsql:connect(Host, Username, Password, conn_opts(Opts)).

conn_opts(Opts) ->
    conn_opts(Opts, []).
conn_opts([], Acc) ->
    Acc;
conn_opts([Opt = {database, _}|Opts], Acc) ->
    conn_opts(Opts, [Opt|Acc]);
conn_opts([Opt = {ssl, _}|Opts], Acc) ->
    conn_opts(Opts, [Opt|Acc]);
conn_opts([Opt = {port, _}|Opts], Acc) ->
    conn_opts(Opts, [Opt|Acc]);
conn_opts([Opt = {timeout, _}|Opts], Acc) ->
    conn_opts(Opts, [Opt|Acc]);
conn_opts([_Opt|Opts], Acc) ->
    conn_opts(Opts, Acc).

squery(Sql) ->
    ecpool:with_client(?POOL, fun(C) -> epgsql:squery(C, Sql) end).

equery(Sql, Params) ->
    ecpool:with_client(?POOL, fun(C) -> epgsql:equery(C, Sql, Params) end).

