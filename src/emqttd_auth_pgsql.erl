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

%% @doc Authentication with PostgreSQL 'mqtt_users' table
-module(emqttd_auth_pgsql).

-behaviour(emqttd_auth_mod).

-include("../../../include/emqttd.hrl").

-export([init/1, check/3, description/0]).

-record(state, {super_query, auth_query, hash_type}).

-define(UNDEFINED(S), (S =:= undefined orelse S =:= <<>>)).

init({SuperQuery, AuthQuery, HashType}) ->
    io:format("~p~n", [SuperQuery]),
    {ok, #state{super_query = SuperQuery, auth_query = AuthQuery, hash_type = HashType}}.

check(#mqtt_client{username = Username}, _Password, _State) when ?UNDEFINED(Username) ->
    {error, username_undefined};

check(Client, Password, #state{super_query = SuperQuery}) when ?UNDEFINED(Password) ->
    case emqttd_plugin_pgsql:is_superuser(SuperQuery, Client) of
        true  -> ok;
        false -> {error, password_undefined}
    end;

check(Client, Password, #state{super_query = SuperQuery,
                               auth_query  = {AuthSql, AuthParams},
                               hash_type   = HashType}) ->
    case emqttd_plugin_pgsql:is_superuser(SuperQuery, Client) of
        false -> case emqttd_plugin_pgsql:equery(AuthSql, AuthParams, Client) of
                    {ok, _, [Record]} ->
                        check_pass(Record, Password, HashType);
                    {ok, _, []} ->
                        {error, not_found};
                    {error, Error} ->
                        {error, Error}
                 end;
        true  -> ok
    end.

check_pass({PassHash}, Password, HashType) ->
    case PassHash =:= hash(HashType, Password) of
        true  -> ok;
        false -> {error, password_error}
    end;
check_pass({PassHash, Salt}, Password, {salt, HashType}) ->
    case PassHash =:= hash(HashType, <<Salt/binary, Password/binary>>) of
        true  -> ok;
        false -> {error, password_error}
    end;
check_pass({PassHash, Salt}, Password, {HashType, salt}) ->
    case PassHash =:= hash(HashType, <<Password/binary, Salt/binary>>) of
        true  -> ok;
        false -> {error, password_error}
    end.

hash(Type, Password) ->
    emqttd_auth_mod:passwd_hash(Type, Password).

description() -> "Authentication with PostgreSQL".

