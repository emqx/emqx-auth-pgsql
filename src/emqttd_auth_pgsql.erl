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

-module(emqttd_auth_pgsql).

-behaviour(emqttd_auth_mod).

-import(proplists, [get_value/2]).

-include("emqttd_auth_pgsql.hrl").

-include_lib("emqttd/include/emqttd.hrl").

-export([init/1, check/3, description/0]).

-behaviour(ecpool_worker).

-export([parse_query/1, connect/1, squery/1, equery/2, equery/3]).

-record(state, {auth_query, super_query, hash_type}).

-define(UNDEFINED(S), (S =:= undefined orelse S =:= <<>>)).

%%--------------------------------------------------------------------
%% Auth Module Callbacks
%%--------------------------------------------------------------------

init({AuthQuery, SuperQuery, HashType}) ->
    {ok, #state{auth_query = AuthQuery, super_query = SuperQuery, hash_type = HashType}}.

check(#mqtt_client{username = Username}, Password, _State) when ?UNDEFINED(Username); ?UNDEFINED(Password) ->
    {error, username_or_password_undefined};

check(Client, Password, #state{auth_query  = {AuthSql, AuthParams},
                               super_query = SuperQuery,
                               hash_type   = HashType}) ->
    Result = case equery(AuthSql, AuthParams, Client) of
                 {ok, _, [Record]} ->
                     check_pass(Record, Password, HashType);
                 {ok, _, []} ->
                     {error, not_found};
                 {error, Reason} ->
                     {error, Reason}
             end,
    case Result of ok -> {ok, is_superuser(SuperQuery, Client)}; Error -> Error end.

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

%%--------------------------------------------------------------------
%% Is Superuser?
%%--------------------------------------------------------------------

-spec(is_superuser(undefined | {string(), list()}, mqtt_client()) -> boolean()).
is_superuser(undefined, _Client) ->
    false;
is_superuser({SuperSql, Params}, Client) ->
    case equery(SuperSql, Params, Client) of
        {ok, [_Super], [{true}]} ->
            true;
        {ok, [_Super], [_False]} ->
            false;
        {ok, [_Super], []} ->
            false;
        {error, _Error} ->
            false
    end.

%%--------------------------------------------------------------------
%% Avoid SQL Injection: Parse SQL to Parameter Query.
%%--------------------------------------------------------------------

parse_query(undefined) ->
    undefined;
parse_query(Sql) ->
    case re:run(Sql, "'%[uca]'", [global, {capture, all, list}]) of
        {match, Variables} ->
            Params = [Var || [Var] <- Variables],
            {pgvar(Sql, Params), Params};
        nomatch ->
            {Sql, []}
    end.

pgvar(Sql, Params) ->
    Vars = ["$" ++ integer_to_list(I) || I <- lists:seq(1, length(Params))],
    lists:foldl(fun({Param, Var}, S) ->
            re:replace(S, Param, Var, [global, {return, list}])
        end, Sql, lists:zip(Params, Vars)).

%%--------------------------------------------------------------------
%% PostgreSQL Connect/Query
%%--------------------------------------------------------------------

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
    ecpool:with_client(?APP, fun(C) -> epgsql:squery(C, Sql) end).

equery(Sql, Params) ->
    ecpool:with_client(?APP, fun(C) -> epgsql:equery(C, Sql, Params) end).

equery(Sql, Params, Client) ->
    ecpool:with_client(?APP, fun(C) -> epgsql:equery(C, Sql, replvar(Params, Client)) end).

replvar(Params, Client) ->
    replvar(Params, Client, []).

replvar([], _Client, Acc) ->
    lists:reverse(Acc);
replvar(["'%u'" | Params], Client = #mqtt_client{username = Username}, Acc) ->
    replvar(Params, Client, [Username | Acc]);
replvar(["'%c'" | Params], Client = #mqtt_client{client_id = ClientId}, Acc) ->
    replvar(Params, Client, [ClientId | Acc]);
replvar(["'%a'" | Params], Client = #mqtt_client{peername = {IpAddr, _}}, Acc) ->
    replvar(Params, Client, [inet_parse:ntoa(IpAddr) | Acc]);
replvar([Param | Params], Client, Acc) ->
    replvar(Params, Client, [Param | Acc]).

