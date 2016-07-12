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

-import(proplists, [get_value/2]).

-behaviour(emqttd_auth_mod).

-include_lib("emqttd/include/emqttd.hrl").

-export([init/1, check/3, description/0]).

-behaviour(ecpool_worker).

-export([is_superuser/2, feed_var/2, parse_query/1, connect/1, squery/1, equery/2, equery/3]).

-record(state, {super_query, auth_query, hash_type}).

-define(UNDEFINED(S), (S =:= undefined orelse S =:= <<>>)).

%%--------------------------------------------------------------------
%% Auth Module Callbacks
%%--------------------------------------------------------------------

init({SuperQuery, AuthQuery, HashType}) ->
    {ok, #state{super_query = SuperQuery, auth_query = AuthQuery, hash_type = HashType}}.

check(#mqtt_client{username = Username}, _Password, _State) when ?UNDEFINED(Username) ->
    {error, username_undefined};

check(Client, Password, #state{super_query = SuperQuery}) when ?UNDEFINED(Password) ->
    case is_superuser(SuperQuery, Client) of
        true  -> ok;
        false -> {error, password_undefined}
    end;

check(Client, Password, #state{super_query = SuperQuery,
                               auth_query  = AuthSql,
                               hash_type   = HashType}) ->
    case is_superuser(SuperQuery, Client) of
        false -> case squery(feed_var(Client, AuthSql)) of
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

%%--------------------------------------------------------------------
%% Is Superuser?
%%--------------------------------------------------------------------

-spec(is_superuser(undefined | {string(), list()}, mqtt_client()) -> boolean()).
is_superuser(undefined, _Client) ->
    false;
is_superuser(SuperSql, Client) ->
    case squery(feed_var(Client, SuperSql)) of
        {ok, [_Super], [{true}]} ->
            true;
        {ok, [_Super], [{<<"t">>}]} ->
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
    ecpool:with_client(?MODULE, fun(C) -> epgsql:squery(C, Sql) end).

equery(Sql, Params) ->
    ecpool:with_client(?MODULE, fun(C) -> epgsql:equery(C, Sql, Params) end).

equery(Sql, Params, Client) ->
    ecpool:with_client(?MODULE, fun(C) -> epgsql:equery(C, Sql, replvar(Params, Client)) end).

feed_var(#mqtt_client{client_id = ClientId,
                      username  = Username,
                      peername  = {IpAddr, _}}, AclSql) ->
    Vars = [{"%u", Username}, {"%c", ClientId}, {"%a", inet_parse:ntoa(IpAddr)}],
    lists:foldl(fun({Var, Val}, Sql) -> feed_var(Sql, Var, Val) end, AclSql, Vars).

feed_var(Sql, Var, Val) ->
    re:replace(Sql, Var, Val, [global, {return, list}]).

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

