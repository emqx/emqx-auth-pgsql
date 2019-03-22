%% Copyright (c) 2013-2019 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(emqx_auth_pgsql_cli).

-behaviour(ecpool_worker).

-include("emqx_auth_pgsql.hrl").

-include_lib("emqx/include/emqx.hrl").

-export([connect/1]).
-export([parse_query/2]).
-export([ equery/2
        , equery/3
        ]).

%%--------------------------------------------------------------------
%% Avoid SQL Injection: Parse SQL to Parameter Query.
%%--------------------------------------------------------------------

parse_query(_Par, undefined) ->
    undefined;
parse_query(Par, Sql) ->
    case re:run(Sql, "'%[uca]'", [global, {capture, all, list}]) of
        {match, Variables} ->
            Params = [Var || [Var] <- Variables],
            {atom_to_list(Par), Params};
        nomatch ->
            {atom_to_list(Par), []}
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
    Host     = proplists:get_value(host, Opts),
    Username = proplists:get_value(username, Opts),
    Password = proplists:get_value(password, Opts),
    {ok, C} = epgsql:connect(Host, Username, Password, conn_opts(Opts)),
    lists:foreach(fun(Par) ->
        Sql0 = application:get_env(?APP, Par, undefined),
        case parse_query(Par, Sql0) of
            undefined -> ok;
            {_, Params} ->
                Sql = pgvar(Sql0, Params),
                epgsql:parse(C, atom_to_list(Par), Sql, [])
        end
    end,  [auth_query, acl_query, super_query]),
    {ok, C}.


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
conn_opts([Opt = {ssl_opts, _}|Opts], Acc) ->
    conn_opts(Opts, [Opt|Acc]);
conn_opts([_Opt|Opts], Acc) ->
    conn_opts(Opts, Acc).

equery(Sql, Params) ->
    ecpool:with_client(?APP, fun(C) -> epgsql:prepared_query(C, Sql, Params) end).

equery(Sql, Params, Credentials) ->
    ecpool:with_client(?APP, fun(C) -> epgsql:prepared_query(C, Sql, replvar(Params, Credentials)) end).

replvar(Params, Credentials) ->
    replvar(Params, Credentials, []).

replvar([], _Credentials, Acc) ->
    lists:reverse(Acc);
replvar(["'%u'" | Params], Credentials = #{username := Username}, Acc) ->
    replvar(Params, Credentials, [Username | Acc]);
replvar(["'%c'" | Params], Credentials = #{client_id := ClientId}, Acc) ->
    replvar(Params, Credentials, [ClientId | Acc]);
replvar(["'%a'" | Params], Credentials = #{peername := {IpAddr, _}}, Acc) ->
    replvar(Params, Credentials, [inet_parse:ntoa(IpAddr) | Acc]);
replvar([Param | Params], Credentials, Acc) ->
    replvar(Params, Credentials, [Param | Acc]).

