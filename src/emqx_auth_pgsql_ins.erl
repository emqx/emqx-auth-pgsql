%%--------------------------------------------------------------------
%% Copyright (c) 2013-2017 EMQ Enterprise, Inc. (http://emqtt.io)
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

-module(emqx_auth_pgsql_ins).

-behaviour(emqx_services).

-include("emqx_auth_pgsql.hrl").

-import(emqx_auth_pgsql_cli, [parse_query/1]).

%% emqx_services callbacks
-export([init/1, create/2, destroy/0, description/0]).


init(_Env) -> ok.

create(Conf, _Env) ->
    lager:debug("create instance ~p", [Conf]),

    PoolSpec = ecpool:pool_spec(?APP, ?APP, emqx_auth_pgsql_cli, value(server, Conf)),
    case supervisor:start_child(emqx_auth_pgsql_sup, PoolSpec) of
        {ok, _Pid} ->
            %% Register AUTH/ACL, Hook etc.
            if_enabled(auth_query, Conf, fun(AuthQuery) ->
                SuperQuery = parse_query(value(super_query, Conf)),
                HashType = value(password_hash, Conf),
                AuthEnv = {AuthQuery, SuperQuery, HashType},
                %% TODO: Can't register mutil instances
                ok = emqx_access_control:register_mod(auth, emqx_auth_pgsql, AuthEnv)
            end),
            if_enabled(acl_query, Conf, fun(AclQuery) ->
                ok = emqx_access_control:register_mod(acl, emqx_acl_pgsql, AclQuery)
            end),
            emqx_auth_pgsql_cfg:register(), ok;
        {error, Reason} ->
            lager:error("start connection pool error ~p", [Reason]),
            {error, Reason}
    end.

destroy() ->
    emqx_access_control:unregister_mod(acl, emqx_acl_pgsql),
    emqx_access_control:unregister_mod(auth, emqx_auth_pgsql),
    emqx_auth_pgsql_cfg:unregister(),
    supervisor:terminate_child(emqx_auth_pgsql_sup, ?APP),
    supervisor:delete_child(emqx_auth_pgsql_sup, ?APP),
    ok.

description() -> "Auth/ACL for PostgreSQL".

%%--------------------------------------------------------------------
%% Interval Functions
%%--------------------------------------------------------------------

if_enabled(Par, Conf, Fun) ->
    case value(Par, Conf) of
        undefined   -> ok;
        Query -> Fun(parse_query(Query))
    end.

value(Key, List) -> proplists:get_value(Key, List).

