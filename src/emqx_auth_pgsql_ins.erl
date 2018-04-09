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
    %% TODO: register Hooks, establish connection
    AuthQuery = parse_query(value(auth_query, Conf)),
    SuperQuery = parse_query(value(super_query, Conf)),
    HashType = value(password_hash, Conf),
    AuthEnv = {AuthQuery, SuperQuery, HashType},
    %% TODO: Can't register mutil instances
    ok = emqx_access_control:register_mod(auth, emqx_auth_pgsql, AuthEnv),

    AclQuery = parse_query(value(acl_query, Conf)),

    ok = emqx_access_control:register_mod(acl, emqx_acl_pgsql, AclQuery),
    emqx_auth_pgsql_cfg:register(),

    %% Establish connections
    %% XXX: Need huang to main superviser
    PoolSpec = ecpool:pool_spec(?APP, ?APP, emqx_auth_pgsql_cli, value(server, Conf)),
    supervisor:start_child(emqx_auth_pgsql_sup, PoolSpec),
    ok.

destroy() ->
    emqx_access_control:unregister_mod(acl, emqx_acl_pgsql),
    emqx_access_control:unregister_mod(auth, emqx_auth_pgsql),
    emqx_auth_pgsql_cfg:unregister(),
    supervisor:terminate_child(emqx_auth_pgsql_sup, ?APP),
    ok.

description() -> "Auth/ACL for PostgreSQL".

value(Key, List) -> proplists:get_value(Key, List).

