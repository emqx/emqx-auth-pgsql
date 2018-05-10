%%--------------------------------------------------------------------
%% Copyright (c) 2013-2018 EMQ Enterprise, Inc. (http://emqtt.io)
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

-module(emqx_auth_pgsql_app).

-behaviour(application).

-include("emqx_auth_pgsql.hrl").

-import(emqx_auth_pgsql_cli, [parse_query/1]).
-import(proplists, [get_value/2]).

%% Application callbacks
-export([start/2, stop/1]).

-behaviour(emqx_services).

%% emqx_services callbacks
-export([create/2, destroy/0, description/0]).

%%--------------------------------------------------------------------
%% Application callbacks
%%--------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    {ok, Sup} = emqx_auth_pgsql_sup:start_link(),
    ok = emqx_services:register(?APP, auth, []),

    {ok, Sup}.

stop(_State) ->
    emqx_services:unregister(?APP).

%%--------------------------------------------------------------------
%% Service callback
%%--------------------------------------------------------------------

create(Conf, _Env) ->
    PoolSpec = ecpool:pool_spec(?APP, ?APP, emqx_auth_pgsql_cli, get_value(server, Conf)),
    case supervisor:start_child(emqx_auth_pgsql_sup, PoolSpec) of
        {ok, _Pid} ->
            %% Register AUTH/ACL, Hook etc.
            if_enabled(auth_query, Conf, fun(AuthQuery) ->
                SuperQuery = parse_query(get_value(super_query, Conf)),
                HashType = get_value(password_hash, Conf),
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
    case get_value(Par, Conf) of
        undefined   -> ok;
        Query -> Fun(parse_query(Query))
    end.

