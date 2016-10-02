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

-module(emqttd_auth_pgsql_app).

-include("emqttd_auth_pgsql.hrl").

-behaviour(application).

-import(emqttd_auth_pgsql, [parse_query/1]).

%% Application callbacks
-export([start/2, prep_stop/1, stop/1]).

%%--------------------------------------------------------------------
%% Application callbacks
%%--------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    gen_conf:init(?APP),
    {ok, Sup} = emqttd_auth_pgsql_sup:start_link(),
    if_enabled(authquery, fun(AuthQuery) ->
        SuperQuery = parse_query(gen_conf:value(?APP, superquery, undefined)),
        {ok, HashType}  = gen_conf:value(?APP, password_hash),
        AuthEnv = {AuthQuery, SuperQuery, HashType},
        ok = emqttd_access_control:register_mod(auth, emqttd_auth_pgsql, AuthEnv)
    end),
    if_enabled(aclquery, fun(AclQuery) ->
        {ok, AclNomatch} = gen_conf:value(?APP, acl_nomatch),
        AclEnv = {AclQuery, AclNomatch},
        ok = emqttd_access_control:register_mod(acl, emqttd_acl_pgsql, AclEnv)
    end),
    {ok, Sup}.

prep_stop(State) ->
    emqttd_access_control:unregister_mod(acl, emqttd_acl_pgsql),
    emqttd_access_control:unregister_mod(auth, emqttd_auth_pgsql),
    State.

stop(_State) ->
    ok.

if_enabled(Cfg, Fun) ->
    case gen_conf:value(?APP, Cfg) of
        {ok, Query} -> Fun(parse_query(Query));
        undefined   -> ok
    end.

