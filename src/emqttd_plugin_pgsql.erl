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

%% @doc emqttd pgsql plugin.
-module(emqttd_plugin_pgsql).

-export([load/0, unload/0]).

load() ->
    {ok, AuthSql}  = application:get_env(?MODULE, authquery),
    {ok, HashType} = application:get_env(?MODULE, password_hash),
    ok = emqttd_access_control:register_mod(auth, emqttd_auth_pgsql, {AuthSql, HashType}),
    with_acl_enabled(fun(AclSql) ->
        {ok, AclNomatch} = application:get_env(?MODULE, acl_nomatch),
        ok = emqttd_access_control:register_mod(acl, emqttd_acl_pgsql, {AclSql, AclNomatch})
    end).

unload() ->
    emqttd_access_control:unregister_mod(auth, emqttd_auth_pgsql),
    with_acl_enabled(fun(_AclSql) ->
        emqttd_access_control:unregister_mod(acl, emqttd_acl_pgsql)
    end).
    
with_acl_enabled(Fun) ->
    case application:get_env(?MODULE, aclquery) of
        {ok, AclSql} -> Fun(AclSql);
        undefined    -> ok
    end.

