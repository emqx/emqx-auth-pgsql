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

-module(emqx_auth_pgsql_app).

-behaviour(application).

-include("emqx_auth_pgsql.hrl").

-import(emqx_auth_pgsql_cli, [parse_query/1]).

%% Application callbacks
-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% Application callbacks
%%--------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    {ok, Sup} = emqx_auth_pgsql_sup:start_link(),
    %if_enabled(auth_query, fun(AuthQuery) ->
    %    SuperQuery = parse_query(application:get_env(?APP, super_query, undefined)),
    %    {ok, HashType}  = application:get_env(?APP, password_hash),
    %    AuthEnv = {AuthQuery, SuperQuery, HashType},
    %    ok = emqx_access_control:register_mod(auth, emqx_auth_pgsql, AuthEnv)
    %end),
    %if_enabled(acl_query, fun(AclQuery) ->
    %    ok = emqx_access_control:register_mod(acl, emqx_acl_pgsql, AclQuery)
    %end),
    %emqx_auth_pgsql_cfg:register(),
    Schema = [[{key, host}, {default, ["127.0.0.1"]}, {required, false}, {descr, <<"Desc">>}],
              [{key, port}, {default, [5432]}, {required, false}, {descr, <<"Desc">>}],
              [{key, username}, {default, ["root"]}, {required, false}, {descr, <<"Desc">>}],
              [{key, database}, {default, ["mqtt"]}, {required, false}, {descr, <<"Desc">>}],
              [{key, encoding}, {default, ["utf8"]}, {required, false}, {descr, <<"Desc">>}],
              [{key, ssl}, {default, [false]}, {required, false}, {descr, <<"Desc">>}],
              [{key, auth_query}, {default, ["select password from mqtt_user where username = '%u' limit 1"]}, {required, false}, {descr, <<"Desc">>}],
              [{key, password_hash}, {default, [sha256, plain, md5, sha, bcrypt]}, {required, false}, {descr, <<"Desc">>}],
              [{key, super_query}, {default, ["select is_superuser from mqtt_user where username = '%u' limit 1"]}, {required, false}, {descr, <<"Desc">>}],
              [{key, acl_query}, {default, ["select allow, ipaddr, username, clientid, access, topic from mqtt_acl where ipaddr = '%a' or username = '%u' or username = '$all' or clientid = '%c'"]}, {required, false}, {descr, <<"Desc">>}]],
    ok = emqx_services:register(emqx_auth_pgsql_ins, ?APP, auth, []),

    {ok, Sup}.

stop(_State) ->
    %emqx_access_control:unregister_mod(acl, emqx_acl_pgsql),
    %emqx_access_control:unregister_mod(auth, emqx_auth_pgsql),
    %emqx_auth_pgsql_cfg:unregister().
    emqx_services:unregister(emqx_auth_pgsql_ins).

if_enabled(Par, Fun) ->
    case application:get_env(?APP, Par) of
        {ok, Query} -> Fun(parse_query(Query));
        undefined   -> ok
    end.

