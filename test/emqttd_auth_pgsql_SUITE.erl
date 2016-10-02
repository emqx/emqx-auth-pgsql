%%--------------------------------------------------------------------
%% Copyright (c) 2012-2016 Feng Lee <feng@emqtt.io>.
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

-module(emqttd_auth_pgsql_SUITE).

-compile(export_all).

-define(PID, emqttd_auth_pgsql).

-include_lib("emqttd/include/emqttd.hrl").

%%setp1 init table
-define(DROP_ACL_TABLE, "DROP TABLE IF EXISTS mqtt_acl").

-define(CREATE_ACL_TABLE, "CREATE TABLE mqtt_acl (
                           id SERIAL primary key,
                           allow integer,
                           ipaddr character varying(60),
                           username character varying(100),
                           clientid character varying(100),
                           access  integer,
                           topic character varying(100))").

-define(INIT_ACL, "INSERT INTO mqtt_acl (id, allow, ipaddr, username, clientid, access, topic)
                   VALUES
	               (1,1,NULL,'$all',NULL,2,'#'),
	               (2,0,NULL,'$all',NULL,1,'$SYS/#'),
	               (3,0,NULL,'$all',NULL,1,'eq #'),
	               (4,1,'127.0.0.1',NULL,NULL,2,'$SYS/#'),
	               (5,1,'127.0.0.1',NULL,NULL,2,'#'),
	               (6,1,NULL,'dashboard',NULL,1,'$SYS/#')").

-define(DROP_AUTH_TABLE, "DROP TABLE IF EXISTS mqtt_user").

-define(CREATE_AUTH_TABLE, "CREATE TABLE mqtt_user (
                            id SERIAL primary key,
                            is_superuser boolean,
                            username character varying(100),
                            password character varying(100),
                            salt character varying(40))").

-define(INIT_AUTH, "INSERT INTO mqtt_user (id, is_superuser, username, password, salt)
                     VALUES  
                     (1, false, 'testuser1', 'pass1', 'plain'),
                     (2, false, 'testuser2', 'pass2', 'plain')").


all() ->
    [{group, emqttd_auth_pgsql}].

groups() ->
    [{emqttd_auth_pgsql, [sequence],
     [check_auth,
      check_acl
      ]}].

init_per_suite(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    application:start(lager),
    application:set_env(emqttd, conf, filename:join([DataDir, "emqttd.conf"])),
    application:ensure_all_started(emqttd),
    application:set_env(emqttd_auth_pgsql, conf, filename:join([DataDir, "emqttd_auth_pgsql.conf"])),
    application:ensure_all_started(emqttd_auth_pgsql),
    Config.

end_per_suite(_Config) ->
    application:stop(emqttd_auth_pgsql),
    application:stop(ecpool),
    application:stop(epgsql),
    application:stop(emqttd),
    emqttd_mnesia:ensure_stopped().

check_acl(_) ->
    init_acl_(),
    User1 = #mqtt_client{client_id = <<"client1">>, username = <<"testuser">>},
    User2 = #mqtt_client{client_id = <<"client2">>, username = <<"xyz">>},
    allow = emqttd_access_control:check_acl(User1, subscribe, <<"users/testuser/1">>),
    allow = emqttd_access_control:check_acl(User2, subscribe, <<"a/b/c">>),
    deny  = emqttd_access_control:check_acl(User1, subscribe, <<"$SYS/testuser/1">>),
    deny  = emqttd_access_control:check_acl(User2, subscribe, <<"$SYS/testuser/1">>),
    drop_acl_().


init_acl_() ->
    {ok, Pid} = ecpool_worker:client(gproc_pool:pick_worker({ecpool, ?PID})),
    {ok, [], []} = epgsql:squery(Pid, ?DROP_ACL_TABLE),
    {ok, [], []} = epgsql:squery(Pid, ?CREATE_ACL_TABLE),
    {ok, _} = epgsql:equery(Pid, ?INIT_ACL).

drop_acl_() -> 
    {ok, Pid} = ecpool_worker:client(gproc_pool:pick_worker({ecpool, ?PID})),
    {ok, [], []}= epgsql:squery(Pid, ?DROP_ACL_TABLE).

check_auth(_) ->
    init_auth_(), 
    User1 = #mqtt_client{client_id = <<"client1">>, username = <<"testuser1">>},
    {ok, false} = emqttd_access_control:auth(User1, <<"pass1">>),
    {error, _} = emqttd_access_control:auth(User1, <<"pass">>),
    drop_auth_().

init_auth_() ->
    {ok, Pid} = ecpool_worker:client(gproc_pool:pick_worker({ecpool, ?PID})),
    {ok, [], []} = epgsql:squery(Pid, ?DROP_AUTH_TABLE),
    {ok, [], []} = epgsql:squery(Pid, ?CREATE_AUTH_TABLE),
    {ok, _} = epgsql:equery(Pid, ?INIT_AUTH).

drop_auth_() ->
    {ok, Pid} = ecpool_worker:client(gproc_pool:pick_worker({ecpool, ?PID})),
    {ok, [], []} = epgsql:squery(Pid, ?DROP_AUTH_TABLE).


