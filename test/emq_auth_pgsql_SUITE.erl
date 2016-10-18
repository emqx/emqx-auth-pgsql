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

-module(emq_auth_pgsql_SUITE).

-compile(export_all).

-define(PID, emq_auth_pgsql).

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
    [{group, emq_auth_pgsql}].

groups() ->
    [{emq_auth_pgsql, [sequence],
     [check_acl,
      check_auth]}].

init_per_suite(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    application:start(lager),
    peg_com(DataDir),
    [start_apps(App, DataDir) || App <- [emqttd, emq_auth_pgsql]],
    Config.

end_per_suite(_Config) ->
    application:stop(emq_auth_pgsql),
    application:stop(ecpool),
    application:stop(epgsql),
    application:stop(emqttd),
    emqttd_mnesia:ensure_stopped().

check_acl(_) ->
    init_acl_(),
    User1 = #mqtt_client{client_id = <<"client1">>, username = <<"testuser">>},
    User2 = #mqtt_client{client_id = <<"client2">>, username = <<"xyz">>},
    deny = emqttd_access_control:check_acl(User1, subscribe, <<"users/testuser/1">>),
    deny = emqttd_access_control:check_acl(User2, subscribe, <<"a/b/c">>),
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

start_apps(App, DataDir) ->
    Schema = cuttlefish_schema:files([filename:join([DataDir, atom_to_list(App) ++ ".schema"])]),
    Conf = conf_parse:file(filename:join([DataDir, atom_to_list(App) ++ ".conf"])),
    NewConfig = cuttlefish_generator:map(Schema, Conf),
    Vals = proplists:get_value(App, NewConfig),
    [application:set_env(App, Par, Value) || {Par, Value} <- Vals],
    application:ensure_all_started(App).

peg_com(DataDir) ->
    ParsePeg = file2(3, DataDir, "conf_parse.peg"),
    neotoma:file(ParsePeg),
    ParseErl = file2(3, DataDir, "conf_parse.erl"),
    compile:file(ParseErl, []),

    DurationPeg = file2(3, DataDir, "cuttlefish_duration_parse.peg"),
    neotoma:file(DurationPeg),
    DurationErl = file2(3, DataDir, "cuttlefish_duration_parse.erl"),
    compile:file(DurationErl, []).
    

file2(Times, Dir, FileName) when Times < 1 ->
    filename:join([Dir, "deps", "cuttlefish","src", FileName]);

file2(Times, Dir, FileName) ->
    Dir1 = filename:dirname(Dir),
    file2(Times - 1, Dir1, FileName).


