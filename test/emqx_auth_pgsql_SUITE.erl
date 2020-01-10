%%--------------------------------------------------------------------
%% Copyright (c) 2020 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(emqx_auth_pgsql_SUITE).

-compile(export_all).

-define(POOL, emqx_auth_pgsql).

-define(APP, emqx_auth_pgsql).

-include_lib("emqx/include/emqx.hrl").

-include_lib("eunit/include/eunit.hrl").

-include_lib("common_test/include/ct.hrl").

%%setp1 init table
-define(DROP_ACL_TABLE, "DROP TABLE IF EXISTS mqtt_acl_test").

-define(CREATE_ACL_TABLE, "CREATE TABLE mqtt_acl_test (
                           id SERIAL primary key,
                           allow integer,
                           ipaddr character varying(60),
                           username character varying(100),
                           clientid character varying(100),
                           access  integer,
                           topic character varying(100))").

-define(INIT_ACL, "INSERT INTO mqtt_acl_test (id, allow, ipaddr, username, clientid, access, topic)
                   VALUES
                   (1,1,'127.0.0.1','u1','c1',1,'t1'),
                   (2,0,'127.0.0.1','u2','c2',1,'t1'),
                   (3,1,'10.10.0.110','u1','c1',1,'t1'),
                   (4,1,'127.0.0.1','u3','c3',3,'t1')").

-define(DROP_AUTH_TABLE, "DROP TABLE IF EXISTS mqtt_user_test").

-define(CREATE_AUTH_TABLE, "CREATE TABLE mqtt_user_test (
                            id SERIAL primary key,
                            is_superuser boolean,
                            username character varying(100),
                            password character varying(100),
                            salt character varying(40))").

-define(INIT_AUTH, "INSERT INTO mqtt_user_test (id, is_superuser, username, password, salt)
                     VALUES
                     (1, true, 'plain', 'plain', 'salt'),
                     (2, false, 'md5', '1bc29b36f623ba82aaf6724fd3b16718', 'salt'),
                     (3, false, 'sha', 'd8f4590320e1343a915b6394170650a8f35d6926', 'salt'),
                     (4, false, 'sha256', '5d5b09f6dcb2d53a5fffc60c4ac0d55fabdf556069d6631545f42aa6e3500f2e', 'salt'),
                     (5, false, 'pbkdf2_password', 'cdedb5281bb2f801565a1122b2563515', 'ATHENA.MIT.EDUraeburn'),
                     (6, false, 'bcrypt_foo', '$2a$12$sSS8Eg.ovVzaHzi1nUHYK.HbUIOdlQI0iS22Q5rd5z.JVVYH6sfm6', '$2a$12$sSS8Eg.ovVzaHzi1nUHYK.'),
                     (7, false, 'bcrypt', '$2y$16$rEVsDarhgHYB0TGnDFJzyu5f.T.Ha9iXMTk9J36NCMWWM7O16qyaK', 'salt')").

all() ->
    [{group, emqx_auth_pgsql_auth},
     {group, emqx_auth_pgsql_acl},
     {group, emqx_auth_pgsql}
     %{group, auth_pgsql_config}
    ].

groups() ->
    [{emqx_auth_pgsql_auth, [sequence], [check_auth]},
     {emqx_auth_pgsql_acl, [sequence], [check_acl, acl_super]},
     {emqx_auth_pgsql, [sequence], [comment_config, placeholders]},
     {auth_pgsql_config, [sequence], [server_config]}].

init_per_suite(Config) ->
    emqx_ct_helpers:start_apps([emqx, emqx_auth_pgsql], fun set_special_configs/1),
    init_auth_(),
    init_acl_(),
    Config.

end_per_suite(_Config) ->
    drop_auth_(),
    drop_acl_(),
    application:stop(emqx_auth_pgsql),
    application:stop(emqx).

set_special_configs(emqx) ->
    application:set_env(emqx, acl_nomatch, deny),
    application:set_env(emqx, acl_file,
                        emqx_ct_helpers:deps_path(emqx, "test/emqx_SUITE_data/acl.conf")),
    application:set_env(emqx, allow_anonymous, false),
    application:set_env(emqx, enable_acl_cache, false),
    application:set_env(emqx, plugins_loaded_file,
                        emqx_ct_helpers:deps_path(emqx, "test/emqx_SUITE_data/loaded_plugins"));
set_special_configs(emqx_auth_pgsql) ->
    {ok, Server} = application:get_env(?APP, server),
    application:set_env(?APP, server,
                        lists:keyreplace(password,
                                         1,
                                         lists:keyreplace(pool_size, 1, Server, {pool_size, 1}),
                                         {password, "public"})),
    application:set_env(?APP, acl_query, "select allow, ipaddr, username, clientid, access, topic from mqtt_acl_test where ipaddr = '%a' or username = '%u' or username = '$all' or clientid = '%c'"),
    application:set_env(?APP, super_query, "select is_superuser from mqtt_user_test where username = '%u' limit 1"),
    application:set_env(?APP, auth_query, "select password from mqtt_user_test where username = '%u' limit 1");
set_special_configs(_App) ->
    ok.

comment_config(_) ->
    AuthCount = length(emqx_hooks:lookup('client.authenticate')),
    AclCount = length(emqx_hooks:lookup('client.check_acl')),
    application:stop(?APP),
    [application:unset_env(?APP, Par) || Par <- [acl_query, auth_query]],
    application:start(?APP),
    ?assertEqual([], emqx_hooks:lookup('client.authenticate')),
    ?assertEqual(AuthCount - 1, length(emqx_hooks:lookup('client.authenticate'))),
    ?assertEqual(AclCount - 1, length(emqx_hooks:lookup('client.check_acl'))).

placeholders(_) ->
    ClientA = #{username => <<"plain">>, clientid => <<"plain">>, zone => external},

    reload([{password_hash, plain},
            {auth_query, "select password from mqtt_user_test where username = '%u' and 'a_cn_val' = '%C' limit 1"}]),
    {error, not_authorized} =
        emqx_access_control:authenticate(ClientA#{password => <<"plain">>}),
    {error, not_authorized} =
        emqx_access_control:authenticate(ClientA#{password => <<"plain">>, cn => undefined}),
    {ok, _} =
        emqx_access_control:authenticate(ClientA#{password => <<"plain">>, cn => <<"a_cn_val">>}),

    reload([{auth_query, "select password from mqtt_user_test where username = '%c' and 'a_dn_val' = '%d' limit 1"}]),
    {error, not_authorized} =
        emqx_access_control:authenticate(ClientA#{password => <<"plain">>}),
    {error, not_authorized} =
        emqx_access_control:authenticate(ClientA#{password => <<"plain">>, dn => undefined}),
    {ok, _} =
        emqx_access_control:authenticate(ClientA#{password => <<"plain">>, dn => <<"a_dn_val">>}).

check_auth(_) ->
    Plain = #{clientid => <<"client1">>, username => <<"plain">>, zone => external},
    Md5 = #{clientid => <<"md5">>, username => <<"md5">>, zone => external},
    Sha = #{clientid => <<"sha">>, username => <<"sha">>, zone => external},
    Sha256 = #{clientid => <<"sha256">>, username => <<"sha256">>, zone => external},
    Pbkdf2 = #{clientid => <<"pbkdf2_password">>, username => <<"pbkdf2_password">>, zone => external},
    BcryptFoo = #{clientid => <<"bcrypt_foo">>, username => <<"bcrypt_foo">>, zone => external},
    User1 = #{clientid => <<"bcrypt_foo">>, username => <<"user">>, zone => external},
    Bcrypt = #{clientid => <<"bcrypt">>, username => <<"bcrypt">>, zone => external},
    reload([{password_hash, plain}]),
    {ok, #{is_superuser := true}} = emqx_access_control:authenticate(Plain#{password => <<"plain">>}),
    reload([{password_hash, md5}]),
    {ok, #{is_superuser := false}} = emqx_access_control:authenticate(Md5#{password => <<"md5">>}),
    reload([{password_hash, sha}]),
    {ok, #{is_superuser := false}} = emqx_access_control:authenticate(Sha#{password => <<"sha">>}),
    reload([{password_hash, sha256}]),
    {ok, #{is_superuser := false}} = emqx_access_control:authenticate(Sha256#{password => <<"sha256">>}),
    reload([{password_hash, bcrypt}]),
    {ok, #{is_superuser := false}} = emqx_access_control:authenticate(Bcrypt#{password => <<"password">>}),
    %%pbkdf2 sha
    reload([{password_hash, {pbkdf2, sha, 1, 16}}, {auth_query, "select password, salt from mqtt_user_test where username = '%u' limit 1"}]),
    {ok, #{is_superuser := false}} = emqx_access_control:authenticate(Pbkdf2#{password => <<"password">>}),
    reload([{password_hash, {salt, bcrypt}}]),
    {ok, #{is_superuser := false}} = emqx_access_control:authenticate(BcryptFoo#{password => <<"foo">>}),
    {error, _} = emqx_access_control:authenticate(User1#{password => <<"foo">>}),
    {error, not_authorized} = emqx_access_control:authenticate(Bcrypt#{password => <<"password">>}).

init_auth_() ->
    {ok, Pid} = ecpool_worker:client(gproc_pool:pick_worker({ecpool, ?POOL})),
    {ok, [], []} = epgsql:squery(Pid, ?DROP_AUTH_TABLE),
    {ok, [], []} = epgsql:squery(Pid, ?CREATE_AUTH_TABLE),
    {ok, _} = epgsql:equery(Pid, ?INIT_AUTH).

drop_auth_() ->
    {ok, Pid} = ecpool_worker:client(gproc_pool:pick_worker({ecpool, ?POOL})),
    {ok, [], []} = epgsql:squery(Pid, ?DROP_AUTH_TABLE).

check_acl(_) ->
    User1 = #{zone => external, peerhost => {127,0,0,1}, clientid => <<"c1">>, username => <<"u1">>},
    User2 = #{zone => external, peerhost => {127,0,0,1}, clientid => <<"c2">>, username => <<"u2">>},
    allow = emqx_access_control:check_acl(User1, subscribe, <<"t1">>),
    deny = emqx_access_control:check_acl(User2, subscribe, <<"t1">>),

    User3 = #{zone => external, peerhost => {10,10,0,110}, clientid => <<"c1">>, username => <<"u1">>},
    User4 = #{zone => external, peerhost => {10,10,10,110}, clientid => <<"c1">>, username => <<"u1">>},
    allow = emqx_access_control:check_acl(User3, subscribe, <<"t1">>),
    allow = emqx_access_control:check_acl(User3, subscribe, <<"t1">>),
    allow = emqx_access_control:check_acl(User3, subscribe, <<"t2">>),%% nomatch -> ignore -> emqttd acl
    allow = emqx_access_control:check_acl(User4, subscribe, <<"t1">>),%% nomatch -> ignore -> emqttd acl

    User5 = #{zone => external, peerhost => {127,0,0,1}, clientid => <<"c3">>, username => <<"u3">>},
    allow = emqx_access_control:check_acl(User5, subscribe, <<"t1">>),
    allow = emqx_access_control:check_acl(User5, publish, <<"t1">>).

acl_super(_Config) ->
    reload([{password_hash, plain}, {auth_query, "select password from mqtt_user_test where username = '%u' limit 1"}]),
    {ok, C} = emqtt:start_link([{host, "localhost"}, {clientid, <<"simpleClient">>},
                                {username, <<"plain">>}, {password, <<"plain">>}]),
    {ok, _} = emqtt:connect(C),
    timer:sleep(10),
    emqtt:subscribe(C, <<"TopicA">>, qos2),
    emqtt:publish(C, <<"TopicA">>, <<"Payload">>, qos2),
    timer:sleep(1000),
    receive
        {publish, #{payload := Payload}} ->
            ?assertEqual(<<"Payload">>, Payload)
    after
        1000 ->
           ct:fail({receive_timeout, <<"Payload">>}),
            ok
    end,
    emqtt:disconnect(C).

server_config(_) ->
    I = [{host, "localhost"},
         {pool_size, 1},
         {port, 5432},
         {auto_reconnect, 1},
         {username, "admin"},
         {password, "public"},
         {database, "sercrit"},
         {encoding, utf8},
         {ssl, false},
         {ssl_opts,[]}],
    SetConfigKeys = ["server=localhost:5432",
                     "pool=1",
                     "username=admin",
                     "password=public",
                     "database=sercrit",
                     "encoding=gbk",
                     "ssl=true",
                     "ssl_opts.keyfile=/etc/keyfile",
                     "ssl_opts.certfile=/key/certfile",
                     "ssl_opts.cacertfile=key/cafile",
                     "password_hash=salt,sha256"],
    lists:foreach(fun set_cmd/1, SetConfigKeys),
    {ok, E} =  application:get_env(emqx_auth_pgsql, server),
    {ok, Hash} =  application:get_env(emqx_auth_pgsql, password_hash),
    ?assertEqual(lists:sort(I), lists:sort(E)),
    ?assertEqual({salt,sha256}, Hash).

set_cmd(Key) ->
    emqx_cli_config:run(["config", "set", string:join(["authenticate.pgsql", Key], "."), "--app=emqx_auth_pgsql"]).

init_acl_() ->
    {ok, Pid} = ecpool_worker:client(gproc_pool:pick_worker({ecpool, ?POOL})),
    {ok, [], []} = epgsql:squery(Pid, ?DROP_ACL_TABLE),
    {ok, [], []} = epgsql:squery(Pid, ?CREATE_ACL_TABLE),
    {ok, _} = epgsql:equery(Pid, ?INIT_ACL).

drop_acl_() ->
    {ok, Pid} = ecpool_worker:client(gproc_pool:pick_worker({ecpool, ?POOL})),
    {ok, [], []}= epgsql:squery(Pid, ?DROP_ACL_TABLE).

reload(Config) when is_list(Config) ->
    application:stop(?APP),
    [application:set_env(?APP, K, V) || {K, V} <- Config],
    application:start(?APP).
