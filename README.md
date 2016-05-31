
emqttd_plugin_pgsql
===================

Authentication/ACL with PostgreSQL Database.

Build Plugin
------------

Build the plugin in emqttd project. Checkout the plugin to 'emqttd/plugins/' folder:

If the submodules exist:

```
git submodule update --remote plugins/emqttd_plugin_pgsql
```

Orelse:

```
git submodule add https://github.com/emqtt/emqttd_plugin_pgsql.git plugins/emqttd_plugin_pgsql

make && make dist
```

Configure Plugin
----------------

File: etc/plugin.config

```erlang
[

  {emqttd_plugin_pgsql, [

    {pgsql_pool, [
      %% ecpool options
      {pool_size, 8},
      {auto_reconnect, 3},

      %% pgsql options
      {host, "localhost"},
      {port, 5432},
      {username, "feng"},
      {password, ""},
      {database, "mqtt"},
      {encoding,  utf8}
    ]},

    %% Variables: %u = username, %c = clientid, %a = ipaddress

    %% Superuser Query
    {superquery, "select is_superuser from mqtt_user where username = '%u' limit 1"},

    %% Authentication Query: select password only
    {authquery, "select password from mqtt_user where username = '%u' limit 1"},

    %% hash algorithm: md5, sha, sha256, pbkdf2?
    {password_hash, sha256},

    %% select password with salt
    %% {authquery, "select password, salt from mqtt_user where username = '%u'"},

    %% sha256 with salt prefix
    %% {password_hash, {salt, sha256}},

    %% sha256 with salt suffix
    %% {password_hash, {sha256, salt}},

    %% Comment this query, the acl will be disabled. Notice: don't edit this query!
    {aclquery, "select allow, ipaddr, username, clientid, access, topic from mqtt_acl
                 where ipaddr = '%a' or username = '%u' or username = '$all' or clientid = '%c'"},

    %% If no rules matched, return...
    {acl_nomatch, allow}
  ]}
].
```

Load Plugin
-----------

```
./bin/emqttd_ctl plugins load emqttd_plugin_pgsql
```

Auth Table
----------

Notice: This is a demo table. You could authenticate with any user table.

```sql
CREATE TABLE mqtt_user (
  id SERIAL primary key,
  is_superuser boolean,
  username character varying(100),
  password character varying(100),
  salt character varying(40)
) 
```

ACL Table
---------

```sql
CREATE TABLE mqtt_acl (
  id SERIAL primary key,
  allow integer,
  ipaddr character varying(60),
  username character varying(100),
  clientid character varying(100),
  access  integer,
  topic character varying(100)
) 

INSERT INTO mqtt_acl (id, allow, ipaddr, username, clientid, access, topic)
VALUES
	(1,1,NULL,'$all',NULL,2,'#'),
	(2,0,NULL,'$all',NULL,1,'$SYS/#'),
	(3,0,NULL,'$all',NULL,1,'eq #'),
	(5,1,'127.0.0.1',NULL,NULL,2,'$SYS/#'),
	(6,1,'127.0.0.1',NULL,NULL,2,'#'),
	(7,1,NULL,'dashboard',NULL,1,'$SYS/#');
```

**Notice that only one value allowed for ipaddr, username and clientid fields.**

Support
-------

Fork this project and implement your own authentication/ACL mechanism.

Contact feng at emqtt.io if any issues.

License
-------

Apache License Version 2.0

