
## Overview 

emqttd Authentication/ACL with PostgreSQL Database.

## Build Plugin

Build the plugin in emqttd project. Checkout this plugin to 'emqttd/plugins/' folder:

If the submodules exist:

```
git submodule update --remote plugins/epgsql
git submodule update --remote plugins/emqttd_plugin_pgsql
```

Orelse:

```
git submodule add https://github.com/epgsql/epgsql.git plugins/epgsql

git submodule add https://github.com/emqtt/emqttd_plugin_pgsql.git plugins/emqttd_plugin_pgsql

make && make dist
```

## Configure Plugin

File: etc/plugin.config

```erlang
[
  {epgsql, [
      {pools, [
          {pgauth, [
              {size, 2},
              {host, "localhost"},
              {port, 5432},
              {username,  ""},
              {password,  ""},
              {database,  "mqtt"},
              {encoding,  utf8}
          ]}
      ]}
  ]},

  {emqttd_plugin_pgsql, [

    %% select password only
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

## Load Plugin

```
./bin/emqttd_ctl plugins load emqttd_plugin_pgsql
```

## Auth Table



## ACL Table



## Support

Fork this project and implement your own authentication/ACL mechanism.

Contact feng@emqtt.io if any issues.


