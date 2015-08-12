
# emqttd_plugin_pgsql

PostgreSQL Authentication Plugin.

Notice: This plugin provide a simple username, password authentication with PostgreSQL. 


## Build Plugin

Build the plugin in emqttd project. Checkout this plugin to 'emqttd/plugins/' folder:

```
git submodule add https://github.com/epgsql/epgsql.git plugins/epgsql

git submodule add https://github.com/emqtt/emqttd_plugin_pgsql.git plugins/emqttd_plugin_pgsql

make && make dist
```

If the submodules exist:

```
git submodule update --remote plugins/epgsql
git submodule update --remote plugins/emqttd_plugin_pgsql
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
      {user_table, mqtt_users},
      %% plain, md5, sha
      {password_hash, plain}, %% Only for test
      {field_mapper, [
          {username, username},
          {password, password}
      ]}
  ]}
].
```

## Load Plugin

```
./bin/emqttd_ctl plugins load emqttd_plugin_pgsql
```

## Customize Plugin

Fork this project and implement your own authentication/ACL mechanism.

## Author 

Feng Lee <feng@emqtt.io>


