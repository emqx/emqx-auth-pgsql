
emq-auth-pgsql
==============

Authentication/ACL with PostgreSQL Database.

Build Plugin
------------

make && make tests

Configuration
-------------

File: etc/emq_auth_pgsql.conf

```
## Postgre Server
auth.pgsql.server = 127.0.0.1:5432

auth.pgsql.pool = 8

auth.pgsql.username = root

#auth.pgsql.password = 

auth.pgsql.database = mqtt

auth.pgsql.encoding = utf8

auth.pgsql.ssl = false

##auth.pgsql.ssl_opts.keyfile =

##auth.pgsql.ssl_opts.certfile =

##auth.pgsql.ssl_opts.cacertfile =

## Variables: %u = username, %c = clientid, %a = ipaddress

## Authentication Query: select password or password, salt
auth.pgsql.auth_query = select password from mqtt_user where username = '%u' limit 1

## Password hash: plain, md5, sha, sha256, bcrypt
auth.pgsql.password_hash = sha256

## sha256 with salt prefix
## auth.pgsql.password_hash = salt,sha256

## sha256 with salt suffix
## auth.pgsql.password_hash = sha256,salt

## bcrypt with salt prefix
## auth.pgsql.password_hash = salt,bcrypt

## pbkdf2 with macfun iterations dklen
## macfun: md4, md5, ripemd160, sha, sha224, sha256, sha384, sha512
## auth.pgsql.password_hash = pbkdf2,sha256,1000,20

## Superuser Query
auth.pgsql.super_query = select is_superuser from mqtt_user where username = '%u' limit 1

## ACL Query. Comment this query, the acl will be disabled.
auth.pgsql.acl_query = select allow, ipaddr, username, clientid, access, topic from mqtt_acl where ipaddr = '%a' or username = '%u' or username = '$all' or clientid = '%c'
```

Load Plugin
-----------

./bin/emqttd_ctl plugins load emq_auth_pgsql

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

License
-------

Apache License Version 2.0

Author
------

EMQ X Team.

