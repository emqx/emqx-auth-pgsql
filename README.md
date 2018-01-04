
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
## PostgreSQL server address.
##
## Value: Port | IP:Port
##
## Examples: 5432, 127.0.0.1:5432, localhost:5432
auth.pgsql.server = 127.0.0.1:5432

## PostgreSQL pool size.
##
## Value: Number
auth.pgsql.pool = 8

## PostgreSQL username.
##
## Value: String
auth.pgsql.username = root

## PostgreSQL password.
##
## Value: String
## auth.pgsql.password =

## PostgreSQL database.
##
## Value: String
auth.pgsql.database = mqtt

## PostgreSQL database encoding.
##
## Value: String
auth.pgsql.encoding = utf8

## Whether to enable SSL connection.
##
## Value: true | false
auth.pgsql.ssl = false

## SSL keyfile.
##
## Value: File
## auth.pgsql.ssl_opts.keyfile =

## SSL certfile.
##
## Value: File
## auth.pgsql.ssl_opts.certfile =

## SSL cacertfile.
##
## Value: File
## auth.pgsql.ssl_opts.cacertfile =

## Authentication query.
##
## Value: SQL
##
## Variables:
##  - %u: username
##  - %c: clientid
##
auth.pgsql.auth_query = select password from mqtt_user where username = '%u' limit 1

## Password hash.
##
## Value: plain | md5 | sha | sha256 | bcrypt
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

## Superuser query.
##
## Value: SQL
##
## Variables:
##  - %u: username
##  - %c: clientid
auth.pgsql.super_query = select is_superuser from mqtt_user where username = '%u' limit 1

## ACL query. Comment this query, the ACL will be disabled.
##
## Value: SQL
##
## Variables:
##  - %a: ipaddress
##  - %u: username
##  - %c: clientid
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

