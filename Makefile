PROJECT = emq_auth_pgsql
PROJECT_DESCRIPTION = Authentication/ACL with PostgreSQL
PROJECT_VERSION = 2.0.1

DEPS = epgsql ecpool

dep_epgsql = git https://github.com/epgsql/epgsql master
dep_ecpool = git https://github.com/emqtt/ecpool master

BUILD_DEPS = emqttd
dep_emqttd = git https://github.com/emqtt/emqttd emq20

NO_AUTOPATCH = cuttlefish
TEST_DEPS = cuttlefish
dep_cuttlefish = git https://github.com/emqtt/cuttlefish

ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app:: rebar.config

app.config::
	cuttlefish -l info -e etc/ -c etc/emq_auth_pgsql.conf -i priv/emq_auth_pgsql.schema -d data

