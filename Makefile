PROJECT = emqx_auth_pgsql
PROJECT_DESCRIPTION = EMQ X Authentication/ACL with PostgreSQL
PROJECT_VERSION = 3.0

DEPS = epgsql ecpool clique

dep_epgsql = git https://github.com/epgsql/epgsql 4.1.0
dep_ecpool = git https://github.com/emqtt/ecpool master
dep_clique = git https://github.com/emqx/clique

BUILD_DEPS = emqx cuttlefish
dep_emqx = git git@github.com:emqtt/emqttd emqx30
dep_cuttlefish = git https://github.com/emqx/cuttlefish

NO_AUTOPATCH = cuttlefish

ERLC_OPTS += +debug_info
ERLC_OPTS += +'{parse_transform, lager_transform}'

TEST_DEPS = emq_auth_username
dep_emqx_auth_username = git https://github.com/emqx/emqx-auth-username emqx30

TEST_ERLC_OPTS += +debug_info
TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app:: rebar.config

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_auth_pgsql.conf -i priv/emqx_auth_pgsql.schema -d data

