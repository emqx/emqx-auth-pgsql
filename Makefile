PROJECT = emqttd_auth_pgsql
PROJECT_DESCRIPTION = Authentication/ACL with PostgreSQL
PROJECT_VERSION = 2.0

DEPS = epgsql ecpool gen_conf

dep_epgsql   = git https://github.com/epgsql/epgsql master
dep_ecpool   = git https://github.com/emqtt/ecpool master
dep_gen_conf = git https://github.com/emqtt/gen_conf master

BUILD_DEPS = emqttd
dep_emqttd = git https://github.com/emqtt/emqttd master

ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app:: rebar.config
