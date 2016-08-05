PROJECT = emqttd_auth_pgsql
PROJECT_DESCRIPTION = emqttd Authentication/ACL against PostgreSQL
PROJECT_VERSION = 2.0

DEPS = epgsql ecpool emqttd 

dep_epgsql  = git https://github.com/epgsql/epgsql master
dep_ecpool = git https://github.com/emqtt/ecpool master
dep_emqttd = git https://github.com/emqtt/emqttd plus

ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app:: rebar.config
