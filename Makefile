PROJECT = emq_modules
PROJECT_DESCRIPTION = EMQ Modules
PROJECT_VERSION = 2.1.1

BUILD_DEPS = emqttd cuttlefish
dep_emqttd = git https://github.com/emqtt/emqttd emq20
dep_cuttlefish = git https://github.com/emqtt/cuttlefish

NO_AUTOPATCH = cuttlefish

ERLC_OPTS += +'{parse_transform, lager_transform}'

include erlang.mk

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emq_modules.conf -i priv/emq_modules.schema -d data
