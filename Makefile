PROJECT = emq_modules
PROJECT_DESCRIPTION = EMQ Modules
PROJECT_VERSION = 2.3.3

DEPS = ecpool brod gpb supervisor3 kafka_protocol snappyer
dep_ecpool = git https://github.com/emqtt/ecpool master
dep_brod = git https://github.com/klarna/brod 3.3.4
dep_gpb = git https://github.com/vleushin/gpb 3.26.6-vsn

BUILD_DEPS = emqttd cuttlefish
dep_emqttd = git https://github.com/emqtt/emqttd master
dep_cuttlefish = git https://github.com/emqtt/cuttlefish

NO_AUTOPATCH = cuttlefish

ERLC_OPTS += +debug_info
ERLC_OPTS += +'{parse_transform, lager_transform}'

TEST_DEPS = emqttc
dep_emqttc = git https://github.com/emqtt/emqttc.git master

TEST_ERLC_OPTS += +debug_info
TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true


include erlang.mk

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emq_modules.conf -i priv/emq_modules.schema -d data
