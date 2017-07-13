PROJECT = emq_modules
PROJECT_DESCRIPTION = EMQ Modules
PROJECT_VERSION = 2.2

DEPS = ecpool brod gpb supervisor3 kafka_protocol snappyer
dep_ecpool = git https://github.com/emqtt/ecpool master
dep_brod = git https://github.com/klarna/brod 2.5.0
dep_gpb = git https://github.com/vleushin/gpb 3.26.6-vsn
dep_supervisor3 = git https://github.com/klarna/supervisor3 1.1.5
dep_kafka_protocol = git https://github.com/klarna/kafka_protocol 0.9.2
dep_snappyer = git https://github.com/zmstone/snappyer master

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
