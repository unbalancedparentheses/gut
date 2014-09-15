PROJECT = gutenberg

DEPS = lager ibrowse jiffy mixer sync
dep_lager = git https://github.com/basho/lager.git master
dep_ibrowse = git https://github.com/cmullaparthi/ibrowse.git master
dep_jiffy = git https://github.com/davisp/jiffy.git master
dep_mixer = git https://github.com/opscode/mixer.git master
dep_sync = git https://github.com/rustyio/sync.git master

include erlang.mk

ERLC_OPTS += +'{parse_transform, lager_transform}'

SHELL_OPTS = -name ${PROJECT}@`hostname` -s ${PROJECT}
