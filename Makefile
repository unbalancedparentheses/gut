PROJECT = gut

DEPS = ibrowse jiffy mixer sync getopt
dep_ibrowse = git https://github.com/cmullaparthi/ibrowse.git master
dep_jiffy = git https://github.com/davisp/jiffy.git master
dep_mixer = git https://github.com/opscode/mixer.git master
dep_sync = git https://github.com/rustyio/sync.git master
dep_getopt = git https://github.com/jcomellas/getopt v0.8.2

include erlang.mk

SHELL_OPTS = -name ${PROJECT}@`hostname` -s ${PROJECT}
