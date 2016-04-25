PROJECT = eovsdb

DEPS = jsone uuid
dep_jsone_commit = 1.2.3
dep_uuid = git https://github.com/avtobiff/erlang-uuid.git v0.5.0

EUNIT_OPTS += verbose
EUNIT_COMPILE_OPTS := +'{src_dirs, ["test"]}'

include erlang.mk
