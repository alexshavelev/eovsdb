#!/bin/sh

erl $@ -pa $PWD/ebin $PWD/deps/*/ebin $PWD/test \
    -eval "application:start(eovsdb),\
          Pid = eovsdb_client:connect(\"localhost:6632\", [<<\"Open_vSwitch\">>])."
