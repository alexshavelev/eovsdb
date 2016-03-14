#!/bin/sh

erl $@ -pa $PWD/ebin $PWD/deps/*/ebin $PWD/test \
    -eval "application:start(eovsdb),\
          eovsdb_client:connect(localhost, 6632)."
