#!/bin/sh

erl $@ -pa $PWD/ebin $PWD/deps/*/ebin $PWD/test \
    -eval "application:start(eovsdb)."
