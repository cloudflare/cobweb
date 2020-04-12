#!/usr/bin/env bash

set -e

source ./deps/emsdk/emsdk_env.sh > /dev/null

cobc -C -Wall -o /tmp/out.c -g -x -static -free $1

EM_ARGS=${EM_ARGS:-""}
EM_OUT=${EM_OUT:-"out.js"}

emcc ./cobaul.c \
    -I./dist/include -L./dist/lib \
    -lgmp -lcob \
    -fno-rtti -fno-exceptions \
    -flto \
    --ignore-dynamic-linking \
    -s EXPORTED_FUNCTIONS="['_get_string', '_entry']" \
    -s EXTRA_EXPORTED_RUNTIME_METHODS="['ccall', 'cwrap']" \
    -s EXIT_RUNTIME=1 \
    -s EXPORT_ES6=1 \
    -s INVOKE_RUN=0 \
    -s MODULARIZE=1 \
    -s DEAD_FUNCTIONS="['_main']" \
    -s STRICT=1 \
    $EM_ARGS \
    -o $EM_OUT
