#! /usr/bin/env bash

set -xe

# Set target to `debug` or `release`
MODE="debug"

FLAG=""
if [[ $MODE != "debug" ]]; then
    FLAG="--$TARGET"
fi

cargo build --$FLAG "$@"
D="$(basename "$PWD")"
F="${D%%.*}"
strip ./target/$MODE/$F
cp -f ./target/$MODE/$F ./

