#!/usr/bin/env bash

set -xe

cd $(dirname $(realpath $0))

export TERM=dumb

find . -name _build | xargs rm -rf

shelltest -c --diff --all --debug --execdir */*.test
