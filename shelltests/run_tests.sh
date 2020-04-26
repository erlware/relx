#!/usr/bin/env bash

set -xe

cd $(dirname $(realpath $0))

export TERM=dumb

find . -name _build | xargs rm -rf

current_dir=$(pwd)
rebar3_dir=$(mktemp -d)

pushd "${rebar3_dir}"

# clone latest rebar3 and build with relx as a checkout
git clone https://github.com/erlang/rebar3 .
mkdir _checkouts
ln -s "$current_dir/../../relx" _checkouts/relx
sed -i 's_relx\(.*\)build/default/lib/_relx\1checkouts_' rebar.config
./bootstrap

popd

PATH="${rebar3_dir}":~/.cabal/bin/:$PATH shelltest -c --diff --all --debug --execdir -- */*.test
