#!/usr/bin/env bash

set -eoxu pipefail

rm -fr ext/LEMS
rm -fr ext/NeuroML2

git submodule update --init

cd ext/LEMS
git apply ../../lems.patch
cd ../..

cd ext/NeuroML2
git apply ../../nml.patch
cd ../..

cargo run --bin schema
