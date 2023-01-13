#!/usr/bin/env bash

set -euxo pipefail

cmake -S . -B build
cmake --build build
cp build/main .
arbor-build-catalogue local cat
./main $*
