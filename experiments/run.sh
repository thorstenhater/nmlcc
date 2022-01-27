#!/usr/bin/env bash

set -xuo pipefail

python3 -c 'import arbor as A; print(A.__config__)'

for e in jnml nmlcc hh hand hand-cf hand-cf-sm
do
    echo $e
    cd $e
    python3 main.py
    cd ..
done
