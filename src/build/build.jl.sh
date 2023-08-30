#! /usr/bin/env bash

set -xe

FILE="file.jl"
# https://stackoverflow.com/a/246128/12069968
PDIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)

julia --project="$PDIR" "$FILE"

