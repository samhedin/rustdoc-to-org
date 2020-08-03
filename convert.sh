#!/usr/bin/env bash

LUA_FILTER=filter.lua

function num_cpus {
    rg -c '^$' /proc/cpuinfo
}

DOC_PATH="$(dirname "$(rustup doc --std --path)")"

fd . \
    -ehtml \
    "$DOC_PATH" \
    -j$(num_cpus) \
    -x pandoc '{}' \
    --lua-filter "$LUA_FILTER" \
    -o '{.}'.org
