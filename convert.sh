#!/usr/bin/env bash

rustup doc

LUA_FILTER=filter.lua

if [ "$RUSTUP_HOME" = "" ]; then
   RUSTUP_HOME="$HOME/.rustup"
fi

function num_cpus {
    rg -c '^$' /proc/cpuinfo
}

function get_toolchain {
    rustup show | gawk 'match($0, /^(.*) \(default\)$/, a) { print a[1]; exit }'
}

fd . \
    -ehtml \
    "$RUSTUP_HOME/toolchains/$(get_toolchain)/share/doc/rust/html" \
    -j$(num_cpus) \
    -x pandoc '{}' \
    --lua-filter "$LUA_FILTER" \
    -o '{.}'.org
