#!/usr/bin/env bash

## Get the absolute path of the filter, so that it'll work after chdir
LUA_FILTER="$(realpath filter.lua)"

function num_cpus {
    rg -c '^$' /proc/cpuinfo
}

function get_toolchain {
    rustup show | sed -nr 's/(.*) \(default\)/\1/p' | head -n 1
}

## Handle arguments
LIBRARY="${1:-std}"
TARGET="${2:-$(get_toolchain)}"

## Users can change the location of the rustup directory
RUSTUP_HOME="${RUSTUP_HOME:-$HOME/.rustup}"

DOC_PATH="$RUSTUP_HOME/toolchains/$TARGET/share/doc/rust/html/$LIBRARY"
DEST_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/emacs/rust-doc/$LIBRARY"

mkdir -p "$DEST_DIR" 2>/dev/null
pushd "$DOC_PATH" &>/dev/null

## Copy directory structure
fd . -td -x mkdir -p "$DEST_DIR/{}" 2>/dev/null

## Find redirect files
ignore_file="$(mktemp)"

## This is slightly wonky, but changes all the '/' to '\/' in $DOC_PATH
## and uses that to remove the leading path from $ignore_file entries,
## as fd wants relative paths here.
sed_path=$(echo $DOC_PATH | sed 's/\//\\\//g')
rg -l "<p>Redirecting to <a href=\"[^\"]*\"" "$DOC_PATH" | \
    sed -nr "s/^$sed_path\///p" > "$ignore_file"

## Convert files, we use 2 * $(num_cpus), as pandoc seems to be slightly IO-bound
fd . \
    -ehtml \
    --ignore-file "$ignore_file" \
    -j$(expr 2 \* $(num_cpus)) \
    -x pandoc '{}' \
    --lua-filter "$LUA_FILTER" \
    -o "$DEST_DIR/{.}.org"
