#!/usr/bin/env bash

LUA_FILTER="$HOME/.local/bin/rustdoc-to-org-filter.lua"

function num_cpus {
    rg -c '^$' /proc/cpuinfo
}

function get_toolchain {
    rustup show | sed -nr 's/(.*) \(default\)/\1/p' | head -n 1
}

if [ "$1" = "" ] || [ "$1" = "--help"  ]; then
    MY_NAME="$(basename "$0")"
    echo "Usage:"
    echo "  $MY_NAME <library>"
    echo "  $MY_NAME <docs src> <docs org dst>"
    exit 0
fi

DOC_PATH="$1"
DEST_DIR="$2"

if [ "$DEST_DIR" = "" ]; then
    LIBRARY="$1"
    TARGET="$(get_toolchain)"

    ## Users can change the location of the rustup directory
    RUSTUP_HOME="${RUSTUP_HOME:-$HOME/.rustup}"
    ## Set
    DOC_PATH="$RUSTUP_HOME/toolchains/$TARGET/share/doc/rust/html/$LIBRARY"
    DEST_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/emacs/rust-doc/$LIBRARY"

    echo "Generating org files in: $DEST_DIR"
fi

mkdir -p "$DEST_DIR" || exit 1
cd "$DOC_PATH" || exit 1

## Copy directory structure
fd . -td -x mkdir -p "$DEST_DIR/{}"

## Find redirect files
ignore_file="$(mktemp)"

## This is slightly wonky, but changes all the '/' to '\/' in $DOC_PATH
## and uses that to remove the leading path from $ignore_file entries,
## as fd wants relative paths here.
sed_path=$(echo "$DOC_PATH" | sed 's/\//\\\//g')
rg -l "<p>Redirecting to <a href=\"[^\"]*\"" "$DOC_PATH" | \
    sed -nr "s/^$sed_path\///p" > "$ignore_file"

## Convert files, we use 2 * $(num_cpus), as pandoc seems to be slightly IO-bound
fd . \
    -ehtml \
    --ignore-file "$ignore_file" \
    -j"$(expr 2 \* "$(num_cpus)")" \
    -x pandoc '{}' \
    --lua-filter "$LUA_FILTER" \
    -o "$DEST_DIR/{.}.org"
