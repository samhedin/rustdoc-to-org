#!/bin/bash
basedir="/home/sam/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/share/doc/rust/html/std/*"
outdir="/home/sam/.emacs.d/private/rustdoc/"
mkdir outdir
for f in $basedir
do
    if [[ -f "$f" ]]; then
        pandoc -f html "$f" -t org --filter rustdoc-to-org-exe -o "$outdir/$(basename "${f%.*}").org"
    fi
done
