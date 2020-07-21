#!/bin/bash
if [ $# -lt 2 ]; then
    echo "You need to provide both input and output dir: sh batch_convert <indir> <outdir>"
    exit 1
fi
# The wc condition is to prevent converting or adding files that are just redirects.
mkdir -p $2
for f in $(find $1 -name '*.html')
do
    if  [[ $(wc -l <$f) -ge 11 ]] && [[ -f "$f" ]]; then
        pandoc "$f" --filter rustdoc-to-org-exe -o "$2/$(basename "${f%.*}").org"
    fi
done
