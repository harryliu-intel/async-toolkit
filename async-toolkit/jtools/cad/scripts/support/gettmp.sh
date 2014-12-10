#!/bin/bash
if [[ -n "$TMP" && -d "$TMP" && -w "$TMP" ]]; then echo "$TMP"; exit 0; fi
for f in /scratch/*.q; do
if [[ -w "$f" ]]; then TMP=$f; fi;
done
if [[ -z "$TMP" ]]; then TMP=/scratch; fi
echo $TMP
