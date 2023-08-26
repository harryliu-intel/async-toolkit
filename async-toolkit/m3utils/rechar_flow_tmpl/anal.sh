#!/bin/sh

for c in `cat missing.lst`; do
    grep $c cell_list
done
