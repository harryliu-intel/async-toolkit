#!/bin/sh -x
. builddefs.sh

cp mains/${whichtest}_Main.m3 build/src/Main.m3

cd build/src
cm3 -x
