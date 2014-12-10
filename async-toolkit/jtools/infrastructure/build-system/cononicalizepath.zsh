#!/bin/zsh

DIRNAME=`dirname "$1"`

PUSHD_SILENT=1 pushd "$DIRNAME"

CONONICALDIR=`pwd`
BASENAME=`basename "$1"`
echo $CONONICALDIR/$BASENAME

PUSHD_SILENT=1 popd
