#!/bin/zsh
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


TEXFILE=$1
DIROFTEXFILE=`dirname $TEXFILE`

RESULTFILE=$2

echo -n "$RESULTFILE: $TEXFILE"

for epsfile in `cat $TEXFILE | sed -e "s/%.*//g" | grep -e "\\\\includegraphics{.\\+}" | sed -e "s/\\(.*\\)\\\\includegraphics{\\([^}]\\+\\)}.*/\\2/"`
do
  FIRSTCHAR=`echo $epsfile | sed -e "s/^\\(.\\).*/\1/"`
  if [[ $FIRSTCHAR == "/" ]] ; then
    echo -n " $epsfile"
  else
    echo -n " $DIROFTEXFILE/$epsfile"
  fi 
done 
echo
