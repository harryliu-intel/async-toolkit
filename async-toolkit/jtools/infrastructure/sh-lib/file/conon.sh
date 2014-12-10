#!/bin/bash
# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$

function conon_path() {
  local the_path="$1"
  if [[ "$the_path" == "." ]] ; then
    ret=`pwd`
  else
    local parent_dir=`dirname "$the_path"`
    local base=`basename "$the_path"`
    pushd "$parent_dir" >/dev/null
    parent_dir=`pwd`
    popd >/dev/null
    if [[ "$parent_dir" == "/" ]] ; then
      ret="/$base"
    else
      ret="$parent_dir/$base"
    fi
  fi
}

