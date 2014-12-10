#!/bin/bash
# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$


function generate_command_script() {
  local target_dir="$1"
  local source_script="$2"
  local sh_lib_dir="$3"
  local bash_cmd="$4"

  local sh_base_name=`basename $source_script`

  local sh_target="$target_dir/$sh_base_name"

  echo "#!$bash_cmd" >$sh_target

  for libscript in `find $sh_lib_dir -type f -name "*.sh"` ; do
    if [[ -f "$libscript" ]] ; then
      echo "source $libscript" >>$sh_target
    fi
  done

  echo "source $source_script" >>$sh_target

  chmod 700 $sh_target
  ret=$sh_target
}
