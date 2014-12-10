#!/bin/bash
# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$


function check_executable_file() {
  local thefile="$1"
  local errorstr="$2"
  local exitcode="$3"

  if [[ ! ( -f "$thefile" && -r "$thefile" && -x "$thefile" ) ]] ; then
    echo "$errorstr"
    exit "$exitcode"
  fi
}

function check_readable_file() {
  local thefile="$1"
  local errorstr="$2"
  local exitcode="$3"

  if [[ ! ( -f "$thefile" && -r "$thefile" ) ]] ; then
    echo "$errorstr"
    exit "$exitcode"
  fi
}

function check_writeable_file() {
  local thefile="$1"
  local errorstr="$2"
  local exitcode="$3"
  local dir=`dirname "$thefile"`


  if [[ ( ! ( -f "$thefile" && -w "$thefile" ) ) &&
        ( ( ! ( -f "$thefile" ) ) && ( ! ( -d "$dir" && -w "$dir" && -x "$dir" ) ) ) ]] ; then
    echo "$errorstr"
    exit "$exitcode"
  fi
}

function check_readwriteable_file() {
  local thefile="$1"
  local errorstr="$2"
  local exitcode="$3"

  if [[ ! ( -f "$thefile" && -w "$thefile" && -r "$thefile" ) ]] ; then
    echo "$errorstr"
    exit "$exitcode"
  fi
}

function check_readable_dir() {
  local thedir="$1"
  local errorstr="$2"
  local exitcode="$3"

  if [[ ! ( -d "$thedir" && -r "$thedir" && -x "$thedir" ) ]] ; then
    echo "$errorstr"
    exit "$exitcode"
  fi
}

function check_writeable_dir() {
  local thedir="$1"
  local errorstr="$2"
  local exitcode="$3"

  if [[ ! ( -d "$thedir" && -r "$thedir" && -x "$thedir" && -w "$thedir" ) ]] ; then
    echo "$errorstr"
    exit "$exitcode"
  fi
}

function check_for_empty_arg() {
  local argvalue="$1"
  local errorstr="$2"
  local exitcode="$3"
  
  if [ -z "$argvalue" ] ; then
    echo "$errorstr"
    usage
    exit "$exitcode"
  fi
}

function mywhich {
    local ret temp pth
    pth="$IFS"
    IFS=':'
    temp=($PATH)
    IFS=$pth
    # avoids problems with aliases
    ret=
    for pth in "${temp[@]}" ; do
        [[ -z "$pth" ]] && pth='.'
        ret="$pth/$1"
        [[ -f "$ret" && -x "$ret" ]] && break
        ret=
    done
    if [[ -z "$ret" ]]; then
        echo "Unable to find $1 executable in \"$PATH\""
        exit 2
    fi
    echo $ret
    exit 0
}

function myexit() {
  local errorstr="$1"
  local exitcode="$2"
  echo "$errorstr"
  exit "$exitcode"
}
