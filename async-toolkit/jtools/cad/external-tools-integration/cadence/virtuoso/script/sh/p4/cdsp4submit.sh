#!/bin/bash
# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$
# UPDATED

export LD_LIBRARY_PATH=
function usage() {
  echo "Usage: $0 "
  echo "  [ --client-spec=client_spec_name ]"
  echo "  [ --change-list=num ]"
  exit 2
  
}


arch_bin_dir=${0%\/*}
package_root=${arch_bin_dir%\/*}
sh_lib_dir="$package_root/share/script/sh/sh-lib"

arch_bin_dir="$package_root/bin"

p4_executable=`which p4`

source "$sh_lib_dir/file/filecheck.sh"
source "$sh_lib_dir/file/conon.sh"
source "$sh_lib_dir/script/generate_script_with_libs.sh"


check_executable_file "$p4_executable" "p4: \"$p4_executable\" is not an exectable file." 2


sedcmd=`which sed`
gawkcmd=`which gawk`
grepcmd=`which grep`
check_executable_file "$sedcmd" "Unable to find sed in \"$PATH\"" 2
check_executable_file "$gawkcmd" "Can't find gawk in \"$PATH\"." 2
check_executable_file "$grepcmd" "Can't find grep in \"$PATH\"." 2


client_spec=
change_list=
verbose=
p4_passwd=
argname=

for arg in $@ ; do
  
  case "$arg" in
  --* )
    argname=
    ;;
  esac
  case "$arg" in
  --verbose )
    verbose=t
    ;;
  --client-spec )
    argname="client_spec"
    ;;
  --client-spec=* )
    client_spec=`echo $arg | $sedcmd -e "s/--client-spec=//"`
    ;;
  --change-list )
    argname="change_list"
    ;;
  --change-list=* )
    change_list=`echo $arg | $sedcmd -e "s/--change-list=//"`
    ;;
  --perforce-password )
    argname="p4_passwd"
    ;;
  --perforce-password=* )
    p4_passwd=`echo $arg | $sedcmd -e "s/--perforce-password=//"`
    ;;
  --* )
    echo "Unknown argument: \"$arg\"."
    usage
    ;;
  * )
    if [ -n "$argname" ]; then
        eval "$argname=$arg";
    else
        usage
    fi
    argname=
  esac
done

p4_command="$p4_executable"

if [ -n "$p4_passwd" ]; then
  p4_command="$p4_command -P $p4_passwd"
fi

if [ -z "$client_spec" ] ; then
  client_spec=`$p4_command client -o | grep -e "^Client:" | sed -e "s/Client:[[:space:]]\+//"`
fi

client_spec_exists=`$p4_command clients | \
        $grepcmd -e "^Client[[:space:]]\+$client_spec[[:space:]]*"`
if [ -z "$client_spec_exists" ] ; then
client_spec_exists=`$p4_cmd_with_passwd clients | \
        $grepcmd -e "^\.\.\.[[:space:]]\+client[[:space:]]\+$client_spec[[:space:]]*\$"`
fi

if [ -n "$client_spec_exists" ] ; then


  p4_command="$p4_command -c $client_spec"
  p4_submit_command="$p4_command submit"

  if [ -n "$change_list" ] ; then
    p4_submit_command="$p4_submit_command -c $change_list"
  fi

  $p4_submit_command

else
  echo "The client specification \"$client_spec\" does not exist."
fi
