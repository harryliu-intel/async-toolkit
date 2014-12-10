#!/bin/bash
# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$
# UPDATED 2

function usage() {
  echo "Usage: $0 "
  echo "  --dfII-dir=dir"
  echo "  ( --file=filename |"
  echo "    --view-name=view_name"
  echo "    [ --file=filename ] )"
  echo "  [ --client-spec=client_spec_name ]"
  echo "  [ --change-list=num ]"
  echo "  [ --no-sync ]"
  echo "  [ --perforce-user=user_name ]"
  echo "  [ --perforce-password=xxx ]"
  echo "  [ --perforce-server=xxx ]"
  echo "  [ --perforce-port=xxx ]"
  echo "  cell1 [ cell2 [ ... ] ]"
  
}

verbose=
oa=1

function message() {
    if [ -n "$verbose" ]; then
      echo "$@"
    fi
}

# assumes run under fulcrum script
arch_bin_dir=${0%\/*}
package_root=${arch_bin_dir%\/*}
p4_executable=`which p4`
sh_lib_dir="$package_root/share/script/sh/sh-lib"

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

cds_sh_lib="$package_root/share/script/sh/util"
check_readable_dir "$cds_sh_lib" \
    "Cadence Shell Script Library: \"$cds_sh_lib\" is not a readable directory." 2
cds_sh_lib_files=`find "$cds_sh_lib" \! -type d`

for file in $cds_sh_lib_files ; do
  source "$file"
done


dfII_dir=
view_name=
file_name=
client_spec=
change_list=
cell_list=
no_sync=
p4_user=
p4_passwd=
p4_server_host=ssl:p4proxy07.devtools.intel.com
p4_server_port=2510
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
  --dfII-dir )
    argname="dfII_dir"
    ;;
  --dfII-dir=* )
    dfII_dir=${arg/--dfII-dir=}
    ;;
  --view-name )
    argname="view_name"
    ;;
  --view-name=* )
    view_name=${arg/--view-name=}
    ;;
  --file )
    argname="file_name"
    ;;
  --file=* )
    file_name=${arg/--file=}
    ;;
  --client-spec )
    argname="client_spec"
    ;;
  --client-spec=* )
    client_spec=${arg/--client-spec=}
    ;;
  --change-list )
    argname="change_list"
    ;;
  --change-list=* )
    change_list=${arg/--change-list=}
    ;;
  --no-sync )
    no_sync=1
    ;;
  --perforce-user )
    argname="p4_user"
    ;;
  --perforce-user=* )
    p4_user=${arg/--perforce-user=}
    ;;
  --perforce-password )
    argname="p4_passwd"
    ;;
  --perforce-password=* )
    p4_passwd=${arg/--perforce-password=}
    ;;
  --perforce-server )
    argname="p4_server_host"
    ;;
  --perforce-server=* )
    p4_server_host=${arg/--perforce-server=}
    ;;
  --perforce-port )
    argname="p4_server_port"
    ;;
  --perforce-port=* )
    p4_server_port=${arg/--perforce-port=}
    ;;
  --* )
    echo "Unknown argument: \"$arg\"."
    usage
    exit 2
    ;;
  * )
    if [ -n "$argname" ]; then
        eval "$argname=$arg"
    else
        cell_list="$cell_list $arg"
    fi
    argname=
    ;;
  esac
done

check_for_empty_arg "$dfII_dir"                   \
    "You must specify the location of directory containing all the dfII data."         2


if [ -z "$file_name" ] ; then
  check_for_empty_arg "$view_name"                \
      "You must specify the view name or file that you want to edit for each cell."    2
fi
check_for_empty_arg "$p4_server_host"             \
    "You must specify a perforce server."                                              2
check_for_empty_arg "$p4_server_port"             \
    "You must specify a perforce server port."                                         2



check_writeable_dir "$dfII_dir"  \
    "dfII directory: \"$dfII_dir\" is not a readable, writeable directory." 2
conon_path "$dfII_dir"
dfII_dir="$ret"

# cd into dfII dir so AltRoots work
cd "$dfII_dir"
if [ -e "OA" ]; then oa=1; fi

p4_executable="$p4_executable -p $p4_server_host:$p4_server_port";
p4_cmd="$p4_executable"

if [ -n "$p4_user" ] ; then
  p4_cmd="$p4_cmd -u $p4_user"
fi

if [ -n "$p4_passwd" ] ; then
  p4_cmd="$p4_cmd -P $p4_passwd"
fi

if [ -z "$client_spec" ] ; then
  client_spec=`$p4_cmd client -o |\
    $gawkcmd '/^Client:/ {print $2}'`
fi 
message "client_spec $client_spec"

qu='"'
dl='$'
client_spec_exists=`$p4_cmd clients | \
    $gawkcmd "${dl}2 == \"${client_spec}\" {print ${dl}2}"`
message "client_spec_exists $client_spec_exists"

if [ -n "$client_spec_exists" ] ; then

  p4_command="$p4_cmd -c $client_spec"
  
  p4_sync_command="$p4_command sync"
  p4_edit_command="$p4_command edit"
  p4_lock_command="$p4_command lock"

  cadence_escape_string "$view_name"
  escaped_view="$ret"
  message "escaped_view $escaped_view"

  if [ -n "$change_list" ] ; then
    p4_edit_command="$p4_edit_command -c $change_list"
  fi

  for cell in $cell_list ; do

    message "cell $cell"
    get_escaped_cell_dir "$cell" "$dfII_dir"
    celltargetdir="$ret"
    message "celltargetdir $celltargetdir"
    
    if [ -n "$celltargetdir" ] ; then
      path_to_edit="$celltargetdir"
 
      if [ -n "$view_name" ] ; then
        path_to_edit="$path_to_edit/$escaped_view"
      fi

      if [ -n "$file_name" ] ; then
        path_to_edit="$path_to_edit/$file_name"
      fi
      message "path_to_edit $path_to_edit"

      if [[ -r "$path_to_edit" && -w "$path_to_edit" && -d "$path_to_edit" ]] ; then
        path_to_edit="$path_to_edit/..."
      else 
        if [[ ! ( -r "$path_to_edit" && -f "$path_to_edit" ) ]] ; then
          echo "ERROR: \"$path_to_edit\" is not a file or directory."
          path_to_edit=
        fi
      fi

      path_to_edit=`echo "$path_to_edit" | sed -e 's/#/%23/g'`

      if [ -n "$path_to_edit" ] ; then
        curr_p4_sync_command="$p4_sync_command $path_to_edit"

        curr_p4_edit_command="$p4_edit_command $path_to_edit"

        curr_p4_lock_command="$p4_lock_command $path_to_edit"

        if [[ "$no_sync" != "1" ]] ; then
          $curr_p4_sync_command &>/dev/null
        fi

        $curr_p4_edit_command
      fi
    else
      echo "\"$cell\" is not a valid cell name." 1>&2
    fi
  done
else
  echo "The client specification \"$client_spec\" does not exist." 1>&2
fi
