#!/bin/bash
# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$
# UPDATED 2

function usage() {
  echo "Usage: $0 "
  echo "  --dfII-dir=dir"
  echo "  [ --client-spec=client_spec_name ]"
  echo "  [ --view-name=view_name ]"
  echo "  [ --change-list=num ]"
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


client_spec=
dfII_dir=
cell_list=
view_name=
change_list=
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
  --client-spec )
    argname="client_spec"
    ;;
  --client-spec=* )
    client_spec=${arg/--client-spec=}
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
  --change-list )
    argname="change_list"
    ;;
  --change-list=* )
    change_list=${arg/--change-list=}
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
    "You must specify the location of directory containing all the dfII data." 2
check_for_empty_arg "$p4_server_host"             \
    "You must specify a perforce server."                                      2
check_for_empty_arg "$p4_server_port"             \
    "You must specify a perforce server port."                                 2

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
  
  p4_delete_command="$p4_command delete"
  
  if [ -n "$change_list" ] ; then
    p4_delete_command="$p4_delete_command -c $change_list"
  fi
  
  
  cadence_escape_string "$view_name"
  escaped_view="$ret"
  message "escaped_view $escaped_view"
  
  for cell in $cell_list ; do
    message "cell $cell"

    bad_cell_name=`echo $cell | $grepcmd -e "#"`
    if [ -z "$bad_cell_name" ] ; then

      get_escaped_cell_dir "$cell" "$dfII_dir"
      celltargetdir="$ret" 
      message "celltargetdir $celltargetdir"
      if [ -n "$celltargetdir" ] ; then
        
        curr_dir_to_delete="$celltargetdir"

        if [ -n "$view_name" ] ; then
          curr_dir_to_delete="$curr_dir_to_delete/$escaped_view"
        fi

        curr_dir_to_delete=`echo "$curr_dir_to_delete" | sed -e 's/#/%23/g'`

        curr_p4_delete_command="$p4_delete_command $curr_dir_to_delete/..."

        $curr_p4_delete_command
      else
        echo "\"$cell\" is not a valid cell name." 1>&2
      fi
    else
      echo "\"$cell\" is not a valid cell name." 1>&2
    fi
  done

else
  echo "The client specification \"$client_spec\" does not exist." 1>&2
fi
