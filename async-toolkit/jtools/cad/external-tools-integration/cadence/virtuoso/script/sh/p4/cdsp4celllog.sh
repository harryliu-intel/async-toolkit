#!/bin/bash
# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$
# UPDATED 2

function usage() {
  echo "Usage: $0 "
  echo "  [ --client-spec=client_spec_name ]"
  echo "  --dfII-dir=dir --view-name=view_name"
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
egrepcmd=`which egrep`
check_executable_file "$sedcmd" "Unable to find sed in \"$PATH\"" 2
check_executable_file "$gawkcmd" "Can't find gawk in \"$PATH\"." 2
check_executable_file "$egrepcmd" "Can't find grep in \"$PATH\"." 2

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

check_for_empty_arg "$dfII_dir" \
    "You must specify the location of directory containing all the dfII data." 2

check_for_empty_arg "$view_name" \
    "You must specify the view name of the cells you want to see the log for." 2


check_writeable_dir "$dfII_dir"  \
    "dfII directory: \"$dfII_dir\" is not a readable, writeable directory." 2
conon_path "$dfII_dir"
dfII_dir="$ret"
cd "$dfII_dir"
if [ -e "OA" ]; then oa=1; fi

p4_cmd="$p4_executable"

if [ -z "$client_spec" ] ; then
  client_spec=`$p4_executable client -o |\
    $gawkcmd '/^Client:/ {print $2}'`
fi 
message "client_spec $client_spec"

qu='"'
dl='$'
client_spec_exists=`$p4_executable clients | \
    $gawkcmd "${dl}2 == \"${client_spec}\" {print ${dl}2}"`
message "client_spec_exists $client_spec_exists"

if [ -n "$client_spec_exists" ] ; then

  cadence_escape_string "$view_name"
  escaped_view="$ret"
  message "escaped_view $escaped_view"

  p4_command="$p4_executable -c $client_spec"

  p4_log_command="$p4_command filelog"

  for cell in $cell_list ; do
    message "cell $cell"
    bad_cell_name=`echo $cell | grep -e "#"`
    if [ -z "$bad_cell_name" ] ; then

      get_escaped_cell_dir "$cell" "$dfII_dir"
      celltargetdir="$ret"
      message "celltargetdir $celltargetdir"
      
      if [ -n "$celltargetdir" ] ; then
        if [ "$oa" == "1" ]; then
            curr_file_to_log="$celltargetdir/$escaped_view/layout.oa"
        else
            curr_file_to_log="$celltargetdir/$escaped_view/layout.cdb"
        fi
       
        curr_file_to_log=`echo "$curr_file_to_log" | sed -e 's/#/%23/g'`
        curr_p4_log_command="$p4_log_command $curr_file_to_log"

        $curr_p4_log_command | \
          $egrepcmd '...[[:space:]]+(change)|(desc)' | \
          $sedcmd -e 's/.*#[0-9]* change /change /' \
                  -e 's/...[[:space:]]+desc[0-9]*/descripion:/'
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
