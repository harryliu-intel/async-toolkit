#!/bin/bash
# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$
# UPDATED 2

function usage() {
  echo "Usage: $0 "
  echo "  --dfII-dir=dir"
  echo "  --force"
  echo "  [ --client-spec=client_spec_name ]"
  echo "  [ --perforce-password=xxx ]"
  echo "  [ --perforce-port=xxx ]"
  echo "  [ --perforce-server=xxx ]"
  echo "  [ --perforce-user=user_name ]"
  echo "  [ --rev=rev-spec]"
  echo "  [ --view-name=view_name ]"
  echo "  [ --verbose ]"
  echo "  [ --fast ]"
  echo "  [ --nosync ]"
  echo "  [ cell1 [ cell2 [ ... ] ] ]"
  exit 2
}

verbose=
fast=
oa=1

function message() {
    if [ -n "$verbose" ]; then
      echo "$@"
    fi
}


temp_cds_lib_generated=

function exit_func() {
    if [ -e "$temp_cds_lib_generated" ]; then
        /bin/rm -f "$temp_cds_lib_generated";
    fi
}

trap exit_func EXIT;

# assumes run under fulcrum script
arch_bin_dir=${0%\/*}
package_root=${arch_bin_dir%\/*}
p4_executable=`which p4`
sh_lib_dir="$package_root/share/script/sh/sh-lib"
if [ ! -r "$sh_lib_dir" -o ! -d "$sh_lib_dir" ]; then
    echo "Script Library: \"$sh_lib_dir\" is not a readable directory."
    exit 2
fi

source "$sh_lib_dir/file/filecheck.sh"
source "$sh_lib_dir/file/conon.sh"
source "$sh_lib_dir/script/generate_script_with_libs.sh"

check_executable_file "$p4_executable" "p4: \"$p4_executable\" is not an exectable file." 2

sedcmd=`which sed`
gawkcmd=`which gawk`
grepcmd=`which grep`
findcmd=`which find`

check_executable_file "$sedcmd" "Unable to find sed in \"$PATH\"" 2
check_executable_file "$gawkcmd" "Can't find gawk in \"$PATH\"." 2
check_executable_file "$grepcmd" "Can't find grep in \"$PATH\"." 2
check_executable_file "$findcmd" "Can't find find in \"$PATH\"." 2

cds_sh_lib="$package_root/share/script/sh/util"
check_readable_dir "$cds_sh_lib" \
    "Cadence Shell Script Library: \"$cds_sh_lib\" is not a readable directory." 2
cds_sh_lib_files=`find "$cds_sh_lib" \! -type d`

for file in $cds_sh_lib_files ; do
  source "$file"
done

client_spec=
rev=
dfII_dir=
cell_list=
view_name=
change_list=
p4_user=
p4_passwd=
if [ -n "$P4PORT" ]; then
p4_server_host=`echo $P4PORT | $gawkcmd -F':' '{print $1":"$2}'`
p4_server_port=`echo $P4PORT | $gawkcmd -F':' '{print $3}'`
else
p4_server_host=ssl:p4proxy07.devtools.intel.com
p4_server_port=2510
fi
sync_force=
nosync=
argname=

for arg in $@ ; do
  
  case "$arg" in
  --* )
    argname=
    ;;
  esac
  case "$arg" in
  --nosync )
    nosync=1
    ;; 
  --fast )
    fast=1
    ;;
  --verbose )
    verbose=t
    ;;
  --force )
    sync_force="-f"
    ;;
  --client-spec )
    argname="client_spec"
    ;;
  --client-spec=* )
    client_spec=`echo $arg | $sedcmd -e "s/--client-spec=//"`
    ;;
  --dfII-dir )
    argname="dfII_dir"
    ;;
  --dfII-dir=* )
    dfII_dir=`echo $arg | $sedcmd -e "s/--dfII-dir=//"`
    ;;
  --view-name )
    argname="view_name"
    ;;
  --view-name=* )
    view_name=`echo $arg | $sedcmd -e "s/--view-name=//"`
    ;; 
  --rev )
    argname="rev"
    ;;
  --rev=* )
    rev=`echo $arg | $sedcmd -e "s/--rev=//"`
    ;;
  --perforce-user )
    argname="p4_user"
    ;;
  --perforce-user=* )
    p4_user=`echo $arg | $sedcmd -e "s/--perforce-user=//"`
    ;;
  --perforce-password )
    argname="p4_passwd"
    ;;
  --perforce-password=* )
    p4_passwd=`echo $arg | $sedcmd -e "s/--perforce-password=//"`
    ;;
  --perforce-server )
    argname="p4_server_host"
    ;;
  --perforce-server=* )
    p4_server_host=`echo $arg | $sedcmd -e "s/--perforce-server=//"`
    ;;
  --perforce-port )
    argname="p4_server_port"
    ;;
  --perforce-port=* )
    p4_server_port=`echo $arg | $sedcmd -e "s/--perforce-port=//"`
    ;;
  --* )
    echo "Unknown argument: \"$arg\"."
    usage
    exit 2
    ;;
  * )
    if [ -n "$argname" ]; then
        eval "$argname=$arg";
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


mkdir -p "$dfII_dir"  # because you may not have done p4 sync yet!
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

  p4_sync_command="$p4_command sync $sync_force"

  if [ -n "$cell_list" ] ; then

    cadence_escape_string "$view_name"
    escaped_view="$ret"

    for rawcell in $cell_list ; do
      bad_cell_name=`echo $rawcell | $grepcmd -e "#"`
      if [ -z "$bad_cell_name" ] ; then

        atstr=`echo $rawcell | $grepcmd -e "@" | $sedcmd -e "s/^.\+@//"`
        cell=`echo $rawcell | $sedcmd -e "s/@.*\$//"`
        
        get_escaped_cell_dir "$cell" "$dfII_dir"
        celltargetdir="$ret"

        if [ -n "$celltargetdir" ] ; then
          
          curr_dir_to_sync="$celltargetdir"
          
          if [ -n "$view_name" ] ; then
            curr_dir_to_sync="$curr_dir_to_sync/$escaped_view"
          fi
          
          curr_dir_to_sync=`echo "$curr_dir_to_sync" | sed -e 's/#/%23/g'`
          curr_p4_sync_command="$p4_sync_command $curr_dir_to_sync/..."
          if [ -n "$verbose" ]; then
              echo "$curr_p4_sync_command" 1>&2
          fi
          
          if [ -n "$atstr" ] ; then
            curr_p4_sync_command="${curr_p4_sync_command}@${atstr}"
          fi

          $curr_p4_sync_command
        else
          echo "\"$cell\" is not a valid cell name."
        fi
      else
        echo "\"$rawcell\" is not a valid cell name."
      fi
    done
  elif [ -z "$nosync" ]; then
     $p4_sync_command //..."$rev"
  fi

  if [ -n "$fast" ]; then
      # this can save a LOT of time but makes assumptions that may not remain true
      all_cds_info_files=`$findcmd $dfII_dir -follow \\( -name '*#*' \\) -type d -prune -o \\( -type f -name cdsinfo.tag -print \\)`
  else
      all_cds_info_files=`$findcmd $dfII_dir -follow  -type f -name "cdsinfo.tag"`
  fi

  lib_dirs=

  for cds_info in $all_cds_info_files ; do
    cds_info_dir=`dirname $cds_info`
    lib_dirs="$lib_dirs $cds_info_dir"
  done

  cds_lib_generated="$dfII_dir/cds.lib.generated"

  temp_cds_lib_generated=`mktemp "$dfII_dir/cds.lib.generated.XXXXXX"`
  rm -f "$temp_cds_lib_generated"

  escapedDFIIDir=`echo "$dfII_dir" | $sedcmd -e "s/\//\\\\\\\\\\//g"`
  for libdir in $lib_dirs ; do
    get_lib_name_for_dir "$libdir" "$dfII_dir"
    libname="$ret"
    if [ -n "$libname" ] ; then
      cadence_escape_string "$libname"
      escaped_lib_name="$ret"
      relativeLibDir=`echo "$libdir" | $sedcmd -e "s/$escapedDFIIDir\///g"`
      echo "DEFINE $escaped_lib_name $relativeLibDir" >>$temp_cds_lib_generated
    fi
  done
  /bin/mv -f "$temp_cds_lib_generated" "$cds_lib_generated"
  
else
  echo "The client specification \"$client_spec\" does not exist."
fi
