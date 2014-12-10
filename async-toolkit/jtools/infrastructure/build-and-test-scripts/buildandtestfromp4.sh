#!/bin/bash
# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$

arch_bin_dir=${0%\/*}
package_root=${arch_bin_dir%\/*}

package_arch_bin="$package_root/bin"

package_share_sh="$package_root/share/script/sh"

file_util="$package_share_sh/file/filecheck.sh"
conon_util="$package_share_sh/file/conon.sh"
map_depot_to_disk="$package_share_sh/p4/map_depot_to_disk.sh"
client_root_sh="$package_share_sh/p4/clientroot.sh"

if [[ -f "$file_util" && -r "$file_util" ]] ; then
  source "$file_util"
else
  echo "\"$file_util\" is not a readable file."
  exit 2
fi

if [[ -f "$conon_util" && -r "$conon_util" ]] ; then
  source "$conon_util"
else
  echo "\"$conon_util\" is not a readable file."
  exit 2
fi

if [[ -f "$map_depot_to_disk" && -r "$map_depot_to_disk" ]] ; then
  source "$map_depot_to_disk"
else
  echo "\"$map_depot_to_disk\" is not a readable file."
  exit 2
fi

if [[ -f "$client_root_sh" && -r "$client_root_sh" ]] ; then
  source "$client_root_sh"
else
  echo "\"$client_root_sh\" is not a readable file."
  exit 2
fi

buildfromsource="$package_arch_bin/buildfromsource"
check_executable_file "$buildfromsource" \
    "Build From Source: \"$buildfromsource\" is not an executable file." 2

runtestsfromsource="$package_arch_bin/runtestsfromsource"
check_executable_file "$runtestsfromsource" \
    "Run Tests From Source: \"$runtestsfromsource\" is not an executable file." 2

function usage() {
  echo "Usage: $0 --root-temp-dir=dir --results=dir"
  echo "          --client-spec-name=p4client"
  echo "          --build-system=p4_path"
  echo "          --source-roots=p4_path1,p4_path2,..."
  echo "          [ --perforce-sever=perforce ] [ --perforce-port=1666 ]"
  echo "          [ --perforce-client=p4 ] [ --perforce-password=xxx ]" 
  echo "          [ --perforce-user=$USER ] "
}



sedcmd=`which sed`
gawkcmd=`which gawk`
grepcmd=`which grep`
findcmd=`which find`
check_executable_file "$sedcmd" "Can't find sed in \"$PATH\"."   2
check_executable_file "$gawkcmd" "Can't find gawk in \"$PATH\"." 2
check_executable_file "$grepcmd" "Can't find grep in \"$PATH\"." 2
check_executable_file "$findcmd" "Can't find find in \"$PATH\"." 2





root_temp_dir=
results_dir=
client_spec=
build_id=
build_system_p4_root=
source_roots=
p4_server=perforce
p4_port=1666
p4_client=p4
p4_passwd=
p4_user=$USER

for arg in $@ ; do

  case "$arg" in
  --root-temp-dir=* )
    root_temp_dir=`echo $arg | $sedcmd -e "s/--root-temp-dir=//"`
    ;;
  --results=* )
    results_dir=`echo $arg | $sedcmd -e "s/--results=//"`
    ;;
  --client-spec-name=* )
    client_spec=`echo $arg | $sedcmd -e "s/--client-spec-name=//"`
    ;;
  --build-system=* )
    build_system_p4_root=`echo $arg | $sedcmd -e "s/--build-system=//"`
    ;;
  --source-roots=* )
    source_roots=`echo $arg | $sedcmd -e "s/--source-roots=//"`
    ;;
  --perforce-server=* )
    p4_server=`echo $arg | $sedcmd -e "s/--perforce-server=//"`
    ;;
  --perforce-port=* )
    p4_port=`echo $arg | $sedcmd -e "s/--perforce-port=//"`
    ;;
  --perforce-client=* )
    p4_client=`echo $arg | $sedcmd -e "s/--perforce-client=//"`
    ;;
  --perforce-password=* )
    p4_passwd=`echo $arg | $sedcmd -e "s/--perforce-password=//"`
    ;;
  --perforce-user=* )
    p4_user=`echo $arg | $sedcmd -e "s/--perforce-user=//"`
    ;;
  esac
done


check_for_empty_arg "$root_temp_dir"        \
    "You must specify a root temporary directory, such as /tmp or /scratch."        2
check_for_empty_arg "$results_dir"          \
    "You must specify a result directory."                                          2
check_for_empty_arg "$client_spec"          \
    "You must specify the name of the client specification to use."                 2
check_for_empty_arg "$build_system_p4_root" \
    "You must specify the perforce path to the build system."                       2
check_for_empty_arg "$source_roots"         \
    "You must specifies the root perforce paths that you want to build."            2
check_for_empty_arg "$p4_server"            \
    "You must specify the host name of the perforce server."                        2
check_for_empty_arg "$p4_port"              \
    "You must specify the port number of the perforce server."                      2
check_for_empty_arg "$p4_client"            \
    "You must specify the perforce client program to use."                          2
check_for_empty_arg "$p4_user"              \
    "You msut specify a perforce user name."                                        2


check_writeable_dir "$root_temp_dir"        \
    "Root Temporary Directory: \"$root_temp_dir\" is not a writeable directory."    2
conon_path "$root_temp_dir"
root_temp_dir="$ret"

check_writeable_dir "$results_dir"          \
    "Results Directory: \"$results_dir\" is not a writeable directory."             2
conon_path "$results_dir"
results_dir="$ret"


#make build identifier from current date and hostname.
build_date_str=`date +%Y-%m-%d-%H-%M-%S-%Z`
host_name=`hostname`
build_id="$host_name-$build_date_str-official"



real_p4_client=`which $p4_client`

check_executable_file "$real_p4_client"     \
    "Unable to find \"$p4_client\" in \"$PATH\"."                                   2


p4_cmd="$real_p4_client -u $p4_user -p $p4_server:$p4_port"
if [ -n "$p4_passwd" ] ; then
  p4_cmd="$p4_cmd -P $p4_passwd"
fi
p4_cmd_with_client="$p4_cmd -c $client_spec"

p4_get_client_root "$p4_cmd" "$client_spec"
p4_client_root="$ret"

if [ -z "$p4_client_root" ] ; then
  echo "Unable to determine the root directory of the client \"$client_spec\"."
  exit 2
fi

log_dir="$results_dir/logs"
if [[ -d "$log_dir" || -f "$log_dir" ]] ; then
  rm -rf $log_dir
fi
mkdir $log_dir 

$p4_cmd_with_client sync &>$log_dir/sync.log
check_readable_dir "$p4_client_root"    \
    "Client Spec Root Dir: \"$p4_client_root\" does not exist after \"$p4_cmd_with_client sync\"." 2

map_depot_path_to_disk "$p4_cmd" "$client_spec" "$build_system_p4_root"
build_system_root="$ret"

if [ -n "$build_system_root" ] ; then
  
  check_readable_dir "$build_system_root"  \
      "Build System Root: \"$build_system_root\" is not a readable directory." 2
  conon_path "$build_system_root"
  build_system_root="$ret"

  $p4_cmd_with_client client -o &>$log_dir/client.log
  $p4_cmd_with_client have &>$log_dir/src.log

  source_disk_roots=

  source_roots=`echo $source_roots | $sedcmd -e "s/,/ /g"`
  for curr_root in $source_roots ; do

    map_depot_path_to_disk "$p4_cmd" "$client_spec" "$curr_root"
    source_disk_root="$ret"

    conon_path "$source_disk_root"
    source_disk_root="$ret"

    if [ -n "$source_disk_root" ] ; then
      check_readable_dir "$source_disk_root" \
          "Source Root: \"$source_disk_root\" is not a readable directory." 1
      source_disk_roots="$source_disk_roots,$source_disk_root"
    else
      echo "Source Root: \"$curr_root\" does not seem to be in the client specification \"$client_spec\""
      exit 1
    fi
  done

  $buildfromsource --root-temp-dir=$root_temp_dir          \
                   --results=$results_dir                  \
                   --build-identifier=$build_id            \
                   --build-system=$build_system_root       \
                   --source-roots=$source_disk_roots
  $runtestsfromsource --root-temp-dir=$root_temp_dir       \
                      --results=$results_dir               \
                      --build-system=$build_system_root    \
                      --source-roots=$source_disk_roots

else
  echo "Build System Root: \"$build_system_p4_root\" does not seem to be"
  echo "  in the client specification \"$client_spec\"."
  exit 2
fi
