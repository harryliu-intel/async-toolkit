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


function usage() {
  echo "Usage: $0 --root-temp-dir=dir --results=dir"
  echo "          --build-identifier=id"
  echo "          --build-system=dir"
  echo "          --source-roots=dir1,dir2,..."
  
}



sedcmd=`which sed`
gawkcmd=`which gawk`
grepcmd=`which grep`
findcmd=`which find`
check_executable_file "$sedcmd" "Can't find sed in \"$PATH\"."   2
check_executable_file "$gawkcmd" "Can't find gawk in \"$PATH\"." 2
check_executable_file "$grepcmd" "Can't find grep in \"$PATH\"." 2
check_executable_file "$findcmd" "Can't find find in \"$PATH\"." 2


runmakecmd="$package_arch_bin/runmake"
check_executable_file "$runmakecmd" \
    "Run Make Command: \"$runmakecmd\" is not an executable file." 2



root_temp_dir=
results_dir=
build_id=
build_system_root=
source_roots=


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
  --build-identifier=* )
    build_id=`echo $arg | $sedcmd -e "s/--build-identifier=//"`
    ;;
  --build-system=* )
    build_system_root=`echo $arg | $sedcmd -e "s/--build-system=//"`
    ;;
  --source-roots=* )
    source_roots=`echo $arg | $sedcmd -e "s/--source-roots=//"`
    ;;
  esac
done


check_for_empty_arg "$root_temp_dir"        \
    "You must specify a root temporary directory, such as /tmp or /scratch."        2
check_for_empty_arg "$results_dir"          \
    "You must specify a result directory."                                          2
check_for_empty_arg "$build_id"             \
    "You must specify a build identifier."                                          2
check_for_empty_arg "$build_system_root" \
    "You must specify the path to the build system."                                2
check_for_empty_arg "$source_roots"         \
    "You must specifies the root paths that you want to build."                     2


check_writeable_dir "$root_temp_dir"        \
    "Root Temporary Directory: \"$root_temp_dir\" is not a writeable directory."    2
conon_path "$root_temp_dir"
root_temp_dir="$ret"

check_writeable_dir "$results_dir"          \
    "Results Directory: \"$results_dir\" is not a writeable directory."             2
conon_path "$results_dir"
results_dir="$ret"


check_readable_dir "$build_system_root"  \
    "Build System Root: \"$build_system_root\" is not a readable directory." 2
conon_path "$build_system_root"
build_system_root="$ret"


package_dir="$results_dir/packages"

if [[ -d "$package_dir" || -f "$package_dir" ]] ; then
  rm -rf "$package_dir"
fi

mkdir $package_dir

long_name_package_dir="$package_dir/longnames"
mkdir $long_name_package_dir

short_name_package_dir="$package_dir/shortnames"
mkdir $short_name_package_dir

source_roots=`echo $source_roots | $sedcmd -e "s/,/ /g"`
for curr_root in $source_roots ; do
  check_readable_dir "$curr_root" "Source Root: \"$curr_root\" is not a readable directory." 1 
done

curr_root_num=0

log_dir="$results_dir/logs"

for curr_root in $source_roots ; do
  curr_log_file="$log_dir/build-$curr_root_num.log"
  $runmakecmd --log-file=$curr_log_file                                          \
              --build-identifier=$build_id                                       \
              --root-temp-dir=$root_temp_dir                                     \
              --results=$long_name_package_dir                                   \
              --source-root=$curr_root                                           \
              --build-system-root=$build_system_root
  (( curr_root_num=$curr_root_num + 1 ))
done

built_packages=`$findcmd $long_name_package_dir -type f`

escaped_build_id=`echo $build_id | $sedcmd -e "s/\./\\\./g"         \
                                           -e "s/\*/\\\*/g"         \
                                           -e "s/\[/\\\[/g"         \
                                           -e "s/\]/\\\]/g"         \
                                           -e "s/\^/\\\^/g"`

for curr_package in $built_packages ; do
  chmod 444 $curr_package
  base_package_name=`basename $curr_package | $sedcmd -e "s/-$escaped_build_id//"`
  ln $curr_package $short_name_package_dir/$base_package_name
done 





