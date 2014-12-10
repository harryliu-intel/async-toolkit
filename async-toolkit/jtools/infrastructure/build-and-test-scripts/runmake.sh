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
  echo "Usage: $0 --log-file=file --root-temp-dir=dir --results=dir"
  echo "   --source-root=dir --build-system-root=dir"
}


sedcmd=`which sed`
makecmd=`which gmake`
check_executable_file "$sedcmd" "Can't find sed in \"$PATH\"."   2
check_executable_file "$makecmd" "Can't find make in \"$PATH\"." 2

build_id=
root_temp_dir=
results_dir=
source_root=
build_system=
for arg in $@ ; do

  case "$arg" in
  --build-identifier=* )
    build_id=`echo $arg | $sedcmd -e "s/--build-identifier=//"`
    ;;
  --log-file=* )
    log_file=`echo $arg | $sedcmd -e "s/--log-file=//"`
    ;;
  --root-temp-dir=* )
    root_temp_dir=`echo $arg | $sedcmd -e "s/--root-temp-dir=//"`
    ;;
  --results=* )
    results_dir=`echo $arg | $sedcmd -e "s/--results=//"`
    ;;
  --source-root=* )
    source_root=`echo $arg | $sedcmd -e "s/--source-root=//"`
    ;;
  --build-system-root=* )
    build_system_root=`echo $arg | $sedcmd -e "s/--build-system-root=//"`
    ;;
  esac
done

check_for_empty_arg "$build_id"          \
    "You must specify a build identifier."                                          2
check_for_empty_arg "$log_file"          \
    "You must specify the name of a log file to be created by this script."         2
check_for_empty_arg "$root_temp_dir"     \
    "You must specify a root temporary directory, such as /tmp or /scratch."        2
check_for_empty_arg "$results_dir"       \
    "You must specify a result directory."                                          2
check_for_empty_arg "$source_root"       \
    "You must specify the location of the source code."                             2
check_for_empty_arg "$build_system_root" \
    "You must specify the location of the build system."                            2
  

log_file_dir=`dirname $log_file`
check_writeable_dir "$log_file_dir"      \
   "Log file: \"$log_file_dir\" is not a writeable directory."                      2

check_writeable_dir "$root_temp_dir"     \
   "Root Temporary Directory: \"$root_temp_dir\" is not a writeable directory,"     2
conon_path "$root_temp_dir"
root_temp_dir="$ret"


check_writeable_dir "$results_dir"       \
   "Results Directory: \"$results_dir\" is not a writeable directory."              2
conon_path "$results_dir"
results_dir="$ret"

check_readable_dir  "$source_root"       \
   "Source Code Root: \"$source_root\" is not a readable directory."                2
conon_path "$source_root"
source_root="$ret"


check_readable_dir  "$build_system_root" \
   "Build System: \"$build_system_root\" is not a readable directory."              2
conon_path "$build_system_root"
build_system_root="$ret"


make_file="$build_system_root/Makefile"

check_readable_file "$make_file"         \
   "Build System: \"$make_file\" is not a readable file."                           2

target_dir=`mktemp -d $root_temp_dir/runmake.XXXXXX`

check_writeable_dir "$target_dir"        \
   "Target Directory: \"$target_dir\" is not a writeable directory."                2


finalcmd="$makecmd -f $make_file 
                      \"BUILD_SYSTEM_ROOT=$build_system_root\" 
                      \"ROOT_PROJECT_DIR=$source_root\" 
                      \"ROOT_TARGET_DIR=$target_dir\" 
                      \"FULCRUM_NO_UTIL_MAKEFILES=1\"
                      \"FULCRUM_RESULTS_DIR=$results_dir\" 
                      \"FULCRUM_BUILD_ID=$build_id\" allpackages"


echo $finalcmd >$log_file
echo "" >>$log_file
echo "" >>$log_file

tmp_log_file=`mktemp $root_temp_dir/runmake.XXXXXX`

#$makecmd -f $make_file "BUILD_SYSTEM_ROOT=$build_system_root" "ROOT_PROJECT_DIR=$source_root" "ROOT_TARGET_DIR=$target_dir" "FULCRUM_NO_UTIL_MAKEFILES=1" "FULCRUM_RESULTS_DIR=$results_dir" "FULCRUM_BUILD_ID=$build_id" allpackages &>$tmp_log_file

eval $finalcmd &>$tmp_log_file

cat $tmp_log_file >>$log_file

rm $tmp_log_file

exit_code=$?

rm -rf $target_dir

exit $exit_code




