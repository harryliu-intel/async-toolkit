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
client_root_sh="$package_share_sh/p4/clientroot.sh"
map_depot_to_disk="$package_share_sh/p4/map_depot_to_disk.sh"

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

if [[ -f "$client_root_sh" && -r "$client_root_sh" ]] ; then
  source "$client_root_sh"
else
  echo "\"$client_root_sh\" is not a readable file."
  exit 2
fi

if [[ -f "$map_depot_to_disk" && -r "$map_depot_to_disk" ]] ; then
  source "$map_depot_to_disk"
else
  echo "\"$map_depot_to_disk\" is not a readable file."
  exit 2
fi

function usage() {
  echo "Usage: $0 --root-temp-dir=dir --results=dir"
  echo "          --build-system=dir"
  echo "          --source-roots=dir1,dir2,..."
}


function get_package_root() {
 
  local tar_file_name=$1
  local untar_dir=$2

  local root_entry=`$tarcmd -tzf $tar_file_name | $grepcmd -e "\.fulcrum-package-root"`

  local package_root=`echo $root_entry | $sedcmd -e "s/\/\.fulcrum-package-root//"`

  package_root="$untar_dir/$package_root"

  ret="$package_root"
      
}


sedcmd=`which sed`
gawkcmd=`which gawk`
grepcmd=`which grep`
findcmd=`which find`
sortcmd=`which sort`
uniqcmd=`which uniq`
tarcmd=`which tar`
check_executable_file "$sedcmd" "Can't find sed in \"$PATH\"."   2
check_executable_file "$gawkcmd" "Can't find gawk in \"$PATH\"." 2
check_executable_file "$grepcmd" "Can't find grep in \"$PATH\"." 2
check_executable_file "$findcmd" "Can't find find in \"$PATH\"." 2
check_executable_file "$sortcmd" "Can't find sort in \"$PATH\"." 2
check_executable_file "$uniqcmd" "Can't find uniq in \"$PATH\"." 2
check_executable_file "$tarcmd"  "Can't find tar in \"$PATH\"."  2


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
check_for_empty_arg "$build_system_root" \
    "You must specify the path to the build system."                                2
check_for_empty_arg "$source_roots"         \
    "You must specifies the root paths that you want to build."                     2

check_writeable_dir "$root_temp_dir"        \
    "Root temporary directory: \"$root_temp_dir\" is not a writeable directory."    2
conon_path "$root_temp_dir"
root_temp_dir="$ret"

check_writeable_dir "$results_dir"          \
    "Results directory: \"$results_dir\" is not a readable, writeable directory."   1
conon_path "$results_dir"
results_dir="$ret"

check_readable_dir  "$build_system_root"    \
    "Build System Root: \"$build_system_root\" is not a readable directory."        2
conon_path "$build_system_root"
build_system_root="$ret"


package_inst="$build_system_root/filetypes/packagingfiles/package-inst.pl"
check_executable_file "$package_inst"       \
    "Package Installation Script: \"$package_inst\" is not an exectutable file."    2


short_package_name_dir="$results_dir/packages/shortnames"

tests_results_dir="$results_dir/testresults"

if [ -e "$tests_results_dir" ] ; then
  rm -rf "$tests_results_dir"
fi

mkdir -p "$tests_results_dir"

check_readable_dir "$short_package_name_dir" \
    "Results Directory: \"$short_package_name_dir is not a readable directory."     1

built_short_name_packages=`$findcmd $short_package_name_dir -type f -name "*.tar.gz"`


#Make sure all the source directories exist and cononacalize their names.
source_roots=`echo $source_roots | $sedcmd -e "s/,/ /g"`
conon_source_roots=
for curr_root in $source_roots ; do
  check_readable_dir "$curr_root"           \
      "Source Root Directories: \"$curr_root\" is not a readable directory" 1
  conon_path "$curr_root"
  curr_root="$ret"
  if [ -n "$conon_source_roots" ] ; then
    conon_source_roots="$conon_source_roots $curr_root"
  else
    conon_source_roots="$curr_root"
  fi
done

#for each package that was built find all the tests scripts, sort them, and execute
#them
for package in $built_short_name_packages ; do
  package_name=`basename $package .tar.gz`
  
  #find all the tests scripts for the current package in all source trees.
  tests_for_curr_package=
  for curr_root in $conon_source_roots ; do
    curr_root_tests=`$findcmd $curr_root -type f -perm +111 -name "$package_name.*.*.sh" -print`
    if [ -n "$tests_for_curr_package" ] ; then
      tests_for_curr_package="$tests_for_curr_package\n$curr_root_tests"
    else
      tests_for_curr_package="$curr_root_tests"
    fi
  done

  #extract all unique test numbers from all the test scripts we found.
  tests_numbers=
  tests_numbers=`echo -e "$tests_for_curr_package"                           | \
                   $sedcmd -e  "s/.*$package_name\.\([0-9]\+\)\..\+\.sh/\1/" | \
                   $sortcmd -g                                               | \
                   $uniqcmd                                                  | \
                   $grepcmd -v "^[[:space:]]*\$"`


  
  #use sorted list of test numbers to sort all the tests we found for the current package.
  ordered_tests_for_curr_package=
  for curr_test_num in $tests_numbers ; do
    tests_with_curr_num=`echo -e "$tests_for_curr_package"                 | \
                         $grepcmd -e "$pacakge_name\.$curr_test_num\..\+"  | \
                         $sortcmd`
    ordered_tests_for_curr_package="$ordered_tests_for_curr_package\n$tests_with_curr_num"
  done
  
  ordered_tests_for_curr_package=`echo -e $ordered_tests_for_curr_package | $grepcmd -v "^[[:space:]]*\$"`

  if [ -n "$ordered_tests_for_curr_package" ] ; then

    curr_tmp_dir=`mktemp -d $root_temp_dir/runtestsfromsource.XXXXXX`
 
    $package_inst $package $curr_tmp_dir
    
    get_package_root $package $curr_tmp_dir
    curr_package_root="$ret"
    conon_path "$curr_package_root"
    curr_package_root="$ret"

    test_exit_code="0"
    
    for curr_test in $ordered_tests_for_curr_package ; do
      if [ "$test_exit_code" == "0" ] ; then
        curr_test_base_name=`basename $curr_test`

        curr_test_result_dir="$tests_results_dir/$curr_test_base_name"

        mkdir "$curr_test_result_dir"      

        dir_containing_test=`dirname $curr_test`
        echo "$curr_test $dir_containing_test $curr_package_root &>$curr_test_result_dir/test.log"
        $curr_test $dir_containing_test $curr_package_root &>$curr_test_result_dir/test.log
        test_exit_code=$?
        echo "$test_exit_code" >$curr_test_result_dir/exitcode.txt

        if [ "$test_exit_code" == "0" ] ; then
          ln $package $curr_test_result_dir
        fi
      fi
    done

    rm -rf "$curr_tmp_dir"
    
  fi
  
  

done
