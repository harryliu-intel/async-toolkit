#!/bin/bash
# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$


function exit_func() {
  if [ -n "$libnamesfile" ] ; then
    rm -f "$libnamesfile"
  fi
  /bin/true
}

trap exit_func EXIT

function usage() {
  echo "Usage: $0 "
  echo "  [ --depot-dfII-dir=dir ]"
  echo "  [ --depot-spec-dir=dir ]"
  echo "  --src-branch=branchname"
  echo "  --dest-branch=branchname"
  echo "  [ --cell-list=file ]"
  echo "  [ cell [ cell [ cell ... ] ] ]"
}


arch_bin_dir=${0%\/*}
package_root=${arch_bin_dir%\/*}
sh_lib_dir="$package_root/share/script/sh/sh-lib"

source "$sh_lib_dir/file/filecheck.sh"
source "$sh_lib_dir/file/conon.sh"
source "$sh_lib_dir/script/generate_script_with_libs.sh"

sedcmd=`which sed`
gawkcmd=`which gawk`
grepcmd=`which grep`
sortcmd=`which sort`
uniqcmd=`which uniq`
check_executable_file "$sedcmd" "Unable to find sed in \"$PATH\"" 2
check_executable_file "$gawkcmd" "Can't find gawk in \"$PATH\"." 2
check_executable_file "$grepcmd" "Can't find grep in \"$PATH\"." 2
check_executable_file "$sortcmd" "Can't find sort in \"$PATH\"." 2
check_executable_file "$uniqcmd" "Can't find uniq in \"$PATH\"." 2

cds_sh_lib="$package_root/share/script/sh/util"
check_readable_dir "$cds_sh_lib" \
    "Cadence Shell Script Library: \"$cds_sh_lib\" is not a readable directory." 2
cds_sh_lib_files=`find "$cds_sh_lib" \! -type d `

for file in $cds_sh_lib_files ; do
  source "$file"
done


depot_dfII_dir=
depot_spec_dir=
src_branch=
dest_branch=
cell_list_file=
suppress_spec=
suppress_dfII=
cells_list=


for arg in $@ ; do
  
  case "$arg" in
  --depot-dfII-dir=* )
    depot_dfII_dir=`echo $arg | $sedcmd -e "s/--depot-dfII-dir=//"`
    ;;
  --depot-spec-dir=* )
    depot_spec_dir=`echo $arg | $sedcmd -e "s/--depot-spec-dir=//"`
    ;;
  --src-branch=* )
    src_branch=`echo $arg | $sedcmd -e "s/--src-branch=//"`
    ;;
  --dest-branch=* )
    dest_branch=`echo $arg | $sedcmd -e "s/--dest-branch=//"`
    ;;
  --cell-list=* )
    cell_list_file=`echo $arg | $sedcmd -e "s/--cell-list=//"`
    ;;
  --* )
    echo "Unknown argument: \"$arg\"."
    usage
    exit 2
    ;;
  * )
    cell_list="$cell_list $arg"
    ;;
  esac
done


check_for_empty_arg "$src_branch"                 \
    "You must specify the name of the source branch."                                  2

check_for_empty_arg "$dest_branch"                \
    "You must specify the name of the destination branch."                             2

if [ -n "$cell_list_file" ] ; then
  check_readable_file "$cell_list_file"           \
    "cell list file: \"$cell_list_file\" is not a readable file."                      2
  cells_in_file=`cat $cell_list_file | $sedcmd -e "s/[[:space:]]*#.*//" | $grepcmd -e "[^[:space:]]"`
  cell_list="$cell_list $cells_in_file"
fi



libnamesfile=`mktemp /tmp/gen_branch_spec.XXXXXX`

for cell in $cell_list ; do

  get_lib_name "$cell"
  libname="$ret"

  echo "$libname" >>$libnamesfile

  if [ -n "$depot_dfII_dir" ] ; then
    get_dfII_depot_path_for_cell_name "$depot_dfII_dir" "$src_branch" "$cell"
    srcDepotPath="$ret"

    get_dfII_depot_path_for_cell_name "$depot_dfII_dir" "$dest_branch" "$cell"
    destDepotPath="$ret"

    echo "$srcDepotPath/... $destDepotPath/..."
  fi

  if [ -n "$depot_spec_dir" ] ; then
    get_spec_depot_path_for_cell_name "$depot_spec_dir" "$src_branch" "$cell"
    srcDepotPath="$ret"

    get_spec_depot_path_for_cell_name "$depot_spec_dir" "$dest_branch" "$cell"
    destDepotPath="$ret"

    echo "$srcDepotPath $destDepotPath"
  fi

done

if [ -n "$depot_dfII_dir" ] ; then
  uniqlibs=`cat $libnamesfile | $sortcmd | $uniqcmd`
  for lib in $uniqlibs ; do
    get_depot_path_for_lib_name "$depot_dfII_dir" "$src_branch" "$lib" 
    srcLibDepotPath="$ret"

    get_depot_path_for_lib_name "$depot_dfII_dir" "$dest_branch" "$lib" 
    destLibDepotPath="$ret"

    echo "$srcLibDepotPath/prop.xx" "$destLibDepotPath/prop.xx"
    echo "$srcLibDepotPath/cdsinfo.tag" "$destLibDepotPath/cdsinfo.tag"
  done
fi

