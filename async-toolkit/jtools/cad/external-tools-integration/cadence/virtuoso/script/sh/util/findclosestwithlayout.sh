#!/bin/bash
# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$

arch_bin_dir=${0%\/*}
package_root=${arch_bin_dir%\/*}
sh_lib_dir="$package_root/share/script/sh/sh-lib"
build_id="$buildid$"




function exit_func() {
  if [ -n "$subtypesListFile" ] ; then
    rm -f "$subtypesListFile"
  fi
  if [ -n "$netlistDistanceOutputFile" ] ; then
    rm -f "$netlistDistanceOutputFile"
  fi
  

}

trap exit_func EXIT



source "$sh_lib_dir/file/filecheck.sh"
source "$sh_lib_dir/file/conon.sh"
source "$sh_lib_dir/script/generate_script_with_libs.sh"



function usage() {
  echo "Usage: $0 "
  echo "  --cell=cell"
  echo "  --dfII-dir=dir"
  echo "  --cast-path=castpath"
  echo "  [ --verbose ]"
  echo "  [ --debug ]"
}

sedcmd=`which sed`
gawkcmd=`which gawk`
grepcmd=`which grep`
bashcmd=`which bash`
findcmd=`which find`
sortcmd=`which sort`
uniqcmd=`which uniq`

check_executable_file "$sedcmd" "Unable to find sed in \"$PATH\"." 2
check_executable_file "$gawkcmd" "Unable to find gawk in \"$PATH\"." 2
check_executable_file "$grepcmd" "Unable to fine grep in \"$PATH\"." 2
check_executable_file "$bashcmd" "Unable to find bash in \"$PATH\"." 2
check_executable_file "$findcmd" "Unable to find find in \"$PATH\"." 2
check_executable_file "$sortcmd" "Unable to find sort in \"$PATH\"." 2
check_executable_file "$uniqcmd" "Unable to find uniq in \"$PATH\"." 2

check_readable_dir "$arch_bin_dir" \
    "Package arch bin: \"$arch_bin_dir\" is not a readable directory." 2

cds_sh_lib="$package_root/share/script/sh/util"
check_readable_dir "$cds_sh_lib" \
    "Cadence Shell Script Library: \"$cds_sh_lib\" is not a readable directory." 2

cds_sh_lib_files=`$findcmd "$cds_sh_lib" \! -type d`
for file in $cds_sh_lib_files ; do
  source "$file"
done

netlistDistance="$arch_bin_dir/netlistDistance"
check_executable_file "$netlistDistance" "\"$netlistDistance\" is not an executable file." 2

layoutViewName="layout"


cell=
dfIIDir=
castPath=
verbose=
debug=



for arg in $@ ; do
  
  case "$arg" in
  --cell=* )
    cell=`echo "$arg" | $sedcmd -e "s/--cell=//"`
    ;;
  --dfII-dir=* )
    dfIIDir=`echo "$arg" | $sedcmd -e "s/--dfII-dir=//"`
    ;;
  --cast-path=* )
    castPath=`echo "$arg" | $sedcmd -e "s/--cast-path=//"`
    ;;
  --verbose )
    verbose=1
    ;;
  --debug )
    verbose=1
    debug=1
    ;;
  --* )
    echo "Unknown argument: \"$arg\"."
    usage
    exit 2
    ;;
  esac
done

check_for_empty_arg "$cell"                                                      \
    "You must specify a cell name."                                                             2
check_for_empty_arg "$dfIIDir"                                                   \
    "You must specify the location of directory containing all the dfII data."                  2
check_for_empty_arg "$castPath"                                                  \
    "You must specify a cast path"                                                              2

check_readable_dir "$dfIIDir"                                                    \
    "DFII Directory: \"$dfIIDir\" is not a readable directory."                                 1
conon_path "$dfIIDir"
dfIIDir="$ret"


get_cell_name_from_cell_name_with_subtype "$cell"
cellBaseName="$ret"
if [ -n "$verbose" ] ; then
  echo "Cell Base Name: \"$cellBaseName\"."
fi

get_library_dir "$cell" "$dfIIDir"
libDir="$ret"
if [ -n "$verbose" ] ; then
  echo "Library Directort: \"$libDir\"."
fi

cadence_escape_string "$cellBaseName"
escapedCellBaseName="$ret"

cadence_escape_string "$cell"
escapedCellName="$ret"

otherSubTypeDirs=`$findcmd "$libDir" -mindepth 1 -maxdepth 1 -type d -name "$escapedCellBaseName#2e*" -not -name "$escapedCellName"`

otherSubTypesWithLayout=

for otherSubTypeDir in $otherSubTypeDirs ; do
  if [[ -d "$otherSubTypeDir/$layoutViewName" && -r "$otherSubTypeDir/$layoutViewName" ]] ; then
    escapedSubTypeName=`basename "$otherSubTypeDir"`
    cadence_reverse_escape_string "$escapedSubTypeName"
    subTypeName="$ret"
    otherSubTypesWithLayout="$otherSubTypesWithLayout $subTypeName"
    if [ -n "$verbose" ] ; then
      echo "Found $subTypeName with layout."
    fi
  fi
done

subtypesListFile=`mktemp /tmp/findclosestwithlayout.XXXXXX`

for otherSubTypeWithLayout in $otherSubTypesWithLayout ; do
  echo "$otherSubTypeWithLayout" >>$subtypesListFile
done

netlistDistanceOutputFile=`mktemp /tmp/findclosestwithlayout.XXXXXX`

netlistDistanceCmd="$netlistDistance \"--cast-path=$castPath\" \"--library-cells-file=$subtypesListFile\" \"--cell=$cell\" >$netlistDistanceOutputFile"

if [ -n "$verbose" ] ; then
  echo "$netlistDistanceCmd"
fi

eval "$netlistDistanceCmd"

cat "$netlistDistanceOutputFile" | gawk -- "{ print \$2 FS \$3 FS \$1}" | sort -n
