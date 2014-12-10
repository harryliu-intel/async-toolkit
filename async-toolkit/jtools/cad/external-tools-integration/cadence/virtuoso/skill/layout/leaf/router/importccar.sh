#!/bin/bash

function usage() {
  echo "Usage: $0"
  echo "  --dest-lib=lib"
  echo "  --dest-cell=cell"
  echo "  --dest-lib=view"
  echo "  --working-dir=dir"
}

function mywhich() {
    which $1 | tail -1
}

arch_bin_dir=${0%\/*}
package_root=${arch_bin_dir%\/*}

sh_lib_dir="$package_root/share/script/sh/sh-lib"

source "$sh_lib_dir/file/filecheck.sh"
source "$sh_lib_dir/file/conon.sh"

sedcmd=`mywhich sed`
icc2cdba=`mywhich icc2cdba`

check_executable_file "$sedcmd"                                 \
   "Unable to find sed in \"$PATH\"."                                             2
check_executable_file "$icc2cdba"                                 \
   "Unable to find icc2cdba in \"$PATH\"."                                             2

dest_lib=
dest_cell=
dest_view=
working_dir=

for arg in $@ ; do
  
  case "$arg" in
  --dest-lib=* )
    dest_lib=`echo $arg | $sedcmd -e "s/--dest-lib=//"`
    ;;
  --dest-cell=* )
    dest_cell=`echo $arg | $sedcmd -e "s/--dest-cell=//"`
    ;;
  --dest-view=* )
    dest_view=`echo $arg | $sedcmd -e "s/--dest-view=//"`
    ;;
  --working-dir=* )
    working_dir=`echo $arg | $sedcmd -e "s/--working-dir=//"`
    ;;
  esac
done


check_for_empty_arg "$dest_lib"                                   \
    "The name of the library of the dest cell view to operate on must be specified."   2
check_for_empty_arg "$dest_cell"                                  \
    "The name of the dest cell to operate on must be specified."                       2
check_for_empty_arg "$dest_view"                                  \
    "The name of the dest view to operate on must be specified."                       2
check_for_empty_arg "$working_dir"                                  \
    "The working directory must be specified."                       2


ses_file=$(find "$working_dir" -name "*.ses" | tail -1 )
icc2cdba "$dest_lib" "$dest_cell" "$dest_view" -session "$ses_file"
