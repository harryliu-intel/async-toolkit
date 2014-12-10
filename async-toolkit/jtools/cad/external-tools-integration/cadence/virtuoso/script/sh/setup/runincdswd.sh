#!/bin/bash

arch_bin_dir=${0%\/*}
package_root=${arch_bin_dir%\/*}
sh_lib_dir="$package_root/share/script/sh/sh-lib"

source "$sh_lib_dir/file/filecheck.sh"
source "$sh_lib_dir/file/conon.sh"
source "$sh_lib_dir/script/generate_script_with_libs.sh"

function usage() {
  echo "usage: $0"
  echo " --dfII-dir=dir"
  echo " --fulcrum-pdk-root=dir"
}

function exit_func() {
  if [ -d "$tmp_dir" ] ; then
    rm -rf "$tmp_dir"
  fi
}

trap exit_func EXIT

sedcmd=`which sed`
check_executable_file "$sedcmd" "Unable to find sed in \"$PATH\"" 2

gotcmd=

dfII_dir=
fulcrum_pdk_root=

for arg in $@ ; do
  case "$arg" in
  --dfII-dir=* )
    if [ -z "$gotcmd" ] ; then
      dfII_dir=`echo $arg | $sedcmd -e "s/--dfII-dir=//"`
      shift
    fi
    ;;
  --fulcrum-pdk-root=* )
    if [ -z "$gotcmd" ] ; then
      fulcrum_pdk_root=`echo $arg | $sedcmd -e "s/--fulcrum-pdk-root=//"`
      shift
    fi
    ;;
  * )
    gotcmd=1
    ;;
  esac
done

check_for_empty_arg "$package_root"                                              \
    "The packageroot variable must contain the location of the package installation."           2
check_for_empty_arg "$dfII_dir"                                                    \
    "The cadence dfII directory  was not specified."                                            2
check_for_empty_arg "$fulcrum_pdk_root"                                          \

check_readable_dir  "$fulcrum_pdk_root"                                          \
    "Fulcrum PDK: \"$fulcrum_pdk_root\" is not a readable directory."                  2
conon_path "$fulcrum_pdk_root"
fulcrum_pdk_root="$ret"

check_readable_dir  "$dfII_dir"                                          \
    "DFII dir: \"$dfII_dir\" is not a readable directory."                  2
conon_path "$dfII_dir"
dfII_dir="$ret"

mkcdswd="$arch_bin_dir/mkcdswd"
check_executable_file "$mkcdswd" "Unable to find mkcdswd in $arch_bin_dir" 2

if [ -d "$TEMP" ] ; then
    temp="$TEMP"
elif [ -d "$TMP" ] ; then
    temp="$TMP"
else
    temp="/tmp"
fi

tmp_dir=$(mktemp -d "$temp/cdswd.XXXXXX")
$mkcdswd --target-dir="$tmp_dir" --dfII-dir="$dfII_dir" --fulcrum-pdk-root="$fulcrum_pdk_root" --force --temp
cd "$tmp_dir"
$@
