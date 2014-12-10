#!/bin/bash

arch_bin_dir=${0%\/*}
package_root=${arch_bin_dir%\/*}
sh_lib_dir="$package_root/share/script/sh/sh-lib"

source "$sh_lib_dir/file/filecheck.sh"
source "$sh_lib_dir/file/conon.sh"

function usage() {
  echo Usage: `basename $0`  
  echo "   --src-dfII-lib-dir=dir"
  echo "   --dest-dfII-dir=dir"
  echo "   --fulcrum-pdk-root=dir"
  echo "   [--force]"
  echo "   [--dest-view=autorouted]"
  echo "   [--src-view=routed]"
}

function exit_func() {
    if [ -d "$cmd_tmp_dir" ] ; then
        rm -rf "$cmd_tmp_dir"
    fi
}

trap exit_func EXIT

sedcmd=`which sed`
bashcmd=`which bash`
grepcmd=`which grep`

check_executable_file "$grepcmd" "Unable to find grep in \"$PATH\"" 2
check_executable_file "$sedcmd" "Unable to find sed in \"$PATH\"" 2
check_executable_file "$bashcmd" "Unable to find bash in \"$PATH\"" 2

source "$package_root/share/script/sh/util/parsecellname"

src_dfII_lib_dir=
dest_dfII_dir=
force=
dest_view="autorouted"
src_view="routed"
fulcrumPDKRoot=

for arg in $@ ; do
  
  case "$arg" in
  --force )
    force=1
    ;;
  --dest-dfII-dir=* )
    dest_dfII_dir=`echo $arg | $sedcmd -e "s/--dest-dfII-dir=//"`
    ;;
  --fulcrum-pdk-root=* )
    fulcrumPDKRoot=`echo $arg | $sedcmd -e "s/--fulcrum-pdk-root=//"`
    ;;
  --src-dfII-lib-dir=* )
    src_dfII_lib_dir=`echo $arg | $sedcmd -e "s/--src-dfII-lib-dir=//"`
    ;;
  --dest-view=* )
    dest_view=`echo $arg | $sedcmd -e "s/--dest-view=//"`
    ;;
  --src-view=* )
    src_view=`echo $arg | $sedcmd -e "s/--src-view=//"`
    ;;
  --* )
    echo "Unknown option \"$arg\"."
    usage
    exit 2
    ;;
  esac
done

check_for_empty_arg "$package_root"                                              \
    "The packageroot variable must contain the location of the package installation."           2
check_for_empty_arg "$dest_dfII_dir"                                           \
    "You must specify the destination dfII dir"                                                 2
check_for_empty_arg "$src_dfII_lib_dir"                                           \
    "You must specify the dfII lib dir from the run"                                            2
check_for_empty_arg "$fulcrumPDKRoot"                                                    \
    "You must specify the location of the fulcrum PDK package installation."             2

check_readable_dir "$fulcrumPDKRoot"                                                     \
    "Fulcrum PDK: \"$fulcrumPDKRoot\" is not a readable directory."                      1
conon_path "$fulcrumPDKRoot"
fulcrumPDKRoot="$ret"

check_readable_dir  "$src_dfII_lib_dir"                                          \
    "src dfII lib: \"$src_dfII_lib_dir\" is not a readable directory."                  2
conon_path "$src_dfII_lib_dir"
src_dfII_lib_dir="$ret"

check_writeable_dir "$dest_dfII_dir"                                                    \
    "dest dfII dir: \"$dest_dfII_dir\" is not a readable, writeable, directory."  1
conon_path "$dest_dfII_dir"
dest_dfII_dir="$ret"

blank_lib="$fulcrumPDKRoot/share/Fulcrum/blank-library"

dfII_cell_dirs=`find $src_dfII_lib_dir -maxdepth 1 -mindepth 1 -type d -printf "%P\n"`

for dfII_cell_dir in $dfII_cell_dirs ; do 
  cadence_reverse_escape_string $dfII_cell_dir
  cell=$ret
  used=

  get_cadence_cell_view_dir $cell "$dest_view" $dest_dfII_dir
  dest_dfII_cell_dir="$ret"
  get_library_dir $cell $dest_dfII_dir
  dest_dfII_lib_dir="$ret"
  if [[ ! ( -d $dest_dfII_lib_dir ) ]] ; then
      cp -a $blank_lib $dest_dfII_lib_dir
  elif [[ ! ( -e $dest_dfII_lib_dir/cdsinfo.tag ) ]] ; then
      cp -a "$blank_lib/"*"" $dest_dfII_lib_dir
  fi

  src_dfII_cell_dir="$src_dfII_lib_dir/$dfII_cell_dir/$src_view"
  if [ -d "$src_dfII_cell_dir" ] ; then
      mkdir -p $dest_dfII_cell_dir
      used=1
      if [ -n "$force" ] ; then
          cp -rf "$src_dfII_cell_dir/"*"" "$dest_dfII_cell_dir"
      else
          cp -r "$src_dfII_cell_dir/"*"" "$dest_dfII_cell_dir"
      fi
  fi

  if [ -n "$used" ] ; then
      echo $cell
  fi
done

 

