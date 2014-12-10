#!/bin/bash
# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$
# UPDATED 2

function usage() {
  echo "Usage: $0 "
  echo "  --dfII-dir=dir"
  echo "  ( --file=filename |"
  echo "    --view-name=view_name"
  echo "    [ --file=filename ] )"
  echo "  [ --file-type=ktext ]"
  echo "  [ --client-spec=client_spec_name ]"
  echo "  [ --change-list=num ]"
  echo "  [ --perforce-user=user_name ]"
  echo "  [ --perforce-password=xxx ]"
  echo "  [ --perforce-server=xxx ]"
  echo "  [ --perforce-port=xxx ]"
  echo "  [ --verbose ]"
  echo "  cell1 [ cell2 [ ... ] ]"
  exit 2
  
}

verbose=

function message() {
    if [ -n "$verbose" ]; then
      echo "$@"
    fi
}

# assumes run under fulcrum script
arch_bin_dir=${0%\/*}
package_root=${arch_bin_dir%\/*}
p4_executable=`which p4`
sh_lib_dir="$package_root/share/script/sh/sh-lib"
source "$sh_lib_dir/file/filecheck.sh"
source "$sh_lib_dir/file/conon.sh"
source "$sh_lib_dir/script/generate_script_with_libs.sh"

check_executable_file "$p4_executable" "p4: \"$p4_executable\" is not an exectable file." 2

sedcmd=`which sed`
gawkcmd=`which gawk`
grepcmd=`which grep`
sortcmd=`which sort`
check_executable_file "$sedcmd" "Unable to find sed in \"$PATH\"" 2
check_executable_file "$gawkcmd" "Can't find gawk in \"$PATH\"." 2
check_executable_file "$grepcmd" "Can't find grep in \"$PATH\"." 2
check_executable_file "$sortcmd" "Can't find sort in \"$PATH\"." 2

cds_sh_lib="$package_root/share/script/sh/util"
check_readable_dir "$cds_sh_lib" \
    "Cadence Shell Script Library: \"$cds_sh_lib\" is not a readable directory." 2
cds_sh_lib_files=`find "$cds_sh_lib" \! -type d`

for file in $cds_sh_lib_files ; do
  source "$file"
done

dfII_dir=
view_name=
file_name=
file_type="ktext"
client_spec=
change_list=default
cell_list=
p4_user=
p4_passwd=
p4_server_host=ssl:p4proxy07.devtools.intel.com
p4_server_port=2510
argname=
oa=1

for arg in $@ ; do
  
  case "$arg" in
  --* )
    argname=
    ;;
  esac
  case "$arg" in
  --verbose )
    verbose=t
    ;;
  --dfII-dir )
    argname="dfII_dir"
    ;;
  --dfII-dir=* )
    dfII_dir=${arg/--dfII-dir=}
    ;;
  --view-name )
    argname="view_name"
    ;;
  --view-name=* )
    view_name=${arg/--view-name=}
    ;;
  --file )
    argname="file_name"
    ;;
  --file=* )
    file_name=${arg/--file=}
    ;;
  --file-type )
    argname="file_type"
    ;;
  --file-type=* )
    file_type=${arg/--file-type=}
    ;;
  --client-spec )
    argname="client_spec"
    ;;
  --client-spec=* )
    client_spec=${arg/--client-spec=}
    ;;
  --change-list )
    argname="change_list"
    ;;
  --change-list=* )
    change_list=${arg/--change-list=}
    ;;
  --perforce-user )
    argname="p4_user"
    ;;
  --perforce-user=* )
    p4_user=${arg/--perforce-user=}
    ;;
  --perforce-password )
    argname="p4_passwd"
    ;;
  --perforce-password=* )
    p4_passwd=${arg/--perforce-password=}
    ;;
  --perforce-server )
    argname="p4_server_host"
    ;;
  --perforce-server=* )
    p4_server_host=${arg/--perforce-server=}
    ;;
  --perforce-port )
    argname="p4_server_port"
    ;;
  --perforce-port=* )
    p4_server_port=${arg/--perforce-port=}
    ;;
  --* )
    echo "Unknown argument: \"$arg\"."
    usage
    exit
    ;;
  * )
    if [ -n "$argname" ]; then
        eval "$argname=$arg"
    else
        cell_list="$cell_list $arg"
    fi
    argname=
    ;;
  esac
done


check_for_empty_arg "$dfII_dir"                   \
    "You must specify the location of directory containing all the dfII data."         2
if [ -z "$file_name" ] ; then
  check_for_empty_arg "$view_name"                \
      "You must specify the view name or file that you want to add for each cell."     2
else
  check_for_empty_arg "$file_type"                \
      "You must specify a file type if you are adding a file."                         2
fi
check_for_empty_arg "$p4_server_host"             \
    "You must specify a perforce server."                                              2
check_for_empty_arg "$p4_server_port"             \
    "You must specify a perforce server port."                                         2

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

  cadence_escape_string "$view_name"
  escaped_view="$ret"
  message "escaped_view $escaped_view"

  p4_add_command="$p4_command add -f"
  p4_fstat_command="$p4_command fstat"
  p4_reopen_command="$p4_command reopen"
  if [ -n "$change_list" -a "$change_list" != "default" ] ; then
    p4_add_command="$p4_add_command -c $change_list"
    p4_reopen_command="$p4_reopen_command -c $change_list"
  fi

  for cell in $cell_list ; do
    message "cell $cell"
    if [ -n "$view_name" ] ; then
      message "view_name $view_name"
      if [ -z "$file_name" ] ; then
        get_cadence_cell_view_binary_files "$cell" "$view_name" "$dfII_dir"
        cell_view_bin_files="$ret"
        message "cell_view_bin_files $cell_view_bin_files"

        get_cadence_cell_view_text_files "$cell" "$view_name" "$dfII_dir"
        cell_view_text_files="$ret"
        message "cell_view_text_files $cell_view_text_files"

        if [[ -n "$cell_view_bin_files" && -n "cell_view_text_files" ]] ; then
          is_invalid_cadence_cell_view "$cell" "$view_name" "$dfII_dir"
          invalid_cell_view="$ret"
          message "invalid_cell_view $invalid_cell_view"
          if [[ "$invalid_cell_view" == "0" ]] ; then

            for bin_file in $cell_view_bin_files ; do
              if [[ ! ( -f "$bin_file"  &&
                        -r "$bin_file"  &&
                        -w "$bin_file" ) ]] ; then
                touch $bin_file
              fi
              escaped_bin_file=`echo "$bin_file" | sed -e 's/#/%23/g'`
              $p4_add_command -t "binary+l" -f "$bin_file"
            done
            for text_file in $cell_view_text_files ; do
              if [[ ! ( -f "$text_file"  &&
                        -r "$text_file"  &&
                        -w "$text_file" ) ]] ; then
                touch $text_file
              fi
              escaped_text_file=`echo "$text_file" | sed -e 's/#/%23/g'`
              $p4_add_command -t "text+l" "$text_file"
            done

          else
            echo "\"$view_name\" of \"$cell\" does not seem to be a valid cell view." 1>&2
            cell_view_files="$cell_view_bin_files $cell_view_text_files"
            for file in $cell_view_files ; do
              if [[ ! ( -r "$file" &&
                        -f "$file"  ) ]] ; then
                echo "\"$file\" is not a readable file." 1>&2
              fi
            done
          fi
        else
          echo "Unable to get list of files for \"$view_name\" of \"$cell\"." 1>&2
        fi
      else
        get_cadence_cell_view_dir "$cell" "$view_name" "$dfII_dir"
        cellViewDir="$ret"
        message "cellViewDir $cellViewDir"

        if [ -n "$cellViewDir" ] ; then
          cellViewFile="$cellViewDir/$file_name"
          message "cellViewFile $cellViewFile"

          if [[ -r "$cellViewFile" && -f "$cellViewFile" ]] ; then
            $p4_add_command -t "$file_type" "$cellViewFile"
          else
            echo "ERROR: \"$cellViewFile\" is not a file." 1>&2
          fi
        else
          echo "ERROR: Unable to get directory for \"$cell\" \"$view_name\"." 1>&2
        fi
      fi
    else
      get_escaped_cell_dir "$cell" "$dfII_dir"
      cellDir="$ret"
      message "cellDir $cellDir"

      if [ -n "$cellDir" ] ; then
        cellFile="$cellDir/$file_name"
        message "cellFile $cellFile"
        escaped_cellFile=`echo "$cellFile" | sed -e 's/#/%23/g'`

        if [[ -r "$cellFile" && -f "$cellFile" ]] ; then
          $p4_add_command -t "$file_type" "$cellFile"
        else
          echo "ERROR: \"$cellFile\" is not a file." 1>&2
        fi
      else
        echo "ERROR: Unable to get directory for \"$cell\"." 1>&2
      fi
    fi
    get_lib_name "$cell"
    lib_list="$ret $lib_list"
    message "lib for $cell is $ret"
  done

  lib_list=`echo "$lib_list" | $sortcmd -u`
  message "lib_list $lib_list"

  for lib in $lib_list ; do
    message "lib $lib"
    get_library_dir_for_libraray "$lib" "$dfII_dir"
    lib_dir="$ret"
    message "lib_dir $lib_dir"
    
    lib_prop_xx="$lib_dir/prop.xx"
    lib_cdsinfo="$lib_dir/cdsinfo.tag"
    lib_oalib="$lib_dir/.oalib"
    lib_techlib="$lib_dir/tech.db"
    lib_datadm="$lib_dir/data.dm"
    
    # prop.xx must not exist in OA
    if [[ ( "$oa" == "1" || 
          ( -f "$lib_prop_xx" &&
          -r "$lib_prop_xx" ) ) &&
          -f "$lib_cdsinfo" &&
          -r "$lib_cdsinfo" &&
          -s "$lib_cdsinfo" &&
          ( "$oa" == "0" ||
          ( -f "$lib_oalib" &&
          -r "$lib_oalib" &&
          -s "$lib_oalib" )
          )
          ]] ; then
      if [ "$oa" == "1" ]; then
          lib_bin_files=
          if [ -f $lib_techlib ]; then lib_bin_files="$lib_techlib"; fi
          if [ -f $lib_datadm ]; then lib_bin_files="$lib_bin_files $lib_datadm"; fi
          lib_text_files="$lib_cdsinfo $lib_oalib"
      else
          lib_bin_files="$lib_prop_xx"
          lib_text_files="$lib_cdsinfo"
      fi
      
      for bin_file in $lib_bin_files ; do
        message "bin_file $bin_file"
        escaped_bin_file=`echo "$bin_file" | sed -e 's/#/%23/g'`
        message "escaped_bin_file $escaped_bin_file"
        depot_path=`$p4_fstat_command "$escaped_bin_file" 2>/dev/null | $grepcmd -e "^\.\.\. depotFile" | $gawkcmd -- " { print \\$3 } "`
        message "depot_path $depot_path"
        if [ -n "$depot_path" ] ; then
          curr_action=`$p4_fstat_command "$escaped_bin_file" | $grepcmd -e "^\.\.\. action" | $gawkcmd -- " { print \\$3 } "`
          message "curr_action $curr_action"
          if [ -n "$curr_action" ] ; then
            curr_change_list=`$p4_fstat_command $escaped_bin_file | $grepcmd -e "^\.\.\. change" | $gawkcmd -- " { print \\$3 } "`
            message "curr_change_list $curr_change_list"
            if [[ "$change_list" != "$curr_change_list" ]] ; then
              message "\"$bin_file\" is open in change list \"$curr_change_list\"."
              message "\"$bin_file\" will now be opened in change list \"$change_list\"."
              $p4_reopen_command -t "binary+l" "$escaped_bin_file"
            else
              message "\"$bin_file\" is already open in change list \"$change_list\", skipping..."
            fi
          else
            message "\"$bin_file\" is already checked into perforce, skipping..."
          fi
        else
          if [[ ! ( -f "$bin_file"  &&
                    -r "$bin_file"  &&
                    -w "$bin_file" ) ]] ; then
            touch "$bin_file"
          fi
          message "\"$bin_file\" has never been added to perforce, adding now."
          $p4_add_command -t "binary+l" "$bin_file"
        fi
      done

      for text_file in $lib_text_files ; do
        message "text_file $text_file"
        escaped_text_file=`echo "$text_file" | sed -e 's/#/%23/g'`
        message "escaped_text_file $escaped_text_file"
        depot_path=`$p4_fstat_command "$escaped_text_file" 2>/dev/null | $grepcmd -e "^\.\.\. depotFile" | $gawkcmd -- " { print \\$3 } "`
        if [ -n "$depot_path" ] ; then
          curr_action=`$p4_fstat_command "$escaped_text_file" | $grepcmd -e "^\.\.\. action" | $gawkcmd -- " { print \\$3 } "`
          message "curr_action $curr_action"
          if [ -n "$curr_action" ] ; then
            curr_change_list=`$p4_fstat_command $escaped_text_file | $grepcmd -e "^\.\.\. change" | $gawkcmd -- " { print \\$3 } "`
            message "curr_change_list $curr_change_list"
            if [[ "$change_list" != "$curr_change_list" ]] ; then
              message "\"$text_file\" is open in change list \"$curr_change_list\"."
              message "\"$text_file\" will now be opened in change list \"$change_list\"."
              $p4_reopen_command -t "text+l" "$escaped_text_file"
            else
              message "\"$text_file\" is already open in change list \"$change_list\", skipping..."
            fi
          else
            message "\"$text_file\" is already checked into perforce, skipping..."
          fi
        else
          if [[ ! ( -f "$text_file"  &&
                    -r "$text_file"  &&
                    -w "$text_file" ) ]] ; then
            touch "$text_file"
          fi
          message "\"$text_file\" has never been added to perforce, adding now."
          $p4_add_command -t "text+l" "$text_file"
        fi
      done
    else
      echo "ERROR: \"$lib\" in \"$lib_dir\" is not a valid library." 1>&2
    fi    
  done

else
  echo "The client specification \"$client_spec\" does not exist." 1>&2
fi
