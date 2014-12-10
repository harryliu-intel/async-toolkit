#!/bin/bash
# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$
# UPDATED 2

function usage() {
  echo "Usage: $0 "
  echo "  --dfII-dir=dir"
  echo "  [ --client-spec=client_spec_name ]"
  echo "  [ --change-list=num ]"
  echo "  [ --perforce-user=user_name ]"
  echo "  [ --perforce-password=xxx ]"
  echo "  [ --perforce-server=xxx ]"
  echo "  [ --perforce-port=xxx ]"
  echo "  [ --fast ]"
  echo "  [ --no-recurse ]"
  
}

verbose=
fast=
oa=1

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
findcmd=`which find`
check_executable_file "$sedcmd" "Unable to find sed in \"$PATH\"" 2
check_executable_file "$gawkcmd" "Can't find gawk in \"$PATH\"." 2
check_executable_file "$grepcmd" "Can't find grep in \"$PATH\"." 2
check_executable_file "$findcmd" "Can't find find in \"$PATH\"." 2

dfII_dir=
view_name=
client_spec=
change_list=
cell_list=
p4_user=
p4_passwd=
p4_server_host=ssl:p4proxy07.devtools.intel.com
p4_server_port=2510
argname=
no_recurse=

for arg in $@ ; do
  
  case "$arg" in
  --* )
    argname=
    ;;
  esac
  case "$arg" in
  --fast )
    fast=t
    ;;
  --verbose )
    verbose=t
    ;;
  --dfII-dir )
    argname="dfII_dir"
    ;;
  --dfII-dir=* )
    dfII_dir=${arg/--dfII-dir=}
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
  --no-recurse )
    no_recurse="-maxdepth 1"
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
    "You must specify the location of directory containing all the dfII data." 2
check_for_empty_arg "$p4_server_host"             \
    "You must specify a perforce server."                                      2
check_for_empty_arg "$p4_server_port"             \
    "You must specify a perforce server port."                                 2

check_writeable_dir "$dfII_dir"  \
    "dfII directory: \"$dfII_dir\" is not a readable, writeable directory." 2
conon_path "$dfII_dir"
dfII_dir="$ret"
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

  p4_add_command="$p4_command add -f"
  p4_edit_command="$p4_command edit"
  p4_fstat_command="$p4_command fstat"
  p4_reopen_command="$p4_command reopen"
  if [ -n "$change_list" ] ; then
    p4_add_command="$p4_add_command -c $change_list"
    p4_edit_command="$p4_edit_command -c $change_list"
    p4_reopen_command="$p4_reopen_command -c $change_list"
  else
    p4_reopen_command="$p4_reopen_command -c default"
    change_list="default"
  fi

  message "Searching for all cdsinfo.tag files under \"$dfII_dir\"."
  if [ -n "$fast" -a -z "$no_recurse" ]; then
      all_cds_info_files=`$findcmd $dfII_dir -follow  \\( \\( -name '*#*' -o -name '[RS]*' \\) -prune \\) -o -type f -name "cdsinfo.tag" -print`
   else
      all_cds_info_files=`$findcmd "$dfII_dir" $no_recurse -type f -name "cdsinfo.tag"`
   fi

  for cds_info in $all_cds_info_files ; do
    cds_info_dir=`dirname $cds_info`

    lib_prop_xx="$cds_info_dir/prop.xx"
    lib_oalib="$cds_info_dir/.oalib"
    lib_techdb="$cds_info_dir/tech.db"
    lib_datadm="$cds_info_dir/data.dm"

    message "\"$cds_info_dir\" smells like a library."
    
    if [ "$oa" == "1" ]; then
        lib_bin_files=
        if [ -f $lib_techdb ]; then lib_bin_files="$lib_techdb"; fi
        if [ -f $lib_datadm ]; then lib_bin_files="$lib_bin_files $lib_datadm"; fi
        lib_text_files="$cds_info $lib_oalib"
    else
        lib_bin_files="$lib_prop_xx"
        lib_text_files="$cds_info"
    fi

    for bin_file in $lib_bin_files ; do
      message "bin_file $bin_file"
      escaped_bin_file=`echo "$bin_file" | sed -e 's/#/%23/g'`
      depot_path=`$p4_fstat_command $escaped_bin_file 2>/dev/null | $grepcmd -e "^\.\.\. depotFile" | $gawkcmd -- " { print \\$3 } "`
      if [ -n "$depot_path" ] ; then
        curr_action=`$p4_fstat_command $escaped_bin_file | $grepcmd -e "^\.\.\. action" | $gawkcmd -- " { print \\$3 } "`
        if [ -n "$curr_action" ] ; then
          curr_change_list=`$p4_fstat_command $escaped_bin_file | $grepcmd -e "^\.\.\. change" | $gawkcmd -- " { print \\$3 } "`
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
      depot_path=`$p4_fstat_command $escaped_text_file 2>/dev/null | $grepcmd -e "^\.\.\. depotFile" | $gawkcmd -- " { print \\$3 } "`
      if [ -n "$depot_path" ] ; then
        curr_action=`$p4_fstat_command $escaped_text_file | $grepcmd -e "^\.\.\. action" | $gawkcmd -- " { print \\$3 } "`
        if [ -n "$curr_action" ] ; then
          curr_change_list=`$p4_fstat_command $escaped_text_file | $grepcmd -e "^\.\.\. change" | $gawkcmd -- " { print \\$3 } "`
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
  done

else
  echo "The client specification \"$client_spec\" does not exist."
fi
