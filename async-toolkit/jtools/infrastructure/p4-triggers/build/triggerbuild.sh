#!/bin/bash
# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$

arch_bin_dir=${0%\/*}
package_root=${arch_bin_dir%\/*}

function exit_func() {
  if [ -n "$root_of_this_build" ] ; then
    rm -rf "$root_of_this_build"
  fi
  if [ -n "$change_list_spec" ] ; then
    rm -rf "$change_list_spec"
  fi
}

trap exit_func EXIT

package_arch_bin="$package_root/bin"

package_share_sh="$package_root/share/script/sh"

file_util="$package_share_sh/file/filecheck.sh"

conon_sh="$package_share_sh/file/conon.sh"

if [[ -f "$file_util" && -r "$file_util" ]] ; then
  source "$file_util"
else
  echo "\"$file_util\" is not a readable file."
  exit 2
fi

if [[ -f "$conon_sh" && -r "$conon_sh" ]] ; then
  source "$conon_sh"
else
  echo "\"$conon_sh\" is not a readable file."
  exit 2
fi

function usage() {
  echo "Usage: $0 --client-spec-name=p4client"
  echo "          --root-temp-dir=dir"
  echo "          --change-list-number=num"
  echo "          --output=file.tar.bz2"
  echo "          --project-root=relative_dir"
  echo "          [ --perforce-sever=perforce ] [ --perforce-port=1666 ]"
  echo "          [ --perforce-client=p4 ] [ --perforce-password=xxx ]" 
  echo "          [ --perforce-user=$USER ]"
  echo "          [ --verbose ]"
}

sedcmd=`which sed`
gawkcmd=`which gawk`
grepcmd=`which grep`
tarcmd=`which tar`
makecmd=`which make`



check_executable_file "$sedcmd" "Can't find sed in \"$PATH\"." 2
check_executable_file "$gawkcmd" "Can't find gawk in \"$PATH\"." 2
check_executable_file "$grepcmd" "Can't find grep in \"$PATH\"." 2
check_executable_file "$tarcmd" "Can't find tar in \"$PATH\"." 2
check_executable_file "$makecmd" "Can't find make in \"$PATH\"." 2


client_spec=
root_temp_dir=
change_list_num=
output_file=
project_root=

p4_server=perforce
p4_port=1666
p4_client=p4
p4_passwd=
p4_user=$USER
verbose=
for arg in $@ ; do

  case "$arg" in
  --client-spec-name=* )
    client_spec=`echo $arg | $sedcmd -e "s/--client-spec-name=//"`
    ;;
  --root-temp-dir=* )
    root_temp_dir=`echo $arg | $sedcmd -e "s/--root-temp-dir=//"`
    ;;
  --change-list-number=* )
    change_list_num=`echo $arg | $sedcmd -e "s/--change-list-number=//"`
    ;;
  --output=* )
    output_file=`echo $arg | $sedcmd -e "s/--output=//"`
    ;;
  --project-root=* )
    project_root=`echo $arg | $sedcmd -e "s/--project-root=//"`
    ;;
  --perforce-server=* )
    p4_server=`echo $arg | $sedcmd -e "s/--perforce-server=//"`
    ;;
  --perforce-port=* )
    p4_port=`echo $arg | $sedcmd -e "s/--perforce-port=//"`
    ;;
  --perforce-client=* )
    p4_client=`echo $arg | $sedcmd -e "s/--perforce-client=//"`
    ;;
  --perforce-password=* )
    p4_passwd=`echo $arg | $sedcmd -e "s/--perforce-password=//"`
    ;;
  --perforce-user=* )
    p4_user=`echo $arg | $sedcmd -e "s/--perforce-user=//"`
    ;;
  --verbose )
    verbose=1
    ;;
  --* )
    echo "Unknown argument: \"$arg\"."
    usage
    exit 2
    ;;
  esac
done


check_for_empty_arg "$client_spec" "You must specify a client specification." 2
check_for_empty_arg "$root_temp_dir" \
                    "You must specify a root tempory directory, such as /tmp or /scratch." 2
check_for_empty_arg "$change_list_num" \
                    "You must specify the change list number that is triggering the build." 2
check_for_empty_arg "$output_file" "You must specify an output file name." 2
check_for_empty_arg "$project_root" "You must specify the directory in the client spec that is the root of the build." 2
check_for_empty_arg "$p4_server" "You must specify the host name of the perforce server." 2
check_for_empty_arg "$p4_port" "You must specify the port ther perforce server is listening on." 2
check_for_empty_arg "$p4_client" "You must specify a perforce client program." 2
check_for_empty_arg "$p4_user" "You must specify a perforce user name." 2


check_writeable_dir "$root_temp_dir" \
                    "Root temporary directory: \"$root_temp_dir\" is not a readable, writeable directory." 2

conon_path "$root_temp_dir"
root_temp_dir="$ret"

check_writeable_file "$output_file"   \
                     "Output: \"$output_file\" is not a writeable file." 2
conon_path "$output_file"
output_file="$ret"

real_p4_client=`which $p4_client`
check_executable_file "$real_p4_client" "Unable to find p4 in \"$PATH\"." 2


if [ -n "$p4_passwd" ] ; then
  p4_cmd="$real_p4_client -u $p4_user -p $p4_server:$p4_port -P $p4_passwd"
else
  p4_cmd="$real_p4_client -u $p4_user -p $p4_server:$p4_port"
fi

client_spec_exists=`$p4_cmd clients | $gawkcmd "{ print \\$2 }" | $grepcmd $client_spec`

if [ -n "$client_spec_exists" ] ; then
  if [ -n "$verbose" ] ; then
    echo "\"$client_spec\" exists."
  fi

  p4_cmd_with_client="$p4_cmd -c $client_spec"

  change_list_spec=`mktemp "$root_temp_dir/triggerbuild.XXXXXX"`

  $p4_cmd_with_client "change" "-o" "$change_list_num" &>$change_list_spec

  change_list_user=`cat "$change_list_spec" | grep -e "^User:[[:space:]]\+" | sed -e "s/User:[[:space:]]\+//"`
  change_list_client=`cat "$change_list_spec" | grep -e "^Client:[[:space:]]\+" | sed -e "s/Client:[[:space:]]\+//"`

  if [[ -n "$change_list_user"   && \
        -n "$change_list_client" ]] ; then
    if [ -n "$verbose" ] ; then
      echo "Change list user is \"$change_list_user\"."
      echo "Change list client is \"$change_list_client\"."
    fi
    #make build identifier from current date and hostname.
    build_date_str=`date +%Y-%m-%d-%H-%M-%S-%Z`
    host_name=`hostname`
    build_id="$host_name-$build_date_str-trigger"

    root_of_this_build=`mktemp -d $root_temp_dir/triggerbuild.XXXXXX`
    check_writeable_dir "$root_of_this_build" "Unable to create \"$root_of_this_build\"." 1
    build_scratch_dir="$root_of_this_build/build-scratch"
    curr_results_dir="$root_of_this_build/results"
    log_dir="$root_of_this_build/logs"
    mkdir "$build_scratch_dir"
    check_writeable_dir "$build_scratch_dir" "Unable to create \"$build_scratch_dir\"." 1
    mkdir "$curr_results_dir"
    check_writeable_dir "$curr_results_dir" "Unable to create \"$curr_results_dir\"." 1
    mkdir "$log_dir"
    check_writeable_dir "$log_dir" "Unable to create \"$log_dir\"." 1


    if [ -n "$verbose" ] ; then
      echo "$p4_cmd -c $client_spec sync \"@$change_list_num\" &>$log_dir/src0.log"
    fi
    $p4_cmd_with_client "sync" "@$change_list_num" &>$log_dir/src0.log
    $p4_cmd_with_client "client" "-o" &>$log_dir/src1.log

    client_root=`cat $log_dir/src1.log                              | \
                   $grepcmd -e "^Root:[[:space:]]"                  | \
                   $sedcmd -e "s/^Root:[[:space:]]*//"`

    check_readable_dir "$client_root" "Unable to determine root of client \"$client_spec\"." 1

    if [ -n "$verbose" ] ; then
      echo "Client Root is \"$client_root\"."
    fi

    code_root="$client_root/$project_root"

    build_system="$client_root/sw/infrastructure/build-system"



    if [ -n "$verbose" ] ; then
      echo "Calling make."
    fi
    #build stuff
    $makecmd -f $build_system/Makefile                                   \
      "BUILD_SYSTEM_ROOT=$build_system"                                  \
      "ROOT_PROJECT_DIR=$code_root"                                      \
      "ROOT_TARGET_DIR=$build_scratch_dir"                               \
      "FULCRUM_NO_UTIL_MAKEFILES=1"                                      \
      "FULCRUM_RESULTS_DIR=$curr_result_dir"                             \
      "FULCRUM_BUILD_ID=$build_id"                                       \
      allpackages &>$log_dir/build.log

    build_exit_status=$?

    output_log="$log_dir/output.log"

    build_success=0
    case "$build_exit_status" in
      "0" )
        build_success=1
        ;;
      * )
        build_success=0 
        ;;
    esac

    echo "USER $change_list_user" >$output_log
    echo "CHANGE $change_list_num" >>$output_log
    echo "CHANGE_CLIENT $change_list_client" >>$output_log
    echo "BUILD_HOST $host_name" >>$output_log
    echo "BUILD_ID $build_id" >>$output_log
    echo "BUILD_EXIT_STATUS $build_exit_status" >>$output_log
    echo "BUILD_SUCCESS $build_success" >>$output_log
    echo "BUILD_CLIENT $client_spec" >>$output_log
    echo "BUILD_ROOT $code_root" >>$output_log
    echo "BUILD_SYSTEM $build_system" >>$output_log

    if [ -n "$verbose" ] ; then
      echo "Writing results."
      echo "$tarcmd -cjf \"$output_file\" -C \"$root_of_this_build\" logs"
    fi

    $tarcmd -cjf "$output_file" -C "$root_of_this_build" logs

  else
    echo "Change list \"$change_list_num\" does not seem to exist."
    exit 1
  fi
else
  echo "Client Specification \"$client_spec\" does not appear to exist."
  exit 2
fi

