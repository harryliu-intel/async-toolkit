#!/bin/bash
# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$
# UPDATED

function usage() {
  echo "Usage: $0 "
  echo "  [ --user=all ]"
  echo "  [ --client-spec=client_spec_name ]"
  echo "  [ --perforce-user=user_name ]"
  echo "  [ --perforce-password=xxx ]"
  echo "  [ --perforce-server=xxx ]"
  echo "  [ --perforce-port=xxx ]"
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

sedcmd=`which sed`
gawkcmd=`which gawk`
grepcmd=`which grep`
sortcmd=`which sort`
uniqcmd=`which uniq`
perlcmd="/usr/intel/bin/perl"
check_executable_file "$sedcmd" "Unable to find sed in \"$PATH\"" 2
check_executable_file "$gawkcmd" "Can't find gawk in \"$PATH\"." 2
check_executable_file "$grepcmd" "Can't find grep in \"$PATH\"." 2
check_executable_file "$sortcmd" "Can't find sort in \"$PATH\"." 2
check_executable_file "$uniqcmd" "Can't find uniq in \"$PATH\"." 2
check_executable_file "$p4_executable" "Can't find p4 in \"$PATH\"." 2

cds_sh_lib="$package_root/share/script/sh/util"
check_readable_dir "$cds_sh_lib" \
    "Cadence Shell Script Library: \"$cds_sh_lib\" is not a readable directory." 2
cds_sh_lib_files=`find "$cds_sh_lib" \! -type d`

for file in $cds_sh_lib_files ; do
  source "$file"
done


user="all"
client_spec=
p4_user=
p4_passwd=
p4_server_host="ssl:p4proxy07.devtools.intel.com"
p4_server_port="2510"
argname=

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
  --user )
    argname="user"
    ;;
  --user=* )
    user=${arg/--user=}
    ;;
  --client-spec )
    argname="client_spec"
    ;;
  --client-spec=* )
    client_spec=${arg/--client-spec=}
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
    exit 2
    ;;
  * )
    if [ -n "$argname" ]; then
        eval "$argname=$arg"
    else
        usage
        exit 2
    fi
    argname=
    ;;
  esac
done

check_for_empty_arg "$user"                       \
    "You must specify a user name with which to filter."                               2
check_for_empty_arg "$p4_server_host"             \
    "You must specify a perforce server."                                              2
check_for_empty_arg "$p4_server_port"             \
    "You must specify a perforce server port."                                         2

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

  p4Command="$p4_cmd -c $client_spec"

  p4OpenedCommand="$p4Command opened -a"

  userPattern=".*"
  if [[ "$user" != "all" ]] ; then
    userPattern="$user"
  fi

  export userPattern
  export client_spec

  $p4OpenedCommand |
    $perlcmd -l -e '
        $user=$ENV{userPattern};
        $client_spec=$ENV{client_spec};
        while(<>) {
            chomp;
            next if ! ((/layout.cdb#/) or (/layout.oa#/));
            @f=split;
            $file=$f[0];
            $file =~ s/#\d+$//;
            $action=$f[2];
            $change=$f[3];
            $client=$f[$#f];
            $client=$f[$#f-1] if ($client =~ /(locked|exclusive)/);
            $client =~ s/@/ /g;
            next if ! ( $client =~ / $client_spec$/);
            next if ( (! ($client =~ /^$user /)) and $user ne ".*" );
            $file =~ s://depot/.*/dfII/::;
            @d=split(/\//,$file);
            $branch=$d[1];
            $view=$d[$#d-1];
            $cell=$d[$#d-2];
            $cell =~ s/%232e/./g;
            $cell =~ s/%232d/-/g;
            $cell =~ s/%2324/\$/g;
            print "$branch $cell $view $action $change $client";
       }'

else
  echo "The client specification \"$client_spec\" does not exist."
fi
