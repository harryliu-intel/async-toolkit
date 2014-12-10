#!/bin/bash
# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$

arch_bin_dir=${0%\/*}
package_root=${arch_bin_dir%\/*}
share_dir="$package_root/share"
sh_lib_dir="$share_dir/script/sh"

templates_dir="$share_dir/templates"


function exit_func() {
  if [ -n "$triggerBuildOutput" ] ; then
    rm -rf "$triggerBuildOutput"
  fi
  if [ -n "$triggerBuildOutputLogFile" ] ; then
    rm -f "$triggerBuildOutputLogFile"
  fi
  if [ -n "$mailToUser" ] ; then
    rm -f "$mailToUser"
  fi
  if [ -n "$mailToCulprit" ] ; then
    rm -f "$mailToCulprit"
  fi
}

trap exit_func EXIT


source "$sh_lib_dir/file/filecheck.sh"
source "$sh_lib_dir/file/conon.sh"


sedcmd=`which sed`
gawkcmd=`which gawk`
grepcmd=`which grep`
bashcmd=`which bash`
findcmd=`which find`
sortcmd=`which sort`
uniqcmd=`which uniq`
tarcmd=`which tar`


check_executable_file "$sedcmd" "Unable to find sed in \"$PATH\"." 2
check_executable_file "$gawkcmd" "Unable to find gawk in \"$PATH\"." 2
check_executable_file "$grepcmd" "Unable to fine grep in \"$PATH\"." 2
check_executable_file "$bashcmd" "Unable to find bash in \"$PATH\"." 2
check_executable_file "$findcmd" "Unable to find find in \"$PATH\"." 2
check_executable_file "$sortcmd" "Unable to find sort in \"$PATH\"." 2
check_executable_file "$uniqcmd" "Unable to find uniq in \"$PATH\"." 2
check_executable_file "$tarcmd"  "Unable to find tar in \"$PATH\"." 2


arch_bin_dir="$package_root/bin"

check_readable_dir "$arch_bin_dir" \
    "Package arch bin: \"$arch_bin_dir\" is not a readable directory." 2

triggerBuild="$arch_bin_dir/triggerbuild"
check_executable_file "$triggerBuild" \
    "triggerbuild: \"$triggerBuild\" is not an executable file." 2


theyFixed=$templates_dir/someonefixedthebuildyoubroke.txt
theyDidNotFix=$templates_dir/theydidnotfixthebuild.txt
youBroke=$templates_dir/youbrokethebuild.txt
youDidNotFix=$templates_dir/youdidnotfixthebuild.txt
youDidNotFixYours=$templates_dir/youdidnotfixthebuildyoubroke.txt
youFixed=$templates_dir/youfixedthebuild.txt
youFixedYours=$templates_dir/youfixedthebuildyoubroke.txt

check_readable_file "$theyFixed" "\"$theyFixed\" is not a readable file." 2
check_readable_file "$theyDidNotFix" "\"$theyDidNotFix\" is not a readable file." 2
check_readable_file "$youBroke" "\"$youBroke\" is not a readable file." 2
check_readable_file "$youDidNotFix" "\"$youDidNotFix\" is not a readable file." 2
check_readable_file "$youDidNotFixYours" "\"$youDidNotFixYours\" is not a readable file." 2
check_readable_file "$youFixed" "\"$youFixed\" is not a readable file." 2
check_readable_file "$youFixedYours" "\"$youFixedYours\" is not a readable file." 2


log_dir="$package_root/logs"

if [[ ! -e "$log_dir" ]] ; then
  mkdir "$log_dir"
fi

check_writeable_dir "$log_dir" \
    "Log Directory \"$log_dir\" is not a writeable directory." 2

if [[ ! -e "$log_dir" ]] ; then
  mkdir "$log_dir"
fi

root_temp_dir="/scratch"

if [[ ! ( -d "$root_temp_dir" && -w "$root_temp_dir" && -r "$root_temp_dir" ) ]] ; then
  root_temp_dir="$TEMP"
fi

check_writeable_dir "$root_temp_dir" \
    "Root temp directory \"$root_temp_dir\" is not a writeable directory." 2


function usage() {
  echo "Usage: $0 --client-spec-name=p4client"
  echo "          --change-list-number=num"
  echo "          --project-root=relative_dir"
  echo "          --build-name=name"
  echo "          [ --perforce-sever=perforce ] [ --perforce-port=1666 ]"
  echo "          [ --perforce-client=p4 ] [ --perforce-password=xxx ]" 
  echo "          [ --perforce-user=$USER ]"
}

function getEntryFromOutputLog() {
  local outputLog="$1"
  local entryName="$2"

  local entryVal=`cat "$outputLog" | \
                    $grepcmd -e "$entryName"    | \
                    $sedcmd -e "s/$entryName //"`
  ret="$entryVal"
}

function makeMail() {
  local templateFileName="$1"
  local targetFileName="$2"
  local user="$3"
  local brokenUser="$4"
  local change="$5"
  local brokenChange="$6"
  local buildName="$7"
  local logArchive="$8"
  local brokenLogArchive="$9"
  local userFirstName="${10}"
  local userFullName="${11}"
  local brokenUserFirstName="${12}"
  local brokenUserFullName="${13}"

  echo "Subject: $buildName Build" >$targetFileName

  cat "$templateFileName"                                            | \
    $sedcmd -e "s=\\\$user\\\$=$user=g"                                \
            -e "s=\\\$brokenuser\\\$=$brokenUser=g"                    \
            -e "s=\\\$changenum\\\$=$change=g"                         \
            -e "s=\\\$brokenchangenum\\\$=$brokenChange=g"             \
            -e "s=\\\$buildname\\\$=$buildName=g"                      \
            -e "s=\\\$logarchive\\\$=$logArchive=g"                    \
            -e "s=\\\$brokenlogarchive\\\$=$brokenLogArchive=g"        \
            -e "s=\\\$userfirstname\\\$=$userFirstName=g"              \
            -e "s=\\\$userfullname\\\$=$userFullName=g"                \
            -e "s=\\\$brokenuserfirstname\\\$=$brokenUserFirstName=g"  \
            -e "s=\\\$brokenuserfullname\\\$=$brokenUserFullName=g"    >>$targetFileName

}


client_spec=
change_list_num=
buildName=
p4_server=perforce
p4_port=1666
p4_client=p4
p4_passwd=
p4_user=$USER
project_root=
verbose=
for arg in $@ ; do

  case "$arg" in
  --client-spec-name=* )
    client_spec=`echo "$arg" | $sedcmd -e "s/--client-spec-name=//"`
    ;;
  --change-list-number=* )
    change_list_num=`echo "$arg" | $sedcmd -e "s/--change-list-number=//"`
    ;;
  --build-name=* )
    buildName=`echo "$arg" | $sedcmd -e "s/--build-name=//"`
    ;;
  --project-root=* )
    project_root=`echo "$arg" | $sedcmd -e "s/--project-root=//"`
    ;;
  --perforce-server=* )
    p4_server=`echo "$arg" | $sedcmd -e "s/--perforce-server=//"`
    ;;
  --perforce-port=* )
    p4_port=`echo "$arg" | $sedcmd -e "s/--perforce-port=//"`
    ;;
  --perforce-client=* )
    p4_client=`echo "$arg" | $sedcmd -e "s/--perforce-client=//"`
    ;;
  --perforce-password=* )
    p4_passwd=`echo "$arg" | $sedcmd -e "s/--perforce-password=//"`
    ;;
  --perforce-user=* )
    p4_user=`echo "$arg" | $sedcmd -e "s/--perforce-user=//"`
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
check_for_empty_arg "$change_list_num" \
                    "You must specify the change list number that is triggering the build." 2
check_for_empty_arg "$project_root" "You must specify the directory in the client spec that is the root of the build." 2
check_for_empty_arg "$buildName" "You must specify the name of the build." 2
check_for_empty_arg "$p4_server" "You must specify the host name of the perforce server." 2
check_for_empty_arg "$p4_port" "You must specify the port ther perforce server is listening on." 2
check_for_empty_arg "$p4_client" "You must specify a perforce client program." 2
check_for_empty_arg "$p4_user" "You must specify a perforce user name." 2


real_p4_client=`which $p4_client`
check_executable_file "$real_p4_client" "Unable to find p4 in \"$PATH\"." 2

changeLogFile="$log_dir/$client_spec.trigger.change.log"

lastChange="0 root"
if [[ -f "$changeLogFile" ]] ; then
  lastChange=`tail -1 "$changeLogFile"`
fi

lastChangeNum=`echo "$lastChange" | $gawkcmd -- "{ print \\$1 }"`
lastChangeUser=`echo "$lastChange" | $gawkcmd -- "{ print \\$2 }"`

if [[ "$lastChangeNum" != "$change_list_num" ]] ; then

  if [ -n "$p4_passwd" ] ; then
    p4_cmd="$real_p4_client -u $p4_user -p $p4_server:$p4_port -P $p4_passwd"
  else
    p4_cmd="$real_p4_client -u $p4_user -p $p4_server:$p4_port"
  fi

  client_spec_exists=`$p4_cmd clients | $gawkcmd "{ print \\$2 }" | $grepcmd $client_spec`

  if [ -n "$client_spec_exists" ] ; then
    if [ -n "$verbose" ] ; then
      echo "\"$client_spec\" exists."
      echo "Running on $(hostname)"
    fi

    p4_cmd_with_client="$p4_cmd -c $client_spec"

    #poll for change list
    change_list_exists=
    p4Tries=0
    while [[ ( -z "$change_list_exists" ) && ( $p4Tries < 10 ) ]] ; do
      change_list_exists=`$p4_cmd_with_client "change" "-o" "$change_list_num" | \
          $grepcmd -e "^User:[[:space:]]\+"                  | \
          $sedcmd -e "s/User:[[:space:]]\+//"`
      p4Tries=$[$p4Tries+1]
      sleep 17
    done

    if [[ -n "$change_list_exists" ]] ; then

      triggerBuildOutput=`mktemp "$root_temp_dir/handlechange.XXXXXX"`


      triggerBuildCommand="$triggerBuild"
      triggerBuildCommand="$triggerBuildCommand \"--client-spec-name=$client_spec\""
      triggerBuildCommand="$triggerBuildCommand \"--root-temp-dir=$root_temp_dir\""
      triggerBuildCommand="$triggerBuildCommand \"--change-list-number=$change_list_num\""
      triggerBuildCommand="$triggerBuildCommand \"--output=$triggerBuildOutput\""
      triggerBuildCommand="$triggerBuildCommand \"--project-root=$project_root\""
      triggerBuildCommand="$triggerBuildCommand \"--perforce-server=$p4_server\""
      triggerBuildCommand="$triggerBuildCommand \"--perforce-port=$p4_port\""
      triggerBuildCommand="$triggerBuildCommand \"--perforce-client=$p4_client\""
      triggerBuildCommand="$triggerBuildCommand \"--perforce-password=$p4_passwd\""
      triggerBuildCommand="$triggerBuildCommand \"--perforce-user=$p4_user\""
      if [ -n "$verbose" ] ; then
        triggerBuildCommand="$triggerBuildCommand \"--verbose\""
        echo "$triggerBuildCommand"
      fi
      eval "$triggerBuildCommand"


      triggerBuildOutputLogFile=`mktemp "$root_temp_dir/handlechange.XXXXXX"`
      $tarcmd "-xjOf" "$triggerBuildOutput" &>$triggerBuildOutputLogFile

      getEntryFromOutputLog "$triggerBuildOutputLogFile" "BUILD_SUCCESS"
      buildSuccess="$ret"

      getEntryFromOutputLog "$triggerBuildOutputLogFile" "BUILD_HOST"
      buildHost="$ret"

      getEntryFromOutputLog "$triggerBuildOutputLogFile" "USER"
      changeUser="$ret"

      getEntryFromOutputLog "$triggerBuildOutputLogFile" "CHANGE_CLIENT"
      changeClient="$ret"

      resultLogFile="$log_dir/$client_spec.trigger.result.log"

      buildLogsDir="$log_dir/$client_spec.build.logs"

      lastEntry="0 0 root"

      if [[ -f "$resultLogFile" ]] ; then
        lastEntry=`tail -1 "$resultLogFile"`
      fi

      lastEntryChangeNum=`echo "$lastEntry" | $gawkcmd -- "{ print \\$1 }"`

      lastEntryBuildSuccess=`echo "$lastEntry" | $gawkcmd -- "{ print \\$2 }"`

      lastEntryChangeUser=`echo "$lastEntry" | $gawkcmd -- "{ print \\$3 }"`

      mailToUser=`mktemp $root_temp_dir/handlechange.XXXXXX`
      mailToCulprit=`mktemp $root_temp_dir/handlechange.XXXXXX`

      ldapsearch=$(which ldapsearch 2>/dev/null)
      if [ -x "$ldapsearch" ] ; then
          changeUserFirstName=`ldapsearch -x "uid=$changeUser" displayName | \
              grep -v "^#"                              | \
              grep displayName                          | \
              gawk -- "{ print \\$2 }"`
          
          changeUserFullName=`ldapsearch -x "uid=$changeUser" displayName  | \
              grep -v "^#"                               | \
              grep displayName                           | \
              sed -e "s/^displayName:[[:space:]]*//"`
          
          lastEntryChangeUserFirstName=`ldapsearch -x "uid=$lastEntryChangeUser" displayName | \
              grep -v "^#"                                                | \
              grep displayName                                            | \
              gawk -- "{ print \\$2 }"`
          lastEntryChangeUserFullName=`ldapsearch -x "uid=$lastEntryChangeUser" displayName  | \
              grep -v "^#"                                        | \
              grep displayName                                    | \
              sed -e "s/^displayName:[[:space:]]*//"`
      else
          changeUserFirstName=$changeUser
          changeUserFullName=$changeUser
          lastEntryChangeUserFirstName=$lastEntryChangeUser
          lastEntryChangeUserFullName=$lastEntryChangeUser
      fi

      if [[ "$buildSuccess" == "1" ]] ; then
        if [[ "$lastEntryBuildSuccess" != "1" ]] ; then

          if [[ "$lastEntryChangeUser" != "$changeUser" ]] ; then

            makeMail "$youFixed"                        \
                     "$mailToUser"                      \
                     "$changeUser"                      \
                     "$lastEntryChangeUser"             \
                     "$change_list_num"                 \
                     "$lastEntryChangeNum"              \
                     "$buildName"                       \
                     ""                                 \
                     ""                                 \
                     "$changeUserFirstName"             \
                     "$changeUserFullName"              \
                     "$lastEntryChangeUserFirstName"    \
                     "$lastEntryChangeUserFullName"

            makeMail "$theyFixed"                       \
                     "$mailToCulprit"                   \
                     "$changeUser"                      \
                     "$lastEntryChangeUser"             \
                     "$change_list_num"                 \
                     "$lastEntryChangeNum"              \
                     "$buildName"                       \
                     ""                                 \
                     ""                                 \
                     "$changeUserFirstName"             \
                     "$changeUserFullName"              \
                     "$lastEntryChangeUserFirstName"    \
                     "$lastEntryChangeUserFullName"

          else

            makeMail "$youFixedYours"                   \
                     "$mailToUser"                      \
                     "$changeUser"                      \
                     "$lastEntryChangeUser"             \
                     "$change_list_num"                 \
                     "$lastEntryChangeNum"              \
                     "$buildName"                       \
                     ""                                 \
                     ""                                 \
                     "$changeUserFirstName"             \
                     "$changeUserFullName"              \
                     "$lastEntryChangeUserFirstName"    \
                     "$lastEntryChangeUserFullName"
          fi
          echo "$change_list_num 1 $changeUser" >>$resultLogFile
          rm -rf "$buildLogsDir"
        fi
      else

        logArchive="$buildLogsDir/$change_list_num.tar.bz2"
        lastEntryLogArchive="$buildLogsDir/$lastEntryChangeNum.tar.bz2"

        if [[ "$lastEntryBuildSuccess" != "1" ]] ; then
          if [[ "$lastEntryChangeUser" != "$changeUser" ]] ; then
            makeMail "$youDidNotFix"                    \
                     "$mailToUser"                      \
                     "$changeUser"                      \
                     "$lastEntryChangeUser"             \
                     "$change_list_num"                 \
                     "$lastEntryChangeNum"              \
                     "$buildName"                       \
                     "$logArchive"                      \
                     "$lastEntryLogArchive"             \
                     "$changeUserFirstName"             \
                     "$changeUserFullName"              \
                     "$lastEntryChangeUserFirstName"    \
                     "$lastEntryChangeUserFullName"
            makeMail "$theyDidNotFix"                   \
                     "$mailToCulprit"                   \
                     "$changeUser"                      \
                     "$lastEntryChangeUser"             \
                     "$change_list_num"                 \
                     "$lastEntryChangeNum"              \
                     "$buildName"                       \
                     "$logArchive"                      \
                     "$lastEntryLogArchive"             \
                     "$changeUserFirstName"             \
                     "$changeUserFullName"              \
                     "$lastEntryChangeUserFirstName"    \
                     "$lastEntryChangeUserFullName"
          else
            makeMail "$youDidNotFixYours"               \
                     "$mailToUser"                      \
                     "$changeUser"                      \
                     "$lastEntryChangeUser"             \
                     "$change_list_num"                 \
                     "$lastEntryChangeNum"              \
                     "$buildName"                       \
                     "$logArchive"                      \
                     "$lastEntryLogArchive"             \
                     "$changeUserFirstName"             \
                     "$changeUserFullName"              \
                     "$lastEntryChangeUserFirstName"    \
                     "$lastEntryChangeUserFullName"
          fi
        else
          echo "$change_list_num 0 $changeUser" >>$resultLogFile
          makeMail "$youBroke"            \
                   "$mailToUser"          \
                   ""                     \
                   "$changeUser"          \
                   ""                     \
                   "$change_list_num"     \
                   "$buildName"           \
                   ""                     \
                   "$logArchive"          \
                   ""                     \
                   ""                     \
                   "$changeUserFirstName" \
                   "$changeUserFullName"
        fi
        if [[ ! ( -e "$buildLogsDir" && -d "$buildLogsDir" ) ]] ; then
          mkdir "$buildLogsDir"
        fi
        cp "$triggerBuildOutput" "$buildLogsDir/$change_list_num.tar.bz2"
        chmod og+r "$buildLogsDir/$change_list_num.tar.bz2"
      fi
      if [[ -f "$mailToUser" && -s "$mailToUser" ]] ; then
        if [[ "$changeUser" != "root" ]] ; then
          /usr/sbin/sendmail $changeUser@fulcrummicro.com <$mailToUser
          #echo "/usr/sbin/sendmail $changeUser@fulcrummicro.com <$mailToUser"
          #cat $mailToUser
        fi
      fi
      if [[ -f "$mailToCulprit" && -s "$mailToCulprit" ]] ; then
        if [[ "$lastEntryChangeUser" != "root" ]] ; then 
          /usr/sbin/sendmail $lastEntryChangeUser@fulcrummicro.com <$mailToCulprit
          #echo "/usr/sbin/sendmail $lastEntryChangeUser@fulcrummicro.com <$mailToCulprit"
          #cat $mailToCulprit
        fi
      fi

      echo "$change_list_num $changeUser" >$changeLogFile
    else
      echo "Change list \"$change_list_num\" does not seem to exist."
      exit 1
    fi
  else
    echo "Client Specification \"$client_spec\" does not appear to exist."
    exit 2
  fi
else
  /bin/true
fi
