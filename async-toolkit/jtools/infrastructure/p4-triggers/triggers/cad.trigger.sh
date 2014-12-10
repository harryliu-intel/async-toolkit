#!/bin/bash
# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$

function exit_func() {
  if [ -n "$tempFile" ] ; then
    rm -f "$tempFile"
  fi
}


trap exit_func EXIT

arch_bin_dir=${0%\/*}
package_root=${arch_bin_dir%\/*}

handleChange="$package_root/bin/handlechange"


logDir="$package_root/logs"

log_dir="$package_root/logs"

if [[ ! -e "$log_dir" ]] ; then
  mkdir "$log_dir"
fi

clientSpec="clayton-cad-trigger"
changeNum="$1"
projectRoot="sw/cad"
perforcePasswd="foo"
perforceUser="clayton"
buildName="cad"

if [[ -f "$handleChange" && -x "$handleChange" ]] ; then

  tempFile=`mktemp /tmp/cad.trigger.XXXXXX`

  if [ -n "$changeNum" ] ; then
    echo "#$ -l sw-cad-p4-trigger=1" >$tempFile
    echo "$handleChange \\" >>$tempFile
    echo "  --client-spec-name=$clientSpec \\" >>$tempFile
    echo "  --change-list-number=$changeNum \\" >>$tempFile
    echo "  --project-root=$projectRoot \\" >>$tempFile
    echo "  --perforce-password=$perforcePasswd \\" >>$tempFile
    echo "  --perforce-user=$perforceUser \\" >>$tempFile
    echo "  --build-name=$buildName \\" >>$tempFile
    echo "  --verbose" >>$tempFile

    chmod 700 "$tempFile"

    $tempFile

  fi

fi
