#!/bin/bash

function exit_func() {
  if [ -f "$pidfile" ] ; then
    rm $pidfile
  fi
}

trap exit_func EXIT

vncserver=`which Xvnc`

pidfile=`mktemp /tmp/vnc.XXXXXX`

if [ -x $vncserver ]; then
  ret=
  displaynum=1
  while [[ ( -z "$ret" ) && ( $displaynum -lt 100 ) ]] ; do
    $vncserver :$displaynum -depth 8 -cc 3 -fp "unix/:7100" -geometry 1024x768 &>/dev/null & echo $! > $pidfile
    usleep 1000
    pid=`cat $pidfile`
    ret=`ps --pid $pid | grep $pid`    
    if [ -z "$ret" ] ; then
      displaynum=$[$displaynum+1]
    fi
  done
  
  if [ -z "$ret" ] ; then
    echo "Couldn't get a VNC display"
    exit 1
  else
    display="$HOSTNAME:$displaynum"
    echo "`cat $pidfile` $display"
  fi
else
  echo "$vncserver is not an executable file"
  exit 2
fi
