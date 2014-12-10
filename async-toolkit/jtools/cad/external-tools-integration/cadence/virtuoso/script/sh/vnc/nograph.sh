#!/bin/bash

#
# Runs $@ from a vncserver
# Usefual if $@ needs an X server, but you don't want to deal with physical
# terminals
#


function exit_func() {
 if [[ ( -n "$pid" ) && ( `ps --pid $pid | grep $pid`) ]] ; then
    echo "Killing VNC display $display"
    kill -9 $pid
  fi
}

trap exit_func EXIT

arch_bin_dir=${0%\/*}
package_root=${arch_bin_dir%\/*}
fvwmrc="$package_root/share/script/sh/vnc/.fvwm2/Main-8-bit"
getvncserver="$package_root/share/script/sh/vnc/getvncserver.sh"

ret=`$getvncserver`

if [ $? -ne 0 ] ; then
  echo "Couldn't get VNC server"
  exit 1
else
  pid=`echo $ret | awk '{print $1}'`
  display=`echo $ret | awk '{print $2}'`
  echo "Running on VNC display $display"
  echo "Xvnc pid=$pid"
  DISPLAY=$display xsetroot -solid grey
  DISPLAY=$display fvwm2 -f $fvwmrc &
  DISPLAY=$display xhost + >/dev/null
  DISPLAY=$display "$@"
fi
