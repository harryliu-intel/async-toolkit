#!/bin/sh
UNAME=uname
TR=tr

OS=`${UNAME} -s`
PROCESSOR=`${UNAME} -m`
# FreeBSD Specific Assumptions at work
VERSION=`${UNAME} -r`
MAJORNUM=`echo $VERSION | awk 'BEGIN {FS=""} {print $1}'`

# Very inaccurate OS detection
if [  "x$OS" = "xFreeBSD" ]; then
  if [ "x$PROCESSOR" = "xi386" ]; then
    if [ "x$MAJORNUM" = "x3" ]; then
      M3ARCH="FreeBSD3"
    else
      M3ARCH="FreeBSD4"
    fi
  fi
elif [ "x$OS" = "xLinux" ]; then
  M3ARCH="LINUXLIBC6"
else
  exit 1
fi
echo $M3ARCH
