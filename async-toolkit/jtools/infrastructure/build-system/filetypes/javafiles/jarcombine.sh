#!/bin/sh

findcmd=$1
jarcmd=$2
resultfile=$3

shift
shift
shift

if [ -n "$jarcmd" ] ; then
  if [ -n "$resultfile" ] ; then
    tempdir=`mktemp -d /tmp/jarcombineXXXXXX`

    manifest_tmp_cpy=`mktemp /tmp/jarcombineXXXXXX`

    if [[ -d "$tempdir" && -f "$manifest_tmp_cpy" ]] ; then
      pushd "$tempdir"

      if [ -f "$1" ] ; then
        manifest_jar_file=$1
        $jarcmd -xf $manifest_jar_file
        cp $tempdir/META-INF/MANIFEST.MF $manifest_tmp_cpy
        shift
      fi

      for arg in "$@" ; do
        if [ -f "$arg" ] ; then
          $jarcmd -xf $arg
        fi
      done
      popd

      sub_dirs=`$findcmd "$tempdir" -mindepth 1 -maxdepth 1 -type d -print`
      base_names=

      for dir in $sub_dirs ; do
        base_name=`basename $dir`
        if [ "$base_name" != "META-INF" ] ; then
          base_names="$base_names -C $tempdir $base_name"
        fi
      done

      if [ ! -z "$base_names" ] ; then
        echo "$jarcmd -cfm $resultfile $manifest_tmp_cpy $base_names"
        $jarcmd -cfm $resultfile $manifest_tmp_cpy $base_names
      else
        echo "Nothing to put into the result jar file."
      fi
    else
      echo "Unable to create temporary directory."
    fi
    if [ -d "$tempdir" ] ; then
      rm -r $tempdir
    fi
    if [ -f "$manifest_tmp_cpy" ]; then
      rm $manifest_tmp_cpy
    fi
  else
    echo "You did not specify a result file name."
    exit 1
  fi
else
  echo "You did not specify the command that should be used to run the jar program."
  exit 1
fi
