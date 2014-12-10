#!/bin/bash
# $Id$
# $DateTime$
# $Author$

<<DOC
<p>Converts a verilog file into a cdl file, using netReader/vldb2spice assura tools.  Used by <tool importPR> for converting verilog from vendors into cdl, which will eventually turn into CAST using com.avlsi.tools.cdl2cast.Cdl2Cast.</p>
DOC

arch_bin_dir=${0%\/*}
package_root=${arch_bin_dir%\/*}
debug=

function exit_func() {
    if [ -z "$debug" ]; then 
        if [ -n "$netReaderCmds" ] ; then
            rm -f "$netReaderCmds"
        fi
        if [ -n "$vldbFile" ] ; then
            rm -f "$vldbFile"
        fi
        if [ -n "$vldbToSpiceDir" ] ; then
            rm -rf "$vldbToSpiceDir"
        fi
        if [ -n "$spiceFile" ] ; then
            rm -f "$spiceFile"
        fi
    fi
}

trap exit_func EXIT

function usage() {
  echo "Usage: $0 "
  echo "  --cell=cell"
  echo "  --output-cdl=file"
  echo "  [ --verilog=file [ --verilog=file [ ... ] ] ]"
  echo "  [ --cdl=file [ --cdl=file [ ... ] ] ]"
  echo "  [ --net-reader-log=/dev/null ]"
  echo "  [ --vldb-to-spice-log=/dev/null ]"
  echo "  [ --verbose ]"
}

shLibDir="$package_root/share/script/sh/sh-lib"
source "$shLibDir/file/filecheck.sh"
source "$shLibDir/file/conon.sh"

vldbToSpice=`mywhich vldbToSpice` || exit 2
netReader=`mywhich netReader` || exit 2
sedcmd=`mywhich sed` || exit 2

verilogFiles=
cdlFiles=
outputCDLFile=
cell=
netReaderLog="/dev/null"
vldbToSpiceLog="/dev/null"
verbose=
# One should not use /tmp for temp files /tmp is for the operating
# system's /tmp.  All temp files created by this script
# go in $tempDir which can be set by the --temp-dir option.
tempDir="/scratch"

for arg in "$@"; do
    case "$arg" in
    --verilog=*)
        thisVerilogFile=`echo "$arg" | $sedcmd -e "s/--verilog=//"`
        verilogFiles="$verilogFiles $thisVerilogFile"
        ;;
    --cdl=*)
        thisCDLFile=`echo "$arg" | $sedcmd -e "s/--cdl=//"`
        cdlFiles="$cdlFiles $thisCDLFile"
        ;;
    --output-cdl=*)
        outputCDLFile=`echo "$arg" | $sedcmd -e "s/--output-cdl=//"`
        ;;
    --cell=*)
        cell=`echo "$arg" | $sedcmd -e "s/--cell=//"`
        ;;
    --net-reader-log=*)
        netReaderLog=`echo "$arg" | $sedcmd -e "s/--net-reader-log=//"`
        ;;
    --vldb-to-spice-log=*)
        vldbToSpiceLog=`echo "$arg" | $sedcmd -e "s/--vldb-to-spice-log=//"`
        ;;
    --verbose)
        verbose=1
        debug=1
        ;;
    --temp-dir=*)
        tempDir=`echo "$arg" | $sedcmd -e "s/--temp-dir=//"`
        ;;
    *)
        echo "Unknown argument: \"$arg\""
        ;;
    esac
done

check_for_empty_arg "$outputCDLFile" "You must specify an output file name." 2

check_for_empty_arg "$netReaderLog" "You must specify a log file for netReader." 2
check_for_empty_arg "$vldbToSpiceLog" "You must specify a  log file for vldbToSpice" 2
check_for_empty_arg "$tempDir" "You must specify a directory to contain temporary files." 2

check_writeable_file "$outputCDLFile" "Output CDL: \"$outputCDLFile\" is not a writable file." 1
conon_path "$outputCDLFile"
outputCDLFile="$ret"

if [[ "$netReaderLog" != "/dev/null" ]] ; then
  check_writeable_file "$netReaderLog" "netReader Log: \"$netReaderLog\" is not a writable file." 1
  conon_path "$netReaderLog"
  netReaderLog="$ret"
fi
if [[ "$vldbToSpiceLog" != "/dev/null" ]] ; then
  check_writeable_file "$vldbToSpiceLog" "vldbToSpice Log: \"$vldbToSpiceLog\" is not a writable file." 1
  conon_path "$vldbToSpiceLog"
  vldbToSpiceLog="$ret"
fi

check_writeable_dir "$tempDir" "\"$tempDir\" is not a writable directory." 1
conon_path "$tempDir"
tempDir="$ret"

netReaderCmds=`mktemp $tempDir/verilog2cdl.XXXXXX`

for file in $cdlFiles ; do
  check_readable_file "$file" "\"$file\" is not a readable file." 1
  conon_path "$file"
  file="$ret"
  if [ -n "$verbose" ] ; then
    echo "cdl $file"
  fi
  echo "cdl $file" >>$netReaderCmds
done

for file in $verilogFiles ; do
  check_readable_file "$file" "\"$file\" is not a readable file." 1
  conon_path "$file"
  file="$ret"
  if [ -n "$verbose" ] ; then
    echo "verilog $file"
  fi
  echo "verilog $file" >>$netReaderCmds
done

if [ -n "$verbose" ] ; then
  echo "topCell $cell"
fi
if [ -n "$cell" ] ; then
    echo "topCell $cell" >>$netReaderCmds
fi

vldbFile=`mktemp $tempDir/verilog2cdl.XXXXXX`
if [ -n "$verbose" ] ; then
  echo "file $vldbFile"
fi
echo "file $vldbFile" >>$netReaderCmds

if [ -n "$verbose" ] ; then
  echo "exit"
fi
echo "exit" >>$netReaderCmds

netReaderCmd="cat $netReaderCmds | $netReader &>$netReaderLog"

if [ -n "$verbose" ] ; then
  echo "$netReaderCmd"
fi

eval "$netReaderCmd"
netReaderRet=$?

if [[ "$netReaderRet" != "0" ]] ; then
  echo "netReader FAILED" >/proc/self/fd/2
  exit $netReaderRet
fi

vldbToSpiceDir=`mktemp -d $tempDir/verilog2cdl.XXXXXX`

spiceFile=`mktemp $tempDir/verilog2cdl.XXXXXX`

vldbToSpiceCmd="pushd \"$vldbToSpiceDir\" >/dev/null ; "
vldbToSpiceCmd="$vldbToSpiceCmd $CADENCE_WRAPPER_SCRIPT $vldbToSpice \"$vldbFile\" \"$spiceFile\" &>$vldbToSpiceLog ;"
vldbToSpiceCmd="$vldbToSpiceCmd popd >/dev/null"

if [ -n "$verbose" ] ; then
  echo "$vldbToSpiceCmd"
fi

eval "$vldbToSpiceCmd"
vldbToSpiceRet=$?

if [[ "$vldbToSpiceRet" != "0" ]] ; then
  echo "vldbToSpice FAILED" 1>2
  exit "$vldbToSpiceRet"
fi

#HACK HACK
#1. Prepend all instances with an extra x, because vldbToSPice uses the first character
#  of instance names that begin with X as the instation operator X in the CDL format.
# Ex: If there is an instance name X8_foo vldbToSpice will emit:
# .
# .
# .
# X8_foo ...
# .
# .
# .
# Rather than xX8_foo or XX8_foo.  Instance names that don't begin with X such 
# as Blah are emited as xBlah.
#2. Fulcrum uses '.' as the hierarchy seperator, verilog generally uses
#  '/', so replace all '/'es with '.'.  vldbToSpice emits all '/'es as "\/" so
# replace all instances of "\/" with '.'
#3 Get rid of all '\'es that precede an identifer.
#4 Get rid of all '\'es that precede an instance name
spiceToCDLCmd="$sedcmd -e \"s/^X/xX/\""
spiceToCDLCmd="$spiceToCDLCmd -e \"s,\\\\\/,.,g\""
spiceToCDLCmd="$spiceToCDLCmd -e \"s/[[:space:]]\+\\\\\/ /g\""
spiceToCDLCmd="$spiceToCDLCmd -e \"s/^[xX]\\\\\/x/\""
spiceToCDLCmd="$spiceToCDLCmd \"$spiceFile\" >$outputCDLFile"

if [ -n "$verbose" ] ; then
  echo "$spiceToCDLCmd"
fi
eval  "$spiceToCDLCmd"
