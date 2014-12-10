#!/bin/bash
# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$

arch_bin_dir=${0%\/*}
packageRoot=${arch_bin_dir%\/*}
shLibDir="$packageRoot/share/script/sh/sh-lib"

archBinDir="$packageRoot/bin"

umask 000
source "$shLibDir/file/filecheck.sh"
source "$shLibDir/file/conon.sh"
source "$shLibDir/script/generate_script_with_libs.sh"

function usage() {
  echo "Usage: $0 "
  echo "  --cell=cellName"
  echo "  --lib=libName"
  echo "  --dfII-dir=dir"
  echo "  --fulcrum-pdk-root=dir"
  echo "  --cast-path=path"
  echo "  --working-dir=dir"
  echo "  [ --def-output=cell.def ]"
  echo "  [ --cadence-log=exportDesignDef.log]"
  echo "  [ --view=layout ]"
  echo "  [ --verbose ]"
  echo "  [ --64 ]"
}

layout=`mywhich layoutPlus` || exit 2
sedcmd=`mywhich sed` || exit 2
gawkcmd=`mywhich gawk` || exit 2
grepcmd=`mywhich grep` || exit 2
bashcmd=`mywhich bash` || exit 2
findcmd=`mywhich find` || exit 2

check_readable_dir "$archBinDir" \
    "Package arch bin: \"$archBinDir\" is not a readable directory." 2

cdsShLib="$packageRoot/share/script/sh/util"
check_readable_dir "$cdsShLib" \
    "Cadence Shell Script Library: \"$cdsShLib\" is not a readable directory." 2

cdsShLibFiles=`$findcmd "$cdsShLib" \! -type d`
for file in $cdsShLibFiles ; do
  source "$file"
done

cadenceName=
cell=
lib=
dfIIDir=
fulcrumPDKRoot=
castPath=
workingDir=
defOutput="cell.def"
bindRulOutput=/dev/null
outputRootCellName=
cadenceLog=exportDesignDef.log
view="layout"
verbose=
debug=
scale=
preservePins=1
bit64=
OutputNets="nil"
AllSpecialNets="nil"
PowerPins="nil"

if [ -n "$CDS_AUTO_64BIT" -a "$CDS_AUTO_64BIT" = "ALL" ]; then
    bit64=1
fi

for arg in $@ ; do
  
  case "$arg" in
  --cell=* )
    cell=`echo "$arg" | $sedcmd -e "s/--cell=//"`
    ;;
  --lib=* )
    lib=`echo "$arg" | $sedcmd -e "s/--lib=//"`
    ;;
  --dfII-dir=* )
    dfIIDir=`echo "$arg" | $sedcmd -e "s/--dfII-dir=//"`
    ;;
  --fulcrum-pdk-root=* )
    fulcrumPDKRoot=`echo "$arg" | $sedcmd -e "s/--fulcrum-pdk-root=//"`
    ;;
  --cast-path=* )
    castPath=`echo "$arg" | $sedcmd -e "s/--cast-path=//"`
    ;;
  --working-dir=* )
    workingDir=`echo "$arg" | $sedcmd -e "s/--working-dir=//"`
    ;;
  --def-output=* )
    defOutput=`echo "$arg" | $sedcmd -e "s/--def-output=//"`
    ;;
  --cadence-log=* )
    cadenceLog=`echo "$arg" | $sedcmd -e "s/--cadence-log=//"`
    ;;
  --view=* )
    view=`echo "$arg" | $sedcmd -e "s/--view=//"`
    ;;
  --verbose )
    verbose=1
    ;;
  --OutputNets )
    OutputNets="t"
    ;;
  --AllSpecialNets )
    AllSpecialNets="t"
    ;;
  --PowerPins )
    PowerPins="t"
    ;;
  --debug )
    debug=1
    ;;
  --64 )
    bit64=1
    ;;
  --* )
    echo "Unknown argument: \"$arg\"."
    usage
    exit 2
    ;;
  esac
done

if [ -n "$debug" ] ; then
  verbose=1
fi

check_for_empty_arg "$packageRoot"                                               \
    "The packageroot variable must contain the location of the package installation."           2
check_for_empty_arg "$cell"                                                  \
    "The cell that you want to lef & def out must be specified."                                   2
check_for_empty_arg "$dfIIDir"                                                   \
    "You must specify the location of directory containing all the dfII data."                  2
check_for_empty_arg "$fulcrumPDKRoot"                                            \
    "You must specify the location of the fulcrum pdk you want to use."                         2
check_for_empty_arg "$castPath"                                                  \
    "You must specify a cast-path"                                                              2
check_for_empty_arg "$workingDir"                                                \
    "You must specify a working-dir"                                                            2
check_for_empty_arg "$cadenceLog"                                                \
    "You must specify a cadence log file."                                                      2
check_for_empty_arg "$view"                                                      \
    "You must specify the view of \"$cell\" that you want to def out."              2

if [[ ( ! ( -w "$workingDir" && -d "$workingDir" ) ) ]] ; then
  echo "Can't write and delete in $workingDir."
  exit 2
fi

check_readable_dir "$packageRoot"                                                \
    "Package Installation: \"$packageRoot\" is not a readable directory."              2

check_readable_dir "$dfIIDir"                                                   \
    "dfII directory: \"$dfIIDir\" is not a readable, writeable directory."             1
conon_path "$dfIIDir"
dfIIDir="$ret"

check_readable_dir "$fulcrumPDKRoot"                                             \
    "Fulcrum PDK: \"$fulcrumPDKRoot\" is not a readable directory."                    2
conon_path "$fulcrumPDKRoot"
fulcrumPDKRoot="$ret"

check_writeable_file "$defOutput"                                              \
    "Def Output File: \"$defOutput\" can not be written to."                       1
conon_path "$defOutput"
defOutput="$ret"

if [ "$cadenceLog" != "/dev/null" ] ; then
  check_writeable_file "$cadenceLog"                                             \
      "Cadence Log File: \"$cadenceLog\" is not a writeable file."                     1
  conon_path "$cadenceLog"
  cadenceLog="$ret"
fi

skillRoot="$packageRoot/share/skill"
check_readable_dir  "$skillRoot"                                                 \
    "Skill Root Directory: \"$skillRoot\" is not a readable directory."                2

mkcdswd="$archBinDir/mkcdswd"
check_executable_file "$mkcdswd"                                                 \
    "mkcdswd: \"$mkcdswd\" is not an executable file."                                 2

skillAutoLoad="$skillRoot/autoload.il"
check_readable_file "$skillAutoLoad"                                             \
   "Skill Autoload: \"$skillAutoLoad\" is not a readable file."                        2

pdkInfoIL="$fulcrumPDKRoot/share/Fulcrum/pdkinfo.il"
check_readable_file "$pdkInfoIL"                                                 \
   "pdkinfo.il: \"$pdkInfoIL\' is not a readable file."                                2


cdsWD=`mktemp -d $workingDir/lefdef.XXXXXX`

if [ -n "$verbose" ] ; then
  echo "Making cadence working directory."
  echo "$mkcdswd \"--dfII-dir=$dfIIDir\" \"--fulcrum-pdk-root=$fulcrumPDKRoot\" \"--cast-path=$castPath\" \"--target-dir=$cdsWD\" \"--force\" \"--temp\""
fi

templateCdsWd=
if [ -n "$CDS_WD_TEMPLATE" ] ; then
    templateCdsWd="--user-template=$CDS_WD_TEMPLATE"
fi

$mkcdswd "--dfII-dir=$dfIIDir" \
         "--fulcrum-pdk-root=$fulcrumPDKRoot" \
         "--cast-path=$castPath" \
         "--target-dir=$cdsWD" \
         "--force" "--temp" "$templateCdsWd"

mainIL=`mktemp $workingDir/lefdef.XXXXXX`

tempLibDir=`mktemp -d $workingDir/lefdef.XXXXXX`
lefTempFile=`mktemp "$workingDir/lef.XXXXXX"`

if [ -n "$verbose" ] ; then
  echo "Generating skill program."
  verbose="t"
else 
  verbose="nil"

fi

cat <<SKILL>$mainIL
(unless ( equal "" "$debug" ) ( ilDebugToolBox ))
( load "$skillAutoLoad" )
( load "$pdkInfoIL" )
defStatus=defDefOut( "$lib" 
           "$cell"
           "$view"
           "$defOutput"
           ?Verbose $verbose
           ?OutputNets $OutputNets 
           ?AllSpecialNets $AllSpecialNets 
           ?PowerPins $PowerPins
)
Status = (cond ( !defStatus 1 ) ( 0 ) )
(when ( equal "" "$debug" ) ( exit Status ) )
SKILL

cadenceLogTemp=`mktemp "$workingDir/cadenceLogTemp.XXXXXX"`
cadenceScript=`mktemp "$workingDir/cadenceScript.XXXXXX"`
cadenceErr=`mktemp "$workingDir/cadenceErr.XXXXXX"`

if [ -n "$bit64" ] ; then
CDS_AUTO_64BIT=ALL
else
CDS_AUTO_64BIT=NONE
fi

graphics=
if [ -z "$debug" ] ; then
    graphics="-nograph"
fi

cat <<EOF>"$cadenceScript"
#!/bin/bash
cd "$cdsWD"
CDS_AUTO_64BIT=$CDS_AUTO_64BIT $layout \
-replay "$mainIL" -log "$cadenceLogTemp" $graphics \
 </dev/null 1>/dev/null 2>"$cadenceErr"
EOF

chmod +x "$cadenceScript"

"$cadenceScript"

if [ $? != 0 ] ; then
  cat "$cadenceErr" 1>&2
  echo "ERROR: layoutPlus returned with non-zero exit status" 1>&2
fi

cp "$cadenceLogTemp" "$cadenceLog"

