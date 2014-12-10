#!/bin/bash
# $Id$
# $DateTime$
# $Author$

<<DOC
Converts gds2 and cdl/verilog into CAST and DFII.
The outputs are:
<ul>
<li> cast - tar of the digital cast
<li> spec - tar of the spec cast
<li> dfII - tar of the generated dfII - layout and floorplan
</ul>

Most importantly, we need to get the .cxl name mapping file from the cdl vs. gdsII.  This is then passed to cdl2cast, which uses it to make SKILL name tables (used by <skill GDSIIHierCopyAndMungeLibraryForImportUsingPDKInfo>) consistent with the cdl and the the bind rul.

<h2> This script uses </h2>
<dl>
<dt> com.avlsi.tools.cdl2cast.Cdl2Cast
<dd>
<dt> <skill GDSIIHierCopyAndMungeLibraryForImportUsingPDKInfo>
<dd> SKILL that implements the name mappings that cdl2cast creates.
<dt> com.avlsi.tools.cdl2cast.Cast2Cdl
<dd> To create cdl for optional final lvs checks.
<dt> <tool updatenetlist>
<dd> To create floorplans.
<dt> <package ve>
<dd> For running lvs/nvn.
</dl>
DOC

arch_bin_dir=${0%\/*}
packageRoot=${arch_bin_dir%\/*}

function exit_func() {
  if [ -n "$debug" -o -n "$verbose" ] ; then
      exit
  fi
  if [ -d "$tempDir" ] ; then
    rm -rf "$tempDir"
  fi
}

trap exit_func EXIT

function usage() {
    cat <<USAGE
Usage: $0
OUTPUT
    --cast=file
    --spec=file
    --dfII=file
INPUTS
    --gdsII=file
    [ --verilog=file [ --verilog=file [ ... ] ] ]
    [ --cdl=file [ --cdl=file [ ... ] ] ]
OPTIONS
    --fulcrum-pdk-root=dir
    --standard-cast-dir=dir (contains base.cast,etc)
    --lib=name (default library for output)
    --sub-type=num (default subtype for output)
    [--cell=cell]
      (do all cells in gds2/cdl if not specified)
    [ --refinement-parent=STD_CELL ]
      (the cell that all the digital CAST refines from)
    [ --meters-per-input-unit=1 ]
      (scaling for cdl input)
    [ --bind-rul=/dev/null ]
      (maps cdl names to final CAST names, overrides lib/subtype for those cellnames with mappings in the bind-rul)
    [ --post-stream-in-skill=file ]
      (arbitrary skill file to execute in layout after streaming in gds2 but before names are changed)
    [ --post-import-skill=file ]
      (arbitrary skill file to execute in layout after names are changed)
    [ --assura-run=file ]
      (tar of gds2 vs original cdl lvs run)
    [ --assura-verilog-check-run=file ]
      (tar of verilog vs cdl generated from generated CAST!)
    [ --assura-check-run=file]
      (tar of generated dfII vs cdl generated from generated CAST!)
    [ --debug ]
      (use SKILL debugger and don't delete working directory - recommended)
    [ --verbose ]
    [ --max-heap-size=size ]
USAGE
}

shLibDir="$packageRoot/share/script/sh/sh-lib"
source "$shLibDir/file/filecheck.sh"
source "$shLibDir/file/conon.sh"

sedcmd=`which sed`
grepcmd=`which grep`
awkcmd=`which grep`
perlcmd=`which perl`
tarcmd=`which tar`
check_executable_file "$sedcmd" "Unable to find sed in \"$PATH\"" 2
check_executable_file "$awkcmd" "Unable to find awk in \"$PATH\"" 2
check_executable_file "$grepcmd" "Unable to find grep in \"$PATH\"" 2
check_executable_file "$perlcmd" "Unable to find perl in \"$PATH\"" 2
check_executable_file "$tarcmd" "Unable to find tar in \"$PATH\"" 2

source "$packageRoot/share/script/sh/util/parsecellname"

archBinDir="$packageRoot/bin"

check_readable_dir "$archBinDir" "\"$archBinDir\" is not a readable directory." 2

mkcdswd="$archBinDir/mkcdswd"
check_executable_file "$mkcdswd" "\"$mkcdswd\" is not an executable file." 2

skillRoot="$packageRoot/share/skill"
check_readable_dir  "$skillRoot"                                                 \
    "Skill Root Directory: \"$skillRoot\" is not a readable directory."                2

skillAutoLoad="$skillRoot/autoload.il"
check_readable_file "$skillAutoLoad"                                             \
   "Skill Autoload: \"$skillAutoLoad\" is not a readable file."                        2

vfeCmd="$packageRoot/share/script/perl/ve/front-end/vfe"

updatenetlist="$archBinDir/updatenetlist"
cdl2cast="$archBinDir/cdl2cast"
cast2cdl="$archBinDir/cast2cdl"
check_executable_file "$updatenetlist" "\"$updatenetlist\" is not an executable file." 2
check_executable_file "$cdl2cast" "\"$cdl2cast\" is not an executable file." 2
check_executable_file "$cast2cdl" "\"$cast2cdl\" is not an executable file." 2

all_cells=1
cell=
inputGdsIIFile=
castFile=
specFile=
dfIIFile=
pdkRoot=
metersPerInputUnit=1
bindRul=/dev/null
defaultOutputLibName=
subType=
postStreamInIL=
postImportIL=
verilogFiles=
cdlFiles=
assuraRunTarFile=
assuraCheckRunTarFile=
assuraVerilogCheckRunTarFile=
standardCastDir=
verbose=
debug=
tempDirRoot="/scratch"
tempDir=
gndNode=VSS
vddNode=VDD
refinementParent=STD_CELL
maxHeapSize="1800M"
layFilters=
schFilters=
doUpdateNetlist=0

for arg in "$@"; do
    case "$arg" in
    --updatenetlist=*)
        doUpdateNetlist=`echo "$arg" | $sedcmd -e "s/--updatenetlist=//"`
        ;;
    --lay-filters=*)
        layFilters=`echo "$arg" | $sedcmd -e "s/--lay-filters=//"`
        ;;
    --sch-filters=*)
        schFilters=`echo "$arg" | $sedcmd -e "s/--sch-filters=//"`
        ;;
    --cell=*)
        cell=`echo "$arg" | $sedcmd -e "s/--cell=//"`
        all_cells=0
        ;;
    --gdsII=*)
        inputGdsIIFile=`echo $arg | $sedcmd -e "s/--gdsII=//"`
        ;;
    --cast=*)
        castFile=`echo $arg | $sedcmd -e "s/--cast=//"`
        ;;
    --spec=*)
        specFile=`echo $arg | $sedcmd -e "s/--spec=//"`
        ;;
    --dfII=*)
        dfIIFile=`echo $arg | $sedcmd -e "s/--dfII=//"`
        ;;
    --fulcrum-pdk-root=*)
        pdkRoot=`echo $arg | $sedcmd -e "s/--fulcrum-pdk-root=//"`
        ;;
    --meters-per-input-unit=*)
        metersPerInputUnit=`echo $arg | $sedcmd -e "s/--meters-per-input-unit=//"`
        ;;
    --refinement-parent=*)
        refinementParent=`echo $arg | $sedcmd -e "s/--refinement-parent=//"`
        ;;
    --gnd-node=*)
        gndNode=`echo $arg | $sedcmd -e "s/--gnd-node=//"`
        ;;
    --vdd-node=*)
        vddNode=`echo $arg | $sedcmd -e "s/--vdd-node=//"`
        ;;
    --bind-rul=*)
        bindRul=`echo $arg | $sedcmd -e "s/--bind-rul=//"`
        ;;
    --lib=*)
        defaultOutputLibName=`echo $arg | $sedcmd -e "s/--lib=//"`
        ;;
    --sub-type=*)
        subType=`echo $arg | $sedcmd -e "s/--sub-type=//"`
        ;;
    --post-stream-in-skill=*)
        postStreamInIL=`echo $arg | $sedcmd -e "s/--post-stream-in-skill=//"`
        ;;
    --post-import-skill=*)
        postImportIL=`echo $arg | $sedcmd -e "s/--post-import-skill=//"`
        ;;
    --verilog=*)
        thisVerilogFile=`echo "$arg" | $sedcmd -e "s/--verilog=//"`
        verilogFiles="$verilogFiles $thisVerilogFile"
        ;;
    --cdl=*)
        thisCDLFile=`echo "$arg" | $sedcmd -e "s/--cdl=//"`
        cdlFiles="$cdlFiles $thisCDLFile"
        ;;
    --assura-run=*)
        assuraRunTarFile=`echo "$arg" | $sedcmd -e "s/--assura-run=//"`
        ;;
    --assura-verilog-check-run=*)
        assuraVerilogCheckRunTarFile=`echo "$arg" | $sedcmd -e "s/--assura-verilog-check-run=//"`
        ;;
    --assura-check-run=*)
        assuraCheckRunTarFile=`echo "$arg" | $sedcmd -e "s/--assura-check-run=//"`
        ;;
    --standard-cast-dir=*)
        standardCastDir=`echo "$arg" | $sedcmd -e "s/--standard-cast-dir=//"`
        ;;
    --verbose)
        verbose=1
        ;;
    --debug)
        debug=1
        ;;
    --temp-dir=*)
        tempDirRoot=`echo "$arg" | $sedcmd -e "s/--temp-dir=//"`
        ;;
    --max-heap-size=*)
        maxHeapSize=`echo "$arg" | $sedcmd -e 's/--max-heap-size=//'`
        ;;
    --libFile=*)
        libFile=`echo "$arg" | $sedcmd -e 's/--libFile=//'`
        ;;
    *)
        echo "Unknown argument: \"$arg\""
        ;;
    esac
done

if [ -n "$debug" ] ; then
  verbose=1
fi

check_for_empty_arg "$inputGdsIIFile" "You must specify a gdsII file containing layout for \"$cell\"." 2
check_for_empty_arg "$castFile" "You must specify a file to contain the generated cast tree." 2
check_for_empty_arg "$specFile" "You must specify a file to contain the generated spec tree." 2
check_for_empty_arg "$dfIIFile" "You must specify a file to contain the generated dfII tree." 2
check_for_empty_arg "$pdkRoot" "You must specify to location of a fulrcum PDK installation." 2
check_for_empty_arg "$metersPerInputUnit" "You must specify the number of meters per input unit." 2
check_for_empty_arg "$bindRul" "You must specify a bind.rul." 2
check_for_empty_arg "$defaultOutputLibName" "You must specify a default output library name." 2
check_for_empty_arg "$subType" "You must specify an output subtype." 2
check_for_empty_arg "$tempDirRoot" "You must specify a directory to contain temporary files." 2
check_for_empty_arg "$libFile" "You must specify a .lib file for getting pin directions" 2

check_readable_file "$inputGdsIIFile" "\"$inputGdsIIFile\" is not a readable file." 1
conon_path "$inputGdsIIFile"
inputGdsIIFile="$ret"

check_readable_file "$libFile" "\"$libFile\" is not a readable file." 1
conon_path "$libFile"
libFile="$ret"

check_writeable_file "$castFile" "\"$castFile\" is not a writable file." 1
conon_path "$castFile"
castFile="$ret"

check_writeable_file "$specFile" "\"$specFile\" is not a writable file." 1
conon_path "$specFile"
specFile="$ret"

check_writeable_file "$dfIIFile" "\"$dfIIFile\" is not a writable file." 1
conon_path "$dfIIFile"
dfIIFile="$ret"

check_readable_dir "$pdkRoot" "\"$pdkRoot\" is not a readable directory." 1
conon_path "$pdkRoot"
pdkRoot="$ret"

if [[ "$bindRul" != "/dev/null" ]] ; then
  check_readable_file "$bindRul" "\"$bindRul\" is not a readable file." 1
  conon_path "$bindRul"
  bindRul="$ret"
fi

if [ -n "$postStreamInIL" ] ; then
  check_readable_file "$postStreamInIL" "\"$postStreamInIL\" is not a readable file." 1
  conon_path "$postStreamInIL"
  postStreamInIL="$ret"
fi

if [ -n "$postImportIL" ] ; then
  check_readable_file "$postStreamInIL" "\"$postImportIL\" is not a readable file." 1
  conon_path "$postImportIL"
  postImportIL="$ret"
fi

check_writeable_dir "$tempDirRoot" "\"$tempDirRoot\" is not a writable directory." 1
conon_path "$tempDir"
tempDirRoot="$ret"
tempDirRoot=`echo $tempDirRoot | sed -e 's:/$::'`

streamInLayerMap="$pdkRoot/share/Fulcrum/stream/strmin.layermap"
check_readable_file "$streamInLayerMap" "\"$streamInLayerMap\" is not a readable file." 2

streamInTemplate="$pdkRoot/share/Fulcrum/stream/strmin.template"
check_readable_file "$streamInTemplate" "\"$streamInTemplate\" is not a readable file." 2

pdkInfoIL="$pdkRoot/share/Fulcrum/pdkinfo.il"
check_readable_file "$pdkInfoIL" "pdkinfo.il: \"$pdkInfoIL\' is not a readable file." 2

pdkAssuraDir="$pdkRoot/share/Fulcrum/assura"
check_readable_dir "$pdkAssuraDir" "\"$pdkAssuraDir\" is not a readable directory." 2

#lvsRSFInclude="$pdkAssuraDir/LVSinclude.rsf"
#check_readable_file "$lvsRSFInclude" "\"$lvsRSFInclude\" is not a readable file." 2

#lvsExtractRules="$pdkAssuraDir/extract.rul"
#check_readable_file "$lvsExtractRules" "\"$lvsExtractRules\" is not a readable file." 2

#lvsCompareRules="$pdkAssuraDir/compare.rul"
#check_readable_file "$lvsCompareRules" "\"$lvsCompareRules\" is not a readable file." 2

lvsBindingRules="$pdkAssuraDir/bind.rul"
check_readable_file "$lvsBindingRules" "\"$lvsBindingRules\" is not a readable file." 2

############ create temporary working dir ##############

tempDir=`echo $inputGdsIIFile | sed -e 's/.*\///' -e 's/\..*//'`
tempDir="$tempDirRoot/importPR.$tempDir"
if [ -n "$verbose" ]; then
  echo $tempDir
fi
/bin/rm -rf "$tempDir"
mkdir -p "$tempDir"

############ get a schematic file from cdl/verilog  ##############

gdsIIFile=$tempDir/cell.gds2
fixGdsCmd="fixgds '$inputGdsIIFile' '$gdsIIFile'";
if [ -n "$verbose" ]; then
  echo "$fixGdsCmd"
fi
eval "$fixGdsCmd"
check_readable_file "$gdsIIFile" "\"$gdsIIFile\" is not a readable file." 2

convertedCDLFile=$tempDir/cell.cdl
fixCdlCmd="fixcdl '$thisCDLFile' '$convertedCDLFile'";
if [ -n "$verbose" ]; then
  echo "$fixCdlCmd"
fi
eval "$fixCdlCmd"
check_readable_file "$convertedCDLFile" "\"$convertedCDLFile\" is not a readable file." 2

############ stream-in the gds2 ##############

streamInWD=$tempDir/streamInWD
mkdir -p $streamInWD 2>/dev/null
dfIIDir=$tempDir/dfII
mkdir -p $dfIIDir 2>/dev/null
if [ -n "$verbose" ]; then
  echo DFII $dfIIDir
fi

makeStreamInWDCmd="$mkcdswd \"--target-dir=$streamInWD\""
makeStreamInWDCmd="$makeStreamInWDCmd \"--dfII-dir=$dfIIDir\""
makeStreamInWDCmd="$makeStreamInWDCmd \"--fulcrum-pdk-root=$pdkRoot\""
makeStreamInWDCmd="$makeStreamInWDCmd \"--force\""
makeStreamInWDCmd="$makeStreamInWDCmd \"--temp\""

if [ -n "$verbose" ] ; then
  echo "$makeStreamInWDCmd"
fi

eval "$makeStreamInWDCmd"
makeStreamInWDRet=$?

if [[ "$makeStreamInWDRet" != "0" ]] ; then
  echo "mkcdswd FAILED" 1>&2
  exit "$makeStreamInWDRet"
fi

# we just create an empty directory
mkdir -p "$streamInWD/STREAMIN"

echo "DEFINE STREAMIN $streamInWD/STREAMIN" >>$streamInWD/cds.lib
mkdir -p $streamInWD/$defaultOutputLibName
cdslibname=`echo $defaultOutputLibName | sed -e 's/\./#2e/g'`;
echo "DEFINE $cdslibname $streamInWD/$defaultOutputLibName" >> $streamInWD/cds.lib

strminConfig=$tempDir/strmin.cfg
strminWD=$tempDir/streamInWD
mkdir -p $strminWD 2>/dev/null

echo "stick 0 0.1" > $strminWD/textFontTable.txt

if [[ $all_cells == 1 ]] ; then
    $sedcmd  \
        -e "s=STRMINLAYERMAP=$streamInLayerMap =" \
        -e "s=LIBRARY=STREAMIN=" \
        -e "s=STRMINTECH==" \
        -e "s=STRMINATTACH=tsmc28=" \
        -e "s=STRMINLOG=StrmIn.log=" \
        -e "s=STRMINGDS=$gdsIIFile=" \
        -e "s=STRMINTECHOUT=/dev/null=" \
        -e "s=STRMINTOPCELL==" \
        -e "s=STRMINVIEW=layout=" \
        -e 's=textFontTable.*""=textFontTable\t"textFontTable.txt"=' \
        $streamInTemplate|grep -v topCell >$strminConfig
else
    $sedcmd \
        -e "s=STRMINLAYERMAP=$streamInLayerMap =" \
        -e "s=LIBRARY=$defaultOutputLibName=" \
        -e "s=STRMINTECH==" \
        -e "s=STRMINLOG=/dev/null=" \
        -e "s=STRMINGDS=$gdsIIFile=" \
        -e "s=STRMINTECHOUT=/dev/null=" \
        -e "s=STRMINTOPCELL=$cell=" \
        -e "s=STRMINVIEW=layout=" \
        -e 's=textFontTable.*""=textFontTable\t"textFontTable.txt"=' \
        $streamInTemplate >$strminConfig
fi

strminOutput=$tempDir/strmin.out

strminCmd="strmin"
strminCmd="$strminCmd -templateFile \"$strminConfig\""
strminCmd="$strminCmd &>$strminOutput"

if [ -n "$verbose" ] ; then
    echo "$strminCmd"
fi

pushd "$streamInWD" >/dev/null
eval "$strminCmd"
strminRet=$?
popd >/dev/null

if [[ "$strminRet" != "0" ]] ; then
  echo "strmin FAILED!!!" 1>&2
  cat $strminOutput 1>&2
  exit "$strminRet"
fi



############ Run post-stream in SKILL ##############

postStreamInRPFile=
postStreamInLog=
if [ -n "$postStreamInIL" ] ; then
  postStreamInRPFile=$tempDir/streamIn.rpt
  postStreamInLog=$tempDir/streamIn.log
  if [ -n "$debug" ] ; then
    echo "( ilDebugToolBox )" >>$postStreamInRPFile
  fi
  echo "( load \"$skillAutoLoad\" )" >>$postStreamInRPFile
  echo "( load \"$pdkInfoIL\" )" >>$postStreamInRPFile
  echo "( load \"$postStreamInIL\" )" >>$postStreamInRPFile
  echo "( ImportPRPostStreamIn \"STREAMIN\" \"$cell\" \"layout\" )" >>$postStreamInRPFile
  if [ -z "$debug" ] ; then
    echo "( exit )" >>$postStreamInRPFile
  fi

  postStreamInCmd="layout"
  postStreamInCmd="$postStreamInCmd -replay \"$postStreamInRPFile\""
  postStreamInCmd="$postStreamInCmd -log \"$postStreamInLog\""
  if [ -z "$debug" ] ; then
    postStreamInCmd="$postStreamInCmd -nograph"
  fi
  postStreamInCmd="$postStreamInCmd </dev/null &>/dev/null"
  if [ -n "$verbose" ] ; then
    echo "$postStreamInCmd"
  fi
  pushd "$streamInWD" >/dev/null
  echo "$postStreamInCmd" 1>&2
  eval "$postStreamInCmd"
  popd >/dev/null
fi

############ Run cdl2cast ##############

layoutToCDLBindRul=/dev/null
cdlToLayoutBindRul=$tempDir/cdl2layoutbind.rul
if [ ! -s "$cdlToLayoutBindRul" ]; then
    touch "$cdlToLayoutBindRul"
    for cell in `awk '/^\.subckt/i {print $2}' $cdlFiles`; do
       echo "C $cell $library.$cell.$subType" >> "$cdlToLayoutBindRul"
    done
fi

specTreeDir=$tempDir/specTreeDir
mkdir -p $specTreeDir 2>/dev/null
castTreeDir=$tempDir/castTreeDir
mkdir -p $castTreeDir 2>/dev/null
skillDir=$tempDir/skill
mkdir -p $skillDir 2>/dev/null
finalCastCellList=$tempDir/finalCastCellList.txt

cdl2castCmd="$cdl2cast --max-heap-size=$maxHeapSize"
cdl2castCmd="$cdl2castCmd \"--cdl-file=$convertedCDLFile\""
cdl2castCmd="$cdl2castCmd \"--output-spec=$specTreeDir\""
cdl2castCmd="$cdl2castCmd \"--output-cast=$castTreeDir\""
cdl2castCmd="$cdl2castCmd \"--name-table-dir=$skillDir\""
cdl2castCmd="$cdl2castCmd \"--vdd-node=$vddNode\""
cdl2castCmd="$cdl2castCmd \"--gnd-node=$gndNode\""
cdl2castCmd="$cdl2castCmd \"--refinement-parent=$refinementParent\""
if [ -n "$defaultOutputLibName" ] ; then
  cdl2castCmd="$cdl2castCmd \"--lib-name=$defaultOutputLibName\""
fi
if [ -n "$subType" ] ; then
  cdl2castCmd="$cdl2castCmd \"--sub-type=$subType\""
fi
cdl2castCmd="$cdl2castCmd \"--meters-per-input-unit=$metersPerInputUnit\""
cdl2castCmd="$cdl2castCmd \"--layout-to-cdl-bind-rul=$layoutToCDLBindRul\""
cdl2castCmd="$cdl2castCmd \"--output-cdl-to-layout-bind-rul=$cdlToLayoutBindRul\""
cdl2castCmd="$cdl2castCmd \"--output-cast-cells=$finalCastCellList\""
cdl2castCmd="$cdl2castCmd \"--bind-rul-header=$lvsBindingRules\""
cdl2castCmd="$cdl2castCmd \"--bind-rul-in=$bindRul\""


if [ -n "$verbose" ] ; then
  echo "$cdl2castCmd"
fi
eval "$cdl2castCmd"
cdl2castRet=$?

if [[ "$cdl2castRet" != "0" ]] ; then
  echo "cdl2cast FAILED" 1>&2
  exit "$cdl2castRet"
fi

for file in `find "$tempDir" -name '*.cast'` ; do
   sed -e 's/, GND, Vdd//' "$file" > "$tempDir/tcast"
   mv "$tempDir/tcast" "$file"
done

############ Munge the dfII names to what cdl2cast says ##############

if [ 1 ]; then
    pushd "$streamInWD/STREAMIN" 2>/dev/null
    for dir in *; do
        if [ -d $dir ]; then
            mv "$dir" "${cdslibname}#2e${dir}#2e$subType";
        fi
    done
    libdir=`echo "$defaultOutputLibName" | sed -e 's:\.:/:g'`
    libdir="$dfIIDir/$libdir/$defaultOutputLibName"
    mkdir -p "$libdir" 2>/dev/null
    mv "$streamInWD/STREAMIN/"/* "$libdir"
    popd 2>/dev/null
else
importRPFile=$tempDir/importRPFile

if [ -n "$debug" ] ; then
  echo "( ilDebugToolBox )" >>$importRPFile
fi
echo "( load \"$skillAutoLoad\" )" >>$importRPFile
echo "( load \"$pdkInfoIL\" )" >>$importRPFile

echo "( GDSIIHierCopyAndMungeLibraryForImportUsingPDKInfo" >>$importRPFile
echo "  \"$skillDir\"" >>$importRPFile
echo "  \"$dfIIDir\"" >>$importRPFile
echo "  \"STREAMIN\"" >>$importRPFile
echo "  ( ListApplyFuncToListAndAccumulateResults" >>$importRPFile
echo "    ( getq ( ddGetObj \"STREAMIN\" ) cells )" >>$importRPFile
echo "    (lambda" >>$importRPFile
echo "      ( CellDDObj )" >>$importRPFile
echo "      ( list" >>$importRPFile
echo "        ( getq CellDDObj name )" >>$importRPFile
echo "        ( sprintf" >>$importRPFile
echo "          nil" >>$importRPFile
echo "          \"$defaultOutputLibName.%s.$subType\"" >>$importRPFile
echo "          ( getq CellDDObj name ) ) ) )" >>$importRPFile
echo "    nil ) )" >>$importRPFile

if [ -n "$postImportIL" ] ; then
  echo "( load \"$postImportIL\" )" >>$importRPFile
  echo "( ImportPRPostImport " >>$importRPFile
  echo "  \"$skillDir\"" >>$importRPFile
  echo "  \"$dfIIDir\"" >>$importRPFile
  echo "  \"STREAMIN\"" >>$importRPFile
  echo "  \"$cell\" " >>$importRPFile
  echo "  \"layout\" )" >>$importRPFile
fi

importLog=$tempDir/import.log

importCmd="layout"
importCmd="$importCmd -replay \"$importRPFile\""
importCmd="$importCmd -log \"$importLog\""
if [ -z "$debug" ] ; then
  importCmd="$importCmd -nograph"
fi
importCmd="$importCmd </dev/null &>/dev/null"

if [ -n "$verbose" ] ; then
  echo "$importCmd"
fi

pushd "$streamInWD" >/dev/null
eval "$importCmd"
popd >/dev/null
fi


############ Get final names ##############

finalCastCell=$(head -1 $finalCastCellList)
finalLayoutCell=$(grep "^C $cell .*" "$cdlToLayoutBindRul" | $awkcmd '{print $3}')
get_lib_name "$finalLayoutCell"
finalLayoutLib="$ret"


doUpdateNetlist=
if [ $doUpdateNetlist ]; then
############ Create floorplan views with updatenetlist #############

updatenetlistCadenceLog=$tempDir/CDS.log
updatenetlistCmd="$updatenetlist --java-max-heap-size=$maxHeapSize"
updatenetlistCmd="$updatenetlistCmd \"--dfII-dir=$dfIIDir\""
updatenetlistCmd="$updatenetlistCmd \"--cast-path=$castTreeDir:$specTreeDir:$standardCastDir\""
if [[ $all_cells == 1 ]] ; then
    #tell updatenetlist to do each cell
    updatenetlistCmd="$updatenetlistCmd \"--cell-list=$finalCastCellList\""
else
    updatenetlistCmd="$updatenetlistCmd \"--cell=$defaultOutputLibName.$cell\""
fi
updatenetlistCmd="$updatenetlistCmd \"--subtype=$subType\""
updatenetlistCmd="$updatenetlistCmd \"--suppress-pins\""
updatenetlistCmd="$updatenetlistCmd \"--suppress-netlist-view\""
updatenetlistCmd="$updatenetlistCmd \"--lock-layout\""
updatenetlistCmd="$updatenetlistCmd \"--update-views\""
updatenetlistCmd="$updatenetlistCmd \"--fulcrum-pdk-root=$pdkRoot\""
updatenetlistCmd="$updatenetlistCmd \"--cadence-log=$updatenetlistCadenceLog\""
if [ -n $verbose ]; then
updatenetlistCmd="$updatenetlistCmd \"--verbose\""
fi

if [ -n "$verbose" ] ; then
  echo "$updatenetlistCmd"
fi

eval "$updatenetlistCmd"
status=$?

if [[ "$status" != "0" ]] ; then
    echo "updatenetlist FAILED" 1>&2
    exit "$status"
fi

fi

if [[ (( -n "$assuraVerilogCheckRunTarFile" ) || \
      ( -n "$assuraCheckRunTarFile" )) &&
      ( $all_cells == 1 ) ]] ;  then

    finalCDLFile=$tempDir/cell.cdl_final

    cast2cdlCmd="$cast2cdl --max-heap-size=$maxHeapSize"
#    cast2cdlCmd="$cast2cdlCmd \"--cdl-file=$finalCDLFile\""
    cast2cdlCmd="$cast2cdlCmd \"--cdl-file=av28m_hvt_lvs.sp.mod\""
    cast2cdlCmd="$cast2cdlCmd \"--cell=$finalCastCell\""
    cast2cdlCmd="$cast2cdlCmd \"--cast-path=$castTreeDir:$standardCastDir:$specTreeDir\""
    cast2cdlCmd="$cast2cdlCmd \"--output=$finalCDLFile\""

    if [ -n "$verbose" ] ; then
        echo "$cast2cdlCmd"
    fi

    eval "$cast2cdlCmd"
    status=$?

    if [[ "$status" != "0" ]] ; then
        echo "cast2cdl failed"
    exit "$status"
    fi

    # Optionally LVS the final cdl against the original verilog
    if [ -n "$assuraVerilogCheckRunTarFile" ] ; then
        lvsRunDir=$tempDir/lvsRunDir
        mkdir -p $lvsRunDir 2>/dev/null
        lvsOutput=$tempDir/lvs.out

        lvsCmd="$vfeCmd"
        lvsCmd="$lvsCmd \"VerificationType=AssuraNvn\""
        lvsCmd="$lvsCmd \"AssuraRsfInclude=$lvsRSFInclude\""
        lvsCmd="$lvsCmd \"CompareFile=$lvsCompareRules\""
        lvsCmd="$lvsCmd \"BindingFile=$cdlToLayoutBindRul\""
        lvsCmd="$lvsCmd \"RunName=lvs\""
        lvsCmd="$lvsCmd \"LayoutFile.=$finalCDLFile:cdl\""
        for verilogFile in $verilogFiles ; do
            lvsCmd="$lvsCmd \"SchematicFile.=$verilogFile:verilog\""
        done
        for cdlFile in $cdlFiles ; do
            lvsCmd="$lvsCmd \"SchematicFile.=$cdlFile:cdl\""
        done
        lvsCmd="$lvsCmd \"WorkingDir=$lvsRunDir\""
        lvsCmd="$lvsCmd &>$lvsOutput"

        if [ -n "$verbose" ] ; then
            echo "$lvsCmd"
        fi

        eval "$lvsCmd"
        lvsRet=$?

        if [[ "$lvsRet" != "0" ]] ; then
            echo "ve FAILED" 1>&2
            cat "$lvsOutput"
            exit "$lvsRet"
        fi

        lvsPass=`$grepcmd -e "^PASS:" $lvsOutput`

        if [[ "$lvsPass" != "PASS:" ]] ; then
            echo "verilog-layout lvs FAILED" 1>&2
            exit 1
        fi

        $tarcmd -cjf "$assuraVerilogCheckRunTarFile" -C "$lvsRunDir" .
    fi

    # Optionally LVS the final layout against the final cdl
    if [ -n "$assuraCheckRunTarFile" ] ; then

        lvsRunDir=$tempDir/lvsRunDir2
        mkdir -p $lvsRunDir 2>/dev/null
        lvsOutput=$tempDir/lvsOutput.out

        lvsCmd="$vfeCmd"
        lvsCmd="$lvsCmd \"VerificationType=AssuraLvs\""
        lvsCmd="$lvsCmd \"AssuraRsfInclude=$lvsRSFInclude\""
        lvsCmd="$lvsCmd \"CdsLib=$streamInWD/cds.lib\""
        lvsCmd="$lvsCmd \"LayoutCell=$finalLayoutCell\""
        lvsCmd="$lvsCmd \"LayoutLibrary=$finalLayoutLib\""
        lvsCmd="$lvsCmd \"LayoutView=layout\""
        lvsCmd="$lvsCmd \"RuleFile=$lvsExtractRules\""
        lvsCmd="$lvsCmd \"CompareFile=$lvsCompareRules\""
        lvsCmd="$lvsCmd \"BindingFile=$lvsBindingRules\""
        lvsCmd="$lvsCmd \"RunName=lvs\""
        lvsCmd="$lvsCmd \"SchematicFile=$finalCDLFile:cdl\""
        lvsCmd="$lvsCmd \"WorkingDir=$lvsRunDir\""
        lvsCmd="$lvsCmd &>$lvsOutput"

        if [ -n "$verbose" ] ; then
            echo "$lvsCmd"
        fi

        eval "$lvsCmd"
        lvsRet=$?

        if [[ "$lvsRet" != "0" ]] ; then
            echo "ve FAILED" 1>&2
            cat "$lvsOutput"
            exit "$lvsRet"
        fi

        lvsPass=`$grepcmd -e "^PASS:" $lvsOutput`

        if [[ "$lvsPass" != "PASS:" ]] ; then
            echo "verilog-layout lvs FAILED" 1>&2
            exit 1
        fi

        $tarcmd -cjf "$assuraCheckRunTarFile" -C "$lvsRunDir" .
    fi
fi

fixPortCmd="fixportdirection --libfile=$libFile --search-dir=$tempDir"
if [ -n "$verbose" ] ; then
  echo "$fixPortCmd"
fi
eval "$fixPortCmd"

castTarCmd="$tarcmd -cjf \"$castFile\""
castTarCmd="$castTarCmd -C \"$castTreeDir\" ."

if [ -n "$verbose" ] ; then
  echo "$castTarCmd"
fi

eval "$castTarCmd"

specTarCmd="$tarcmd -cjf \"$specFile\""
specTarCmd="$specTarCmd -C \"$specTreeDir\" ."

if [ -n "$verbose" ] ; then
  echo "$specTarCmd"
fi

eval "$specTarCmd"

echo DFII $dfIIDir
dfIITarCmd="$tarcmd -cjf \"$dfIIFile\""
dfIITarCmd="$dfIITarCmd -C \"$dfIIDir\" ."

if [ -n "$verbose" ] ; then
  echo "$dfIITarCmd"
fi

eval "$dfIITarCmd"
