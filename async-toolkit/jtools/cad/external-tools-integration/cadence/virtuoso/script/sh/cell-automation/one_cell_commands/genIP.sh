#!/bin/bash
# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$


<<DOC
 <pre>
genIP is a script to generate a bzip2 compressed
tar file of all the things we need to send to a
customer that is buying one of our circuits.

From a high level genIP does the following:
1. Parse and validate command line arguments
2. Call jauto to get strength report.
3. Call GenerateIPData java program to generate CDL,
   verilog models, skill name tables for gdsII writing,
   and rename the contents of the strength report to use
   the names in the CDL, verilog, and GDSII.
4. Generate a shell script to run PIPO to stream DFII data to GDSII.
5. Generate a skill program that uses the skill name tables to
   munge all the layout and the pipo script to stream the munged
   layout to GDSII.
6. Put all the required files in the output tar archive
 </pre>
DOC

arch_bin_dir=${0%\/*}
packageRoot=${arch_bin_dir%\/*}
shLibDir="$packageRoot/share/script/sh/sh-lib"


if [ -z "$CADENCE_WRAPPER_SCRIPT" ] ; then
  CADENCE_WRAPPER_SCRIPT="fulcrum --pdk=nv90 ic icc assura "
fi

function exit_func() {
  #echo "Deleting temporary files";
  #delete_nfs_temp_files
  #delete_local_temp_files
  /bin/true
}

function delete_nfs_temp_files() {
  if [ -n "$ipDataDir" ] ; then
    rm -rf "$ipDataDir"
  fi
  if [ -n "$pipoConf0" ] ; then
    rm -f "$pipoConf0"
  fi
  if [ -n "$pipoConf1" ] ; then
    rm -f "$pipoConf1"
  fi
  if [ -n "$pipoWD" ] ; then
    rm -rf "$pipoWD"
  fi
  if [ -n "$pipoLog" ] ; then
    rm -f "$pipoLog"
  fi
  if [ -n "$gdsIIOutput" ] ; then
    rm -f "$gdsIIOutput"
  fi
  if [ -n "$pipoScript" ] ; then
    rm -f "$pipoScript"
  fi
  if [ -n "$cdsWD" ] ; then
    rm -rf "$cdsWD"
  fi
  if [ -n "$mainIL" ] ; then
    rm -f "$mainIL"
  fi
  if [ -n "$tempLibDir" ] ; then
    rm -rf "$tempLibDir"
  fi
  
}

function delete_local_temp_files() {
  if [ -n "$jautoToilet" ] ; then
    rm -rf "$jautoToilet"
  fi
  if [ -n "$inputBindRul" ] ; then
    rm -f "$inputBindRul"
  fi
  if [ -n "$genIPDataOutput" ]; then
    rm -f "$genIPDataOutput"
  fi
  if [ -n "$verilogTree" ] ; then
    rm -rf "$verilogTree"
  fi
  if [ -n "$tarDir" ] ; then
    rm -rf "$tarDir"
  fi
}

#prs2verilog uses macros to instantiate
#logic gates.  This function generates
#the definitions of those macros. Each
#macro is defined to use its corresponding
#built-in verilog operator.
function make_verilog_macro_definitions() {

  srcVerilogFile="$1"
  outputVerilogFile="$2"
  blackBoxCellName="$3"
  
  echo "" >$outputVerilogFile

  andGateTypes=`$grepcmd -e "^\\\`AND[1-9][0-9]*" "$srcVerilogFile" | \
                    $sedcmd -e "s/^\\\`[a-zA-Z]\+//g"   | \
                    $sortcmd -n                         | \
                    $uniqcmd`
  for typeNum in $andGateTypes ; do
    echo "\`define AND$typeNum and" >>$outputVerilogFile 
  done

  nandGateTypes=`$grepcmd -e "^\\\`NAND[1-9][0-9]*" "$srcVerilogFile" | \
                      $sedcmd -e "s/^\\\`[a-zA-Z]\+//g"   | \
                      $sortcmd -n                         | \
                      $uniqcmd`
  for typeNum in $nandGateTypes ; do
    echo "\`define NAND$typeNum nand" >>$outputVerilogFile 
  done

  orGateTypes=`$grepcmd -e "^\\\`OR[1-9][0-9]*" "$srcVerilogFile" | \
                  $sedcmd -e "s/^\\\`[a-zA-Z]\+//g"   | \
                  $sortcmd -n                        | \
                  $uniqcmd`
  for typeNum in $orGateTypes ; do
    echo "\`define OR$typeNum or" >>$outputVerilogFile 
  done

  norGateTypes=`$grepcmd -e "^\\\`NOR[1-9][0-9]*" "$srcVerilogFile" | \
                    $sedcmd -e "s/^\\\`[a-zA-Z]\+//g"   | \
                    $sortcmd -n                        | \
                    $uniqcmd`
  for typeNum in $norGateTypes ; do
    echo "\`define NOR$typeNum nor" >>$outputVerilogFile 
  done
  
  echo "\`define INV not" >>$outputVerilogFile
  echo "\`define BLACKBOX $blackBoxCellName" >>$outputVerilogFile
  
}

#Delete tempfiles on exit.
trap exit_func EXIT

localWorkingDir="$TEMP"

archBinDir="$packageRoot/bin"

rename="$archBinDir/rename"
source "$shLibDir/file/filecheck.sh"
source "$shLibDir/file/conon.sh"
source "$shLibDir/file/config.sh"
source "$shLibDir/script/generate_script_with_libs.sh"

function usage() {
  echo "Usage: $0 "
  echo "  --cell=cell"
  echo "  --fulcrum-pdk-root=dir"
  echo "  --temp-dir=dir"
  echo "  --dfII-dir=dir"
  echo "  --cast-path=path"
  echo "  --version=str"
  echo "  [--verilog-test-benches-dir=file ]" 
  echo "  [ --output=cell.tar.bz2 ]"
  echo "  [ --custom-bind-rul=file ]"
  echo "  [ --cadence-log=file]"
  echo "  [ --view=layout ]"
  echo "  [ --doc-dir=dir ]"
  echo "  [ --verbose ]"
  echo "  [ --debug-java ]"
}

sedcmd=`which sed`
gawkcmd=`which gawk`
grepcmd=`which grep`
bashcmd=`which bash`
findcmd=`which find`
tarcmd=`which tar`
sortcmd=`which sort`
uniqcmd=`which uniq`

check_executable_file "$sedcmd" "Unable to find sed in \"$PATH\"" 2
check_executable_file "$gawkcmd" "Unable to find gawk in \"$PATH\"" 2
check_executable_file "$grepcmd" "Unable to fine grep in \"$PATH\"" 2
check_executable_file "$bashcmd" "Unable to find bash in \"$PATH\"" 2
check_executable_file "$findcmd" "Unable to find find in \"$PATH\"" 2
check_executable_file "$tarcmd" "Unable to find tar in \"$PATH\"" 2
check_executable_file "$sortcmd" "Unable to find sort in \"$PATH\"" 2
check_executable_file "$uniqcmd" "Unable to find uniq in \"$PATH\"" 2


check_readable_dir "$archBinDir" \
    "Package arch bin: \"$archBinDir\" is not a readable directory." 2

cdsSHLib="$packageRoot/share/script/sh/util"
check_readable_dir "$cdsSHLib" \
    "Cadence Shell Script Library: \"$cdsSHLib\" is not a readable directory." 2

cdsSHLibFiles=`$findcmd "$cdsSHLib" \! -type d`
for file in $cdsSHLibFiles ; do
  source "$file"
done

cadenceRootCell=
rootCell=
fulcrumPDKRoot=
localWorkingDir=
castPath=
versionStr=
verilogBlackBox=
verilogTestBenchesDir=
outputFile=
customBindRul=
cadenceLog=/dev/null
view="layout"
docDir=
pipoRun=
verbose=
debugJava=
javaMaxHeapSize="1800M"


for arg in $@ ; do
  
  case "$arg" in
  --cell=* )
    rootCell=`echo $arg | $sedcmd -e "s/--cell=//"`
    ;;
  --fulcrum-pdk-root=* )
    fulcrumPDKRoot=`echo $arg | $sedcmd -e "s/--fulcrum-pdk-root=//"`
    ;;
  --temp-dir=* )
    localWorkingDir=`echo $arg | $sedcmd -e "s/--temp-dir=//"`
    ;;
  --dfII-dir=* )
    dfIIDir=`echo $arg | $sedcmd -e "s/--dfII-dir=//"`
    ;;
  --cast-path=* )
    castPath=`echo $arg | $sedcmd -e "s/--cast-path=//"`
    ;;
  --version=* )
    versionStr=`echo $arg | $sedcmd -e "s/--version=//"`
    ;;
  --verilog-test-benches-dir=* )
    verilogTestBenchesDir=`echo $arg | $sedcmd -e "s/--verilog-test-benches-dir=//"`
    ;;
  --output=* )
    outputFile=`echo $arg | $sedcmd -e "s/--output=//"`
    ;;
  --custom-bind-rul=* )
    customBindRul=`echo $arg | $sedcmd -e "s/--custom-bind-rul=//"`
    ;;
  --cadence-log=* )
    cadenceLog=`echo $arg | $sedcmd -e "s/--cadence-log=//"`
    ;;
  --view=* )
    view=`echo $arg | $sedcmd -e "s/--view=//"`
    ;;
  --doc-dir=* )
    docDir=`echo $arg | $sedcmd -e "s/--doc-dir=//"`
    ;;
  --pipo-run=*)
    pipoRun=`echo $arg | $sedcmd -e "s/--pipo-run=//"`
    ;;
  --verbose )
    verbose=1
    ;;
  --debug-java )
    debugJava=1
    ;;
  --java-max-heap-size=* )
    javaMaxHeapSize=`echo "$arg" | $sedcmd -e "s/--java-max-heap-size=//"`
    ;;
  --* )
    echo "Unknown argument: \"$arg\"."
    usage
    exit 2
    ;;
  esac
done
 
cadenceRootCell=$(echo "$rootCell" | $rename --type=cell --from=cast --to=cadence)
if [ -z "$localWorkingDir" ] ; then
  localWorkingDir="/scratch/"
fi

check_for_empty_arg "$packageRoot"                                                   \
    "The packageroot variable must contain the location of the package installation."           2
check_for_empty_arg "$rootCell"                                                      \
    "The cell that you want to stream out must be specified."                                   2
check_for_empty_arg "$fulcrumPDKRoot"                                                \
    "You must specify the location of the fulcrum pdk you want to use."                         2
check_for_empty_arg "$localWorkingDir"                                               \
    "You must specify an NFS working directory for temporary files generated by this script."   2
check_for_empty_arg "$dfIIDir"                                                       \
    "You must specify the location of directory containing all the dfII data."                  2
check_for_empty_arg "$castPath"                                                      \
    "You must specify a cast path"                                                              2
check_for_empty_arg "$versionStr"                                                    \
    "You must specify a version string."                                                        2
check_for_empty_arg "$cadenceLog"                                                    \
    "You must specify a cadence log file."                                                      2
check_for_empty_arg "$view"                                                          \
    "You must specify the view of \"$rootCell\" that you want to stream to GDSII."              2

check_for_empty_arg "$javaMaxHeapSize"                                               \
     "You must specify a maximum java heap size."                                               2

check_readable_dir "$packageRoot"                                                    \
    "Package Installation: \"$packageRoot\" is not a readable directory."                       2

check_readable_dir "$fulcrumPDKRoot"                                                 \
    "Fulcrum PDK: \"$fulcrumPDKRoot\" is not a readable directory."                             2
conon_path "$fulcrumPDKRoot"
fulcrumPDKRoot="$ret"

check_writeable_dir "$localWorkingDir"                                               \
    "working directory: \"$localWorkingDir\" is not a readable, writeable directory."           1
conon_path "$localWorkingDir"
localWorkingDir="$ret"

check_readable_dir "$dfIIDir"                                                       \
    "dfII directory: \"$dfIIDir\" is not a readable, writeable directory."                      1
conon_path "$localWorkingDir"
localWorkingDir="$ret"

conon_path "$verilogTestBenchesDir"
verilogTestBenchesDir="$ret"

if [ -n "$customBindRul" ] ; then
  check_readable_file "$customBindRul"                                               \
     "Custom Bind.rul: \"$customBindRul\" is not a readable file."                              1
  conon_path "$customBindRul"
  customBindRul="$ret"
else
  customBindRul="/dev/null"
fi


if [ -z "$outputFile" ] ; then
  outputFile=`pwd`
  outputFile="$outputFile/$rootCell.tar.bz2"
fi

check_writeable_file "$outputFile"                                                  \
    "Output File: \"$outputFile\" can not be written to."                                       1
conon_path "$outputFile"
outputFile="$ret"

if [ "$cadenceLog" != "/dev/null" ] ; then
  check_writeable_file "$cadenceLog"                                                \
      "Cadence Log File: \"$cadenceLog\" is not a writeable file."                              1
  conon_path "$cadenceLog"
  cadenceLog="$ret"
fi

if [ -n "$docDir" ] ; then
  check_readable_dir "$docDir"                                                      \
      "Documentation Directory: \'$docDir\" is not a readable directory."                       1
  conon_path "$docDir"
  docDir="$ret"
fi

if [ -n "$pipoRun" ] ; then
  check_writeable_file "$pipoRun" "PIPO Run: \"$pipoRun\" is not a writeable file."             1
  conon_path "$pipoRun"
  pipoRun="$ret"
fi

templatesDir="$packageRoot/share/script/sh/cell-automation/templates"
check_readable_dir "$templatesDir"                                                  \
    "Templates Directory: \"$templatesDir\" is not a readable directory."                       2

skillRoot="$packageRoot/share/skill"
check_readable_dir  "$skillRoot"                                                    \
    "Skill Root Directory: \"$skillRoot\" is not a readable directory."                         2

generateIPData="$archBinDir/generate_ip_data"
check_executable_file "$generateIPData"                                             \
    "generateIPData: \"$generateIPData\" is not an executable file."                            2

jauto="$archBinDir/jauto"
check_executable_file "$jauto"                                                      \
    "jauto: \"$jauto\" is not an executable file."                                              2

mkcdswd="$archBinDir/mkcdswd"
check_executable_file "$mkcdswd"                                                    \
    "mkcdswd: \"$mkcdswd\" is not an executable file."                                          2

techBindRul="$fulcrumPDKRoot/share/Fulcrum/assura/bind.rul"
check_readable_file "$techBindRul"                                                  \
    "Assura LVS name bindings: \"$techBindRul\" is not a readable file."                        2

presizeConfig="$fulcrumPDKRoot/share/Fulcrum/jauto/presize.config"
check_readable_file "$presizeConfig"                                                \
    "Jauto Config: \"$presizeConfig\" is not a readable file."                                  2

processConfig="$fulcrumPDKRoot/share/Fulcrum/jauto/process.config"
check_readable_file "$processConfig"                                                \
    "Process Config: \"$processConfig\" is not a readable file."                                  2

verilogGLMConfig="$fulcrumPDKRoot/share/Fulcrum/prs2verilog/glm.config"
check_readable_file "$verilogGLMConfig"                                            \
    "Verilog GLM Config file: \"$verilogGLMConfig\" is not a readable file."             2

verilogELMConfig="$fulcrumPDKRoot/share/Fulcrum/prs2verilog/elm.config"
check_readable_file "$verilogELMConfig"                                            \
    "Verilog ELM Config file: \"$verilogELMConfig\" is not a readable file."             2

skillAutoLoad="$skillRoot/autoload.il"
check_readable_file "$skillAutoLoad"                                                \
   "Skill Autoload: \"$skillAutoLoad\" is not a readable file."                                 2

propMapFile="$fulcrumPDKRoot/share/Fulcrum/stream/prop.map"
check_readable_file "$propMapFile"                                                  \
    "Stream Out Property Map: \"$propMapFile\" is not a readable file."                         2  

streamOutPipoConfigTemplate="$fulcrumPDKRoot/share/Fulcrum/stream/pipo.streamout.tmpl"
check_readable_file "$streamOutPipoConfigTemplate"                                  \
    "Stream Out Config: \"$streamOutPipoConfigTemplate is not a readable file."                 2

pdkInfoIL="$fulcrumPDKRoot/share/Fulcrum/pdkinfo.il"
check_readable_file "$pdkInfoIL"                                                \
   "pdkinfo.il: \"$pdkInfoIL\' is not a readable file."                                         2

techLib="$fulcrumPDKRoot/share/Fulcrum/pdk.config"

#config_get_value "$fulcrumPDKRoot/share/Fulcrum/pdk.config" tech_lib
#techLib="$ret"
#techLibDir="$fulcrumPDKRoot/share/Fulcrum/$techLib"
#check_readable_dir "$techLibDir"                                               
#   \
#    "Technology file : \"$techLibDir\" is not a readable dir."                 
                 # 2




get_lib_name "$rootCell"
libName="$ret"
check_for_empty_arg "$libName" "Unable to get library name for \"$rootCell\"."                  2

get_cell_name_from_cell_name_with_subtype "$rootCell"
rootCellWithoutSubType="$ret"
check_for_empty_arg "$rootCellWithoutSubType" "Unable to remove subtype from cell name \"$rootCell\"." 2

get_subtype_from_cell_name_with_subtype "$rootCell"
rootCellSubType="$ret"
check_for_empty_arg "$rootCellSubType" "Unable to get subtype from cell name \"$rootCell\"."    2

if [ -n "$verbose" ] ; then
  echo "\"$rootCell\" is in library \"$libName\""
  echo "\"$rootCell\" is subtype \"$rootCellSubType\" of \"$rootCellWithoutSubType\"."
fi

if [ -n "$verbose" ] ; then
  echo "Making bind.rul from \"$techBindRul\" and \"$customBindRul\""
fi
inputBindRul=`mktemp $localWorkingDir/genIP.XXXXXX`

#See if the specified customBindRul file tells us what to call the top level cell
#in the output.
outputRootCellName=`cat $customBindRul | \
                          $grepcmd -F "$rootCell" | \
                          $gawkcmd -- "{ print \\$3 }"`
cat "$techBindRul"   >$inputBindRul
if [ -z "$outputRootCellName" ] ; then
    #The customBindRul file didn't tell us what to call the top level cell
    #in the output so we will just call it "TOP" in the output.
    outputRootCellName="TOP"
    echo "" >>$inputBindRul
    echo "" >>$inputBindRul
    echo "C $cadenceRootCell TOP" >>$inputBindRul
    echo "" >>$inputBindRul
    echo "" >>$inputBindRul  
fi
cat "$customBindRul" >>$inputBindRul


#Run jauto to get the strength report.

jautoToilet=`mktemp -d $localWorkingDir/genIP.XXXXXX`

echo $presizeConfig;
echo $processConfig;
jautoCmd="$jauto    \"--config=$presizeConfig\""
jautoCmd="$jautoCmd \"--config=$processConfig\""
jautoCmd="$jautoCmd \"--castInRoot=$castPath\""
jautoCmd="$jautoCmd \"--cellName=$rootCellWithoutSubType\""
jautoCmd="$jautoCmd \"--subtype=$rootCellSubType\""
jautoCmd="$jautoCmd \"--outRoot=$jautoToilet\""
jautoCmd="$jautoCmd \"--cdlRoot=$jautoToilet\""
jautoCmd="$jautoCmd \"--subtypePath=$jautoToilet\""
jautoCmd="$jautoCmd \"--max-heap-size=$javaMaxHeapSize\""
jautoCmd="$jautoCmd \"--lancelotTemp=$jautoToilet/lancelot\""
jautoCmd="$jautoCmd &>$jautoToilet/presize.output.txt"

if [ -n "$verbose" ] ; then
  echo "Generating strength report using jauto."
  echo "$jautoCmd"
fi
eval "$jautoCmd"

strengthReportFile="$jautoToilet/hier_strength.debug"

if [[ ( ! ( -r "$strengthReportFile" && -s "$strengthReportFile" ) ) ]] ; then
  echo "WARNING: Unable to generate strength report."
  cat "$jautoToilet/presize.output.txt"
fi


ipDataDir=`mktemp -d $localWorkingDir/genIP.XXXXXX`

genIPDataOutput=`mktemp $localWorkingDir/genIP.XXXXXX`

# set options to generate verilog for only gatelevel and emulation model
glModelName="$outputRootCellName";
emulationModelName="$outputRootCellName-emulation";
prs2VerilogArg="$glModelName:--config=$verilogGLMConfig";
prs2VerilogArg="$prs2VerilogArg:$emulationModelName:--config=$verilogELMConfig";

echo "prs2verilog: $prs2verilogArg"

#call generatIPData to get CDL, verilog, renamed strength report,
#and skill name tables.
generateIPDataCmd="$generateIPData \"--max-heap-size=$javaMaxHeapSize\""
generateIPDataCmd="$generateIPDataCmd \"--cast-path=$castPath\""
generateIPDataCmd="$generateIPDataCmd \"--cell=$rootCell\""
generateIPDataCmd="$generateIPDataCmd \"--gdsII-name-interface=pmc\""
generateIPDataCmd="$generateIPDataCmd \"--bind-rul=$inputBindRul\""
generateIPDataCmd="$generateIPDataCmd \"--prs2verilog=$prs2VerilogArg\""
generateIPDataCmd="$generateIPDataCmd \"--output-dir=$ipDataDir\""
generateIPDataCmd="$generateIPDataCmd \"--strength-report=$strengthReportFile\""
if [ -n "$debugJava" ] ; then
  generateIPDataCmd="$generateIPDataCmd --run-debugger"
fi
generateIPDataCmd="$generateIPDataCmd &>$genIPDataOutput"

if [ -n "$verbose" ] ; then
  echo "Top level cell in GDSII file will be called \"$outputRootCellName\"."
  echo "Generating verilog, CDL, and name mapping files."
  echo "$generateIPDataCmd"
fi

eval "$generateIPDataCmd"

generatedBindRul="$ipDataDir/bind.rul"
generatedCDLFile="$ipDataDir/gdsII.cdl"
generatedILFiles="$ipDataDir/ilnames"
generatedStrengthReport="$ipDataDir/$rootCell.strength.report.txt"
generatedglModel="$ipDataDir/$glModelName"
generatedemulationModel="$ipDataDir/$emulationModelName"

if [[ ( ! ( -r "$generatedBindRul" && -s "$generatedBindRul" ) ) || 
      ( ! ( -r "$generatedCDLFile" && -s "$generatedCDLFile" ) ) ||
      ( ! ( -r "$generatedStrengthReport" && -s "$generatedStrengthReport" ) ) ]] ; then
    echo "WARNING: Unable to generate verilog, CDL, and name mapping files."
    cat $genIPDataOutput
    exit 1
fi

# Collect all the verilog models and put them in the verilog tree that 
# we will use in the output.


pipoConf0=`mktemp $localWorkingDir/genIP.XXXXXX`
pipoConf1=`mktemp $localWorkingDir/genIP.XXXXXX`

pipoWD=`mktemp -d $localWorkingDir/genIP.XXXXXX`

pipoLog=`mktemp $localWorkingDir/genIP.XXXXXX`

gdsIIOutput=`mktemp $localWorkingDir/genIP.XXXXXX`

if [ -n "$verbose" ] ; then
  echo "Generating config file for PIPO."
fi

#Generate the pipo config file.
$sedcmd -e "s=\\\$workingdir\\\$=$pipoWD=g"      \
        -e "s=\\\$outputfile\\\$=$gdsIIOutput=g" \
        -e "s=\\\$propmap\\\$=$propMapFile=g"    \
        -e "s=\\\$logfile\\\$=$pipoLog=g"        \
        -e "s=\\\$flatten_pcells\\\$=t=g"        \
        $streamOutPipoConfigTemplate             \
        >$pipoConf0

pipoScript=`mktemp $localWorkingDir/genIP.XXXXXX`
pipoOutput="$pipoWD/pipo.out"

cdsWD=`mktemp -d $localWorkingDir/genIP.XXXXXX`

#Make a cadence working directory in which to copy and munge the
#layout for export.
makeCdsWDCmd="$mkcdswd \"--dfII-dir=$dfIIDir\""
makeCdsWDCmd="$makeCdsWDCmd \"--fulcrum-pdk-root=$fulcrumPDKRoot\""
makeCdsWDCmd="$makeCdsWDCmd \"--target-dir=$cdsWD\""
makeCdsWDCmd="$makeCdsWDCmd \"--force\""
makeCdsWDCmd="$makeCdsWDCmd \"--temp\""

if [ -n "$verbose" ] ; then
  echo "$makeCdsWDCmd"
fi

eval "$makeCdsWDCmd"

#Generate a shell script that the skill code can execute to stream
#out the munged layout.  Things are done this way because
#only the skill code knows things like user units per database unit and
#the user unit.
if [ -n "$verbose" ] ; then
  echo "Generating script to run PIPO."
fi
echo "#!/bin/bash"                                                        >$pipoScript
echo "pushd $cdsWD >/dev/null"                                           >>$pipoScript
echo "$sedcmd \\"                                                        >>$pipoScript
echo "  -e \"s=\\\\\\\$lib\\\\\\\$=\$1=g\"       \\"                     >>$pipoScript
echo "  -e \"s=\\\\\\\$cell\\\\\\\$=\$2=g\"      \\"                     >>$pipoScript
echo "  -e \"s=\\\\\\\$view\\\\\\\$=\$3=g\"      \\"                     >>$pipoScript
echo "  -e \"s=\\\\\\\$uuperdbu\\\\\\\$=\$4=g\"  \\"                     >>$pipoScript
echo "  -e \"s=\\\\\\\$userunit\\\\\\\$=\$5=g\"  \\"                     >>$pipoScript
echo "  $pipoConf0                               \\"                     >>$pipoScript
echo "  >$pipoConf1"                                                     >>$pipoScript
echo "$CADENCE_WRAPPER_SCRIPT pipo strmout $pipoConf1 >$pipoOutput"      >>$pipoScript
if [ -n "$pipoRun" ] ; then
  echo "cp \"$pipoConf0\" \"$pipoWD/pipo.conf.0.txt\""                   >>$pipoScript
  echo "cp \"$pipoConf1\" \"$pipoWD/pipo.conf.1.txt\""                   >>$pipoScript
  echo "cp \"$pipoScript\" \"$pipoWD/pipo.run.script\""                  >>$pipoScript
  echo "tar -cjf \"$pipoRun\" -C \"$pipoWD\" ."                          >>$pipoScript
fi
echo "popd >/dev/null"                                                   >>$pipoScript

chmod 755 "$pipoScript"

mainIL=`mktemp $localWorkingDir/genIP.XXXXXX`

tempLibDir=`mktemp -d $localWorkingDir/genIP.XXXXXX`

if [ -n "$verbose" ] ; then
  echo "Generating skill program."
fi


#Generate the skill program that will copy, munge, and stream
#out the layout for export.
echo "( load \"$skillAutoLoad\" )"                                      >$mainIL
echo "( load \"$pdkInfoIL\" )"                                         >>$mainIL
echo "( GDSIIHierCopyAndMungeAndStreamCellTrippleForExportWithPDKInfo" >>$mainIL
echo "  \"$generatedILFiles\""                                         >>$mainIL
echo "  \"GDSIIWrite\""                                                >>$mainIL
echo "  \"$tempLibDir\""                                               >>$mainIL
echo "  \"$libName\""                                                  >>$mainIL
echo "  \"$cadenceRootCell\""                                          >>$mainIL
echo "  \"$view\""                                                     >>$mainIL
echo "  \"$outputRootCellName\""                                       >>$mainIL
echo "  \"$pipoScript\" )"                                             >>$mainIL
echo "( exit )"                                                        >>$mainIL

#Generate the command the will run the generated skill program in cadence.
cadenceCmd="$CADENCE_WRAPPER_SCRIPT layout "
cadenceCmd="$cadenceCmd -replay \"$mainIL\""
cadenceCmd="$cadenceCmd -log \"$cadenceLog\""
cadenceCmd="$cadenceCmd -nograph"
cadenceCmd="$cadenceCmd &>/dev/null </dev/null"
if [ -n "$verbose" ] ; then
  echo "Running Cadence"
  echo "$cadenceCmd"
fi

pushd $cdsWD >/dev/null
eval "$cadenceCmd"
popd >/dev/null

#See if we made a GDSII file.
if [[ -f "$gdsIIOutput" && -s "$gdsIIOutput" ]] ; then
    echo "GDSII output successfully created";
else
    echo "WARNING: Unable to generate GDSII."
fi

tarDir=`mktemp -d $localWorkingDir/genIP.XXXXXX`

tarContentsDir="$tarDir/$outputRootCellName-$versionStr"
mkdir -p "$tarContentsDir"
mkdir -p "$tarContentsDir/design/"
mkdir -p "$tarContentsDir/testbench/"
mkdir -p "$tarContentsDir/design/gdsII"
mkdir -p "$tarContentsDir/design/emulation"
mkdir -p "$tarContentsDir/design/glm"
mkdir -p "$tarContentsDir/design/netlist"


if [ -n "$verilogTestBenchesDir" ] ; then
    cpTBFileCmd="cp -r \"$verilogTestBenchesDir\" \"$tarContentsDir/testbench\""
    if [ -n "$verbose" ] ; then
          echo "$cpTBFileCmd"
    fi
      #eval "$cpTBFileCmd"
fi

cpGDSIICmd="cp -r \"$gdsIIOutput\" \"$tarContentsDir/design/gdsII/$outputRootCellName.gdsII\""
if [ -n "$verbose" ] ; then
    echo "$cpGDSIICmd"
fi  
  eval "$cpGDSIICmd"
  
  cpVerilogTreeCmd="cp -Rv \"$generatedglModel/\"* \"$tarContentsDir/design/glm/\""
  if [ -n "$verbose" ] ; then
      echo "$cpVerilogTreeCmd"
  fi
  eval "$cpVerilogTreeCmd"
  
  cpVerilogTreeCmd="cp -Rv \"$generatedemulationModel/\"* \"$tarContentsDir/design/emulation/\""
  if [ -n "$verbose" ] ; then
      echo "$cpVerilogTreeCmd"
  fi
  eval "$cpVerilogTreeCmd"
  
  #cpTechCmd="cp -rf \"$techLibDir\" \"$tarContentsDir/design/gdsII\""
  #if [ -n "$verbose" ] ; then
  #    echo "$cpTechCmd"
  #fi
  #eval "$cpTechCmd"
  
  cpCDLCmd="cp -r \"$generatedCDLFile\" \"$tarContentsDir/design/netlist/$outputRootCellName.cdl\""
  if [ -n "$verbose" ] ; then
      echo "$cpCDLCmd"
  fi
  eval "$cpCDLCmd"
  
  cpStrengthReportCmd="cp -r \"$generatedStrengthReport\" \"$tarContentsDir/design/netlist/$outputRootCellName.strength.report.txt\""
  if [ -n "$verbose" ] ; then
      echo "$cpStrengthReportCmd"
  fi
  eval "$cpStrengthReportCmd"
  
  tarOutputCmd="$tarcmd -cjf \"$outputFile\" -C \"$tarDir\" \".\""
  if [ -n "$verbose" ] ; then
      echo "$tarOutputCmd"
  fi
  eval "$tarOutputCmd"


