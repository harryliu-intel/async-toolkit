#!/bin/bash
# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$

arch_bin_dir=${0%\/*}
package_root=${arch_bin_dir%\/*}
sh_lib_dir="$package_root/share/script/sh/sh-lib"
build_id="$buildid$"

#To debug updatenetlist problems in skill comment out the if blocks
#for skill_netlist_dir, directives_skill_dir,
#and main_il.  You will also need to insert the "exit" command immediately
#before the command that runs ssh to run commands on the sun.

function exit_func() {
  if [ -n "$local_working_dir" ] ; then
    rm -rf "$local_working_dir"
  fi
}

trap exit_func EXIT

function usage() {
  echo "Usage: $0 "
  echo "  --cell=cell | --cell-list=<filename>"
  echo "  --subtype=num"
  echo "  --fulcrum-pdk-root=dir"
  echo "  --dfII-dir=dir"
  echo "  --cast-path=castpath"
  echo "  [ --density-factor-default=15 ]"
  echo "  [ --bound-scale-factor=1 ]"
  echo "  [ --suppress-pins ]"
  echo "  [ --cadence-log=/dev/null ]"
  echo "  [ --update-views ]"
  echo "  [ --change-list=num ]"
  echo "  [ --check-log=/dev/null ]"
  echo "  [ --lock-bound ]"
  echo "  [ --use-layout-height ]"
  echo "  [ --force-pins ]"
  echo "  [ --suppress-netlist-view ]"
  echo "  [ --lock-layout ]"
  echo "  [ --version ]"
  echo "  [ --debug ]"
  echo "  [ --disable-license-queuing ]"
  echo "  [ --cast2skill-options=options ]"
}

source "$sh_lib_dir/file/filecheck.sh"
source "$sh_lib_dir/file/conon.sh"
source "$sh_lib_dir/script/generate_script_with_libs.sh"

layout=`mywhich layout` || exit 2
sedcmd=`mywhich sed` || exit 2
gawkcmd=`mywhich gawk` || exit 2
grepcmd=`mywhich grep` || exit 2
bashcmd=`mywhich bash` || exit 2
findcmd=`mywhich find` || exit 2
sortcmd=`mywhich sort` || exit 2
uniqcmd=`mywhich uniq` || exit 2

# parsecellname demands that sedcmd be defined first
source "$package_root/share/script/sh/util/parsecellname"

check_readable_dir "$arch_bin_dir" \
    "Package arch bin: \"$arch_bin_dir\" is not a readable directory." 2

cast2skill="$arch_bin_dir/cast2skill"
check_executable_file "$cast2skill" \
    "cast2skill: \"$cast2skill\" is not an executable file." 2


cds_sh_lib="$package_root/share/script/sh/util"
check_readable_dir "$cds_sh_lib" \
    "Cadence Shell Script Library: \"$cds_sh_lib\" is not a readable directory." 2

cds_sh_lib_files=`$findcmd "$cds_sh_lib" \! -type d`
for file in $cds_sh_lib_files ; do
  source "$file"
done


mkcdswd="$arch_bin_dir/mkcdswd"
check_executable_file "$mkcdswd"                                                 \
    "mkcdswd: \"$mkcdswd\" is not an executable file."                                 2

netlist_view_name="netlist"
layout_view_name="layout"
floorplan_view_name="floorplan"

root_cell=
root_cell_subtype=
fulcrum_pdk_root=
dfII_dir=
cast_path=
density_factor_default=15
bound_scale_factor=1
cadence_log=/dev/null
client_spec=
update_views=
verbose=
change_list_num=
check_log=/dev/null
no_netlist_views=
lock_bound=nil
suppress_pins=nil
suppress_wiring_directives=nil
force_pins=nil
use_layout_height=nil
lock_layout=nil
profile_out=
debug=
javaMaxHeapSize="1800M"
tempDir="$TMPDIR"
cellList=
cast2skill_opt=
license_wait='WAIT'

for arg in "$@" ; do
  
  case "$arg" in
  --cell=* )
    root_cell=`echo "$arg" | $sedcmd -e "s/--cell=//"`
    ;;
  --cell-list=* )
    cellList=`echo "$arg" | $sedcmd -e "s/--cell-list=//"`
    ;;
  --subtype=* )
    root_cell_subtype=`echo "$arg" | $sedcmd -e "s/--subtype=//"`
    ;;
  --fulcrum-pdk-root=* )
    fulcrum_pdk_root=`echo "$arg" | $sedcmd -e "s/--fulcrum-pdk-root=//"`
    ;;
  --dfII-dir=* )
    dfII_dir=`echo "$arg" | $sedcmd -e "s/--dfII-dir=//"`
    ;;
  --cast-path=* )
    cast_path=`echo "$arg" | $sedcmd -e "s/--cast-path=//"`
    ;;
  --density-factor-default=* )
    density_factory_default=`echo "$arg" | $sedcmd -e "s/--density-factor-default=//"`
    ;;
  --bound-scale-factor=* )
    bound_scale_factor=`echo "$arg" | $sedcmd -e "s/--bound-scale-factor=//"`
    ;;
  --cadence-log=* )
    cadence_log=`echo "$arg" | $sedcmd -e "s/--cadence-log=//"`
    ;;
  --update-views )
    update_views=1
    ;;
  --verbose )
    verbose=1
    ;;
  --check-log=* )
    check_log=`echo "$arg" | $sedcmd -e "s/--check-log=//"`
    ;;
  --cds-wd=* )
    echo "--cds-wd option is no longer needed.  \"$arg\" ignored."
    ;;
  --suppress-netlist-view )
    no_netlist_views=t
    ;;
  --lock-bound )
    lock_bound=t
    ;;
  --force-pins )
    force_pins=t
    ;;
  --lock-layout )
    lock_layout=t
    ;;
  --profile-out=* )
    profile_out=`echo "$arg" | $sedcmd -e "s/--profile-out=//"`
    ;;
  --use-layout-height )
    use_layout_height=t
    ;;
  --suppress-pins )
    suppress_pins=t
    ;;
  --suppress-wiring-directives )
    suppress_wiring_directives=t
    ;;
  --version )
    echo "$build_id"
    ;;
  --debug )
    debug=1
    ;;
  --java-max-heap-size=* )
    javaMaxHeapSize=`echo "$arg" | $sedcmd -e "s/--java-max-heap-size=//"`
    ;;
  --temp-dir=* )
    tempDir=`echo "$arg" | $sedcmd -e "s/--temp-dir=//"`
    ;;
  --cast2skill-options=* )
    cast2skill_opt=`echo "$arg" | $sedcmd -e "s/--cast2skill-options=//"`
    ;;
  --disable-license-queuing )
    license_wait='NOWAIT'
    ;;
  --* )
    echo "Unknown argument: \"$arg\"."
    usage
    exit 2
    ;;
  esac
done

check_for_empty_arg "$package_root"                                              \
    "The packageroot variable must contain the location of the package installation."           2

if [[ ( -z "$cellList" ) && \
      (( -z $root_cell ) || ( -z "$root_cell_subtype" )) ]] ; then
    usage
    echo "A cell list or cell name/subtype must be specified"
    exit 2
fi


check_for_empty_arg "$fulcrum_pdk_root"                                          \
    "You must specify the location of the fulcrum pdk you want to use."                         2
check_for_empty_arg "$dfII_dir"                                                  \
    "You must specify the location of directory containing all the dfII data."                  2
check_for_empty_arg "$cast_path"                                                 \
    "You must specify a cast path"                                                              2
check_for_empty_arg "$density_factor_default"                                    \
    "You must specify a default value for the density factor directive."                        2
check_for_empty_arg "$bound_scale_factor"                                        \
    "You must specify a boundary scaling factor"                                                2
check_for_empty_arg "$cadence_log"                                               \
    "You must specify a cadence log file."                                                      2
check_for_empty_arg "$check_log"                                                 \
    "You must specify a file into which the check log is written."                              2

check_for_empty_arg "$javaMaxHeapSize"                                           \
    "You must specify a maximum java heap size."                                                2

if [ -z "$tempDir" ] ; then
  tempDir="/scratch"
fi

local_working_dir=`mktemp -d "$tempDir/updatenetlist.XXXXXX"` || exit 1

check_readable_dir "$package_root"                                               \
    "Package Installation: \"$package_root\" is not a readable directory."             2

check_readable_dir "$fulcrum_pdk_root"                                           \
    "Fulcrum PDK: \"$fulcrum_pdk_root\" is not a readable directory."                  2
conon_path "$fulcrum_pdk_root"
fulcrum_pdk_root="$ret"

check_writeable_dir "$dfII_dir"                                                  \
    "Generated Libraries Root: \"$dfII_dir\" is not a writeable, readable directory."  1
conon_path "$dfII_dir"
dfII_dir="$ret"

if [ "$cadence_log" != "/dev/null" ] ; then
  check_writeable_file "$cadence_log"                                             \
      "Cadence Log File: \"$cadence_log\" is not a writeable file."                    1
  conon_path "$cadence_log"
  cadence_log="$ret"
fi

if [ "$check_log" != "/dev/null" ] ; then
  check_writeable_file "$check_log"                                               \
      "Check Log: \"$check_log\" is not a writeable file."                             1
  conon_path "$check_log"
  check_log="$ret"
fi




lib_commands_dir="$package_root/share/script/sh/cell-automation/lib_commands"
skill_root="$package_root/share/skill"
setup_dir="$package_root/share/script/sh/setup"
change_list_template="$package_root/share/data/change_list_template.txt"


check_readable_dir "$lib_commands_dir"                                           \
    "commands directory: \"$lib_commands_dir\" is not a directory."                    2
check_readable_dir  "$skill_root"                                                \
    "Skill Root Directory: \"$skill_root\" is not a readable directory."               2
check_readable_file "$change_list_template"                                      \
    "change_list_template: \"$change_list_template\" is not a readable file."          2


pdkinfo_il="$fulcrum_pdk_root/share/Fulcrum/pdkinfo.il"
check_readable_file "$pdkinfo_il"                                                \
   "pdkinfo.il: \"$pdkinfo_il\' is not a readable file."                               2

skill_auto_load="$skill_root/autoload.il"
check_readable_file "$skill_auto_load"                                           \
   "Skill Autoload: \"$skill_auto_load\" is not a readable file."                      2

mkcdslib_source="$lib_commands_dir/mkcdslib"
check_readable_file "$mkcdslib_source"                                           \
    "mkcdslib: \"$mkcdslib_source\" is not a readable file."                           2

blank_cds_library="$fulcrum_pdk_root/share/Fulcrum/blank-library"
check_readable_dir  "$blank_cds_library"                                         \
    "Blank Cadence Library: \"$blank_cds_library\" is not a readable directory."       2




cast2skill_output=`mktemp -d "$local_working_dir/updatenetlist.XXXXXX"`  


cast2skill_params="--cast-version=2                     \
                   --cast-path=$cast_path               \
                   --output-dir=$cast2skill_output      \
                   --max-heap-size=$javaMaxHeapSize     \
                   --fulcrum-pdk-root=$fulcrum_pdk_root"

if [ -n "$cast2skill_opt" ] ; then
  cast2skill_params="$cast2skill_params $cast2skill_opt"
fi

#do one cell or a cell list
if [ -n "$cellList" ] ; then
    cast2skill_params="$cast2skill_params --cells=$cellList"
else
    cast2skill_params="$cast2skill_params --cell=$root_cell.$root_cell_subtype"
fi

if [ "$suppress_pins" == "t" ] ; then
  cast2skill_params="$cast2skill_params --suppress-pins"
fi

if [ "$suppress_wiring_directives" == "t" ] ; then
  cast2skill_params="$cast2skill_params --suppress-wiring-directives"
fi

cast2SkillCmd="$cast2skill $cast2skill_params"

if [ -n "$verbose" ] ; then
  echo "Running cast2skill to generate skill netlist, directives, pins, etc.  for \"$root_cell.$root_cell_subtype\"."
  echo "$cast2SkillCmd"
fi

if [ -n "$profile_out" ] ; then
  (echo "cast2verilog started at " `date`; \
   $cast2skill $cast2skill_params &>$cast2skill_output/cast2skill.out; \
   echo "cast2verilog ended at " `date`; \
   times) &> "$profile_out"
else
  $cast2skill $cast2skill_params &>$cast2skill_output/cast2skill.out
fi

cast2SkillOutput="$?"

if [[ "$cast2SkillOutput" != "0" ]] ; then
  echo "Unable to generate skill from CAST."
  cat "$cast2skill_output/cast2skill.out"
  exit 1
fi

cmd_tmp_dir=`mktemp -d "$local_working_dir/updatenetlist.XXXXXX"`

generate_command_script "$cmd_tmp_dir" "$mkcdslib_source" "$sh_lib_dir" "$bashcmd"
mkcdslib="$ret"
check_executable_file "$mkcdslib" \
    "mkcdslib: \"$mkcdslib\" is not readable, executable file."  2

skill_netlist_dir="$cast2skill_output/ilnets"

cell_list_file="$cast2skill_output/cellnames.txt"

lib_list_file="$cast2skill_output/libnames.txt"

directives_skill_dir="$cast2skill_output/ildirectives"

autopins_skill_dir="$cast2skill_output/autopins"

cells=`cat "$cell_list_file"`
libraries=`cat "$lib_list_file"`

cdsWD=`mktemp -d "$local_working_dir/updatenetlist.XXXXXX"`

mkcdswdCmd="$mkcdswd \"--dfII-dir=$dfII_dir\""
mkcdswdCmd="$mkcdswdCmd \"--fulcrum-pdk-root=$fulcrum_pdk_root\""
mkcdswdCmd="$mkcdswdCmd \"--target-dir=$cdsWD\""
mkcdswdCmd="$mkcdswdCmd \"--force\""
mkcdswdCmd="$mkcdswdCmd \"--cast-path=$cast_path\""

if [ -n "$verbose" ] ; then
  echo "Generated cdsWD in \"$cdsWD\"."
  echo "$mkcdswdCmd"
fi

eval "$mkcdswdCmd"


for lib in $libraries ; do
  if [ -n "$verbose" ] ; then
    echo "Ensuring that library \"$lib\" exists."
  fi
  # bug 14632
  if [ ! -d "$fulcrum_pdk_root/share/Fulcrum/dfII/$lib" ]; then
    $mkcdslib "--cds-wd=$cdsWD"                                     \
            "--generated-libraries-root=$dfII_dir"                 \
            "--blank-cds-library=$blank_cds_library"               \
            "--lib=$lib --cadence-shell-library=$cds_sh_lib"
  fi
done

if [ -n "$verbose" ] ; then
  echo "Generating main skill program."
fi


mainIL=`mktemp "$local_working_dir/updatenetlist.XXXXXX"`
mainILProf="$mainIL.profile"

if [ -n "$debug" ] ; then
  echo "( ilDebugToolBox )"                        >>$mainIL
fi
echo "( load \"$skill_auto_load\" )"               >>$mainIL
echo "( load \"$pdkinfo_il\" )"                    >>$mainIL
echo "( UIInit )"                                  >>$mainIL

update_netlist_check_result=`mktemp "$local_working_dir/updatenetlist.XXXXXX"`


cell_list_file=`mktemp "$local_working_dir/updatenetlist.XXXXXX"`

for cell in $cells ; do

  get_lib_name $cell
  cell_lib="$ret"

  if [ -n "$cell_lib" ] ; then
    echo "$cell_lib $cell" >>$cell_list_file
  else
    echo "\"$cell\" is not a valid cell name."
  fi

done

if [ -n "$profile_out" ] ; then
  echo "(profile 'time)" >> $mainIL
fi

if [[ "$update_views" != 1 ]] ; then
  echo "( UpdateNetlistCompareCellsToSkillNetlistsInToFileUsingPDKInfo" >>$mainIL
  echo "  ( LibCellViewReadLibCellPairsFromFile"                        >>$mainIL
  echo "    \"$cell_list_file\" )"                                      >>$mainIL
  if [[ "$no_netlist_views" == t ]] ; then
    echo "  nil"                                                        >>$mainIL
  else
    echo "  \"$netlist_view_name\""                                     >>$mainIL
  fi
  echo "  \"$layout_view_name\""                                        >>$mainIL
  echo "  \"$floorplan_view_name\""                                     >>$mainIL
  echo "  \"$skill_netlist_dir\""                                       >>$mainIL
  echo "  \"$directives_skill_dir\""                                    >>$mainIL
  echo "  \"$autopins_skill_dir\""                                      >>$mainIL
  echo "  $density_factor_default"                                      >>$mainIL
  echo "  $bound_scale_factor"                                          >>$mainIL
  echo "  \"$update_netlist_check_result\""                             >>$mainIL
  echo "  $lock_bound"                                                  >>$mainIL
  echo "  $suppress_pins"                                               >>$mainIL
  echo "  $force_pins"                                                  >>$mainIL
  echo "  $use_layout_height"                                           >>$mainIL
  echo "  $lock_layout"                                                 >>$mainIL
  echo " )"                                                             >>$mainIL
else
  echo '(if ( LicenseGetLicense "Virtuoso_Layout_Suite_GXL" (getLicVersion) 0 ?LicenseType "'$license_wait'" )'                                           >>$mainIL
  echo "( UpdateNetlistUpdateCellsFromSkillNetlistsToFileUsingPDKInfo"  >>$mainIL
  echo "  ( LibCellViewReadLibCellPairsFromFile"                        >>$mainIL
  echo "    \"$cell_list_file\" )"                                      >>$mainIL
  if [[ "$no_netlist_views" == t ]] ; then
    echo "  nil"                                                        >>$mainIL
  else
    echo "  \"$netlist_view_name\""                                     >>$mainIL
  fi
  echo "  \"$layout_view_name\""                                        >>$mainIL
  echo "  \"$floorplan_view_name\""                                     >>$mainIL
  echo "  \"$skill_netlist_dir\""                                       >>$mainIL
  echo "  \"$directives_skill_dir\""                                    >>$mainIL
  echo "  \"$autopins_skill_dir\""                                      >>$mainIL
  echo "  $density_factor_default"                                      >>$mainIL
  echo "  $bound_scale_factor"                                          >>$mainIL
  echo "  \"$update_netlist_check_result\""                             >>$mainIL
  echo "  $lock_bound"                                                  >>$mainIL
  echo "  $suppress_pins"                                               >>$mainIL
  echo "  $force_pins"                                                  >>$mainIL
  echo "  $use_layout_height"                                           >>$mainIL
  echo "  $lock_layout"                                                 >>$mainIL
  echo " ) )"                                                           >>$mainIL
fi

if [ -n "$profile_out" ] ; then
  echo "(profileSummary ?children t ?file \"$mainILProf\")" >> $mainIL
fi

if [ -z "$debug" ] ; then
  echo "( exit )"                                                       >>$mainIL
fi

if [ -n "$verbose" ] ; then
  echo "Generating shell code to run cadence."
fi
cadenceScript=`mktemp "$local_working_dir/updatenetlist.XXXXXX"`
cadenceLogTemp=`mktemp "$local_working_dir/updatenetlist.XXXXXX"`
cadenceErr=`mktemp "$local_working_dir/gdsIIWrite.XXXXXX"`

if [ -n "$verbose" ] ; then
  echo "Running cadence"
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
if [ -n "$profile_out" ] ; then
  (echo "updatenlist started at " `date`; \
   "$cadenceScript"; \
   status=$?; \
   echo "updatenlist ended at " `date`; \
   times; \
   exit $status) >> "$profile_out" 2>&1
else
  "$cadenceScript"
fi

ret=$?

cat "$cadenceLogTemp" > "$cadence_log"
cat "$update_netlist_check_result" > "$check_log"

if [ -n "$profile_out" ] ; then
  cat "$mainILProf" >> "$profile_out"
fi

#check for error in cadence
#FIXME: this is unreliable because a cell name could contain "ERROR"
([[ $ret != 0 ]] || grep -i Error "$cadence_log") && tail -10 "$cadence_log" && exit 2


error_in_update=`cat "$update_netlist_check_result" | $grepcmd -e "^ERROR:"`

if [ -n "$error_in_update" ] ; then
  cat $update_netlist_check_result | $grepcmd -e "\(^ERROR:\)\|\(^+\)" | $sedcmd -e "s/+//"
  exit 1
fi


if [[ "$update_views" != 1 ]] ; then
  cat $update_netlist_check_result | $grepcmd -v "\(^NET:\)\|\(^IGNORE:\)"

fi
