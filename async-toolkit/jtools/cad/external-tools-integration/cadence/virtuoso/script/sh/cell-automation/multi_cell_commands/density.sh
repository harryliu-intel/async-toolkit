#!/bin/bash
<<DOC
Reports on density/coverage of leaf cells and mid level cells.
See <module layout.density.density>.
DOC

input=
output=
cds_wd=
cds_wd_template=$CDS_WD_TEMPLATE
debug=


function exit_func() {
    if [[ $debug != t ]] ; then
        [ -d "$cds_wd" ] && rm -rf "$cds_wd"
        [ -f "$input" ] && rm -f "$input"
        [ -f "$output" ] && rm -f "$output"
    fi
}

trap exit_func EXIT

arch_bin_dir=${0%\/*}
package_root=${arch_bin_dir%\/*}

sh_lib_dir="$package_root/share/script/sh/sh-lib"

source "$sh_lib_dir/file/filecheck.sh"
source "$sh_lib_dir/file/conon.sh"

#
##### Parse arguments #####
#

function usage() {
    cat<<USAGE
    Usage: $0
    --fulcrum-pdk-root=dir
    --view=name
    [--lib=name]
    [--transistor-base-width=0.0(meters)]
    [--dfII-dir=dir] ( Must specify if not in cds_wd )
    [--working-dir=dir] ( Must specify if not in cds_wd )
    <cell1> <cell2> ...
USAGE
}

sedcmd=`which sed`
bashcmd=`which bash`
grepcmd=`which grep`

check_executable_file "$grepcmd" "Unable to find grep in \"$PATH\"" 2
check_executable_file "$sedcmd" "Unable to find sed in \"$PATH\"" 2
check_executable_file "$bashcmd" "Unable to find bash in \"$PATH\"" 2

cell_list=
view=
fulcrum_pdk_root=
working_dir=
dfII_dir=
cds_wd=
transistor_base_width=0.0
debug=nil

for arg in $@ ; do
  case "$arg" in
  --fulcrum-pdk-root=* )
    fulcrum_pdk_root=`echo $arg | $sedcmd -e "s/--fulcrum-pdk-root=//"`
    ;;
  --view=* )
    view=`echo $arg | $sedcmd -e "s/--view=//"`
    ;;
  --lib=* )
    lib=`echo $arg | $sedcmd -e "s/--lib=//"`
    ;;
  --transistor-base-width=* )
    transistor_base_width=`echo $arg | $sedcmd -e "s/--transistor-base-width=//"`
    ;;
  --dfII-dir=* )
    dfII_dir=`echo "$arg" | $sedcmd -e "s/--dfII-dir=//"`
    ;;
  --working-dir=* )
    working_dir=`echo "$arg" | $sedcmd -e "s/--working-dir=//"`
    ;;
  --debug )
    debug=t
    ;;
  --* )
    echo "Unknown argument: \"$arg\"."
    usage
    exit 2
    ;;
  * )
    cell_list="$cell_list $arg"
    ;;
  esac
done

check_for_empty_arg "$package_root"                                              \
    "The packageroot variable must contain the location of the package installation."           2
check_for_empty_arg "$view"                                              \
    "The view name must be specified."           2
check_for_empty_arg "$transistor_base_width"                            \
    "The transistor base width (in meters) must be specified."           2
check_for_empty_arg "$fulcrum_pdk_root"                                              \
    "The fulcrum pdk root mist be specified."           2

check_readable_dir "$fulcrum_pdk_root"                                           \
    "Fulcrum PDK: \"$fulcrum_pdk_root\" is not a readable directory."                  2
conon_path "$fulcrum_pdk_root"
fulcrum_pdk_root="$ret"


check_for_empty_arg "$working_dir"                                              \
    "An NFS working dir must be speciified."           2
check_for_empty_arg "$dfII_dir"                                              \
    "A dfII dir must be speciified."           2

check_readable_dir "$working_dir"                                           \
    "NFS working dir: \"$working_dir\" is not a readable directory."                  2
conon_path "$working_dir"
working_dir="$ret"

check_readable_dir "$dfII_dir"                                           \
    "dfII dir: \"$dfII_dir\" is not a readable directory."                  2
conon_path "$dfII_dir"
dfII_dir="$ret"    

cds_wd=$(mktemp -d "$working_dir/cds_wd.XXXXXX")

mkcdswd="$arch_bin_dir/mkcdswd"
check_executable_file "$mkcdswd"                                                 \
    "mkcdswd: \"$mkcdswd\" is not an executable file."                                 2

$mkcdswd "--dfII-dir=$dfII_dir"                            \
    "--fulcrum-pdk-root=$fulcrum_pdk_root"            \
    "--cast-path=foo"                                 \
    "--target-dir=$cds_wd"                            \
    "--force"                                         \
    "--user-template=$cds_wd_template"
cd "$cds_wd"

cells=""
for cell in $cell_list ; do
  cells="$cells \"$cell\""
done

input="$cds_wd/input"
output="$cds_wd/output"

cat<< EOF > "$input"
(load "$package_root/share/skill/autoload.il")
(load "$fulcrum_pdk_root/share/Fulcrum/pdkinfo.il")
(when $debug
  ilToolBox()
  ilDebugToolBox()
  ilDebugToolBoxForm->autoStacktrace->value = "16"
  hiCloseWindow(ilDebugToolBoxWindow) )

(let (
      ( Output ( outfile "$output" ) ) )
  ( foreach CellName ( list $cells )
            (let (
                  ( LibName 
                    (cond (
                           ( not ( equal "" "$lib" ) )
                           "$lib" )
                          (
                           ( car 
                             ( NameParseCellName
                               CellName ) ) ) ) ) )
              (let (
                    ( DDObj ( ddGetObj 
                              LibName
                              CellName
                              "$view" ) ) )
                (cond (
                       ( and
                         DDObj
                         ( ddIsObjReadable DDObj ) )
                       ( fprintf
                         Output
                         "%s\n"
                         ( DensityFactorVerboseUsingPDKInfo
                           ( dbOpenCellViewByType
                             LibName
                             CellName
                             "$view"
                             nil
                             "r" )
                           $transistor_base_width ) ) )
                      (
                       ( fprintf Output "%s %s %s doesn't exist...did you use cadence names?  Try rename --from=cast --to=cadence --type=cell\n"
                                 LibName
                                 CellName
                                 "$view" ) ) ) ) ) )
  ( close Output ) )
(exit)
EOF

cdsLog="$cds_wd/CDS.log"

$cmd layout -nograph -replay "$input" -log "$cdsLog" </dev/null &>/dev/null

[[ $? == 0 ]] || cat "$cdsLog"
cat "$output"
