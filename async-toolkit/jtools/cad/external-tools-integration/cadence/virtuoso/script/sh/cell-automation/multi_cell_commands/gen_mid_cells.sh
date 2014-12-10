#!/bin/bash

function my_exec() {
    qrsh -V -now n -cwd -nostdin -noshell -l mem=500 -p -20 $@ </dev/null &>/dev/null &
}

function exit_func() {
    true
}

trap exit_func EXIT

arch_bin_dir=${0%\/*}
package_root=${arch_bin_dir%\/*}
sh_lib_dir="$package_root/share/script/sh/sh-lib"

source "$sh_lib_dir/file/filecheck.sh"
source "$sh_lib_dir/file/conon.sh"
source "$sh_lib_dir/file/config.sh"

cds_wd_template="$package_root/share/script/sh/setup/cds_wd_default_template"


#
##### Parse arguments #####
#

function usage() {

  echo "Usage: $0"
  echo "  --fulcrum-pdk-root=dir"
  echo "  --working-dir=dir"
  echo "  --dfII-dir=dir"
  echo "  --cast-path=cast_path"
  echo "  [--graphics]"
  echo "  <cell1> <cell2>..."
}

layout=`mywhich layout` || exit 2
sedcmd=`mywhich sed` || exit 2
grepcmd=`mywhich grep` || exit 2
findcmd=`mywhich find` || exit 2
awkcmd=`mywhich awk` || exit 2

cell_list=
fulcrum_pdk_root=
working_dir=
dfII_dir=
lib=dfII
noprepare=
graphics=nil

for arg in $@ ; do
  case "$arg" in
  --fulcrum-pdk-root=* )
    fulcrum_pdk_root=`echo $arg | $sedcmd -e "s/--fulcrum-pdk-root=//"`
    ;;
  --dfII-dir=* )
    dfII_dir=`echo "$arg" | $sedcmd -e "s/--dfII-dir=//"`
    ;;
  --working-dir=* )
    working_dir=`echo "$arg" | $sedcmd -e "s/--working-dir=//"`
    ;;
  --cast-path=* )
    cast_path=`echo "$arg" | $sedcmd -e "s/--cast-path=//"`
    ;;
  --noprepare )
    noprepare=1
    ;;
  --graphics )
    graphics=t
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
    "The packageroot variable must contain the location of the package installation."  \         2
check_for_empty_arg "$fulcrum_pdk_root"                                              \
    "The fulcrum pdk root mist be specified."           2
check_for_empty_arg "$working_dir"                                              \
    "An NFS working dir must be speciified."           2
check_for_empty_arg "$dfII_dir"                                              \
    "A dfII dir must be speciified."           2
check_for_empty_arg "$cast_path"                                              \
    "A cast path must be speciified."           2

check_readable_dir "$fulcrum_pdk_root"                                           \
    "Fulcrum PDK: \"$fulcrum_pdk_root\" is not a readable directory."         2
conon_path "$fulcrum_pdk_root"
fulcrum_pdk_root="$ret"

check_readable_dir "$working_dir"                                           \
    "NFS working dir: \"$working_dir\" is not a readable directory."                  2
conon_path "$working_dir"
working_dir="$ret"

check_readable_dir "$dfII_dir"                                           \
    "dfII dir: \"$dfII_dir\" is not a readable directory."                  2
conon_path "$dfII_dir"
dfII_dir="$ret"

blank_lib="$fulcrum_pdk_root/share/Fulcrum/blank-library"
mkdir -p "$working_dir"
cds_wd="$working_dir"

mkcdswd="$arch_bin_dir/mkcdswd"
check_executable_file "$mkcdswd"                                                 \
    "mkcdswd: \"$mkcdswd\" is not an executable file."                                 2

$mkcdswd "--dfII-dir=$dfII_dir"                            \
         "--fulcrum-pdk-root=$fulcrum_pdk_root"            \
         "--cast-path=$cast_path"                          \
         "--target-dir=$cds_wd"                            \

newlibdir="$working_dir/dfII"
if [ ! -d "$newlibdir" ] ; then
    cp -a "$blank_lib" "$newlibdir"
    $findcmd "$newlibdir" -print0 | xargs -0 chmod u+w
fi
if [ -z "$(grep "DEFINE dfII $newlibdir" "$cds_wd/cds.lib" )" ] ; then
    echo "DEFINE dfII $newlibdir" >> "$cds_wd/cds.lib"
fi

cast2skill="$arch_bin_dir/cast2skill"
cast2cdl="$arch_bin_dir/cast2cdl"
check_executable_file "$cast2skill" \
    "cast2skill: \"$cast2skill\" is not a readable, executable file."  2
check_executable_file "$cast2cdl" \
    "cast2cdl: \"$cast2cdl\" is not a readable, executable file."  2

pdk_config="$fulcrum_pdk_root/share/Fulcrum/pdk.config"
config_get_all_values source pdk_config

k=1
n=$(echo $cell_list | wc | $awkcmd '{print $2}' )

for cell in $cell_list ; do

    cell_dir="$working_dir/$cell"
    mkdir -p "$cell_dir"
    input="$cell_dir/mid.rp"

    echo "$cell ($k of $n)"
    cdl_file="$cell_dir/cell.cdl"
    if [[ ! ( -s "$cdl_file" ) || ( -z "$noprepare" ) ]] ; then
        echo "Cast2Cdl..."
        $cast2cdl --cell="$cell" \
            --cast-path="$cast_path" \
            --output="$cdl_file" \
            --cadence-name
    fi

    k=$(($k + 1))

    directives_dir="$cell_dir/ildirectives"
    directives_file="$directives_dir/$cell.directives.il"   
    if [[ ! ( -s "$directives_file" ) || ( -z "$noprepare" ) ]] ; then
        echo "Cast2Skill..."
        $cast2skill --cell="$cell" \
            --cast-path="$cast_path" \
            --output-dir="$cell_dir" \
            --root-only \
            --cadence-name
    fi

cat<<EOF > "$input"
(load "$package_root/share/skill/autoload.il")
(load "$fulcrum_pdk_root/share/Fulcrum/pdkinfo.il")
(let (
      ( CellName "$cell" )
      ( TargetLibName "$lib" )
      ( FulcrumPDKRoot "$fulcrum_pdk_root" )
      ( SchematicFile "$cdl_file" )
      ( WorkingDir "$cell_dir" )
      ( LPPsToUse ( list Metal2LPP Metal3LPP ) )
      ( LPPsToSearch ( list Metal4LPP Metal5LPP ) )
    )
    
    ( MidLevelAutoUsingPDKInfo 
      CellName
      TargetLibName
      FulcrumPDKRoot
      SchematicFile
      WorkingDir
      LPPsToUse
      LPPsToSearch
      ?Graphics $graphics ) )
(exit)
EOF

cmd="layout -replay $input -log $cell_dir/CDS.log"
if [[ "$graphics" == "nil" ]]; then
    cmd="$cmd -nograph"
fi

cp -f "$cds_wd/cds.lib" "$cell_dir/cds.lib"
cp -f "$cds_wd/display.drf" "$cell_dir/display.drf"

cd "$cell_dir" && my_exec $cmd


done

wait
