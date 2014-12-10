#!/bin/bash
# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$

<<DOC
<h2> Description </h2>
Generates 'abstract' views in your dfII directory.
Abstract views are intended to be used for routing purposes, as they greatly simplify the problem to the router, while keeping all relevant information.  These abstract views:
<ul>
<li> Have pins and boundary.
<li> Have no subcells.
<li> Have merged keepout ('boundary' purpose) instead of the original shapes on non-terminal nets.
<li> Have detail around pins on terminal nets, which is on the 'drawing' purpose.  The --limit-pins option will limit the detail to a radius of one powergrid around the pins.
<li> Have no vias, unles the --draw-vias option is used.
<li> Have no powergrid, unless the --keep-power-grid option is used.  Use the --keepout-power-grid to convert the power grid into keepout instead of metal.
</ul>

<h2> Implementation </h2>
<p> See <skill AbstractCellUsingPDKInfo> and <a href=http://internal.avlsi.com/eng/depot/hw/layout/tsmc13/pdk/main/Fulcrum/cell_automation/abstract.rul>the assura rules from PDK</a>.</p>

<h2> Usage Notes / Debugging </h2>
* Cadence logs are at [output-dir]/[cell]/CDS.log.
* ASSURA runs are found in [output-dir]/ASSURA.XXXXX
DOC


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

arch=x86_64

function usage() {
cat<<USAGE
Usage: $0
--fulcrum-pdk-root=dir
--working-dir=dir
--dfII-dir=dir
[--debug] (use SKILL debugger)
[--p4] (p4 edit cells before creating)
[--subcells] (abstract subcells of cell instead of the cell)
[--simplify] (bloat m3 keepout by half a power grid)
[--draw-vias] (keep via shapes)
[--limit-pins] (abstract shapes more than a powergrid away from pins) [default is to keep all detail connected to pins]
[--keep-power-grid] [default is to delete power grid shapes]
[--keepout-power-grid] [convert powergrid to keepout]
[--lib-cells-to-ignore=[ lib/cell,lib/cell ... ] (regex of subcells to skip)
[--arch=$arch]
  <cell1> <cell2>(cadence names)...
USAGE

}

layout=`mywhich layout` || exit 2
sedcmd=`mywhich sed` || exit 2
grepcmd=`mywhich grep` || exit 2
findcmd=`mywhich find` || exit 2

cell_list=
fulcrum_pdk_root=
working_dir=
dfII_dir=
debug=nil
libCellsToIgnore=""
ErasePowerGrid=t
KeepoutPowerGrid=nil
Internal=nil
Subcells=nil
Simplify=nil
P4=nil
DrawVias=nil
LimitPins=nil
NoPCells=t
verbose=


Host=$(hostname)

for arg in $@                           ; do
case "$arg" in
--fulcrum-pdk-root=* )
  fulcrum_pdk_root=`echo $arg | $sedcmd -e "s/--fulcrum-pdk-root=//"`
  ;;
--dfII-dir=* )
  dfII_dir=`echo "$arg" | $sedcmd -e "s/--dfII-dir=//"`
  ;;
--arch=* )
  arch=`echo "$arg" | $sedcmd -e "s/--arch=//"`
  ;;
--working-dir=* )
  working_dir=`echo "$arg" | $sedcmd -e "s/--working-dir=//"`
  ;;
--lib-cells-to-ignore=* )
  libCellsToIgnore=`echo "$arg" | $sedcmd -e "s/--lib-cells-to-ignore=//"`
  ;;
--debug )
  debug=t
  ;;
--verbose )
  verbose=t
  ;;
--p4 )
  P4=t
  ;;
--draw-vias )
  DrawVias=t
  ;;
--limit-pins )
  LimitPins=t
  ;;
--subcells )
  Subcells=t
  ;;
--keep-pcells )
  NoPCells=nil
  ;;
--simplify )
  Simplify=t
  ;;
--keep-power-grid )
  ErasePowerGrid=nil
  ;;
--keepout-power-grid )
  KeepoutPowerGrid=t
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
    "An NFS working dir must be specified."           2
check_for_empty_arg "$dfII_dir"                                              \
    "A dfII dir must be specified."           2

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

mkcdswd "--dfII-dir=$dfII_dir"                            \
         "--fulcrum-pdk-root=$fulcrum_pdk_root"            \
         "--cast-path=$cast_path"                          \
         "--target-dir=$cds_wd"                            \
         --force

k=1
n=$(echo $cell_list | wc | awk '{print $2}' )

input="$working_dir/rp"
errFile="$working_dir/err"

rm -f "$errFile"
cat<<EOF > "$input"
(load "$package_root/share/skill/autoload.il")
(load "$fulcrum_pdk_root/share/Fulcrum/pdkinfo.il")
EOF

for cell in $cell_list ; do
    cell_dir="$working_dir/$cell"
    mkdir -p "$cell_dir"

    echo "$cell ($k of $n)"
    k=$(($k + 1))

cat<<SKILL>> "$input"

(when $debug ( ilDebugToolBox ) )
(let (
      ( WorkingDir "$working_dir" )
      ( LibCellsToIgnore
        ( append
          ( ListGroup 2 ( parseString "$libCellsToIgnore" ",/" ) )
          ( append
            WiringCellLibCellPairRegExs
            ( append
              TechLibCellPairRegExs
              ( append
                GateLibCellPairRegExs
                StackLibCellPairRegExs ) ) ) ) )
      ( OutPort ( outfile "$errFile" "a" ) )
      ( CellView
        ( dbOpenCellViewByType
          ( car ( NameParseCellName "$cell" ) )
          "$cell"
          "layout"
          "maskLayout"
          "r" ) ) )
    (let (
          ( CellsToAbstract
            (cond (
                   $Subcells          
                   ( ListUniqNoTableElements
                     ( car ( NameFilterInstances
                             CellView->instances
                             LibCellsToIgnore ) ) ~> master
                   ) )
                  (
                   ( list CellView ) ) ) ) )

  (when $P4
    ( CDSP4EditLibCellViewTripples 
      "$Host"
      "$P4USER"
      "$P4PASSWD"
      "$P4CLIENT"
      "$P4CONFIG"
      "default"
      ( mapcar
        (lambda ( CellView )
          ( list 
            CellView->libName 
            CellView->cellName 
            "abstract" ) )
        CellsToAbstract )
      16
      ( sprintf nil "%s/p4.log" WorkingDir )
      WorkingDir ) )
    

  ( foreach
    CellView
    CellsToAbstract
    ( println CellView->cellName )
    (let ( 
          ( DDCell
            ( ddGetObj 
              CellView->libName 
              CellView->cellName 
              "abstract" ) ) )
        (cond (
                ( or ( null DDCell )
                    ( ddIsObjWritable DDCell ) )
                (let (
                      ( Abstract 
                        ( AbstractCellUsingPDKInfo
                          CellView
                          WorkingDir
                          ?ErasePowerGrid
                                        ;don't erase powergrid in powergrid
                          (unless ( rexMatchp "globals" CellView->libName )
                            $ErasePowerGrid )
                          ?Simplify $Simplify
                          ?DrawVias $DrawVias
                          ?KeepoutPowerGrid $KeepoutPowerGrid
                          ?LimitPins $LimitPins
                          ?NoPCells $NoPCells              

                          ) ) )
                  ( leMergeShapes Abstract->shapes )
                  ( dbSave Abstract )
                  ( dbPurge Abstract )
                  ) )
              (
               ( fprintf OutPort "Couldn't write to %L %L %L\n"
                         CellView->libName 
                         CellView->cellName 
                         "abstract" ) ) ) ) )

    ( close OutPort )
) )
SKILL
done

if [[ "$debug" == "nil" ]]; then
    echo "(exit)" >> "$input"
fi

rm -f "$working_dir/CDS.log"
cmd="/p/rrc/tools/bin/ic /p/rrc/tools/bin/assura layout -replay $input -log $working_dir/CDS.log"
if [[ "$debug" == "nil" ]]; then
    cmd="$cmd -nograph"
fi

function my_exec() {
    qrsh -V -now n -cwd -nostdin -noshell -l a=$arch -l mem=1500M $@ </dev/null &>/dev/null
}

cd "$cds_wd" && my_exec $cmd & pid=$!
if [ -n "$verbose" ] ; then
    tail -F --pid=$pid "$working_dir/CDS.log"
else
    wait $pid
fi

cat "$errFile"
