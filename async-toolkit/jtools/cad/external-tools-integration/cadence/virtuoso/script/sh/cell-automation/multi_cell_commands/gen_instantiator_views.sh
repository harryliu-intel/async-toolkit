#!/bin/bash
# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$

<<DOC
Make 'instantiator' views for cells in your dfII directory.  See <module layout.hierarchy.instantiator>.
<ol>
<li> Uses <skill InstantiatorsCreateInstantiatorsView> to search up through the hierarchy to find shapes that are on top of the cell somehwere in the hierarchy.
<li> Uses <skill KeepOutCreateRoutingView> to do simple layer processing.
<li> <a href=http://internal.avlsi.com/eng/depot/hw/layout/tsmc13/pdk/main/share/Fulcrum/cell_automation/keepout.rules>the assura rules from PDK</a>
</ol>

<h2> Usage Notes / Debugging </h2>
* output-dir becomes a cadence working directory.  
* Cadence logs are at [output-dir]/CDS.log.
* ASSURA runs are found in [output-dir]/[cell]/ASSURA.XXXXXX
* The generated views are in your dfII-dir.
DOC



function exit_func() {
true
}

trap exit_func EXIT

# assumes run under fulcrum script
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
 "Usage: $0"
--fulcrum-pdk-root=dir
--working-dir=dir
--dfII-dir=dir
[--debug]
   (use SKILL debugger)

[--assura-only] 
  (only do simplification step)

[--max-depth=1024]
  (maximum depth in hierarchy to look for inter-hierarchical use of layers, decrease only if you know it's ok.  This can speed up the process.)

[--use-existing-instantiator-views]
  (if an instantiator view is reached in hierarchical search, copy its contents instead of continuing search upwards)

[--lib-cells-to-ignore=[ lib/cell,lib/cell ... ]
   (regex of subcells to skip during hierarchical search)

[--arch=$arch] (for qrsh)
  <cell1> <cell2>(cadence names)..."
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
lib=dfII
debug=nil
client_spec=
assuraOnly=nil
useExistingInstantiatorViews=nil
libCellsToIgnore=""
p4=1
maxDepth=1024
bottomLayer=2
topLayer=7

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
--assura-only )
  assuraOnly=t
  ;;
--bottom-layer=* )
  bottomLayer=`echo "$arg" | $sedcmd -e "s/--bottomLayer=//"`
  ;;
--top-layer=* )
  topLayer=`echo "$arg" | $sedcmd -e "s/--topLayer=//"`
  ;;
--use-existing-instantiator-views )
  useExistingInstantiatorViews=t
  ;;
--max-depth=* )
  maxDepth=`echo "$arg" | $sedcmd -e "s/--max-depth=//"`
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
         "--target-dir=$cds_wd" --force

keepout_rules="$fulcrum_pdk_root/share/Fulcrum/cell_automation/keepout.rules"

k=1
n=$(echo $cell_list | wc | awk '{print $2}' )

input="$working_dir/rp"
errFile="$working_dir/err"

rm -f "$errFile"
cat<<EOF > "$input"
(load "$package_root/share/skill/autoload.il")
(load "$fulcrum_pdk_root/share/Fulcrum/pdkinfo.il")
InstantiatorsTable = 
( HierarchyMakeCellsInstantiatorsTable
  "floorplan"
  "layout"
  ( ListGroup 2 ( parseString "$libCellsToIgnore" ",/" ) )
 )
EOF

for cell in $cell_list ; do
    cell_dir="$working_dir/$cell"
    mkdir -p "$cell_dir"

    echo "$cell ($k of $n)"
    k=$(($k + 1))

cat<<SKILL >> "$input"

(when $debug ( ilDebugToolBox ) )
(let (
      ( LibCellsToIgnore
        ( append GateLibCellPairRegExs
                 StackLibCellPairRegExs ) )
      ( OutPort ( outfile "$errFile" "a" ) )
      ( PinNetNamesToIgnoreLPP
        ( list GNDNetName VddNetName ) )
      ( LayoutView
        ( dbOpenCellViewByType
          ( car ( NameParseCellName "$cell" ) )
          "$cell"
          "layout"
          "maskLayout"
          (if (let (
                    ( LayoutDDObj
                      ( ddGetObj 
                        ( car ( NameParseCellName "$cell" ) )
                        "$cell"
                        "layout" ) ) )
                ( and
                  LayoutDDObj
                  ( getq LayoutDDObj files )
                  ( forall
                    FileDDObj
                    ( getq LayoutDDObj files )
                    ( ddIsObjWritable FileDDObj ) ) ) )
              "a" "r" )
          ) )
      ( InstantiatorView
        ( dbOpenCellViewByType
          ( car ( NameParseCellName "$cell" ) )
          "$cell"
          "instantiator"
          "maskLayout"
          (if $assuraOnly "a" "w" )
          ) )
      ( EmptyCellView
        ( dbOpenCellViewByType
          ( car ( NameParseCellName "$cell" ) )
          "$cell"
          "instantiator_tmp"
          "maskLayout"
          "w" ) )
      )
  (let (
        ( LPPs 
          ( ListAppend
            (if ( and 
                  ( leqp $bottomLayer 2 )
                  ( geqp $topLayer 2 ) )
                ( list Metal2LPP ) )
            (if ( and 
                  ( leqp $bottomLayer 3 )
                  ( geqp $topLayer 3 ) )
                ( list Metal3LPP ) )
            (if ( and 
                  ( leqp $bottomLayer 4 )
                  ( geqp $topLayer 4 ) )
                ( list Metal4LPP ) )
            (if ( and 
                  ( leqp $bottomLayer 5 )
                  ( geqp $topLayer 5 ) )
                ( list Metal5LPP  ) )
            (if ( and 
                  ( leqp $bottomLayer 6 )
                  ( geqp $topLayer 6 ) )
                ( list Metal6LPP ) )
            (if ( and 
                  ( leqp $bottomLayer 7 )
                  ( geqp $topLayer 7 ) )
                ( list Metal7LPP ) )
            (if ( and 
                  ( leqp $bottomLayer 8 )
                  ( geqp $topLayer 8 ) )
                ( list Metal8LPP ) ) ) ) )

    (unless $assuraOnly
        ( InstantiatorsCreateInstantiatorsView
            LayoutView
            InstantiatorView
            EmptyCellView
            InstantiatorsTable
            "layout"
            "floorplan"
            "dummy"
            BoundaryLPP
            LPPs
            PinNetNamesToIgnoreLPP
            $useExistingInstantiatorViews
            OutPort
            LibCellsToIgnore
            $maxDepth
        )
      )

    ;move target to temp view
    ( dbCopyCellView 
      InstantiatorView
      ( getq EmptyCellView libName )
      ( getq EmptyCellView cellName )
      ( getq EmptyCellView viewName )
      nil nil t )

    ;make final target
    (when t
    ( KeepOutCreateRoutingView
      EmptyCellView
      ( getq InstantiatorView libName )
      ( getq InstantiatorView cellName )
      ( getq InstantiatorView viewName )
      "$keepout_rules"
      "$cell_dir" 
      ( mapcar
       (lambda ( LPP )
         ( KeepOutCreateRoutingLayerStruct LPP ) )
       LPPs )
      PinNetNamesToIgnoreLPP
      )
    )

    ( close OutPort )
    ( dbSave EmptyCellView )
    ( dbPurge EmptyCellView )
    ( dbSave InstantiatorView )
    ( dbPurge InstantiatorView )
    ( dbPurge LayoutView )
) )
SKILL
done

if [[ "$debug" == "nil" ]]; then
    echo "(exit)" >> "$input"
fi

cmd="/p/rrc/tools/bin/ic /p/rrc/tools/bin/assura layout -replay $input -log $working_dir/CDS.log"
if [[ "$debug" == "nil" ]]; then
    cmd="$cmd -nograph"
fi

function my_exec() {
    qrsh -V -now n -cwd -nostdin -noshell -l a=$arch -l mem=1500M $@ </dev/null &>/dev/null
}

cd "$cds_wd" && my_exec $cmd
cat "$errFile"
