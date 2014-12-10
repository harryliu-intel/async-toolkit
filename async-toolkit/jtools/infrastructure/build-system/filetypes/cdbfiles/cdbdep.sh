#!/bin/bash

tempdir=
function exit_func() {
  if [ -d "$tempdir" ] ; then
    cd / && rm -rf "$tempdir"
  fi
}

trap exit_func EXIT

# fixes mostly affecting solaris
unalias sed 2>/dev/null
sedcmd=`which sed`
unalias layout 2>/dev/null
layout=`which layout`

[ -x "$layout" ] || exit 2

parsecellname=$1
#mkcdswd=$2  # don't use this mkcdswd
fulcrum_pdk_root=$3
dfII_dir=$4
file=$5
dep_files=$6
dep_file=$7

source "$fulcrum_pdk_root/share/Fulcrum/script/sh/file/config.sh"
pdk_config="$fulcrum_pdk_root/share/Fulcrum/pdk.config"
config_get_all_values $pdk_config

if [ -d "$TEMP" ] ; then
  basetempdir=$TEMP
elif [ -d "/scratch" ] ; then
  basetempdir=/scratch
else
  basetempdir="$PWD"
fi

tempdir=`mktemp -d "$basetempdir/cdbdep.XXXXXX"`
cds_wd="$tempdir/cds_wd"
mkdir -p "$cds_wd"

mkcdswd "--dfII-dir=$dfII_dir" \
         "--fulcrum-pdk-root=$fulcrum_pdk_root" \
         "--target-dir=$cds_wd" \
         "--force" "--temp"

source "$parsecellname"

get_cadence_cell_name_for_dfII_file $file
cell=$ret

get_cadence_view_name_for_dfII_file $file
view=$ret

get_lib_name $cell
lib=$ret

input="$tempdir/cds.in"
tmp_dep=`mktemp $tempdir/dep.XXXXXX`

cat <<EOF >"$input"
(let (
      ( DepFiles ( parseString "$dep_files" " " ) ) )
  (let (
      ( Outport ( outfile "$tmp_dep" ) )
      ( CellView
        ( dbOpenCellViewByType
          "$lib"
          "$cell"
          "$view"
          nil
          "r" ) ) )
  (let (
        ( SubCellFiles ( reverse ( HierarchyGetAllCellsInTree CellView
    ( list ( list "^$tech_lib$" ".*" )
           ( list "^$gate_lib$" ".*" )
           ( list "^$stack_lib$" ".*" ) ) ) )
           ~> fileName ) )
    ( fprintf Outport "CDBDEP_TARGET_FILES := " )
    ( foreach DepFile DepFiles ( fprintf Outport "%s " DepFile ) )
    ( fprintf Outport "\n" )
    ( fprintf Outport "\$(CDBDEP_TARGET_FILES): \\\\\n" )
    ( foreach SubCellFile SubCellFiles
       (if ( not ( exists DepFile DepFiles ( equal SubCellFile DepFile ) ) )
         ( fprintf Outport "%s \\\\\n" SubCellFile ) ) )
    ( fprintf Outport "\n" )
    ( close Outport ) ) ) )
(exit)
EOF

cd "$cds_wd" && $layout -log "$tempdir/cds.log" -nograph -replay "$input" </dev/null &>/dev/null

cat "$tmp_dep" | sed -e "s/\#/\\\#/g" > "$dep_file"

[ -s "$dep_file" ] || ( echo "Failed to create $dep_file" && exit 2 )
