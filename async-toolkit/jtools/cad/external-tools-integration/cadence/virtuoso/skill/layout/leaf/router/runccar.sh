#!/bin/bash

function usage() {
  echo "Usage: $0"
  echo "  --src-lib=lib"
  echo "  --src-cell=cell"
  echo "  --src-lib=view"
  echo "  --dest-lib=lib"
  echo "  --dest-cell=cell"
  echo "  --dest-lib=view"
  echo "  --rule-file=file"
  echo "  --do-file=file"
  echo "  --working-dir=dir"
  echo "  --log-file=file"
  echo "  [--conductor-depth=int"
  echo "  [--pin-connect=strong|weak]"
  echo "  [--keepout-depth=int]" 
  echo "  [--quit]"
  echo "  [--import]"
  echo "  [--no-export-pcells"
  echo "  [--nog]"
}

function mywhich() {
    which $1 | tail -1
}

arch_bin_dir=${0%\/*}
package_root=${arch_bin_dir%\/*}

sh_lib_dir="$package_root/share/script/sh/sh-lib"

source "$sh_lib_dir/file/filecheck.sh"
source "$sh_lib_dir/file/conon.sh"

sedcmd=`mywhich sed`
ccar=`mywhich vcar`
icc2cdba=`mywhich icc2cdba`
cdba2icc=`mywhich cdba2icc`

check_executable_file "$sedcmd"                                 \
   "Unable to find sed in \"$PATH\"."                                             2
check_executable_file "$ccar"                                 \
   "Unable to find ccar in \"$PATH\"."                                             2
check_executable_file "$icc2cdba"                                 \
   "Unable to find icc2cdba in \"$PATH\"."                                             2
check_executable_file "$cdba2icc"                                 \
   "Unable to find cdba2icc in \"$PATH\"."                                             2

src_lib=
src_cell=
src_view=
dest_lib=
dest_cell=
dest_view=
rule_file=
do_files=
conductor_depth=32
keepout_depth=32
nog=
quit=
import=
log_file=/dev/null
debug_log_file=/dev/null
pids=
pinConnect=strong
noExportPCells=
fulcrum_pdk_root=
area=
dfII_dir=

for arg in $@ ; do
  
  case "$arg" in
  --dfII-dir=* )
    dfII_dir=`echo "$arg" | $sedcmd -e "s/--dfII-dir=//"`
    ;;
  --fulcrum-pdk-root=* )
    fulcrum_pdk_root=`echo $arg | $sedcmd -e "s/--fulcrum-pdk-root=//"`
    ;;
  --src-lib=* )
    src_lib=`echo $arg | $sedcmd -e "s/--src-lib=//"`
    ;;
  --src-cell=* )
    src_cell=`echo $arg | $sedcmd -e "s/--src-cell=//"`
    ;;
  --src-view=* )
    src_view=`echo $arg | $sedcmd -e "s/--src-view=//"`
    ;;
  --dest-lib=* )
    dest_lib=`echo $arg | $sedcmd -e "s/--dest-lib=//"`
    ;;
  --dest-cell=* )
    dest_cell=`echo $arg | $sedcmd -e "s/--dest-cell=//"`
    ;;
  --dest-view=* )
    dest_view=`echo $arg | $sedcmd -e "s/--dest-view=//"`
    ;;
  --conductor-depth=* )
    conductor_depth=`echo $arg | $sedcmd -e "s/--conductor-depth=//"`
    ;;
  --pin-connect=* )
    pinConnect=`echo $arg | $sedcmd -e "s/--pin-connect=//"`
    ;;
  --keepout-depth=* )
    keepout_depth=`echo $arg | $sedcmd -e "s/--keepout-depth=//"`
    ;;
  --do-file=* )
    do_file=`echo $arg | $sedcmd -e "s/--do-file=//"`
    do_files="$do_files $do_file"
    ;;
  --rule-file=* )
    rule_file=`echo $arg | $sedcmd -e "s/--rule-file=//"`
    ;;
  --working-dir=* )
    working_dir=`echo $arg | $sedcmd -e "s/--working-dir=//"`
    ;;
  --log-file=* )
    log_file=`echo $arg | $sedcmd -e "s/--log-file=//"`
    ;;
  --area=* )
    area=`echo $arg | $sedcmd -e "s/--area=//"`
    ;;
  --debug-log-file=* )
    debug_log_file=`echo $arg | $sedcmd -e "s/--debug-log-file=//"`
    ;;
  --quit )
    quit=1
    ;;
  --no-export-pcells )
    noExportPCells=1
    ;;
  --nog )
    nog=1
    ;;
  --import )
    import=1
    ;;
  esac
done



check_for_empty_arg "$src_lib"                                   \
    "The name of the library of the src cell view to operate on must be specified."   2
check_for_empty_arg "$src_cell"                                  \
    "The name of the src cell to operate on must be specified."                       2
check_for_empty_arg "$src_view"                                  \
    "The name of the src view to operate on must be specified."                       2
check_for_empty_arg "$dest_lib"                                   \
    "The name of the library of the dest cell view to operate on must be specified."   2
check_for_empty_arg "$dest_cell"                                  \
    "The name of the dest cell to operate on must be specified."                       2
check_for_empty_arg "$dest_view"                                  \
    "The name of the dest view to operate on must be specified."                       2
check_for_empty_arg "$working_dir"                                  \
    "The working directory must be specified."                       2
check_for_empty_arg "$conductor depth"                                  \
    "The conductor depth must be specified."                       2
check_for_empty_arg "$keepout depth"                                  \
    "The keepout depth must be specified."                       2
check_for_empty_arg "$pinConnect"                                  \
    "The pin connect type must be specified."                       2

check_readable_file "$rule_file"                            \
    "Rule File: \"$rule_file\" is not a readable file."                      2
conon_path "$rule_file"
rule_file="$ret"


ccar_cmd="$ccar $working_dir/$src_cell.dsn -virtuoso"

if [ -n "$quit" ] ; then 
  ccar_cmd="$ccar_cmd -quit"
fi

if [ -n "$nog" ] ; then 
  ccar_cmd="$ccar_cmd -nog"
fi

for do_file in $do_files ; do
    check_readable_file "$do_file"                            \
        "Rule File: \"$do_file\" is not a readable file."                      2
    conon_path "$do_file"
    do_file="$ret"
    ccar_cmd="$ccar_cmd -do $do_file"
done

if [ ! -e "$working_dir" ] ; then
  mkdir -p "$working_dir"
fi

echo "Exporting to CCAR..." >> $debug_log_file

if [ -n "$noExportPCells" ] ; then
grep SOFTINCLUDE cds.lib > "$working_dir/cds.lib"
cat<<EOF >> "$working_dir/cds.lib"
UNDEFINE gate
UNDEFINE stack
UNDEFINE gate#2dfake
UNDEFINE stack#2dfake
DEFINE $gate_lib $fulcrum_pdk_root/share/Fulcrum/dfII/gate-fake
DEFINE $stack_lib $fulcrum_pdk_root/share/Fulcrum/dfII/stack-fake
EOF
cp display.drf "$working_dir"
pushd "$working_dir"
else
pushd "$PWD"
fi

#export
if [ -n "$area" ] ; then
    areaOpt="-area $(echo $area | sed -e 's/,/ /g')"
else
    areaOpt=
fi

cat<<EOF> $working_dir/.cdsenv
iccTranslator   exportVersion   int     11
iccTranslator   keepViaImageOnly boolean nil
EOF

cdba2icc $src_lib $src_cell $src_view \
          -exportDirectory $working_dir      \
          -conductorDepth  $conductor_depth  \
          -keepoutDepth    $keepout_depth    \
          -fullConnectivity               \
          -interLayer                     \
          $areaOpt                           \
          -template $rule_file            \
          -pinConnect $pinConnect         \
          -noIncrementalUpdate
popd

[ $? == 0 ] || ( echo "Export Failed" && exit 2)
#route
mytemp=$working_dir

ret=1
while [[ ! ( $ret == 0 ) ]] ; do
  echo "Starting CCAR..." >> $debug_log_file
  TEMP=$mytemp TMP=$mytemp HOME=$working_dir LM_LICENSE_FILE=/usr/local/flexlm/licenses/license.0048546B71E0.cadence.dat OA_HOME=/mnt/fulcrum/local/common/cadence/install/OA-2.2-p056-Linux/OpenAccess $ccar_cmd  2> "$working_dir/ccar.err" 1> "$working_dir/ccar.out"
  ret=$?
  if [ -z "$quit" ] ; then
      ret=0
  fi
  # Don't do this hack any more(always leave the loop).
  # Left here for posterity.
  ret=0
  if [[ ! ( $ret == 0 ) ]] ; then sleep 10 ; fi
done

#import
if [ -n "$import" ] ; then 
    icc2cdba \
     $dest_lib $dest_cell $dest_view \
    -session "$working_dir/$dest_cell.ses"
fi

echo "Writing Results" >> $debug_log_file

echo "Starting Import..." >> $debug_log_file
cross_clear_unroute=`cat $working_dir/monitor.sts | tail -2 | head -1 | cut -f4,5,7 -d "|" | sed -e "s/|//g"`
completion=`grep "Completion" $working_dir/monitor.sts | awk '{print $3}' | sed -e "s/\%//"`

echo $cross_clear_unroute $completion > $log_file

echo "Exiting..." >> $debug_log_file

sync
