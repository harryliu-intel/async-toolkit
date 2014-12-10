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
  echo "  --spec-file=file"
  echo "  --working-dir=dir"
  echo "  --log-file=file"
  echo "  [--conductor-depth=int"
  echo "  [--keepout-depth=int]" 
  echo "  [--quit]"
  echo "  [--space]"
  echo "  [--import]"
  echo "  [--nog]"
}

function mywhich() {
    which $1 | tail -1
}

package_root="$packageroot$"

sh_lib_dir="$package_root/share/script/sh/sh-lib"

source "$sh_lib_dir/file/filecheck.sh"
source "$sh_lib_dir/file/conon.sh"

placer_pl="$package_root/share/skill/layout/leaf/placer/placer.pl"

sedcmd=`mywhich sed`
place=`mywhich vcp.exe`
icc2cdba=`mywhich icc2cdba`
cdba2icc=`mywhich cdba2icc`

check_executable_file "$sedcmd"                                 \
   "Unable to find sed in \"$PATH\"."                                              2
check_executable_file "$place"                                  \
   "Unable to find ccar in \"$PATH\"."                                             2
check_executable_file "$icc2cdba"                               \
   "Unable to find icc2cdba in \"$PATH\"."                                         2
check_executable_file "$cdba2icc"                               \
   "Unable to find cdba2icc in \"$PATH\"."                                         2

src_lib=
src_cell=
src_view=
dest_lib=
dest_cell=
dest_view=
rule_file=
spec_file=
conductor_depth=32
keepout_depth=32
space=
nog=
quit=
import=
log_file=/dev/null
debug_log_file=/dev/null

for arg in $@ ; do
  
  case "$arg" in
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
  --keepout-depth=* )
    keepout_depth=`echo $arg | $sedcmd -e "s/--keepout-depth=//"`
    ;;
  --rule-file=* )
    rule_file=`echo $arg | $sedcmd -e "s/--rule-file=//"`
    ;;
  --spec-file=* )
    spec_file=`echo $arg | $sedcmd -e "s/--spec-file=//"`
    ;;
  --working-dir=* )
    working_dir=`echo $arg | $sedcmd -e "s/--working-dir=//"`
    ;;
  --log-file=* )
    log_file=`echo $arg | $sedcmd -e "s/--log-file=//"`
    ;;
  --debug-log-file=* )
    debug_log_file=`echo $arg | $sedcmd -e "s/--debug-log-file=//"`
    ;;
  --quit )
    quit=1
    ;;
  --space )
    space=1
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

check_readable_file "$rule_file"                            \
    "Rule File: \"$rule_file\" is not a readable file."                      2
conon_path "$rule_file"
rule_file="$ret"

check_writeable_file "$spec_file"                            \
    "Spec File: \"$spec_file\" is not a writeable file."                      2
conon_path "$spec_file"
spec_file="$ret"



if [ ! -e "$working_dir" ] ; then
  mkdir -p "$working_dir"
fi

echo "Exporting to sbtools..." >> $debug_log_file

#export
$cdba2icc $src_lib $src_cell $src_view \
          -exportDirectory $working_dir   \
          -conductorDepth  $conductor_depth  \
          -keepoutDepth    $keepout_depth            \
          -fullConnectivity               \
          -interLayer                     \
          -template $rule_file            \
          -pinConnect strong              \
          -noIncrementalUpdate            \
          &>/dev/null                     \

echo "Starting sbtools..." >> $debug_log_file

mytemp=`mktemp -d $working_dir/placer.XXXXXX`

do_file="$working_dir/$src_cell.do"
dsn_file="$working_dir/$src_cell.dsn"
wir_file="$working_dir/$src_cell.wir"

#setup
echo 'application_mode placement'                        > $do_file
echo 'dlp_set serial_node_weight 15.0'                  >> $do_file
echo 'dlp_set (dp_design_style uniheight_std_cell)'     >> $do_file
echo 'dlp_set auto_row_column on'                       >> $do_file

$placer_pl $spec_file $dsn_file $wir_file               >> $do_file

bbox=`cat $spec_file | grep "^\#" | sed -e "s/\#//"`
echo "select all io_port"                        >> $do_file
echo "unprotect selected io_port"                >> $do_file
echo "unselect all io_port"                      >> $do_file

#place

#echo 'dlp_set dp_permit_rotate on'                                    >> $do_file
#echo "dlp_set (ch_compact_style vertical)"                            >> $do_file
#echo "dlp_set (allow_diff_row_sharing on)"                            >> $do_file
#echo 'dlp_set do_channel_compact on'                                  >> $do_file
#echo 'dlp_set ch_check_component_spacing on'                          >> $do_file
#echo 'dlp_set ch_congestion_analysis on'                              >> $do_file

echo 'dlp_set dp_penalty_violation 100.0'                             >> $do_file
echo 'dlp_set dp_permit_move on'                                      >> $do_file
echo 'dlp_set dp_permit_swap on'                                      >> $do_file
echo 'dlp_set dp_permit_group_move on'                                >> $do_file
echo 'dlp_set dp_permit_group_swap on'                                >> $do_file
echo 'dlp_set swap_ports off'                                         >> $do_file
echo 'dlp_set fix_orient_flip off'                                    >> $do_file
echo 'device_gplace'                                                  >> $do_file
echo 'dlp_set (dp_internal_loop 2)'                                  >> $do_file
echo 'dlp_set (dp_external_loop 2)'                                  >> $do_file
echo 'device_dplace (dp_run_quick_dplace on) (dp_run_move_n_swap on)' >> $do_file
#echo 'dlp_set dp_run_place_compaction on'                             >> $do_file
#echo "device_compact_design"                                          >> $do_file

#post place spacer
if [ -n "$space" ] ; then
 cat $spec_file | grep "^define" | awk '{print "fence " $5 " " $6 " " $7 " " $8}' >> $do_file
 echo "fence digitized"                                                >> $do_file
 echo "dlp_set (swap_ports off)"                                       >> $do_file
 echo "device_compact_design"                                          >> $do_file
 echo "delete fence"                                                   >> $do_file
fi

#output
echo "report place_density "$working_dir/$src_cell.cng""              >> $do_file
echo "report place_status "$working_dir/$src_cell.pst""               >> $do_file
echo "report dlp_violation "$working_dir/$src_cell.vio""              >> $do_file
echo "write session "$working_dir/$src_cell.ses""                     >> $do_file

#place
place_cmd="$place -product 3100 $dsn_file -guidir "$working_dir" -do $do_file -noclean"
if [ -n "$quit" ] ; then 
  place_cmd="$place_cmd -quit"
fi

if [ -n "$nog" ] ; then 
  place_cmd="$place_cmd -nog"
fi

TEMP=$mytemp TMP=$mytemp LM_LICENSE_FILE=/usr/local/flexlm/licenses/license.0048546B71E0.cadence.dat $place_cmd >> $debug_log_file

echo "Starting Import..." >> $debug_log_file

#import
if [ -n "$import" ] ; then 
  $icc2cdba $dest_lib $dest_cell $dest_view \
            -session $working_dir/$src_cell.ses >> $debug_log_file
fi

echo "Writing Results" >> $debug_log_file

rm -rf $mytemp

cat $working_dir/$src_cell.vio > $log_file

echo "Exiting..." >> $debug_log_file

sync
