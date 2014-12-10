#!/bin/bash
# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$

<<DOC
</h2> Summary </h2>
<p> Generates leaf cells using virtuoso,ccar,vcp and SKILL code, starting with CAST. </p>
The cells:<br>
<ul>
<li> Are the minimal width the placer/router could obtain.
<li> Are fixed height, based on the bitpitch/height directives.
<li> Have components placed in alternating (n-p)-(p-n) columns.
<li> LVS clean
<li> Have well plugs(but not guarnteed to be enough)
<li> Are not always DRC clean after routing, but an attempt is made to clean the results using assura layer processor functionality.
<li> Are best suited for medium sized leaf cells(not to say it only works on medium sized cells).  Big cells (>8 columns,) tend to confuse it, and small cells are often better left up to layout engineers.
<li> Have contact shared gates/stacks.
</ul>

<p> The tool is intended to work for all available technologies.  It gets all the information for the technology in various places in the <package fulcrum-pdk>.  In particular, the SKILL global variable definitions defining names of things, and important numbers are in the pdkinfo.il files. See <module tsmc13.pdkinfo> and <module tsmc13.pdkinfo>.</p>

<h2> Description </h2>

<p> I break up the description into two parts.  The <a href=#glcscript>Script Description</a> is for everything the script does, up until running a SKILL replay file. The <a href=glcskill>SKILL description</a> describes the meat of the tool, which takes place in the skill.</p>

<h2><a name=glcscript> Script Description </a></h2>
This script:
<ol>
<li> Creates a usable cadence working directory with <tool mkcdswd>.
<li> For each cell in the cell list, creates a cell directory [output-root]/<cell>
<li> For each cell, calls com.avlsi.tools.jauto.Cast2Cdl to create [output-root]/<cell>/cell.cdl
<li> For each cell, calls com.avlsi.tools.cast2skill.Cast2Skill to create :
 <ol> 
 <li> [output-root]/[cell]/ilnets/*.netlist.il  needed to generated components with <module updatenetlist>/<module genfromsource> 
 <li> [output-root]/[cell]/autopins/[cell].pins.il files to draw pins.
 <li> [output-root]/[cell]/directives/[cell].directives.il for directives used in routing/pins.
 </ol>
<li> Creates a replay file leaf.il from <a href=http://internal/eng/depot/sw/cad/external-tools-integration/cadence/virtuoso/main/script/sh/cell-automation/templates/leaf_cell.il.template>leaf_cell.il.template</a>
<li> Runs <tool qb> layout -replay leaf.il 
</ol>

<h2> <a name=glcskill> SKILL Description </a> </h2>

<p> We break up the execution into 3 steps, instantiation of the netlist, .</p>

<h3> <skill GenFromSource> </h3>
<ol>
<li> Instantiate the gates and stacks in the 'genfromsource' view.
</ol>

<h3> <skill PlacerSearch> </h3>

<ol>
<li> Tweak parameters Fold and chain the chains.
<li> Fold and chain the chains into superstacks based on heuristics. 
<li> Fold and chain the gates into superstacks based on heuristsics. 
<li> Choose n/p widths.
<li> Draw pins prior to placement.
<li> Place the superstacks.
<li> Vertically and horizontally optimize stacks in each column.  That is, squeeze the folding as much as possible while still fitting in the column.  Tricky.  There's an option --nocompactsuperstacks to turn this off as it sometimes makes things worse.  Usually it helps quite a bit to leave it on.
<li> Vertically spread out gates/stacks placement to improve routability.
<li> Draw well plugs (substrate contacts) in each column.
<li> Now that we know what's in all of the columns(they've been compacted and well plugs have been placed), squish(abut) the columns together horizontally.
<li> Draw well/implant in each column.
<li> Redraw the boundary and pins.
<li> Check the resulting placement for valididty using a simple drc check.  If passes, create a 'preroute_XXX' and 'placed_XXX' view.
<li> Do a binary search on the n/p widths based on the success of the drccheck.  Go back to 1 unless we've searched all possible column n/p widths.
</ol>

<h3> <skill RouterSearch> </h3>
<ol>
<li> Space the chains out from the center to allow for more poly routing bandwidth.
<li> Space apart the columns (add routing width) for more vertical m2 routing bandwidth.
<li> Route the cell.
<li> Check LVS
<li> Attempt to fix DRC errors from router.
<li> Do a binary search on the routing widths of each column, based on the success of the lvs check.  Go back to 1 unless we've searched all possible router widths between 0 and 16*wirepitch for each column.
 </ol>


<h2> References </h2>
<ul>
<li> See <module layout.notches.notches> for how we fix DRC errors from the router.
<li> See <module layout.chain.superstack_tools> for heuristics used in supestacking.
<li> See <tool runplacer>/<module layour.leaf.placer.placer> for how we do placement. 
<li> See <tool runccar>/<module layout.leaf.router.router> for how we do routing.
<li> See <module layout.pins.autopin> and cast2skill to see how we generate pins.
<li> See <module layout.pins.vstruts> for how we do Vertical m2 and Horizontal m3 GND/Vdd struts and contacts.
<li> See <module layout.leaf.plugs.plugs> to see how we generate plugs/substrate contacts.
</ul>

<h2> Output/Debugging </h2>
For debugging, note the location of these various files:<br>

<dl>
 <dt> cadence log.  Dump of a bunch of SKILL debugging information.  
      grep for Error if something horrible happened.
 <dd> [output-root]/[cell]/CDS.log

 <dt> Generated layout.  Consists of various types of views.<br>
      The _XXX corresponds to the iteration number.<br>
      In the order they are created, these are:<br>
      <dl>
      <dt> scratch
      <dd> The view that glc last touched.

      <dt> genfromsource
      <dd> Unplaced components.

      <dt> preroutescratch_XXX
      <dd> Placed components with no implant/nwell layers/power routing.

      <dt> placedscratch_XXX
      <dd> Placed components with implant/nwell layers/power routing.

      <dt> preroute_XXX
      <dd> A preroutescratch_XXX view that passes a rudimentary prerouting drc check.  Used by the routing stage.

      <dt> placed_XXX
      <dd> A placedscratch_XXX view that passes a rudimentary prerouting drc check.  A good starting point for semi-automated layout if routing doesn't give good results.

      <dt> placed
      <dd> The smallest placed view.

      <dt> preroute
      <dd> The smallest preroute view.

      <dt> routedscratch_XXX
      <dd> A view routed with ccar.

      <dt> routed_XXX
      <dd> A view routed with ccar that passes LVS!

      <dt> routed
      <dd> The smallest routed_XXX view.
      </dl>
 <dd> [output-root]/dfII/[cell]/<br>
 
 <dt> Routing (icc/ccar working dir)
 <dd> [output-root]/CCAR.XXXXXX<br>

 <dt> Placement (icc/sbtool.exe working dir)
 <dd> [output-root]/PLACER.XXXXXX
 
 <dt> All assura runs (temporary working dir)
 <dd> [output-root]/ASSURA.XXXXXX
 
 <dt> well plugs (temporary dfII library)
 <dd> [output-root]/PLUGS...
 
 <dt> assura drc (completed)
 <dd> [output-root]/[cell]/assura_drc/[view]
 
 <dt> assura lvs (completed)
 <dd> [output-root]/[cell]/assura_lvs/[view]
 
 </dl>
 
DOC
 
function usage() {
    cat<<USAGE 
    $0
        --output-root=dir
        --cast-path=dir
        --fulcrum-pdk-root=dir
        --cell-list=file (one cadence-named cell per line)
       [--noplace](don't run placement stage)
       [--noroute](don't run routing stage)
       [--noprepare](don't re-run java tools)
       [--nocompactsuperstacks](don't run compactSuperstacks)
       [--no-placer-history](don't keep placer scratch views )
       [--no-router-history](don't keep router scratch views )
       [--full-drc](routing must pass drc)
       [--dfII-dir=dir](used for instantiator views and for mkcdswd)
       [--cds-wd-template=dir](for mkcdswd)
       [--os=lx24-amd64](the -l a= parameter for qb, default lx24-amd64)
       [--mem=2000](the -l mem= parameter for qb, default 2000)
       [--numruns=8](the number of compute jobs to start with qb)
       [--debug](execute w/ cadence SKILL debugger)
USAGE
}

function exit_func() {
    if [ -d "$cmd_tmp_dir" ] ; then
        rm -rf "$cmd_tmp_dir"
    fi
}

trap exit_func EXIT

arch_bin_dir=${0%\/*}
package_root=${arch_bin_dir%\/*}
bin_dir="$package_root/bin"
sh_lib_dir="$package_root/share/script/sh/sh-lib"

source "$sh_lib_dir/file/filecheck.sh"
source "$sh_lib_dir/file/conon.sh"
source "$sh_lib_dir/file/config.sh"
source "$sh_lib_dir/script/generate_script_with_libs.sh"

layout=`mywhich layout` || myexit "$layout" 2
assura=`mywhich assura` || myexit "$assura" 2
ccar=`mywhich ccar` || myexit "$ccar" 2
sedcmd=`mywhich sed` || myexit "$sedcmd" 2
grepcmd=`mywhich grep` || myexit "$grepcmd" 2
bashcmd=`mywhich bash` || myexit "$bashcmd" 2

graphics=nil
cast_path=
fulcrum_pdk_root=
place=t
route=t
noprepare=t
compactsuperstacks=t
cell_list_file=
keepplacerviews=t
keeprouterviews=t
numruns=8
output_root_dir=
cds_wd_template="$package_root/share/script/sh/setup/cds_wd_default_template"
plugs_in_routing_search=nil
#these are hard-coded now
dfII_lib_name=dfII
debug=nil
dfII_dir=
os="lx24-amd64"
mem=2000M

for arg in $@ ; do
    
    case "$arg" in
        --cast-path=* )
        cast_path=`echo $arg | $sedcmd -e "s/--cast-path=//"`
        ;;
        --dfII-dir=* )
        dfII_dir=`echo $arg | $sedcmd -e "s/--dfII-dir=//"`
        ;;
        --fulcrum-pdk-root=* )
        fulcrum_pdk_root=`echo $arg | $sedcmd -e "s/--fulcrum-pdk-root=//"`
        ;;
        --debug )
        debug=t
        ;;
        --noplace )
        place=nil
        ;;
        --noroute )
        route=nil
        ;;
        --noprepare )
        noprepare=1
        ;;
        --os=* )
        os=`echo $arg | $sedcmd -e "s/--os=//"`
        ;;
        --mem=* )
        mem=`echo $arg | $sedcmd -e "s/--mem=//"`
        ;;
        --nocompactsuperstacks )
        compactsuperstacks=nil
        ;;
        --no-placer-history )
        keepplacerviews=nil
        ;;
        --no-router-history )
        keeprouterviews=nil
        ;;
        --full )
        semiauto=
        ;;
        --plugs-in-routing-search )
        plugs_in_routing_search=1
        ;;
        --cds-wd-template=* )
        cds_wd_template=`echo $arg | $sedcmd -e "s/--cds-wd-template=//"`
        ;;
        --cell-list=* )
        cell_list_file=`echo $arg | $sedcmd -e "s/--cell-list=//"`
        ;;
        --numruns=* )
        numruns=`echo $arg | $sedcmd -e "s/--numruns=//"`
        ;;
        --output-root=* )
        output_root_dir=`echo $arg | $sedcmd -e "s/--output-root=//"`
        ;;
        --* )
        echo "Unknown option \"$arg\"."
        usage
        exit 2
        ;;
    esac
done

check_for_empty_arg "$package_root"                                              \
    "The packageroot variable must contain the location of the package installation."           2
check_for_empty_arg "$cast_path"                                                    \
    "The cast path was not specified."                                                          2
check_for_empty_arg "$fulcrum_pdk_root"                                          \
    "You must specify the location of the fulcrum pdk."                                         2
check_for_empty_arg "$cell_list_file"                                            \
    "You must specify a file listing all the cell to operate on."                               2
check_for_empty_arg "$output_root_dir"                                           \
    "You must specify an output directory for all the files generated by this script."          2
check_for_empty_arg "$numruns"                                                  \
    "You must specify the number of cells per layout run"                                       2

check_readable_dir "$package_root"                                               \
    "Package Installation: \"$package_root\" is not a readable directory."             2

check_readable_dir  "$fulcrum_pdk_root"                                          \
    "Fulcrum PDK: \"$fulcrum_pdk_root\" is not a readable directory."                  2
conon_path "$fulcrum_pdk_root"
fulcrum_pdk_root="$ret"

check_readable_file "$cell_list_file"                                            \
    "Cell list file: \"$cell_list_file\" is not a readable file."                      1
conon_path "$cell_list_file"
cell_list_file="$ret"

check_writeable_dir "$output_root_dir"                                           \
    "Root output directory: \"$output_root_dir\" is not a readable, writeable directory." 1
conon_path "$output_root_dir"
output_root_dir="$ret"

cds_sh_lib="$package_root/share/script/sh/util"
source "$cds_sh_lib/parsecellname"
lib_commands_dir="$package_root/share/script/sh/cell-automation/lib_commands"
templates_dir="$package_root/share/script/sh/cell-automation/templates"
pdk_config="$fulcrum_pdk_root/share/Fulcrum/pdk.config"
blank_lib="$fulcrum_pdk_root/share/Fulcrum/blank-library"

check_readable_dir "$cds_sh_lib" \
    "Cadence Shell Script Library: \"$cds_sh_lib\" is not a readable directory." 2
check_readable_dir "$bin_dir" \
    "Package bin directory \"$bin_dir\" is not a readable directory." 2
check_readable_dir "$lib_commands_dir"                                           \
    "library commands directory: \"$lib_commands_dir\" is not a directory."            2
check_readable_dir "$templates_dir"                                              \
    "Templates Directory: \"$templates_dir\" is not a readable directory."             2
check_readable_dir "$cds_wd_template"                                              \
    "CDS working directory Template: \"$cds_wd_template\" is not a readable directory."             2

cds_wd=$output_root_dir
mkdir -p $output_root_dir

### pdk config

#if core was dumped...so silly
[ -f "$output_root_dir/core" ] && rm "$output_root_dir/core"

### Setup output dfII_dir
dfII_lib_name=dfII
output_dfII_dir="$output_root_dir/$dfII_lib_name"
echo "DFII lib located at: $output_dfII_dir"
logfile="$output_dfII_dir/gen_leaf_cells.log"
rm -f $logfile


cmd_tmp_dir=`mktemp -d /tmp/gen_leaf_cells.XXXXXX`
#mkcdswd_src="$bin_dir/mkcdswd"
#check_readable_file "$mkcdswd_src"     \
#    "mkcdswd: \"$mkcdswd\" is not a readable file."          2
#generate_command_script "$cmd_tmp_dir" "$mkcdswd_src"  "$sh_lib_dir" "$bashcmd"
#mkcdswd="$ret"
#check_executable_file "$mkcdswd"     \
#    "mkcdswd: \"$mkcdswd\" is not a readable, executable file."    2
mkcdswd="$bin_dir/mkcdswd"
check_readable_file "$mkcdswd"     \
    "mkcdswd: \"$mkcdswd\" is not a readable file."          2



qb="$bin_dir/qb"

##cds_sh_lib
cds_sh_lib="$package_root/share/script/sh/util"
check_readable_dir "$cds_sh_lib" \
    "Cadence Shell Script Library: \"$cds_sh_lib\" is not a readable directory." 2
cds_sh_lib_files=`find "$cds_sh_lib" \! -type d `

mkcdslibs_src="$lib_commands_dir/mkcdslibs"
check_readable_file "$mkcdslibs_src"     \
    "mkcdslibs: \"$mkcdslibs\" is not a readable file."          2
generate_command_script "$cmd_tmp_dir" "$mkcdslibs_src"  "$sh_lib_dir" "$bashcmd"
mkcdslibs="$ret"
check_executable_file "$mkcdslibs"     \
    "mkcdslibs: \"$mkcdslibs\" is not a readable, executable file."    2

#mkcdslib_src="$lib_commands_dir/mkcdslib"
#check_readable_file "$mkcdslib_src"     \
#    "mkcdslib: \"$mkcdslib\" is not a readable file."          2
#generate_command_script "$cmd_tmp_dir" "$mkcdslib_src"  "$sh_lib_dir" "$bashcmd"
#mkcdslib="$ret"
mkcdslib="$bin_dir/mkcdslib"
check_executable_file "$mkcdslib"     \
    "mkcdslib: \"$mkcdslib\" is not a readable, executable file."    2

for file in $cds_sh_lib_files ; do
    source "$file"
done

### Make cds_wd
echo "Making $cds_wd..."
$mkcdswd --target-dir=$cds_wd --user-template=$cds_wd_template --fulcrum-pdk-root=$fulcrum_pdk_root --cast-path=$cast_path --force --dfII-dir=$dfII_dir
echo "done"

rm -f "$output_root_dir/cds.lib.generated"

### Make dfII libraries
echo "creating libs in DFII..."
$mkcdslib  --generated-libraries-root=$output_root_dir  \
    --cds-wd=$cds_wd                     \
    --blank-cds-library=$blank_lib       \
    --cadence-shell-library=$cds_sh_lib  \
    --lib=$dfII_lib_name
echo "done"

cells=`cat $cell_list_file | $grepcmd "^[^#]"`

#include dfII_dir (for instantiator views)
if [ -n "$dfII_dir" ] ; then
    cds_lib_generated="$dfII_dir/cds.lib.generated"
    iscdslib=$($grepcmd "SOFTINCLUDE $cds_lib_generated" "$cds_wd/cds.lib")
    if [ -z "$iscdslib" ] ; then
        mv "$cds_wd/cds.lib" "$cds_wd/cds.lib_bak"
        echo "SOFTINCLUDE $cds_lib_generated" > "$cds_wd/cds.lib"
        echo "UNDEFINE gate" >> "$cds_wd/cds.lib"
        echo "UNDEFINE stack" >> "$cds_wd/cds.lib"
        cat "$cds_wd/cds.lib_bak" >> "$cds_wd/cds.lib"
    fi
fi

target_layout_script="$output_root_dir/main.sh"
echo 'source /usr/local/grid/default/common/settings.sh' > $target_layout_script

k=0
n=$(wc $cell_list_file | awk '{print $2}' )

for cell in $cells; do
    k=$(($k + 1))
    echo "Doing $cell ( $k of $n )"

    #make dirs
    cell_dir="$output_root_dir/$cell"
    cell_wd="$cell_dir/cds_wd"
    mkdir -p "$cell_wd"

    #make files
    input="$cell_dir/leaf.il"
    cadence_log="$cell_dir/CDS.log"
    cell_script="$cell_dir/layout.sh"
    cdl_file="$cell_dir/cell.cdl"
    
    cp -f "$cds_wd/cds.config" "$cell_wd/cds.config"
    cp -f "$cds_wd/.cdsinit" "$cell_wd/.cdsinit"
    cp -f "$cds_wd/cds.lib" "$cell_wd/cds.lib"
    cp -f "$cds_wd/display.drf" "$cell_wd/display.drf"

    run_num=$[ $[ $k - 1 ] % $numruns ]
    this_run_script="$output_root_dir/glc"_"$run_num.sh"

    #starting a new run script
    if [[ $[ $[ $k - 1 ] / $numruns ] == 0 ]] ; then
        echo "QRSH_FLAGS='-l mem=$mem -l a=$os -l centos=4 -N glc_$run_num' QB_RUN_NAME='glc_$run_num' QB_DIAG_FILE="$output_root_dir/glc_$run_num.diag" $qb $this_run_script &" >>$target_layout_script
        echo "#!/bin/bash" > $this_run_script
        chmod 755 $this_run_script
    fi

    echo "source \"$cell_script\"" >>$this_run_script

    cast2skill="$bin_dir/cast2skill"
    cast2cdl="$bin_dir/cast2cdl"
    
    check_executable_file "$cast2skill" \
        "cast2skill: \"$cast2skill_src\" is not an executable  file."  2
    check_executable_file "$cast2cdl" \
        "cast2cdl: \"$cast2cdl_src\" is not an executable file."  2

    ### prepare cell output dir
    if [[ ! -d "$cell_dir" ]] ; then
        mkdir -p $cell_dir
    fi
    
    ### the library to use is  the dfII unilib if given, else the parsed lib name
    
    if [ -n "$dfII_lib_name" ] ; then 
        lib=$dfII_lib_name
    else
        get_lib_name "$cell"
        lib="$ret"
    fi
    
    if [[ ( -z "$noprepare" ) || ! ( -e "$cdl_file" ) ]] ; then
        echo "Cast2Cdl..."
        $cast2cdl   --cell="$cell" --cast-path=$cast_path \
                    --output=$cdl_file --cadence-name --process-dependent-name
        echo "Cast2Skill..."
        $cast2skill --cell="$cell" --cast-path=$cast_path \
                    --output-dir=$cell_dir \
                    --root-only --cadence-name
    fi

    if [[ ( -e $cdl_file ) ]] ; then
        config_fill_template_from_env "$templates_dir/leaf_cell.il.template" \
        > $input
    else
        echo "Skipping $cell"
    fi
    
if [[ "$graphics" == "t" ]] ; then
    nograph=
else
    nograph=-nograph
fi

cat<<EOF>$cell_script
cd "$cell_wd" && layoutPlus $nograph -replay $input -log $cell_dir/CDS.log </dev/null &>run.err
EOF

done

echo 'wait' >> $target_layout_script
source $target_layout_script

