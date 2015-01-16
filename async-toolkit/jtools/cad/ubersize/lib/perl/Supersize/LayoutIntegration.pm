#
# LayoutIntegration
#
# Supersize module for integrating with Cadence floorplanning files
#
#

package Supersize::LayoutIntegration;

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT = qw(
        &run_updatenetlist
        &extract_dfII_instances
        &extract_dfII_instances_list
        &get_dfII_subcells
        &get_floorplan_area
        &to_cadence
        &from_cadence
        &copy_cadence_cell_view
        &mkcdslib
        &get_cadence_lib
        &get_cadence_lib_dir
        &get_cadence_cell_dir
        &get_cadence_cell_view_dir
    );
    our @EXPORT_OK = qw( 
        &get_module_data
    );
}

use strict;
use Supersize::Util;
use Supersize::TypeUtil;
use Supersize::JavaUtil;
use File::Find;

sub get_module_data {
  return { NAME => "LayoutIntegration",
           DESC => "Commands for generating and manipulating Cadence " .
                   "floorplan views.",
           COMMANDS => {
    "update_floorplan"    => {
        SUBREF => \&update_floorplan,
        USAGE  => "update_floorplan",
        DESC   => 
          "Formerly known as the confusingly named 'updatenetlist'. " .
          "However, there are some important differences between this " .
          "command and ubersize's updatenetlist:\n\n" .
          "  - This command will create DFII_DIR if it doesn't already\n" .
          "    exist.\n\n" .
          "  - This command gives you a brief summary of what updatenetlist\n" .
          "    did.  For example, it gives the number of cells that it\n" .
          "    couldn't modify due permissions problems, not including\n" .
          "    those that do not exist in the supersize subtype space\n" .
          "    (these are presumed to be reused fixed-size cells).",
        GLOBALS => { TOP          => { REQUIRED => 1 },
                     WORK_DIR     => { REQUIRED => 1 },
                     CAST_DIR     => { REQUIRED => 1 },
                     SPEC_DIR     => { REQUIRED => 1 },
                     IC           => { REQUIRED => 1 },
                     DFII_DIR     => { REQUIRED => 1 },
                     MEM          => { REQUIRED => 0 },
                     PDK_ROOT     => { REQUIRED => 1 },
                     PACKAGE_ROOT => { REQUIRED => 1 } },
        IV     => { 
          fast => {
            TYPE => $TYPE_SCALAR,
            DESC => "Use faster UpdateFloorplan.  Disables all other options.",
            REQUIRED => 0 },
          bound_scale_factor => {
            TYPE => $TYPE_SCALAR,
            DESC => "Scale bounding boxes by this factor.",
            REQUIRED => 0 },
          include_pins => {
            TYPE => $TYPE_SCALAR,
            DESC => "Include pins in floorplan views.",
            REQUIRED => 0 },
          lock_bound => {
            TYPE => $TYPE_SCALAR,
            DESC => "Lock bounding boxes in some manner.  Dunno, ask Chris.".
                    "The default, inherited from ubersize, is 0.",
            REQUIRED => 0 },
          lock_layout => {
            TYPE => $TYPE_SCALAR,
            DESC => "Don't modify the layout views presumably.  Default is 0.",
            REQUIRED => 0 },
          enable_profiler => {
            TYPE => $TYPE_SCALAR,
            DESC => "Turn on SKILL profiler for updatenetlist.  Default is 0.",
            REQUIRED => 0 },
          sloppy_update => {
            TYPE => $TYPE_SCALAR,
            DESC => "Ignore certain directives for faster processing. ".
                    "Default is 0.",
            REQUIRED => 0 },
          use_layout_height => {
            TYPE => $TYPE_SCALAR,
            DESC => "Set the floorplan view to have the layout view's height, ".
                    "regardless of the height directive (maybe).  Beware, if ".
                    "the layout view doesn't exist, the floorplan view is ".
                    "shriveled up into a miniscule box.  Default is 0.",
            REQUIRED => 0 },
          disable_license_queuing => {
            TYPE => $TYPE_SCALAR,
            DESC => "Set to 1 to immediately exit if a license cannot be ".
                    "acquired, instead of waiting indefinitely. Default is 0.",
            REQUIRED => 0 } },
        RV     => {
          attempted => {
            TYPE => $TYPE_LIST,
            DESC => "List of cells that updatenetlist attempted to modify ".
                    "(perhaps unsuccessfully -- see unwriteable)." },
          modified => {
            TYPE => $TYPE_LIST,
            DESC => "List of cells that updatenetlist actually modified." },
          below_cutoff => {
            TYPE => $TYPE_LIST,
            DESC => "List of leaf cells whose prBound widths were rounded " .
                    "up to the cutoff width." },
          unwriteable => {
            TYPE => $TYPE_LIST,
            DESC => "List of cells which were not modified due to unwriteable ".
                    "floorplan views." } } },
    "init_floorplan"    => {
        SUBREF => \&init_floorplan,
        USAGE  => "init_floorplan",
        DESC   => 
          "Calls initFloorplan from Virtuoso-Intergration package.",
        GLOBALS => { TOP          => { REQUIRED => 1 },
                     WORK_DIR     => { REQUIRED => 1 },
                     CAST_DIR     => { REQUIRED => 1 },
                     SPEC_DIR     => { REQUIRED => 1 },
                     IC           => { REQUIRED => 1 },
                     DFII_DIR     => { REQUIRED => 1 },
                     MEM          => { REQUIRED => 0 },
                     PDK_ROOT     => { REQUIRED => 1 },
                     PACKAGE_ROOT => { REQUIRED => 1 } },
        IV     => { 
          bound_scale_factor => {
            TYPE => $TYPE_SCALAR,
            DESC => "Scale bounding boxes by this factor.",
            REQUIRED => 0 },
          include_pins => {
            TYPE => $TYPE_SCALAR,
            DESC => "Include pins in floorplan views.",
            REQUIRED => 0 },
          lock_bound => {
            TYPE => $TYPE_SCALAR,
            DESC => "Lock bounding boxes in some manner.  Dunno, ask Chris.".
                    "The default, inherited from ubersize, is 0.",
            REQUIRED => 0 },
          lock_layout => {
            TYPE => $TYPE_SCALAR,
            DESC => "Don't modify the layout views presumably.  Default is 0.",
            REQUIRED => 0 },
          use_layout_height => {
            TYPE => $TYPE_SCALAR,
            DESC => "Set the floorplan view to have the layout view's height, ".
                    "regardless of the height directive (maybe).  Beware, if ".
                    "the layout view doesn't exist, the floorplan view is ".
                    "shriveled up into a miniscule box.  Default is 0.",
            REQUIRED => 0 } },
        RV     => {
          modified => {
            TYPE => $TYPE_LIST,
            DESC => "List of leaf cells that updatenetlist attempted to " .
                    "modify (perhaps unsuccessfully -- see uunwriteable)." },
          below_cutoff => {
            TYPE => $TYPE_LIST,
            DESC => "List of leaf cells whose prBound widths were rounded " .
                    "up to the cutoff width." },
          unwriteable => {
            TYPE => $TYPE_LIST,
            DESC => "List of cells which were not modified due to unwriteable ".
                    "floorplan views." } } },
    "copy_floorplan"    => {
        SUBREF => \&copy_floorplan,
        USAGE  => "copy_floorplan [--src-dir=" . underline("dfII-dir")."] " .
                                 "[--make-writeable] " .
                                 "[--skip-if-exists | --abort-if-exists] " .
                                 "[[--view=".underline("view-name")."] | " .
                                 "[--src-view=".underline("view-name")."] " .
                                 "[--dest-view=".underline("view-name")."]] " .
                                 "[[".underline("src_subtype").":]" .
                                 underline("dest_subtype")."] ... | " .
                                 "[" . underline("split_map") . "]",
        DESC   =>
          "With no arguments, does nothing.  If a source dfII directory is " .
          "specified which is different from DFII_DIR, then any floorplan " .
          "views in the source dfII hierarchy which are used in TOP will " .
          "be copied to DFII_DIR.\n\n" .
          "Copying can be restricted to a specific list of subtypes by " .
          "specifying them on the command line.  Copying from one subtype " .
          "to another can be specified using the src:dest form.  If ".
          underline("dest_subtype") . " is a number, the base cell " .
          "name is taken from " . underline("src_subtype") . ".\n\n" .
          "A specific set of subtypes to copy can also be specified using ".
          "a so-called " . underline("split_map") . ", a map variable whose ".
          "keys correspond to fully qualified subtypes in the source dfII ".
          "directory, with values corresponding to the destination " .
          "subtypes to copy each key subtype to.  The common usage of this ".
          "follows a split_subtypes command:\n\n" .
          "  split_subtypes\n" .
          "  copy_floorplan \$split_subtypes.split_map\n" .
          "  update_floorplan\n\n" .
          "This has the effect of splitting both subtypes and floorplan.\n\n" .
          "If unspecified, the view to be copied is the floorplan view.  " .
          "It can be overriden with --view.  If the source and the " .
          "destination views have different names, they can be specified " .
          "using --src-view and --dest-view.\n\n" .
          "By default, if a destination view to be copied already exists, ".
          "it will be overwritten.  If --skip-if-exists is specified, such ".
          "views will be skipped.  If --abort-if-exists is specified, the ".
          "command will abort if any such views are encountered.\n\n" .
          "Unless the --make-writeable option is specified, copied views ".
          "will be left with the same permissions as the source.  This ".
          "ensures that fixed-size views aren't modified by subsequent ".
          "commands.",
        GLOBALS => { DFII_DIR     => { REQUIRED => 1 },
                     WORK_DIR     => { REQUIRED => 1 },
                     PDK_ROOT     => { REQUIRED => 1 },
                     PACKAGE_ROOT => { REQUIRED => 1 },
                     JUST_PRINT   => { REQUIRED => 0 } } }, 
    "check_floorplan"    => {
        SUBREF => \&check_floorplan,
        USAGE  => "check_floorplan [--src-dir=" . underline("dfII-dir")."] " .
                                 "[--view=".underline("view-name")."] " .
                                 "[--exists | --not-exists | " .
                                 "--writable | --not-writable] " .
                                 "[" . underline("subtype_list")."]",
        DESC   =>
          "Filters the list of subtypes, returning only those whose ".
          "view (default floorplan) satisfies the condition (default exists).",
        GLOBALS => { DFII_DIR     => { REQUIRED => 1 },
                     WORK_DIR     => { REQUIRED => 1 },
                     PDK_ROOT     => { REQUIRED => 1 },
                     PACKAGE_ROOT => { REQUIRED => 1 } }, 
        RV     => {
          result => { 
            TYPE => $TYPE_LIST,
            DESC => "List of cells that satisfy the condition." }} },
    "reuse_floorplan"    => {
        SUBREF => \&reuse_floorplan,
        USAGE  => "reuse_floorplan [--src-dir=" . underline("dfII-dir")."] ".
                                  "[--subtype-range=" . underline("from") .
                                  "[:".underline("to")."]] " .
                                  "[".underline("subtype_list")."]",
        DESC   =>
          "Reuses any existing floorplanning from " . underline("dfII-dir") .
          " of cells instantiated under TOP which have not yet been " .
          "floorplanned in DFII_DIR.  If an explicit " .
          underline("subtype_list") . " is specified, only the floorplan ".
          "views of those subtypes will be reused.  If a subtype range is ".
          "specified, only source subtypes within the range will be ".
          "considered for reuse.",
        GLOBALS => { DFII_DIR     => { REQUIRED => 1 },
                     WORK_DIR     => { REQUIRED => 1 },
                     PDK_ROOT     => { REQUIRED => 1 },
                     PACKAGE_ROOT => { REQUIRED => 1 } } },
    "resolve_overlap"    => {
        SUBREF => \&resolve_overlap,
        USAGE  => "resolve_overlap [--src-dir=" . underline("dfII-dir")."] ".
                                  "[--subtype-range=" . underline("from") .
                                  "[:".underline("to")."]] " .
                                  "[".underline("subtype_list")."]",
        DESC   =>
          "Resolves overlap.",
        GLOBALS => { DFII_DIR     => { REQUIRED => 1 },
                     WORK_DIR     => { REQUIRED => 1 },
                     PDK_ROOT     => { REQUIRED => 1 },
                     PACKAGE_ROOT => { REQUIRED => 1 } } },
    "floorplan_area"    => {
        SUBREF => \&floorplan_area,
        USAGE  => "floorplan_area " . underline("cell") . " | " .
                                      underline("cell-list"),
        DESC   =>
          "Determines the floorplan areas of the specified cells.  For " .
          "a single cell, returns the area to standard output.  For a " .
          "list, returns the cell areas in the return 'result' map " .
          "variable.",
        GLOBALS => { DFII_DIR     => { REQUIRED => 1 },
                     WORK_DIR     => { REQUIRED => 1 },
                     PDK_ROOT     => { REQUIRED => 1 },
                     PACKAGE_ROOT => { REQUIRED => 1 } },
        RV     => {
          result => {
            TYPE => $TYPE_MAP,
            DESC => "Map of cell to floorplan area.  Defined when the " .
                    "command's argument is a cell list." } } },
   "setup_cdswd" => {
        SUBREF => \&setup_cdswd,
        USAGE  => "setup_cdswd [--user-template=" . underline("template-dir")."]",
        DESC   =>
            "Creates DFII_DIR (if it doesn't exist) and local cds_wd\n" .
            "with suitable configurations.  Can point to a user template\n" .
            "directory to copy display.drf, autoload.il and bindkeys.",
        GLOBALS => { DFII_DIR     => { REQUIRED => 1 },
                     WORK_DIR     => { REQUIRED => 1 },
                     PDK_ROOT     => { REQUIRED => 1 } } },
   "layout" => {
        SUBREF => \&layout,
        USAGE  => "layout [--user-template=" . underline("template-dir") . "]" .
            " [--init=" . underline("init.il") . "]",
        DESC   =>
            "Launch Virtuoso interactively from local cds_wd.  Calls\n" .
            "setup_cdswd first if cds_wd doesn't exist.  Optionally execute\n" .
            "init.il in Virtuoso.",
        GLOBALS => { DFII_DIR     => { REQUIRED => 1 },
                     WORK_DIR     => { REQUIRED => 1 },
                     PDK_ROOT     => { REQUIRED => 1 } } },
    },
    GLOBALS => {
        IC             => 
          { DESC => "Path to the ic wrapper script" },
        DFII_DIR       => 
          { DESC => "Cadence DFII (layout) directory" }
    }
  };
}

#
# update_floorplan
#
sub update_floorplan {
    # Get reference to global variables
    my ($SS_r) = @_;
    my $GS_r   = $SS_r->{GS};
    my $cds_wd = "$GS_r->{WORK_DIR}/cds_wd";
    my $fqcn   = $SS_r->{GS}{TOP};
    my $wdir   = get_work_dir($SS_r);

    # setup_cdswd
    setup_cdswd(@_) if (!-e $cds_wd);

    # Parse arguments
    shift;
    if (num_args(\@_)) {
        command_die($SS_r, "Too many arguments to update_floorplan.");
        return;
    }

    # Create DFII_DIR if it doesn't exist
    if (!-e $GS_r->{DFII_DIR}) {
        print "Creating $GS_r->{DFII_DIR}.\n";
        mkdir $GS_r->{DFII_DIR} || 
            command_die($SS_r, "Error creating $GS_r->{DFII_DIR}: $!");
    }

    # run faster UpdateFloorplan
    if ((get_cmd_input_variable($SS_r, "fast"))[1]) {
        open IL, ">$cds_wd/replay.il" or die;
        $fqcn = to_cadence($fqcn);
        print IL "(UpdateFloorplan ?cellName \"$fqcn\")\n(exit)\n";
        close IL;
        my $cmd = "cd $cds_wd; \\\n" .
            (path_to_tool($SS_r, "layoutPlus"))[0] . " -replay replay.il -nograph \\\n";
        supersize_system($SS_r, $cmd, $LOCAL_JOB);
        return;
    }

    # Set certain updatenetlist parameters appropriately
    my %p;
    $p{bound_scale_factor} = 
        (get_cmd_input_variable($SS_r, "bound_scale_factor"))[1];
    $p{include_pins}       = 
        (get_cmd_input_variable($SS_r, "include_pins"))[1];
    $p{lock_bound}         = 
        (get_cmd_input_variable($SS_r, "lock_bound"))[1];
    $p{lock_layout}        = 
        (get_cmd_input_variable($SS_r, "lock_layout"))[1];
    $p{enable_profiler}    = 
        (get_cmd_input_variable($SS_r, "enable_profiler"))[1];
    $p{sloppy_update}      = 
        (get_cmd_input_variable($SS_r, "sloppy_update"))[1];
    $p{use_layout_height}  = 
        (get_cmd_input_variable($SS_r, "use_layout_height"))[1];
    $p{disable_license_queuing}  = 
        (get_cmd_input_variable($SS_r, "disable_license_queuing"))[1];

    # Run updatenetlist
    print "Running updatenetlist.\n" if ($GS_r->{VERBOSE});
    run_updatenetlist($SS_r, $fqcn, \%p);

    # Look through updatenetlist.out and updatenetlist.log to see if 
    # there were problems or warnings worth noting.  Set return variables.
    post_updatenetlist($SS_r, get_work_dir($SS_r) . "/updatenetlist");
}

#
# init_floorplan
#
sub init_floorplan {
    # Get reference to global variables
    my $SS_r   = shift;
    my $fqcn   = shift;

    my $lib    = get_cadence_lib($fqcn);
    my $GS_r   = $SS_r->{GS};
    my $wdir      = get_work_dir($SS_r);
    my $cast_path = get_cast_path($SS_r);

    # Parse arguments
    if (num_args(\@_)) {
        command_die($SS_r, "Too many arguments to init_floorplan.");
        return;
    }

    # Command setup
    my $cmd = "$GS_r->{IC} " . 
              (path_to_tool($SS_r, "initFloorplan"))[0] . " \\\n";
    $cmd .= "--cell=\"$fqcn\" \\\n";
    $cmd .= "--lib=\"$lib\" \\\n";
    $cmd .= "--fulcrum-pdk-root=$GS_r->{PDK_ROOT} \\\n";
    $cmd .= "--dfII-dir=\"$GS_r->{DFII_DIR}\" \\\n";
    $cmd .= "--cast-path=\"$cast_path\" \\\n";
    $cmd .= "--working-dir=\"$wdir\" \\\n";
    $cmd .= "--cadence-log=\"$wdir/updatenetlist.log\" \\\n";
    $cmd .= "--view=floorplan \\\n";
    $cmd .= "--max-heap-size=$GS_r->{MEM} \\\n" if (defined $GS_r->{MEM});
    $cmd .= "--verbose \\\n";

    # Create DFII_DIR if it doesn't exist
    if (!-e $GS_r->{DFII_DIR}) {
        print "Creating $GS_r->{DFII_DIR}.\n";
        mkdir $GS_r->{DFII_DIR} || 
            command_die($SS_r, "Error creating $GS_r->{DFII_DIR}: $!");
    }

    # Run updatenetlist
    supersize_system($SS_r, $cmd);

    # Assume all was fine (could check if there were any errors)
    return 1;

}


#
# Common code for running updatenetlist
#
# Defined parameters:
#   bound_scale_factor  - Multiply width by this factor (undef => 1.0)
#   include_pins        - Include pins in views (undef => 0)
#   lock_bound          - Don't change bounding boxes[?] (undef => 0)
#   lock_layout         - Don't change layout views[?] (undef => 0)
#   use_layout_height   - Always use layout height (undef => 0)
#
# Required variables: IC, PDK_ROOT, PACKAGE_ROOT, MEM, DFII_DIR,
#                     CAST_DIR, SPEC_DIR, WORK_DIR, TOP
#
sub run_updatenetlist {
    my $SS_r   = shift;
    my $fqcn   = shift;
    my $p_r    = shift;

    my $GS_r      = $SS_r->{GS};
    my $wdir      = get_work_dir($SS_r);
    my $cast_path = get_cast_path($SS_r);

    # Determine fqcn and subtype
    my $cell = basetype_of($fqcn);
    my $subtype = subtype_number_of($fqcn);
    if (!defined $cell || !defined $subtype) {
        command_die($SS_r, "Invalid TOP cell name $fqcn.");
    }

    # Command setup
    my $cmd = "$GS_r->{IC} " . 
              (path_to_tool($SS_r, "updatenetlist"))[0] . " \\\n";
    $cmd .= "--cell=\"$cell\" \\\n";
    $cmd .= "--subtype=$subtype \\\n";
    $cmd .= "--fulcrum-pdk-root=$GS_r->{PDK_ROOT} \\\n";
    $cmd .= "--dfII-dir=\"$GS_r->{DFII_DIR}\" \\\n";
    $cmd .= "--cast-path=\"$cast_path\" \\\n";
    $cmd .= "--cadence-log=\"$wdir/updatenetlist.log\" \\\n";
    $cmd .= "--check-log=\"$wdir/updatenetlist.check\" \\\n";
    $cmd .= "--suppress-netlist-view \\\n";
    $cmd .= "--update-views \\\n";
    $cmd .= "--verbose \\\n";
    if (exists $p_r->{lock_bound} && $p_r->{lock_bound}) {
        $cmd .= "--lock-bound \\\n";
    }
    if (exists $p_r->{lock_layout} && $p_r->{lock_layout}) {
        $cmd .= "--lock-layout \\\n";
    }
    if (exists $p_r->{enable_profiler} && $p_r->{enable_profiler}) {
        $cmd .= "--profile-out=\"$wdir/updatenetlist.profile\" \\\n";
    }
    if (exists $p_r->{use_layout_height} && $p_r->{use_layout_height}) {
        $cmd .= "--use-layout-height \\\n";
    }
    if (exists $p_r->{disable_license_queuing} &&
        $p_r->{disable_license_queuing}) {
        $cmd .= "--disable-license-queuing \\\n";
    }
    if (exists $p_r->{bound_scale_factor} && 
        defined $p_r->{bound_scale_factor}) {
        $cmd .= "--bound-scale-factor=\"" . $p_r->{bound_scale_factor} .  
                                     "\" \\\n";
    }
    if (exists $p_r->{include_pins} && $p_r->{include_pins}) {
        $cmd .= "--force-pins \\\n";
    }
    else {
        $cmd .= "--suppress-pins \\\n";
    }
    if (exists $p_r->{sloppy_update} && $p_r->{sloppy_update}) {
        $cmd .= "--suppress-wiring-directives \\\n";
    }
    $cmd .= "--cast2skill-options='" . get_cast_defines($SS_r, 1) . "' \\\n";
    $cmd .= "--java-max-heap-size=$GS_r->{MEM} \\\n" if (defined $GS_r->{MEM});
    $cmd .= "> \"$wdir/updatenetlist.out\"";

    # unlink files produced by updatenetlist if they exist because error
    # checking looks at the existence of these files to infer if updatenetlist
    # succeeded
    foreach my $ext ('log', 'check', 'profile', 'out') {
        my $file = "$wdir/updatenetlist.$ext";
        unlink $file if -e $file;
    }

    # Run updatenetlist
    supersize_system($SS_r, $cmd, $MINOR_JOB);

    # Assume all was fine (could check if there were any errors)
    return 1;
}

#
# Post-process updatenetlist's droppings to check for conditions it
# really should tell us about.
#
sub post_updatenetlist {
    my $SS_r = shift;
    my $file = shift;
    my @fatal_errors = ();
    my @lock_errors = ();
    my @attempted_cells = ();
    my @cutoff_cells = ();
    my @unwriteable_cells = ();
    my @height_errors = ();
    my @modified_cells = ();

    my $wdir = get_work_dir($SS_r);

    # Look for errors in updatenetlist.out
    if (!open (OUT, "$file.out")) {
        print "Error reading $file.out.\n";
    }
    else {
        my @lines = <OUT>;
        close OUT;
        if (grep {/^Unable to generate skill from CAST/} @lines) {
            command_die($SS_r, 
                "Failed to parse $SS_r->{GS}{TOP}.  See $file.out.");
        }
    }

    # Read updatenetlist.log
    if (!-e "$file.log" || !open (LOG, "$file.log")) {
        command_die($SS_r, "Updatenetlist failed (no log file), nothing updated.");
    }

    my $license = '';
    while (<LOG>) {
        chomp;
        if (/\*WARNING\* Couldn't get a write lock for/) {
            # Sometimes the check file doesn't list cells that are 
            # unwriteable due to file locking.  Report the log file warning
            # messages only if the check file seems wrong.
            push @lock_errors, $_;
        }
        elsif (/Failed to get (.*) license/) {
            $license = $1;
        }
    }
    close LOG;
    if ($license) {
        command_die($SS_r,
            "Updatenetlist failed (unable to acquire $license license), ".
            "nothing updated. Try again later, or do not disable license ".
            "queuing (set update_floorplan.disable_license_queuing=0)");
    }

    # Interpret updatenetlist.check
    if (!-e "$file.check" || -z "$file.check" || !open (CHECK, "$file.check")) {
        command_die($SS_r, "Updatenetlist failed, nothing updated.  See $file.out.");
    }
    my $cutoff_area = 0.0;
    my @other_errors;
    my $cell;
    my $cell_height;
    my $unwriteable_fixed = 0;
    while (<CHECK>) {
        if (/^(CELL|CREATE):\s+([^\s]+)\s+([^\s]+)\s+floorplan$/) {
            $cell = from_cadence($3);
            push @attempted_cells, $cell;
        }
        elsif (/^BOUNDARY: HEIGHT will be (.*).$/) {
            $cell_height = $1;
        }
        elsif (/^BOUNDARY: WIDTH WARNING Estimated width ([^\s]+)/) {
            push @cutoff_cells, $cell;
            #my $ideal_width = $1;
            #if (/below cutoff of (.+)\.\.\./) {
            #    $cutoff_area += $cell_height * ($1 - $ideal_width);
            #    # TODO: Multiply by instance count if available
            #}
        }
        elsif (/^Error: (.*)$/) {
            print if ($SS_r->{GS}{DEBUG});
            if ($1 =~ /^Unable to get writeable view for (.*)\.$/) {
                my $cell = from_cadence((split(/\" \"/, $1))[1]);
                if (-e fqcn_to_file("$wdir/cast", $cell)) {
                    push @unwriteable_cells, $cell;
                }
                else {
                    $unwriteable_fixed++;
                }
            }
            else {
                chomp;
                push @other_errors, $1;
            }
        }
        elsif (/^ERROR: HEIGHT "([^"]+)" "([^"]+)" directive specifies \S+ non-empty cell has height of \S+/) {
            push @height_errors, from_cadence($1);
        }
        elsif (/^ERROR:/) {
            push @fatal_errors, $_;
        }
        elsif (/^SAVED:\s+([^\s]+)\s+floorplan$/) {
            push @modified_cells, from_cadence($1);
        }
    }
    close CHECK;

    if (@height_errors || @fatal_errors) {
        if (@fatal_errors) {
            print "The following fatal errors were detected:\n";
            print @fatal_errors;
            print "\n";
        }
        if (@height_errors) {
            print "The following cells have discrepancies between the height directive in the\n" .
                  "spec and the height of the layout view (usually fixed by annotating the layout\n" .
                  "height into the spec):\n";
            foreach my $err (@height_errors) {
                print "    $err\n";
            }
            print "\n";
        }
        command_die($SS_r, "Updatenetlist failed, nothing updated.  See $file.check.");
    }

    print "-"x78 . "\n";
    print "Cell count requiring update:   " . 
          sprintf("%-6s", scalar(@attempted_cells)) . 
          "(All attempted. See 'attempted'.)\n";
    print "Modified cell count:           " . 
          sprintf("%-6s", scalar(@modified_cells)) . 
          "(Actually modified. See 'modified'.)\n";
    print "Cell count below width cutoff: " . sprintf("%-6s",
          scalar(@cutoff_cells)) . "(See 'below_cutoff')\n";
    #if ($cutoff_area > 0.0) {
    #    print "Total area lost due to cutoff: " . 
    #          sprintf("%g", $cutoff_area) .  "\n";
    #}
    print "Unwriteable cell view count:   " . 
          sprintf("%-6s", scalar(@unwriteable_cells)) . 
          "(Non-reused subtypes. See 'unwriteable')\n";
    print "Unwriteable fixed view count:  " .
          sprintf("%-6s", $unwriteable_fixed) .
          "(Reused subtypes.)\n";
    print "Other errors:                  ";
    push @other_errors, @lock_errors if (!@unwriteable_cells);
    if (@other_errors) {
        print "\n";
        foreach my $error (@other_errors) {
            print "  $error\n";
        }
    }
    else {
        print "None\n";
    }
    print "-"x78 . "\n";

    set_cmd_return_variable($SS_r,"attempted", new_list(\@attempted_cells));
    set_cmd_return_variable($SS_r,"modified", new_list(\@modified_cells));
    set_cmd_return_variable($SS_r,"below_cutoff", new_list(\@cutoff_cells));
    set_cmd_return_variable($SS_r,"unwriteable", new_list(\@unwriteable_cells));
}


#
# copy_floorplan
#
sub copy_floorplan {
    my $SS_r = shift;

    # Parse command args to determine what we're doing
    my $if_exists = 0;      # ignore/overwrite
    my $preserve_perm = 1;
    my $view = "floorplan";
    my ($src_view, $dest_view);
    my $src_dfII_dir = $SS_r->{GS}{DFII_DIR};
    my @subtypes;
    my @cmd_split_maps;
    while (num_args(\@_)) {
        my @arg = shift_next_arg(\@_);
        if ($arg[0] == $TYPE_SCALAR && $arg[1] eq "--src-dir") {
            my $eq        = shift_next_scalar(\@_);
            $src_dfII_dir = shift_next_scalar(\@_);
            if (!defined $eq || $eq ne "=" || !defined $src_dfII_dir) {
                command_die($SS_r, "Bad --src-dir arguments.\n");
                return;
            }
        }
        elsif ($arg[0] == $TYPE_SCALAR && $arg[1] eq "--make-writeable") {
            $preserve_perm = 0;
        }
        elsif ($arg[0] == $TYPE_SCALAR && $arg[1] eq "--skip-if-exists") {
            $if_exists = 1;
        }
        elsif ($arg[0] == $TYPE_SCALAR && $arg[1] eq "--abort-if-exists") {
            $if_exists = 2;
        }
        elsif ($arg[0] == $TYPE_SCALAR && $arg[1] eq "--view") {
            my $eq  = shift_next_scalar(\@_);
            $view   = shift_next_scalar(\@_);
            if (!defined $eq || $eq ne "=" || !defined $view) {
                command_die($SS_r, "Bad --view arguments.\n");
                return;
            }
        }
        elsif ($arg[0] == $TYPE_SCALAR && $arg[1] eq "--src-view") {
            my $eq    = shift_next_scalar(\@_);
            $src_view = shift_next_scalar(\@_);
            if (!defined $eq || $eq ne "=" || !defined $src_view) {
                command_die($SS_r, "Bad --src-view arguments.\n");
                return;
            }
        }
        elsif ($arg[0] == $TYPE_SCALAR && $arg[1] eq "--dest-view") {
            my $eq     = shift_next_scalar(\@_);
            $dest_view = shift_next_scalar(\@_);
            if (!defined $eq || $eq ne "=" || !defined $dest_view) {
                command_die($SS_r, "Bad --dest-view arguments.\n");
                return;
            }
        }
        elsif ($arg[0] == $TYPE_SCALAR && $arg[1] =~ /^--/) {
            command_die($SS_r, "Bad argument: $arg[1].");
            return;
        }
        elsif ($arg[0] == $TYPE_SCALAR) {
            push @subtypes, $arg[1];
        }
        elsif ($arg[0] == $TYPE_LIST) {
            push @subtypes, @{$arg[1]};
        }
        elsif ($arg[0] == $TYPE_MAP) {
            push @cmd_split_maps, $arg[1];
        }
        else {
            command_die($SS_r, "Unexpected argument.");
            return;
        }
    }
    $dest_view = $view unless defined $dest_view;
    $src_view  = $view unless defined $src_view;

    if (!@subtypes && $src_dfII_dir ne $SS_r->{GS}{DFII_DIR}) {
        # Attempt to copy ALL subtypes instantiated under TOP
        if (query_cast_server($SS_r, "--task=subcells --cell=$SS_r->{GS}{TOP}",
                              \@subtypes, 1) < 0) {
            command_die($SS_r, "Error determining TOP subtypes.  " .
                "Run 'query subcells' to debug.");
        }
    }

    # Construct split_map from individual subtypes specified on cmd line
    my %split_map;
    foreach my $s (@subtypes) {
        if ($s =~ /^([^:]+)(:[^:]*)?$/) {
            my $src = $1;
            $split_map{$src} = [] if (!defined $split_map{$src});
            if (defined $2) {
                (my $dest = $2) =~ s/^://;
                if ($dest =~ /^\d.*$/) {
                    $dest = substr($src,0,rindex($src,".")) . ".$dest";
                }
                print "Copying to $dest\n";
                push @{$split_map{$src}}, $dest;
            }
            else {
                push @{$split_map{$src}}, $src;
            }
        }
        else {
            print "Warning: skipping unrecognized subtype $s.\n";
        }
    }
    # Add any specified split_map variables to the split map
    foreach my $sm (@cmd_split_maps) {
        foreach my $src (keys %{$sm}) {
            $split_map{$src} = [] if (!defined $split_map{$src});
            if ($sm->{$src}[0] == $TYPE_SCALAR) {
                push @{$split_map{$src}}, $sm->{$src}[1];
            }
            elsif ($sm->{$src}[0] == $TYPE_LIST) {
                push @{$split_map{$src}}, @{$sm->{$src}[1]};
            }
            else {
                command_die($SS_r, "Unrecognized map type specified.");
            }
        }
    }
        
    print "Attempting to copy " . scalar(keys %split_map) . " source views " .
          "to " .  scalar(values %split_map) . " destination views.\n";

    # Sanity check
    if ($src_dfII_dir eq $SS_r->{GS}{DFII_DIR} && !%split_map) {
        print "copy_floorplan: Nothing to do.\n";
        return;
    }
    if (! -e $SS_r->{GS}{DFII_DIR}) {
        print "Creating $SS_r->{GS}{DFII_DIR}.\n";
        `mkdir -p "$SS_r->{GS}{DFII_DIR}"`;
        #command_die($SS_r, "DFII_DIR $SS_r->{GS}{DFII_DIR} doesn't exist.");
    }

    # Copy floorplan views
    unlink "$SS_r->{GS}{WORK_DIR}/copy_floorplan.spec";
    copy_views($SS_r, \%split_map, $src_dfII_dir, $if_exists, $preserve_perm,
               $src_view, $dest_view);
}

#
# reuse_floorplan
#
sub reuse_floorplan {
    my $SS_r = shift;
    command_die($SS_r, "Sorry, reuse_floorplan isn't implemented yet.");
}

#
# resolve_overlap
#
sub resolve_overlap {
    my $SS_r = shift;
    command_die($SS_r, "Sorry, resolve_overlap isn't implemented yet.");
}

#
# floorplan_area
#
sub floorplan_area {
    my $SS_r = shift;

    # parse args
    my ($type, $val) = shift_next_arg(\@_);
    if (!defined $type || ($type != $TYPE_SCALAR && $type != $TYPE_LIST) ||
        num_args(\@_)) {
        command_die($SS_r, "Wrong arguments to floorplan_area.");
    }

    # determine cell list
    my $cell_list_r;
    my %areas;
    if ($type == $TYPE_SCALAR) {
        $cell_list_r = [ $val ];
    }
    else {
        $cell_list_r = $val;
    }
    
    # generate floorplan instances files
    extract_dfII_instances_list($SS_r, $cell_list_r, "floorplan", 
                                get_work_dir($SS_r));

    # Determine areas
    foreach my $cell (@{$cell_list_r}) {
        my $a = get_floorplan_area($SS_r, $cell);
        if (!defined $a) {
            print STDERR "Warning: Couldn't extract area for cell $cell.\n";
        }
        else {
            $areas{$cell} = [ $TYPE_SCALAR, $a ];
        }
    }
    
    if ($type == $TYPE_SCALAR && exists $areas{$val}) {
        print "Area = " . sprintf("%.3g", $areas{$val}->[1]) . "\n";
    }
    set_cmd_return_variable($SS_r, "result", [$TYPE_MAP, \%areas]);
}

#
# check_floorplan
#
sub check_floorplan {
    my $SS_r = shift;

    # Parse command args to determine what we're doing
    my $op = sub { -e $_[0] };
    my $view = "floorplan";
    my $src_dfII_dir = $SS_r->{GS}{DFII_DIR};
    my @subtypes;
    while (num_args(\@_)) {
        my @arg = shift_next_arg(\@_);
        if ($arg[0] == $TYPE_SCALAR && $arg[1] eq "--src-dir") {
            my $eq        = shift_next_scalar(\@_);
            $src_dfII_dir = shift_next_scalar(\@_);
            if (!defined $eq || $eq ne "=" || !defined $src_dfII_dir) {
                command_die($SS_r, "Bad --src-dir arguments.\n");
                return;
            }
        }
        elsif ($arg[0] == $TYPE_SCALAR && $arg[1] eq "--exists") {
            $op = sub { -e $_[0] };
        }
        elsif ($arg[0] == $TYPE_SCALAR && $arg[1] eq "--not-exists") {
            $op = sub { not -e $_[0] };
        }
        elsif ($arg[0] == $TYPE_SCALAR && $arg[1] eq "--writable") {
            $op = sub { -w $_[0] };
        }
        elsif ($arg[0] == $TYPE_SCALAR && $arg[1] eq "--not-writable") {
            $op = sub { not -w $_[0] };
        }
        elsif ($arg[0] == $TYPE_SCALAR && $arg[1] eq "--view") {
            my $eq  = shift_next_scalar(\@_);
            $view   = shift_next_scalar(\@_);
            if (!defined $eq || $eq ne "=" || !defined $view) {
                command_die($SS_r, "Bad --view arguments.\n");
                return;
            }
        }
        elsif ($arg[0] == $TYPE_SCALAR && $arg[1] =~ /^--/) {
            command_die($SS_r, "Bad argument: $arg[1].");
            return;
        }
        elsif ($arg[0] == $TYPE_SCALAR) {
            push @subtypes, $arg[1];
        }
        elsif ($arg[0] == $TYPE_LIST) {
            push @subtypes, @{$arg[1]};
        }
        else {
            command_die($SS_r, "Unexpected argument.");
            return;
        }
    }

    my $subtype;
    my @result;
    foreach $subtype (@subtypes) {
        my $file = get_cadence_cell_view_dir($src_dfII_dir,
                                             to_cadence($subtype),
                                             $view) . "/layout.cdb";
        if (&$op($file)) {
            print $subtype, "\n";
            push @result, $subtype;
        }
    }

    set_cmd_return_variable($SS_r, "result", [ $TYPE_LIST, \@result ]);
}


############################## Utility Functions ##############################

# Invoke mk_instance to extract instance information of a specified
# DFII cell view.
sub extract_dfII_instances {
    my $SS_r = shift;
    my $fqcn = shift;
    my $view = shift;
    my $wdir = shift;

    my $cmd = "$SS_r->{GS}{IC} " . 
              (path_to_tool($SS_r, "runincdswd"))[0] . " \\\n";
    $cmd .= "\"--dfII-dir=$SS_r->{GS}{DFII_DIR}\" \\\n";
    $cmd .= "\"--fulcrum-pdk-root=$SS_r->{GS}{PDK_ROOT}\" \\\n";
    $cmd .= (path_to_tool($SS_r, "mk_instance"))[0] . " \\\n";
    $cmd .= "\"--lib=" . get_cadence_lib(to_cadence($fqcn)) . "\" \\\n";
    $cmd .= "\"--cell=$fqcn\" \\\n";
    $cmd .= "\"--outdir=$wdir/instances\" \\\n";
    $cmd .= "\"--view=$view\" \\\n";
    $cmd .= "\"--fulcrum-pdk-root=$SS_r->{GS}{PDK_ROOT}\" ";
    supersize_system($SS_r, $cmd, $MINOR_JOB, {}, "$wdir/mk_instance.out");
}

# Invoke mk_instance_multi to extract instance information of a 
# list of DFII cell views.
sub extract_dfII_instances_list {
    my $SS_r     = shift;
    my $cells_lr = shift;
    my $view     = shift;
    my $wdir     = shift;

    my $cmd = "$SS_r->{GS}{IC} " . 
              (path_to_tool($SS_r, "runincdswd"))[0] . " \\\n";
    $cmd .= "\"--dfII-dir=$SS_r->{GS}{DFII_DIR}\" \\\n";
    $cmd .= "\"--fulcrum-pdk-root=$SS_r->{GS}{PDK_ROOT}\" \\\n";
    $cmd .= (path_to_tool($SS_r, "mk_instance_multi"))[0] . " \\\n";
    $cmd .= "\"--cadence-log=$wdir/mk_instance_multi.log\" \\\n";
    $cmd .= "\"--outdir=$wdir/instances\" \\\n";
    $cmd .= "\"--fulcrum-pdk-root=$SS_r->{GS}{PDK_ROOT}\" ";
    my ($mk_in, $cmd_pid, $pid) =
        supersize_system($SS_r, $cmd, $MINOR_JOB, {},
                         "$wdir/mk_instance_multi.out", 1);
    foreach my $c (@{$cells_lr}) {
        print $mk_in $c . " $view " . get_cadence_lib(to_cadence($c)) . "\n";
    }
    close $mk_in;
    waitpid ($cmd_pid, 0);
    waitpid ($pid, 0);
}

#
# Determines dfII subcell instances of a particular cell by looking at the
# dfII view's pc.db file.  (If use_mk_instance is defined and set to 1, it
# will instead generate and look at .instance files.) Returns a reference 
# to a map of inst_name -> subtype.
#
sub get_dfII_subcells {
    my $SS_r            = shift;
    my $fqcn            = shift;
    my $view            = shift;
    my $use_mk_instance = shift;
    my $insts_r;

    my $wdir = get_work_dir($SS_r);

    if (!defined $use_mk_instance || $use_mk_instance==0) {
        # Find cell's pc.db file
        my $file = get_cadence_cell_view_dir($SS_r->{GS}{DFII_DIR}, 
                                        to_cadence($fqcn), $view) . "/pc.db";
        return undef if (!-e $file);

        # Parse pc.db file
        $insts_r = {};
        open (PCDB, $file) || command_die($SS_r, "Couldn't read $file.");
        my $state = 0;
        my $line = 1;
        my $type;
        while (<PCDB>) {
            chomp;
            if (/^\#(.*)$/) {
                if ($1 eq "ISCELL") {
                    # These are layout objects of some kind
                    $state = 2;
                }
                else {
                    $state = 0;
                }
            }
            elsif ($state==0) {
                my ($l,$t,$v) = split /\s+/, $_;
                if (!defined $v) {
                    command_die("Unexpected syntax on line $line of $file.");
                }
                if ($t =~ /^gate\./ || $t =~ /^stack\./ ||
                    $t =~ /^nmos/ || $t =~ /^pmos/) {
                    # ignore gates, stacks, transistors, etc.
                    $state = 2;
                }
                else {
                    # Note: no checking of view
                    $type = $t;
                    $state = 1;
                }
            }
            elsif ($state==1) {
                (my $inst = $_) =~ s/\]\[/,/g;
                $insts_r->{$inst} = from_cadence($type);
            }
            $line++;
        }
        close PCDB;
    }
    else {
        # Generate and read .instance file
        my $dir = "$wdir/instances";
        mkdir $dir if (!-e $dir);
        my $file = "$dir/" . to_cadence($fqcn) . ".instances";
        if (!-e $file) {
            extract_dfII_instances($SS_r, $fqcn, "layout", $wdir);
        }
        return undef if (!-e $file);

        # Read instance file, determine subcell instances
        $insts_r = {};
        open (INST, $file) || command_die($SS_r, "Couldn't read $file.");
        my $state = -2;
        my $level = 0;
        my $line = 1;
        my ($type,$inst);
        while (<INST>) {
            my $new_level = $level;
            if (/^beginlist$/) {
                $new_level++;
            }
            elsif (/^endlist$/) {
                $new_level--;
            }
            if ($state==-2 && $new_level==2 && $level==1) {
                # First coordinate point
                $state = -1;
            }
            elsif ($state==-1 && $new_level==2 && $level==1) {
                # Second coordinate point
                $state = 0;
            }
            elsif ($state==0 && $new_level==2 && $level==1) {
                # Subcell definition
                $state = 1;
            }
            elsif ($state==1 && /^\"([^\"]+)\"$/) {
                $type = $1;
                $state = 2;
            }
            elsif ($state==2 && /^\"([^\"]+)\"$/) {
                $inst = $1;
                $state = 3;
            }
            elsif ($state==1 || $state==2) {
                command_die($SS_r, "Unexpected syntax in $file, line $line.");
            }
            elsif ($state==3 && $new_level==1 && $level==2) {
                $inst =~ s/\]\[/,/g;
                $insts_r->{$inst} = from_cadence($type);
                $state = 0;
            }
            $level = $new_level;
            $line++;
        }
        close INST;
    }
    return $insts_r;
}


#
# Retrieve floorplan area of a cell, in um^2.
#
sub get_floorplan_area {
    my $SS_r          = shift;
    my $subtype       = shift;
    my $instances_dir = shift;      # optional

    my $file = (defined $instances_dir ? $instances_dir : 
        get_work_dir($SS_r) . "/instances") . "/" . to_cadence($subtype) .
               ".instances";
    return undef if (!-e $file);
    open (INST, "$file") || command_die($SS_r, "Couldn't read $file.");
    my @coords;
    my $i = 0;
    while (<INST>) {
        if (/^([\d+\.e-]+)$/) {
            push @coords, $1; $i++; } last if ($i==4);
    }
    close INST;
    command_die($SS_r, "Bad syntax in $file.") if (@coords < 4) ;
    return (($coords[2]-$coords[0]) * ($coords[3]-$coords[1])) * 1e12;
}

#
# Copies/splits floorplan views from src_dfII_dir to DFII_DIR as 
# specified by a map of 
#   src_subtype -> [ dest_subtype1, dest_subtype2, .. ].
#
sub copy_views {
    my $SS_r          = shift;
    my $split_hash_r  = shift;
    my $src_dfII_dir  = shift;
    my $if_exists     = shift;    # 0:overwrite 1:skip 2:abort
    my $preserve_perm = shift;
    my $src_view      = shift;    # defaults to floorplan
    my $dest_view     = shift;    # defaults to floorplan

    $src_dfII_dir = $SS_r->{GS}{DFII_DIR} if (!defined $src_dfII_dir);

    # Add hash-specified cells to the split spec file
    if (%{$split_hash_r}) {
        foreach my $src_subtype (sort keys %{$split_hash_r}) {
            my $basetype = basetype_of($src_subtype);
            my $subtype  = subtype_number_of($src_subtype);
            foreach my $dest_subtype (@{$split_hash_r->{$src_subtype}}) {
                my $action = copy_cadence_cell_view($SS_r, $src_dfII_dir, 
                    $src_subtype, $src_view, $SS_r->{GS}{DFII_DIR}, 
                    $dest_subtype, $dest_view, 
                    ($if_exists==1 || $if_exists==2), $preserve_perm);
                if ($SS_r->{GS}{VERBOSE}) {
                    if ($action == -1) {
                        print "Unable to copy $dest_subtype: source subtype " .
                              "doesn't exist.\n";
                    }
                    elsif ($action == 0 && $if_exists==2) {
                        print "Aborted copy: $dest_subtype already exists.\n";
                        return;
                    }
                    elsif ($action == 0) {
                        print "Skipped copy of $dest_subtype.\n";
                    }
                    else {
                        print "Copied $dest_subtype.\n";
                    }
                }
            }
        }
    }
}

# Returns 
#   -1 : source view doesn't exist
#    0 : aborted due to skip_if_exists==1 and destination view exists,
#        or if source & destination views are identical.
#    1 : copies successfully
sub copy_cadence_cell_view {
    my $SS_r          = shift;
    my $src_root_dir  = shift;  # source dfII directory
    my $src_fqcn      = shift;  # source fqcn (cast name)
    my $src_view      = shift;  # source view name (e.g. "floorplan")
    my $dest_root_dir = shift;  # destination dfII dir
    my $dest_fqcn     = shift;  # destination fqcn (cast name)
    my $dest_view     = shift;  # destination view name
    my $skip_if_exists = shift; # 0:overwrite 1:skip
    my $preserve_perm = shift;  # preserve source permissions

    # Check for identical src/dest case
    if ($src_fqcn eq $dest_fqcn && $src_root_dir eq $dest_root_dir &&
        $src_view eq $dest_view) {
        return 0;
    }

    # Convert to cadence cell names
    $src_fqcn  = to_cadence($src_fqcn);
    $dest_fqcn = to_cadence($dest_fqcn);

    # Make sure the source view exists
    my $src_cell_view_dir = 
        get_cadence_cell_view_dir($src_root_dir, $src_fqcn, $src_view);
    return -1 if (!-e $src_cell_view_dir);

    # Directory/library setup
    my $blank_cds_library =
        "$SS_r->{GS}{PDK_ROOT}/share/Fulcrum/blank-library";
    if (!is_valid_lib_dir($blank_cds_library,
                          get_cadence_lib_dir($dest_root_dir, $dest_fqcn))) {
        mkcdslib($dest_root_dir, $blank_cds_library,
                 get_cadence_lib($dest_fqcn));
    }
    my $dest_cell_view_dir = 
        get_cadence_cell_view_dir($dest_root_dir, $dest_fqcn, $dest_view);
    if (-e $dest_cell_view_dir) {
        return 0 if ($skip_if_exists==1);
        # overwrite
        `rm -rf "$dest_cell_view_dir"`;
    }
    `mkdir -p "$dest_cell_view_dir"` if (!-d $dest_cell_view_dir);

    # Copy view files
    `cp -a "$src_cell_view_dir"/* "$dest_cell_view_dir"`;
    `chmod -R u+w "$dest_cell_view_dir"` unless ($preserve_perm);

    # delete lock file if applicable
    unlink "$dest_cell_view_dir/layout.cdb.cdslck"
        if (-e "$dest_cell_view_dir/layout.cdb.cdslck");

    return 2;
}

# Translates a Cast cell name to a Cadence (layout) name
sub to_cadence {
    my $fqcn = shift;
    $fqcn =~ s/[\(\{]/-L/g;
    $fqcn =~ s/[\)\}]/-R/g;
    $fqcn =~ s/,/_/g;
    return $fqcn;
}

# Translates a Cast cell name back from a Cadence (layout) name
sub from_cadence {
    my $cfqcn = shift;
    my $idx_l = index $cfqcn, "-L";
    my $idx_r = rindex $cfqcn, "-R";
    return $cfqcn if ($idx_l == -1);
    my $fqcn = substr $cfqcn, 0, $idx_l;
    my $meta = substr $cfqcn, $idx_l+2, $idx_r-$idx_l-2;
    $meta =~ s/-L/\{/g; $meta =~ s/-R/\}/g; $meta =~ s/_/,/g;
    return substr($cfqcn,0,$idx_l) . "(" . $meta . ")" . 
           substr($cfqcn, $idx_r+2);
}

my %blank_library_files = ();
sub get_blank_library_files {
    my $blank_cds_library = shift;
    if (!exists($blank_library_files{$blank_cds_library})) {
        my @files = ();
        find({ no_chdir => 1,
               wanted => sub { my $x = $_;
                               $x =~ s/^\Q$blank_cds_library\E\/?//;
                               push @files, $x; }
             }, $blank_cds_library);
        $blank_library_files{$blank_cds_library} = \@files;
    }
    return $blank_library_files{$blank_cds_library};
}

sub is_valid_lib_dir {
    my $blank_cds_library = shift;
    my $lib_dir = shift;
    my $files = get_blank_library_files($blank_cds_library);
    foreach my $file (@{$files}) {
        return 0 unless -e "$lib_dir/$file";
    }
    return 1;
}

# Reimplementation of mkcdslib script (114 lines shorter)
sub mkcdslib {
    my $generated_libs_root = shift;
    my $blank_cds_library   = shift;    # why this?
    my $lib                 = shift;

    my $cadence_lib_dir = escape_cadence_dir($lib);
    (my $lib_dir = $lib) =~ s/\./\//g;

    # Check if the specified library exists already, as far as
    # cds.lib.generated is concerned
    if (open(CDSLG, "$generated_libs_root/cds.lib.generated")) {
        my $lib_exists = 0;
        while (<CDSLG>) {
            next if (/^--/);
            if (/^DEFINE\s+([^\s]+)\s+/ && $1 eq $cadence_lib_dir) {
                $lib_exists = 1;
                last;
            }
        }
        close CDSLG;
        return if ($lib_exists);
    }

    # Create library directory, copy necessary files from template
    if (!is_valid_lib_dir($blank_cds_library, "$generated_libs_root/$lib_dir"))
    {
        `mkdir -p "$generated_libs_root/$lib_dir"`;
        `(cd "$blank_cds_library" && tar cf - .) | (cd "$generated_libs_root/$lib_dir" && tar xpf -)`;
    }
    # TODO: Create individual directories, copy individual files if the
    #       directory already exists.  (Really necessary?)

    # Add library entry to cds.lib.generated
    if (!open(CDSLG, ">>$generated_libs_root/cds.lib.generated")) {
        print STDERR "Couldn't write to " .
                     "$generated_libs_root/cds.lib.generated\n";
        return;
    }
    print CDSLG "DEFINE $cadence_lib_dir $lib_dir\n";
    close CDSLG;
}


# dir should be derived from a layout cell name
sub escape_cadence_dir {
    my $dir = shift;
    $dir =~ s/\./#2e/g;
    $dir =~ s/-/#2d/g;
    $dir =~ s/\\$/#24/g;
    return $dir;
}

# Removes last two .foo components of fqcn (layout form)
# e.g. lib.buffer.half.BUF_1of4.400 -> lib.buffer.half
sub get_cadence_lib {
    my $fqcn = shift;
    (my $lib = $fqcn) =~ s/\.[^\.]+$//;
    $lib =~ s/\.[^\.]+$//;
    return $lib;
}

# fqcn should be layout form
sub get_cadence_lib_dir {
    my $root_dir = shift;
    my $fqcn     = shift;
    my $lib_dir  = get_cadence_lib($fqcn);
    $lib_dir =~ s/\./\//g;
    return $root_dir . "/" . $lib_dir;
}

# fqcn should be layout form
sub get_cadence_cell_dir {
    my $root_dir = shift;
    my $fqcn     = shift;
    my $lib_dir  = get_cadence_lib_dir($root_dir, $fqcn);
    return $lib_dir . "/" . escape_cadence_dir($fqcn);
}

# fqcn should be layout form
sub get_cadence_cell_view_dir {
    my $root_dir = shift;
    my $fqcn     = shift;
    my $view     = shift;
    my $lib_dir  = get_cadence_lib($fqcn);
    $lib_dir =~ s/\./\//g;
    return $root_dir . "/" . $lib_dir .  "/" .  escape_cadence_dir($fqcn) .
           "/" . escape_cadence_dir($view);
}

# Set up DFII_DIR and cds_wd directories
sub setup_cdswd {
    # Get reference to global variables
    my $SS_r   = shift;
    my $GS_r   = $SS_r->{GS};

    # create DFII_DIR
    if (!-e $GS_r->{DFII_DIR}) {
        print "Creating Cadence floorplan directory $GS_r->{DFII_DIR}.\n";
        mkdir $GS_r->{DFII_DIR};
    }

    # create cds_wd
    print "Creating layoutPlus working directory $GS_r->{WORK_DIR}/cds_wd.\n";
    mkdir "$GS_r->{WORK_DIR}/cds_wd";
    
    # parse args
    my $user_template = "$GS_r->{PDK_ROOT}/share/Fulcrum/supersize/cds_wd";
    while (num_args(\@_)) {
        my @arg = shift_next_arg(\@_);
        if ($arg[0] == $TYPE_SCALAR && $arg[1] eq "--user-template") {
            my $eq         = shift_next_scalar(\@_);
            $user_template = shift_next_scalar(\@_);
            if (!defined $eq || $eq ne "=" || !defined $user_template) {
                command_die($SS_r, "Bad --user_template arguments.\n");
                return;
            }
        }
    }

    # mkcdswd
    my $cmd = (path_to_tool($SS_r, "mkcdswd"))[0] . " \\\n";
    $cmd .= "'--cast-path=" . get_cast_path($SS_r) . "' \\\n";
    $cmd .= "'--target-dir=$GS_r->{WORK_DIR}/cds_wd' \\\n";
    $cmd .= "'--dfII-dir=$GS_r->{DFII_DIR}' \\\n";
    $cmd .= "--force \\\n";
    $cmd .= "--p4-client=$GS_r->{P4_DFII_CLIENT} \\\n";
    $cmd .= "--user-template=$user_template \\\n";
    supersize_system($SS_r, $cmd, $LOCAL_JOB, {}, "/dev/null");
}

# launch Virtuoso interactively
sub layout {
    # Get reference to global variables
    my ($SS_r) = @_;
    my $GS_r = $SS_r->{GS};
    my $top = to_cadence($SS_r->{GS}{TOP});
    my $cds_wd = "$GS_r->{WORK_DIR}/cds_wd";

    # setup_cdswd
    setup_cdswd(@_) if (!-e "$GS_r->{WORK_DIR}/cds_wd");

    # parse extra arguments
    shift;
    my $init;
    while (num_args(\@_)) {
        my ($type, $arg) = shift_next_arg(\@_);
        if ($type == $TYPE_SCALAR) {
            if ($arg =~ /^--init/ ) {
                shift_next_scalar(\@_);
                $init = shift_next_scalar(\@_);
            }
        }
    }

    # run Virtuoso
    open IL, ">$cds_wd/replay.il" or die;
    print IL "TOP = \"$top\"\n";
    if (defined($init)) {
        print IL "load \"$init\"\n";
    } else {
        print IL "Open TOP \"floorplan\"\n";
    }
    close IL;
    my $cmd = "cd $cds_wd; \\\n";
    $cmd .= (path_to_tool($SS_r, "layoutPlus"))[0] . " -replay replay.il \\\n";
    supersize_system($SS_r, $cmd, $LOCAL_JOB, {}, "/dev/stdout");
}

1;
