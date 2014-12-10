#
# MergeLibrary
#
# Merging library characterization
#
#

package Supersize::MergeLibrary;

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT = qw(
        &read_characterization_data
    );
    our @EXPORT_OK = qw( 
        &get_module_data
    );
}

use strict;
use POSIX;
use Supersize::ModifySubtypes;
use Supersize::Netlist;
use Supersize::Sizing;
use Supersize::LayoutIntegration;
use Supersize::Util;
use Supersize::TypeUtil;
use Supersize::JavaUtil;
use Term::ANSIColor;
use Text::Wrap;

sub get_module_data {
  return { 
    NAME => "MergeLibrary",
    DESC => "Commands for merge library characterization",
    COMMANDS => {
    "characterize" => {
        SUBREF => \&characterize,
        USAGE  => "characterize [--library[=".underline("lib-cell-name")."]] ".
                               "[--update-floorplan] ".
                               "[--skip-sizing] " .
                               "[" . underline("leaf-cell-list") ."]",
        DESC   =>
          "Characterizes a set of leaf cells in preparation for port-based ".
          "merging.  With no options, characterizes all of the sizable " .
          "leaf cells instantiated under TOP.  If the --library option ".
          "is specified, all fixed-size leaf cells will be characterized. ".
          "Usually, a specific list of cells under SPEC_DIR will be " .
          "provided in the latter case.  The default library cell name " .
          "is basetype(TOP).LIB.subtype(TOP).  You'll probably want to " .
          "explicitly set it to something else with the optional argument ".
          "to --library (should be a fully-qualified valid subtype name).\n\n".
          "The final output of the characterization procedure is a ".
          "WORK_DIR/".underline("lib-cell-name").".sslib file which contains ".
          "all the necessary data for port-based merging.  Several other ".
          "intermediate files are also produced (such as a subtype wrapper ".
          "cast file under WORK_DIR/cast) which may be safely deleted.  In ".
          "particular, a sizing run is performed so you will find all the " .
          "usual sizing debug files polluting WORK_DIR.\n\n".
          "By default, the bounding box sizes of the leaf cells will be ".
          "taken directly from the floorplan views.  However, if the layout ".
          "views are available, the --update-floorplan option can be given, ".
          "which will cause all floorplan views to first be updated based on ".
          "the layout bounding boxes.\n\n" .
          "If for some reason you only want to regenerate the .sslib file ".
          "from a prior library sizing run, --skip-sizing will eliminate most ".
          "of the command's run-time.",
        IV => {
          output_wire_length => {
            DESC => "Specifies the wirelength (in M) to put on all output ".
                    "ports, with default width/spacing. Default is 0.  If ".
                    "greater than 0, non-monotonic comparisons can occur." } },
        GLOBALS => {
          WORK_DIR => { REQUIRED => 1 },
          CAST_DIR => { REQUIRED => 1 },
          SPEC_DIR => { REQUIRED => 1 },
          TAU      => { REQUIRED => 1 },
          IC       => { REQUIRED => 1 },
          PDK_ROOT => { REQUIRED => 1 },
          MEM      => { REQUIRED => 1 },
          DFII_DIR => { REQUIRED => 1 } } },
    },
    GLOBALS => {}
  };
}

#
# characterize
#
sub characterize {
    my $SS_r        = shift;
    my $is_library  = 0;
    my $update_floorplan = 0;
    my $skip_sizing = 0;
    my $cell_name;
    my @cell_list;

    my $wdir = get_work_dir($SS_r);

    # parse args
    while (num_args(\@_)) {
        my ($type, $val) = shift_next_arg(\@_);
        if ($type == $TYPE_SCALAR) {
            if ($val eq "--library") {
                $is_library = 1;
                my $eq = next_scalar(\@_);
                if (defined $eq && $eq eq "=") {
                    shift_next_scalar(\@_);
                    $cell_name = shift_next_scalar(\@_);
                    if (!defined $cell_name) {
                        command_die($SS_r, "Bad argument to $val.");
                    }
                }
            }
            elsif ($val eq "--update-floorplan") {
                $update_floorplan = 1;
            }
            elsif ($val eq "--skip-sizing") {
                $skip_sizing = 1;
            }
            elsif ($val =~ /^--/) {
                command_die($SS_r, "Uncrecognized option $val.");
            }
            else {
                command_die($SS_r, "Too many arguments to characterize.");
            }
        }
        elsif ($type == $TYPE_LIST) {
            if (!@cell_list) {
                @cell_list = sort @{$val};
            }
            else {
                command_die($SS_r, "Too many arguments to characterize.");
            }
        }
        else {
            command_die($SS_r, "Bad argument to characterize.");
        }
    }
    if ((!@cell_list || !defined($cell_name)) && !defined($SS_r->{GS}{TOP})) {
        command_die($SS_r, "TOP must be set unless --library is specified ".
                           "along with a cell list");
    }
    if (!@cell_list) {
        my $query_cmd = "--cell=$SS_r->{GS}{TOP} --task=subcells --filter=leaf";
        if (!$is_library) {
            print "Characterizing all sizable leaf cells.\n";
            $query_cmd .= "&!fixed";
        }
        else {
            print "Characterizing all fixed-size leaf cells.\n";
            $query_cmd .= "&fixed";
        }
        my @lines;
        query_cast_server($SS_r, $query_cmd, \@lines, 1);
        foreach my $l (@lines) {
            $l =~ s/\s*$//; chomp $l;
            push @cell_list, $l;
        }
    } else {
        my @unparseable = ();
        my @midlevel = ();
        my @leaf = ();
        foreach my $cell (@cell_list) {
            my @lines = ();
            my $query_cmd = "--cell=$cell --task=subcells --filter=!leaf";
            my $status = query_cast_server($SS_r, $query_cmd, \@lines, 1,
                                           undef, 1,
                                           ($SS_r->{GS}{VERBOSE} ? -1 : 0));
            if ($status == 0) {
                if (@lines) {
                    push @midlevel, $cell;
                } else {
                    push @leaf, $cell;
                }
            } else {
                push @unparseable, $cell;
            }
        }
        if (@unparseable) {
            print "Skipping unparseable cells:\n   " .
                  join("\n   ", sort @unparseable) .
                  "\n";
        }
        if (@midlevel) {
            print "Skipping midlevel cells:\n   " .
                  join("\n   ", sort @midlevel) .
                  "\n";
        }
        if (@leaf) {
            @cell_list = @leaf;
        } else {
            command_die($SS_r, "No leaf cells to characterize.");
        }
    }

    if (!defined $cell_name) {
        my $suffix = $is_library ? "LIB" : "LEAF";
        $cell_name = basetype_of($SS_r->{GS}{TOP}) . ".$suffix." . 
                     subtype_number_of($SS_r->{GS}{TOP});
    }
    my $output_wire_length = get_cmd_input_scalar($SS_r,
                                "output_wire_length");
    $output_wire_length = 0 if (!defined $output_wire_length);

    # Generate the sizing wrapper so we can size all leaf cells
    # in one go independently
    print "Creating subtype wrapper for $cell_name.\n";
    create_sizing_wrapper($SS_r, $cell_name, $is_library, \@cell_list);

    if ($update_floorplan) {
        # Run updatenetlist to create dfII wrapper (note this will usually
        # be putting the file under layout/tsmc13/dfII for library 
        # characterization)
        print "Updating floorplan views of all leaf cells.\n";
        run_updatenetlist($SS_r, $cell_name);
    }

    if (!$skip_sizing) {
        # Invoke mk_instane_multi to extract floorplan areas of all
        # leaf cells.
        print "Extracting floorplan views of all leaf cells.\n";
        extract_dfII_instances_list($SS_r, \@cell_list, "floorplan", $wdir);

        # Now size all cells at once
        print "Calling Jauto to evaluate all sizing paths.\n";
        my $err = size_common($SS_r, $wdir, $cell_name,
            { use_floorplan             => 1,
              all_fixed_size            => 1,
              use_existing_instances    => 1,
              dont_clear_density_factor => 1,
              extra_jauto_args          =>
                ["--enableCatPath=0", "--defaultWireLength=$output_wire_length"]
            });
        
        command_die($SS_r, "Error: Jauto exited abnormally.") if ($err);
    }

    # Determine characterization data from jauto.as.info
    print "Processing output files.\n";
    my $cdata_r = characterize_cells($SS_r, $wdir, $cell_name, \@cell_list);

    # Write characterization data to .sslib file
    print "Writing $cell_name.sslib.\n";
    write_characterization_data($SS_r, "$wdir/$cell_name.sslib", $cdata_r);

    # Debugging/summary output
    my $worst_tau   = 0.0;  my $worst_tau_cell   = "";
    my $best_tau    = 1e9;  my $best_tau_cell    = "";
    my $worst_load  = 0.0;  my $worst_load_cell  = "";
    my $best_load   = 1e9;  my $best_load_cell   = "";
    my $worst_slack = 1e9;  my $worst_slack_cell = "";
    my $best_slack  = -1e9; my $best_slack_cell  = "";
    my $worst_ohms  = 0.0;  my $worst_ohms_cell  = "";
    my $best_ohms   = 1e9;  my $best_ohms_cell   = "";
    foreach my $c (keys %{$cdata_r}) {
        if ($cdata_r->{$c}{TAU} > $worst_tau) {
            $worst_tau = $cdata_r->{$c}{TAU};
            $worst_tau_cell = $c;
        }
        if ($cdata_r->{$c}{TAU} < $best_tau && $cdata_r->{$c}{TAU} != 0.0) {
            $best_tau = $cdata_r->{$c}{TAU};
            $best_tau_cell = $c;
        }
        if ($SS_r->{GS}{DEBUG}) {
            print bold("Cell $c:") . "\n";
            print "  Worst tau = ".sprintf("%.3g", $cdata_r->{$c}{TAU})."\n";
            print "  Area = ".sprintf("%.4g", $cdata_r->{$c}{AREA}) . "\n";
        }
        my $str = "Input ports:";
        foreach my $ip (keys %{$cdata_r->{$c}{I}}) {
            my $load = $cdata_r->{$c}{I}{$ip}{GATE_LOAD};
            if ($load > $worst_load) {
                $worst_load = $load;
                $worst_load_cell = $c;
            }
            if ($load < $best_load) {
                $best_load = $load;
                $best_load_cell = $c;
            }
            $str .= " $ip(" . 
                sprintf("%.3g", $load*1e15) .
                ")";
        }
        $str .= "\n";
        print wrap("  ", "    ", $str) if ($SS_r->{GS}{DEBUG});
        $str = "Output ports:";
        foreach my $op (keys %{$cdata_r->{$c}{O}}) {
            my $slack = $cdata_r->{$c}{O}{$op}{SLACK};
            if ($slack < $worst_slack) {
                $worst_slack = $slack;
                $worst_slack_cell = $c;
            }
            if ($slack > $best_slack) {
                $best_slack = $slack;
                $best_slack_cell = $c;
            }
            my $ohms_up = $cdata_r->{$c}{O}{$op}{OHMS_UP};
            if ($ohms_up > $worst_ohms) {
                $worst_ohms = $ohms_up;
                $worst_ohms_cell = $c;
            }
            if ($ohms_up < $best_ohms && $ohms_up != -1.0) {
                $best_ohms = $ohms_up;
                $best_ohms_cell = $c;
            }
            my $ohms_dn = $cdata_r->{$c}{O}{$op}{OHMS_DN};
            if ($ohms_dn > $worst_ohms) {
                $worst_ohms = $ohms_dn;
                $worst_ohms_cell = $c;
            }
            if ($ohms_dn < $best_ohms && $ohms_dn != -1.0) {
                $best_ohms = $ohms_dn;
                $best_ohms_cell = $c;
            }
            $str .= " ${op}(";
            $str .= sprintf("%.3g", $slack) . ",";
            $str .= sprintf("%.3g", $ohms_up/1e3) .
                    "K,";
            $str .= sprintf("%.3g", $ohms_dn/1e3) . 
                    "K)";
        }
        $str .= "\n";
        print wrap("  ", "    ", $str) if ($SS_r->{GS}{DEBUG});
    }

    if ($SS_r->{GS}{VERBOSE}) {
        for (0..77) { print "-"; }
        print "\n";
        print "CHARACTERIZATION COMPLETE\n";
        print "  Cell count              = " . scalar(keys %{$cdata_r}) . "\n";
        print "  Overall worst tau       = " . sprintf("%.3g", $worst_tau) . 
                    " ps ($worst_tau_cell)\n";
        print "  Overall best tau        = " . sprintf("%.3g", $best_tau) . 
                    " ps ($best_tau_cell)\n";
        print "  Worst output slack      = " . sprintf("%.3g", $worst_slack) .
                    " ps ($worst_slack_cell)\n";
        print "  Best output slack       = " . sprintf("%.3g", $best_slack) .
                    " ps ($best_slack_cell)\n";
        print "  Weakest output driver   = " . sprintf("%.3g", $worst_ohms/1e3).
                    " K-ohms ($worst_ohms_cell)\n";
        print "  Strongest output driver = " . sprintf("%.3g", $best_ohms/1e3) .
                    " K-ohms ($best_ohms_cell)\n";
        for (0..77) { print "-"; }
        print "\n";
    }
}

# Creates wrapper cell for sizing a set of leaf cells independently
sub create_sizing_wrapper {
    my $SS_r          = shift;
    my $cell_name     = shift;
    my $is_library    = shift;
    my $cell_list_r   = shift;
    my $std_proc_cell = get_pdk_variable($SS_r, "jauto/process.config", 
                                         "defaultLayoutAttributes");

    # Create wrapper subtype cast file for sizing purposes
    my $wdir      = get_work_dir($SS_r);
    my $cell_file = fqcn_to_file("$wdir/cast", $cell_name);
    (my $cell_dir  = $cell_file) =~ s/\/[^\/]+$//;
    `mkdir -p '$cell_dir'` if (!-e $cell_dir);
    open (CAST, ">$cell_file") || 
        command_die($SS_r, "Couldn't write to $cell_file.");
    print CAST "/**\n";
    if ($is_library) {
        print CAST " * Supersize Library Characterization File\n";
    }
    else {
        print CAST " * Supersize Leaf Cell Characterization File\n";
    }
    print CAST " * Generated by $ENV{USER} on " . localtime() . "\n";
    print CAST " * Contains " . scalar(@{$cell_list_r}) . " cells.\n";
    print CAST " **/\n";
    print CAST "module " . basetype_of($cell_name) . ";\n";
    print CAST "define \"" . subtype_number_of($cell_name) . "\" " .
               "()() <+ $std_proc_cell {\n";
    print CAST "  subcells {\n";
    my $i=1;
    foreach my $c (@{$cell_list_r}) {
        print CAST "    inline WRAPPER_$i x$i;\n";
        $i++;
    }
    print CAST "  }\n";
    print CAST "  directives { floorplan=false; }\n";
    print CAST "}\n\n";

    # Generate individual leaf cell wrappers to avoid horrendous 
    # Reset/_Reset aliasing
    $i=1;
    foreach my $c (@{$cell_list_r}) {
        print CAST "define WRAPPER_$i ()()() <+ $std_proc_cell <: NULL {\n";
        print CAST "  subcells {\n";
        my @ports = ();
        query_cast_server($SS_r, "--cell=$c --task=ports", \@ports, 1);
        foreach my $port (@ports) {
            if ($port =~ /^1 (\S+) [+-]*(\S+)$/) {
                print CAST "    $1 $2;\n";
            }
        }
        print CAST "    $c y;\n";
        print CAST "  }\n";
        print CAST "}\n\n";
        $i++;
    }
    close CAST;
}


# Reads jauto.as.info, fqcn.spice, and strength.debug to determine the 
# following properties of all cells in @{$cells_r}:
#   - maximum cell tau
#   - unloaded slacks on all output ports
#   - gate load on all input ports
#   - pull-up/pull-down resistances of all output ports
#
sub characterize_cells {
    my $SS_r    = shift;
    my $wdir    = shift;
    my $fqcn    = shift;
    my $cells_r = shift;

    # initialize return data hash
    my $cdata_r = {};
    foreach my $c (@{$cells_r}) {
        $cdata_r->{$c} = {};
        $cdata_r->{$c}{I} = {};
        $cdata_r->{$c}{O} = {};
    }

    # read jauto.as.info
    print "  Reading slack report.\n" if ($SS_r->{GS}{VERBOSE});
    parse_jauto_as_info($SS_r, "$wdir/jauto.as.info", $cdata_r);

    # Determine input loads of all cells
    print "  Determining input loads.\n" if ($SS_r->{GS}{VERBOSE});
    split_jauto_spice_file($SS_r, "$wdir/$fqcn.spice", "$wdir/characterize");
    foreach my $c (keys %{$cdata_r}) {
        my $missing_load = 0;
        # Explicitly ignore GND, Vdd, reset input ports
        foreach my $ip (keys %{$cdata_r->{$c}{I}}) {
            if ($ip eq "Vdd" || $ip eq "GND" || $ip eq "_RESET" ||
                $ip eq "_Reset" || $ip eq "Reset") {
                delete $cdata_r->{$c}{I}{$ip};
            }
        }
        # Read lib CDL for input load (unfortunately we're reparsing this
        # file many many times, but oh well).
        my $i_load_r = captally($SS_r, $c, "$wdir/characterize/${c}.spice", 
                                "$wdir/characterize/gs.spice",
                                keys %{$cdata_r->{$c}{I}});
        foreach my $ip (keys %{$cdata_r->{$c}{I}}) {
            if (exists $i_load_r->{$ip}) {
                $cdata_r->{$c}{I}{$ip}{GATE_LOAD} = $i_load_r->{$ip};
            }
            else {
                $missing_load = 1;
                print "Missing load for port $ip of cell $c.\n"
                    if ($SS_r->{GS}{DEBUG});
            }
        }
        if ($missing_load) {
            print STDERR "Warning: Missing input loads for cell $c.\n";
        }
    }

    # Determine effective resistances of all output half operators
    print "  Reading output strength report.\n" if ($SS_r->{GS}{VERBOSE});
    parse_strength_report($SS_r, $wdir, $cdata_r);

    # Get area data from instances files
    print "  Calculating cell areas.\n" if ($SS_r->{GS}{VERBOSE});
    foreach my $c (keys %{$cdata_r}) {
        $cdata_r->{$c}{AREA} = get_floorplan_area($SS_r, $c);
        if (!defined $cdata_r->{$c}{AREA}) {
            print "    Warning: Area unknown for $c.\n";
            $cdata_r->{$c}{AREA} = 0;
        }
    }

    return $cdata_r;
}


# Read jauto's strength.debug file, determining each output driver's
# pull-up & pull-down resistance.
sub parse_strength_report {
    my $SS_r = shift;
    my $wdir = shift;
    my $cdata_r = shift;

    # Initialize resistances
    foreach my $c (keys %{$cdata_r}) {
        foreach my $op (keys %{$cdata_r->{$c}{O}}) {
            $cdata_r->{$c}{O}{$op}{OHMS_UP} = -1.0;
            $cdata_r->{$c}{O}{$op}{OHMS_DN} = -1.0;
        }
    }

    # Read strength.debug for output strengths (resistances)
    open (STR, "$wdir/strength.debug") || 
        command_die($SS_r, "Couldn't read $wdir/strength.debug.");
    my $in_cell    = 0;
    my $cell = "";
    while (<STR>) {
        if ($cell eq "" && /^CELL\s+([^\s]+)\s+\{/) {
            if (exists $cdata_r->{$1}) {
                $cell = $1;
            }
        }
        elsif ($cell ne "" && /^\}/) {
            $cell = "";
        }
        elsif ($cell ne "" && /^\s*([^\s]+)\s+([+-])\s+([\d\.eE-]+)$/) {
            if (exists $cdata_r->{$cell}{O}{$1}) {
                if ($2 eq "+") {
                    $cdata_r->{$cell}{O}{$1}{OHMS_UP} = $3;
                }
                else {
                    $cdata_r->{$cell}{O}{$1}{OHMS_DN} = $3;
                }
            }
        }
    }
    close STR;

    # Check for problems
    foreach my $c (keys %{$cdata_r}) {
        my $missing_strength = 0;
        foreach my $op (keys %{$cdata_r->{$c}{O}}) {
            if ($cdata_r->{$c}{O}{$op}{OHMS_UP} == -1.0 &&
                $cdata_r->{$c}{O}{$op}{OHMS_DN} == -1.0) {
                print "Missing resistance for port $op of cell $c.\n"
                    if ($SS_r->{GS}{DEBUG});
                $missing_strength = 1;
            }
        }
        if ($missing_strength) {
            print STDERR "Warning: Missing output resistances for cell $c.\n";
        }
    }
}

# Read jauto's jauto.as.info file, determining the i/o ports of each
# cell, each cell's worst internal tau value, and the worst slack of
# each output port.
sub parse_jauto_as_info {
    my $SS_r    = shift;
    my $file    = shift;
    my $cdata_r = shift;

    open (INFO, "$file") || command_die($SS_r, "Couldn't read $file.");
    my $depth = 0;
    my $block = 0;
    my $cell  = "";
    my $net   = "";
    my $endnet = "";
    my $delay;
    my $is_port = 0;
    my $dir;
    my $lnum  = 0;
    while (<INFO>) {
        $lnum++;
        chomp; s/^\s*//; s/\s*$//; next if (/^$/);
        if ($depth==0 && /^CELL\s+([^\s+]+)\s+{$/) {
            $cell = $1 if (exists $cdata_r->{$1});
            $depth++;
            $block = "CELL";
        }
        elsif ($depth==0 && /{$/) {
            $depth++;
            $block = "unknown";
        }
        elsif ($depth==0) {
            print STDERR "Ignoring unknown syntax on line $lnum of $file.\n";
        }

        elsif ($depth==1 && $block eq "CELL" &&  $cell ne "" && 
               /^CELL_NET\s+([^\s]+)\s+{$/) {
            $depth++;
            $block = "CELL_NET";
            $net   = $1;
        }
        elsif ($depth==1 && $block eq "CELL" && $cell ne "" &&
               /^SIZING_PATH\s+([^\s]+)\s+{$/) {
            $depth++;
            $block = "SIZING_PATH";
        }
        elsif ($depth==1 && /{$/) {
            $depth++;
        }
        elsif ($depth==1 && /^}$/) {
            $depth--;
            if ($cell ne "" && !exists $cdata_r->{$cell}{TAU}) {
                # many simple leaf cells don't have internal paths, thus
                # they really don't have a defined TAU.
                $cdata_r->{$cell}{TAU} = 0.0;
            }
            $cell = "";
            $block = "";
        }
        
        elsif ($depth==2 && $block eq "CELL_NET" && 
               /^is_port\s*=\s*([^;]+);/) {
            $is_port = ($1 eq "true");
        }
        elsif ($depth==2 && $block eq "CELL_NET" && 
               /^port_direction\s*=\s*([^;]+);/) {
            if ($1 eq "0") {
                print STDERR "Redefining net $net of cell $cell on line " .
                    "$lnum!\n" if (exists $cdata_r->{$cell}{I}{$net});
                $cdata_r->{$cell}{I}{$net} = {};
                $dir = 0;
            }
            else {
                print STDERR "Redefining net $net of cell $cell on line " .
                    "$lnum!\n" if (exists $cdata_r->{$cell}{O}{$net});
                $cdata_r->{$cell}{O}{$net} = {};
                $dir = 1;
            }
        }
        elsif ($depth==2 && $block eq "CELL_NET" && $is_port &&
               $dir==0 && /^n_sinks\s*=\s*0;/) {
            delete $cdata_r->{$cell}{I}{$net};
        }
        elsif ($depth==2 && $block eq "CELL_NET" && $is_port &&
                $dir==1 && /^n_sources\s*=\s*0;/) {
            delete $cdata_r->{$cell}{O}{$net};
        }
        elsif ($depth==2 && $block eq "SIZING_PATH" &&
               /^end_net\s*=\s*([^;]+);/) {
            $endnet = $1 if (exists $cdata_r->{$cell}{O}{$1});
        }
        elsif ($depth==2 && $block eq "SIZING_PATH" &&
               /^delay\s*=\s*\[([\d\.eE-]+)\];/ && $endnet eq "") {
            $delay = $1;
        }
        elsif ($depth==2 && $block eq "SIZING_PATH" &&
               /^slack\s*=\s*\[([\d\.eE-]+)\];/ && $endnet eq "") {
            my $tau = $SS_r->{GS}{TAU}*$delay/($delay+$1);
            if (!exists $cdata_r->{$cell}{TAU} ||
                $tau > $cdata_r->{$cell}{TAU}) {
                $cdata_r->{$cell}{TAU} = $tau;
            }
        }
        elsif ($depth==2 && $block eq "SIZING_PATH" &&
               /^slack\s*=\s*\[([\d\.eE-]+)\];/ && $endnet ne "") {
            if (!exists $cdata_r->{$cell}{O}{$endnet}{SLACK} ||
                $1 < $cdata_r->{$cell}{O}{$endnet}{SLACK}) {
                $cdata_r->{$cell}{O}{$endnet}{SLACK} = $1;
            }
        }
        elsif ($depth==2 && /{$/) {
            $depth++;
        }
        elsif ($depth==2 && /^}/) {
            if ($block eq "SIZING_PATH" && $endnet ne "") {
                # cellnonobservable cells unfortunately don't have 
                # delay/slack measurements
                if (!exists $cdata_r->{$cell}{O}{$endnet}{SLACK}) {
                    $cdata_r->{$cell}{O}{$endnet}{SLACK} = 0.0;
                }
            }
            $depth--;
            $endnet = "";
            $is_port = 0;
            if ($block eq "SIZING_PATH" || $block eq "CELL_NET") {
                $block = "CELL";
            }
        }

        elsif ($depth>2 && /{$/) {
            $depth++;
        }
        elsif ($depth>2 && /^}/) {
            $depth--;
        }
    }
    close INFO;
}
                
# Writes a merge library characterization file (.sslib) for later merging
# use.
sub write_characterization_data {
    my $SS_r    = shift;
    my $file    = shift;
    my $cdata_r = shift;

    open (SSLIB, ">$file") || command_die($SS_r, "Couldn't write to $file.");
    foreach my $c (sort keys %{$cdata_r}) {
        print SSLIB "CELL $c\n";
        print SSLIB "  TAU $cdata_r->{$c}{TAU}\n";
        print SSLIB "  AREA $cdata_r->{$c}{AREA}\n";
        foreach my $ip (sort keys %{$cdata_r->{$c}{I}}) {
            print SSLIB "  INPUT $ip $cdata_r->{$c}{I}{$ip}{GATE_LOAD}\n";
        }
        foreach my $op (sort keys %{$cdata_r->{$c}{O}}) {
            print SSLIB "  OUTPUT $op ";
            print SSLIB $cdata_r->{$c}{O}{$op}{SLACK} . " ";
            print SSLIB $cdata_r->{$c}{O}{$op}{OHMS_UP} . " ";
            print SSLIB $cdata_r->{$c}{O}{$op}{OHMS_DN} . "\n";
        }
    }
    close SSLIB;
}


# Reads a merge library characterization file (.sslib), returns the 
# data in a map
#
# (basetype)                                - base cell name
#   -> (subtype)                            - fqcn subtype name
#           -> TAU                          - worst cell tau (ps)
#           -> AREA                         - cell area (square microns)
#           -> I -> (input)  -> GATE_LOAD   - input gate load (F)
#           -> O -> (output) -> SLACK       - output slack (ps)
#                            -> OHMS_UP     - driver resistance to Vdd (ohms)
#                            -> OHMS_DN     - driver resistance to GND (ohms)
#
# Note: the extra base -> subtype level of mapping compared to the
# write_characterization_data structure in order to allow more efficient
# searching during merging.
#
sub read_characterization_data {
    my $SS_r = shift;
    my $file = shift;
    my $cdata_r = {};

    open (SSLIB, $file) || command_die($SS_r, "Couldn't read $file.");
    my ($b, $s);
    while (<SSLIB>) {
        s/^\s*//; s/\s*(#.*)?$//; chomp;
        next if (/^$/);
        my @f = split /\s+/, $_;
        if ($f[0] eq "CELL") {
            $s = $f[1];
            $b = basetype_of($s);
            $cdata_r->{$b} = {} if (!exists $cdata_r->{$b});
            $cdata_r->{$b}{$s} = {};
            $cdata_r->{$b}{$s}{I} = {};
            $cdata_r->{$b}{$s}{O} = {};
        }
        elsif ($f[0] eq "TAU") {
            $cdata_r->{$b}{$s}{TAU} = $f[1];
        }
        elsif ($f[0] eq "AREA") {
            $cdata_r->{$b}{$s}{AREA} = $f[1];
        }
        elsif ($f[0] eq "INPUT") {
            $cdata_r->{$b}{$s}{I}{$f[1]} = {};
            $cdata_r->{$b}{$s}{I}{$f[1]}{GATE_LOAD} = $f[2];
        }
        elsif ($f[0] eq "OUTPUT") {
            $cdata_r->{$b}{$s}{O}{$f[1]} = {};
            $cdata_r->{$b}{$s}{O}{$f[1]}{SLACK} = $f[2];
            $cdata_r->{$b}{$s}{O}{$f[1]}{OHMS_UP} = $f[3];
            $cdata_r->{$b}{$s}{O}{$f[1]}{OHMS_DN} = $f[4];
        }
        else {
            print STDERR "Unrecognized line in $file:\n  $_\n";
        }
    }
    close SSLIB;

    return $cdata_r;
}

# Splits up Jauto's output spice file into one file per leaf cell, puts
# all stack & gate definitions into a separate file (gs.spice).  This vastly 
# improves the overall captally run time.
sub split_jauto_spice_file {
    my $SS_r       = shift;
    my $spice_file = shift;
    my $out_dir    = shift;

    mkdir $out_dir if (!-e $out_dir);
    open (IN, $spice_file) || 
        command_die($SS_r, "Couldn't read $spice_file.");
    open (OUT, ">$out_dir/gs.spice") ||
        command_die($SS_r, "Couldn't write to $out_dir/gs.spice");
    while (<IN>) {
        if (/^\.SUBCKT\s+([^\s]+)/) {
            my $fqcn = $1;
            close OUT;
            my $file = ">$out_dir/$fqcn.spice";
            if ($fqcn =~ /^gate\./ || $fqcn =~ /^stack\./) {
                $file = ">>$out_dir/gs.spice";
            }
            open (OUT, $file) || command_die($SS_r, "Couldn't write to $file.");
        }
        print OUT;
    }
    close IN;
    close OUT;
}


1;
