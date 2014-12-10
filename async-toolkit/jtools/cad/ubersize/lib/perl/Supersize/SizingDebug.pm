#
# SizingDebug
#
# Supersize module for processing the Jauto *.debug files.
#
#

package Supersize::SizingDebug;

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT = qw(
        &generate_sizing_report
        &generate_derived_files
    );
    our @EXPORT_OK = qw( 
        &get_module_data
    );
}

use strict;
use Supersize::Merging;
use Supersize::Util;
use Supersize::JavaUtil;
use Supersize::TypeUtil;

sub get_module_data {
  return { NAME => "SizingDebug",
           DESC => "Commands for processing Jauto paths_*.debug files.",
           COMMANDS => {
    "path_query" => {
        SUBREF => \&path_query,
        USAGE  => "path_query [--fixed|sizable|all] " .
                             "[--sort-by=" . underline("sort_expression") ."] ".
                             "[--report=containers|endnets|cells] ".
                             underline("filter_condition"),
        DESC   =>
          "Reads and filters jauto's paths_*.debug file, presenting the " .
          "information in a form that's more useful for debugging sizing " .
          "problems.\n\n" .
          "By default, the paths_all.debug will be read, unless overridden " .
          "with --fixed or --sizable.  By default, the list of cells " .
          "containing complete qualifying paths will be reported " .
          "(--report=containers).  However, --report=endnets or " .
          "--report=cells will report qualifying path endnets or all cells " .
          "included in qualifying paths, respectively.\n\n" .
          "The " . underline("filter_condition") . " identifies which paths " .
          "will be included in the report.  Arbitrary expressions involving " .
          "the four variables slack, delay, avg_width, and tau are permitted. ".
          "For example, \"slack < -1\" or \"-slack/delay > 0.1\".  " .
          "All numerical values are in picoseconds or microns, depending on " .
          "the variable.  The tau variable is unique in that it is derived ".
          "from the slack, delay, and global TAU variables.  Specifically, ".
          "tau is substituted for TAU * delay / (delay+slack). " .
          "Note that you'll need to quote the filtering " .
          "expression if it includes '>' or '<' symbols.\n\n" .
          "Results are sorted from higher to lower numerical values of ".
           underline("sort_expression") . ", which can be any expression in ". 
           "terms of the above variables.  Choose your sort expression such ".
           "that larger numerical values correspond to worse violations.  ".
           "If you don't specify a sort expression, supersize tries to guess ".
           "what you want.",
        GLOBALS => { 
          WORK_DIR => { REQUIRED => 1 },
          TAU => {
            DESC => "Nominal tau value the unit was sized at.  Used in " .
                    "the calculation of the 'tau' variable." } },
        RV  => {
          "containers" => {
            TYPE => $TYPE_LIST,
            DESC => "List of containers with qualifying paths" },
          "endnets"    => {
            TYPE => $TYPE_LIST,
            DESC => "List of nets ending qualifying paths." },
          "cells"      => {
            TYPE => $TYPE_LIST,
            DESC => "List of cells falling on qualifying paths." } } },
    "leaf_query" => {
        SUBREF => \&leaf_query,
        USAGE  => "leaf_query [--sort-by=" . underline("sort_expression") ."] ".
                             underline("filter_condition") . " | " .
                             underline("fqcn"),
        DESC   =>
          "Outputs a filtered and sorted listing of the cells in Jauto's " .
          "cells.debug file.  (Note that despite its name, the cells.debug " .
          "file only includes leaf cells.)\n\n" .
          "The " . underline("filter_condition") . " identifies which cells " .
          "will be included in the report.  Arbitrary expressions involving " .
          "the five numerical variables instances, all_width, avg_width, " .
          "max_width, min_width, and prs, as well as the boolean 'fixed' are " .
          "supported.  For example, \"!fixed && instances > 10 && " .
          "avg_width > 3\".  All width values are in units of microns.  The " .
          "'prs' variable refers to the cell's production rule count. " .
          "Note that you'll need to quote the filtering " .
          "expression if it includes '>' or '<' symbols.\n\n" .
          "Results are sorted from higher to lower numerical values of ".
           underline("sort_expression") . ", which can be any expression in ". 
           "terms of the above variables.  If you don't specify a sort " .
           "expression, supersize tries to guess what you want.\n\n".
           "If either the 'fixed' or 'prs' variables are referenced in " . 
           underline("filter_condition") . ", then the Java Cast server will ". 
           "be invoked in order to obtain this information.  This could ".
           "significantly slow down the run-time of this command.  In this " .
           "case you must also ensure that TOP is set to match the " .
           "cells.debug file's top-level cell.\n\n" .
           "If instead a cell name ".underline("fqcn")." is specified, then ".
           "leaf_query returns a list of sizing quantities (e.g. avg_width, ".
           "max_width, etc.) associated with that cell.  If a base cell type ".
           "is specified, then the information for all subtypes of that cell ".
           "is listed.",
        GLOBALS => { 
            WORK_DIR => { REQUIRED => 1 },
            TOP => { DESC => "Needs to be set if 'fixed' or 'prs' appears ".
                             "in either the sort variable or the filter " .
                             "condition." } },
        RV  => {
          "cells" => {
            TYPE => $TYPE_LIST,
            DESC => "List of filtered cells." } } },
    "sizing_report" => {
        SUBREF => \&sizing_report,
        USAGE  => "sizing_report [--trial=".underline("trial-list")."]",
        DESC   =>
          "Produces a brief sizing report of the most recent sizing run.  ".
          "A report of one or more recent trial_size runs can be generated ".
          "using the --trial argument.  (See ".bold("trial_size")." for a ".
          "description of the list's format.)\n\n".
          "The average width cell categories are defined as follows:\n".
          "  fixed    - All cells with fixed_size=true.\n".
          "  flow     - All lib.flow.sizing cells with fixed_size=false.\n".
          "  sizable  - All other cells with fixed_size=false.\n".
          "  smr buf  - All sizable refinement children of\n".
          "             lib.buffer.smr.SMR_BUF_1of\n".
          "  fragment - All sizable simple operators.\n".
          "  custom   - All sizable cells under lib.sram and lib.router.\n".
          "  general  - All other sizable cells.\n",
        GLOBALS => { 
            WORK_DIR => { REQUIRED => 1 },
            TAU => { REQUIRED => 1 },
            CAST_DIR => { REQUIRED => 1 },
            SPEC_DIR => { REQUIRED => 1 },
            TOP => { REQUIRED => 1 } } },
    }
  };
}

#
# path_query
#
sub path_query {
    # Get reference to global variables
    my $SS_r   = shift;
    my $GS_r   = $SS_r->{GS};
    my @legal_vars = ("slack", "real_slack", "delay", "avg_width", "tau", "reff", "c_wire", "budget_cap");

    my $wdir   = get_work_dir($SS_r);

    # Initialize return variables (hashes used to ensure uniqueness)
    $SS_r->{RV}{path_query} = {};
    my $RV_r   = $SS_r->{RV}{path_query};
    my %containers = ();
    my %endnets    = ();
    my %cells      = ();

    # Parse arguments
    my $type    = "all";
    my $report  = "containers";
    my $filter  = "";
    my $sort_by = "";
    while (num_args(\@_)) {
        my $arg = shift_next_scalar(\@_);
        if ($arg eq "--fixed" || $arg eq "--sizable" || $arg eq "--all") {
            $arg =~ /^--(.*)$/; $type = $1;
        }
        elsif ($arg eq "--sort-by") {
            my $eq = shift_next_scalar(\@_);
            $sort_by = shift_next_scalar(\@_);
            if ($eq ne "=" || !grep {$sort_by =~ /$_/} @legal_vars) {
                usage($SS_r, "Bad argument to --sort-by.");
                return;
            }
        }
        elsif ($arg eq "--report") {
            my $eq = shift_next_scalar(\@_);
            $report = shift_next_scalar(\@_);
            if ($eq eq "" || $report eq "" || $report ne "containers" &&
                $report ne "endnets" && $report ne "cells") {
                usage($SS_r, "Bad argument to --report.");
                return;
            }
        }
        elsif ($arg =~ /^--/) {
            usage($SS_r, "Unknown path_query argument '$arg'.");
            return;
        }
        else {
            $filter .= "$arg ";
        }
    }

    # Magically set sort_by if the user didn't
    if ($sort_by eq "") {
        if ($filter =~ /tau/) {
            $sort_by = "tau";
        }
        elsif ($filter =~ /slack/) {
            $sort_by = "-slack";
        }
        elsif ($filter =~ /avg_width/) {
            $sort_by = "avg_width";
        }
        elsif ($filter =~ /delay/) {
            $sort_by = "delay";
        }
        elsif ($filter =~ /reff/) {
            $sort_by = "reff";
        }
        elsif ($filter =~ /c_wire/) {
            $sort_by = "c_wire";
        }
        elsif ($filter =~ /budget_cap/) {
            $sort_by = "budget_cap";
        }
    }

    # Substitute tau expression
    if (($filter =~ /tau/ || $sort_by =~ /tau/) && !defined $GS_r->{TAU}) {
        command_die($SS_r, "Required variable TAU isn't set.");
    }
    $filter  =~ s/tau/$GS_r->{TAU}\*delay\/\(delay+slack\)/g;
    $sort_by =~ s/tau/$GS_r->{TAU}\*delay\/\(delay+slack\)/g;

    # Process filter string
    eval { $filter = process_expression($filter, @legal_vars) };
    if ($@) {
        usage($SS_r, "Illegal filter expression.");
        return;
    }
    eval { $sort_by = process_expression($sort_by, @legal_vars); };
    if ($@) {
        usage($SS_r, "Illegal --sort-by expression.");
        return;
    }

    # Read path file
    my %results;
    if (! -e "$wdir/paths_${type}.debug") {
        print "Error: No paths_${type}.debug file.\n";
        return;
    }
    open (PATHS, "$wdir/paths_${type}.debug");
    my $pstate    = 0;
    my $container = "";
    my %vars;
    my $matches   = 0;
    my $ranking;
    while (<PATHS>) {
        chomp;
        if ($pstate == 0) {
            if (/^\/\/ container=(.*)$/) {
                $container = $1;
                $pstate    = 1;
            }
            else {
                print "Warning: Unexpected syntax in paths_${type}.debug:\n";
                print "$_\n";
            }
        }
        elsif ($pstate == 1) {
            if (/^\/\/ delay=([\d\.]+)ps slack=([-\d\.E]+)ps (?:real_slack=([-\d\.E]+)ps )?avg_width=([\d\.]+)um/) {
                $vars{delay} = $1; $vars{slack} = $2;
                $vars{real_slack} = $3 ? $3 : $2; $vars{avg_width} = $4;
                $matches = eval($filter);
                $ranking = eval($sort_by);
                if ($matches) {
                    $containers{$container} = 1;
                    if ($report eq "containers") {
                        if (!defined($results{$container}) ||
                            $results{$container} < $ranking) {
                            $results{$container} = $ranking;
                        }
                    }
                }
                if (/ reff=(\d+\.\d+)/) {
                    $vars{reff}=$1;
                }
                if (/ c_wire=(\d+\.\d+)fF/) {
                    $vars{c_wire}=$1;
                }
                if (/ budget_cap=([-\d]+\.\d+)/) {
                    $vars{budget_cap}=$1;
                }
                $pstate = 2;
            }
            else {
                print "Warning: Unexpected syntax in paths_${type}.debug:\n";
                print "$_\n";
                $pstate = 0;
            }
        }
        elsif ($pstate == 2) {
            if (/^\/\/ endnet=(.*)$/) {
                if ($matches) {
                    $endnets{$1} = 1;
                    if ($report eq "endnets") {
                        if (!defined($results{$1}) ||
                            $results{$1} < $ranking) {
                            $results{$1} = $ranking;
                        }
                    }
                }
                $pstate = 3;
            }
            else {
                print "Warning: Unexpected syntax in paths_${type}.debug:\n";
                print "$_\n";
                $pstate = 0;
            }
        }
        elsif ($pstate == 3) {
            if (/^\/\/ delaybias=(.*)$/) {
                # ignore for now
            }
            elsif (/^([^:]+):.*$/) {
                if ($matches) {
                    $cells{$1} = 1;
                    if ($report eq "cells") {
                        if (!defined($results{$1}) ||
                            $results{$1} < $ranking) {
                            $results{$1} = $ranking;
                        }
                    }
                }
            }
            elsif (/^\s*$/) {
                $pstate = 0;
            }
            else {
                print "Warning: Unexpected syntax in paths_${type}.debug:\n";
                print "$_\n";
                $pstate = 0;
            }
        }
    }
    close PATHS;

    # Sort and report results
    print "Matching ${report}:\n";
    my @sorted_keys = sort { $results{$b} <=> $results{$a} } keys %results;
    foreach my $k (@sorted_keys) {
        my $num_str = sprintf '%5.4g', $results{$k};
        print " $num_str $k\n";
    }

    # Set return variables
    my @cont  = keys %containers;
    set_cmd_return_variable($SS_r, "containers", [ $TYPE_LIST, \@cont ]);
    my @endn  = keys %endnets;
    set_cmd_return_variable($SS_r, "endnets", [ $TYPE_LIST, \@endn ]);
    my @cells = keys %cells;
    set_cmd_return_variable($SS_r, "cells", [ $TYPE_LIST, \@cells ]);
}

#
# leaf_query
#
sub leaf_query {
    # Get reference to global variables
    my $SS_r = shift;
    my $GS_r = $SS_r->{GS};
    my $wdir = get_work_dir($SS_r);
    my @legal_vars = ( "avg_width", "max_width", "all_width", 
                       "instances", "prs", "min_width", "fixed" );

    # Parse arguments
    my $sort_by = "";
    my $filter  = "";
    while (num_args(\@_)) {
        my $arg = shift_next_scalar(\@_);
        if ($arg eq "--sort-by") {
            my $eq = shift_next_scalar(\@_);
            $sort_by = shift_next_scalar(\@_);
            if ($eq ne "=" || !grep {$sort_by =~ /$_/} @legal_vars) {
                usage($SS_r, "Bad argument to --sort-by.");
                return;
            }
        }
        elsif ($arg =~ /^--/) {
            usage($SS_r, "Unknown cell_query argument '$arg'.");
            return;
        }
        else {
            if ($filter eq "") {
                $filter = $arg;
            }
            else {
                $filter .= " $arg";
            }
        }
    }

    # Magically set sort_by if the user didn't
    if ($sort_by eq "") {
        foreach my $v (@legal_vars) {
            if ($filter =~ /$v/) {
                $sort_by = $v;
                last;
            }
        }
    }

    my $list_cell = 0;
    my $fqcn;
    if ($filter =~ /^(\w+\.)+[\w\(\),\{\}]*(\.\w+)?$/) {
        # looks like a fqcn; don't interpret as a filter condition
        $list_cell = 1;
        $fqcn      = $filter;
    }

    if (($filter =~ /prs/ || $filter =~ /fixed/ ||
        $sort_by =~ /prs/ || $sort_by =~ /fixed/ || $list_cell) && 
        !defined $GS_r->{TOP}) {
        command_die($SS_r, "Required variable TOP isn't set.");
    }

    # Process filter string
    if (!$list_cell) {
        eval { $filter = process_expression($filter, @legal_vars); };
        if ($@) {
            usage($SS_r, "Illegal filter expression.");
            return;
        }
        eval { $sort_by = process_expression($sort_by, @legal_vars); };
        if ($@) {
            usage($SS_r, "Illegal --sort-by expression.");
            return;
        }
    }

    # Check for existence of cells.debug file
    if (! -e "$wdir/cells.debug") {
        usage($SS_r, "Error: No cells.debug file.");
        return;
    }

    # Get list of fixed size leaf cells if filter condition includes "fixed"
    # or if listing specific cell(s).
    my %fixed_cells = ();
    if ($filter =~ /\{fixed\}/ || $sort_by =~ /\{fixed\}/ || $list_cell) {
        my @output = ();
        query_cast_server($SS_r, "--filter=leaf&fixed --cell=$GS_r->{TOP} " .
                                 "--task=subcells", \@output, 1);
        # Store in hash to avoid repeated o(n) searches
        foreach my $cell (@output) {
            $fixed_cells{$cell} = 1;
        }
    }

    # Get prs counts of all leaf cells if filter condition includes "prs"
    my %prs = ();
    if ($filter =~ /\{prs\}/ || $sort_by =~ /\{prs\}/ || $list_cell) {
        my @output = ();
        query_cast_server($SS_r, "--filter=leaf --cell=$GS_r->{TOP} " .
                                 "--task=prs", \@output, 1);
        foreach my $line (@output) {
            my ($cell, $prs) = split /\s+/, $line;
            $prs{$cell} = $prs;
        }
    }

    # Read cells.debug file
    if (!open (CELLS, "$wdir/cells.debug")) {
        usage($SS_r, "Error: Couldn't read cells.debug file.");
        return;
    }
    my %results;
    my %vars;
    my $cell = "";
    my $match = 0;
    while (<CELLS>) {
        chomp;
        if (/^CELL\s+([^\s]+)\s+\{\s*$/) {
            $cell = $1;
            if ($list_cell && (substr($cell,0,length($fqcn)) eq $fqcn)) {
                $match = 1;
                if (!exists $prs{$cell}) {
                    $match = 0;
                    if ($SS_r->{GS}{VERBOSE}) {
                        print "$cell:\n";
                        print "  Not a leaf cell under TOP.\n\n";
                    }
                }
                else {
                    print "$cell:\n";
                    print "  fixed = " . 
                        (exists $fixed_cells{$cell} ? "1" : "0") . "\n";
                    print "  prs = " . $prs{$cell} . "\n";
                }
            }
            else {
                $match = 0;
            }
        }
        elsif ($cell ne "") {
            if (/^\s*(\w+)\s*=\s*([\d.]+)u?/) {
                $vars{$1} = $2;
                if ($match) {
                    print "  $1 = $2\n";
                }
            }
            elsif (/^\}/) {
                if ($list_cell && $match) {
                    print "\n";
                    $results{$cell} = 1;
                }
                elsif (%vars) {
                    # evaluate cell if it was a leaf cell
                    if ($fixed_cells{$cell}) {
                        $vars{fixed} = 1;
                    }
                    else {
                        $vars{fixed} = 0;
                    }
                    $vars{prs} = $prs{$cell} if ($prs{$cell});
                    $results{$cell} = eval($sort_by) if (eval($filter));
                }
                $cell = "";
                %vars = ();
            }
            else {
                print "Warning: Unexpected syntax in cells.debug:\n";
                print "$_\n";
                $cell = "";
                %vars = ();
            }
        }
    }
    close CELLS;

    # Sort and report results
    if (!$list_cell) {
        print "Matching cells:\n";
        my @sorted_keys = sort { $results{$b} <=> $results{$a} } keys %results;
        foreach my $k (@sorted_keys) {
            my $num_str = sprintf '%5.4g', $results{$k};
            print " $num_str $k\n";
        }
    }
    elsif (!%results) {
        print "No matching cells in cells.debug.\n";
    }

    my @cells = keys %results;
    set_cmd_return_variable($SS_r, "cells", [ $TYPE_LIST, \@cells ]);
}

# Calls die, so call with eval{}.
sub process_expression {
    my $str = shift;
    my $leftover = $str;
    foreach my $var (@_) {
        $str =~ s/$var/\$vars\{$var\}/g;
        $leftover =~ s/$var/ /g;
    }
    $leftover =~ s/[\d\.]+//g;
    foreach my $ok ("and", "or", '\&\&', "!", '\|\|', '\(', '\)', '>=', '<=', 
                    '>', '<', '\*', '\/', '==', '\-', '\+') {
        $leftover =~ s/$ok/ /g;
    }
    die if ($leftover !~ /^\s+$/);
    return $str;
}

#
# sizing_report
#
sub sizing_report {
    my $SS_r = shift;
    my @trials;

    # Parse args
    while (num_args(\@_)) {
        my $arg = shift_next_scalar(\@_);
        if ($arg eq "--trial") {
            my $eq = shift_next_scalar(\@_);
            if (!defined $eq || $eq ne "=") {
                command_die($SS_r, "Bad argument to --trial specified.");
            }
            my ($type, $val) = shift_next_arg(\@_);
            if ($type == $TYPE_SCALAR) {
                push @trials, parse_trials($SS_r, $val);
            }
            elsif ($type == $TYPE_LIST) {
                push @trials, @{$val};
            }
            else {
                command_die($SS_r, "Bad argument to --trial specified.");
            }
        }
        else {
            command_die($SS_r, "Unrecognized argument '$arg'.");
        }
    }

    if (!@trials) {
        print generate_sizing_report($SS_r, get_work_dir($SS_r),
                                     "Sizing Report");
    }
    else {
        foreach my $trial (@trials) {
            my $dir = trial_to_merge_dir($SS_r, $trial); 
            set_server_merge_dir($SS_r, $dir);
            print generate_sizing_report($SS_r, $dir,
                    "Trial $trial Sizing Report");
        }
    }
}

sub generate_sizing_report {
    my $SS_r  = shift;
    my $dir   = shift;
    my $title = shift;
    my $str   = "";

    # header
    my $width = 52;
    my $lnum  = ($width - length($title))/2;
    for (0..$lnum-1) { $str .= "-"; }
    $str .= " $title ";
    for (0..($width-1-length($title)-$lnum)) { $str .= "-"; }
    $str .= "\n";

    # check for existence of necessary files
    if (!-e "$dir/size.out" && !-e "$dir/trial_size.out" && !-e "$dir/presize.out" ||
        !-e "$dir/size.err" && !-e "$dir/trial_size.err" && !-e "$dir/presize.err" ||
        !-e "$dir/cells.debug" || !-e "$dir/paths_all.debug") {
        $str .= "Incomplete sizing data in $dir.\n";
        $str .= "No report generated.\n";
        return $str;
    }
    
    # Report worst tau
    $str .= format_report_line("Worst tau:", 34, 
                               parse_worst_tau($SS_r, $dir) . " ps");

    # Report # cells above target tau
    $str .= format_report_line("Number of cells > $SS_r->{GS}{TAU} ps:", 34,
                               scalar(parse_bad_tau_cells($SS_r, $dir)));

    # Report worst negative slack
    $str .= format_report_line("Worst negative slack:", 34,
                               parse_worst_slack($SS_r, $dir) . " ps");

    # Report average transistor widths
    my ($ave_r, $pct_r) = calc_ave_widths($SS_r, $dir);
    $str .= format_report_line("Average transistor width:", 
                34, "$ave_r->{all} um");
    $str .= format_report_line("  in fixed-size cells:",
                34, $ave_r->{fixed} .  ($ave_r->{fixed} ne "-" ? " um" : ""), 
                44, "($pct_r->{fixed})");
    $str .= format_report_line("  in lib.flow.sizing cells:",
                34, $ave_r->{flow} .  ($ave_r->{flow} ne "-" ? " um" : ""), 
                44, "($pct_r->{flow})");
    $str .= format_report_line("  in sizable cells:", 
                34, $ave_r->{sizable}.($ave_r->{sizable} ne "-" ? " um" : ""), 
                44, "($pct_r->{sizable})");
    $str .= format_report_line("    in smr buffers:", 
                34, $ave_r->{smr}.($ave_r->{smr} ne "-" ? " um" : ""), 
                44, "($pct_r->{smr})");
    $str .= format_report_line("    in fragments:", 
                34, $ave_r->{fragment}.($ave_r->{fragment} ne "-" ? " um" : ""),
                44, "($pct_r->{fragment})");
    $str .= format_report_line("    in custom cells:", 
                34, $ave_r->{custom}.($ave_r->{custom} ne "-" ? " um" : ""), 
                44, "($pct_r->{custom})");
#    $str .= format_report_line("  in sizable buffers:", 
#                34, $ave_r->{buffer}.($ave_r->{buffer} ne "-" ? " um" : ""), 
#                44, "($pct_r->{buffer})");
    $str .= format_report_line("    in general logic:", 
                34, $ave_r->{other}.($ave_r->{other} ne "-" ? " um" : ""), 
                44, "($pct_r->{other})");
    
    return $str;
}

sub format_report_line {
    my $str   = shift;
    
    while (@_) {
        my $width = shift;
        my $val   = shift;
        for (0..($width-length($str)-1)) { $str .= " "; }
        $str .= $val;
    }

    return $str . "\n";
}

# Parses size.err/trial_size.err/presize.err to obtain worst tau
sub parse_worst_tau {
    my $SS_r = shift;
    my $dir  = shift;
    my $file = "$dir/size.err";

    if (!-e $file) { 
        if (-e "$dir/trial_size.err") {
            $file = "$dir/trial_size.err";
        }
        elsif (-e "$dir/presize.err") {
            $file = "$dir/presize.err";
        }
        else {
            return "Unknown";
        }
    }

    open (SIZE_ERR, $file) || command_die($SS_r, "Couldn't read $file.");
    my $worst_tau;
    while (<SIZE_ERR>) {
        if (/Worst\s+Tau=([\d\.]+)ps/i) {
            $worst_tau = $1;
        }
    }
    close SIZE_ERR;
    return defined $worst_tau ? $worst_tau : "Unknown";
}

# Parses size.out/trial_size.out/presize.out to obtain list of cells w/ bad taus
sub parse_bad_tau_cells {
    my $SS_r = shift;
    my $dir  = shift;
    my $file = "$dir/size.out";

    if (!-e $file) {
        if (-e "$dir/trial_size.out") {
            $file = "$dir/trial_size.out";
        }
        elsif (-e "$dir/presize.out") {
            $file = "$dir/presize.out";
        }
        else {
            command_die($SS_r, "Couldn't locate size.out or trial_size.out or presize.out in ".
                               $dir);
        }
    }

    open (SIZE_OUT, $file) || command_die($SS_r, "Couldn't read $file.");
    my @cells;
    while (<SIZE_OUT>) {
        if (/^Cell Tau=[\d\.]+ps for (.*)$/) {
            push @cells, $1;
        }
    }
    close SIZE_OUT;
    return @cells;
}

# Parses paths_all.debug to determine the smallest slack value
sub parse_worst_slack {
    my $SS_r = shift;
    my $dir  = shift;
    my $file = "$dir/paths_all.debug";
    if (!-e $file) {
        return "Unknown";
    }
    open (PATHS, $file) || command_die($SS_r, "Couldn't read $file.");
    my $worst;
    while (<PATHS>) {
        if (/ slack=([-\d\.]+)ps/) {
            if (!defined $worst || $1 < $worst) {
                $worst = $1;
            }
        }
    }
    close PATHS;
    return defined $worst ? $worst : "Unknown";
}

# Reads cells.debug, tallies average transistor width statistics for
# different cell categories.  Must parse the cast to determine list of
# fixed-size cells.  Cast is assumed to be in $dir/cast.
sub calc_ave_widths {
    my $SS_r = shift;
    my $dir  = shift;
    my $file = "$dir/cells.debug";
    my $wdir = get_work_dir($SS_r);

    # Determine fixed-size cells plus list of fragment cells
    my @fixed;
    my @frags;
    my @smr;
    my $query_cmd = "--cell=$SS_r->{GS}{TOP} --task=subcells";
    query_cast_server($SS_r, "$query_cmd --filter=leaf&fixed", \@fixed, 1);
    query_cast_server($SS_r, "$query_cmd --filter=leaf&!fixed&".
                             "directive=fragment:true", \@frags, 1);
    query_cast_server($SS_r, "$query_cmd --filter=" .
                             "ancestor=lib.buffer.smr.SMR_BUF_1of", \@smr, 1);

    #my @prims;
    #my $lineage_cmd = "--cell=$SS_r->{GS}{TOP} --task=subcells,refinement_lineage";
    #query_cast_server($SS_r, "$query_cmd --filter=leaf&!fixed&shared=PRIMATIVE", \@prims, 1);


    
    # Parse cells.debug file
    my %all = (
        all => 0.0, fixed => 0.0, sizable => 0.0, custom => 0.0, 
        fragment => 0.0, smr => 0.0, flow => 0.0, other => 0.0
    );
    my %numt = (
        all => 0.0, fixed => 0.0, sizable => 0.0, custom => 0.0, 
        fragment => 0.0, smr => 0.0, flow => 0.0, other => 0.0
    );
    open (CELLS, $file) || command_die($SS_r, "Couldn't read $file.");
    my ($type, $inst, $ave_width, $all_width);
    while (<CELLS>) {
        chomp;
        next if (/^\s*$/);
        if (/^CELL ([^\s]+) \{/) {
            my $cell = $1;
            if (grep {$_ eq $cell} @fixed) {
                $type = "fixed";
            }
            elsif ($cell =~ /^lib\.flow\.sizing/) {
                $type = "flow";
            }           
            elsif (grep {$_ eq $cell} @smr) {
                $type = "smr";
            }
            elsif ($cell =~ /^lib\.sram/ || $cell =~ /^lib\.router/) {
                $type = "custom";
            }
            elsif ($cell =~ /^lib\.util\.operators/ || $cell =~ /^standard\.reset/) {
                $type = "fragment";
            }           
#            elsif (grep {$_ eq $cell} @frags) {
#                $type = "fragment";
#            }
#            elsif ($cell =~ /^lib\.buffer/) {
#                $type = "buffer";
#            }
            else {
                $type = "other";
            }
            debug_print($SS_r, "cell = $cell, type = $type\n");
        }
        elsif (/\s*instances\s*=\s*(\d+)$/) {
            $inst = $1;
        }
        elsif (/\s*all_width\s*=\s*([\d\.]+)u/) {
            $all_width = $1;
        }
        elsif (/\s*avg_width\s*=\s*([\d\.]+)u/) {
            $ave_width = $1;
        }
        elsif (/^\}/) {
            if (defined $type && defined $inst && defined $all_width &&
                defined $ave_width) {
                $all{all} += $inst * $all_width;
                $numt{all} += $inst * $all_width / $ave_width;
                $all{$type} += $inst * $all_width;
                $numt{$type} += $inst * $all_width / $ave_width;
                if ($type ne "fixed" && $type ne "flow") {
                    $all{sizable} += $inst * $all_width;
                    $numt{sizable} += $inst * $all_width / $ave_width;
                }
                $type = undef;
                $inst = undef;
                $all_width = undef;
                $ave_width = undef;
            }
        }
    }
    close CELLS;

    # Calculate averages
    my $ave_r = {};
    my $contrib_r = {};
    foreach $type (keys %all) {
        $ave_r->{$type} = ($numt{$type} != 0.0) ? 
                      sprintf("%.4g", $all{$type} / $numt{$type}) : "-";
        $contrib_r->{$type} =
            sprintf("%6.2f%%",
                    $numt{$type} == 0.0 ? 0.0 : 100*$all{$type}/$all{all});
    }
    return ($ave_r, $contrib_r);
}

# Create some useful additional files based on jauto's output files
sub generate_derived_files {
    my ($dir, $size) = @_;
    colorize_sizing_output("$dir/$size.err", "$dir/$size.html");
    sort_wires_by_rc("$dir/wires.debug", "$dir/wires.rc-sorted");
}

sub colorize_sizing_output {
    my ($src, $dst) = @_;
    if (open(SIZE_ERR, $src) and open(SIZE_HTML, ">$dst")) {
        print SIZE_HTML
            "<html><head><title>Jauto output</title></head><body><pre>\n";
        while (<SIZE_ERR>) {
            s/\&/&amp;/g;
            s/\</&lt;/g;
            s/\>/&gt;/g;
            s/\033\[1\;31m/<font color="red">/g;
            s/\033\[1\;32m/<font color="green">/g;
            # yellow shows up badly on white
            s/\033\[1\;33m/<font color="orange">/g;
            s%\033\[0m%</font>%g;
            print SIZE_HTML $_;
        }
        print SIZE_HTML "</pre></body></html>\n";
        close SIZE_HTML;
        close SIZE_ERR;
    }
}

sub sort_wires_by_rc {
    my ($src, $dst) = @_;
    if (open(WIRES_DEBUG, $src) and open(WIRES_SORTED, ">$dst")) {
        my $skip = 1;
        my %map;
        while (<WIRES_DEBUG>) {
            if (/^CELL SORTED_ALL_NETS \{/) {
                $skip = 0;
            } elsif (!$skip and /^(.*)C\=([\d\.]+)f\s+R\=([\d\.]+)/) {
                my ($line, $c, $r) = ($1, $2, $3);
                my $rc = $r * $c;
                $line .= sprintf("RC=%d", $rc);
                $map{$line} = $rc;
            }
        }
        my $line;
        foreach $line (sort {$map{$b} <=> $map{$a}} keys %map) {
            print WIRES_SORTED $line, "\n";
        }
        close WIRES_DEBUG;
        close WIRES_SORTED;
    }
}

1;
