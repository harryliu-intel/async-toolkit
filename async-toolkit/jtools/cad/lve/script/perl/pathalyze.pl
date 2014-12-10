#!/usr/bin/perl -w

################################### Setup ###################################

my %VARS;

select STDERR; $|=1;
select STDOUT; $|=1;

my $package_root;
my $java_package_root;
BEGIN {
    $package_root = $0;
    my $exe = $package_root;
    $exe =~ s:.*/::;
    if (! ($package_root =~ m:^/:)) {
        my $pwd = `pwd`;
        chomp $pwd;
        $package_root = $pwd;
        $package_root .= "/$0";
        $package_root =~ s:$exe$::;
        $package_root =~ s://:/:g;
        chdir $package_root;
        $package_root = `pwd`;
        chomp $package_root;
        chdir $pwd;
    }
    else {
        $package_root =~ s:/bin/$exe::;
    }
    $java_package_root = $package_root;
    push @INC, "$package_root/lib/perl";
}

use strict;
use Pathalyze;

#
# Program usage
#

sub usage_exit {
    print STDERR << "USAGE";
Usage: pathalyze
 REQUIRED ARGUMENTS
  --lve-dir=<lve>[:<lve2>:...]       LVE results directory (can be a list of
                                     directories to search.)
  <pathspec>                         Path timing analysis specification file.

 GENERAL ARGUMENTS
  [--root-subtype=<root>]            Cell to which nodes in <pathspec> are
                                     referenced (unless overridden with 'cell'
                                     statement.)
  [--spec-dir=<spec>]                Layout subtype spec directory.
                                     (Default is ~/hw/layout/tsmc13/spec.)
  [--cast-dir=<spec>]                Cast base cell directory.
                                     (Default is ~/hw/cast.)
  [--path-dir=<dir>]                 Directory to output path delay info.
  [--progress-report-rate=<num>]     Report progress every <num> paths
                                     processed.  Default is 0, i.e. disabled.
  [--lve-cap-dir=<capdir>            Root directory containing lve directories
   --output-caps=<cap1>,<cap2>,...]  named "cap1", "cap2", etc., corresponding
                                     to cap sweep alint results of cell output
                                     transitions.
  [--max-heap-size=<mem>]            Max heap size to pass to the JVM.
  [--verbose]                        Verbose output.

 LVE RESULTS PROCESSING ARGUMENTS
  [--no-sweep-data]                  Specify this when input slew rate sweeping
                                     has not been done.  Delay results will be
                                     of questionable accuracy.
  [--view=<view>]                    DFII view to use (Default is layout).
  [--mode=<mode>]                    Use results from extracted vs estimated 
                                     mode.  (Default is extracted.)
  [--corner=<corner>]                Specifies process corner (Default: ss).
                                     Must be one of either tt, ff, or ss.
  [--temp=<temp>]                    Specifies temperature. Default is 
                                     dependent on process corner: 
                                     ss=125, tt=90, ff=-40.
  [--voltage=<volts>]                Specifies voltage.  Default is dependent 
                                     on process corner: ss=0.9, tt=0.9, ff=1.2.
  [--flat]                           Do pathalyze in flat mode (assumes all 
                                     nodes have been alinted from the top-level 
                                     cell).
  [--routed]                         Do pathalyze in routed mode (determines 
                                     node locality based on the routed 
                                     directives, also assumes alint was tested 
                                     according to this hierarchy.)

 DELAY CALCULATION ARGUMENTS
  [--input-slew=<slew> |             Use specified input slew rate(s).  If more
                <s1>,<s2>,... ]      than one is specified, then all paths
                                     and parameters are calculated over the
                                     entire set of slew rates.
                                     Default slew rate is 30 ps.
  [--no-pin-to-pin]                  Don't attempt to model individual 
                                     pin-to-pin delays.  This can cause a huge
                                     impact on accuracy.
  [--conservative-extrapolation=0|1] Controls how delay and slew rates are
                                     extrapolated when an input slew rate fall 
                                     outside of the characterized range.  
                                     Default is 1
  [--extrapolation-warn-limits=      Warn about extrapolations more than s_lo
                <s_lo>,<s_hi>        lower than the smallest slew point or
                                     s_hi higher than the largest slew point.
                                     Defaults are s_lo=0.0, s_hi=0.0.

USAGE
    exit 1;
}

#
# Variables, Configuration
#

%VARS = default_vars($java_package_root);
# override default package name
if ($package_root =~ m:/([^/]+)/[^/]+$:) {
    $VARS{PACKAGE_NAME} = $1;
}
elsif ($package_root =~ m:/([^/]+)/[^/]+/$:) {
    $VARS{PACKAGE_NAME} = $1;
}

my @OK_CORNERS      = ( "ss", "tt", "ff", "sf", "fs" ),
my $DEBUG           = 0;
my $pathspec_file   = "";
my $path_dir        = "";

#
# Command-line parsing
#

my $last_dir;

while (@ARGV) {
    if ($ARGV[0] =~ /^--flat$/) {
        if ($VARS{ALINT_MODE} eq "routed") {
            die "Error: --flat and --routed are mutually exclusive.\n";
        }
        $VARS{ALINT_MODE} = "flat";
    }
    elsif ($ARGV[0] =~ /^--routed$/) {
        if ($VARS{ALINT_MODE} eq "flat") {
            die "Error: --flat and --routed are mutually exclusive.\n";
        }
        $VARS{ALINT_MODE} = "routed";
    }
    elsif ($ARGV[0] =~ /^--root-subtype=(.*)$/) {
        $VARS{ROOT_SUBTYPE} = $1;
    }
    elsif ($ARGV[0] =~ /^--spec-dir=(.*)$/) {
        $VARS{SPEC_DIR} = $1;
    }
    elsif ($ARGV[0] =~ /^--cast-dir=(.*)$/) {
        $VARS{CAST_DIR} = $1;
    }
    elsif ($ARGV[0] =~ /^--lve-dir=(.*)$/) {
        push @{$VARS{LVE_DIRS}}, split /:/, $1;
    }
    elsif ($ARGV[0] =~ /^--progress-report-rate=(.*)$/) {
        $VARS{PATH_REPORT_RATE} = $1;
    }
    elsif ($ARGV[0] eq "--no-sweep-data") {
        $VARS{NO_SWEEPS} = 1;
    }
    elsif ($ARGV[0] =~ /^--view=(.*)$/) {
        $VARS{VIEW} = $1;
    }
    elsif ($ARGV[0] =~ /^--mode=(.*)$/) {
        $VARS{MODE} = $1;
        die "Unrecognized mode $1.\n" 
            if ($1 ne "extracted" && $1 ne "estimated");
    }
    elsif ($ARGV[0] =~ /^--corner=(.*)$/) {
        $VARS{CORNER} = $1;
        die "Unrecognized corner $1.\n" 
            if (! grep {$1 eq $_} @OK_CORNERS);
    }
    elsif ($ARGV[0] =~ /^--temp=(.*)$/) {
        $VARS{TEMP} = $1;
        die "Must supply a numeric value to --temp.\n"
            if ($VARS{TEMP} !~ /^-?[\d\.]+$/);
    }
    elsif ($ARGV[0] =~ /^--voltage=(.*)$/) {
        $VARS{VOLTAGE} = $1;
        die "Must supply a numeric value to --voltage.\n"
            if ($VARS{VOLTAGE} !~ /^[\d\.]+$/);
    }
    elsif ($ARGV[0] =~ /^--slew/) {
        print "Note: --slew argument is now deprecated (on by default).\n";
    }
    elsif ($ARGV[0] =~ /^--input-slew=(.*)$/) {
        my $slew_arg = $1;
        my @slews = split /,/, $1;
        $VARS{IN_SLEWS} = \@slews;
        foreach my $slew (@slews) {
            if ($slew !~ /^\d+(\.\d+)?$/) {
                die "Specify --input-slew in picoseconds.\n"
            }
        }
    }
    elsif ($ARGV[0] =~ /^--use-partial-extract=(.*)$/) {
        die "Sorry: --use-partial-extract is now unsupported.\n";
    }
    elsif ($ARGV[0] =~ /^--hierarchy-separator=(.*)$/) {
        die "Sorry: --hierarchy-separator is now unsupported (use '.').\n";
    }
    elsif ($ARGV[0] =~ /^--path-dir=(.*)$/) {
        print STDERR "Warning: --path-dir is deprecated. Use --output-dir.\n";
    }
    elsif ($ARGV[0] =~ /^--output-dir=(.*)$/) {
        $VARS{OUTPUT_DIR} = $1;
        `mkdir -p '$VARS{OUTPUT_DIR}'` if (!-e $VARS{OUTPUT_DIR});
    }
    elsif ($ARGV[0] =~ /^--lve-cap-dir=(.*)$/) {
        $VARS{LVE_CAP_DIR} = $1;
    }
    elsif ($ARGV[0] =~ /^--output-caps=(.*)$/) {
        my @caps = split /,/, $1;
        foreach my $cap (@caps) {
            if ($cap !~ /^\d\.\d+[eE]-\d+$/ || $cap > 1e-6) {
                die "Error: Bad --output-caps list specified.\n" .
                    "Values must be of the form e.g. 0.08532e-12 (Farads).\n";
            }
        }
        $VARS{OUT_CAPS} = \@caps;
    }
    elsif ($ARGV[0] eq "--no-pin-to-pin") {
        $VARS{PIN_TO_PIN} = 0;
    }
    elsif ($ARGV[0] =~ /--conservative-extrapolation=(.*)$/) {
        if ($1 eq "0" || $1 eq "1") {
            $VARS{CONSERVATIVE} = $1;
        }
        else {
            die "Invalid argument to --conservative-extrapolation.\n";
        }
    }
    elsif ($ARGV[0] =~ /^--extrapolation-warn-limits=([^,]+),(.*)$/) {
        $VARS{SLEW_WARN_LO} = $1;
        $VARS{SLEW_WARN_HI} = $2;
    }
    elsif ($ARGV[0] =~ /^--max-heap-size=(.*)$/) {
        $VARS{HEAP_SIZE} = $1;
    }
    elsif ($ARGV[0] eq "--verbose") {
        $VARS{VERBOSE} = 1;
    }
    elsif ($ARGV[0] eq "--debug") {
        $DEBUG = 1;
        $VARS{DEBUG} = 1;
    }
    elsif ($ARGV[0] =~ /^--define=(.*)$/) {
        push @{$VARS{CAST_DEFINES}}, $1;
    }
    elsif ($ARGV[0] =~ /^--package-root=(.*)$/) {
        $VARS{PACKAGE_ROOT} = $1;
    }
    elsif ($ARGV[0] =~ /^--package-name=(.*)$/) {
        $VARS{PACKAGE_NAME} = $1;
    }
    elsif ($ARGV[0] !~ /^--/) {
        usage_exit() if ($pathspec_file ne "");
        $pathspec_file = $ARGV[0];
    }
    elsif ($ARGV[0] eq "--help") {
        usage_exit();
    }
    else {
        print STDERR "Unrecognized argument '$ARGV[0]'.\n";
        usage_exit();
    }
    shift;
}

# Check for required arguments
usage_exit() if (!@{$VARS{LVE_DIRS}} || $pathspec_file eq "");

#
# Parse all paths & params, measure all paths, calculate and report
# all params.
#
my $P_r = pathalyze(\%VARS, $pathspec_file);

