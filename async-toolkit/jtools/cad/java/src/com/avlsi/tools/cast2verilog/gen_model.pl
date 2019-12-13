#!/usr/intel/bin/perl
use strict;
use warnings;
use Getopt::Long;
use Pod::Usage;
use File::Spec::Functions qw/:ALL/;
use FindBin;
use Cwd qw/abs_path/;

my ($cast_path, $spar_dir, $cell, $env, $cosim, @cast_defines, $beh, $fpga_path, $help);
my $width = 300;
GetOptions("cast-path=s" => \$cast_path,
           "spar-dir=s"  => \$spar_dir,
           "cell=s"      => \$cell,
           "env=s"       => \$env, 
           "cosim=s"     => \$cosim,
           "width=i"     => \$width,
           "fpga-path=s" => \$fpga_path,
           "defines=s"   => \@cast_defines,
           "beh!"        => \$beh,
           "help!"       => \$help) || pod2usage(2);

pod2usage(-verbose => 1) if $help;

if (defined($cell) and defined($env) and !defined($cosim)) {
    $cosim = "$cell\{verilog.gate\}:$env" unless $cosim
}
pod2usage("Either --cosim, or --cell and --env must be specified") unless $cosim;

my @netlists = map { abs_path($_) } @ARGV;
pod2usage("No Verilog specified") unless @netlists or $beh;

if (!defined($cast_path)) {
    my $hw_dir = catdir($FindBin::Bin, updir(), updir(), updir());
    $cast_path = join(':', abs_path(catdir($hw_dir, 'cast')),
                           abs_path(catdir($hw_dir, 'spec')));
}

my $runvcs = "run-vcs";

my $flist = "testbench.f";

my $cmd = ("cast2verilog --cast-path=\Q$cast_path\E " .
           "--register-width=$width " .
           "--max-heap-size=16G --generate-testbench --enable-orphan-bdc --ifdef " .
           "--structure-declarations=structs.txt " .
           "--cell=\Q$cosim\E " .
           "--file-list=$flist " .
           join('', map { "--define=\Q$_\E " } @cast_defines) .
           "--output-file=testbench.v 2> testbench.err");

system($cmd) == 0 || die "Error executing cast2verilog: $!";

my $instdir = $ENV{'FULCRUM_PACKAGE_ROOT'};

my $vcfg = $beh ? 'beh.vcfg' : 'fpga.vcfg';

my @defines = ();

push @defines, "+define+FPGA_HIER_PATH=.\Q$fpga_path\E." if $fpga_path;

if (-s $flist) {
    $flist = "-file $flist";
} else {
    $flist = '';
}

open my $fh, ">$runvcs" || die "Can't open $runvcs: $!";
print $fh <<EOF;
export SPAR="$spar_dir"
export COLLATERAL=/nfs/sc/proj/ctg/mrl108/mrl/collateral
export CAST2VERILOG_RUNTIME="$instdir/share/cast2verilog"
vcs -licqueue -debug_access+dmptf+all -debug_region=lib+cell -full64 @defines -file "\$CAST2VERILOG_RUNTIME/$vcfg" testbench.v $flist @netlists
EOF
close $fh;
chmod 0755, $runvcs;

__END__
=head1 NAME

gen_model.pl - Create collateral for VCS simulation from fpga.vg

=head1 SYNOPSIS

gen_model.pl [options] [verilog files...]

 Options:
   --cast-path     Specify the CAST path
   --cell          Name of the CAST cell
   --env           Name of the CAST environment
   --cosim         Or, specify a cosim spec instead of --cell and --env
   --width         Specify the maximum bitwidth for temporary variables
   --fpga-path     Specify the instance path of the FPGA relative to the top
   --define        Override CAST variables; can be specified any number of times
   --beh           If specified, generate a behavior model
=cut
