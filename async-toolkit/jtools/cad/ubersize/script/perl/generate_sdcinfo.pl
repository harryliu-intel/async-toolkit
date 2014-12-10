#!/usr/intel/bin/perl
use strict;
use warnings;
use Getopt::Long;
use File::Temp qw/tempdir/;
use File::Spec::Functions;

my ($cast_dir, $spec_dir, $dfII_dir, $pdk_root, $cdc_fqcn, $tau, $output);
my $debug=0;
my $delay_scale = 50/32;

GetOptions("cast-dir=s"         => \$cast_dir,
           "spec-dir=s"         => \$spec_dir,
           "dfII-dir=s"         => \$dfII_dir,
           "fulcrum-pdk-root=s" => \$pdk_root,
           "cdc-fqcn=s"         => \$cdc_fqcn,
           "tau=i"              => \$tau,
           "delay-scale=f"      => \$delay_scale,
           "output=s"           => \$output,
           "debug"              => \$debug);

if (!defined($cdc_fqcn) || !defined($tau) || !defined($output) ||
    !defined($cast_dir) || !defined($spec_dir) || !defined($dfII_dir) ||
    !defined($pdk_root)) {
    print STDERR "Error: Not all required arguments given.\n";
    print <<EOF;
USAGE: $0 [options]

Required:
    --cast-dir=<dir>         (cast directory)
    --spec-dir=<dir>         (spec directory)
    --dfII-dir=<dir>         (dfII directory)
    --fulcrum-pdk-root=<dir> (location of Fulcrum PDK)
    --cdc-fqcn=<cell>        (fully qualified cell name of a CDC)
    --tau=<int>              (tau in ps)
    --output=<file>          (name of SDC info output file)

Optional:
    --delay-scale=<float>    (scale applied to Jauto delay; default 50/32)
EOF
    exit 1;
}

my $short_cdc;
if ($cdc_fqcn =~ /lib.synchronous.conversion.v3.(SCAN_(?:S2A|A2S)\(\d+\))/) {
    $short_cdc = $1;
} else {
    print STDERR "Error: Unexpected CDC type name: $cdc_fqcn\n";
    exit 2;
}

my $workdir = tempdir(CLEANUP => !$debug);
print "Not deleting temporary working directory: $workdir\n" if $debug;

# setup supersize variables, and write out a list of subcells to generate
# instances files
my $setup = catfile($workdir, 'setup.ss');
open(my $setupfh, ">$setup") || die "Error: Cannot write to $setup: $!";
print $setupfh <<SETUP;
set CAST_DIR=$cast_dir
set SPEC_DIR=$spec_dir
set DFII_DIR=$dfII_dir
set TOP=lib.synchronous.conversion.v3.sizing.EVAL_$short_cdc.10000
set TAU=$tau
set MEM=512M
create_subtypes
resubtype \${TOP} $cdc_fqcn
resubtype \${TOP} lib.buffer.half.MBUF_1of4.1000
resubtype \${TOP} lib.buffer.half.MBUF_1of3.1000
setup_cdswd
query subcells
write_file "$workdir/cells.list" \$query.result
SETUP
close($setupfh);
system("supersize --fulcrum-pdk-root='$pdk_root' --work-dir='$workdir' < '$setup' > '$setup.output' 2> '$setup.err'");

# generate instances files
system("cd '$workdir/cds_wd'; mk_instance_multi --fulcrum-pdk-root='$pdk_root' --view=floorplan --outdir='$workdir/instances' < '$workdir/cells.list' > '$workdir/mk_instance.out' 2> '$workdir/mk_instance.err'");

# run actual sizing evaluation
my $ss = catfile($workdir, 'sdc.ss');
open(my $ssfh, ">$ss") || die "Error: Cannot write to $ss: $!";
my @cdclibs = (
    'lib.synchronous.conversion.v3.SCAN_A2S_CTRL.500',
    'lib.synchronous.conversion.v3.SCAN_S2A_CTRL(0).500',
    'lib.synchronous.conversion.v3.SCAN_S2A_CTRL(1).500',
    'lib.synchronous.conversion.v3.SCAN_S2A_CTRL(2).500',
    'lib.synchronous.conversion.v3.SCAN_S2A_CTRL(3).500',
    'lib.synchronous.conversion.v3.a2s.A2S_CLK_BUF.500',
    'lib.synchronous.conversion.v3.a2s.A2S_LX_ACK_4.500',
    'lib.synchronous.conversion.v3.a2s.A2S_LX_DYB.500',
    'lib.synchronous.conversion.v3.a2s.A2S_SCAN_BIT.500',
    'lib.synchronous.conversion.v3.a2s.A2S_SXR_BIT.500',
    'lib.synchronous.conversion.v3.a2s.A2S_XR_ACK_4.500',
    'lib.synchronous.conversion.v3.s2a.S2A_COMPLETE_ACK_8.500',
    'lib.synchronous.conversion.v3.s2a.S2A_COMPLETE_DYB.500',
    'lib.synchronous.conversion.v3.s2a.S2A_PCFBUF_ACK_8.500',
    'lib.synchronous.conversion.v3.s2a.S2A_PCFBUF_DYB.500',
    'lib.synchronous.conversion.v3.s2a.S2A_SDPU_CTRL.500',
    'lib.synchronous.conversion.v3.s2a.S2A_SFLOP_S_e1of2.500',
    'lib.synchronous.conversion.v3.tree.AC_DETECT4_1of1(false).500',
    'lib.synchronous.conversion.v3.tree.AC_DETECT4_1of1(true).500',
    'lib.synchronous.conversion.v3.tree.AC_DETECT8_1of1(false).500',
    'lib.synchronous.conversion.v3.tree.AC_DETECT8_1of1(true).500',
    'lib.synchronous.conversion.v3.tree.CDC_BUF_1of1.500',
    'lib.synchronous.conversion.v3.tree.CDC_TOKBUF_1of1.500',
    'lib.synchronous.conversion.v3.tree.CLK_INV.500',
    'vendor.avago.svt.gates.av_buf_a2.0',
    'vendor.avago.svt.gates.av_buf_a2.500',
    'vendor.avago.svt.gates.av_buf_a4.0',
    'vendor.avago.svt.gates.av_buf_a4.500',
    'vendor.avago.svt.gates.av_buf_a4.501',
    'vendor.avago.svt.gates.av_inv_a2.0',
    'vendor.avago.svt.gates.av_inv_a2.500',
    'vendor.avago.svt.gates.av_sbuf_b8.0',
    'vendor.avago.svt.gates.av_sbuf_b8.501',
    'vendor.avago.svt.gates.av_sinv_a2.0'
);
my $cdclibs = join(':', @cdclibs);
print $ssfh <<SS;
size --extra-jauto-args "--generateSdcConstraint --disableImmediateCatPathReduction --catPathReductionThreshold=-100000 --sdcDelayScale=$delay_scale --sdcLibs=$cdclibs"
SS
close($ssfh);
system("supersize --fulcrum-pdk-root='$pdk_root' --work-dir='$workdir' < '$ss' > '$ss.output' 2> '$ss.err'");

my $sdc = catfile($workdir, 'sdc.debug');
if (open(my $sdcfh, $sdc)) {
    open(my $ofh, ">$output") || die "Error: Cannot write to $output: $!";
    my $time = localtime;
    my $login = getpwuid($<);
    print $ofh <<HEADER;
# Generated on $time by $login
# $0
# --cast-dir=$cast_dir
# --spec-dir=$spec_dir
# --dfII-dir=$dfII_dir
# --fulcrum-pdk-root=$pdk_root
# --cdc-fqcn=$cdc_fqcn
# --tau=$tau
# --delay-scale=$delay_scale
# --output=$output
HEADER
    print $ofh (<$sdcfh>);
    close($sdcfh);
    close($ofh);
} else {
    print STDERR "Error: No SDC info file generated; see stderr from supersize below:\n";
    if (open(my $efh, "$ss.err")) {
        print STDERR (<$efh>);
        close($efh);
    }
    exit 3;
}
