#!/usr/intel/bin/perl -lw
# $Id$
# $DateTime$
# $Author$
use strict;

use Cwd;
use IPC::Open2;
use Getopt::Long;


$ENV{SNPSLMD_QUEUE}="true";
my $pwd=getcwd();

my $working_dir = "$pwd";
my $gdsii="";
my $bit64=0;
my $priority=0;
my $mem="4G";
my $pdk_root="";
my $tapeout=0;
my $proteus=0;
my $full_chip=0;
my $qsub_extras="";
my $hdrc_density=0;
my $threads=2;
my $verbose=0;
my $batch=0;
my $mmonitor=0;

sub usage {
    my ($msg) = @_;
    print STDERR "$msg" if defined $msg;
    my $usage  = "Usage: hdrc [args] topcell\n";
    $usage .= "  Args includes:\n";
    $usage .= "    GENERAL OPTIONS\n";
    $usage .= "    --batch [$batch]\n";
    $usage .= "    --density [$hdrc_density]\n";
    $usage .= "    --fulcrum-pdk-root=[$pdk_root]\n";
    $usage .= "    --full-chip [$full_chip]\n";
    $usage .= "    --gds2-file=[$gdsii]\n";
    $usage .= "    --mem=[$mem]\n";
    $usage .= "    --mmonitor=[$mmonitor]\n";
    $usage .= "    --priority=[$priority]\n";
    $usage .= "    --qsub-extras [$qsub_extras]\n";
    $usage .= "    --tapeout [$tapeout]\n";
    $usage .= "    --threads [$threads]\n";
    $usage .= "    --verbose [$verbose]\n";
    $usage .= "    --working-dir=[$working_dir]\n";
    print STDERR "$usage";
    exit 1;
}

my %options = (
   "working-dir=s" => \$working_dir,
   "gds2-file=s" => \$gdsii,
   "priority=s" => \$priority,
   "mem=s" => \$mem,
   "mmonitor" => \$mmonitor,
   "fulcrum-pdk-root=s" => \$pdk_root,
   "full-chip" => \$full_chip,
   "qsub-extras=s" => \$qsub_extras,
   "density" => \$hdrc_density,
   "threads=i" => \$threads,
   "verbose" => \$verbose,
   "batch" => \$batch,
);

GetOptions( %options ) or usage("Illegal Option");

if ($threads < 1) {
    $threads=1;
}
usage if (! defined ($gdsii) or ! -s $gdsii);
my $cell_name;
if (! @ARGV) {
    # find top cell
    if (my $pid =open2( \*RD, \*WR, "aaggds '$gdsii' 2>\&1")) {
        print STDERR "Finding top cell" if $verbose;
        print WR "t";
        $cell_name=<RD>;
        chomp $cell_name;
        $cell_name =~ s/.* //;
        print WR "q";
        close WR;
        close RD;
        kill 9, $pid;
        waitpid $pid, 0;
        if ($cell_name eq "0000") {
            print STDERR "Gds is too large to find top cell on a 32 bit system.";
            exit (1);
        }
        print STDERR "Cell Name $cell_name" if $verbose;
    }
    else {
        usage ("Cannot find top cell");
    }
}
else {
    $cell_name=shift;
}

# Configuration

-d $working_dir ne "" or $working_dir = ".";
chomp $working_dir;
$pdk_root="$ENV{FULCRUM_PDK_ROOT}" if ( ! ( -d $pdk_root ) and -d $ENV{FULCRUM_PDK_ROOT});
-d $pdk_root or usage("fulcrum-pdk-root improperly defined");
my $escaped_cell_name="\Q$cell_name\E";

## Reading the pdk.config
my %config;
open CONFIG, "<$pdk_root/share/Fulcrum/pdk.config";
while(my $line=<CONFIG>)  {
  chomp $line;
  my @fields=split/\=/,$line;
  chomp($fields[1]);
  $config{$fields[0]}=$fields[1];
}
close(CONFIG);

my $longcellnametop = length($cell_name) > 75 ? 1 : 0;
my $topcell=$longcellnametop ? "TOP_CELL" : $cell_name;

## CREATING DIRECTORY STRUCTURE AND COPYING FILES
system("chmod 2775 \"$working_dir\"");

##########################################################################
#                               Hercules                                 #
##########################################################################

if( -e "$working_dir/group") {
   my_system("rm -rf '$working_dir/group'");
} 
if( -e "$working_dir/run_details") {
   my_system("rm -rf '$working_dir/run_details'");
} 
my_system("mkdir -p '$working_dir'"); 
chdir "$working_dir";
if ($longcellnametop) {
    unlink "cell.gds2";
    open (GIN, "rdgds '$gdsii' |");
    open (GOUT, "| wrgds > cell.gds2");
    while (<GIN>) {
        chomp;
        s/^  *//;
        my ($x,$name)=split;
        if ($x eq "SNAME" or $x eq "STRNAME") {
            $_ = "$x $topcell" if ($longcellnametop and $name eq $cell_name);
        }
        print GOUT "$_";
    }
    close GIN;
    close GOUT;
}
else {
    my_system("rm -f cell.gds2; ln -sf '$gdsii' cell.gds2"); 
}

my $conf_dir="$pdk_root/share/Fulcrum/hdrc";
my $hdrc_rul= "$conf_dir/hdrc.rul";
$priority *= 100;

if ($batch) {
    print STDERR "Submitting job" if $verbose;
}
else {
    print STDERR "Running job" if $verbose;
}
my $sync="";
$sync="-sync y" if ! $batch;
open (P, "| qsub $sync -p $priority -l mem=$mem,hdrc=1,cc=$threads -cwd -N hdrc $qsub_extras");
print P "#!/bin/bash";
if ($mmonitor) {
    print P "/p/rrc/tools/bin/fulcrum mmonitor &";
}
print P "LD_LIBRARY_PATH= DENSITY_FOR_NONFULLCHIP=$hdrc_density FULL_CHIP=$full_chip $ENV{HERC_SCRIPT} hercules -threads $threads -b $topcell -i 'cell.gds2' '$hdrc_rul'";
if ($mmonitor) {
    print P "pkill -HUP -s 0 mmonitor";
    print P "pkill -s 0 mmonitor";
}
close P;

if ($mmonitor) {
    print STDERR "Look for line with VSZ in the hdrc grid log files for memory info.";
}
# execute a command, taking care of exit status
sub my_system {
    my ($cmd)=@_;
    my @cmdlist=@_;
    if ($#cmdlist > 0) {
    my $status = system(@_);
    $status == 0 or die "ERROR: @_ failed.\n";
    }
    else {
    my $status = system($cmd);
    $status == 0 or die "ERROR: $cmd failed.\n";
    }
}
