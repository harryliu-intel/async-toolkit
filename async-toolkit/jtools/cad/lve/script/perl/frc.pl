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
my $proteus=0;
my $full_chip=0;
my $qsub_extras="";
my $hdrc_density=0;
my $threads=2;
my $verbose=0;
my $mmonitor=0;
my $topcell="";
my $dfIIdir="";
my $view="";

sub usage {
    my ($msg) = @_;
    print STDERR "$msg" if defined $msg;
    my $usage  = "Usage: frc [args] topcell\n";
    $usage .= "  Args includes:\n";
    $usage .= "    GENERAL OPTIONS\n";
    $usage .= "    --dfII-dir=[$dfIIdir]\n";
    $usage .= "    --fulcrum-pdk-root=[$pdk_root]\n";
    $usage .= "    --gds2-file=[$gdsii]\n";
    $usage .= "    --mem=[$mem]\n";
    $usage .= "    --mmonitor=[$mmonitor]\n";
    $usage .= "    --priority=[$priority]\n";
    $usage .= "    --qsub-extras [$qsub_extras]\n";
    $usage .= "    --threads [$threads]\n";
    $usage .= "    --verbose [$verbose]\n";
    $usage .= "    --view [$view]\n";
    $usage .= "    --working-dir=[$working_dir]\n";
    print STDERR "$usage";
    print STDERR "Topcell=$topcell";
    exit 1;
}

my %options = (
   "working-dir=s" => \$working_dir,
   "gds2-file=s" => \$gdsii,
   "dfII-dir=s" => \$dfIIdir,
   "priority=s" => \$priority,
   "mem=s" => \$mem,
   "mmonitor" => \$mmonitor,
   "fulcrum-pdk-root=s" => \$pdk_root,
   "qsub-extras=s" => \$qsub_extras,
   "threads=i" => \$threads,
   "verbose" => \$verbose,
   "view=s" => \$view,
);

GetOptions( %options ) or usage("Illegal Option");

if ($threads < 1) {
    $threads=1;
}
usage("gds2-file not defined or invalid!") if ! defined ($gdsii) or ! -s $gdsii;
usage("dfII-dir is not valid or undefined!") if ( ! -d $dfIIdir or ! -s "$dfIIdir/cds.lib.generated" );
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

my $dohercules=0;
my $doskill=0;

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
$topcell=$longcellnametop ? "TOP_CELL" : $cell_name;
$dohercules = 1 if -s "$pdk_root/share/Fulcrum/frc/frc.rul";
$doskill = 1 if -s "$pdk_root/share/Fulcrum/frc/frc.il";

## CREATING DIRECTORY STRUCTURE AND COPYING FILES
my_system("mkdir -p '$working_dir'"); 
system("chmod 2775 \"$working_dir\"");

##########################################################################
#                               Hercules                                 #
##########################################################################
if ( $dohercules ) {

    if( -e "$working_dir/group") {
       my_system("rm -rf '$working_dir/group'");
    } 
    if( -e "$working_dir/run_details") {
       my_system("rm -rf '$working_dir/run_details'");
    } 
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

    my $hdrc_rul= "$pdk_root/share/Fulcrum/frc/frc.rul";
    $priority *= 100;

    #if ($mmonitor) {
    #    `mmonitor &`;
    #}
    my_system("LD_LIBRARY_PATH= $ENV{ICV_SCRIPT} icv -c $topcell -i 'cell.gds2' -f GDSII '$hdrc_rul'");
    if ($topcell ne $cell_name and -e "$topcell.LAYOUT_ERRORS") {
        rename ("$topcell.LAYOUT_ERRORS", "$cell_name.LAYOUT_ERRORS");
    }
    #if ($mmonitor) {
    #    `pkill -HUP -s 0 mmonitor; pkill -s 0 mmonitor`;
    #}
}
if ($doskill or 1) {
    chdir $working_dir;
    my $ctopcell=`echo '$cell_name' | rename --type=cell --from=gds2 --to=cadence`;
    chomp $ctopcell;
    my @cellhier=split(/\./, $ctopcell);
    pop @cellhier;
    pop @cellhier;
    my $clib=join(".", @cellhier);
    my $celldir=$clib;
    $celldir =~ s/\./\//g;
    $celldir="$dfIIdir/$celldir";
    my $dname=$ctopcell;
    $dname =~ s/\./#2e/g;
    $dname =~ s/-/#2d/g;
    $celldir="$celldir/$dname";
    $view .= "_tag"
        if -s "$celldir/${view}_tag/layout.oa";
    open FRC, ">frcrun.il";
    print FRC "(load \"$pdk_root/share/Fulcrum/frc/frc.il\" )";
    print FRC "(load \"$pdk_root/share/Fulcrum/pdkinfo.il\" )";
    print FRC "(runFrc \"$clib\" \"$ctopcell\" \"$view\" )";
    close FRC;

    my_system("mkcdswd --dfII-dir='$dfIIdir' --fulcrum-pdk-root='$pdk_root' --target-dir='$working_dir' --force --temp");
    my_system("DISPLAY= virtuoso -nograph -replay 'frcrun.il' -log skill.log </dev/null >/dev/null");
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
