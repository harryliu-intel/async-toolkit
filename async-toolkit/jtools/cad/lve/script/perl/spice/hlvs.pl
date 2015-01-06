#!/usr/intel/bin/perl -lw
# $Id$
# $DateTime$
# $Author$
use strict;

BEGIN {
    unshift @INC, "$ENV{'FULCRUM_PACKAGE_ROOT'}/lib/perl";
}

use Cwd;
use IPC::Open2;
use Getopt::Long;
use Hercules;


$ENV{SNPSLMD_QUEUE}="true";
my $pwd=getcwd();

my $working_dir = "$pwd";
my $gdsii="";
my $plugWells=0;
my $pin_order_file="";
my $graycell_file="";
my $bit64=0;
my $cdl_file="";
my $cdl_cell_name="";
my $priority=-1;
my $mem="500M";
my $pdk_root="";
my $task;
my $graycell_list="";
my $blackboxHercules=0;
my $grayboxHercules=0;
my $fixbulk=0;
my $doflat=0;
my @validviews=("layout", "layout_pg", "layout_tag", "lvsclean");
my $merge_paths=0;
my $tapeout=0;
my $proteus=0;
my $layoutdir;
my $hlvsmode="flat";
my $threads=2;
my $eplfix=0;
my $erc_mode=0;

sub usage {
    my ($msg) = @_;
    print STDERR "$msg" if defined $msg;
    my $usage  = "Usage: hlvs [args] cell\n";
    $usage .= "  Args includes:\n";
    $usage .= "    GENERAL OPTIONS\n";
    $usage .= "    --gds2-file=[$gdsii]\n";
    $usage .= "    --working-dir=[$working_dir]\n";
    $usage .= "    --64bit=[$bit64]\n";
    $usage .= "    --cdl-file=[$cdl_file]\n";
    $usage .= "    --cdl-cell-name=[$cdl_cell_name]\n";
    $usage .= "    --priority=[$priority]\n";
    $usage .= "    --mem=[$mem]\n";
    $usage .= "    --fulcrum-pdk-root=[$pdk_root]\n";
    $usage .= "    --plug-wells=[$plugWells]\n";
    $usage .= "    --merge-paths=[$merge_paths]\n";
    $usage .= "    --tapeout [$tapeout]\n";
    $usage .= "    --proteus [$proteus]\n";
    $usage .= "    --hlvs-mode=[$hlvsmode]\n";
    $usage .= "    --threads=[$threads]\n";
    $usage .= "    --eplfix=[$eplfix]\n";
    $usage .= "    --erc=[$erc_mode]\n";
    print STDERR "$usage";
    exit 1;
}

my %options = (
   "fixbulk:i" => \$fixbulk,
   "flat:i" => \$doflat,
   "working-dir=s" => \$working_dir,
   "gds2-file=s" => \$gdsii,
   "plug-wells:i" => \$plugWells,
   "gray-cell-list=s" => \$graycell_file,
   "64bit:i" => \$bit64,
   "cdl-file=s" => \$cdl_file,
   "cdl-cell-name=s" => \$cdl_cell_name,
   "threads=i" => \$threads,
   "eplfix=i" => \$eplfix,
   "hlvs-mode=s" => sub {
        if ($_[1] =~ /^black/) {
            $hlvsmode="blackbox";
            $grayboxHercules=0;
            $blackboxHercules=1;
        }
        elsif ($_[1] =~ /^gray/) {
            $hlvsmode="graybox";
            $grayboxHercules=1;
            $blackboxHercules=0;
        }
        else {
            $hlvsmode="";
        }
    },
   "priority=s" => \$priority,
   "mem=s" => \$mem,
   "fulcrum-pdk-root=s" => \$pdk_root,
   "blackbox-hercules:i" => \$blackboxHercules,
   "merge-paths:i" => \$merge_paths,
   "tapeout" => \$tapeout,
   "proteus" => \$proteus,
   "erc" => \$erc_mode,
);

GetOptions( %options ) or usage("Illegal Option");

if ($threads < 1) {
    $threads=1;
}
if ($proteus) {
    $layoutdir=".";
}
elsif (defined ($gdsii) and -s $gdsii and ! defined($layoutdir)) {
    $layoutdir = $gdsii;
    $layoutdir =~ s/\/[^\/]+$//;
}
else {
    $layoutdir = ".";
}
@ARGV == 1 or usage("No cell");
## cell name
my $cell_name = shift;

# Configuration

-d $working_dir ne "" or $working_dir = `mktemp -d /scratch/hlvs.XXXXXX`;
chomp $working_dir;
$cdl_cell_name ne "" or $cdl_cell_name=$cell_name;
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

# get Vdd/GND names
# we will also treat ^$Vdd\S*$ and ^$GND\S*$ as power 
my $GND = $config{GNDNetName};
$GND = "GND" if ! defined $GND;
my $Vdd = $config{VddNetName};
$Vdd = "Vdd" if ! defined $Vdd;

my $graybox_list_file = "$working_dir/graybox_list.txt";
my $longcellnametop = length($cell_name) > 75 ? 1 : 0;
my $topcell=$longcellnametop ? "TOP_CELL" : $cell_name;
my $longcellnamegray=0;
my %graylist=();
my %reversegraylist=();

sub makegdsgraylist {
    my ($graycell_file) = @_;
    my $n=0;
    if ( -s "$graycell_file") {
        if ($tapeout) {
            open (P, "rename --type=cell --from=cast --to=cadence <'$graycell_file' |");
        }
        else {
            open (P, "rename --type=cell --from=cast --to=gds2 <'$graycell_file' |");
        }
        while (<P>) {
            chomp;
            s/\s+/ /g;
            s/^\s//;
            s/\s$//;
            if (length ($_) > 1) {
                $graylist{$_}=$_;
                $reversegraylist{$_}=$_;
                if (length ($_) > 64 && ! $tapeout) {
                    $graylist{$_}=sprintf "SUBCELL_%04d", $n;
                    $reversegraylist{$graylist{$_}} = $_;
                    $longcellnamegray = 1;
                }
                $n++;
            }
        }
        close P;
    }
}

## CREATING DIRECTORY STRUCTURE AND COPYING FILES
system("chmod 2755 \"$working_dir\"");

##########################################################################
#                               Hercules                                 #
##########################################################################

#print "Running Hercules LVS..." if $verbose;
makegdsgraylist($graycell_file);
if( -e "$working_dir/group") {
   my_system("rm -rf '$working_dir/group'");
} 
my_system("mkdir -p '$working_dir'"); 
chdir "$working_dir";
if ($longcellnametop or $longcellnamegray) {
    unlink "cell.gds2";
    open (GIN, "rdgds '$gdsii' |");
    open (GOUT, "| wrgds > cell.gds2");
    while (<GIN>) {
        chomp;
        s/^  *//;
        my ($x,$name)=split;
        if ($x eq "SNAME" or $x eq "STRNAME") {
            $_ = "$x $topcell" if ($longcellnametop and $name eq $cell_name);
            $_ = "$x $graylist{$name}" if ($longcellnamegray and defined ($graylist{$name}));
        }
        print GOUT "$_";
    }
    close GIN;
    close GOUT;
}
else {
    my_system("rm -f cell.gds2; ln -sf '$gdsii' cell.gds2"); 
}

my $equivalence="/* 	EQUIVALENCE = ./equiv */";
my $blackbox_file="/* 	BLACK_BOX_FILE = ./blackbox_file */";
my $doblackorgraybox="no";
my %equiv=();
my %requiv=();
if ($tapeout) {
    $equivalence=" 	EQUIVALENCE = ./equiv";
    if ( -s "../$topcell.bind") {
        open (P, "<../$topcell.bind");
        while (<P>) {
            if (/^C\s+(\S+)\s+(\S+)/) {
                $equiv{$1}=$2;
                $requiv{$2}=$1;
            }
        }
    }
    open (P, "<$cdl_file");
    open (Q, ">$working_dir/equiv") or
        die "CAN'T OPEN $working_dir/equiv for write\n";
    while (<P>) {
        if (/^\.subckt/i) {
            chomp;
            my ($x,$subckt)=split;
            if ($subckt eq $cdl_cell_name) {
                print Q "equiv $subckt=$topcell {}";
            }
            elsif ( ! ($subckt =~ /^(gate|stack)\./) and ($subckt =~ /\./) and length($subckt) > 3) {
                my $lay = $subckt;
                if ($lay =~ /[\(\)\{\}]/) {
                    $lay=`echo "$subckt" | rename --type=cell --from=cast --to=cadence`;
                    chomp $lay;
                    $lay=$subckt if $lay eq "";
                }
                if (defined ($equiv{$lay})) {
                    $lay=$equiv{$lay};
                }
                else {
                    $lay =~ s/[^a-zA-Z0-9_]/_/g;
                    $equiv{$subckt}=$lay;
                }
                print Q "equiv $subckt=$lay {}";
            }
        }
    }
    close P;
    close Q;
}
if(-s $graycell_file and ($blackboxHercules==1 or $grayboxHercules==1)) {
    $doblackorgraybox="yes";
#    if (! $tapeout) {
        open EQUIV_FILE, ">$working_dir/equiv" or die "CAN'T OPEN $working_dir/equiv for write\n";
        my $cdlname=$topcell;
        $cdlname=$requiv{$topcell} if defined $requiv{$topcell};
        print EQUIV_FILE "equiv $cdlname=$topcell {}";
#    }
    if( $blackboxHercules==1){
       open BLACKBOX_FILE, ">$working_dir/blackbox_file" or die "CAN'T OPEN $working_dir/blackbox_file for write\n";
       print BLACKBOX_FILE "LVS = {";
    }
    foreach my $name (sort keys %graylist) {
        $name = $graylist{$name};
        my $gdsname = $name;
        if (defined ($equiv{$name})) {
            $gdsname=$equiv{$name};
        }
        if ($blackboxHercules==1) {
            print EQUIV_FILE "BLACK_BOX $name=$gdsname {remove_layout_ports = {N_* }}";
            print BLACKBOX_FILE " $name=$gdsname ";
        }
        else {
            print EQUIV_FILE "equiv $name=$gdsname {}";
        }
    }
    if( $blackboxHercules==1){
        print BLACKBOX_FILE "}";
        close(BLACKBOX_FILE );
        $blackbox_file=" 	BLACK_BOX_FILE = ./blackbox_file";
    }
    close(EQUIV_FILE);
    $equivalence=" 	EQUIVALENCE = ./equiv";
}

my $conf_dir="$pdk_root/share/Fulcrum/extract";
my $lvs_process= "$conf_dir/hercules/LVS_Hercules.process";
my $lvs_rule_template= "$conf_dir/hercules/LVS_Hercules.rule";
my $lvs_deck= "$working_dir/lvs_deck";
my $lvs_rule= "$working_dir/LVS_Hercules.rule";
my $sch_out= "$working_dir/${cell_name}.sch_out";
my $herc_out = "$working_dir/temp.sch_out";
my_system("ln -sf '$conf_dir/hercules/DFM' DFM");
# this is stupid but hercules cannot read an include filename
# with a ',' in it's path. (AAG)
symlink $cdl_file, "$working_dir/cell.cdl_gds2";
open (X, ">$working_dir/nettran.cdl");
print X "*.SCALE meter";
print X ".include '$working_dir/cell.cdl_gds2'";
close X;

my $macro_models = Hercules::get_device_names($pdk_root);
Hercules::write_nettran_devmap("$working_dir/devMap", $macro_models);

my_system("LD_LIBRARY_PATH= $ENV{ICV_SCRIPT} icv_nettran -sp '$working_dir/nettran.cdl' -sp-slashSpace -sp-devMap '$working_dir/devMap' -logFile nettran.log -outName '$herc_out'");
if ($longcellnametop or $longcellnamegray) {
    open (GIN, "<$herc_out");
    open (GOUT, ">$sch_out");
    while (<GIN>) {
        chomp;
        # extra stuff added by icv_nettran
        s/{TYPE [A-Z]+}//;
        if (/^.cell /) {
            my ($x, $name) = split;
            if ($name eq $cell_name and $longcellnametop) {
                $_ = "$x $topcell";
            }
            elsif (defined $graylist{$name} and $longcellnamegray) {
                $_ = "$x $graylist{$name}";
            }
        }
        elsif (/^.inst /) {
            my ($x,$name) = split(/=/,$_);
            if (defined($graylist{$name}) and $longcellnamegray) {
                $_ = "$x=$graylist{$name}";
            }
        }
        print GOUT "$_";
    }
    close GIN;
    close GOUT;
    unlink $herc_out;
}
else {
    # extra stuff added by icv_nettran
    my_system("sed -e 's/{TYPE [A-Z][A-Z]*}//' '$herc_out' > '$sch_out'");
    unlink $herc_out;
}

open LVS_DECK, ">$lvs_deck" or die "CAN'T OPEN lvs_deck for write\n";
if (open LVS_RULE, "<$lvs_process") { ; # don't worry if this does not exist
    while (<LVS_RULE>) {
        chomp;
        print LVS_DECK;
    }
    close LVS_RULE;
}
open LVS_RULE_TEMPLATE, "<$lvs_rule_template" or die "CAN'T OPEN $lvs_rule_template for read\n";

my $line=<LVS_RULE_TEMPLATE>;
while($line !~/HEADER \{/){
    chomp $line;
    if ($eplfix and ($line =~ /MESSAGE_ERROR/)) {
       print LVS_DECK "        SCHEMATIC_GLOBAL = { GND }";
    }
    else {
       print LVS_DECK $line;
    }
    # syntax for 65nm
    if ($fixbulk and ($line =~ /^\s*EQUATE mos_type src_name/)) {
       print LVS_DECK "        IGNORE = { BULK } \\";
    }
    $line=<LVS_RULE_TEMPLATE>;
}
my $cdlcell=$topcell;
$cdlcell=$cdl_cell_name if (($proteus or $tapeout) and ($cdl_cell_name ne ""));
print LVS_DECK <<END_OF_LVS_HEADER;
HEADER {
INLIB = cell.gds2
OUTLIB = EV_OUT
BLOCK = $topcell
GROUP_DIR = group
FORMAT = GDSII
OUTPUT_FORMAT = GDSII
OUTPUT_LAYOUT_PATH = .
SCHEMATIC = $sch_out
SCHEMATIC_FORMAT = HERCULES
SCHEMATIC_TOP_BLOCK = $cdlcell
$equivalence
$blackbox_file
}
END_OF_LVS_HEADER

while(<LVS_RULE_TEMPLATE> !~/\}/){};
my $deletedone=0;
$deletedone=1 if ($erc_mode);
while($line=<LVS_RULE_TEMPLATE>){
   if(! $deletedone && ($line =~ /DECK_TYPE=="LVS_DECK"\s*\|\|\s*CROSS_REFERENCE=="yes"/)) {
      while($line=<LVS_RULE_TEMPLATE>) {
        last if $line =~ /SPICE.*PRECISION/;
      }
      $deletedone=1;
      next;
   }
   chomp $line;
   if ($line =~ /MERGE_PATHS *=/) {
      $line =~ s/MERGE_PATHS=.*/MERGE_PATHS=TRUE/
        if ($merge_paths);
      $line =~ s/MERGE_PATHS=.*/MERGE_PATHS=FALSE/
        if (! $merge_paths);
   }
   if ($eplfix and ($line =~ /MESSAGE_ERROR/)) {
      print LVS_DECK "        SCHEMATIC_GLOBAL = { GND }";
   }
   else {
      print LVS_DECK $line;
   }
   # syntax for 130nm
   if ($fixbulk and ($line =~ /^ *EQUATE [NP]MOS/)) {
      print LVS_DECK "        IGNORE = { BULK } \\";
   }
   # syntax for 65nm
   if ($fixbulk and ($line =~ /^\s*EQUATE mos_type src_name/)) {
      print LVS_DECK "        IGNORE = { BULK } \\";
   }
}

close(LVS_DECK);
close(LVS_RULE_TEMPLATE);

my $DECK_TYPE="RC_DECK";
$DECK_TYPE = "LVS_DECK" if $erc_mode;
my $exarg="";
$exarg = "-ex" if $erc_mode;
my $npcarg="";
$npcarg="NPC_IGNORE_LOCAL_TEXT=";
my_system("checklic -w herc; $npcarg AVANTI_MULTI_BIG=FALSE LD_LIBRARY_PATH= DECK_TYPE=$DECK_TYPE BLACKBOX_MODE=yes $ENV{HERC_SCRIPT} hercules $exarg -threads $threads -lvs-license $lvs_deck > hercules.log");

if( -e "$topcell.LVS_ERRORS" ){ 

  open LVS_ERRORS, "<$topcell.LVS_ERRORS";
  my $Result=1;
  while($line=<LVS_ERRORS>)  {
    chomp $line;
    if($line=~/TOP BLOCK COMPARE RESULTS: PASS/i or $line=~/Top block compare result: PASS/i){ $Result=0; }
  }
  close LVS_ERRORS;
  my_system ("more run_details/compare/*/sum.* | cat > '$layoutdir/hlvs.compare'");
  my_system ("parse_hlvs '$layoutdir/hlvs.compare'");
  if($Result == 0){  
    print "HERCULES/NVN SUCCESS!";
  } else {
    # note that the string 'NVN FAILED' must appear for lve
    print "HERCULES/NVN FAILED: schematic and layout does not match.";
    die "Error: HERCULES/NVN FAILED: schematic and layout does not match for $topcell\n";
  }

} else {
  die "Error: HERCULES/NVN FAILED: fail to extract layout for $topcell.\n";
}

############################### SUBROUTINES GO HERE ######################

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
