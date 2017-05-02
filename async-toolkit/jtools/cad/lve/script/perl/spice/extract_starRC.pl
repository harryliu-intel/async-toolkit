#!/usr/intel/bin/perl -w
# $Id$
# $DateTime$
# $Author$
use strict;

BEGIN {
    unshift @INC, "$ENV{'FULCRUM_PACKAGE_ROOT'}/lib/perl";
}

my $doc = <<DOC;

<h2> Summary </h2>

<p> Extracts gds2 to a flat full-dspf spice file, using the <cadence sev>-simplex tools</a> to implement <flow extraction>.  This script is used by <tool lve> to make spice files which are then converted to aspice files with <tool rc_spice2aspice> and simulated with aspice. </p>

<h2> Description </h2>
See <a href=http://internal/eng/depot/sw/cad/doc/flow/ExtractionFlow.pdf>a flow chart</a>.
<p> The primary inputs are .gds2, a .cdl file, and a cellname.  XTC reads in the gds2.  NVN converts structure names to the names as in the cdl file by doing netlist cross-referencing.  FIRE/ICE do the R/C extraction and DISTRC/RAIn put them back together in a spice file with the cross-referenced (cdl) names.</p>

<h2> Implementation </h2>
<dl>

<dt> nvn
<dd> <p> Simplex can't reliably figure out proper names for internal nets on its own.  This means just naively going through their tool chain will result in internal net names in the spice file that don't match the schematic.  This of course causes problems if we want to simulate specific internal nets.</p>
<p> We resolve this by running <cadence nvn> and look at the .cxl file which maps net names in standard bind.rul format.  After mucking with the file format, rain then takes the file as a back annotated node list(banode file) and converts the names when outputting the spice file.  </p>

<dt> licenses
<dd> We set the CADENCE_SEV_LICENSE_OPTIONS=-wait in order to wait for licences instead of dying awkwardly.

</dl>
DOC

use Cwd;
use LveUtil qw /reName2 read_cdl_aliases/;
use Hercules;


$ENV{CADENCE_SEV_LICENSE_OPTIONS}='-wait';
my $pwd=getcwd();

#@pids = ();

#$SIG{CHLD} = sub { wait };

sub usage() {
    my $usage  = "USAGE: extract  [args] cell\n";
    $usage .= "  Args includes:\n";
    $usage .= "    GENERAL OPTIONS\n";
    $usage .= "    --root-target-dir=<path>\n";
    $usage .= "    --current-target-dir=<path>\n";
    $usage .= "    --gds2-file=<file_name>\n";
    $usage .= "    --working-dir=<path> (default /scratch/tmp.extract.cellName.$$)\n";
    $usage .= "    --delete=(0|1) (default 1)\n";
    $usage .= "    --64bit=(0|1) (default 0)\n";
    $usage .= "    --cdl-file=<file name>\n";
    $usage .= "    --cdl-cell-name=<cell> (defaults to cell)\n";
    $usage .= "    --alias-file=<file name> (CDL aliases, for SPEF canonicalization)\n";
    $usage .= "    --routed-alias-file=<file name> (CDL aliases, for SPEF canonicalization)\n";
    $usage .= "    --spice-topcell=<file name> default(`pwd`/cellname.top)\n";
    $usage .= "    --spice-target=<file name> default(`pwd`/cellname.spice)\n";
    $usage .= "    --node-props=<node-prop>\n";
    $usage .= "    --extract-power=[NO|YES|DEVICE_LAYERS] (default NO)\n";
    $usage .= "    --extracted-view=[]\n";
    $usage .= "    --extract-corner=[name] (default typical)\n";
    $usage .= "    --minR=value (default 0.1)\n";
    $usage .= "    --minC=value (default 0)\n";
    $usage .= "    --minCC=value (default 0)\n";
    $usage .= "    --maxF=value (default 1e12)\n";
    $usage .= "    --fulcrum-pdk-root=<path name> default()\n\n";
    $usage .= "    --flat (do flat extract, not really used)\n";
    $usage .= "    --dfII-dir=value (the dfII dir, used to find if subcells exist)\n";
    $usage .= "    HERCULES OPTIONS\n";
    $usage .= "    --fixbulk (extract ignorning missing well plugs\n";
    $usage .= "    ICV OPTIONS\n";
    $usage .= "    --threads=value (how many threads for ICV, defaults to 2)\n";
    $usage .= "    --extra-extract-equiv=value (file with list of extra equiv cast cells)\n";
    $usage .= "    --starRC1-extra-options=value (extra icv command options)\n";
    $usage .= "    STARRC OPTIONS\n";
    $usage .= "    --temperature=value (default 90C)\n";
    $usage .= "    --plug-wells=(0|1) (default 1)\n";
    $usage .= "    --pin-order-file=<filename with .SUBCKT cellname [ports] .ENDS> where defining the order of port list\n";
    $usage .= "    --gray-cell-list=<file name>\n";
    $usage .= "    --blackbox=(0|1) (defaults to 0)\n";
    $usage .= "    --extractReduce=[YES|NO|LAYER|NO_EXTRA_LOOPS|HIGH]\n";
    $usage .= "    --spef=[0|1] generate a spef instead of spice\n";
    $usage .= "    --nt-mode=[0|1] generate files for Nanotime\n";
    $usage .= "    --totem-mode=[0|1] for totem prep\n";
    $usage .= "    --instance-port=[CONDUCTIVE|NOT_CONDUCTIVE|SUPERCONDUCTIVE]\n";
    $usage .= "    --ccp=[0|1] (whether to enable CCP; defaults to 0\n";
    $usage .= "    NVN OPTIONS\n";
    $usage .= "    --nvn-bind-file=<name> (for renaming devices in extracted netlist)\n";
    $usage .= "    --nvn-log=<filename> (the .cls file from nvn on transistor netlist)\n";
    $usage .= "    --swappin-log=<filename> (pinswap err file)\n";
    $usage .= "    --ignore-nvn=[0|1] (continue with extraction even nvn fails)\n";
    $usage .= "    --task=[stage1|stage2|stage3] (defaults to all)\n\n";
    die "$usage";
}

my $working_dir;
my $root_target_dir;
my $current_target_dir;
my $gdsii;
my $del;
my $pin_order_file;
my $graycell_file;
my $mode_op;
my $cdl_file;
my $cdl_cell_name;
my $alias_file;
my $routed_alias_file;
my $spice_target;
my $spice_topcell;
my $extractPower;
my $extractedView = "layout";
my $extractCorner;
my $xtc_cmd_file;
my $fire_cmd_file;
my $ice_cmd_file;
my $rain_setup_file;
my $FireStepSize;
my $minR;
my $minC;
my $minCC;
my $maxF;
my $temperature;
my $magnification_factor=1.0;
my $extra_extract_equiv="";
my $lvs_extra_options="";
my $pdk_root;
my $nvn_bind_file;
my $nvn_log;
my $swappin_log;
my $ignoreNVN=1;
my $task;
my $graycell_list="";
my $blackbox=0;
my $fixbulk=0;
my $doflat=0;
my $extractReduce="NO_EXTRA_LOOPS";
my $genspef=0;
my $merge_paths=0;
my $dfIIdir;
my $threads=2;
my $nt_mode=0;
my $totem_mode=0;
my $instancePort='CONDUCTIVE';
my $nodeprops;
my $ccp=0;

while (defined $ARGV[0] and $ARGV[0] =~ /^--(.*)/) {
    my ($flag, $value) = split("=",$1);
    $value=1 if ! defined $value;
    if ($flag eq "fixbulk") {
            $fixbulk = $value;
    } elsif ($flag eq "flat") {
            $doflat = $value;
    } elsif ($flag eq "working-dir") {
            $working_dir = $value;
    } elsif ($flag eq "root-target-dir") {
            $root_target_dir = $value;
    } elsif ($flag eq "current-target-dir") {
            $current_target_dir = $value;
    } elsif ($flag eq "gds2-file") {
            $gdsii = $value;
    } elsif ($flag eq "delete") {
            $del = $value;
    } elsif ($flag eq "pin-order-file") {
            $pin_order_file = $value;
    } elsif ($flag eq "gray-cell-list") {
            $graycell_file = $value;
    } elsif ($flag eq "64bit") {
            $mode_op = $value;
    } elsif ($flag eq "cdl-file") {
            $cdl_file = $value;
    } elsif ($flag eq "cdl-cell-name") {
            $cdl_cell_name = $value;
    } elsif ($flag eq "alias-file") {
            $alias_file = $value;
    } elsif ($flag eq "routed-alias-file") {
            $routed_alias_file = $value;
    } elsif ($flag eq "spice-target") {
            $spice_target = $value;
    } elsif ($flag eq "spice-topcell") {
            $spice_topcell = $value;
    } elsif ($flag eq "extract-power") {
            $extractPower =$value;
    } elsif ($flag eq "extract-corner") {
            $extractCorner =$value;
    } elsif ($flag eq "extracted-view") {
            $extractedView =$value;
    } elsif ($flag eq "minR") {
            $minR = $value;
    } elsif ($flag eq "minC") {
            $minC = $value;
    } elsif ($flag eq "minCC") {
            $minCC = $value;
    } elsif ($flag eq "maxF") {
            $maxF = $value;
    } elsif ($flag eq "temperature") {
            $temperature = $value;
    } elsif ($flag eq "fulcrum-pdk-root") {
            $pdk_root = $value;
    } elsif ($flag eq "nvn-bind-file") {
            $nvn_bind_file = $value;
    } elsif ($flag eq "nvn-log") {
            $nvn_log = $value;
    } elsif ($flag eq "swappin-log") {
            $swappin_log = $value;
    } elsif ($flag eq "ignore-nvn") {
            $ignoreNVN = $value;
    } elsif ($flag eq "blackbox") {
            $blackbox = $value;
    } elsif ($flag eq "task") {
        $task = $value;
    } elsif ($flag eq "extractReduce") {
        $extractReduce = $value;
    } elsif ($flag eq "spef") {
        $genspef = $value;
    } elsif ($flag eq "dfII-dir") {
        $dfIIdir = $value;
    } elsif ($flag eq "threads") {
        $threads = $value;
    } elsif ($flag eq "extra-extract-equiv") {
        $extra_extract_equiv = $value;
    } elsif ($flag eq "lvs-extra-options") {
        $lvs_extra_options = $value;
    } elsif ($flag eq "nt-mode") {
        $nt_mode = $value;
    } elsif ($flag eq "totem-mode") {
        $totem_mode = $value;
    } elsif ($flag eq "instance-port") {
        $instancePort = $value;
    } elsif ($flag eq "ccp") {
        $ccp = $value;
    } elsif ($flag eq "node-props") {
        $nodeprops = $value;
    } else {
        print STDERR "Error: argument --${flag}=${value} not recognized.\n";
        &usage();
    }
    shift @ARGV;
}


my @validviews=split(/\s+/,$extractedView);;

@ARGV == 1 or usage();
## cell name from cadence
my $cell_name= $ARGV[0];

my $stage1 = (!(defined $task) or $task eq "stage1");
my $stage2a = (!(defined $task) or $task eq "stage2a");
my $stage2b = (!(defined $task) or $task eq "stage2b");
my $stage2c = (!(defined $task) or $task eq "stage2c");
my $stage3a = (!(defined $task) or $task eq "stage3a");
my $stage3b = (!(defined $task) or $task eq "stage3b");
my $stage3c = (!(defined $task) or $task eq "stage3c");
my $stage4a = (!(defined $task) or $task eq "stage4a");
my $stage4b = (!(defined $task) or $task eq "stage4b");
my $stage4c = (!(defined $task) or $task eq "stage4c");
my $stage4d = (!(defined $task) or $task eq "stage4d");

if(! (!(defined $task) or
      $task eq "stage1" or
      $task eq "stage2a" or
      $task eq "stage2b" or
      $task eq "stage2c" or
      $task eq "stage3a" or
      $task eq "stage3b" or
      $task eq "stage3c" or
      $task eq "stage4a" or
      $task eq "stage4b" or
      $task eq "stage4c" or
      $task eq "stage4d") ){
    print "Error: $task is not recognised";
    usage();
}

# Configuration

defined $pin_order_file or $pin_order_file = "";
defined $del or $del=1;
defined $working_dir or $working_dir = "$pwd";
defined $task or $task = "stage1";
defined $mode_op or $mode_op=0;
defined $extractPower or $extractPower="NO";
defined $extractedView or $extractedView="";
defined $cdl_file or $cdl_file="";
defined $cdl_cell_name or $cdl_cell_name=$cell_name;
defined $xtc_cmd_file or $xtc_cmd_file="";
defined $fire_cmd_file or $fire_cmd_file="";
defined $ice_cmd_file or $ice_cmd_file="";
defined $rain_setup_file or $rain_setup_file="";
defined $minR or $minR = 0.1;
defined $minC or $minC = 0;
defined $minCC or $minCC = 0;
defined $ignoreNVN or $ignoreNVN = 1;
defined $blackbox or $blackbox = 0;
defined $maxF or $maxF = 1e12;
defined $temperature or $temperature = 90;
defined $FireStepSize or $FireStepSize=7;
defined $spice_target or $spice_target = "$pwd/$cell_name\.spice";
defined $spice_topcell or $spice_topcell = "$pwd/$cell_name\.top";
defined $pdk_root or $pdk_root="$ENV{FULCRUM_PDK_ROOT}";
my $machine=`uname -m`;
chomp $machine;
$mode_op = 0 if ($machine =~ /x86_64/ and $ENV{PATH} =~ /SEV-32/);
my $conf_dir="$pdk_root/share/Fulcrum/starrcxt";
my $AssuraHome="$pdk_root/share/Fulcrum/assura";
my $extract_dir=$spice_target;
my $upf_version="x2r2";
$extract_dir =~ s:/[^/]*$::;
$extract_dir =~ s:.*/::;
$extract_dir =~ s/accurate/extracted/;

#defined $nvn_bind_file or $nvn_bind_file="$AssuraHome/bind.rul";
defined $nvn_bind_file or $nvn_bind_file="";

my $escaped_cell_name="\Q$cell_name\E";

## Reading the pdk.config
my %config;
open CONFIG, "<$pdk_root/share/Fulcrum/pdk.config";
while(my $line=<CONFIG>)  {
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

my $spiceFile = "$working_dir/starRC/${escaped_cell_name}.spice";
my $nmapFile = "$working_dir/nvn2/${escaped_cell_name}.nmap";
my $starRC_dir="$working_dir/starRC";
my $pin_file = "$working_dir/starRC/pin_file.sp";
my $graybox_list_file = "$working_dir/starRC/graybox_list.txt";
my $longcellnametop = length($cell_name) > 75 ? 1 : 0;
my $topcell=$longcellnametop ? "TOP_CELL" : $cell_name;
my $longcellnamegray=0;
my %graylist=();
my %reversegraylist=();
my %reNameContext = ();

sub makegdsgraylist {
    my ($graycell_file) = @_;
    my $n=0;
    if ( -s "$graycell_file") {
        open (P, "rename --type=cell --from=cast --to=gds2 <'$graycell_file' |");
        while (<P>) {
            chomp;
            s/\s+/ /g;
            s/^\s//;
            s/\s$//;
            if (length ($_) > 1) {
                $graylist{$_}=$_;
                $reversegraylist{$_}=$_;
                if (length ($_) > 64) {
                    $graylist{$_}=sprintf "SUBCELL_%04d", $n;
                    $reversegraylist{$graylist{$_}} = $_;
                    $longcellnamegray = 1;
                }
                $n++;
            }
        }
        close P;
    }
    if ($longcellnamegray==1 or 1) {
        my $fx;
        `mkdir -p "$working_dir/starRC"`;
        open $fx, ">$working_dir/starRC/gray_list.xref" or warn "Cannot create $working_dir/starRC/gray_list.xref\n";
        foreach my $name (sort keys %graylist) {
            print $fx "$name $graylist{$name}\n";
        }
        close $fx;
    }
}



if($stage1){
## CREATING DIRECTORY STRUCTURE AND COPYING FILES
    system("chmod 755 \"$working_dir\"");

    print "Extraction Working directory is $working_dir\n";
    ##########################################################################
    #                              ICV LVS                                   #
    ##########################################################################
    print "Running ICV LVS...\n";
    $starRC_dir="$working_dir/starRC";
    if( -e "$working_dir/starRC/group") {
       my_system("rm -rf '$working_dir/starRC/group'");
    }
    my_system("mkdir -p '$starRC_dir'");
    chdir "$starRC_dir";
    my @lvs_cmd=("lvs",
    "--working-dir=$starRC_dir",
    "--gds2-file=$gdsii",
    "--cdl-file=$cdl_file",
    "--cdl-cell-name=$cdl_cell_name",
    "--blackbox=$blackbox",
    "--threads=$threads",
    "--rc-database=1",
    "--node-props=$nodeprops",
    "--fulcrum-pdk-root=$pdk_root");
    push @lvs_cmd, "--icv-options=$lvs_extra_options" if ($lvs_extra_options ne "");
    push @lvs_cmd, "--gray-cell-list=$graycell_file" if ($graycell_file ne "");
    push @lvs_cmd, "--extra-extract-equiv=$extra_extract_equiv" if (defined $extra_extract_equiv and $extra_extract_equiv ne "");
    push @lvs_cmd, "$cell_name";
    system(@lvs_cmd);

    if( -e "$topcell.LVS_ERRORS" ){
      if( -e "run_details/$topcell.cmperr" and defined $nvn_log ){
          my_system("cp 'run_details/$topcell.cmperr' '$nvn_log'");
      }

      open LVS_ERRORS, "<$topcell.LVS_ERRORS";
      my $Result=1;
      while(<LVS_ERRORS>)  {
        if(/^Final comparison result:PASS/){ $Result=0; }
      }
      if($Result == 0){
        print "ICV/NVN SUCCESS!\n";
      } else {
        # note that the string 'NVN FAILED' must appear for lve
        print "ICV/NVN FAILED: schematic and layout does not match.\n";
        die "Error: ICV/NVN FAILED: schematic and layout does not match for $topcell\n";
      }

    } else {
      die "Error: ICV/NVN FAILED: fail to extract layout for $topcell.\n";
    }


}

if($stage2a){

    ##########################################################################
    #                               Pin Order File                           #
    ##########################################################################
        chdir "$starRC_dir";

        $graycell_list="";
        my $renamed_graycell_list="";
        my $skip_cell_cmd="";
        if(-e $graycell_file) {

            makegdsgraylist($graycell_file);
            open GRAYCELL,">$graybox_list_file";
            foreach my $name (sort keys %graylist) {
                print GRAYCELL "SKIP_CELLS: $graylist{$name}\n";
                $graycell_list .= " $name";
                $renamed_graycell_list .= " $graylist{$name}";
            }
            close GRAYCELL;
            $skip_cell_cmd="SKIP_CELLS_FILE: $graybox_list_file";
            $graycell_list =~ s/^ //;
            $renamed_graycell_list =~ s/^ //;
        } else {
            $graycell_list="";
            $skip_cell_cmd="SKIP_CELLS:                   !*";
        }

        if(-e $pin_order_file) {
            my_system("cp '$pin_order_file' '$pin_file'");
        } else {
            my $begin = 0;
            open CDL, "<$cdl_file" or die "CAN'T OPEN CDL FILE\n";
            open PINFILE, ">$pin_file" or die "CAN'T OPEN PIN FILE\n";

            while(my $line=<CDL>)  {
                if(($line=~/\.subckt\s+(\S+)\s/i)and($begin==0)){
                    my $subckt_name=$1;
                    if($graycell_list =~ /\Q$subckt_name\E/ or $subckt_name eq $cell_name){
                        $begin=1;
                        $line =~ s/$cell_name/$topcell/ if $longcellnametop and $subckt_name eq $cell_name;
                        $line =~ s/$subckt_name/$graylist{$subckt_name}/ if $longcellnamegray and defined ($graylist{$subckt_name});
                        print PINFILE $line;
                    }
                }
                elsif(($begin==1)and( $line=~/^\+\s*/))  {
                    ## Continuation of subckt definition
                    $line =~ s/$cell_name/$topcell/ if $longcellnametop;
                    foreach my $name (sort keys %graylist) {
                        $line =~ s/$name/$graylist{$name}/;
                    }
                    print PINFILE $line;
                }
                elsif($begin==1)  {
                    $begin=0;
                    print PINFILE ".ENDS\n";
                }
            }
            close(CDL);
            close(PINFILE);
        }
}

sub read_starcmd {
    my ($file, $cmd) = @_;
    open my $fh, $file or die "Can't read $file: $!";
    while (my $line = <$fh>) {
        if ($line =~ /^\*/) {}
        elsif ($line =~ /^(\S+):\s*(\S.*)$/) {
            $cmd->{$1} = $2;
        }
    }
    close $fh;
}

if ($stage2b) {

    ##########################################################################
    #                               StarRC                                   #
    ##########################################################################

    print "Running StarRC Extraction ...\n";
    chdir "$starRC_dir";

    # read default star.cmd from PDK
    my %cmd = ();
    read_starcmd("$conf_dir/star.cmd", \%cmd);

    # set options
    $cmd{"TCAD_GRD_FILE"} = "$conf_dir/$upf_version.nxtgrd";
    if ( -f "$conf_dir/$upf_version.$extractCorner.nxtgrd") {
        $cmd{"TCAD_GRD_FILE"} = "$conf_dir/$upf_version.$extractCorner.nxtgrd";
    }
    my $skip_cell_cmd="";
    if(-s $graycell_file) {
        $cmd{"SKIP_CELLS_FILE"} = $graybox_list_file;
    } else {
        $cmd{"SKIP_CELLS"} = "!*";
    }
    my $tailcomments="NO";
    my $reduction=$extractReduce;
    if ($extractReduce =~ /^(NO_EXTRA_LOOPS|YES|HIGH)$/) {
        $tailcomments = "NO";
    }
    elsif ($extractReduce =~ /^(NO|LAYER)$/) {
        $tailcomments = "YES";
    }
    else {
        $reduction = "NO_EXTRA_LOOPS";
        $tailcomments="NO";
    }
    my ($netlist_format, $netlist_gnd, $remove_dangling_branches);
    if ($genspef) {
        $netlist_format="SPEF";
        $tailcomments="NO";
        $netlist_gnd="GND";
        $reduction = "YES";
        $remove_dangling_branches = "NO";
        $cmd{"XREF"}="COMPLETE";
    }
    else {
        $netlist_format="NETNAME";
        $netlist_gnd="COUPLING_GND";
        $remove_dangling_branches = "YES";
    }
    $cmd{"EXTRACTION"} = "RC";
    my $passive_params="YES";
    my $hierSep='/';
    if ($totem_mode) {
        $cmd{"EXTRACTION"} = "C";
        $cmd{"PLACEMENT_INFO_FILE"} = "YES";
        $tailcomments="NO";
        $reduction="HIGH";
    }
    if ($nt_mode) {
        $netlist_format="SPEF";
        $reduction="NO";
        $remove_dangling_branches = "NO";
        $passive_params="NO";
        $hierSep='.';
        my $dpf = "${cell_name}.dpf";
        $cmd{"NETLIST_IDEAL_SPICE_FILE"} = $dpf;
        symlink($dpf, 'cell.dpf');
        symlink("${cell_name}.spef", 'cell.spef');
    }
    my $spiceExt = $netlist_format eq 'SPEF' ? 'spef' : 'spf';
    my $spiceTemp = "${cell_name}.$spiceExt";
    if (-e 'starrc.report') {
        $cmd{"ICV_RUNSET_REPORT_FILE"} = "starrc.report";
    } else {
        die "Error: STAR_RC FAILED: Can't find LVS results.\n";
    }
    if ($threads > 1) {
        $cmd{"NUM_CORES"} = $threads;
        $cmd{"STARRC_DP_STRING"}= "list localhost:$threads";
    }

    # override some default commands
    $cmd{"BLOCK"} = $topcell;
    $cmd{"INSTANCE_PORT"} = $instancePort;
    $cmd{"NETLIST_REMOVE_DANGLING_BRANCHES"} = $remove_dangling_branches;
    $cmd{"OPERATING_TEMPERATURE"} = $temperature;
    $cmd{"SPICE_SUBCKT_FILE"} = $pin_file;
    $cmd{"HIERARCHICAL_SEPARATOR"} = $hierSep;
    $cmd{"NETLIST_FORMAT"} = $netlist_format;
    $cmd{"NETLIST_PASSIVE_PARAMS"} = $passive_params;
    $cmd{"NETLIST_FILE"} = $spiceTemp;
    $cmd{"POWER_EXTRACT"} = $extractPower;
    $cmd{"NETLIST_TAIL_COMMENTS"} = $tailcomments;
    $cmd{"REDUCTION"} = $reduction;
    $cmd{"NETLIST_GROUND_NODE_NAME"} = $netlist_gnd;
    $cmd{"VIA_COVERAGE_OPTION_FILE"} = "$conf_dir/$upf_version.via_coverage.custom";

    # setup CCP
    if ($ccp) {
        my %ccpcmd = ();
        read_starcmd("$conf_dir/ccp.cmd", \%ccpcmd);
        $ccpcmd{'DEVICE_MODEL_MAP_FILE'} = "$conf_dir/$upf_version.device_map";
        $ccpcmd{'RCMODEL_MAP_FILE'} = "$conf_dir/$upf_version.rc_map";
        $ccpcmd{'STARRCXT_CCP_EXECUTABLE'} = "$ENV{CCP_INSTALL_PATH}/ccp";
        $ccpcmd{'STARRCXT_CCP_VERSION'} = "$ENV{CCP_VERSION}";
        $ccpcmd{'UPF_FILE'} = "$conf_dir/$upf_version.be.upf";
        $ccpcmd{'VIA_FILE'} = "$conf_dir/$upf_version.via_table.ctf";
        open my $fh, '>', 'ccp.cmd' or die "Can't create ccp.cmd: $!";
        print $fh join('', map { "$_: $ccpcmd{$_}\n" } sort keys %ccpcmd);
        close($fh);
        $cmd{'CCP_EXTRACTION_FILE'} = 'ccp.cmd';
    }

    # print out star.cmd for this run
    open STAR_CMD, ">star.cmd" or die "Can not create star.cmd file!\n";
    foreach my $key (sort keys %cmd) {
        print STAR_CMD "${key}: $cmd{$key}\n";
    }
    close STAR_CMD;

    # run starRc
    {
        local $ENV{'PATH'} = $ENV{'PATH'} . ":.";
        my_system("LD_LIBRARY_PATH= $ENV{STAR_SCRIPT} StarXtract -clean star.cmd > star.log");
    }
    print "StarRC Extraction ... done\n";

    # postprocess
    if ($totem_mode) { # modify spf file
        open (I, "<$spiceTemp");
        open (O, ">cell.spf");
        while (<I>) {
            s/\//./g;
            s/\\/./g;
            s/nmos/nch/g;
            s/pmos/pch/g;
            s/$topcell/$cell_name/g if $topcell ne $cell_name;
            print O;
        }
        close I;
        close O;
        # to keep files where they used to be
        symlink('cell.spf',"${spiceTemp}_mod");
        open (I, "<${topcell}.placement_info");
        open (O, ">cell.placement_info");
        while (<I>) {
            s/\//./g;
            s/\\/./g;
            s/nmos/nch/g;
            s/pmos/pch/g;
            s/= $topcell/= $cell_name/g if $topcell ne $cell_name;
            print O;
        }
        close I;
        close O;
        # to keep files where they used to be
        symlink('cell.placement_info',"${cell_name}.placement_info_mod");
    }
}
if ($stage2c) {

    ##########################################################################
    #                 Re-Format Spice File                                   #
    ##########################################################################

    makegdsgraylist($graycell_file);
    if ($genspef) {
        my $cast_cell = reName2(\%reNameContext, 'cell', 'gds2', 'cast', $cell_name);

        my %canonical = ();
        read_cdl_aliases(\%canonical, $alias_file) if $alias_file;
        my $curr = $canonical{$cast_cell} || {};
        my %routed_canonical = ();
        read_cdl_aliases(\%routed_canonical, $routed_alias_file) if $routed_alias_file;
        my $routed_curr = $routed_canonical{$cast_cell} || {};
        my %remap = map { $curr->{$_} => $routed_curr->{$_} }
                        grep { exists $curr->{$_} } keys %{$routed_curr};
        foreach my $node (keys %{$curr}) {
            my $canon = $curr->{$node};
            if (exists($remap{$canon})) {
                $canon = $remap{$canon};
                $curr->{$node} = $canon;
            }
        }

        reformat_spef("$starRC_dir/${cell_name}.spef", $spice_target, $canonical{$cast_cell});

        exit 0;
    }

    chdir "$starRC_dir";
    print "Re-Formating Spice File ...\n";
    my %bind=();
    if(-r $nvn_bind_file){
    open (BIND, "<$nvn_bind_file");
    while (<BIND>) {
        chomp;
        if (/^c /) {
            s/  */ /g;
            my ($c,$in,$out)=split;
            $bind{uc $in}=$out;
            $bind{lc $in}=$out;
            $bind{uc $out}=$out;
            $bind{lc $out}=$out;
        }
    }
    close BIND;
    }
    reformat_spice("${cell_name}.spf", $spiceFile, \%bind);
    print "Re-Formating Spice File ... done.\n";

    my_system("cp -f '$spiceFile' '$spice_topcell'");
}

sub reformat_spef {
    my ($spiceTemp, $spice_target, $canonical) = @_;
    open (P, "<$spiceTemp") or die "Can't open $spiceTemp: $!";
    open (Q, ">$spice_target") or die "Can't open $spice_target: $!";
    while (<P>) {
        chomp;
        s/\\//;
        if (/^(\*I.*\*D\s+)(\S+)$/) {
            my ($prefix, $cellName) = ($1, $2);
            $cellName = $reversegraylist{$cellName} if defined ($reversegraylist{$cellName});
            $_ = "$prefix$cellName";
        }
        my $name_map = 0;
        if ((/^\*NAME_MAP/ and $name_map = 1)..(!$name_map and /^\*[^\d]/)) {
            if (/^(\*\d+\s+)(\S+)$/) {
                my ($prefix, $name) = ($1, $2);
                $name =~ s/\//_D_/g;
                my $cast_name = reName2(\%reNameContext, 'node', 'gds2', 'cast', $name);
                my $canon = $canonical->{$cast_name};
                $name = reName2(\%reNameContext, 'node', 'cast', 'gds2', $canon)
                    if $canon;
                $_ = "$prefix$name";
            }
        }
        print Q "$_\n";
    }
    close Q;
    close P;
}

sub reformat_spice {
    my ($spiceTemp, $spiceFile, $bind) = @_;
    open( SOURCE, "<$spiceTemp") or die "Can't open '$spiceTemp' for reading.\n";
    open( TARGET, ">$spiceFile") or die "Can't open '$spiceFile' for writing.\n";
    $. = 0;
    my $line = <SOURCE>;
    my $next_line="";
    my %layer;
    while ($line) {
      $next_line = <SOURCE>;

      #Read in Layer Map
      if( $line=~/^\*LAYER_MAP/ ){
        $line= $next_line;
        while($line=~/^\s+$/ or $line=~/^\*([\d]+)\s+([0-9a-zA-Z_]+)\s*$/){
          if($line !~/^\s+$/){$layer{$1}="\U$2";}
          $line= <SOURCE>;
        }
        $next_line = <SOURCE>;
      }


      $line=~ s/@/_D_/g;
      $line=~ s/\\M/_U_M/g;
      $line=~ s/\\/@/g;
      if ($line =~ /^X/i) {
          # concatenate any continued lines
          while (defined $next_line and $next_line =~ s/^\+/ /) {
            chomp $line;
            $line .= $next_line;
            $next_line = <SOURCE>;
          }
          $line =~s/\n\+/ /g; $line=~s/\s\// /g; $line=~s/\s+/ /g;
          my ($oldName, $temp)=split(/ /,$line, 2);
          $oldName=~s/^X//;
          my $newName=convertInstName( $oldName );
          my @pin_list=split(/ /, $temp);
          my $cellName=$pin_list[$#pin_list];
          $cellName = $reversegraylist{$cellName} if defined ($reversegraylist{$cellName});
          print TARGET "X$newName\n";
          for(my $k=0; $k<$#pin_list; $k++){
            if($pin_list[$k]=~/^$oldName/ && $pin_list[$k]!~/^${oldName}_D_/){
              $pin_list[$k]=~s/$oldName//;
              $pin_list[$k]=substr($pin_list[$k], 1, length($pin_list[$k])-1);
              $pin_list[$k]="${newName}_D_$pin_list[$k]";
            } else {
              $pin_list[$k]=convertNetName($pin_list[$k]);
            }
            print TARGET "+$pin_list[$k]\n";
          }
          print TARGET "+$cellName\n";
      }
      elsif ($line =~ /^M/i) {
          # concatenate any continued lines
          while (defined $next_line and $next_line =~ s/^\+/ /) {
            chomp $line;
            $line .= $next_line;
            $next_line = <SOURCE>;
          }
          $line =~s/\n\+/ /g; $line=~s/\s+/ /g;

        # process transistor
          my @fields = split(' ',$line, 7);
          my $name =$fields[0];
          my $term1=$fields[1];
          my $term2=$fields[2];
          my $term3=$fields[3];
          my $term4=$fields[4];
          my $type =$fields[5];
          my $rest =$fields[6];
          $name=~s/^M//;
          if ($fixbulk) {
              if ($type =~ /^n/) {
                $term4="$GND";
              }
              if ($type =~ /^p/) {
                $term4="$Vdd";
              }
          }
          $term1 =~ /^\S+/ or print STDERR "Transistor has no terminals\n";
          $term2 =~ /^\S+/ or print STDERR "Transistor has only one terminal\n";
          $term3 =~ /^\S+/ or print STDERR "Transistor has only two terminals\n";
          $term4 =~ /^\S+/ or print STDERR "Transsitor has only three terminal\n";
          $name = convertInstName( $name );
          $term1 = convertNetName( $term1 );
          $term2 = convertNetName( $term2 );
          $term3 = convertNetName( $term3 );
          $term4 = convertNetName( $term4 );
          print TARGET "M$name $term1 $term2 $term3 $term4 $type $rest\n";
      }
      elsif ($line =~ /^D/i) {
          # concatenate any continued lines
          while (defined $next_line and $next_line =~ s/^\+/ /) {
            chomp $line;
            $line .= $next_line;
            $next_line = <SOURCE>;
          }
          $line =~s/\n\+/ /g; $line=~s/\s+/ /g;

        # process diode
          my @fields = split(' ',$line, 5);
          my $name=$fields[0];
          my $term1=$fields[1];
          my $term2=$fields[2];
          my $device=$fields[3];
          my $rest=$fields[4];
          $name=~s/^D//;
          $term1 =~ /^\S+/ or print STDERR "Diode has no terminals\n";
          $term2 =~ /^\S+/ or print STDERR "Diode has only one terminal\n";
          $device =~ /^\S+/ or print STDERR "Diode has no name\n";
          $name = convertInstName( $name );
          $term1 = convertNetName( $term1 );
          $term2 = convertNetName( $term2 );
          # in lieu of changing bind.rul file, and so spice will work
          # this really should be a rule in the PDK or in STARRC
          $device = $bind->{$device} if defined $bind->{$device};
          print TARGET "D$name $term1 $term2 $device $rest\n";
      }
      elsif ($line =~ /^C/i) {
          # concatenate any continued lines
          while (defined $next_line and $next_line =~ s/^\+/ /) {
            chomp $line;
            $line .= $next_line;
            $next_line = <SOURCE>;
          }
          $line =~s/\n\+/ /g; $line=~s/\s+/ /g;

        # process capacitor
          my @fields = split(' ',$line, 4);
          my $name=$fields[0];
          my $term1=$fields[1];
          my $term2=$fields[2];
          my $rest=$fields[3];
          $term1 =~ /^\S+/ or print STDERR "Capacitor has no terminals\n";
          $term2 =~ /^\S+/ or print STDERR "Capacitor has only one terminal\n";
          $term1 = convertNetName( $term1 );
          $term2 = convertNetName( $term2 );
          print TARGET "$name $term1 $term2 $rest\n";
      }
      elsif ($line =~ /^R/i) {

          # concatenate any continued lines
          while (defined $next_line and $next_line =~ s/^\+/ /) {
            chomp $line;
            $line .= $next_line;
            $next_line = <SOURCE>;
          }
          $line =~s/\n\+/ /g; $line=~s/\s+/ /g;

        # process resistor
          my @fields = split(' ',$line);
          my $name=$fields[0];
          my $term1=$fields[1];
          my $term2=$fields[2];
          $term1 =~ /^\S+/ or print STDERR "Resistor has no terminals\n";
          $term2 =~ /^\S+/ or print STDERR "Resistor has only one terminal\n";
          $term1 = convertNetName( $term1 );
          $term2 = convertNetName( $term2 );
          my $res=sprintf("%f", $fields[3]);
          # NOTE: temperature corrections assume that STARRC extracts
          # at 90C
          if( $#fields >= 5){
            my ($foo, $length)= split('=', $fields[4]);
            if($foo =~ /\$a/){
              my ($foo, $layerNum)= split('=', $fields[5]);
              print TARGET "$name $term1 $term2 $layer{$layerNum} $res A=$length W=0.001\n";
            } else {
              my ($foo1, $width)= split('=', $fields[5]);
              my ($foo2, $layerNum)= split('=', $fields[6]);
              print TARGET "$name $term1 $term2 $layer{$layerNum} $res L=$length W=$width\n";
            }
          } else {
            my $term1x = $term1;
            $term1x =~ s/:.*//;
            my $term2x = $term2;
            $term2x =~ s/:.*//;
            print TARGET "$name $term1 $term2 $res";
            if ($term1x ne $term2x) {
                my ($a,$dev)=split(/=/, $fields[4]);
                $dev = $bind->{$dev} if defined $bind->{$dev};
                $fields[4] = "$a=$dev";
                print TARGET " $fields[4]" if $term1x ne $term2x;
            }
            print TARGET "\n";
          }
      } elsif ($line !~ /^\*/) {

        # pass through non-resistor lines
        $line =~ s/$topcell/$cell_name/ if $longcellnametop;
        print TARGET $line;
      }
      $line = $next_line;
    }

    close(SOURCE);
    close(TARGET);
}


##########################################################################
#                             Flat Mode                                  #
##########################################################################
if($stage3a){
    ##########################################################################
    #                             NVN2                                       #
    ##########################################################################

    print "Running NVN2a ...\n";
    if( -e "$working_dir/nvn2") {
       my_system("rm -rf '$working_dir/nvn2'");
    }
    my_system("mkdir -p '$working_dir/nvn2'");

    if($ignoreNVN) {
        my_system("touch '$nmapFile'");
    } else {
        if( nvn_spice1("$working_dir/nvn2", "$spiceFile", "$nmapFile", 1)){
           # note that the string 'NVN FAILED' must appear for lve
           print "STAR_RC/NVN FAILED Phase 1\n"; die "Error: STAR_RC/NVN FAILED Phase 1\n";
        }
    }
}
if($stage3b){
    ##########################################################################
    #                             NVN2                                       #
    ##########################################################################

    print "Running NVN2 ...\n";
    if($ignoreNVN) {
        my_system("touch '$nmapFile'");
    } else {
        if( nvn_spice2("$working_dir/nvn2", "$spiceFile", "$nmapFile", 1)){
           # note that the string 'NVN FAILED' must appear for lve
           print "STAR_RC/NVN FAILED Phase 2\n"; die "Error: STAR_RC/NVN FAILED Phase 2\n";}
    }

    #copy the result
    unlink $spice_target;
    rename $spice_topcell, $spice_target;
    print "Spice file is $spice_target.\n";

    if($del==1)
    {
        chdir "$pwd";
        print "deleting work directories !\n";
        system("rm -rf $working_dir 2>/dev/null; rm -rf $working_dir 2>/dev/null");
    } else {
        print "Extraction directory is $working_dir.\n";
    }

    if( $ignoreNVN == 2){
        print "STARRC_NVN2/NVN ... failed but ignored\n";
    }
    if( $ignoreNVN == 1){
        print "STARRC_NVN2/NVN ... skipped\n";
    }
    if( $ignoreNVN == 0){
        print "STARRC_NVN2/NVN ... success\n";
    }

}
if ($stage3c) {
    exit 0 if ! -s "$swappin_log";
    my %nmap;
    my $cnt = 0;
    open(my $fh, "<$swappin_log") || die "Cannot open $swappin_log: $!";
    while(<$fh>) {
        chomp;
        my ($from, $to) = split;
        $from =~ s:_S_:/:g;
        $nmap{$from} = $to;
        $cnt++;
    }
    close($fh);

    exit 0 if $cnt == 0;
    open (STDIN, "<$spice_target");
    open (P, ">$spice_target.munged");
    select P;
    while(<STDIN>) {
        chomp;
        my @tokens = split;
        foreach my $token (@tokens) {
            my ($net, $subnet);
            if ($token =~ /(.*)(:[^:]+)/) {
                ($net, $subnet) = ($1, $2);
            } else {
                ($net, $subnet) = ($token, '');
            }
            $net = $nmap{$net} if (exists($nmap{$net}));
            $token = $net . $subnet;
        }
        print join(' ', @tokens) . "\n";
    }
    my_system("/bin/mv -f '$spice_target' '$spice_target.unmunged'");
    my_system("/bin/mv -f '$spice_target.munged' '$spice_target'");
    my_system("/bin/gzip '$spice_target.unmunged'");
}
##########################################################################
#                             Graybox Mode                               #
##########################################################################
if($stage4a){

    # make sure graybox extract didn't mess up for BUG 20233
    print "Checking for any top-level transistors ...\n";
    my $flat_transistors = `grep -c ^M '$spice_topcell'`;
    chomp($flat_transistors);
    if ($flat_transistors!=0) {
        print "Error: Found $flat_transistors flat transistors in graybox $spice_topcell (see BUG 20233).\n";
        die "Error: flat transistors exist in graybox $spice_topcell (see BUG 20233).\n";
    }

    ##########################################################################
    #                  Checking all SubCkts are there                        #
    ##########################################################################
    print "Checking all SubCkts ...\n";

    my $cell_total_filename="$working_dir/cell_total";

    if(-e $cell_total_filename){ my_system("rm '$cell_total_filename'");}

    my_system("echo '* cell.spice_gds2 for $cdl_cell_name' > '$cell_total_filename'");
    open( SOURCE, "<$graycell_file") or die "Can't open cell list file $graycell_file for read";
    my @cellname_list=<SOURCE>;
    close( SOURCE );
    my $subckt_incomplete=0;
    my $lve_dir=$spice_target;
    my @lve_dir = split(/\//, $lve_dir);
    # remove target file and 'extracted' dir and 'layout' dir
    pop @lve_dir;
    pop @lve_dir;
    pop @lve_dir;
    my $topcelldir = $cdl_cell_name;
    $topcelldir =~ s/_D_/./g;
    my @topcelldir=split(/\./, $topcelldir);
    $"="/";
    # remove the cell path to get the real lve_dir
    foreach my $x (@topcelldir) {
        pop @lve_dir;
    }
    $lve_dir="@lve_dir";
    undef @topcelldir;
    undef @lve_dir;
    my %subckt_list=();
    my %includedcells=();
    my $dfiifound=1;
    foreach my $cell (@cellname_list)  {
        $cell=~s/\s+//g;
        my $cell_dir=$cell;
        $cell_dir=~s/\./\//g;
        if (! $includedcells{$cell}) {
            my $found = 0;
            my @dirs=("$current_target_dir");
      	    push @dirs, split(":", $root_target_dir);
	 		
            foreach my $dir (@dirs) {

                if ( $dir ne "" and -d "$dir") {
                    foreach my $view (@validviews) {
                        if(-s "$dir/$cell_dir/$view/$extract_dir/cell.aspice" ){
                            if (! $doflat) {
                                my_system("echo \".inc '$dir/$cell_dir/$view/$extract_dir/cell.spice_gds2'\" >> '$cell_total_filename' ");
                            }
                            else {
                                my_system("cat '$dir/$cell_dir/$view/$extract_dir/cell.spice_gds2' >> '$cell_total_filename' ");
                            }
                            print "FOUND: subckt $cell cell.aspice file in $dir.\n";
                            $includedcells{$cell}=1;
                            $found=1;
                            last;
                        }
                        # routed-estimate mode as Andrew's request, bug 20302
                        if(-s "$dir/$cell_dir/$view/estimated/cell.spice" ){
                            if( ! -s "$dir/$cell_dir/$view/estimated/cell.spice_gds2" ){
                                print "Creating $dir/$cell_dir/$view/estimated/cell.spice_gds2\n";
                                system("$ENV{FULCRUM} cdl_renamer --name-in=cast --name-out=gds2 --source-cdl-file='$dir/$cell_dir/$view/estimated/cell.spice' " .
                                    "--translated-cdl='$dir/$cell_dir/$view/estimated/cell.spice_gds2'");
                            }
                            if (! $doflat) {
                                my_system("echo \".inc '$dir/$cell_dir/$view/estimated/cell.spice_gds2'\" >> '$cell_total_filename' ");
                            }
                            else {
                                my_system("cat '$dir/$cell_dir/$view/estimated/cell.spice_gds2' >> '$cell_total_filename' ");
                            }
                            print "FOUND: estimated subckt $cell cell.spice file in $dir.\n";
                            $includedcells{$cell}=1;
                            $found=1;
                            last;
                        }
                    }
                }
                last if $found;
            }
            if ( ! $found ) {
                print "Error: SubCkt Not Found: ($cell) $root_target_dir/$cell_dir/*/$extract_dir/cell.aspice does not exist.\n";
                $subckt_incomplete=1;
                $subckt_list{"$cell"}="$cell_dir";
            }
            if ( ! $doflat and ! $found and defined($dfIIdir) and -d $dfIIdir) {
                # look to see which view would be used if extract were done on subcell
                my $libdir=$cell_dir;
                $libdir =~ s:/[^/]*/[^/]*$::;
                my $dfiicn=$cell;
                $dfiicn = `echo "$cell" | rename --type=cell --from=cast --to=cast`;
                chomp $dfiicn;
                $dfiicn =~ s/\./#2e/g;
                $dfiicn =~ s/-/#2d/g;
                foreach my $view (@validviews) {
                    if ( -s "$dfIIdir/$libdir/$dfiicn/$view/layout.oa" ) {
                        my_system("echo \".inc '$root_target_dir/$cell_dir/$view/$extract_dir/cell.spice_gds2'\" >> '$cell_total_filename' ");
                        $includedcells{$cell}=1;
                        $found=1;
                        last;
                    }
                }
            }
            $dfiifound = 0 if ! $found;
        }
    }
    my $includefile = $spice_target;
    $includefile =~ s/\.[^\/]*$/.spice_include/;
    my_system ("cp '$cell_total_filename' '$includefile'");
    if ($subckt_incomplete){
        printf "%s: Can not Generate cell_total due to error in generating subckt spice files:\n",
            $dfiifound ? "Warning" : "Error";
        foreach my $cell (keys %subckt_list){
            print "  Error in extracting $cell cell.spice_gds2 file.\n";
        }
        if($del==1)
        {
            chdir "$pwd";
            print "deleting work directories !\n";
            system("rm -rf $working_dir 2>/dev/null; rm -rf $working_dir 2>/dev/null");
        } else {
            print "Logs are in $working_dir.\n";
        }
        die "Error: Subckt spice file missing.\n";;
    }

    if ($doflat) {
        #Delete redundant spice subckt definitions.
        open( SOURCE, "<$cell_total_filename") or die "Can't open '$cell_total_filename' for reading.\n";
        open( TARGET, ">$spice_target") or die "Can't open '$spice_target' for writing.\n";
        $. = 0;
        my %cell_total=();
        while (my $line=<SOURCE>) {
            my $skip=0;
            if ($line =~ /^\*/){ $skip=1; }
            if ($line =~ /^\.SUBCKT/i) {
                my $temp=$line;
                while($line =~/^\+/){$temp.=$line; $line=<SOURCE>;}
                my $temp1=$temp;
                $temp=~s/\n\+/ /g; $temp=~s/\s+/ /g; $temp=~s/^\.SUBCKT //g;
                my ($cellname, $rest)= split(/ /, $temp, 2);
                if( !defined $cell_total{$cellname} or $cell_total{$cellname} eq ""){
                  $cell_total{$cellname}=$rest;
                  $skip=1;
                  print TARGET $temp1;
                } else {
                  printf( "Delete Duplicated SUBCKT %s in cell_total...\n", $cellname);
                  if($cell_total{$cellname} ne $rest){
                     printf( "Pin Mis-match: %s\n", $cell_total{$cellname});
                     printf( "             : %s\n", $rest);
                  }
                  while($line !~/^\.ENDS/){$line=<SOURCE>;}
                  $skip=1;
                }
            }
            if ( $skip==0 ){
                print TARGET $line;
            }
        }
        close(SOURCE);
        close(TARGET);
    }
    else {
        my_system("mv '$cell_total_filename' '$spice_target'");
    }

    my_system("sed -e '/^\$/d' -e 's/  */ /g' '$spice_topcell' >> '$spice_target'");

    print "Spice Generation Success: $spice_target created with no error!\n";
}
if($stage4b){

    ##########################################################################
    #                             NVN2 Phase 1                               #
    ##########################################################################
    if( -e "$working_dir/nvn2") {
           my_system("rm -rf '$working_dir/nvn2'");
    }
    my_system("mkdir -p '$working_dir/nvn2'");

    if($ignoreNVN) {
        my_system("touch '$nmapFile'");
    } else {
        if( nvn_spice1("$working_dir/nvn2", "$spice_target", "$nmapFile", 1)){
           # note that the string 'NVN FAILED' must appear for lve
           print "STARRC/NVN FAILED Phase 1\n"; die "STARRC/NVN FAILED Phase 1\n";}
    }

}
if($stage4c){

    ##########################################################################
    #                             NVN2 Phase 2                               #
    ##########################################################################
    if($ignoreNVN) {
        my_system("touch '$nmapFile'");
    } else {
        if( nvn_spice2("$working_dir/nvn2", "$spice_target", "$nmapFile", 1)){
           # note that the string 'NVN FAILED' must appear for lve
           print "STARRC/NVN FAILED Phase 2\n";
           die "STARRC/NVN FAILED Phase 2\n";
        }
        if( -z "${nmapFile}.err" ){
           my_system("rm '${nmapFile}.err'");
        } else {
           print "Warning: Pin Swap found, see $swappin_log.\n"
        }
    }

    if($del==1)
    {
        chdir "$pwd";
        print "deleting work directories !\n";
        system("rm -rf $working_dir 2>/dev/null; rm -rf $working_dir 2>/dev/null");
    } else {
        print "Logs are in $working_dir.\n";
    }
    if( $ignoreNVN == 2){
        print "STARRC_NVN2/NVN ... failed but ignored\n";
    }
    if( $ignoreNVN == 1){
        print "STARRC_NVN2/NVN ... skipped\n";
    }
    if( $ignoreNVN == 0){
        print "STARRC_NVN2/NVN ... success\n";
    }
}
if ($stage4d) {
    exit 0 if ! -s "$swappin_log";
    my %nmap;
    my $cnt = 0;
    open(my $fh, "<$swappin_log") || die "Cannot open $swappin_log: $!";
    while(<$fh>) {
        chomp;
        my ($from, $to) = split;
        $from =~ s:_S_:/:g;
        $nmap{$from} = $to;
        $cnt++;
    }
    close($fh);

    exit 0 if $cnt == 0;
    open (STDIN, "<$spice_target");
    open (P, ">$spice_target.munged");
    select P;
    while(<STDIN>) {
        chomp;
        my @tokens = split;
        foreach my $token (@tokens) {
            my ($net, $subnet);
            if ($token =~ /(.*)(:[^:]+)/) {
                ($net, $subnet) = ($1, $2);
            } else {
                ($net, $subnet) = ($token, '');
            }
            $net = $nmap{$net} if (exists($nmap{$net}));
            $token = $net . $subnet;
        }
        print join(' ', @tokens) . "\n";
    }
    unlink($spice_target);
    my_system("/bin/mv -f '$spice_target.munged' '$spice_target'");
}
############################### SUBROUTINES GO HERE ######################

sub convertInstName {
    my ($oldName)=@_;
    chomp($oldName);
    # process two cases:
    # 1. XinstName/instName ==> instName_D_instName
    # 2. XinstName ==> instName

    $oldName=~s/^X//;
    $oldName=~s/\//_D_/g;
    $oldName=~s/:/_D_/g;

    return "$oldName";
}

sub convertNetName {
    my ($oldName)=@_;
    chomp($oldName);
    # process two cases:
    # 1. XinstName/instName:pinName ==> instName_D_instName_D_pinName
    #    XinstName:pinName ==> instName_D_pinName
    # 2. XinstName/netName:number  ==> instName_D_netName:number
    #    netName:number  ==> netName:number

    my @parts=split("\/", $oldName);
    my $lastPart="$parts[$#parts]";
    $oldName=~s/$lastPart$//;
    $oldName=~s/^X//;
    $oldName=~s/\//_D_/g;
    $oldName=~s/\/$/_D_/g;
    $oldName=~s/:/_D_/g;
    @parts=split(":", $lastPart);
    my $pinName="$parts[$#parts]";
    $lastPart=~s/$pinName$//;
    $lastPart=~s/:$//;
    $lastPart=~s/:/_D_/g;
    if($pinName=~/^[F]*[0-9]+$/){
      if($lastPart ne ""){
        if($lastPart=~ /@/){
          $lastPart=~ s/^X//;
          $lastPart=~ s/@/_D_/g;
        }
        $lastPart="${lastPart}:";
      }
    } else {
      $lastPart=~s/^X//;
      $lastPart=~ s/@/_D_/g;
      if($lastPart ne "" and $pinName ne ""){
        $lastPart="${lastPart}_D_";
      }
    }
    return "$oldName$lastPart$pinName";
}

sub nvn_spice1 {
    my ($nvn_dir,$spice,$nmapFile,$rc) = @_;


    my_system("mkdir -p '$nvn_dir'");
    chdir "$nvn_dir";

    #get rid of /'s
    if($rc) {
        my $cmd="spice_reduce --infile='$spice' --outfile='$nvn_dir/spice' --mode=nvn --cdlgds2-file='$cdl_file'";
        $cmd .= " --graybox-list='$graybox_list_file'" if -r "$graybox_list_file";
        my_system("$cmd");
    } else {
        my_system("cp '$spice' '$nvn_dir/spice'");
    }

    #make bind.rul
    open (P, "<$cdl_file");
    open (Q, ">$nvn_dir/cell.cdl_gds2");
    my %ispower=("Vdd"=>1,"GND"=>1,"VSS"=>1, "VCC"=>1);
    my %isresistor=(
        "rgcnfm1" => 1,
        "rtcnfm1" => 1,
        "rm2" => 1,
        "rm2w" => 1,
        "rm3" => 1,
        "rm3w" => 1,
        "rm4" => 1,
        "rm4w" => 1,
        "rm5" => 1,
        "rm5w" => 1,
        "rm6" => 1,
        "rm6w" => 1,
        "rm7" => 1,
        "rm7w" => 1,
        "rm8" => 1,
        "rm8w" => 1,
        "rm9" => 1,
        "rm9w" => 1,
        "rm10" => 1,
        "rm10w" => 1,
        "rppolywo" => 1,
        "rnods" => 1,
        "rpods" => 1,
        "rnwsti" => 1,
        "rnpolys" => 1,
        "rnpolyl" => 1,
        "rpodwo" => 1,
        "rnodl" => 1,
        "rnpolywo" => 1,
        "rppolyl" => 1,
        "rnodwo" => 1,
        "rpodl" => 1,
        "rnwod" => 1,
        "rppolys" => 1,
        "rupolym" => 1,
    );
    while (<P>) {
        while (/^.subckt r/i) {
            while (<P>) {
                last if /^.ends/i;
            }
            $_=<P>;
        }
        if (/^x/i and /\s+r/i and /\sl=/i and /\sw=/i) {
            my $ln=$_;
            $ln =~ s/\s+\/\s+/ /g;
            my @f=split(/ /, $ln);
            # resistor subckts
            if ($#f == 5 and $isresistor{$f[3]}) {
                $_ = "*$_";
            }
        }
        if ($fixbulk and /^m/i) {
            # shorted transistors
            my @f=split;
            if ($ispower{$f[1]} and $ispower{$f[2]} and $ispower{$f[3]} and $ispower{$f[4]}) {
                $_ = "*$_";
            }
        }
        print Q;
    }
    close P;
    close Q;
    if($nvn_bind_file ne "") {
        my_system("cp -f '$nvn_bind_file' '$nvn_dir/bind.rul'");
    }else{
        my_system("touch '$nvn_dir/bind.rul'");
    }
    my_system("chmod 664 '$nvn_dir/bind.rul'");
    open BIND,">>$nvn_dir/bind.rul" or die "Cannot open $nvn_dir/bind.rul";
    print BIND "C $cdl_cell_name $cell_name\n";
    close BIND;


    # make run.rsf
    ## XTC generates custom resistors without Values only, lengths and widths and model name
    ## This is enough for spice deck
    ## However the cdl file supplied might have lengths or widths missing
    ## Hence we want to have a way where we just check connectivity of resistors and don't compare parameters
    ## Basically concatenate the rules file provided in the nvn directory to run.rsf
    ## This rules file contains options to turn off Resistor parameter comparison
    ## Concatenating the res_no_comp.rsf is not enough since assura gives more preference to the
    ## built in comparison function for resistors which is called in the extract.rul
    ## even though we call a skill function later
    ## Hence we need to parse the rsf file and comment out or remove that call

    my $rsfFile = "$nvn_dir/run.rsf";
    open RSFIN,"<$AssuraHome/LVSinclude.rsf";
    open RSFOUT,">$rsfFile";
    print RSFOUT "(defvar useCompareMOS nil )\n";

    while(my $line=<RSFIN>)  {
        if ($line =~ /^ *compareParameter. *RES  *percent/) {
            $line = " compareParameter( RES compareRES ) )";
        }
        print RSFOUT $line;
    }
    close(RSFIN);
    close(RSFOUT);
    my_system("fgrep -v 'filter(' '$AssuraHome/compare.rul' >> '$rsfFile'");


    ####preparing run.rsf file
    open RSFOUT, ">>$rsfFile";
    print RSFOUT "avCompareRules(\n";
    #print RSFOUT "verifyTopSchPins(nil)\n";
    print RSFOUT "layout(\n";
    print RSFOUT "netlist(spice \"$nvn_dir/spice\")\n";
    print RSFOUT "filterDevice(CAP)\n";
    print RSFOUT "filterOptions(MOS \"JA\")\n";
    print RSFOUT "filterOptions(MOS \"X\")\n";
    print RSFOUT "filterOptions(RES \"X\")\n";
    print RSFOUT "filterOptions(DIO \"X\")\n";
    print RSFOUT "filterOptions(BJT \"X\")\n";
    print RSFOUT ";filterDevice(\"DW\")\n";
    print RSFOUT ";filterDevice(\"dw\")\n";
    print RSFOUT ";filterDevice(\"diodenw\")\n";
    print RSFOUT ")\n";
    print RSFOUT "schematic(\n";
    print RSFOUT "netlist( cdl \"$nvn_dir/cell.cdl_gds2\")\n";
    print RSFOUT "filterOptions(MOS \"JA\")\n";
    print RSFOUT "filterOptions(MOS \"X\")\n";
    print RSFOUT "filterOptions(RES \"X\")\n";
    print RSFOUT "filterOptions(DIO \"X\")\n";
    print RSFOUT "filterOptions(BJT \"X\")\n";
    print RSFOUT ";filterDevice(\"DW\")\n";
    print RSFOUT ";filterDevice(\"dw\")\n";
    print RSFOUT ";filterDevice(\"diodenw\")\n";
    print RSFOUT ")\n";
    print RSFOUT "bindingFile(\"bind.rul\")\n";
    if( $graycell_list ne ""){
      my @graybox_name=split(' ', $graycell_list);
      for(my $i=0; $i<=$#graybox_name; $i++){
        print RSFOUT "blackBox(\"$graybox_name[$i]\" lay)\n";
        print RSFOUT "blackBox(\"$graybox_name[$i]\" sch)\n";
      }
    }
    print RSFOUT "runName(\"$cell_name\")\n";
    print RSFOUT "listXRef()\n" if($nmapFile);
    print RSFOUT ")\n";
    close(RSFOUT);
    ######
    0;
}

sub nvn_spice2 {
    my ($nvn_dir,$spice,$nmapFile,$rc) = @_;


    my_system("mkdir -p '$nvn_dir' 2>/dev/null");
    chdir "$nvn_dir";

    my $status=system("LD_LIBRARY_PATH= $ENV{ASSURA_SCRIPT} nvn run.rsf 1> nvn.out 2> nvn.err");
    if($status!=0){print "Error: assura nvn failed.\n";}
    my_system("cp '${escaped_cell_name}.cls' '$nvn_log'") if defined $nvn_log;

    # XXX - if there are just parameter mismatches, pass.  This is because NVN
    # confuses identical subnets
    my $Result=1;
    open CSM,"<$nvn_dir/$cell_name.csm" or die "Problem with NVN, .csm file not created\n";
    while(my $line=<CSM>)  {
        if($line=~/Schematic and Layout Match/)  {
            $Result=0;
        }
    }
    close(CSM);
    if( $ignoreNVN>0 and $Result==1){
        $Result=0; $ignoreNVN=2;
    }

    return $Result if($Result);

    if($nmapFile) {
        ## creating nmap file
        open CXL, "<$nvn_dir/$cell_name.cxl" or die "xtc generated netlist and cdl did not match\n";
        open NMAP, ">$nmapFile";
        open NMAPI, ">${nmapFile}.i";
        open NMAP_ERR, ">${nmapFile}.err";
        my $main_subckt=0;
        while(<CXL>)  {
            # XXX - why do we get rid of these bindings?
            if($_=~/^C \Q$cell_name\E/) { $main_subckt=1;}
            if(($main_subckt==1)and($_=~/^N/)and($_!~/DRAIN|GATE|SOURCE|SUBSTRATE/)) {
               chomp($_);
               my @fields=split;
               print NMAP "$fields[2] $fields[1]\n";
               my @parts=split("\/", $fields[1]);
               my $lastPart="$parts[$#parts]";
               $fields[1]=~s/$lastPart$//;
               $fields[1]=~s/^X//;
               $fields[1]=~s/\//_D_/g;
               $fields[1]=~s/\/$/_D_/g;
               $fields[1]="$fields[1]$lastPart";
               if( $fields[2] ne $fields[1] ){
                 print NMAP_ERR "$fields[2] $fields[1]\n";
               }
            }
            if(($main_subckt==1)and($_=~/^I/)){
               chomp($_);
               my @fields=split(' ', $_);
               $fields[1]=~s/^X//;
               $fields[1]=~s/\//_D_/g;
               $fields[1]=~s/\//_D_/g;
               $fields[2]=~s/^X//;
               print NMAPI "$fields[2] $fields[1]\n";
            }
        }
        close(CXL);
        close(NMAP);
        close(NMAPI);
        close(NMAP_ERR);
        my_system("cp '${nmapFile}.err' '$swappin_log'") if defined $swappin_log;

    }

    return $Result;
}

# execute a command, taking care of exit status
sub my_system {
    my ($cmd)=@_;
    my @cmdlist=@_;
    if ($#cmdlist > 0) {
    my $status = system(@_);
    $status == 0 or die "Error: @_ failed.\n";
    }
    else {
    my $status = system($cmd);
    $status == 0 or die "Error: $cmd failed.\n";
    }
}
