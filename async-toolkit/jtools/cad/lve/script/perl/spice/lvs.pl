#!/usr/intel/bin/perl -w
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
use LveUtil qw /reName parse_nodeprops/;


$ENV{SNPSLMD_QUEUE}="true";
my $pwd=getcwd();

my $working_dir = "$pwd";
my $gdsii="";
my $graycell_file="";
my $cdl_file="";
my $cdl_cell_name="";
my $pdk_root="";
my $task;
my $graycell_list="";
my $extra_extract_equiv="";
my $blackbox=0;
my $icv_options;
my $threads=2;
my $icv_runset_path="/nfs/sc/proj/ctg/mrl108/mrl/tools/rdt/kits/p1273_14.2.1/runsets/icvalidator/verification_runsets/latest";
my $rc_database=0;
my $nodeprops='';

sub usage {
    my ($msg) = @_;
    print STDERR "$msg\n" if defined $msg;
    my $usage  = "Usage: lvs [args] cell\n";
    $usage .= "  Args includes:\n";
    $usage .= "    GENERAL OPTIONS\n";
    $usage .= "    --gds2-file=[$gdsii]\n";
    $usage .= "    --working-dir=[$working_dir]\n";
    $usage .= "    --cdl-file=[$cdl_file]\n";
    $usage .= "    --cdl-cell-name=[$cdl_cell_name]\n";
    $usage .= "    --gray-cell-list=[$graycell_file] (gray box cell list)\n";
    $usage .= "    --extra-extract-equiv=[$extra_extract_equiv] (file with list of extra equiv cast cells.
                    \t\tFormat: cast_cell_name  layout_cell_name)\n";
    $usage .= "    --blackbox=[$blackbox]\n";
    $usage .= "    --icv-options=[$icv_options] (Extra ICV command options)\n";
    $usage .= "    --threads=[$threads] (ICV thread)\n";
    $usage .= "    --rc-database=[$rc_database] (Generate RC database)\n";
    $usage .= "    --node-props=[$nodeprops] (Node properties file to find supply nets)\n";
    $usage .= "    --fulcrum-pdk-root=[$pdk_root]\n";

    print STDERR "$usage\n";
    exit 1;
}

#my %options = (
#   "working-dir=s" => \$working_dir,
#   "gds2-file=s" => \$gdsii,
#   "cdl-file=s" => \$cdl_file,
#   "cdl-cell-name=s" => \$cdl_cell_name,
#   "blackbox:i" => \$blackbox,
#   "gray-cell-list=s" => \$graycell_file,
#   "extra-extract-equiv=s" => \$extra_extract_equiv,
#   "icv-options=s" => \$icv_options,
#   "threads=i" => \$threads,
#   "rc-database=i" => \$rc_database,
#   "fulcrum-pdk-root=s" => \$pdk_root
#);
#GetOptions( %options ) or usage("Illegal Option");
while (defined $ARGV[0] and $ARGV[0] =~ /^--(.*)/) {
    my ($flag, $value) = split("=",$1);
#    my $value_org=$value;
    $value=1 if ! defined $value;
    if ($flag eq "working-dir") {
            $working_dir = $value if(defined $value);
    } elsif ($flag eq "gds2-file") {
            $gdsii = $value if(defined $value);
    } elsif ($flag eq "gray-cell-list") {
            $graycell_file = $value if(defined $value);
    } elsif ($flag eq "cdl-file") {
            $cdl_file = $value if(defined $value);
    } elsif ($flag eq "cdl-cell-name") {
            $cdl_cell_name = $value if(defined $value);
    } elsif ($flag eq "fulcrum-pdk-root") {
            $pdk_root = $value  if(defined $value);
    } elsif ($flag eq "blackbox") {
            if(defined $value){
              $blackbox = $value;
            }else{
              $blackbox = 1;
            }
    } elsif ($flag eq "threads") {
        $threads = $value if (defined $value);
    } elsif ($flag eq "extra-extract-equiv") {
        $extra_extract_equiv = $value  if(defined $value);
    } elsif ($flag eq "icv-options") {
        $icv_options = $value  if(defined $value);
    } elsif ($flag eq "rc-database") {
            if(defined $value){
              $rc_database = $value;
            }else{
              $rc_database = 1;
            }
    } elsif ($flag eq "node-props") {
        $nodeprops = $value;
    } else {
        print STDERR "Error: argument --${flag}=${value} not recognized.\n";
        &usage();
    }
    shift @ARGV;
}


@ARGV == 1 or usage("No Cell");
my $cell_name = shift;

if ($threads < 1) {
    $threads=1;
}
-d $working_dir ne "" or $working_dir = `mktemp -d /scratch/lvs.XXXXXX`;
chomp $working_dir;
$cdl_cell_name ne "" or $cdl_cell_name=$cell_name;
$pdk_root="$ENV{FULCRUM_PDK_ROOT}" if ( ! ( -d $pdk_root ) and -d $ENV{FULCRUM_PDK_ROOT});
-d $pdk_root or usage("fulcrum-pdk-root improperly defined");


my $longcellnametop = length($cell_name) > 75 ? 1 : 0;
my $topcell=$longcellnametop ? "TOP_CELL" : $cell_name;
my $longcellnamegray=0;
my %graylist=();
my %reversegraylist=();

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
        open $fx, ">$working_dir/gray_list.xref" or warn "Cannot create $working_dir/gray_list.xref\n";
        foreach my $name (sort keys %graylist) {
            print $fx "$name $graylist{$name}\n";
        }
        close $fx;
    }
}

## CREATING DIRECTORY STRUCTURE AND COPYING FILES
system("chmod 2755 \"$working_dir\"");

##########################################################################
#                               LVS                                      #
##########################################################################
main();





##########################################################################
sub main{
  if( -e "$working_dir/group") {
     my_system("rm -rf '$working_dir/group'");
  } 
  my_system("mkdir -p '$working_dir'"); 
  chdir "$working_dir";

  makegdsgraylist($graycell_file);

  fix_gds_long_name(); 
  
  my $equivlance_file=prepare_equiv_file();
  my $schematic_file=prepare_sch_file();
  my $clf_file=prepare_clf_file($schematic_file,$equivlance_file);

  {
      local %ENV = %ENV;
      delete $ENV{'LD_LIBRARY_PATH'};
      #my_system("source $lvs_cmd_file");
      my_system($ENV{'ICV_SCRIPT'}, 'icv', '-clf',"$clf_file");
  }
  if( -e "$topcell.LVS_ERRORS" ){
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


sub prepare_clf_file {
   my ($sch_file, $equivlance_file)=@_;

   # write supply net derived from CAST
   my $supply_file="$working_dir/lve_supplies.rs";
   open(my $supply_fh, ">$supply_file") or die "Cannot write to $supply_file\n";
   if ($nodeprops) {
      my $props=parse_nodeprops($nodeprops);
      my @types=('ground', 'power');
      my %nets=();
      foreach my $type (@types) {
         $nets{$type}=[ grep { $props->{$_}->{"is_$type"} } keys %{$props} ];
      }
      my %nmap=();
      reName('rename', 'cast', 'gds2', 'node', \%nmap,
             [ map { @{$nets{$_}} } @types ]);
      foreach my $type (@types) {
         print $supply_fh "lve_$type = {" .
                          join(', ', map { "\"$nmap{$_}\"" } @{$nets{$type}}) .
                          "};\n";
      }
   } else {
      print $supply_fh "// --node-props not given\n";
   }
   close($supply_fh);

   my $lvs_clf_file="$working_dir/lvs.clf";
   open(LVS_CLF, ">$lvs_clf_file") or die "Cannot write to $lvs_clf_file\n";
   print LVS_CLF <<ET;
-I .
-I $pdk_root/share/Fulcrum/icv/lvs
-I $icv_runset_path/PXL_ovrd
-I $icv_runset_path/PXL
-I $icv_runset_path/StandAlone/dotOne
-I $icv_runset_path/util/dot1/HIP
-I $icv_runset_path/util/Cadnav
-I $icv_runset_path/util/denplot
-I $working_dir
-D _drIncludePort
-D NOCLD
-vue
-dp$threads
-turbo
-D _drMaxError=100000000
-D _drUSENDG=_drNO
-D _drUSERDEFINESUIN
-D _drMSR
-D _drCaseSensitive
-D _drTOPCHECK=_drmixed
-i cell.gds2
-s $sch_file
-sf ICV
-stc $topcell
-c $topcell
ET
    print LVS_CLF "-D _drRCextract\n"  if ($rc_database);
    print LVS_CLF "-e $equivlance_file\n" if (-r $equivlance_file);
    print LVS_CLF "$icv_options\n" if (defined $icv_options and $icv_options ne "");

    my $ilvs_deck="$icv_runset_path/StandAlone/dotOne/trclvs.1.rs";
    print LVS_CLF "$ilvs_deck\n";
    close(LVS_CLF);
    return $lvs_clf_file;
}

sub prepare_sch_file {
  my $sch_out_tmp= "$working_dir/${cell_name}.sch_out.tmp";
  my $sch_out= "$working_dir/${cell_name}.sch_out";
  symlink $cdl_file, "$working_dir/cell.cdl_gds2";
  open (X, ">$working_dir/nettran.cdl");
  print X "*.SCALE meter\n";
  print X ".include '$working_dir/cell.cdl_gds2'\n";
  close X;
  my %map=();
  %map=%graylist if $longcellnamegray;
  $map{$cell_name}=$topcell if $longcellnametop;

  system("LD_LIBRARY_PATH= $ENV{ICV_SCRIPT} icv_nettran -sp '$working_dir/nettran.cdl' -sp-slashSpace -logFile nettran.log -outName '$sch_out_tmp'");
  if (%map) {
        open (GIN, "<$sch_out_tmp");
        open (GOUT, ">$sch_out");
        while (<GIN>) {
            chomp;
            if (%map) {
                if (/^(\{cell\s+|\{inst[^=]+=)(\S+)(.*)/) {
                    $_ = "$1$map{$2}$3" if (defined($map{$2}));
                }
            }
            print GOUT "$_\n";
        }
        close GIN;
        close GOUT;
        unlink $sch_out_tmp;
    }
    else {
        rename $sch_out_tmp, $sch_out;
    }


  return $sch_out;
}

sub fix_gds_long_name {
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
          print GOUT "$_\n";
      }
      close GIN;
      close GOUT;
  }
  else {
      my_system("rm -f cell.gds2; ln -sf '$gdsii' cell.gds2"); 
  }

}



sub get_equiv {
    my ($graylist, $extra_extract_equiv) = @_;

    my %equiv = ();
    if (-e $extra_extract_equiv) {
        open(my $fh, $extra_extract_equiv)
            or die "Error: can't read $extra_extract_equiv: $!";
        while (<$fh>) {
            next if /^#/ || /^\s+$/;
            my @parts = split;
            my ($sch, $lay);
            if (@parts == 1) {
                ($sch, $lay) = ($parts[0], '');
            } elsif (@parts == 2) {
                ($sch, $lay) = ($parts[0], $parts[1]);
            } else {
                warn "Warning: malformed equiv on line $. in $extra_extract_equiv";
            }
            $equiv{$sch} = $lay;
        }
        close $fh;
    }

    my %nmap = ();
    reName('rename', 'cast', 'gds2', 'cell', \%nmap, [ keys %equiv ])
        if %equiv;

    my %result = map { $graylist->{$_} => { $graylist->{$_} => 1 } } keys %{$graylist};
    foreach my $sch (keys %equiv) {
        my $schgds = $nmap{$sch};
        $schgds = $graylist->{$schgds} if exists $graylist->{$schgds};
        my $lay = $equiv{$sch};
        if ($lay eq '') {
            $lay = $schgds;
        } else {
            $lay = $graylist->{$lay} if exists $graylist->{$lay};
        }
        my $prev = $result{$schgds};
        $prev = $result{$schgds} = {} unless $prev;
        $prev->{$lay} = 1;
    }

    return \%result;
}




sub prepare_equiv_file{
  my %equiv=();
  my %requiv=();
  my $equivalence="equiv"; 
  
  if(-s $graycell_file) {
      open EQUIV_FILE, ">$working_dir/equiv" or die "CAN'T OPEN $working_dir/equiv for write\n";
      my $cdlname=$topcell;
      $cdlname=$requiv{$topcell} if defined $requiv{$topcell};
      print EQUIV_FILE "equiv_options(equiv_cells={{\"$cdlname\",\"$topcell\"}});\n";
      my $equiv = get_equiv(\%graylist, $extra_extract_equiv);
      
      foreach my $sch (sort keys %{$equiv}) {
          foreach my $lay (sort keys %{$equiv->{$sch}}) {
              my $type = $blackbox == 1 ? 'lvs_black_box_options'
                                                : 'equiv_options';
              print EQUIV_FILE "$type(equiv_cells={{\"$sch\",\"$lay\"}});\n";
          }
      }
      close(EQUIV_FILE);
  }

  return $equivalence;
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
