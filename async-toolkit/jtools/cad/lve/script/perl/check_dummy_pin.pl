#!/usr/intel/bin/perl
# Copyright 2005 Fulcrum Microsystems.  All rights reserved.
# Authors: Kai He

use FileHandle;
use IPC::Open2;
use File::Temp qw/ tempfile tempdir /;

# Find relevant packaged tools
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

$rename = "rename";
$mkcdswd =  "mkcdswd";
$cds_sh_lib="$package_root/share/script/sh/util";
$dfII_lib_name="dfII";

$output_dir      = "$ENV{PWD}";
$cast_dir        = "$ENV{HOME}/hw/cast";
$spec_dir        = "$ENV{HOME}/hw/layout/tsmc13/spec";
$dfII_dir        = "$ENV{HOME}/hw/layout/tsmc13/dfII";
$pdk_root        = "$ENV{FULCRUM_PDK_ROOT}";


sub usage() {
    $usage  = "USAGE: $0 \n";
    $usage .= "    --fulcrum-pdk-root=<path name> default()\n";
    $usage .= "    --dfII-dir=<path name> (used for instantiator views and for mkcdswd)\n";
    $usage .= "    --cell=<cellname> default()\n";
    $usage .= "    --cdl=<filename> \n";
    $usage .= "    --cast-path=<pathname> \n";
    $usage .= "    --max-heap-size=<512M> \n";
    $usage .= "    --java-flag=<string> \n";
    $usage .= "    [--cds-log=<filename>] \n";
    $usage .= "    [--working-dir=(default /scratch/XXXX)] \n";
    $usage .= "    [--view=layout] \n";
    $usage .= "    [--outfile=<filename>] \n";
    die "$usage";
}

while (defined $ARGV[0] && $ARGV[0] =~ /^--(.*)/) {
    ($flag,$value) = split('=',$1);
    if ($flag eq "fulcrum-pdk-root") {
        $fulcrum_pdk_root = $value;
        shift @ARGV;
    } elsif ($flag eq "dfII-dir") {
        $dfII_dir = $value;
        shift @ARGV;
    } elsif ($flag eq "cell") {
        $cell = $value;
        shift @ARGV;
    } elsif ($flag eq "cdl") {
        $cdl = $value;
        shift @ARGV;
    } elsif ($flag eq "cast-path") {
        $cast_path = $value;
        shift @ARGV;
    } elsif ($flag eq "working-dir") {
        $working_dir = $value;
        shift @ARGV;
    } elsif ($flag eq "max-heap-size") {
        $maxHeapSize = $value;
        shift @ARGV;
    } elsif ($flag eq "java-flag") {
        $javaFlag = $value;
        shift @ARGV;
    } elsif ($flag eq "outfile") {
        $out_file = $value;
        shift @ARGV;
    } elsif ($flag eq "view") {
        $view = $value;
        shift @ARGV;
    } elsif ($flag eq "cds-log") {
        $cds_log = $value;
        shift @ARGV;
   } else { 
    shift @ARGV;
    }
}

defined $fulcrum_pdk_root or usage();
defined $dfII_dir or usage();
defined $cell or usage();
defined $cast_path or usage();
defined $cdl or $cdl="";
defined $view or $view="layout";
defined $big_lef or $big_lef=0;
defined $maxHeapSize or $maxHeapSize="512";
if( defined $javaFlag && $javaFlag=~/--max-heap-size=(\S+)/){
$maxHeapSize=$1;
}
$maxHeapSize =~ s/G/000/;
$maxHeapSize =~ s/M//;

$blank_lib="$fulcrum_pdk_root/share/Fulcrum/blank-library";

$pid1=open2(*CELL_IN,*CELL_OUT,"$rename --type=cell --from=cast --to=cadence");

sub cell_rename {
    print CELL_OUT $_[0]."\n";
    my $name=<CELL_IN>;
    chomp($name);
    return $name;
}


defined $working_dir or
    $working_dir  = tempdir( CLEANUP => 1 , DIR => "/scratch" );
if (defined ($ENV{LVE_DIRMODE})) {
    chmod $ENV{LVE_DIRMODE}, "$working_dir";
    if (defined($ENV{LVE_SGID}) and $ENV{LVE_SGID} == 1) {
        `chgrp $ENV{LVE_GID} "$working_dir" &>/dev/null`;
    }
}
else {
    chmod 0755, $working_dir;
}

$cmd="$mkcdswd --target-dir=$working_dir --fulcrum-pdk-root=$fulcrum_pdk_root --cast-path=$cast_path --force --dfII-dir=$dfII_dir";
print "Creating temp dir ($working_dir)...\n";
system($cmd);

open( SOURCE, "<$working_dir/cds.lib");
open( TARGET, ">$working_dir/cds.lib_tmp");
while($line=<SOURCE>){
    $line=~s/\$\{FULCRUM_PDK_ROOT\}/$fulcrum_pdk_root/g;
    print TARGET $line;  
}
close(TARGET);
close(SOURCE);
system("mv $working_dir/cds.lib_tmp $working_dir/cds.lib");

$cellname=cell_rename($cell);
@segments=split('\.', $cellname);
$libname=$segments[0];
for($i=1; $i<=$#segments-2; $i++){
        $libname.=".".$segments[$i];
}

if( -e "$cdl"){
    system("ln -s '$cdl' '$working_dir/temp/${cellname}.cdl'");
}

defined $cds_log or $cds_log="$working_dir/CDS.log";
defined $out_file or $out_file="$working_dir/check_dummy_pin.out";

$skill_file="$working_dir/check_dummy_pin.il";
$pdkInfoIL="$fulcrum_pdk_root/share/Fulcrum/pdkinfo.il";

  open( TARGET, ">$skill_file") or die "Can not open '$skill_file' for writing.\n";
  print TARGET <<SKILL;

cellView=dbOpenCellViewByType( "$libname" "$cellname" "$view" )
if( cellView then
  p=outfile("$out_file")
  if( p then
    nrCreateSingleNetConnector( cellView MetalLPPs
                            ?reportOnly t
                            ?reportPort p
                            ?MaxHeapSize $maxHeapSize
      )

    nrCreateNeighborNetConnector( cellView 
                                  2*KeepoutPrBoundSpacing
                                  MetalLPPs
                                  ?maxFanout 2
                                  ?reportPort p
      )

    close(p)
    status=0
  else
    printf("Can not open $out_file for write.\n")
    status=1
  )
else
  printf("CellView $libname:$cellname:$view does not exist.\n")
  status=1
)
exit(status)
SKILL

  close(TARGET);
  $cmd="cd $working_dir && icca -nograph -replay '$skill_file' -log '$cds_log' </dev/null &>run.err";
  print "Checking Dangling Nodes/Neighbor Connector...\n";
  $ret=system($cmd)>>8;
  open(SOURCE, "<$out_file") or
    die "Dangling Nodes/Neighbor Connector Check fails.\n";
  my $pass = 0;
  my $errors = '';
  while($line=<SOURCE>){
    next if $line =~ /^Processing /;
    $pass++ if $line =~ /^PASS:/;
    $errors .= $line if( $line =~ /^ERROR:/);
  } 
  die "$errors" unless $pass == 2;
  close(SOURCE);
  print "PASS: No dangling node found and no neighbor connector needed.\n"; 
exit(0);
