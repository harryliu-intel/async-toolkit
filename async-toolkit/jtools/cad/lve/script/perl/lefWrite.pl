#!/usr/intel/bin/perl
# Copyright 2005 Fulcrum Microsystems.  All rights reserved.
# Authors: Kai He
 
use FileHandle;
use IPC::Open2;
use File::Temp qw/ tempfile tempdir /;

$lef_rename      = "rename";
$rename          = "lef_rename";
$mkcdswd         = "mkcdswd";
$cell_lef        = "cell_lef";
$output_dir      = "$ENV{PWD}";
$cast_dir        = "$ENV{HOME}/hw/cast";
$spec_dir        = "$ENV{HOME}/hw/layout/tsmc13/spec";
$dfII_dir        = "$ENV{HOME}/hw/layout/tsmc13/dfII";
$pdk_root        = "$ENV{FULCRUM_PDK_ROOT}";

sub usage() {
    $usage  = "USAGE: $0 \n";
    $usage .= "    --fulcrum-pdk-root=<path name> default()\n";
    $usage .= "    --root-target-dir=<path>\n";  
    $usage .= "    --global-target-dir=<path>\n";  
    $usage .= "    --dfII-dir=<path name> (used for instantiator views and for mkcdswd)\n";
    $usage .= "    --cell-list=<filename> default()\n";
    $usage .= "    --lef-output=<filename> \n";
    $usage .= "    --big-lef=[0|1] default=0 \n";
    $usage .= "    [--cds-log=<filename>] \n";
    $usage .= "    [--working-dir=(default /scratch/XXXX)] \n";
    $usage .= "    [--extracted-view=layout] \n";
    die "$usage";
}

while (defined $ARGV[0] && $ARGV[0] =~ /^--(.*)/) {
    ($flag,$value) = split('=',$1);
    if ($flag eq "fulcrum-pdk-root") {
        $fulcrum_pdk_root = $value;
        shift @ARGV;
    } elsif ($flag eq "root-target-dir") {
        $root_target_dir = $value;
        shift @ARGV;
    } elsif ($flag eq "global-target-dir") {
        $global_target_dir = $value;
        shift @ARGV;
    } elsif ($flag eq "dfII-dir") {
        $dfII_dir = $value;
        shift @ARGV;
    } elsif ($flag eq "cell-list") {
        $cell_list = $value;
        shift @ARGV;
    } elsif ($flag eq "lef-output") {
        $lef = $value;
        shift @ARGV;
    } elsif ($flag eq "big_lef") {
        $big_lef = $value;
        shift @ARGV;
    } elsif ($flag eq "working-dir") {
        $working_dir = $value;
        shift @ARGV;
    } elsif ($flag eq "extracted-view") {
        $abstract_view = $value;
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
defined $cell_list or usage();
defined $lef or usage();
defined $extracted_view or $extracted_view="layout";
defined $big_lef or $big_lef=0;
defined $working_dir or $working_dir  = tempdir( CLEANUP => 1 );
if (defined ($ENV{LVE_DIRMODE})) {
    chmod $ENV{LVE_DIRMODE}, "$working_dir";
    if (defined($ENV{LVE_SGID}) and $ENV{LVE_SGID} == 1) {
        `chgrp $ENV{LVE_GID} "$working_dir" &>/dev/null`;
    }
}
else {
    chmod 0755, $working_dir;
}
$tech_only_lef="$fulcrum_pdk_root/share/Fulcrum/lefdef/tech_only.lef";

print "Checking all lib lef files ...\n";

$cell_total_filename="$working_dir/cell_total.lef";

if(-e $cell_total_filename){ my_system("rm '$cell_total_filename'");}

open( SOURCE, "<$cell_list") or die "can not open $cell_list for read.\n";
@cellname_list=<SOURCE>;
close( SOURCE );
$subckt_incomplete=0;

foreach $cell (@cellname_list){
  $cell=~s/\s+//g;
  $cell_dir=$cell;
  $cell_dir=~s/\./\//g;
  if(-e "$global_target_dir/$cell_dir/layout/lib.lef" ){
    system("cat '$global_target_dir/$cell_dir/layout/lib.lef' >> '$cell_total_filename' ");
    print "FOUND: subckt $cell lib.lef file in global lve dir.\n";
  }elsif(-e "$root_target_dir/$cell_dir/layout/lib.lef" ){
    system("cat '$root_target_dir/$cell_dir/layout/lib.lef' >> '$cell_total_filename' ");
    print "FOUND: subckt $cell lib.lef file in local lve dir.\n";
  }else{
    print STDERR "Generating: subckt $cell lib.lef file...\n";
    $cmd="mkdir -p '$root_target_dir/$cell_dir/layout'; $cell_lef --fulcrum-pdk-root=$fulcrum_pdk_root --dfII-dir=$dfII_dir --cell='$cell' --lef-output='$root_target_dir/$cell_dir/layout/lib.lef' --cds-log='$root_target_dir/$cell_dir/layout/libLef.log' --abstract-log='$root_target_dir/$cell_dir/layout/createAbstract.log' --extracted-view=$extracted_view --big-lef=$big_lef";
    print "$cmd\n";
    system($cmd);
    if(-e "$root_target_dir/$cell_dir/layout/lib.lef" ){        
      print "GENERATED: subckt $cell lib.lef file.\n";
      system("cat '$root_target_dir/$cell_dir/layout/lib.lef' >> '$cell_total_filename' ");
    } else {
      $subckt_incomplete=1;
      $subckt_list{"$cell"}="$cell_dir";
    }
  }
}
    
if ($subckt_incomplete){
    print "Error: Can not Generate cell_total due to error in generating subckt lef files:\n";
    foreach $cell (keys %subckt_list){
        print "  Error in extracting $cell lib.lef file.\n";
        print STDERR "Error in extracting $cell lib.lef file.\n";
    }
    die; 
} else {
    system("cat '$tech_only_lef' '$cell_total_filename' >'$lef'");
    system("echo 'END LIBRARY' >>'$lef'");
    print "Success: Lef file is at $lef\n";
}
    




