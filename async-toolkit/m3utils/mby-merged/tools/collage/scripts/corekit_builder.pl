#!/usr/intel/bin/perl -w
##################################################################
# Script used for generated corekit builder tcl file based on 
# specified HDL files and top module
##################################################################



##################################################################

use Getopt::Long;
use Carp;
use strict;
use warnings FATAL => qw(all);
use File::Basename;
use Data::Dumper;

my $help_txt = <<HELP;
**************************************************************

  Welcome to $0

  Description:
      Generates builder_<file>.tcl based on provided HDL and top module.
  
  Inputs:
      module_top|m=s  [string] - Top level module that coreKit needs to be generated from
      hdl_file|f=s  [string] - HDL File for lib that coreKit needs to be generated from
      debug|d    [flag]   - Display debug information
      help|h     [flag]   - Display this help information
  
  Outputs:
      Builder File:   builder_<file>.gen.tcl

  Usage:
    1. Display the help information.
       $0 -help     

**************************************************************
HELP


## -------------------------
##   GetOptions Variables
##
my ($module_top,$hdl_file);
my ($help,$debug);
GetOptions(
    "module_top|m=s"  => \$module_top,
    "hdl_file|f=s"    => \$hdl_file,
    "debug|d"       => \$debug,
    "help|h"        => \$help,

) or die "Error parsing: $!\n";

if ($help){
  print $help_txt;
  exit(0);
}

## -------------------------
##   Error Checking
##
my @errors;
push (@errors, "$hdl_file not found\n") unless (-f $hdl_file);
if (scalar @errors > 0){
  print $help_txt;
  print "##############################################################\n";
  print "## ERRORS FOUND IN ARGUMENT PARSING  ##\n";
  print "#  HINT: see above help for usage details\n\n";
  foreach (@errors) {
    print "ERROR: $_\n"; 
  }
  print "\n";
  exit;
}

## -------------------------
##    MAIN
##

# ----------------------------------
# Load vlog files and inc dirs based on hdl file
# ----------------------------------
open( HDL, "<$hdl_file" ) or die "Cannot open file '$hdl_file'\n";
my $slash = $/;    # save original value
undef $/;          # enable whole-file mode
my $contents = <HDL>;    # read the control file
close(HDL);              # close file handle
$/ = $slash;

my $hdl_spec;
die "Error - unable to eval '$hdl_file'" unless eval $contents;
die "Error - no \$hdl_spec found in '$hdl_file'." unless $hdl_spec;

# add prefix on paths
# note:  src files generally point to src, so, only need the mbyc prefix
#  inc dirs can also point to generated content in target subdir
#  could optimize this to only apply certain prefixes depending on 'target' or 'src' location
#  but for now, just apply both prefixes to the inc dirs
my $prefix_mbyc = "\$::env(MODEL_ROOT)/subBlock/mbyc/";
my $prefix = "\$::env(MODEL_ROOT)/";
my @files = join " ", map { $prefix_mbyc . $_ } @{$hdl_spec->{-vlog_files}};
# prefix both, cause some of the paths reference target instead of src
my @rtl_inc_dirs_mbyc = join " ", map { $prefix_mbyc . $_ } @{$hdl_spec->{-vlog_incdirs}};
my @rtl_inc_dirs = join " ", map { $prefix . $_ } @{$hdl_spec->{-vlog_incdirs}};

# ----------------------------------
#  Create builder file 
# ----------------------------------
open (BUILDER, ">builder.$module_top.gen.tcl") or die "ERROR: Unable to open builder.tcl";
print BUILDER <<START;
###########################################################################
# IP info settings
###########################################################################
set ::collage_ip_info::ip_name            "$module_top"
set ::collage_ip_info::ip_top_module_name "$module_top"
set ::collage_ip_info::ip_version         "1.0"
set ::collage_ip_info::ip_intent_sp       ""
set ::collage_ip_info::ip_input_language  SystemVerilog

set ::collage_ip_info::ip_rtl_inc_dirs    "@rtl_inc_dirs_mbyc @rtl_inc_dirs"
set ::collage_ip_info::ip_input_files     "@files"
set ::collage_ip_info::ip_plugin_dir      "" ; # Directories - space separated list - with tcl plugin files
set ::collage_ip_info::ip_ifc_def_hook    "\${::collage_ip_info::ip_top_module_name}_create_ifc_instances" ; # Set this to procedure to add IP interfaces

######################################################################
# Procedure to create IP interfaces
######################################################################
proc \${::collage_ip_info::ip_top_module_name}_create_ifc_instances {} {
  # insert std interfaces to be used here...
  return
}

######################################################################
# Lines below are generic and should not include design specific info
######################################################################
collage_simple_build_flow -exit -copy_corekit

START
close (BUILDER) or die "Unable to close builder_$module_top.tcl: $!\n";


