#!/usr/intel/bin/perl -w
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
      Generates mby_interface.tcl and builder_<file>.tcl per wrapper file.
  
  Inputs:
      files|f=s  [string] - File that coreKit needs to be generated from
                            Accepts more than one -file <file1> -file <file2>
      param|p=s  [string] - Param files that are needed to that coreKit needs to be generated from
                            (Optional) Only needed if the above files use PARAMs 
      debug|d    [flag]   - Display debug information
      help|h     [flag]   - Display this help information
  
  Outputs:
      Interface File: mby_interface.tcl
      Builder File:   builder_<file>.tcl

  Usage:
    1. Display the help information.
       $0 -help     

**************************************************************
HELP


## -------------------------
##   GetOptions Variables
##
my (@files,@params);
my ($help,$debug);
GetOptions(
    "files|f=s"     => \@files,
    "params|p=s"    => \@params,
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

if (scalar @files == 0){
  push (@errors, "No files to process were provided"); 
} else {
  foreach my $file (@files){
    push (@errors, "$file not found\n") unless (-f $file);
  }
}

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

my $proj = 'mby';
## -------------------------
##    MAIN
##
## --------------------
#   Parameters  
## --------------------
my $param_hash;
foreach my $param (@params){
  my $base =  basename($param);
  $base =~ s/\.s.*//g;
  print "Parsing: $param -- base $base\n";
  open (PARAM, "$param") or die "Unable to open $param: $!\n";
  foreach my $line (<PARAM>){
    next if $line =~ /^\s*\/\/.*$/;
    next unless $line =~ /^\s*(?:local|)param\s+(\S+)\s*=\s*(\S+)\s*;/;
    my ($name,$val) = ($1,$2);
    $$param_hash{"${base}::${name}"}=$val;
    print "     $name=$val\n" if ($debug); 
  } 
  close (PARAM) or die "Unable to close $param: $!\n";
}
# Create a string of the param packages
my $pkg_param_str = join ("|",keys %$param_hash);

## --------------------
#   Interfaces
## --------------------
my $interfaces;
my $wrappers;
my ($intf_name, $inst_name);
my $intf_hash = {};
my $file_name;
foreach my $file (@files){
  my $base =  basename($file);
  $base =~ s/\.s.*//g;
  print "Parsing: $file -- base $base\n";
  open (FILE, "<$file") or die "Unable to open $file: $!";
  my $intf_flag = 0;
  foreach my $line (<FILE>){
    next unless ($line =~ /Blasted Interface/ or $intf_flag);
    #print "LINE: ",$line;
    # Check if line is empty, clear flag if so
    if ($line =~ /^\s*$/){
      printf "INTF END: %s $inst_name \n", $intf_name if ($debug);
      if (exists $$interfaces{$intf_name}){
        my $orig = scalar keys %{$$interfaces{$intf_name}};
        my $new =  scalar keys %$intf_hash;
        printf "    WARN: $file redefining $intf_name (Signals Orig: $orig Signals New: $new)\n";
      }
      # Assign new interface into hash, clean up variables to avoid pollution in following iterations 
      $$interfaces{$intf_name} = $intf_hash;
      $$wrappers{$base}{$inst_name} = $intf_hash;
      $$wrappers{$base}{$file_name} = $file;
      $intf_flag = 0;
      $intf_name = undef;
      $inst_name = undef;
      $intf_hash = {};
    } else {
      if ($line =~ /Blasted Interface\s+(\S+)\s+(\w+)\s*$/){
        $intf_flag = 1;
        ($intf_name,$inst_name) = ($1, $2);    # INST_NAME
        #my $inst_name = $1; # INTF_NAME
        printf "  INTF Found: $inst_name  %s\n", $intf_name;
        push (@{$$wrappers{$base}{insts}}, "$intf_name=$inst_name");
      }
      my ($dir,$type,$name);
      ($dir,$type,$name) = $line =~ /^(\S+)\s*(\S+).*\s+(\w+),/;
      my ($wdth_lft,$wdth_rght) = $line =~ /\[(.*):(.*)\]/;
      if (defined $wdth_lft and ($wdth_lft =~ /$pkg_param_str/ or $wdth_rght =~ /$pkg_param_str/)){
        foreach ($wdth_lft, $wdth_rght){
          my @vars = $_ =~ /\w+::\w+/g;
          foreach my $var (@vars){
            $_ =~ s/$var/$$param_hash{$var}/g;
          }
        }
      }
      my $wdth = (defined $wdth_lft ? ((eval $wdth_lft)-(eval $wdth_rght)+1) : 1);
      
      $name =~ s/${inst_name}_//;
      printf "       DIR: %s TYPE: %s NAME: %s %s\n",($dir,$type,$name,(defined $wdth? "WIDTH: $wdth" : "")) if ($debug);
      $$intf_hash{$name} = {
                              dir   => $dir,
                              type  => $type,
                              name  => $name,
                              width => $wdth,
                           };
    }
  } 
  close (FILE) or die "Unable to close $file: $!";
}

#print "***********************Debug: Finished Interfaces**************************\n" if $debug;
#print Dumper $interfaces if $debug;
#print "***********************Debug: Finished Interfaces**************************\n" if $debug;


# ----------------------------------
#  INTF 
# ----------------------------------
my $interface_tcl = "interface_$proj.tcl";
open (INTF, ">$interface_tcl" ) or die "ERROR: Unable to open $interface_tcl: $!\n"; 
print INTF <<START;
###############################################################################
#------------------------------------
# Autogenerated $0
#------------------------------------
set MBY_Version 2018.09.24

START
foreach my $intf (sort keys %$interfaces){

print INTF <<START;
###############################################################################
#------------------------------------
# Interface: $intf 
#------------------------------------
set Ifc_Id $intf 

# todo: add min/max consumers if relevant
create_interface \$Ifc_Id  \\
    -version \$MBY_Version \\
    -description \\
    \"Automatically Generated $intf.\"

#------------------------------------
# Interface attributes: $intf
#------------------------------------
set_interface_attribute \$Ifc_Id AutoConnectWhen never

START

  foreach my $sig (keys %{$$interfaces{$intf}}){
    my $hash = $$interfaces{$intf}{$sig};
    my $dir  = ($$hash{dir} =~ /output/ ? "fromProvider" : "fromConsumer");
print INTF <<SIG;
create_interface_port $$hash{name} \\
    -interface \$Ifc_Id \\
    -direction $dir \\
    -constant zero \\
    -size $$hash{width} \\
    -separate \\
    -description \\
    \"Automatically Generated port: $$hash{name}.\"

SIG
  }
print INTF <<END;

complete_interface_definition \$Ifc_Id
unset Ifc_Id
###############################################################################

END
}

foreach my $base (sort keys %$wrappers){
  print "Creating Builder for $base\n";
# ----------------------------------
#  BUILDER 
# ----------------------------------
open (BUILDER, ">builder.$base.tcl") or die "ERROR: Unable to open builder.tcl";
print BUILDER <<START;
###########################################################################
# IP info settings
###########################################################################
set ::collage_ip_info::ip_name            "$base"
set ::collage_ip_info::ip_top_module_name "\${::collage_ip_info::ip_name}"
set ::collage_ip_info::ip_version         "1.0"
set ::collage_ip_info::ip_intent_sp       ""
set ::collage_ip_info::ip_input_language  SystemVerilog

set ::collage_ip_info::ip_input_files     "@params $$wrappers{$base}{$file_name}"
set ::collage_ip_info::ip_rtl_inc_dirs    ""
set ::collage_ip_info::ip_plugin_dir      "" ; # Directories - space separated list - with tcl plugin files
set ::collage_ip_info::ip_ifc_def_hook    "\${::collage_ip_info::ip_top_module_name}_create_ifc_instances" ; # Set this to procedure to add IP interfaces

######################################################################
# Procedure to create IP interfaces
######################################################################
proc \${::collage_ip_info::ip_top_module_name}_create_ifc_instances {} {

START

foreach my $inst (@{$$wrappers{$base}{insts}}){
  my ($intf_name,$inst_name) = split (/=/,$inst);
  my $type = "ERROR";
  foreach my $sig (keys %{$$wrappers{$base}{$inst_name}}){
    if ($$wrappers{$base}{$inst_name}{$sig}{dir} eq  $$interfaces{$intf_name}{$sig}{dir}){
      $type = "provider";
    } else {
      $type = "consumer";
    } 
    last;  # really only need to check the first signal
  }
print BUILDER <<INST;
  ############################################
  # INTF: $intf_name INST: $inst_name 
  ############################################
  set ifc_inst_name "$inst_name"
  create_interface_instance \$ifc_inst_name -type $type -interface "$intf_name" -version "2018.09.24" -associationformat "${inst_name}_%s"

  # Open parameters
  set open_parameters { \\
  }

  set open_ports { \\
  }
  collage_set_open_interface -ifc_inst_name \$ifc_inst_name -ports \$open_ports -parameters \$open_parameters

INST
  
}
print BUILDER <<END;
  return
}

######################################################################
# Create workspace (see help for options)
######################################################################
collage_create_workspace
collage_add_ifc_def_files -files dirname($interface_tcl)/basename($interface_tcl)
#collage_add_ifc_def_files -files /nfs/sc/disks/sc_mby_00038/ymhardwi/mby/work_root/test/interface_mby.tcl 

######################################################################
# Lines below are generic and should not include design specific info
######################################################################
collage_simple_build_flow -exit -copy_corekit -dont_create_ws
END

close (BUILDER) or die "Unable to close builder_$base.tcl: $!\n";
}


