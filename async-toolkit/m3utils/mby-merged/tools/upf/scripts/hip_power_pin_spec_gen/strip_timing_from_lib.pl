#!/usr/intel/bin/perl
# Author: psayyaga
# Date: WW50.3 '13
# Strips out timing information from .lib file
# Use when there is an issue with the size of .lib causing idlsh used 
# to create supply and ground information for UPF runs out of memory
# created for MPHY but can be used on any .lib file
#
#
use Getopt::Long;
use strict;
use warnings;
use FileHandle;

my $lib_name;
my $help;
my $flag = 0;

GetOptions (
        "lib_name=s",     \$lib_name,
        "help",           \$help,
        "h",              \$help
);

if (defined $help || !defined $lib_name) {
    print <<HELP;
usage $0 :
    REQUIRED:
    -lib_name <Path to .lib file to strip out timing information>

HELP
    exit 1;
}

my $fh1 = new FileHandle("${lib_name}", "r") || die;
my $fh2 = new FileHandle("${lib_name}.strip", "w") || die;

print "INFO: Processing $lib_name ... stripping out timing information\n";
while (my $line = <$fh1>) {
  if ($line =~ m/timing \(\) \{/) {
    print $fh2 $line;
    $flag = 1;
    next;
  }

  if ($line =~ m/\s} \/\* end of arc/) {
    $flag = 0;
    print $fh2 $line;
    next;
  }

  if ($flag == 0) {
    print $fh2 $line;
  }

}

print "INFO: Processed $lib_name ... and wrote ${lib_name}.strip\n";

close ($fh1);
close ($fh2);
