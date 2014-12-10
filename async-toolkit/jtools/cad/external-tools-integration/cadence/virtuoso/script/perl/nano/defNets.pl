#!/usr/bin/perl
#
# Process DEF file to remove all sections except NETS.
#
#
# trj 12/14/04
#
######################################################
# setup
######################################################
require 'ctime.pl';

#
# usage
#
$toolName = "defNets";
$toolRev = '$Revision$$DateTime$';
$toolRev =~ s/\$//g;
$toolRev =~ s/DateTime: //;
 
$usage = "
Usage: $toolName
  --defin=<file>      : DEF file to process.
  [ --defout=<file> ] : Output DEF file (default=<defin>.$toolName).
  [ --force ]         : Overwrite output DEF file if it exists.
  [ --special-nets ]  : Include SPECIAL NETS section if found (default=omit)
  [ --ldtrDefRead ]   : Modify output DEF file to support skill ldtrDefRead function";


#
# default and initial argument values
#
$defInFile = "";
$defOutFile = "";
$force = 0;
$inclSnets = 0;
$netsFound = 0;
$ldtrDefRead = 0;


######################################################
# parse command line
######################################################
#
# display usage if no args
#
if ($#ARGV < 0) {
    print "$usage\n\n";
    exit;
}

# 
# parse args
#
while ($#ARGV>=0) {

    # --help
    if ($ARGV[0] eq "--help") {
	print "$usage\n\n";
	exit;
    }

    # --debug (undocumented arg)
    if ($ARGV[0] eq "--debug") {
	$debug = 1;
	shift(@ARGV);
	next;
    }

    # --defin=<file>
    if ($ARGV[0] =~ /--defin=(\S+)/) {
	$defInFile = $1;
 	shift(@ARGV);
	next;
    }

    # --defout=<file>
    if ($ARGV[0] =~ /--defout=(\S+)/) {
	$defOutFile = $1;
 	shift(@ARGV);
	next;
    }

    # --force
    if ($ARGV[0] eq "--force") {
	$force = 1;
	shift(@ARGV);
	next;
    }

    # --special-nets
    if ($ARGV[0] eq "--special-nets") {
	$inclSnets = 1;
	shift(@ARGV);
	next;
    }

    # --ldtrDefRead
    if ($ARGV[0] eq "--ldtrDefRead") {
	$ldtrDefRead = 1;
	shift(@ARGV);
	next;
    }

    # unknown argument
    &errorExit("Unknown or invalid argument '$ARGV[0]'.\n $usage \n");
}


######################################################
# validate inputs
######################################################
#
# check for required arguments
#
&errorExit("Must specify DEF input --defin.\n $usage \n") if !$defInFile;


#
# validate paths & files
#
$defOutFile = $defInFile . ".$toolName" unless $defOutFile;
&errorExit("DEF input '$defInFile' not found.\n $usage \n") if !-e $defInFile;
&errorExit("DEF output '$defOutFile' already exists. Use --force to overwrite.\n") 
    if -e $defOutFile && !$force;


######################################################
# Parse DEF
######################################################
# 
# open DEF files
#
open(DEFIN, "$defInFile") || &errorExit("Unable to open DEF file '$defInFile': $!\n");
open(DEFOUT, ">$defOutFile") || &errorExit("Unable to open DEF file '$defOutFile': $!\n");
print DEFOUT "#######################################################\n";
print DEFOUT "#\n";
print DEFOUT "# Modified by: $toolName, $toolRev ", &ctime(time);
print DEFOUT "#\n";
print DEFOUT "#   DEF Input File: $defInFile\n";
print DEFOUT "#   SPECIALNETS   : ";
print DEFOUT $inclSnets ? "Included (if found)" : "Omit";
print DEFOUT "\n";
print DEFOUT "#\n";
print DEFOUT "#   Skill ldtrDefRead support:\n" if $ldtrDefRead;
print DEFOUT "#      Change BUSBITCHARS to '<>'\n" if $ldtrDefRead;
print DEFOUT "#      Filter '\\' from all names\n" if $ldtrDefRead;
print DEFOUT "#\n" if $ldtrDefRead;
print DEFOUT "#######################################################\n";


#
# copy header
#
while(<DEFIN>) {
    print DEFOUT if /^#|^VERSION|^NAMES|^DIVIDER|^DESIGN|^TECH|^UNITS|^DIEAREA/;

    # change BUSBITCHARS to '<>' if --ldtrDefRead
    if (/^BUSBITCHARS/) {
	if ($ldtrDefRead) {
	    print DEFOUT "BUSBITCHARS \"<>\" ;\n";
	} else {
	    print DEFOUT;
	}
    }


    # optionally include SPECIALNETS
    if (/^SPECIALNETS/ && $inclSnets) {
	print DEFOUT "\n$_";
	while(<DEFIN>) {
	    $line = $_;
	    $line =~ s/\\//g if $ldtrDefRead;
	    print DEFOUT $line;
	    last if /^END SPECIALNETS/;
	}
    }
    
    # find NETS
    if (/^NETS/) {
	$netsFound = 1;
	print DEFOUT "\n$_";
	while(<DEFIN>) {
	    $line = $_;
	    $line =~ s/\\//g if $ldtrDefRead;
	    print DEFOUT $line;
	    last if /^END NETS/;
	}
	print DEFOUT "\nEND DESIGN\n";
	last;
    }
}
close(DEFIN);
close(DEFOUT);

&errorExit("Unexpected EOF while processing DEF.\n") unless $netsFound;
exit;

                                                                                          
######################################################
# functions
######################################################
# error exit
sub errorExit {
    print "\nERROR: @_ \n";
    exit 1;
}
