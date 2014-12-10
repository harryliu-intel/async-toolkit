#!/usr/intel/bin/perl -w
#
# Generate abstract.log file with md5sum of source view and optional
# tool parameters.
# 
# trj  4/27/06
#
######################################################
# setup
######################################################
use strict;

require 'ctime.pl';
require 'flush.pl';


#
# usage
#
my $toolName = "createAbstractCellLog";
my $toolRev = &ctime(time);
chop($toolRev);
$toolRev = '$Revision: #2 $$DateTime: 2006/03/16 13:44:22 $';
$toolRev =~ s/\$//g;
$toolRev =~ s/DateTime: //;
my $logFile = "$toolName.log";
my $cellLogFileName = "abstract.log";

my $usage = "
Usage: $toolName
  --cell=<libName>:<cellName>:<viewName>  : source dfII cell specification
  [ --cell-log=<file> ]                   : name of abstract cell log file (default=$cellLogFileName)
  [ --client-spec=<spec> ]                : p4 client-spec
  --dfII-dir=<dir>                        : dfII path
  [ --tool-log=<file> ]                   : name of tool log file (default=$logFile)
  [ --str=<str> ]                         : optional string to include in log file. Enclose 
                                            in quotes if string contains spaces. Use '\\n' to 
                                            separate multiple lines.";


#
# default and initial values
#
my $debug = 0;
my $clientSpec = "";
my $optStr = "";
my $errorCnt = 0;
my $warningCnt = 0;
my $logOpen = 0;
my $skipP4 = 0;
my $user = $ENV{'USER'};
my $cellLogFileFound = 0;


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
my $srcLib;
my $srcCell;
my $srcView;
my $dfiiDir;
my @optStrList;
my $srcLibPath;
my $cellName;
my $srcCellPath;

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

    # --cell=<libName>:<cellName>:<viewName>
    if ($ARGV[0] =~ /--cell=(\S+):(\S+):(\S+)/) {
	($srcLib, $srcCell, $srcView) = ($1, $2, $3);
 	shift(@ARGV);
	next;
    }

    # --cell-log=<file>
    if ($ARGV[0] =~ /--cell-log=(\S+)/) {
	$cellLogFileName = $1;
 	shift(@ARGV);
	next;
    }

    # --client-spec=<spec>
    if ($ARGV[0] =~ /--client-spec=(\S*)/) {
	$clientSpec = $1;
 	shift(@ARGV);
	next;
    }

    # --dfII-dir=<dir>
    if ($ARGV[0] =~ /--dfII-dir=(\S+)/) {
	$dfiiDir = $1;
 	shift(@ARGV);
	next;
    }
    
    # --tool-log=<file>
    if ($ARGV[0] =~ /--tool-log=(\S+)/) {
	$logFile = $1;
 	shift(@ARGV);
	next;
   }

    # --opt-str=<str>
    if ($ARGV[0] =~ /--opt-str=(.*)/) {
	$optStr = $1;
	@optStrList = split('\\\n', $optStr);
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
if (!$clientSpec) {
    warningMsg("No client-spec given, p4 operations will be skipped.");
    $skipP4=1;
}
&errorExit("--cell not specified.\n $usage \n") unless $srcLib;
&errorExit("--dfII-dir not specified.\n $usage \n") unless $dfiiDir;


#
# validate paths & files
#
&errorExit("dfII-dir '$dfiiDir' not found.\n") unless -e $dfiiDir;
&errorExit("dfII-dir '$dfiiDir' not a directory.\n") unless -d $dfiiDir;
$srcLibPath = join('/', ($dfiiDir, split('\.',$srcLib)));
$cellName = &decodeName($srcCell); 
$srcCellPath = join('/', ($srcLibPath, $cellName));
&errorExit("Source cell path '$srcCellPath' not found.\n") unless -e $srcCellPath;
&errorExit("Source cell path '$srcCellPath' not a directory.\n") unless -d $srcCellPath;


#
# log options
#
if (open(LOG, ">$logFile")) {
    $logOpen = 1;
} else {
    &errorMsg("Unable to open log file '$logFile': $!\n");
}

&echoMsg("\n$toolName, $toolRev\n\t", &ctime(time));
&echoMsg("\n");
&echoMsg("Command options:\n");
&echoMsg("  Lib Name    = $srcLib\n");
&echoMsg("  Cell Name   = $srcCell\n");
&echoMsg("  View Name   = $srcView\n");
&echoMsg("  Client Spec = $clientSpec\n");
&echoMsg("  dfII dir    = $dfiiDir\n");
if ($optStr) {
    &echoMsg("  Opt String  = $optStrList[0]\n");
    for(my $i=1; $i<=$#optStrList; $i++) {
	&echoMsg("                $optStrList[$i]\n");
    }
}
&echoMsg("\n");


#
# find cell log file
#
my $cellLogFile = join('/', ($srcCellPath, $cellLogFileName));
&echoMsg("  Looking for cell log file: $cellLogFile\n") if $debug;
if (-e $cellLogFile) {
    $cellLogFileFound = 1;
    if ($debug) {
	&echoMsg("    Found cell log file\n");
	open(OPT, "$cellLogFile") || &errorMsg("Unable to open file '$cellLogFile': $!\n");
	while(<OPT>) {
	    &echoMsg("      $_");
	}
    }
} else {
    &echoMsg("    Cell log file NOT FOUND\n");
}
 

#
# verify layout.oa exists for source cellview
#
&echoMsg("\n") if $debug;
&echoMsg("  Verify source cellview exists...\n") if $debug;
my $viewName = &decodeName($srcView);
my $srcViewCDB = join('/',($srcCellPath, $viewName, "layout.oa"));
&echoMsg("    Source cdb file = $srcViewCDB\n") if $debug;
&errorExit("Source cdb file '$srcViewCDB' not found.\n") unless -e $srcViewCDB;
&echoMsg("      Source cdb file EXISTS.\n") if $debug;


#
# initial p4 actions
#
if (!$skipP4) {

    #
    # verify client spec
    #
    &echoMsg("\n") if $debug;
    &echoMsg("  Verify client spec exists...\n") if $debug;
    my $cmd = "p4 clients";
    &echoMsg("    Command: $cmd\n") if $debug;
    open(CMD, "$cmd 2>\&1 |") || &errorExit("Unable to execute command '$cmd': $!\n");
    my $clientFound = 0;
    while (<CMD>) {
	if (/Client\s+(\S+)\s+/ && $clientSpec eq $1) {
	    $clientFound = 1;
	    last;
	}
    }
    close(CMD);
    &errorExit("Client-spec '$clientSpec' not found.\n") unless $clientFound;
    &echoMsg("      Client EXISTS.\n") if $debug;


    #
    # create a p4 change specification file
    #
    &echoMsg("\n") if $debug;
    &echoMsg("  Create cell log p4 change spec...\n");
    $cmd = "date +%C%y%m%d%H%M%S";
    open(CMD, "$cmd 2>\&1 |") || &errorExit("Unable to execute command '$cmd': $!\n");
    my $timestamp = <CMD>;
    chop($timestamp);
    close(CMD);
    
    my $cmdChangeSpecFile = "tmp." . $timestamp;
    open(TMP, ">$cmdChangeSpecFile")  || &errorExit("Unable to create file '$cmdChangeSpecFile': $!\n");
    print TMP "Change: new\n\n";
    print TMP "Client: $clientSpec\n\n";
    print TMP "User:   $user\n\n";
    print TMP "Status: new\n\n";
    print TMP "Description:\n";
    print TMP "\tSave abstract options and md5sum.\n";
    close(TMP);
    &echoMsg("    File $cmdChangeSpecFile created.\n") if $debug;


    #
    # create change spec
    #
    $ENV{'P4CLIENT'} = $clientSpec;
    delete $ENV{'P4CONFIG'};
    $cmd = "p4 change -i < $cmdChangeSpecFile";
    &echoMsg("    Command: $cmd\n") if $debug;
    open(CMD, "$cmd 2>\&1 |")  || &errorExit("Unable to execute command '$cmd': $!\n");
    my $tmp = <CMD>;
    close(CMD);
    my $cmdChangeSpec;
    if ($tmp =~ /Change\s+(\d+)\s+created/) {
	$cmdChangeSpec = $1;
    } else {
	&errorExit("Unable to create p4 change specification.\n");
    }
    &echoMsg("    P4 change spec num = $cmdChangeSpec\n");
    my $cmdChangeList = "--change-list=$cmdChangeSpec";
    system("rm $cmdChangeSpecFile");
    
    
    #
    # edit cell logs file if it exists
    #
    if ($cellLogFileFound) {
	&echoMsg("\n") if $debug;
	&echoMsg("  P4 edit cell log...\n");
	$cmd = "cdsp4edit $cmdChangeList --dfII-dir=$dfiiDir --file=$cellLogFileName $srcCell";
	&echoMsg("    Command: $cmd\n") if $debug;
        open(CMD, "$cmd 2>\&1 |") || &errorMsg("Command '$cmd' failed: $!\n");
	my @tmp = <CMD>;
	close(CMD);
	&echoMsg(@tmp);
    }
    
    
    #
    # write cell log file
    #
    &echoMsg("\n") if $debug;
    &echoMsg("    Write cell log file\n") if $debug;
    open(OPT, ">$cellLogFile") || &errorExit("Unable to open file '$cellLogFile': $!\n");
    for(my $i=0; $i<=$#optStrList; $i++) {
	print OPT "$optStrList[$i]\n";
    }
    print OPT "abstract source view: $srcView\n";
    close(OPT);
    
    
    #
    # generate md5sum checksum
    #
    &echoMsg("    Add md5sum to cell log file\n") if $debug;
    my $md5sumCmd = "cd $srcCellPath ; /usr/bin/md5sum $viewName/layout.oa >> $cellLogFileName";
    print "      md5sum Cmd: $md5sumCmd\n" if $debug;
    system(" $md5sumCmd");

    
    #
    # add cell logs file if it does not exist
    #
    if (!$cellLogFileFound) {
	&echoMsg("\n") if $debug;
	&echoMsg("  P4 add cell log...\n");
	$cmd = "cdsp4add $cmdChangeList --dfII-dir=$dfiiDir --file=$cellLogFileName $srcCell";
	&echoMsg("    Command: $cmd\n");
	open(CMD, "$cmd 2>\&1 |") || &errorMsg("Command '$cmd' failed: $!\n");
	my @tmp = <CMD>;
	close(CMD);
	&echoMsg(@tmp);
	&echoMsg("\n");
    }


    #
    # submit cell logs file
    #
    &echoMsg("\n") if $debug;
    &echoMsg("  P4 submit cell log file...\n");
    $cmd = "cdsp4submit $cmdChangeList";
    &echoMsg("    Command: $cmd\n") if $debug;
    open(CMD, "$cmd 2>\&1 |") || &errorMsg("Command '$cmd' failed: $!\n");
    my @tmp = <CMD>;
    close(CMD);
    &echoMsg(@tmp);
    &echoMsg("\n");

    # if change is empty, delete it
    `p4 change -d $cmdChangeSpec 2>/dev/null 1>/dev/null`;
}


######################################################
# Done.
######################################################
DONE:
&echoMsg("$toolName finished at ", &ctime(time));
&echoMsg("  Warnings: $warningCnt\n");
&echoMsg("    Errors: $errorCnt\n");
&echoMsg("\n");
close(LOG);
exit;


######################################################
# functions
######################################################
# echo output to std & log
sub echoMsg {
    print "@_";
    print LOG "@_" if $logOpen;
}
                                                                                                
# error exit
sub errorExit {
    &echoMsg("\nERROR: @_ \n");
    exit 1;
}

# display error msg
sub errorMsg {
    &echoMsg("ERROR: @_\n");
    $errorCnt++;
}

# display warning msg
sub warningMsg {
    &echoMsg("WARNING: @_\n");
    $warningCnt++;
}

# decode dfii name -- change \W to #xx
sub decodeName {
    my $name = $_[0];
    if ($name =~ /\W/) {
	$name = $` . "#" . sprintf("%x", ord($&)) . &decodeName($');
    }
    return($name);
}
