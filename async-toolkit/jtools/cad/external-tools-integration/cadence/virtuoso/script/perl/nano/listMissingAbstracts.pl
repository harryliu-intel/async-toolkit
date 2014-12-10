#!/usr/bin/perl
#
# Verify presence of dfII abstract cellviews for the specific list of cells.
# Sort list into leaf, mid and power cells.
#
# trj 5/10/06
#
######################################################
# setup
######################################################
require 'ctime.pl';


#
# usage
#
$toolName = "listMissingAbstracts";
$cellListFile = "flatten_cells_list";
$logFile = $toolName . ".log";
$toolRev = '$Revision: #3 $$DateTime: 2006/04/26 11:33:34 $';
$toolRev =~ s/\$//g;
$toolRev =~ s/DateTime: //;
$maxHeapSize = "1800M";

$usage = " 
Usage: $toolName
  --cast-path=<path>            : path to cast and spec directories
  [ --cell-list=<file> ]        : cells in design to verify (default=$cellListFile)
  --design=<top-cell-name>      : name of design
  --dfII-dir=<dir>              : dfII path
  [ --log=<file> ]              : name of log file (default=$logFile)
  [ --max-heap-size=<memory> ]  : java max-heap-size to use for cast_query (default=$maxHeapSize)
  [ --regen-leaf-list ]         : regenerate leaf cell list using cast_query";


#
# default and initial argument values
#
$castPath = "";
$design = "";
$dfiiDir = "";
$regenLeafList = 0;
$debug = 0;


# 
# more inits
#
$logOpen = 0;
$sumOpen = 0;
$errorCnt = 0;
$warningCnt = 0;
$cellListCnt = 0;
$cellListErrors = 0;
$leafCnt = 0;
$missingAbsCnt = 0;
$missingLeafCnt = 0;
$missingMidCnt = 0;
$missingOtherCnt = 0;
$missingCellsList = "missingAbstracts";
$missingLeafFile = $missingCellsList . ".leaf";
$missingMidFile = $missingCellsList . ".mid";
$missingOtherFile = $missingCellsList . ".other";
$leafCellListFile = $toolName . ".leaf";
$userAbsView = "abstract_edit";
$castQueryLog = "cast_query.log";


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

    # --cast-path=<path>
    if ($ARGV[0] =~ /--cast-path=(\S+)/) {
	$castPath = $1;
 	shift(@ARGV);
	next;
    }

    # --cell-list=<file>
    if ($ARGV[0] =~ /--cell-list=(\S+)/) {
	$cellListFile = $1;
 	shift(@ARGV);
	next;
    }

    # --design=<top-cell-name>
    if ($ARGV[0] =~ /--design=(\S+)/) {
	$design = $1;
 	shift(@ARGV);
	next;
    }
    
    # --dfII-dir=<dir>
    if ($ARGV[0] =~ /--dfII-dir=(\S+)/) {
	$dfiiDir = $1;
 	shift(@ARGV);
	next;
    }
    
    # --log=<file>
    if ($ARGV[0] =~ /--log=(\S+)/) {
	$logFile = $1;
 	shift(@ARGV);
	next;
    }

    # --max-heap-size=<memory>
    if ($ARGV[0] =~ /--max-heap-size=(\S+)/) {
	$maxHeapSize = $1;
 	shift(@ARGV);
	next;
    }

    # --regen-leaf-list
    if ($ARGV[0] =~ /--regen-leaf-list/) {
	$regenLeafList = 1;
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
&errorExit("--cast-path not specified.\n $usage \n") unless $castPath;
&errorExit("--design not specified.\n $usage \n") unless $design;
&errorExit("--dfII-dir not specified.\n $usage \n") unless $dfiiDir;


#
# validate paths & files
#
#&errorExit("cast-path '$castPath' not found.\n") unless -e $castPath;
#&errorExit("cast-path '$castPath' not a directory.\n") unless -d $castPath;
&errorExit("cell-list '$cellListFile' not found.\n") unless -e $cellListFile;
&errorExit("dfII-dir '$dfiiDir' not found.\n") unless -e $dfiiDir;
&errorExit("dfII-dir '$dfiiDir' not a directory.\n") unless -d $dfiiDir;


#
# log options
#
open(LOG, ">$logFile") || &errorExit("Unable to open log file '$logFile': $!\n");
$logOpen = 1;
$sumFile = substr($logFile, 0, length($logFile)-3);
$sumFile .= "sum";
open(SUM, ">$sumFile") || &errorExit("Unable to open summary file '$sumFile': $!\n");
$sumOpen = 1;
&echoMsg("\n$toolName, $toolRev\n");
&echoMsg("  $toolName started at ", &ctime(time));
&echoMsg("\n");
&echoMsg("  Cast Path      = $castPath\n");
&echoMsg("  dfII Dir       = $dfiiDir\n");
&echoMsg("  Cell List File = $cellListFile\n");
&echoMsg("  Design         = $design\n");
&echoMsg("\n");


##########################################################################
# create and parse leaf cell list
##########################################################################
#
# create leaf cell list
#
if ($regenLeafList || !-e $leafCellListFile) {
    &echoMsg("Generate leaf cell list...\n");
    system("rm -f $leafCellListFile") if -e $leafCellListFile;
    $castQueryCmd = "cast_query --cadence-name --cast-path=$castPath --cell=$design --task=subcells --filter=leaf --translate=cadence --output=$leafCellListFile --max-heap-size=$maxHeapSize &> $castQueryLog";
    &echoMsg("  Command: $castQueryCmd\n");
    system($castQueryCmd);
    &errorExit("Unable to create leaf cell list. See $castQueryLog.\n") unless -e $leafCellListFile;
}


# 
# parse leaf cell list
#
&echoMsg("Parse leaf cell list...\n");
open(LEAF, $leafCellListFile) || &errorExit("Unable to open leaf cell list '$leafCelllist': $!\n");
while (<LEAF>) {
    if (/(\S+)/) {
	$leafCnt++;
	$leafCellList{&decodeName($1)} = 1;
    }
}
&echoMsg("  Found $leafCnt leaf cells.\n");
close(LEAF);


##########################################################################
# parse cell list
##########################################################################
&echoMsg("Parse cell list...\n");
open(CELL, $cellListFile) || &errorExit("Unable to open leaf cell list '$cellListFile': $!\n");
while (<CELL>) {
    next if /^\s*$/ || /^#/;
    if (/\s*(\S+)\s+(\S+)/) {
	($lib, $cell) = ($1, $2);
	$cellListCnt++;
	$cellList{&decodeName($cell)} = $lib;
    } else {
	$cellListErrors++;
	&warningMsg("Unable to process cell list line '$_'\n");
    }
}
&echoMsg("  Found $cellListCnt cells in cell list.\n");
close(CELL);


##########################################################################
# check for missing abstracts
##########################################################################
&echoMsg("Check for missing abstracts...\n");
foreach $cell (sort(keys %cellList)) {
    &echoMsg("  Check cell $cell...\n") if $debug;

    # verify cell path exists
    $cellPath = join('/', ($dfiiDir, split('\.',$cellList{$cell}), $cell));
    &echoMsg("    Path=$cellPath\n") if $debug;
    if (!-e $cellPath) {
	&errorMsg("Cell '$cell' in cell-list not found in dfII-dir");
	$cellListErrors++;
	next;
    }

    # verify abstract or abstract_edit exists
    $abstractCDB = $cellPath . "/abstract/layout.cdb";
    $abstractEditCDB = $cellPath . "/abstract_edit/layout.cdb";
    next if -e $abstractCDB || -e $abstractEditCDB;

    # not found -- sort into missing cell class
    $missingAbsCnt++;
    &echoMsg("    Abstract not found for &encodeName($cell)\n") if $debug;
    if ($cell =~ /#2ewires#2e/) {
	$missingOtherList[$missingOtherCnt] = &encodeName($cell);
	$missingOtherCnt++;
    } elsif ($leafCellList{$cell}) {
	$missingLeafList[$missingLeafCnt] = &encodeName($cell);
	$missingLeafCnt++;
    } else {
	$missingMidList[$missingMidCnt] = &encodeName($cell);
	$missingMidCnt++;
    }
}


######################################################
# Show results and write out list files
######################################################
&echoMsg("  Discovered $missingAbsCnt missing abstracts.\n");
&echoMsg("\n");

# delete old missing files
system("rm -f $missingLeafFile") if -e $missingLeafFile;
system("rm -f $missingMidFile") if -e $missingMidFile;
system("rm -f $missingOtherFile") if -e $missingOtherFile;

# leaf cells
if ($missingLeafCnt) {
    &echoMsg("The following $missingLeafCnt leaf cells are missing abstracts:\n");
    open(OUT, ">$missingLeafFile") || &errorExit("Unable to open list file '$missingLeafFile': $!\n");
    for($i=0; $i<$missingLeafCnt; $i++) {
	print OUT "$missingLeafList[$i]\n";
	&echoMsg("  $missingLeafList[$i]\n");
    }
    close(OUT);
    &echoMsg("\n");
}

# mid cells
if ($missingMidCnt) {
    &echoMsg("The following $missingMidCnt mid-level cells are missing abstracts:\n");
    open(OUT, ">$missingMidFile") || &errorExit("Unable to open list file '$missingMidFile': $!\n");
    for($i=0; $i<$missingMidCnt; $i++) {
	print OUT "$missingMidList[$i]\n";
	&echoMsg("  $missingMidList[$i]\n");
    }
    close(OUT);
    &echoMsg("\n");
}

# other cells
if ($missingOtherCnt) {
    &echoMsg("The following $missingOtherCnt other cells are missing abstracts:\n");
    open(OUT, ">$missingOtherFile") || &errorExit("Unable to open list file '$missingOtherFile': $!\n");
    for($i=0; $i<$missingOtherCnt; $i++) {
	print OUT "$missingOtherList[$i]\n";
	&echoMsg("  $missingOtherList[$i]\n");
    }
    close(OUT);
    &echoMsg("\n");
}


######################################################
# Summary
######################################################

&echoMsg("\n");
&echoMsg("Summary:\n");
&echoMsg("  Cells in cell-list         = $cellListCnt \n");
&echoMsg("    Cells not checked        = $cellListErrors\n");
&echoMsg("  Total missing abstracts    = $missingAbsCnt\n");
&echoMsg("    Leaf cell abstracts      = $missingLeafCnt\n");
&echoMsg("    Mid-level cell abstracts = $missingMidCnt\n");
&echoMsg("    Other abstracts          = $missingOtherCnt\n");
&echoMsg("\n");
&echoMsg("   WARNINGS: $warningCnt\n");
&echoMsg("     ERRORS: $errorCnt\n");
&echoMsg("Done\t", &ctime(time));

print SUM "Summary:\n";
print SUM "  Cells in cell-list         = $cellListCnt \n";
print SUM "    Cells not checked        = $cellListErrors\n";
print SUM "  Total missing abstracts    = $missingAbsCnt\n";
print SUM "    Leaf cell abstracts      = $missingLeafCnt\n";
print SUM "    Mid-level cell abstracts = $missingMidCnt\n";
print SUM "    Other abstracts          = $missingOtherCnt\n";
print SUM "\n";
print SUM "   WARNINGS: $warningCnt\n";
print SUM "     ERRORS: $errorCnt\n";
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
    if( $sumOpen ) {
        print SUM "$toolName FAILED.\n";
        print SUM "\nERROR: @_ \n";
    }
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

# encode dfii name 
#
# not generic -- only translates #2e,#2d
sub encodeName {
    my $name = $_[0];
    $name =~ s/#2e/./g;
    $name =~ s/#2d/-/g;
    return($name);
}

# escape name
sub escapeName {
    my $name = $_[0];
    my $origName = $name;
    $name =~ s/\[/\\\[/g;
    $name =~ s/\]/\\\]/g;
    $name =~ s/\(/\\\(/g;
    $name =~ s/\)/\\\)/g;
    $name =~ s/\{/\\\{/g;
    $name =~ s/\}/\\\}/g;
    print "    Mapping $origName to $name\n" if $debug2;
    return($name);
}

# max
sub max {
    return($_[0]>$_[1]?$_[0]:$_[1]);
}
