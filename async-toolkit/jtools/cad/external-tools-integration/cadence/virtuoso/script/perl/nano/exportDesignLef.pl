#!/usr/intel/bin/perl
#
# Generate LEF file from dfII abstract cellviews for the specific list of cells referenced
# in a flatten cellview. Uses Cadence lefout function.
#
# trj 12/6/04
#
######################################################
# setup
######################################################
require 'ctime.pl';
use strict;

my $package_root = $0;
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

#
# usage
#
my $toolName = "exportDesignLef";
my $cellList = "flatten_cells_list";
my $logFile = "$toolName.log";
my $toolRev = '$Revision$$DateTime$';
my $toolRev =~ s/\$//g;
my $toolRev =~ s/DateTime: //;
my $userAbsView = "abstract_edit";

my $usage = "
Usage: $toolName
  [ --cdswd=<dir> ]                : cadence working directory (default=cwd)
  [ --cell-list=<file> ]           : cells in design to export (default=$cellList)
  [ --client-spec=<spec> ]         : p4 client-spec, required for --create-abstract
  [ --create-abstract ]            : invoke createAbstract for missing abstracts
  [ --dfII-dir=<dir> ]             : dfII path, required if not --fix-lef-only
  --design=<top-cell-name>         : name of design
  [ --fix-lef-only ]               : process existing LEF; don't create LEF or abstracts
  --fulcrum-pdk-root=<dir>         : pdk path
  [ --keep-temp-lef ]              : do not delete the intermediate LEF file
  [ --lefin=<file> ]               : input to --fix-lef-only (default=<design>_tmp.lef)
  [ --lefout=<file> ]              : output LEF file (default=<design>.lef)
  [ --log=<file> ]                 : name of log file (default=$logFile)
  [ --write-lef ]                  : overwrite existing output LEF file
  [ --power-grid-cell=<cellName> ] : powergrid cell (default=<lib>.wires.<type>_POWER_GRID_TIEOFF)
  [ --include-USE ]                : insert USE statements on power nets

  Note: User modified abstract cellviews '$userAbsView' will be used whenever found.";


#
# default and initial argument values
#
my $cdswd = '';
my $clientSpec = "";
my $createAbstract = 0;
my $dfiiDir = "";
my $designName = "";
my $fixLefOnly = 0;
my $pdkRoot = "";
my $keepTempLef = 0;
my $lefInFile = "";
my $lefOutFile = "";
my $writeLef = 0;
my $powerGridCellName = "";
my $includeUSE = 0;
my $debug = 0;


#
# more inits
#
my $powerGridCellFound = 0;
my $logOpen = 0;
my $sumOpen = 0;
my $errorCnt = 0;
my $warningCnt = 0;
my $createAbsCnt = 0;
my $userAbsCnt = 0;
my $cellListCnt = 0;
my $macroCnt = 0;
my $cellListErrors = 0;
my $createAbsErrors = 0;
my $invalidAbsCnt = 0;
my $unvalidatedAbsCnt = 0;
my $lefoutOverwrite = "t";
my $leaf = "leaf";
my $mid = "mid";
my $power = "power";
my $foundForeign = 0;
my $foundOrigin = 0;
my $foundSize = 0;
my $obsRectCnt = 0;
my $units = 1000;


#
# post processing variables
#
my $vdd = 'Vdd';
my $gnd = 'GND';
my $pinShape = "ABUTMENT";
my $indentSpace = '       ';
my $metal1PinCnt = 0;
my $metal2PinCnt = 0;
my %pinUse = ( $vdd, 'POWER',
	    $gnd, 'GROUND' );
my %pinsFound = ( $vdd, 0,
	      $gnd, 0 );
my %pinsOmit = ( $vdd, 0,
	      $gnd, 0 );
my %pinsFixedUse = ( $vdd, 0,
		  $gnd, 0 );
my %pinsFixedShape = ( $vdd, 0,
		    $gnd, 0 );
my @layerNames = ( 'METAL1', 'METAL2', 'METAL3', 'METAL4' ,'METAL5', 'METAL6', 'METAL7' );


#
# define commands
#
my $lefoutCmdFile = $toolName . "Out.cmd";
my $lefoutLogFile = $toolName . "Out.log";
my $lefoutCmd = "virtuoso -replay $lefoutCmdFile -nograph -log $lefoutLogFile";
my $createAbstractCmd = "createAbstract";

# command options file to look for in dfII cell directory
my $cmdOptFileName = "abstract.log";
my $cmdOptFileFound = 0;
my $cmdOptSrcView = '';
my @routedSrcViews = ("layout", "layout_pg", "floorplan", "flatten");


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

    # --cdswd=<dir>
    if ($ARGV[0] =~ /--cdswd=(\S+)/) {
	$cdswd = $1;
 	shift(@ARGV);
	next;
    }

    # --cell-list=<file>
    if ($ARGV[0] =~ /--cell-list=(\S+)/) {
	$cellList = $1;
 	shift(@ARGV);
	next;
   }

    # --client-spec=<spec>
    if ($ARGV[0] =~ /--client-spec=(\S+)/) {
	$clientSpec = $1;
 	shift(@ARGV);
	next;
    }

    # --create-abstract
    if ($ARGV[0] eq "--create-abstract") {
	$createAbstract = 1;
	shift(@ARGV);
	next;
    }

    # --dfII-dir=<dir>
    if ($ARGV[0] =~ /--dfII-dir=(\S+)/) {
	$dfiiDir = $1;
 	shift(@ARGV);
	next;
    }

    # --design=<top-cell-name>
    if ($ARGV[0] =~ /--design=(\S+)/) {
	$designName = $1;
 	shift(@ARGV);
	next;
    }

    # --fix-lef-only
    if ($ARGV[0] eq "--fix-lef-only") {
	$fixLefOnly = 1;
	shift(@ARGV);
	next;
    }

    # --fulcrum-pdk-root=<dir>
    if ($ARGV[0] =~ /--fulcrum-pdk-root=(\S+)/) {
	$pdkRoot = $1;
 	shift(@ARGV);
	next;
    }

    # --keep-temp-lef
    if ($ARGV[0] eq "--keep-temp-lef") {
	$keepTempLef = 1;
	shift(@ARGV);
	next;
    }

    # --lefin=<file>
    if ($ARGV[0] =~ /--lefin=(\S+)/) {
	$lefInFile = $1;
 	shift(@ARGV);
	next;
    }

    # --lefout=<file>
    if ($ARGV[0] =~ /--lefout=(\S+)/) {
	$lefOutFile = $1;
 	shift(@ARGV);
	next;
    }

    # --log=<file>
    if ($ARGV[0] =~ /--log=(\S+)/) {
	$logFile = $1;
 	shift(@ARGV);
	next;
    }

    # --write-lef
    if ($ARGV[0] eq "--write-lef") {
	$writeLef = 1;
	shift(@ARGV);
	next;
    }

    # --power-grid-cell=<cellName>
    if ($ARGV[0] =~ /--power-grid-cell=(\S+)/) {
	$powerGridCellName = $1;
 	shift(@ARGV);
	next;
   }

    # --include-USE
    if ($ARGV[0] eq "--include-USE") {
	$includeUSE = 1;
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
# check mutually exclusive arguments
#
&errorExit("Do not specify --create-abstract and --fix-lef-only.\n $usage \n")
    if $createAbstract && $fixLefOnly;
&errorExit("Do not specify --lef-in and without --fix-lef-only.\n $usage \n")
    if $lefInFile && !$fixLefOnly;


#
# check for required arguments
#
&errorExit("--design not specified.\n $usage \n") unless $designName;
&errorExit("--dfII-dir not specified.\n $usage \n") if !$fixLefOnly && !$dfiiDir;
&errorExit("--client-spec not specified. Required for --create-abstract.\n $usage \n")
    if $createAbstract && !$clientSpec;
&errorExit("--fulcrum-pdk-root not specified.\n $usage \n") unless $pdkRoot;


#
# generate timestamp for use in tmp filenames
#
my $cmd = "date +%C%y%m%d%H%M%S";
open(CMD, "$cmd |") || &errorExit("Unable to execute command '$cmd': $!\n");
my $timestamp = <CMD>;
chop($timestamp);
close(CMD);
my $lefoutListFile = "tmp.lefoutlist." . $timestamp;
$lefInFile = "tmp.lef." . $timestamp unless $lefInFile;


#
# define names dependent upon <design>
#
$lefOutFile = $designName . ".lef" unless $lefOutFile;

my @tmp = split('\.', $designName);
my $subType = pop(@tmp);
my $type = pop(@tmp) . "_" . $subType;
my $libName = join('.', @tmp);
$powerGridCellName = $libName . ".wires." . $type . "_POWER_GRID_TIEOFF" unless $powerGridCellName;

#
# validate paths & files
#
my $cdsLib = "cds.lib";
if ($cdswd) {
    &errorExit("cdswd '$cdswd' not found.\n") unless -e $cdswd;
    &errorExit("cdswd '$cdswd' not a directory.\n") unless -d $cdswd;
    chdir($cdswd) || &errorExit("Unable to cd to '$cdswd': $!\n");
    $cdsLib = $cdswd . "/" . $cdsLib;
}
&errorExit("cdswd ".$cdswd." not a valid Cadence working directory.\n") unless -e $cdsLib;
&errorExit("cell-list '$cellList' not found.\n") unless -e $cellList;
&errorExit("dfII-dir '$dfiiDir' not found.\n") if !$fixLefOnly && ! -e $dfiiDir;
&errorExit("dfII-dir '$dfiiDir' not a directory.\n") if !$fixLefOnly && ! -d $dfiiDir;
&errorExit("fulcrum-pdk-root '$pdkRoot' not found.\n") unless -e $pdkRoot;
&errorExit("fulcrum-pdk-root '$pdkRoot' not a directory.\n") unless -d $pdkRoot;
&errorExit("Package root '$package_root' not found.\n") unless -e $package_root;
&errorExit("lefin '$lefInFile' not found.\n") if $lefInFile && $fixLefOnly && !-e $lefInFile;
&errorExit("lefout '$lefOutFile' already exists. Use --write-lef to overwrite.\n $usage \n")
	if -e $lefOutFile && !$writeLef;
$keepTempLef = 1 if $debug;

my %cdslibs=();
my $fcdslib;
if (open ($fcdslib, "<$cdsLib")) {
    while (<$fcdslib>) {
        chomp;
        next if /^\s*#/;
        if (/INCLUDE (\S+)/) {
            my $include=$1;
            $include =~ s/\$\{FULCRUM_PDK_ROOT\}/$pdkRoot/;
            my $dir=$include;
            $dir =~ s/\/[^\/]+$//;
            my $finc;
            open ($finc, "<$include");
            while (<$finc>) {
                chomp;
                if (/^DEFINE (\S+) (\S+)/) {
                    my $lib=$1;
                    my $d="$dir/$2";
                    $lib =~ s/#2e/./g;
                    $lib =~ s/#2d/-/g;
                    $cdslibs{$lib}=$d;
                }
            }
            close $finc;
        }
        if (/^DEFINE (\S+) (\S+)/) {
            my $lib=$1;
            my $d=$2;
            $lib =~ s/#2e/./g;
            $lib =~ s/#2d/-/g;
            $cdslibs{$lib}=$d;
        }
    }
    close $fcdslib;
}
else {
    &errorMsg(" Cannot open $cdsLib\n");
}


#
# define names dependent upon package_root
#
my $skillRoot = "$package_root/share/skill";
my $skillAutoLoad = "$skillRoot/autoload.il";
my $pdkInfoIL = "$pdkRoot/share/Fulcrum/pdkinfo.il";


#
# log options
#
open(LOG, ">$logFile") || &errorExit("Unable to open log file '$logFile': $!\n");
$logOpen = 1;
my $sumFile = substr($logFile, 0, length($logFile)-3);
$sumFile .= "sum";
open(SUM, ">$sumFile") || &errorExit("Unable to open summary file '$sumFile': $!\n");
$sumOpen = 1;
&echoMsg("\n$toolName, $toolRev\n");
&echoMsg("  $toolName started at ", &ctime(time));
&echoMsg("\n");
if ($package_root =~ /tomjones/) {
    &echoMsg("\n");
    &warningMsg("### USING PKGROOT=$package_root ###\n");
    &echoMsg("\n");
}
&echoMsg("Mode options:\n");
&echoMsg("  Create abstracts =", $createAbstract?"TRUE":"FALSE", "\n");
&echoMsg("  Fix LEF only     =", $fixLefOnly?"TRUE":"FALSE", "\n");
&echoMsg("  Keep Temp LEF    =", $keepTempLef?"TRUE":"FALSE", "\n");
&echoMsg("\n");
&echoMsg("Source design data:\n");
&echoMsg("  CDS Working Dir = $cdswd\n") if $cdswd;
&echoMsg("  dfII Dir        = $dfiiDir\n") if $dfiiDir;
&echoMsg("  Design Name     = $designName\n");
&echoMsg("  Cell List File  = $cellList\n");
&echoMsg("  Client Spec     = $clientSpec\n") if $clientSpec;
&echoMsg("  Power Grid Cell = $powerGridCellName\n") unless $powerGridCellName eq "none";
&echoMsg("  LEF Input File  = $lefInFile\n") if $fixLefOnly || $keepTempLef;
&echoMsg("  LEF Output File = $lefOutFile\n");
&echoMsg("\n");

goto PROCESSLEF if ($fixLefOnly);


##########################################################################
# parse cell list, check for missing abstracts and validate timestamps
##########################################################################
#
# parse cell list
#
my @missingAbs=();
my @invalidAbs=();
my @unvalidatedAbs=();
&echoMsg("Evaluating cell list and validating abstracts...\n");
open(LIST, ">$lefoutListFile") || &errorExit("Unable to open temp file '$lefoutListFile': $!\n");
open(CELLS, "$cellList") || &errorExit("Unable to open cell-list '$cellList': $!\n");
while (<CELLS>) {
    next if /^\s*$/ || /^#/;
    if (/\s*(\S+)\s+(\S+)\s+(\S+)/) {
	my ($srcLib, $srcCell, $cellView) = ($1, $2, "abstract");
	my $cellName = &decodeName($srcCell);
	$cellListCnt++;

	# verify cell exists
	&echoMsg("  Validating cell $srcCell \n") if $debug;
	my $srcCellPath = join('/', ($cdslibs{$srcLib}, $cellName));
	if (!-e $srcCellPath) {
	    &errorMsg("Cell '$srcLib:$srcCell' in cell-list not found in dfII-dir");
	    $cellListErrors++;
	    next;
	}

	# verify abstract exists
	&echoMsg("    Looking for abstract cellview(s)\n") if $debug;
	my $cellViewCDB = join('/', ($cdslibs{$srcLib}, $cellName, $cellView, "layout.oa"));
	my $cellViewFound = -e $cellViewCDB ? 1 : 0;
	&echoMsg("      Found $cellView cellview\n") if $debug && $cellViewFound;
	#$cellViewAge = -M $cellViewCDB if $cellViewFound;
	#&echoMsg("        $cellView age=$cellViewAge days\n") if $debug && $cellViewFound;

	my $userViewCDB = join('/', ($cdslibs{$srcLib}, $cellName, $userAbsView, "layout.oa"));
	my $userViewFound = -e $userViewCDB ? 1 : 0;
	&echoMsg("      Found $userAbsView cellview\n") if $debug && $userViewFound;
	#$userViewAge = -M $userViewCDB if $userViewFound;
	#&echoMsg("        $userAbsView age=$userViewAge days\n") if $debug && $userViewFound;

	# check for user modified abstract
	if ($userViewFound) {
	    $cellView = $userAbsView;
	    #$absViewAge = $userViewAge;
	    $userAbsCnt++;
	    &echoMsg("INFO: Using modified cellview $srcCell $cellView\n");

	    # check if generate file exists and if newer
	    #if ($cellViewFound) {
		#if ($cellViewAge < $userViewAge) {
		#    &errorMsg("Generated abstract cellview is more recent than $cellView cellview.\n");
		#}
	    #}
	#} else {
	#    $absViewAge = $cellViewAge;
	}

	# check for missing abstract
	if (!$cellViewFound && !$userViewFound) {
	    push(@missingAbs, "$srcLib $srcCell $cellView");
	    &echoMsg("      NO ABSTRACT FOUND!\n") if $debug;
	    next;
	}

	# validate abstract is current
	my $cmdOptFile = join('/', ($srcCellPath, $cmdOptFileName));
	&echoMsg("    Looking for command option file: $cmdOptFile\n") if $debug;
	&echoMsg("    Looking for command option file\n") if $debug;

	if (-e $cmdOptFile) {
	    $cmdOptFileFound = 1;
	    &echoMsg("      Found command option file: $cmdOptFile\n") if $debug;
	    &echoMsg("      Found command option file\n") if $debug;
	    open(OPT, "$cmdOptFile") || &errorMsg("Unable to open file '$cmdOptFile': $!\n");
            my $absSrcView="layout";
	    while (<OPT>) {
		if (/abstract source view:\s+(\S+)/) {
		    $absSrcView = $1;
		}
	    }
	    close(OPT);

	    &echoMsg("    Looking for abstract source view '$absSrcView'\n") if $debug;
	    my $absSrcViewCDB = join('/', ($srcCellPath, $absSrcView, "layout.oa"));
	    if (-e $absSrcViewCDB) {
		&echoMsg("      Found abstract source view \n") if $debug;

		#
		# types of md5sum results
		# layout/layout.oa: OK                                                 -- validated
		# layout/layout.oa: FAILED                                             -- md5sum mismatch
		# layout/layout.oa: FAILED open or read                                -- source missing
		# md5sum: abstract.log: no properly formatted MD5 checksum lines found  -- md5sum missing
		#
		&echoMsg("      Checking md5sum for source view\n") if $debug;
		my $md5sumResults = `cd $srcCellPath ; md5sum -c $cmdOptFileName 2>&1`;
		chomp $md5sumResults;
		&echoMsg("        Results: '$md5sumResults'\n") if $debug;
		if ($md5sumResults =~ /: OK/) {
		    &echoMsg("        Validation PASS\n") if $debug;
		} elsif ($md5sumResults =~ /: FAILED/) {
		    &echoMsg("        Validation FAIL\n") if $debug;
		    push(@invalidAbs, "$srcCell $absSrcView $cellView");
		} elsif ($md5sumResults =~ /: no properly formatted MD5 checksum lines found/) {
		    &echoMsg("        Unable to validate\n") if $debug;
		    push(@unvalidatedAbs, "$srcCell $absSrcView $cellView");
		} else {
		    &errorMsg("        Validation failure for $srcCell $absSrcView $cellView\n");
		    push(@unvalidatedAbs, "$srcCell $absSrcView $cellView");
		}


		#&echoMsg("    Comparing dates: source '$absSrcView' vs abstract '$cellView'\n") if $debug;
		#$absSrcViewAge = -M $absSrcViewCDB;
        	#&echoMsg("        $absSrcView age=$absSrcViewAge days\n") if $debug;
		#if ($absSrcViewAge < $absViewAge) {
		    #push(@invalidAbs, "$srcCell $absSrcView $cellView");
		    #&errorMsg("Abstract source view '$absSrcView' is more recent than '$cellView' cellview for cell $srcCell.\n");
		#}
	    }

	    # look for routed source views
	    &echoMsg("    Looking for other routed source views\n") if $debug;
	    my $routedSrcView = 0;
	    my $routedSrcViewFound = 0;
	    foreach my $view (@routedSrcViews) {
		$routedSrcView = 1 if $view eq $absSrcView;
		&echoMsg("      Found $view\n") if $debug && $view eq $absSrcView;
	    }
	    if (!$routedSrcView) {
		foreach my $view (@routedSrcViews) {
		    my $viewName = &decodeName($view);
		    my $viewCDB = join('/',($srcCellPath, $viewName, "layout.oa"));
		    &errorMsg("Using source view '$absSrcView' when routed source view '$view' exists for cell $srcCell.\n")
			if -e $viewCDB;
		}
	    }
	} else {
	    print "Ignoring command options file $cmdOptFile\n";
#	    &errorMsg("Abstract not validated due to missing command options file $cmdOptFile\n");
	}
	print LIST "$srcLib $srcCell $cellView\n";

    } else {
	&warningMsg("Unable to parse line of cell-list: \n   $_\n");
    }
}
close(CELLS);
close(LIST);


#
# display list of invalid abstracts
#
if ($#invalidAbs >= 0) {
    $invalidAbsCnt = $#invalidAbs+1;
    &echoMsg("\n  ERROR: $invalidAbsCnt abstract(s) FAILED validation -- source view has changed since abstract generated:\n\n");
    $errorCnt += $invalidAbsCnt;

    # format list: "$srcCell $absSrcView $cellView"
    my $maxCellLen = 0;
    my $maxSrcLen = 0;
    my $maxAbsLen = 0;
    for (my $i=0; $i<=$#invalidAbs; $i++) {
	my @cell = split(' ', $invalidAbs[$i]);
	$maxCellLen = &max($maxCellLen, length($cell[0]));
	$maxSrcLen = &max($maxSrcLen, length($cell[1]));
	$maxAbsLen = &max($maxAbsLen, length($cell[2]));
    }

    #        Source   Abstract
    # Cell    View      View
    #
    my ($f1, $f2, $f3) = ($maxCellLen, $maxSrcLen, $maxAbsLen);
    my $fmt = "    %-*s   %-*s   %-*s\n";
    &echoMsg(sprintf($fmt, $f1, "          ", $f2, "Source", $f3, "Abstract"));
    &echoMsg(sprintf($fmt, $f1, "Cell      ", $f2, " View ", $f3, "  View  "));
    &echoMsg(sprintf($fmt, $f1, "----------", $f2, "------", $f3, "--------"));

    for (my $i=0; $i<=$#invalidAbs; $i++) {
	my @list = split(' ', $invalidAbs[$i]);
	&echoMsg(sprintf($fmt, $f1, $list[0], $f2, $list[1], $f3, $list[2]));
    }
}


#
# display list of unvalidated abstracts
#
if ($#unvalidatedAbs >= 0) {
    $unvalidatedAbsCnt = $#unvalidatedAbs+1;
    &echoMsg("  ERROR: $unvalidatedAbsCnt abstract(s) are unvalidated -- unable to determine if abstract is valid\n\n");
    $errorCnt += $unvalidatedAbsCnt;

    # format list: "$srcCell $absSrcView $cellView"
    my $maxCellLen = 0;
    my $maxSrcLen = 0;
    my $maxAbsLen = 0;
    for (my $i=0; $i<=$#unvalidatedAbs; $i++) {
	my @cell = split(' ', $unvalidatedAbs[$i]);
	$maxCellLen = &max($maxCellLen, length($cell[0]));
	$maxSrcLen = &max($maxSrcLen, length($cell[1]));
	$maxAbsLen = &max($maxAbsLen, length($cell[2]));
    }

    #        Source   Abstract
    # Cell    View      View
    #
    my ($f1, $f2, $f3) = ($maxCellLen, $maxSrcLen, $maxAbsLen);
    my $fmt = "    %-*s   %-*s   %-*s\n";
    &echoMsg(sprintf($fmt, $f1, "          ", $f2, "Source", $f3, "Abstract"));
    &echoMsg(sprintf($fmt, $f1, "Cell      ", $f2, " View ", $f3, "  View  "));
    &echoMsg(sprintf($fmt, $f1, "----------", $f2, "------", $f3, "--------"));

    for (my $i=0; $i<=$#unvalidatedAbs; $i++) {
	my @list = split(' ', $unvalidatedAbs[$i]);
	&echoMsg(sprintf($fmt, $f1, $list[0], $f2, $list[1], $f3, $list[2]));
    }
}
&echoMsg("\n") if $invalidAbsCnt || $unvalidatedAbsCnt;
&echoMsg("  Finished validating abstracts\n");


#
# display list of missing abstracts
#
if ($#missingAbs >= 0) {
    &warningMsg("\nThe following cells are missing abstract cellviews:\n");

    # format list
    my $maxLibLen = 0;
    my $maxCellLen = 0;
    for (my $i=0; $i<=$#missingAbs; $i++) {
	my @cell = split(' ', $missingAbs[$i]);
	$maxLibLen = &max($maxLibLen, length($cell[0]));
	$maxCellLen = &max($maxCellLen, length($cell[1]));
    }

    #             Abstract  Layout  Default
    # Lib   Cell  View      View    CellType
    my ($f1, $f2, $f3, $f4) = ($maxLibLen, $maxCellLen, 6, 5);
    my $fmt = "  %-*s  %-*s  %-*s  %-*s  %-*s\n";
    &echoMsg(sprintf($fmt, $f1, "LibName", $f2, "CellName", $f3, "Source", $f4, "Type"));
    &echoMsg(sprintf($fmt, $f1, "-------", $f2, "--------", $f3, "------", $f4, "----"));

    for (my $i=0; $i<=$#missingAbs; $i++) {
	my @list = split(' ', $missingAbs[$i]);
	if ($list[1] eq $powerGridCellName || $list[1] =~ /POWER.*GRID/) {
	    $type = $power;
	} elsif ($list[0] =~ /^core/) {
	    $type = $mid;
	} else {
	    $type = $leaf;
	}
	push(@list, "layout", $type);
	$missingAbs[$i] = join(" ", @list);
	&echoMsg(sprintf($fmt, $f1, $list[0], $f2, $list[1], $f3, $list[3], $f4, $list[4]));
    }

    if (!$createAbstract) {
	&echoMsg("\nINFO: Use --create-abstract or invoke createAbstract tool to generate missing abstracts.\n");
	&echoMsg("      --create-abstract will use Source and Type as source cellview and abstract cell type.\n");
	goto DONE;
    }
    &echoMsg("\n");
}


######################################################
# invoke createAbstract for missing abstracts
######################################################
if ($#missingAbs >= 0) {
    &echoMsg("\nInvoking createAbstract on missing cells...\n");
    for (my $i=0; $i<=$#missingAbs; $i++) {

	# invoke createAbstract
	my @list = split(' ', $missingAbs[$i]);
	my $cell = "$list[0]:$list[1]:$list[3]";
	my $absLogFile = "createAbstract.$list[1].log";
	$cmd = "$createAbstractCmd --cell=$cell --client-spec=$clientSpec --dfII-dir=$dfiiDir --log=$absLogFile --type=$list[4]";
	&echoMsg("  Command: $cmd\n");
	system($cmd);

	# check log file for errors
	open(ABSLOG, "$absLogFile") || &errorExit("Unable to open log file '$absLogFile': $!\n");
	my $status = "";
	while(<ABSLOG>) {
	    if (/Abstract Generator Status: (\w+)/) {
		$status = $1;
		last;
	    }
	}
	&echoMsg("\n");
	if ($status eq "FAILURE") {
	    &errorMsg("createAbstract FAILED for cell '$cell'\n");
	    $createAbsErrors++;
	} elsif ($status eq "SUCCESS") {
	    &echoMsg("createAbstract SUCCEEDED for cell '$cell'\n");
	} else {
	    &errorMsg("Unable to determine createAbstract status for cell '$cell'\n") unless $status;
	    $createAbsErrors++;
	}
	close(ABSLOG);
	#&echoMsg("\n");
    }
}
if ($cellListErrors || $createAbsErrors) {
    &echoMsg("INFO: Aborting due to earlier errors.\n") ;
    goto DONE;
}


#######################################################
# create lefout command file
#######################################################
open(CMD, ">$lefoutCmdFile") || &errorExit("Unable to open command file '$lefoutCmdFile': $!\n");
print CMD "\;------------------------------------------------------------\n";
print CMD "\;\n";
print CMD "\; LEFOUT command file \n";
print CMD "\;\n";
print CMD "\; Created by: $toolName, $toolRev ", &ctime(time);
print CMD "\;\n";
print CMD "\;   Source design data:\n";
print CMD "\;      CDS Working Dir = $cdswd\n" if $cdswd;
print CMD "\;      dfII Dir        = $dfiiDir\n" if $dfiiDir;
print CMD "\;      Design Name     = $designName\n";
print CMD "\;      Cell List File  = $cellList\n";
print CMD "\;      Client Spec     = $clientSpec\n" if $clientSpec;
print CMD "\;      Power Grid Cell = $powerGridCellName\n" unless $powerGridCellName eq "none";
print CMD "\;      LEF Input File  = $lefInFile\n" if $fixLefOnly || $keepTempLef;
print CMD "\;      LEF Output File = $lefOutFile\n";
print CMD "\;\n";
print CMD "\;------------------------------------------------------------\n";
print CMD "\n";
print CMD "load( \"$skillAutoLoad\" )\n";
print CMD "load( \"$pdkInfoIL\" )\n";
print CMD "\n";
print CMD "\; omit technology info\n";
print CMD "\;gec3SkipLefTECH = t\n";
print CMD "gec3CurrentLEFVersion=\"5.5\"\n";
print CMD "\n";
print CMD "copyViewToAbstractTmp(\"$lefoutListFile\" \"$powerGridCellName\")\n";
print CMD "returnCode = nil \n";
print CMD "returnCode = ldtrLefWriteOA( \"$lefInFile\" \n";
print CMD "                       \"abstract_tmp\"\n";
print CMD "                       \"\"\n";
print CMD "                       \"$lefoutListFile.tmp\"\n";
print CMD "                       \"abstract_tmp\" \"\" t \"5.4\" nil )\n";
print CMD "\n";
print CMD "deleteAbstractTmp(\"$lefoutListFile\")\n" if !$debug;
print CMD "if( returnCode \"SUCCESS\" \"FAILURE\" )\n";
print CMD "exit\n";
close(CMD);


#####################################################
# invoke lefout
######################################################
&echoMsg("\n");
&echoMsg("Invoking lefout...\n");
$cmd = $lefoutCmd;
&echoMsg("  Command: $cmd\n");
system($cmd);


######################################################
# check lefout log for errors
######################################################
##
## NOTE: lefdef.exe has invalid return status so it is not
## possible to check log file or function return code to
## determine success or failure. Instead verify existence
## of temp lef file.
##
if (-e $lefInFile) {
    &echoMsg("  lefout completed successfully.\n");
} else {
    &errorMsg("  lefout FAILED.\n");
    goto DONE;
}


######################################################
# process lef for nanoroute
######################################################
PROCESSLEF:
&echoMsg("\nProcessing lef for Nanoroute...\n") if $debug;
#
# print LEF header
#
open(LEFIN, "$lefInFile") || &errorExit("Unable to open temp LEF file '$lefInFile': $!\n");
open(LEFOUT, ">$lefOutFile") || &errorExit("Unable to open LEF file '$lefOutFile': $!\n");
print LEFOUT "############################################################\n";
print LEFOUT "#\n";
print LEFOUT "# Created by: $toolName, $toolRev" , &ctime(time);
print LEFOUT "#\n";
print LEFOUT "#   Source design data:\n";
print LEFOUT "#      CDS Working Dir = $cdswd \n" if $cdswd;
print LEFOUT "#      dfII Dir        = $dfiiDir\n" if $dfiiDir;
print LEFOUT "#      Design Name     = $designName\n";
print LEFOUT "#      Cell List File  = $cellList\n";
print LEFOUT "#      Client Spec     = $clientSpec\n" if $clientSpec;
print LEFOUT "#      Power Grid Cell = $powerGridCellName\n" unless $powerGridCellName eq "none";
print LEFOUT "#      LEF Input File  = $lefInFile\n" if $fixLefOnly || $keepTempLef;
print LEFOUT "#      LEF Output File = $lefOutFile\n";
print LEFOUT "#\n";
print LEFOUT "############################################################\n";
print LEFOUT "\nVERSION 5.5 \;\n";
print LEFOUT "\nNAMESCASESENSITIVE ON \;\n";
print LEFOUT "\nDIVIDERCHAR \"|\" \;\n";
print LEFOUT "BUSBITCHARS \"<>\" \;\n\n";


my $line="";
my $macroName;
my %layer=();
my $macroX;
my $macroX1;
my $macroX2;
my $pinName;
my $pinNameEsc;
my $layerName;
my @layers=();
my $pinUseStmt="";
my $pinCnt=0;
my $pinOmitCnt=0;
my $missingAbsCnt=0;
my $missingMacroCnt=0;
my $pinFixedUseCnt=0;
my $pinFixedShapeCnt=0;
my $mfgGrid=0.005;
my $start=0;

#
# process macros
#
while (<LEFIN>) {

    # detect grid
    if (/MANUFACTURINGGRID\s+(\S+)\s*;/) {
        $mfgGrid=$1;
    }

    # macro header
    if (/MACRO\s+(.+)\s*/) {
	print LEFOUT;
        $start = 1;
	$macroName = $1;
	$macroCnt++;
	$foundForeign = 0;
	$foundOrigin = 0;
	$foundSize = 0;
	$powerGridCellFound = 1 if $macroName eq $powerGridCellName;
	&echoMsg("  Found Macro $macroName\n") if $debug;
        # add CLASS and SYMMETRY
        my $class = ($macroName eq $powerGridCellName) ? 'COVER' : 'BLOCK';
        print LEFOUT "  CLASS $class ;\n";
        print LEFOUT "  SYMMETRY X Y R90 ;\n";
	next;
    }

    # skip header fields
    next if (!$start);

    # ignore original CLASS
    if (/CLASS (.*)/) {
        next;
    }

    # ignore original SYMMETRY
    if (/SYMMETRY (.*)/) {
        next;
    }

    ######################################################################
    # NOTE: Perl comparison can fail with floating point numbers due to
    # roundoff. Scale geometries by UNITS and use integer comparisons.
    ######################################################################

    # find foreign
    if (/FOREIGN\s+(\S+)\s+(-*\d+\.*\d*)/) {
        my $foreignMacro=$1;
	$macroX1 = int($2 * $units);
	$foundForeign = 1;
	&echoMsg("    Found FOREIGN X=$macroX1\n") if $debug;
        &errorMsg("FOREIGN $foreignMacro does not match MACRO $macroName")
            if $macroName ne $foreignMacro;
	print LEFOUT;
	next;
    }

    # find origin
    if (/ORIGIN\s+(-*\d+\.\d+)/) {
	$macroX = int($1 * $units);
	$foundOrigin = 1;
	&echoMsg("    Found ORIGIN  X=$macroX\n") if $debug;
	print LEFOUT;
	next;
    }

    # find size
    if (/SIZE\s+(\d+\.\d+)/) {
	$macroX2 = int($1 * $units);
	$foundSize = 1;
	&echoMsg("    Found SIZE    X=$macroX2\n") if $debug;
	$macroX2 += $macroX1 if $foundForeign;
	print LEFOUT;
	next;
    }

    # make symmetry universal
    if (/^(\s*)SYMMETRY\s+/) {
        print LEFOUT "${1}SYMMETRY X Y R90 ;\n";
        next;
    }

    # find pins
    if (/PIN\s+(\S*)/) {

	# check for origin and size
	if (!$foundForeign || !$foundSize) {
	    &warningMsg("FOREIGN and/or SIZE not found for macro $macroName\n");
	    $foundForeign = 1;
	    $foundSize = 1;
	    $macroX1 = 0;
	    $macroX2 = 0;
	}

	# process pin
	$pinName = $1;
	$pinNameEsc = &escapeName($pinName);
	&echoMsg("    Found pin $pinName\n") if $debug;
        my $pin = $_;
	do {
	    $line = <LEFIN>;
            my $ok=1;
            if ($line =~ /POLYGON.*;/) {
                my $t=$line;
                $t =~ s/\s+/ /g;
                $t =~ s/^\s//;
                $t =~ s/\s$//;
                my @ln=split(/ /, $t);
                my $npt = ($#ln-1)/2;
                if ($ln[$#ln-1] eq $ln[2] and $ln[$#ln-2] eq $ln[1]) {
                    $npt--; # handle end point == first point
                }
                if ($ok and $npt < 4) { # need 4 points to make a rectangle
                    print STDERR "Warning: removed a $npt point polygon ($t)\n";
                    $warningCnt++;
                    $ok=0;
                }
                if ($ok) {
                    for my $coord (1..$#ln-1) {
                        if ( int(abs($ln[$coord])/$mfgGrid*100+0.5)%100 != 0) {
                            $ok=0;
                            last;
                        }
                    }
                    if (! $ok) {
                        print STDERR "Warning: off grid coordinates found and removed ($t)\n";
                        $warningCnt++;
                    }
                }
            }
	    $pin .= $line if $ok;
	} until ($pin =~ /END\s+\S+/);

	# process power pin
	if ($pinNameEsc eq $vdd || $pinNameEsc eq $gnd) {
	    $pinsFound{$pinName}++;

	    # verify USE
	    if ($pin =~ /USE\s+(\w+)\s+;/) {
	       $pinUseStmt = $1;
	       $pinUseStmt =~ tr/a-z/A-Z/;
	       if ($pinUseStmt ne $pinUse{$pinName}) {
	           &errorMsg("Inconsistent USE '$1' found for pin ($macroName:$pinName)");
	           $pin =~ s/$&/USE $pinUse{$pinName} \;/;
	           $pinsFixedUse{$pinName}++;
	       }
	    } else {
		if ($includeUSE) {
	          #&errorMsg("USE statement not found for pin ($macroName:$pinName)");
	          &echoMsg("    Add 'USE $pinUse{$pinName}'\n") if $debug;
	          $pin =~ s/PORT/USE $pinUse{$pinName} \;\n$indentSpace PORT/;
	          $pinsFixedUse{$pinName}++;
	        }
	    }
	
	    # verify SHAPE
	    if ($pin =~ /SHAPE\s+(\w+)\s+;/) {
		if ($1 ne $pinShape) {
		    &errorMsg("Inconsistent SHAPE '$1' found for pin ($macroName:$pinName)");
		    $pin =~ s/$&/SHAPE $pinShape/;
		    $pinsFixedShape{$pinName}++;
		}
	    } else {
		#&errorMsg("SHAPE statement not found for pin ($macroName:$pinName)");
		&echoMsg("    Add 'SHAPE $pinShape'\n") if $debug;
		$pin =~ s/PORT/SHAPE $pinShape \;\n$indentSpace PORT/;
		$pinsFixedShape{$pinName}++;
	    }
	}

	# warn if pin contains METAL1 or METAL2
	&warningMsg("Pin ($macroName:$pinName) uses LAYER METAL1.") if $pin =~ /METAL1/;
	$metal1PinCnt++ if $pin =~ /METAL1/;
	&warningMsg("Pin ($macroName:$pinName) uses LAYER METAL2.") if $pin =~ /METAL2/;
	$metal2PinCnt++ if $pin =~ /METAL2/;

	print LEFOUT $pin;
	next;
    }

    # find obs
    if (/^\s+OBS\s*$/) {
	print LEFOUT;
	&echoMsg("    Found OBS section\n") if $debug;
        my @lines=();
        my %obsl=();
        $layerName="";
        my $end = " END";
	while($line=<LEFIN>) {
            if ($line =~ /\s+END\s*$/) {
                $end = $line;
                chomp $end;
                last;
            }
	    if ($line =~ /\s+LAYER\s+(\w+)/) {
		$layerName = $1;
                push @{$obsl{$layerName}}, $line;
                my $pl=$line;
                chomp $pl;
                &echoMsg("$pl:$layerName:\n") if $debug;
	    }	
            elsif ($layerName ne "") {
                push @{$obsl{$layerName}}, $line;
            }
	}
        foreach my $ln (sort keys %obsl) {
            my @f=@{$obsl{$ln}};
            chomp @f;
            next if $#f <= 0; # strip empty section(s)
            print LEFOUT join("\n", @f)."\n";
        }
        print LEFOUT "$end\n";;
	next;
    }

    # otherwise
    print LEFOUT;
}
close(LEFIN);
close(LEFOUT);
&errorMsg("power-grid-cell $powerGridCellName not found while processing LEF.")
    unless $powerGridCellFound or $powerGridCellName eq "none";


######################################################
# Summary
######################################################
DONE:
system("rm $lefInFile") if !$keepTempLef && !$fixLefOnly && -e $lefInFile;
system("rm $lefoutListFile") if !$debug && -e $lefoutListFile;

$pinCnt = $pinsFound{$vdd} + $pinsFound{$gnd};
$pinOmitCnt = $pinsOmit{$vdd} + $pinsOmit{$gnd};
$pinFixedUseCnt = $pinsFixedUse{$vdd} + $pinsFixedUse{$gnd};
$pinFixedShapeCnt = $pinsFixedShape{$vdd} + $pinsFixedShape{$gnd};
$missingAbsCnt = $#missingAbs + 1;
$cellListCnt = $macroCnt if $fixLefOnly;
$missingMacroCnt = $cellListCnt - $macroCnt;
&errorMsg("$missingMacroCnt cells not exported to LEF")
    if $missingMacroCnt && !($cellListErrors || $createAbsErrors);

&echoMsg("\n");
&echoMsg("Summary:\n");
&echoMsg("   Cell Abstracts:\n");
&echoMsg("      Cells in cell-list      = $cellListCnt \n");
&echoMsg("      Invalid Abstracts       = $invalidAbsCnt\n");
&echoMsg("      Missing Abstracts       = $missingAbsCnt\n");
&echoMsg("      Errors in cell-list     = $cellListErrors\n");
&echoMsg("      Abstracts Generated     = $createAbsCnt\n");
&echoMsg("      User Modified Abstracts = $userAbsCnt\n");
&echoMsg("\n");
&echoMsg("   LEF Generation:\n");
&echoMsg("      Cells Exported          = $macroCnt\n");
if ($debug) {
    &echoMsg("      Power Pins Found        = $pinCnt ($pinsFound{$vdd}/$pinsFound{$gnd})\n");
    &echoMsg("         Omitted              = $pinOmitCnt ($pinsOmit{$vdd}/$pinsOmit{$gnd})\n");
    &echoMsg("         Fixed USE            = $pinFixedUseCnt ($pinsFixedUse{$vdd}/$pinsFixedUse{$gnd})\n");
    &echoMsg("         Fixed SHAPE          = $pinFixedShapeCnt ($pinsFixedShape{$vdd}/$pinsFixedShape{$gnd})\n");
}
&echoMsg("      Pins Using METAL1       = $metal1PinCnt\n") if $metal1PinCnt;
&echoMsg("      Pins Using METAL2       = $metal2PinCnt\n") if $metal2PinCnt;
&echoMsg("      Shapes Added to OBS     = $obsRectCnt\n");
&echoMsg("\n");
&echoMsg("   WARNINGS: $warningCnt\n");
&echoMsg("     ERRORS: $errorCnt\n");
&echoMsg("Done\t", &ctime(time));
close(LOG);

print SUM "Summary:\n";
print SUM "   Cell Abstracts:\n";
print SUM "      Cells in cell-list      = $cellListCnt \n";
print SUM "      Invalid Abstracts       = $invalidAbsCnt\n";
print SUM "      Missing Abstracts       = $missingAbsCnt\n";
print SUM "      Errors in cell-list     = $cellListErrors\n";
print SUM "      Abstracts Generated     = $createAbsCnt\n";
print SUM "      User Modified Abstracts = $userAbsCnt\n";
print SUM "\n";
print SUM "   LEF Generation:\n";
print SUM "      Cells Exported          = $macroCnt\n";
if ($debug) {
    print SUM "      Power Pins Found        = $pinCnt ($pinsFound{$vdd}/$pinsFound{$gnd})\n";
    print SUM "         Omitted              = $pinOmitCnt ($pinsOmit{$vdd}/$pinsOmit{$gnd})\n";
    print SUM "         Fixed USE            = $pinFixedUseCnt ($pinsFixedUse{$vdd}/$pinsFixedUse{$gnd})\n";
    print SUM "         Fixed SHAPE          = $pinFixedShapeCnt ($pinsFixedShape{$vdd}/$pinsFixedShape{$gnd})\n";
}
print SUM "      Pins Using METAL1       = $metal1PinCnt\n" if $metal1PinCnt;
print SUM "      Pins Using METAL2       = $metal2PinCnt\n" if $metal2PinCnt;
print SUM "      Shapes Added to OBS     = $obsRectCnt\n";
print SUM "\n";
print SUM "   WARNINGS: $warningCnt\n";
print SUM "     ERRORS: $errorCnt\n";
close(SUM);
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
    print "    Mapping $origName to $name\n" if $debug;
    return($name);
}

# max
sub max {
    return($_[0]>$_[1]?$_[0]:$_[1]);
}
