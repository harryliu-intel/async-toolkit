#!/usr/bin/perl -w
#
# Generate dfII abstract cellview from routed layout cellview for single cell,
# using Cadence Abstract Generator. P4 submit generated abstract.
# 
# trj  11/29/04
#
######################################################
# setup
######################################################
require 'ctime.pl';
require 'flush.pl';

#
# usage
#
$toolName = "createAbstract";
$toolRev = &ctime(time);
chop($toolRev);
$toolRev = '$Revision$$DateTime$';
$toolRev =~ s/\$//g;
$toolRev =~ s/DateTime: //;
$logFile = "$toolName.log";
$extract = "extract";
$leaf = "leaf";
$mid = "mid";
$stdleaf = "stdleaf";
$power = "power";
$bbLeaf = "bbleaf";
$bbSram = "bbsram";
$absView = "abstract";
$userAbsView = "abstract_edit";
$extAbsView = "abstract_ext";

$usage = "
Usage: $toolName
  [ --cdswd=<dir> ]                      : cadence working directory (default=cwd)
  --cell=<libName>:<cellName>:<viewName> : source dfII cell specification
  [ --client-spec=<spec> ]               : p4 client-spec (required unless type=extract)
  --dfII-dir=<dir>                       : dfII path
  [ --edit-abstract ]                    : edit and regenerate existing abstract
  --fulcrum-pdk-root=<dir>               : pdk path
  [ --log=<file> ]                       : name of log file (default=$logFile)
  [ --metal1pins ]                       : extract METAL1 and METAL2 pins (default=METAL3)
  [ --metal2pins ]                       : extract METAL2 pins (default=METAL3)
  [ --no-submit ]                        : do not submit abstract (default=submit)
  [ --type={$leaf | $mid | $power | $extract | $stdleaf} ] : source cell type (default=$leaf)

  Notes: 
  1. --type=$extract implies --no-submit and --metal1pins. All p4 operations omitted.
  2. --type=$stdleaf implies --metal1pins.";


#
# define celltypes & map to Abstract Generator bins
#
$extractBinName = "Extract";
$stdLeafBinName = "StdLeaf";
$leafBinName = "Leaf";
$midBinName = "MidLevel";
$powerBinName = "PowerGrid";
$bbLeafBinName = "BlackBoxLeaf";
$bbSramBinName = "BlackBoxSram";
%cellTypes = ($extract, $extractBinName,
	      $stdleaf, $stdLeafBinName, 
	      $leaf, $leafBinName, 
	      $mid, $midBinName, 
	      $power, $powerBinName, 
	      $bbLeaf, $bbLeafBinName,
	      $bbSram, $bbSramBinName);


#
# default and initial values
#
$cdswd = '';
$debug = 0;
$updateMd5sum = 0;
$editAbs = 0;
$submitAbs = 1;
$clientSpec = "";
$changeList = "";
$logOpen = 0;
$sumOpen = 0;
$errorCnt = 0;
$warningCnt = 0;
$cellType = $leaf;
$user = $ENV{'USER'};
$cwd = $ENV{'PWD'};
$absExist = 0;
$absP4Exist = 0;
$skipP4 = 0;
$m2pins = 0;
$m1pins = 0;


#
# define files & commands
#
$abGenOptFile = ".abstract.options";
$abGenCmdFile = $toolName . "AG.cmd";
$abGenLogFile = $toolName . "AG.log";
$abGenCmd = "fulcrum ic abstract";  # Note: flow assumes ic = ic.50.33u3
$cdsLib = "cds.lib";


# command options file to look for in dfII cell directory
$cmdOptFileName = "abstract.log";
$evalCmdOpt = 0;
$cmdOptFileFound = 0;
$cmdOpt = '';
$cmdOptSrcView = '';
@routedSrcViews = ("layout", "layout_abs");


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
# ignore command options if type=extract
#
for ($i=0; $i<=$#ARGV; $i++) {
    if ($ARGV[$i] =~ /--type=$extract/) {
	&echoMsg("\nINFO: Ignore command options for extract cell type.\n");
	$evalCmdOpt = 1;
	last;
    }
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

    # --update-md5sum (undocumented arg)
    if ($ARGV[0] eq "--update-md5sum") {
	$updateMd5sum = 1;
	shift(@ARGV);
	next;
    }

    #
    # load additional options once path to file derivable
    #
    if (!$evalCmdOpt && $srcLib && $srcCell && $dfiiDir) {
	$srcLibPath = join('/', ($dfiiDir, split('\.',$srcLib)));
	$cellName = &decodeName($srcCell); 
	$cmdOptFile = join('/', ($srcLibPath, $cellName, $cmdOptFileName));
	$evalCmdOpt = 1;
	&echoMsg("  Looking for command option file: $cmdOptFile\n") if $debug;
	if (-e $cmdOptFile) {
	    $cmdOptFileFound = 1;
	    &echoMsg("    Found command option file: $cmdOptFile\n") if $debug;
	    open(OPT, "$cmdOptFile") || &errorExit("Unable to open file '$cmdOptFile': $!\n");
	    while (<OPT>) {
		if (/$toolName options:\s+/) {
		    $cmdOpt = $';
		    $cmdOpt =~ s/\n//g;
		    &echoMsg("      Command options: $cmdOpt\n") if $debug;
		    @cmdOptList = split('\s+', $cmdOpt);
		    foreach $cmd (@cmdOptList) {
			push(@ARGV, $cmd);
			&echoMsg("        Adding '$cmd' to ARGV\n") if $debug;
		    }
		}

		if (/abstract source view:\s+(\S+)/) {
		    $cmdOptSrcView = $1;
		}
	    }
	    close(OPT);
	}
	next; 
    }

    # --metal2pins
    if ($ARGV[0] eq "--metal2pins") {
	$m2pins = 1;
	shift(@ARGV);
	next;
    }

    # --metal1pins
    if ($ARGV[0] eq "--metal1pins") {
	$m1pins = 1;
	shift(@ARGV);
	next;
    }

    # --cdswd=<dir>
    if ($ARGV[0] =~ /--cdswd=(\S+)/) {
	$cdswd = $1;
 	shift(@ARGV);
	next;
    }
    
    # --cell=<libName>:<cellName>:<viewName>
    if ($ARGV[0] =~ /--cell=(\S+):(\S+):(\S+)/) {
	($srcLib, $srcCell, $srcView) = ($1, $2, $3);
 	shift(@ARGV);
	next;
   }

    # --client-spec=<spec>
    if ($ARGV[0] =~ /--client-spec=(\S+)/) {
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
    
    # --edit-abstract
    if ($ARGV[0] eq "--edit-abstract") {
	$editAbs = 1;
	shift(@ARGV);
	next;
    }

    # --fulcrum-pdk-root=<dir>
    if ($ARGV[0] =~ /--fulcrum-pdk-root=(\S+)/) {
	$pdkRoot = $1;
 	shift(@ARGV);
	next;
    }
    
    # --log=<file>
    if ($ARGV[0] =~ /--log=(\S+)/) {
	$logFile = $1;
 	shift(@ARGV);
	next;
   }

    # --no-submit
    if ($ARGV[0] eq "--no-submit") {
	$submitAbs = 0;
	shift(@ARGV);
	next;
    }

    # --type={leaf | mid | power | extract}
    if ($ARGV[0] =~ /--type=(\w+)/) {
	$cellType = $1;
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
# set implied arguments
#
if ($cellType eq $extract) {
    &echoMsg("INFO: Using special extract settings\n") if $debug;
    $m1pins = 1;
    $submitAbs = 0;
    $skipP4 = 1;
    $absView = $extAbsView;
} elsif ($cellType eq $stdleaf) {
    $m1pins = 1;
}


#
# check for required arguments
#
&errorExit("--cell not specified.\n $usage \n") unless $srcLib;
&errorExit("Invalid cell type '$cellType'.\n $usage \n") unless $cellTypes{$cellType};
&errorExit("--client-spec not specified.\n $usage \n") if !$skipP4 && !$clientSpec;
&errorExit("--dfII-dir not specified.\n $usage \n") unless $dfiiDir;
&errorExit("--fulcrum-pdk-root not specified.\n $usage \n") unless $pdkRoot;

$m2pins = 1 if $m1pins; # inclusive option


#
# validate paths & files
#
if ($cdswd) {
    &errorExit("cdswd '$cdswd' not found.\n") unless -e $cdswd;
    &errorExit("cdswd '$cdswd' not a directory.\n") unless -d $cdswd;
    chdir($cdswd) || &errorExit("Unable to cd to '$cdswd': $!\n");
    $cdsLib = $cdswd . "/" . $cdsLib;
}
&errorExit("cdswd ",$cdswd?$cdswd:$cwd," not a valid Cadence working directory.\n") unless -e $cdsLib;
&errorExit("dfII-dir '$dfiiDir' not found.\n") unless -e $dfiiDir;
&errorExit("dfII-dir '$dfiiDir' not a directory.\n") unless -d $dfiiDir;
&errorExit("fulcrum-pdk-root '$pdkRoot' not found.\n") unless -e $pdkRoot;
&errorExit("fulcrum-pdk-root '$pdkRoot' not a directory.\n") unless -d $pdkRoot;

$abGenCfgFile = $pdkRoot . "/share/Fulcrum/nano/abstract.options.template";
##$abGenCfgFile = "/home/user/tomjones/pdk/hw/layout/tsmc13/pdk/Fulcrum/nano/abstract.options.template";
##&echoMsg("\n");
##&warningMsg("#### USING LOCAL abstract.options.template FILE\n");
&errorExit("Abstract Generator options file '$abGenCfgFile' not found.\n") unless -e $abGenCfgFile;


#
# log options
#
open(LOG, ">$logFile") || &errorExit("Unable to open log file '$logFile': $!\n");
$logOpen = 1;
if ($logFile =~ /(\S+)\.log/) {
    $sumFile = $1 . ".sum";
} else {
    $sumFile = substr($logFile, 0, length($logFile)-3);
    $sumFile .= "sum";
}
open(SUM, ">$sumFile") || &errorExit("Unable to open summary file '$sumFile': $!\n");
$sumOpen = 1;

#
# define command options used
#
$curCmdOpt = "";
$curCmdOpt .= "--metal1pins " if $m1pins;
$curCmdOpt .= "--metal2pins " if $m2pins;
$curCmdOpt .= "--type=$cellType";
$curOpt = $curCmdOpt ? $curCmdOpt : "NONE";
$prevOpt = $cmdOpt ? $cmdOpt : "NONE";
$prevCmdOptFile = $cmdOptFileFound ? $cmdOptFile : "NONE";

&echoMsg("\n$toolName, $toolRev\n");
&echoMsg("  $toolName started at: ", &ctime(time));
&echoMsg("\n");
&echoMsg("Command options:\n");
&echoMsg("  Options File     = $prevCmdOptFile\n");
&echoMsg("  Previous Options = $prevOpt\n");
&echoMsg("  Current Options  = $curOpt\n");
&echoMsg("\n");
if ($cmdOptFileFound) {
    &warningMsg("Command options have changed.\n") if $prevOpt ne $curOpt;
    &warningMsg("Abstract source view has changed.\n") if $srcView ne $cmdOptSrcView;
}

&echoMsg("Source library options:\n");
&echoMsg("  CDS Working Dir = $cdswd\n") if $cdswd;
&echoMsg("  pdkRoot  = $pdkRoot\n") if $debug;
&echoMsg("  dfII-dir = $dfiiDir\n");
&echoMsg("  libName  = $srcLib\n");
&echoMsg("  cellName = $srcCell\n");
&echoMsg("  viewName = $srcView\n");
&echoMsg("  cellType = $cellType\n");
&echoMsg("\n");
&echoMsg("Destination library options:\n");
&echoMsg("  viewName = $absView\n");
&echoMsg("\n");
&echoMsg("NOTE: Allowing METAL2 signal pins.\n") if $m2pins && $cellType ne $stdleaf;
&echoMsg("NOTE: Allowing METAL1 signal pins.\n") if $m1pins && $cellType ne $stdleaf;
&echoMsg("\n") if $m2pins || $m1pins;
&echoMsg("Perforce options:\n");
if ($skipP4) {
    &echoMsg("  NONE\n");
} else {
    &echoMsg("  Edit existing abstract    =", $editAbs?"TRUE":"FALSE", "\n");
    &echoMsg("  Submit generated abstract =", $submitAbs?"TRUE":"FALSE", "\n");
    &echoMsg("  Submission client spec    = $clientSpec\n") if $clientSpec;
}
if ($updateMd5sum) {
    &echoMsg("\n");
    &echoMsg("NOTE: Updating md5sum only. Abstract will not be generated.\n");
    &echoMsg("\n");
}


#
# verify layout.cdb exists for source cellview
#
&echoMsg("    Verify source cellview exists...\n") if $debug;
$viewName = &decodeName($srcView);
$srcViewCDB = join('/',($srcLibPath, $cellName, $viewName, "layout.cdb"));
&echoMsg("      Source db file = $srcViewCDB\n") if $debug;
&errorExit("Source db file '$srcViewCDB' not found.\n") unless -e $srcViewCDB;
&echoMsg("        Source db file EXISTS.\n") if $debug;


# 
# check for final layout source cellview
#
$routedSrcView = 0;
$routedSrcViewFound = 0;
foreach $view (@routedSrcViews) {
    $routedSrcView = 1 if $view eq $srcView;
}    
if (!$routedSrcView) {
    foreach $view (@routedSrcViews) {
	$viewName = &decodeName($view);
	$viewCDB = join('/',($srcLibPath, $cellName, $viewName, "layout.cdb"));
	$routedSrcViewFound = 1 if -e $viewCDB;
	&errorMsg("Using source view '$srcView' when routed source view '$view' exists.\n")
	    if -e $viewCDB;
    }
}


#
# initial p4 actions
#
if (!$skipP4) {

    #
    # verify client spec
    #
    &echoMsg("    Verify client spec exists...\n") if $debug;
    $cmd = "p4 clients";
    &echoMsg("      Command: $cmd\n") if $debug;
    open(CMD, "$cmd |") || &errorExit("Unable to execute command '$cmd': $!\n");
    $clientFound = 0;
    while (<CMD>) {
	if (/Client\s+(\S+)\s+/ && $clientSpec eq $1) {
	    $clientFound = 1;
	    last;
	}
    }
    close(CMD);
    &errorExit("Client-spec '$clientSpec' not found.\n") unless $clientFound;
    &echoMsg("        Client EXISTS.\n") if $debug;

    #
    # create abstract cell log
    #
    &echoMsg("    Create abstract cell log...\n") if $debug;
    $cmd = "createAbstractCellLog --cell=$srcLib:$srcCell:$srcView --client-spec=$clientSpec --dfII-dir=$dfiiDir --opt-str=\"$toolName options: $curCmdOpt\"";
    $cmd .= " --debug" if $debug;
    &echoMsg("      Command: $cmd\n") if $debug;
    &echoMsg("==================================================\n") if $debug;
    open(CMD, "$cmd |") || &errorMsg("Command '$cmd' failed: $!\n");
    @tmp = <CMD>;
    close(CMD);
    &echoMsg(@tmp) if $debug;
    &echoMsg("==================================================\n") if $debug;
}
goto DONE if $updateMd5sum;


#
# check for existing abstract cellview
#
if (!$skipP4) {
    &echoMsg("    Check for existing abstract cellview...\n") if $debug;
    $absCDB = join('/',($srcLibPath, $cellName, "abstract/layout.cdb"));
    &echoMsg("      Abstract db file = $absCDB\n") if $debug;
    $absExist = 1 if -e $absCDB;
    &echoMsg("        Abstract does", $absExist?"EXIST":"NOT exist", "\n") if $debug;
    $userAbsCDB = join('/',($srcLibPath, $cellName, $userAbsView, "layout.cdb"));
    if (-e $userAbsCDB) {
	&echoMsg("\n");
	&warningMsg("Found modified abstract cellview '$userAbsView'.\n") ;
    }
    &errorExit("Abstract already exists. Use --edit-abstract to overwrite.\n") 
	if $absExist && !$editAbs;
    &warningMsg("Missing command options file for existing abstract\n") if $absExist && !$cmdOptFileFound;
}


#
# create change list if submit
#
if ($submitAbs) {
    #
    # create a p4 change specification file
    #
    $cmd = "date +%C%y%m%d%H%M%S";
    open(CMD, "$cmd |") || &errorExit("Unable to execute command '$cmd': $!\n");
    $timestamp = <CMD>;
    close(CMD);

    $changeSpecFile = "tmp." . $timestamp;
    open(TMP, ">$changeSpecFile")  || &errorExit("Unable to create file '$changeSpecFile': $!\n");
    print TMP "Change: new\n\n";
    print TMP "Client: $clientSpec\n\n";
    print TMP "User:   $user\n\n";
    print TMP "Status: new\n\n";
    print TMP "Description:\n";
    print TMP "\tGenerate abstract for cell using $toolName.\n";
    close(TMP);

    #
    # create change spec
    #
    &echoMsg("    Create change spec...\n") if $debug;
    $cmd = "p4 change -i < $changeSpecFile";
    &echoMsg("      Command: $cmd") if $debug;
    open(CMD, "$cmd |")  || &errorExit("Unable to execute command '$cmd': $!\n");
    $tmp = <CMD>;
    close(CMD);
    if ($tmp =~ /Change\s+(\d+)\s+created/) {
	$changeSpec = $1;
    } else {
	&errorExit("Unable to create p4 change specification.\n");
    }
    &echoMsg("  Abstract change spec num  = $changeSpec\n");
    $changeList = "--change-list=$changeSpec";
    system("rm $changeSpecFile");
}


#
# open abstract for edit if it already exists
#
if ($absExist) {
    &echoMsg("\nCheck out cell abstract for edit...\n");

    #
    # find depot path to abstract
    #
    &echoMsg("  Find depot root for cell...\n") if $debug;
    $srcCellPath = join('/', ($srcLibPath, $cellName));
    &echoMsg("    SrcCellPath = $srcCellPath\n") if $debug;
    $p4SrcCellPath = $srcCellPath;
    $p4SrcCellPath =~ s/#/%23/g;
    $cmd = "p4 dirs $p4SrcCellPath";
    &echoMsg("      Command: $cmd\n") if $debug;
    open(CMD, "$cmd |") || &errorExit("Unable to execute command '$cmd': $!\n");
    $tmp = <CMD>;
    close(CMD);
    if ($tmp && $tmp =~ /^\/\/depot\//) {
	$depotRoot = $tmp;
	chop($depotRoot);
	$depotRoot =~ s/\$/\\\$/g;
	&echoMsg("        Depot Root  = $depotRoot\n") if $debug;
    } else {
	&errorMsg("Depot Root not found.\n");
	$skipP4 = 1;
	goto AG;
    }


    #
    # check if abstract exists in perforce
    #
    &echoMsg("  Check if abstract exists in perforce...\n") if $debug;
    $absPath = $p4SrcCellPath . "/abstract";
    &echoMsg("    absPath = $absPath\n") if $debug;
    $cmd = "p4 dirs $absPath";
    &echoMsg("      Command: $cmd\n") if $debug;
    open(CMD, "$cmd |") || &errorExit("Unable to execute command '$cmd': $!\n");
    $tmp = <CMD>;
    close(CMD);
    if ($tmp =~ /^\/\/depot\//) {
	&echoMsg("        Abstract EXISTS in perforce.\n") if $debug;
	$absP4Exist = 1;
    } else {
	&echoMsg("        Abstract does NOT exist in perforce.\n") if $debug;
    }


    #
    # check if it is already open
    #
    if ($absP4Exist) {
	&echoMsg("  Check if abstract already open for edit...\n") if $debug;
	$cmd = "p4 opened";
	&echoMsg("    Command: $cmd\n") if $debug;
	$depotTarget = $depotRoot . "/abstract/layout.cdb";
	&echoMsg("      Target = $depotTarget\n") if $debug;
	open(CMD, "$cmd |") || &errorExit("Unable to execute command '$cmd': $!\n");
	$absOpen = 0;
        $oldChangeNum = 0;
	while (<CMD>) {
	    if (/$depotTarget/) {
		&echoMsg("      Match  = $_") if $debug;
		$absOpen = 1;

                # save change number
                if (/edit change (\d+)/) {
                  $oldChangeNum = $1;
                  &echoMsg("        Old edit change number = $oldChangeNum\n") if $debug;
                }
		last;
	    }
	}
	close(CMD);
	&echoMsg("        Abstract", $absOpen?"OPEN":"NOT open", "\n") if $debug;
    }


    #
    # reopen or edit
    #
    if ($absP4Exist) {
	if (!$absOpen) {
	    $cmd = "cdsp4edit";
	    $cmd .= " $changeList" if $submitAbs;
	    $cmd .= " --dfII-dir=$dfiiDir --view-name=abstract $srcCell";
	    &echoMsg("  Exists and not open -- check out for edit...\n") if $debug;
	    &echoMsg("    Command: $cmd\n");
	    print "\n";
	    open(CMD, "$cmd |") || &errorMsg("Command '$cmd' failed: $!\n");
	    @tmp = <CMD>;
	    close(CMD);
	    &echoMsg(" @tmp");
	} elsif ($submitAbs) {
	    @list = split('/', $depotTarget);
	    pop(@list);
	    $reopenTarget = join('/',(@list, "..."));
	    &echoMsg("  Reopen using current change spec...\n");
	    $cmd = "p4 reopen -c $changeSpec $reopenTarget";
	    &echoMsg("    Command: $cmd\n");
	    print "\n";
	    open(CMD, "$cmd |") || &errorMsg("Command '$cmd' failed: $!\n");
	    @tmp = <CMD>;
	    close(CMD);
	    &echoMsg(" @tmp");

            # delete old change number
            $cmd = "p4 change -d $oldChangeNum > /dev/null";
            &echoMsg("  Delete old change number $oldChangeNum if empty\n") if $debug && $oldChangeNum;
            &echoMsg("    Command: $cmd\n") if $debug && $oldChangeNum;
            system($cmd) if $oldChangeNum;
	} else {
	    &echoMsg("  Already open");
	}
	&echoMsg("\n");
    }
}


######################################################
# create Abstract Generator command file
######################################################
AG:
open(CMD, ">$abGenCmdFile") || &errorExit("Unable to open command file '$abGenCmdFile': $!\n");
print CMD "######################################################\n";
print CMD "#\n";
print CMD "# Abstract Generator command file \n";
print CMD "#\n";
print CMD "# Created by: $toolName, $toolRev\n";
print CMD "#   ", &ctime(time);
print CMD "#\n";
print CMD "# Command options:\n";

if ($cmdOptFileFound) {
    print CMD "#   Options File    = $cmdOptFile\n";
    print CMD "#   Command Options = $cmdOpt\n";
    print CMD "# \n";
} else {
    print CMD "#   Command Options = NONE\n";
    print CMD "# \n";
}

print CMD "#   Source library options:\n";
print CMD "#     dfII-dir = $dfiiDir\n";
print CMD "#     libName  = $srcLib\n";
print CMD "#     cellName = $srcCell\n";
print CMD "#     viewName = $srcView\n";
print CMD "#     cellType = $cellType\n";
print CMD "#\n";
print CMD "#   Destination library options:\n";
print CMD "#     viewName = $absView\n";
print CMD "#\n" if $m2pins || $m1pins;
print CMD "#   NOTE: Allowing METAL2 signal pins.\n" if $m2pins;
print CMD "#   NOTE: Allowing METAL1 signal pins.\n" if $m1pins;
print CMD "#\n";
print CMD "######################################################\n";
print CMD "\n";
print CMD "# load library\n";
print CMD "absSetLibrary \"$srcLib\"\n";
print CMD "\n";
print CMD "# define source view\n";
print CMD "absSetOption \"ViewLayout\" \"$srcView\"\n";
print CMD "\n";

if ($cellType eq $extract) {
    print CMD "# define destination view\n";
    print CMD "absSetOption \"ViewAbstract\" \"abstract_ext\"\n";
    print CMD "\n";
}

print CMD "# reset bins by moving all cells to Block\n";
print CMD "absSelectBinFrom \"Core\" \"PowerGrid\"\n";
print CMD "absSelectCells\n";
print CMD "absMoveSelectedCellsToBin \"Block\"\n";
print CMD "absDeselectBinFrom \"Core\" \"$cellTypes{$power}\"\n";
print CMD "\n";
print CMD "# move target cell to appropriate bin\n";
print CMD "absSelectBinFrom \"Block\" \"Block\"\n";
print CMD "absDeselectCells\n";
print CMD "absSelectCellFrom \"$srcCell\" \"$srcCell\"\n";
print CMD "absMoveSelectedCellsToBin \"$cellTypes{$cellType}\"\n";
print CMD "absDeselectBinFrom \"Block\" \"Block\"\n";
print CMD "absSelectBinFrom \"$cellTypes{$cellType}\" \"$cellTypes{$cellType}\"\n";
print CMD "absDeselectCells\n";
print CMD "\n";

if ($cellType ne $extract) {
    if ($m1pins) {
	print CMD "# allow metal2 and metal1 pins\n";
	print CMD "absSetBinOption  \"$cellTypes{$cellType}\" \"ExtractPinLayersSig\"",
	" \"METAL1 METAL2 METAL3 METAL4 METAL5 METAL6 METAL7\"\n";
	print CMD "absSetBinOption  \"$cellTypes{$cellType}\" \"ExtractPinLayersPwr\"",
	" \"METAL1 METAL2 METAL3 METAL4 METAL5 METAL6 METAL7\"\n";
	print CMD "\n";
    } elsif ($m2pins) {
	print CMD "# allow metal2 pins\n";
	print CMD "absSetBinOption  \"$cellTypes{$cellType}\" \"ExtractPinLayersSig\"",
	" \"METAL2 METAL3 METAL4 METAL5 METAL6 METAL7\"\n";
	print CMD "absSetBinOption  \"$cellTypes{$cellType}\" \"ExtractPinLayersPwr\"",
	" \"METAL2 METAL3 METAL4 METAL5 METAL6 METAL7\"\n";
	print CMD "\n";
    }
}

print CMD "# create abstract and update properties\n";
print CMD "absSelectCellFrom \"$srcCell\" \"$srcCell\"\n";
print CMD "absAbstract\n";
print CMD "absSetCellProp \"$srcCell\" \"symmetry\" \"X Y\"\n";
print CMD "absSetCellProp \"$srcCell\" \"prCellClass\" \"cover\"\n" if ($cellType eq $power);
print CMD "\n";
print CMD "# done\n";
print CMD "absExit\n";
close(CMD);


######################################################
# Invoke Abstract Generator
######################################################
#
# copy AG options file to src lib
#
&errorExit("Abstract Generator options template file '$abGenCfgFile' not found\n") 
    unless -e $abGenCfgFile;
&echoMsg("    Copy $abGenCfgFile to $srcLibPath/$abGenOptFile\n") if $debug;
system("cp $abGenCfgFile $srcLibPath/$abGenOptFile");
open(OPT, "$srcLibPath/$abGenOptFile") || 
    &errorExit("Unable to open options file '$srcLibPath/$abGenOptFile': $!\n");
while (<OPT>) {
    if (/(Revision.*)/) {
        $abGenCfgRev = $1;
        $abGenCfgRev =~ s/\$//g;
        $abGenCfgRev =~ s/\;//g;
        $abGenCfgRev =~ s/DateTime: //;
        last;
    }
}
close(OPT);


#
# append ViewLayout option to end of AG options file
# (This is done to prevent false errors if view is other than 'layout'.
#  It is not clear why this is necessary, but it seems to work.) 
#
if ($srcView ne "layout") {
    open(OPT, ">>$srcLibPath/$abGenOptFile") || 
	&errorExit("Unable to open options file '$srcLibPath/$abGenOptFile': $!\n");
    print OPT "absSetOption( \"ViewLayout\" \"$srcView\")\n";
    close(OPT);
}


#
# invoke AG
#
&echoMsg("\nInvoking Abstract Generator...\n");
&echoMsg("  Using Abstract Generator Options, $abGenCfgRev\n");
$cmd = "$abGenCmd -tcl -nogui -replay $abGenCmdFile -log $abGenLogFile";
&echoMsg("  Command: $cmd\n\n");
open(CMD, "$cmd |") || &errorMsg("Command '$cmd' failed: $!\n");
@tmp = <CMD>;
close(CMD);
&echoMsg(@tmp);
&echoMsg("\n");


######################################################
# check abstract generator log for errors
######################################################
open(AGLOG, "$abGenLogFile") || &errorExit("Unable to open log file '$abGenLogFile': $!\n");
$abGenErrors = 0;
$abGenWarnings = 0;
while (<AGLOG>) {
    $abGenErrors++ if /^\s*ERROR\s+/;
    $abGenWarnings++ if /^\s*\*WARNING\*\s+/;

    # flag dbCopyCellView WARNING as ERROR
    if (/^\s*\*WARNING\*\s+dbCopyCellView:\s*(.*)\s*Copy aborted/) {
	$abGenErrors++;
	$reason = $1;
	if ($lastLine =~ /^\s*\*WARNING\*\s+dbCopyCellView:\s*(.*)/) {
	    $reason = $1;
	}
	&echoMsg("\nERROR: Copy aborted -- $reason.\n\n");
    }
    $lastLine = $_;
}
close(AGLOG);

#
# update accumulated errors and warnings
#
$errorCnt += $abGenErrors;
$warningCnt += $abGenWarnings;
$submitAbs = 0 if $abGenErrors;

#
# log status
#
&echoMsg("  Abstract Generator Status:", $abGenErrors?"FAILURE":"SUCCESS", "\n");
&echoMsg("    Warnings: $abGenWarnings\n");
&echoMsg("      Errors: $abGenErrors\n");
&echoMsg("    Log File: $abGenLogFile\n");
&echoMsg("\n");
goto DONE if $abGenErrors;


######################################################
# submit abstract
######################################################
goto DONE if $skipP4;

#
# p4 add if new cellview
#
if (!$absExist || !$absP4Exist) {
    &echoMsg("Add cell abstract...\n");
    $cmd = "cdsp4add";
    $cmd .= " $changeList" if $submitAbs;
    $cmd .= " --dfII-dir=$dfiiDir --view-name=abstract $srcCell";
    &echoMsg("  Command: $cmd\n");
    print "\n";
    open(CMD, "$cmd |") || &errorMsg("Command '$cmd' failed: $!\n");
    @tmp = <CMD>;
    close(CMD);
    &echoMsg(@tmp);
    &echoMsg("\n");
}	


# 
# p4 submit
#
if ($submitAbs) {
    &echoMsg("Submit cell abstract...\n");
    $cmd = "cdsp4submit --change-list=$changeSpec";
    &echoMsg("  Command: $cmd\n");
    print "\n";
    open(CMD, "$cmd |") || &errorMsg("Command '$cmd' failed: $!\n");
    @tmp = <CMD>;
    close(CMD);
    &echoMsg(@tmp);
    &echoMsg("\n");
}


######################################################
# Done.
######################################################
DONE:
&exitSummary;


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

# normal exit print summary file
sub exitSummary {
    &echoMsg("$toolName finished at ", &ctime(time));
    &echoMsg("  Warnings: $warningCnt\n");
    &echoMsg("    Errors: $errorCnt\n");
    &echoMsg("\n");
    close(LOG);
    print SUM "$toolName finished at ", &ctime(time);
    print SUM "  Warnings: $warningCnt\n";
    print SUM "    Errors: $errorCnt\n";
    close(SUM);
    exit;
}
