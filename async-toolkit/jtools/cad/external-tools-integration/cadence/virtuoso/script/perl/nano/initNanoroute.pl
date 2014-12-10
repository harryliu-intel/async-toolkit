#!/usr/bin/perl -w
#
# Generate Nanoroute command file to perform the first pass detailed route for
# the specified design. Nanoroute is then invoke using this command file.
# 
# trj  12/14/04
#
######################################################
# setup
######################################################
require 'ctime.pl';

#
# usage
#
$toolName = "initNanoroute";
$toolRev = '$Revision$$DateTime$';
$toolRev =~ s/\$//g;
$toolRev =~ s/DateTime: //;
$logFile = "$toolName.log";
$cmdFile = "$toolName.tcl";
$botLayer = 3;
$topLayer = 5;
$maxOptPassDefault = 20;
$maxOptPass = $maxOptPassDefault;
$trackPitchDef = 0.24;
$trackPitchMin = 0.06;
$no_exit = 0;

$usage = "
Usage: $toolName

  Nanoroute Input Files:
    --design=<top-cell-name>      : name of design
    [ --verilog=<file> ]          : design Verilog file (default=<design>.v)
    [ --lef=<file> ]              : design LEF file (default=<design>.lef)
    [ --def=<file> ]              : design DEF file (default=<design>.def)
    [ --nd-def=<file> ]           : nondefault routing directives DEF
    [ --tracks-def=<file> ]       : routing track DEF
    [ --addl-def=<file> ]         : additional DEF files, 1 file per switch in desired order
    [ --priority-nets=<file> ]    : list of nets for priority routing, design saved after route.
                                    list syntax: 'selectNet <net>', one net per line. 
    [ --out-cmd=<file> ]          : Nanoroute command file (default=$cmdFile)

  Nanoroute Output Files:
    [ --out-priority-db=<file> ]  : db with priority routes (default=<design>_priority)
    [ --out-routed-db=<file> ]    : db with all nets routed (default=<design>_routed)
    [ --out-vias-db=<file> ]      : db with multi-cut vias (default=<design>_routed_via)

  Nanoroute Configuration:
    [ --autoStop-false ]          : true causes routing to abort if number of violations are
                                    too high (default), false will force routing to continue.
    [ --skip-multi-cut-vias ]     : skip routing pass to add multi-cut vias (default is do not
                                    skip), design is saved before and after pass.
    [ --bottom-layer=<num> ]      : bottom routing layer (default=$botLayer)
    [ --top-layer=<num> ]         : top routing layer (default=$topLayer)
    [ --max-pass=<num> ]          : maximum optimization routing passes (default=$maxOptPass)
    [ --bundled ]                 : include bundled channel DEF and TCL
    [ --no-exit]                  : leave interactive session after finishing
  $toolName Tool Control:
    [ --force ]                   : overwrite output files if they exist
    --fulcrum-pdk-root=<dir>      : pdk path
    [ --log=<file> ]              : name of $toolName log file (default=$logFile)

  Other:
    [ --track-pitch=<num> ]       : (OBSOLETE) routing track pitch in microns (default=$trackPitchDef)";


#
#  default and initial argument values
#
$defFile = "";
$design = "";
$force = 0;
$pdkRoot = "";
$lefFile = "";
$ndDefFile = "";
$tracksDefFile = "";
$verilogFile = "";
$debug = 0;
$errorCnt = 0;
$warningCnt = 0;
$logOpen = 0;
$sumOpen = 0;
@addlDefFiles = ();
$priorityNetsFile = "";
$autoStop = 1;
$skipMultiCutVias = 0;
$priorityDB = "";
$routedDB = "";
$viaDB = "";
$bundled = 0;



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

    #
    # Nanoroute Input Files 
    #
    # --design=<top-cell-name>
    if ($ARGV[0] =~ /--design=(\S+)/) {
	$design = $1;
 	shift(@ARGV);
	next;
    }

    # --verilog=<file>
    if ($ARGV[0] =~ /--verilog=(\S+)/) {
	$verilogFile = $1;
 	shift(@ARGV);
	next;
    }

    # --lef=<file>
    if ($ARGV[0] =~ /--lef=(\S+)/) {
	$lefFile = $1;
 	shift(@ARGV);
	next;
    }

    # --def=<file>
    if ($ARGV[0] =~ /--def=(\S+)/) {
	$defFile = $1;
 	shift(@ARGV);
	next;
    }

    # --nd-def=<file>
    if ($ARGV[0] =~ /--nd-def=(\S+)/) {
	$ndDefFile = $1;
 	shift(@ARGV);
	next;
    }

    # --tracks-def=<file>
    if ($ARGV[0] =~ /--tracks-def=(\S+)/) {
	$tracksDefFile = $1;
 	shift(@ARGV);
	next;
    }
    
    # --addl-def=<file>
    if ($ARGV[0] =~ /--addl-def=(\S+)/) {
	push(@addlDefFiles, $1);
 	shift(@ARGV);
	next;
    }

    # --out-cmd=<file>
    if ($ARGV[0] =~ /--out-cmd=(\S+)/) {
	$cmdFile = $1;
 	shift(@ARGV);
	next;
    }
    
    # --priority-nets=<file>
    if ($ARGV[0] =~ /--priority-nets=(\S+)/) {
	$priorityNetsFile = $1;
 	shift(@ARGV);
	next;
    }
    
    #
    # Nanoroute Output Files 
    #
    # --out-priority-db=<file>
    if ($ARGV[0] =~ /--out-priority-db=(\S+)/) {
	$priorityDB = $1;
 	shift(@ARGV);
	next;
    }

    # --out-routed-db=<file>
    if ($ARGV[0] =~ /--out-routed-db=(\S+)/) {
	$routedDB = $1;
 	shift(@ARGV);
	next;
    }

    # --out-vias-db=<file>
    if ($ARGV[0] =~ /--out-vias-db=(\S+)/) {
	$viaDB = $1;
 	shift(@ARGV);
	next;
    }

    #
    # Nanoroute Configuration 
    #
    # --bottom-layer=<num>
    if ($ARGV[0] =~ /--bottom-layer=(\d+)/) {
	$botLayer = $1;
 	shift(@ARGV);
	next;
    }

    # --top-layer=<num>
    if ($ARGV[0] =~ /--top-layer=(\d+)/) {
	$topLayer = $1;
 	shift(@ARGV);
	next;
    }
    
    # --max-pass=<num>
    if ($ARGV[0] =~ /--max-pass=(\d+)/) {
	$maxOptPass = $1;
 	shift(@ARGV);
	next;
    }

    # --autoStop-false
    if ($ARGV[0] =~ /--autoStop-false/) {
	$autoStop = 0;
 	shift(@ARGV);
	next;
    }

    # --skip-multi-cut-vias
    if ($ARGV[0] =~ /--skip-multi-cut-vias/) {
	$skipMultiCutVias = 1;
 	shift(@ARGV);
	next;
    }

    # --bundled
    if ($ARGV[0] =~ /--bundled/) {
	$bundled = 1;
 	shift(@ARGV);
	next;
    }

    # --no-exit
    if ($ARGV[0] =~ /--no-exit/) {
        $no_exit = 1;
        shift (@ARGV);
        next;
    }

    #
    # initNanoroute Tool Control 
    #
    # --force
    if ($ARGV[0] =~ /--force/) {
	$force = 1;
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

    #
    # Other
    #
    # --track-pitch=<file>
    if ($ARGV[0] =~ /--track-pitch=(\d*\.\d+)/) {
	#$trackPitch = $1;
        &echoMsg("\n--track-pitch no longer used.\n");
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
&errorExit("--design not specified. \n $usage \n") unless $design;
&errorExit("--fulcrum-pdk-root not specified. \n $usage \n") unless $pdkRoot;


#
# define parameters dependent on inputs
#
$verilogFile = "$design.v" unless $verilogFile;
$lefFile = "$design.lef" unless $lefFile;
$defFile = "$design.def" unless $defFile;

if ($priorityNetsFile) {
    $priorityDB = $design . "_priority" unless $priorityDB;
    $priorityDefFile = $priorityDB . ".def";
    $priorityNanoDB = $priorityDB . ".enc";
}

$routedDB = $design . "_routed" unless $routedDB;
$routedDefFile = $routedDB . ".def";
$routedNanoDB = $routedDB . ".enc";

if (!$skipMultiCutVias) {
    $viaDB = $design . "_routed_via" unless $viaDB;
    $viaDefFile = $viaDB . ".def";
    $viaNanoDB = $viaDB . ".enc";
}
$trackPitchFile = "tracks_" . $trackPitch*1000 . ".def" if $trackPitch;


#
# validate files and paths
#
&errorExit("--fulcrum-pdk-root dir '$pdkRoot' not found.\n") unless -e $pdkRoot;
&errorExit("--fulcrum-pdk-root '$pdkRoot' not directory.\n") unless -d $pdkRoot;
$techLefFile = $pdkRoot . "/share/Fulcrum/nano/tech.lef";
&errorExit("Technology LEF file '$techLefFile' not found.\n") unless -e $techLefFile;
$initTclFile = $pdkRoot . "/share/Fulcrum/nano/nano.init.tcl";
&errorExit("Nanoroute init file '$initTclFile' not found.\n") unless -e $initTclFile;

# validate existing outputs
if ($priorityNetsFile) {
    &errorExit("Priority nets DEF file '$priorityDefFile' already exists. Use --force to overwrite.\n") 
	if -e $priorityDefFile && !$force;
    $priorityNanoDBDir = $priorityNanoDB . ".dat";
    &errorExit("Nanoroute priority nets DB '$priorityNanoDB' already exists. Use --force to overwrite.\n") 
	if -e $priorityNanoDBDir && !$force;
}

&errorExit("Routed nets DEF file '$routedDefFile' already exists. Use --force to overwrite.\n") 
    if -e $routedDefFile && !$force;
$routedNanoDBDir = $routedNanoDB . ".dat";
&errorExit("Nanoroute routed nets DB '$routedNanoDB' already exists. Use --force to overwrite.\n") 
    if -e $routedNanoDBDir && !$force;

if (!$skipMultiCutVias) {
    &errorExit("Multi-cut via nets DEF file '$viaDefFile' already exists. Use --force to overwrite.\n") 
	if -e $viaDefFile && !$force;
    $viaNanoDBDir = $viaNanoDB . ".dat";
    &errorExit("Nanoroute priority nets DB '$viaNanoDB' already exists. Use --force to overwrite.\n") 
	if -e $viaNanoDBDir && !$force;
}

&errorExit("--track-pitch $trackPitch must be multiple of $trackPitchMin micron\n") 
    if $trackPitch && (($trackPitch*1000)%($trackPitchMin*1000));


######################################################
# Log Options
######################################################
open(LOG, ">$logFile") || &errorExit("Unable to open log file '$logFile': $!\n");
$logOpen = 1;
$sumFile = substr($logFile, 0, length($logFile)-3);
$sumFile .= "sum";
open(SUM, ">$sumFile") || &errorExit("Unable to open summary file '$sumFile': $!\n");
$sumOpen = 1;

&echoMsg("\n$toolName, $toolRev \n");
&echoMsg("\n");
&echoMsg("Nanoroute Input Files:\n");
&echoMsg("   Design Name     = $design\n");
&echoMsg("   Design Verilog  = $verilogFile\n");
&echoMsg("   Design LEF      = $lefFile\n");
&echoMsg("   Design DEF      = $defFile\n");
&echoMsg("   Nondefault DEF  = $ndDefFile\n") if $ndDefFile;
for($i=0; $i<=$#addlDefFiles; $i++) {
    &echoMsg("   Additional DEF  = $addlDefFiles[$i]\n");
}
&echoMsg("   Tracks DEF      = $tracksDefFile\n") if $tracksDefFile;
&echoMsg("   Track Pitch DEF = $trackPitchFile\n") if $trackPitchFile;
&echoMsg("   Priority Nets   = $priorityNetsFile\n") if $priorityNetsFile;
&echoMsg("   Command File    = $cmdFile\n");
&echoMsg("\n");

# validate input files
&errorMsg("--verilog file '$verilogFile' not found.") unless -e $verilogFile;
&errorMsg("--lef file '$lefFile' not found.") unless -e $lefFile;
&errorMsg("--def file '$defFile' not found.") unless -e $defFile;
&errorMsg("--nd-def file '$ndDefFile' not found.") if $ndDefFile && !-e $ndDefFile;
for($i=0; $i<=$#addlDefFiles; $i++) {
    &errorMsg("--addl-def file '$addlDefFiles[$i]' not found.") unless -e $addlDefFiles[$i]
}
&errorMsg("--tracks-def file '$tracksDefFile' not found.") if $tracksDefFile && !-e $tracksDefFile;
&errorMsg("--priority-nets file '$priorityNetsFile' not found.") if $priorityNetsFile && !-e $priorityNetsFile;

&echoMsg("\n") if $errorCnt;

&echoMsg("Nanoroute Output Files:\n");
&echoMsg("   Priority Nets DB   = $priorityDB\n") if $priorityNetsFile;
&echoMsg("   Routed Nets DB     = $routedDB\n");
&echoMsg("   Multi-Cut Vias DB  = $viaDB\n") unless $skipMultiCutVias;
&echoMsg("\n");

&echoMsg("Nanoroute Options:\n");
&echoMsg("   Route Auto Stop      = ");
$autoStop?&echoMsg("true"):&echoMsg("false");
&echoMsg("\n");
&echoMsg("   Route Priority Nets  = ");
$priorityNetsFile?&echoMsg("true"):&echoMsg("false");
&echoMsg("\n");
&echoMsg("   Skip Multi-Cut Vias  = ");
$skipMultiCutVias?&echoMsg("true"):&echoMsg("false");
&echoMsg("\n");
&echoMsg("   Bottom Routing Layer = $botLayer\n");
&echoMsg("   Top Routing Layer    = $topLayer\n");
&echoMsg("   Use Bundled Channels = $bundled\n");
&echoMsg("   Max Passes           = $maxOptPass");
&echoMsg(" (default)") if $maxOptPass == $maxOptPassDefault;
&echoMsg("\n");



&echoMsg("   Track Pitch  = $trackPitch\n") if $trackPitch;
&echoMsg("\n");


######################################################
# Create track pitch file
######################################################
if ($trackPitch) {
    &echoMsg("Generating routing tracks def file...\n");
    $cmd = "genTracks --refdef=$defFile --outfile=$trackPitchFile";
    $cmd .= " --track-pitch=$trackPitch --force";
    &echoMsg("   Command: $cmd\n   ") if $debug;
    open(CMD, "$cmd |") || &errorMsg("Command '$cmd' failed: $!\n");
    @tmp = <CMD>;
    close(CMD);
    &echoMsg("  ", @tmp);
    &echoMsg("\n");
}


######################################################
# Create command file
######################################################
open(CMD, ">$cmdFile") || &errorExit("Unable to open command file '$cmdFile': $!\n");

#
# log options
#
print CMD "########################################################################################\n";
print CMD "#\n";
print CMD "# Created by $toolName, $toolRev \t", &ctime(time);
print CMD "#\n";
print CMD "# Nanoroute Input Files:\n";
print CMD "#   Design Name     = $design\n";
print CMD "#   Design Verilog  = $verilogFile\n";
print CMD "#   Design LEF      = $lefFile\n";
print CMD "#   Design DEF      = $defFile\n";
print CMD "#   Nondefault DEF  = $ndDefFile\n" if $ndDefFile;
for($i=0; $i<=$#addlDefFiles; $i++) {
    print CMD "#   Additional DEF  = $addlDefFiles[$i]\n";
}
print CMD "#   Tracks DEF      = $tracksDefFile\n" if $tracksDefFile;
print CMD "#   Track Pitch DEF = $trackPitchFile\n" if $trackPitchFile;
print CMD "#   Priority Nets   = $priorityNetsFile\n" if $priorityNetsFile;
print CMD "#   Command File    = $cmdFile\n";
print CMD "#\n";
print CMD "# Nanoroute Output Files:\n";
print CMD "#   Priority Nets DB   = $priorityDB\n" if $priorityNetsFile;
print CMD "#   Routed Nets DB     = $routedDB\n";
print CMD "#   Multi-Cut Vias DB  = $viaDB\n" unless $skipMultiCutVias;
print CMD "#\n";
print CMD "# Nanoroute Options:\n";
print CMD "#   Route Auto Stop      = ";
$autoStop?print CMD "true":print CMD "false";
print CMD "\n";
print CMD "#   Route Priority Nets  = ";
$priorityNetsFile?print CMD "true":print CMD "false";
print CMD "\n";
print CMD "#   Skip Multi-Cut Vias  = ";
$skipMultiCutVias?print CMD "true":print CMD "false";
print CMD "\n";
print CMD "#   Bottom Routing Layer = $botLayer\n";
print CMD "#   Top Routing Layer    = $topLayer\n";
print CMD "#   Max Passes           = $maxOptPass";
print CMD " (default)" if $maxOptPass == $maxOptPassDefault;
print CMD "\n";
print CMD "#   Track Pitch          = $trackPitch\n" if $trackPitch;
print CMD "#\n";
print CMD "########################################################################################\n";
print CMD "#\n";
print CMD "# Command line usage: \n";
print CMD "#   fulcrum encounter -init <cmdFile> [-64] [-SOCE|-socel] [-nowin]\n";
print CMD "#     -init   Load and run cmdFile (this script). Script must exit when using qsub.\n";
print CMD "#     -64     64-bit executable. Implied on 64-bit machines. Required for >4GB memory.\n";
print CMD "#     -SOCE   Use full SOC Encounter license (Default).\n";
print CMD "#     -socel  Use limited SOC Encounter DBS license.\n";
print CMD "#     -nowin  No graphics window, ie command line mode. Required for qsub.\n";
print CMD "#\n";
print CMD "\n";


#
# include init file
#
open(INIT, "$initTclFile") || &errorExit("Unable to open Nanoroute init file '$initTclFile': $!\n");
while (<INIT>) {
    print CMD $_;
}
close(INIT);


#
# setup design options
#
print CMD "\n";
print CMD "#\n";
print CMD "# user defined mode settings\n";
print CMD "#\n";
print CMD "setNanoRouteMode -quiet -routeTopRoutingLayer $topLayer\n";
print CMD "setNanoRouteMode -quiet -routeBottomRoutingLayer $botLayer\n";
print CMD "setNanoRouteMode -quiet -drouteEndIteration ";
if ($maxOptPass == $maxOptPassDefault) {
    print CMD "default\n";
} else {
    print CMD "$maxOptPass\n";
}
print CMD "setNanoRouteMode -quiet -drouteAutoStop ";
$autoStop?print CMD "true":print CMD "false";
print CMD "\n";
print CMD "#########################################################################################\n";
print CMD "# To force Nanoroute to continue as specified by the drouteEndIteration \n";
print CMD "# variable despite the number of violations, set drouteAutoStop to false\n";
print CMD "#########################################################################################\n";
print CMD "\n";

#
# load design files
#
print CMD "#\n";
print CMD "# load design\n";
print CMD "#\n";
print CMD "set rda_Input(ui_netlist) \"$verilogFile\"\n";
print CMD "set rda_Input(ui_netlisttype) {Verilog}\n";
print CMD "set rda_Input(ui_topcell) {\\$design}\n";
print CMD "set rda_Input(ui_leffile) \"$techLefFile $lefFile\"\n";
print CMD "commitConfig\n";
print CMD "\n";

print CMD "if {[file exists \"$design.GNDShieldTieOff.tcl\" ] == 1} {\n";
print CMD "  source $design.GNDShieldTieOff.tcl\n";
print CMD "  AddTieOffCell\n";
print CMD "}\n";
print CMD "\n";

print CMD "defIn $defFile\n";
print CMD "defIn $ndDefFile\n" if $ndDefFile;
for($i=0; $i<=$#addlDefFiles; $i++) {
    print CMD "defIn $addlDefFiles[$i]\n";
}
print CMD "defIn $tracksDefFile\n" if $tracksDefFile;
print CMD "defIn $trackPitchFile\n" if $trackPitch;
print CMD "\n";

# 
# optionally route priority nets
#
if ($priorityNetsFile) {
    print CMD "#\n";
    print CMD "# route priority nets\n";
    print CMD "#\n";
    print CMD "########################################################################################\n";
    print CMD "# Priority nets list file syntax: selectNet <net>\n";
    print CMD "# List one net per line. Wild cards are acceptable.\n";
    print CMD "########################################################################################\n";
    print CMD "source $priorityNetsFile\n";
    print CMD "setNanoRouteMode -quiet -routeSelectedNetOnly true\n";
    print CMD "globalDetailRoute\n";
    print CMD "saveDesign $priorityNanoDB\n";
    print CMD "defOut -floorplan -netlist -routing $priorityDefFile\n";
    print CMD "\n";
}

#
# route all unrouted nets (remainder in case of priority routing)
#

print CMD "# insert skiproutes here. syntax - setAttribute -net ts\\[* -skip_routing true \n";
print CMD "#\n";

print CMD "#\n";
print CMD "# route all nets\n";
print CMD "#\n";
if ($bundled) {
    print CMD "defIn  ${design}_bundled.def\n";
    print CMD "source ${design}_bundled.tcl\n";
} else {
    print CMD "setNanoRouteMode -quiet -routeSelectedNetOnly false\n";
}
print CMD "routeDesign\n";
print CMD "saveDesign $routedNanoDB\n";
print CMD "defOut -floorplan -netlist -routing \\\n  $routedDefFile\n";

# 
# add multi-cut vias
#
if (!$skipMultiCutVias) {
    print CMD "\n";
    print CMD "#\n";
    print CMD "# add multi-cut vias\n";
    print CMD "#\n";

    print CMD "if {[file exists \"$design.SetSkipRouting.tcl\" ] == 1} {\n";
    print CMD "  source $design.SetSkipRouting.tcl\n";
    print CMD "  ProteusUserSetSkipRouting\n";
    print CMD "}\n";
    print CMD "\n";

    print CMD "setNanoRouteMode -quiet \\\n";
    print CMD "  -droutePostRouteSwapVia multiCut \\\n";
    print CMD "  -drouteUseMultiCutViaEffort medium\n";
    print CMD "routeDesign -viaOpt\n";

    print CMD "saveDesign $viaNanoDB\n";
    print CMD "defOut -floorplan -netlist -routing \\\n  $viaDefFile\n";
}

print CMD "\n";
if ($no_exit==0) {
    print CMD "exit\n";
}
close(CMD);

######################################################
# Done.
######################################################
DONE:
&echoMsg("$toolName Done\n");
&echoMsg("  Warnings: $warningCnt\n");
&echoMsg("    Errors: $errorCnt\n");
&echoMsg("\n");
close(LOG);
print SUM "$toolName Done\n";
print SUM "  Warnings: $warningCnt\n";
print SUM "    Errors: $errorCnt\n";
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

