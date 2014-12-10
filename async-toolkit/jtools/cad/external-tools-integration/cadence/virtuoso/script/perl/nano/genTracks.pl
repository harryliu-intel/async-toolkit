#!/usr/bin/perl
#
# Generate DEF TRACKS information for nanoroute for a specified design.
#
######################################################
# setup
######################################################
require 'ctime.pl';
use POSIX;

# set defaults for TSMC 28nm
$units = 1000; # 1000 microns per DBU
$metal = "M";
$trackPitch = 0.13; 
$routingPitch = 0.13;
$powerGridPitch = 1.56;
$gcellPitch = 4*$powerGridPitch;
$ArtisanPitch = 0.1; # track pitch
$RowPitch = 1.56;     # row   pitch
@powerGridLayers = (3,5,7);

#
# usage
#
$usage = "
Usage: genTracks
  { --refdef=<defFile> |           : Get units, diearea and design from <defFile>.
    --design=<top-cell-name>       : Specify design name <top-cell-name>
    --diearea=<x1>:<y1>:<x2>:<y2>  : Specify diearea coordinates in microns.
    [--units=<num> ] }             : Specify units for 1 micron (default=$units).
  [ --track-pitch=<num> ]          : Track pitch in microns (default=$trackPitch).
  [ --ArtisanTracks ]              : Use ${ArtisanPitch}um tracks for M1 and M2.
  [ --ArtisanRows ]                : Add row definitions. Implies --ArtisanTracks.
  [ --rotate ]                     : Change orientation of preferred directions
  { --outfile=<file> |             : Output DEF filename (default='tracks.def').
    --stdout }                     : Output to stdout instead of def file.";

#
# default and initial argument values
#
$refDef = 0;
$defInFile = "";
$userDieArea = 0;
@dieAreaCoord = (0,0,0,0);
$userDesign = 0;
$designName = "";
$defOutFile = "tracks.def";
$outDef = 1;
$outStd = 0;
$userUnits = 0;
$debug = 0;
$ArtisanTracks = 0;
$ArtisanRows = 0;
$rotate = 0;

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

    if ($ARGV[0] eq "--tsmc130") {
        TSMC130;
        shift(@ARGV);
    }
    
    if ($ARGV[0] eq "--tsmc65") {
        TSMC65;
        shift(@ARGV);
    }

    # --debug (undocumented arg)
    if ($ARGV[0] eq "--debug") {
	$debug = 1;
	shift(@ARGV);
	next;
    }

    # --refdef <defFile>
    if ($ARGV[0] =~ /--refdef=(\S+)/) {
	$defInFile = $1;
	$refDef = 1;
 	shift(@ARGV);
	next;
    }

    # --diearea=<x1>:<y1>:<x2>:<y2>
    if ($ARGV[0] =~ /--diearea=(.*):(.*):(.*):(.*)/) {
	@dieAreaCoord = ($1, $2, $3, $4);

	$userDieArea = 1;
 	shift(@ARGV);
	next;
    }

    # --design=<top-cell-name>
    if ($ARGV[0] =~ /--design=(\S+)/) {
	$designName = $1;
	$userDesign = 1;
 	shift(@ARGV);
	next;
    }

    # --ArtisanTracks
    if ($ARGV[0] eq "--ArtisanTracks") {
	$ArtisanTracks = 1;
	shift(@ARGV);
	next;
    }

    # --ArtisanRows
    if ($ARGV[0] eq "--ArtisanRows") {
	$ArtisanRows = 1;
        $ArtisanTracks = 1;
	shift(@ARGV);
	next;
    }

    # --rotate
    if ($ARGV[0] eq "--rotate") {
	$rotate = 1;
	shift(@ARGV);
	next;
    }

    # --outfile=<file>
    if ($ARGV[0] =~ /--outfile=(\S+)/) {
	$defOutFile = $1;
	$outFile = 1;
 	shift(@ARGV);
	next;
    }

    # --stdout
    if ($ARGV[0] eq "--stdout") {
	$outStd = 1;
	$outDef = 0;
	shift(@ARGV);
	next;
    }

    # --units=<num>
    if ($ARGV[0] =~ /--units=(\d+)/) {
	$units = $1;
	$userUnits = 1;
 	shift(@ARGV);
	next;
    }

    # --track-pitch=<num>
    if ($ARGV[0] =~ /--track-pitch=(\d*\.\d+)/) {
	$trackPitch = $1;
 	shift(@ARGV);
	next;
    }

    # unknown argument
    &errorExit("Unknown or invalid argument '$ARGV[0]'.\n $usage \n");
}

# scale user diearea
if ($userDieArea) {
    @dieAreaCoord = ($dieAreaCoord[0]*$units,$dieAreaCoord[1]*$units,
                     $dieAreaCoord[2]*$units,$dieAreaCoord[3]*$units);
}

######################################################
# validate inputs
######################################################
#
# check for required arguments
#
&errorExit("Must specify --diearea or --refdef.\n $usage \n") if !$userDieArea && !$refDef;
&errorExit("Do not specify --outfile with --stdout.\n $usage \n") if $outFile && $outStd;

if ($refDef) {
    &errorExit("Do not specify --design, --diearea or --units with --refdef.\n $usage \n") 
	if $userDieArea || $userDesign || $userUnits;
} elsif (!$outStd) {
    &errorExit("Must specify --design and --diearea when not using --refdef.\n $usage \n") 
	if !$userDieArea || !$userDesign;
}


#
# validate paths & files
#
&errorExit("refdef '$defInFile' not found.\n") if !-e $defInFile && $refDef;

######################################################
# get design name, units and coords from def
######################################################
if ($refDef) {

    # get design name
    open(DEFIN, "$defInFile") || &errorExit("Unable to open file $defIn: $!");
    while (<DEFIN>) {
	if (/DESIGN\s+(\S+)/) {
	    $designName = $1;
	    last;
	}
    }
    &errorExit("Unable to find DESIGN statement in $defIn.") if !$designName;

    # get units
    while (<DEFIN>) {
	if (/UNITS DISTANCE MICRONS (\d+)/) {
	    $designUnits = $1;
	    unless ($outStd) {
		print "WARNING: Modifying UNITS $units to match design UNITS $designUnits.\n"
		    if $units != $designUnits;
	    }
	    $units = $designUnits;
	    last;
	}
    }
    &errorExit("Unable to find UNITS statement in $defIn.") if !$designUnits;

    # get diearea
    while (<DEFIN>) {
	if (/DIEAREA\s+\(\s+(-*\d+)\s+(-*\d+)\s+\)\s+\(\s+(-*\d+)\s+(-*\d+)\s+\)/) {
	    @dieAreaCoord = ($1, $2, $3, $4);
	    last;
	}
    }
    &errorExit("Unable to find DIEAREA statement in $defIn.") if !@dieAreaCoord;
    close(DEFIN);
}


######################################################
# generate tracks
######################################################
#
# open output destination
#
if ($outDef) {
    open(DEFOUT, ">$defOutFile") || &errorExit("Unable to open file $defOut: $!");
} else {
    open(DEFOUT, ">&STDOUT") || &errorExit("Unable to open STDOUT: $!");
}


#
# print def header
#
if ($outDef) {
    print DEFOUT "#************************************************************\n";
    print DEFOUT "#\n";
    print DEFOUT "# created by genTracks.pl  ", &ctime(time);
    print DEFOUT "#\n";
    print DEFOUT "#   Reference DEF = $defInFile\n" if $refDef;
    print DEFOUT "#   User specified DIEAREA\n" if $userDieArea;
    print DEFOUT "#   User specified DESIGN\n" if $userDesign;
    print DEFOUT "#   User specified UNITS\n" if $userUnits;
    print DEFOUT "#\n";
    print DEFOUT "#************************************************************\n";
    print DEFOUT "VERSION 5.5 \;\n";
    print DEFOUT "\n";
    print DEFOUT "NAMESCASESENSITIVE ON \;\n";
    print DEFOUT "DIVIDERCHAR \"|\" \;\n";
    print DEFOUT "BUSBITCHARS \"<>\" \;\n";
    print DEFOUT "\n";
    print DEFOUT "DESIGN $designName \;\n";
    print DEFOUT "\n";
    print DEFOUT "UNITS DISTANCE MICRONS $units \;\n";
}
print DEFOUT "\n";
print DEFOUT "DIEAREA ( $dieAreaCoord[0] $dieAreaCoord[1] ) ( $dieAreaCoord[2] $dieAreaCoord[3] ) \;\n";
print DEFOUT "\n";

# scale by units
$trackPitch *= $units;
$powerGridPitch *= $units;
$routingPitch *= $units;
$ArtisanPitch *= $units;
$RowPitch *= $units;

#
# define rows
#

if ($ArtisanRows==1) {
    $firstX = ceilCoord($dieAreaCoord[0],0,$ArtisanPitch);
    $firstY = ceilCoord($dieAreaCoord[1],0,$RowPitch);
    $lastX = floorCoord($dieAreaCoord[2],0,$ArtisanPitch);
    $lastY = floorCoord($dieAreaCoord[3],0,$RowPitch);
    $stepsX = POSIX::ceil(($lastX-$firstX)/$ArtisanPitch);
    $stepsY = POSIX::ceil(($lastY-$firstY)/$RowPitch);

    for (my $i=0; $i<$stepsY; $i++) {
        my $orient = "FS"; $orient = "N" if ($i%2==0);
        my $y = $firstY + $RowPitch*$i;
        print DEFOUT "ROW CORE_ROW_${i} cnx4site $firstX $y $orient DO $stepsX BY 1 STEP $ArtisanPitch 0 ;\n";
    }
    print DEFOUT "\n";
}

#
# print base tracks
#
$trackStep = $trackPitch;
for ($layer = 1; $layer < 8; $layer++) {
    for ($dir=0; $dir<2; $dir++) {
        if ($ArtisanTracks && ($layer<=2)) {
            my $off = 0; 
            if ($dir==0) { $off=-$ArtisanPitch/2; }
            $first = (&ceilCoord($dieAreaCoord[$dir], $off, $ArtisanPitch));
            $last = (&floorCoord($dieAreaCoord[2+$dir], $off, $ArtisanPitch));
            $steps = 1+POSIX::ceil(($last-$first) / $ArtisanPitch);
            print DEFOUT "TRACKS $direction[$dir] $first DO $steps STEP $ArtisanPitch LAYER ${metal}${layer} \;\n";

        } else {
            $first = (&ceilCoord($dieAreaCoord[$dir], -$routingPitch/2, $trackPitch));
            $last = (&floorCoord($dieAreaCoord[2+$dir], -$routingPitch/2, $trackPitch));
            $steps = 1+POSIX::ceil(($last-$first) / $trackStep);
            print DEFOUT "TRACKS $direction[$dir] $first DO $steps STEP $trackStep LAYER ${metal}${layer} \;\n";
        }
    }
}
print DEFOUT "\n";

#
# print power grid tracks for vias to power stripes
#
print DEFOUT "# power grid tracks for horizontal power stripes\n";
foreach $layer (@powerGridLayers) {
    $dir = (($layer-$rotate)%2) ? 1 : 0;
    $first = (&ceilCoord($dieAreaCoord[$dir],   0, $powerGridPitch));
    $last = (&floorCoord($dieAreaCoord[2+$dir], 0, $powerGridPitch));
    $steps = 1+POSIX::ceil(($last-$first) / $powerGridPitch);
    print DEFOUT "TRACKS $direction[$dir] $first DO $steps STEP $powerGridPitch LAYER ${metal}${layer} \;\n";
}
print DEFOUT "\n";

#
# print gcells
#
$gcellStep = $gcellPitch * $units; 
$firstX = ceilCoord($dieAreaCoord[0],0,$gcellStep);
$lastX  = floorCoord($dieAreaCoord[2],0,$gcellStep);
$firstY = ceilCoord($dieAreaCoord[1],0,$gcellStep);
$lastY  = floorCoord($dieAreaCoord[3],0,$gcellStep);
$stepsX = 1+POSIX::ceil(($lastX-$firstX)/$gcellStep);
$stepsY = 1+POSIX::ceil(($lastY-$firstY)/$gcellStep);

# grid inside die area
print DEFOUT "GCELLGRID X $firstX DO $stepsX STEP $gcellStep \;\n";
print DEFOUT "GCELLGRID Y $firstY DO $stepsY STEP $gcellStep \;\n";

# grid on the die boundary as well
print DEFOUT "GCELLGRID X $dieAreaCoord[0] DO 1 STEP $gcellStep \;\n";
print DEFOUT "GCELLGRID X $dieAreaCoord[2] DO 1 STEP $gcellStep \;\n";
print DEFOUT "GCELLGRID Y $dieAreaCoord[1] DO 1 STEP $gcellStep \;\n";
print DEFOUT "GCELLGRID Y $dieAreaCoord[3] DO 1 STEP $gcellStep \;\n";

# finish and close file
print DEFOUT "\n";
print DEFOUT "END DESIGN\n" if $outDef;
close(DEFOUT);
print "Wrote $defOutFile\n" if $outDef;
exit;

                                                                                                
######################################################
# functions
######################################################
# error exit
sub errorExit {
    print "\nERROR: @_ \n";
    exit 1;
}

# display error msg
sub errorMsg {
    print "ERROR: @_\n";
    $errorCnt++;
}

# calculate first coordinate rounded down to pitch and offset
sub ceilCoord {
    my ($baseCoord, $offset, $pitch) = @_;
    return($offset + $pitch * POSIX::ceil(($baseCoord-$offset)/$pitch));
}

# calculate last coordinate rounded up to pitch and offset
sub floorCoord {
    my ($baseCoord, $offset, $pitch) = @_;
    return($offset + $pitch * POSIX::floor(($baseCoord-$offset)/$pitch));
}
