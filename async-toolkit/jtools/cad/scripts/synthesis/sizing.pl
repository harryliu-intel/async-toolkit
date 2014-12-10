#!/usr/bin/perl -w 

#-------------------------------------------------------------------------------
#	Program usage
#-------------------------------------------------------------------------------

sub usage_exit {
    print STDERR << "USAGE";
sizing.pl
  [--cast-dir=<dir>]     Cast directory
  [--dfII-dir=<dir>]     Layout subtype spec directory
  [--spec-dir=<dir>]     Directory to new spec tree.
  [--layoutClient=<dir>] layout client.
  [--specClient=<dir>]   spec client.

USAGE
    exit 1;
}

#-------------------------------------------------------------------------------
#	Global variables, paths and configuration
#-------------------------------------------------------------------------------

# These are default variables that can and should be overwritten when any
# other user uses this script
my %VARS = (
    CAST_DIR      => "/home/user/nkim/project/hw/cast/main:/home/user/nkim/project/hw/cast/synthesis/main/:/home/user/nkim/project/hw/layout/tsmc13/spec/main",
    DFII_DIR      => "/home/user/nkim/project/hw/layout/tsmc13/dfII/project/main/mld",
    SPEC_DIR      => "/home/user/nkim/project/hw/layout/tsmc13/spec/mld",
    LAYOUT_CLIENT => "nkim-cast-and-tools",
    SPEC_CLIENT   => "nkim-cast-and-tools",
    SPEC          => "logic.mld.sizing.SIZE_LOGIC",
    PRESIZE       => 10000,
    UPDATE        => 32,
    OPTION        => "spec,presize,updatenetlist,set_fixed",
    CLEAN         => 0,
    BASE_DIR      => "$ENV{PWD}"
);

#-------------------------------------------------------------------------------
#	Command-line parsing
#-------------------------------------------------------------------------------

while (@ARGV) {
    if ($ARGV[0] =~ /^--cast-dir=(.*)$/) {
        $VARS{CAST_DIR} = $1;
    } elsif ($ARGV[0] =~ /^--dfII-dir=(.*)$/) {
        $VARS{DFII_DIR} = $1;
    } elsif ($ARGV[0] =~ /^--spec-dir=(.*)$/) {
        $VARS{SPEC_DIR} = $1;
    } elsif ($ARGV[0] =~ /^--layoutClient=(.*)$/) {
        $VARS{LAYOUT_CLIENT} = $1;
    } elsif ($ARGV[0] =~ /^--specClient=(.*)$/) {
        $VARS{SPEC_CLIENT} = $1;
    } elsif ($ARGV[0] =~ /^--update=(.*)$/) {
        $VARS{UPDATE} = $1;
    } elsif ($ARGV[0] =~ /^--clean/) {
        $VARS{CLEAN} = 1;
    } else {
        print STDERR "Unrecognized argument '$ARGV[0]'.\n";
        usage_exit();
    }
    shift;
}

#-------------------------------------------------------------------------------
#	Sizing -
#-------------------------------------------------------------------------------

sub sizing {
    my $ubersize = "fulcrum --pdk=tsmc13 --before 60373 ubersize --mem=1G ";
    $ubersize   .= "--cast-dir=$VARS{CAST_DIR} --dfII-dir=$VARS{DFII_DIR} ";
    $ubersize   .= "--spec-dir=$VARS{SPEC_DIR} ";
    $ubersize   .= "--layoutClient=$VARS{LAYOUT_CLIENT} ";
    $ubersize   .= "--specClient=$VARS{SPEC_CLIENT} ";
    $ubersize   .= "--qsub=1 --serverLimit=5 --useCG ";
    $ubersize   .= "--suppress-pins=1 --verbose";

    my $option   = "$VARS{OPTION} $VARS{SPEC} $VARS{PRESIZE} $VARS{UPDATE}";
    print "$ubersize $option\n";
    system("$ubersize $option");

    my $sizeDir  = "$VARS{BASE_DIR}/$VARS{SPEC}_$VARS{UPDATE}ps/";
    $sizeDir    .= "cast/logic/mld";
    edit_cast("$sizeDir/sizing");
#    edit_cast("$sizeDir/completion");

    exit 0;
}

#-------------------------------------------------------------------------------
#	edit_cast - 
#-------------------------------------------------------------------------------

sub edit_cast {
    my ($sizeDir) = @_;

    my @castDir = get_cast($sizeDir);
    foreach $cast (@castDir) {
        my $castFile = "$sizeDir/$cast/$VARS{PRESIZE}.cast";
        system("cp $castFile $castFile.org");
        open(NEW_CAST, ">$sizeDir/$cast/$VARS{PRESIZE}.cast") ||
            die("*Error> cannot open cast \"$castFile\".\n");

        open(ORG_CAST, "$sizeDir/$cast/$VARS{PRESIZE}.cast.org") ||
            die("*Error> cannot open cast \"$castFile.org\".\n");
        while (<ORG_CAST>) {
            if ($_ =~ /GND GND go/) {
                if ($_ =~ /NW(\d+)=(\d+\.\d+)/) {
                    my $nwNum = $1;
                    my $nwVal = $2;
                    my $newNW = $nwVal * 2;
                    print "NMOS Width of go in $cast: $nwVal -> $newNW\n";
                    s/NW$nwNum=$nwVal/NW$nwNum=$newNW/g;
                } elsif ($_ =~ /NW(\d+)=(\d+)/) {
                    my $nwNum = $1;
                    my $nwVal = $2;
                    my $newNW = $nwVal * 2;
                    print "NMOS Width of go in $cast: $nwVal -> $newNW\n";
                    s/NW$nwNum=$nwVal/NW$nwNum=$newNW/g;
                } elsif ($_ =~ /NW=(\d+\.\d+)/) {
                    my $nwVal = $1;
                    my $newNW = $nwVal * 2;
                    print "NMOS Width of go in $cast: $nwVal -> $newNW\n";
                    s/NW=$nwVal/NW=$newNW/g;
                }
            }
            print NEW_CAST $_;
        }
        close(ORG_CAST);
        close(NEW_CAST);
    }
}

sub get_cast {
    my ($sizeDir) = @_;

    opendir(IMD, $sizeDir) || 
        die ("*Error> Cannot open size directory \"$sizeDir\"!");
    my @dirs = readdir(IMD);
    closedir(IMD);

    my @castList = ();

    foreach $dir (@dirs) {
        if ($dir =~ /\(/) {
            push @castList, "\"$dir\"" if ($sizeDir =~ /completion/);
        } elsif ($dir =~ /^BUF/) {
            push @castList, $dir;
        } elsif ($dir =~ /^LOGIC/) {
            push @castList, $dir;
        }
    }

    return @castList;
}

#-------------------------------------------------------------------------------
#	Program
#-------------------------------------------------------------------------------

if ($VARS{CLEAN} == 1) {
    print "rm -rf $VARS{DFII_DIR}/logic/mld/sizing/*\n"; 
    system("rm -rf $VARS{DFII_DIR}/logic/mld/sizing/*");
}

sizing();

