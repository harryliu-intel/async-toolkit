#!/usr/bin/perl -w 

#-------------------------------------------------------------------------------
#	Program usage
#-------------------------------------------------------------------------------

sub usage_exit {
    print STDERR << "USAGE";
char_cadence_lib.pl
  [--cast-dir=<dir>]        Cast directory
  [--dfII-dir=<dir>]        Layout subtype spec directory
  [--base-dir=<dir>]        Directory to output info.
  [--spec-dir=<dir>]        Sizing spec directory.
  [--timing-spec-dir=<dir>] Path-spec and timing-sense files are under this.
  [--cell=<cell_name>]      Cell name (e.g. LOGIC2_1_X1, not fqcn)
  [--inputs=<inputs>]       The number of inputs
  [--task=<task>]           Tasks are
                              image    :Generate liberty info.
                              dual     :Generate liberty info.
                              nand     :Generate liberty info.
  [--target=<file>]         Targets are
                               alint   :Run 'lve --task=alint'.
                               libName :Run 'pathalyze'.
  [--include=<file>]        Can include other argument definitions.

USAGE
    exit 1;
}

#-------------------------------------------------------------------------------
#	Global variables, paths and configuration
#-------------------------------------------------------------------------------

#- These are default variables that can and should be overwritten when any
#- other user uses this script
my %VARS = (
    CAST_DIR   => "$ENV{USER}/hw/cast/main",
    DFII_DIR   => "$ENV{USER}/hw/layout/tsmc13/dfII/project/main/mld",
    PDK_DIR    => "/home/group/cadadmin/technology/fulcrum-tsmc13-pdk/1.0",
    SPEC_DIR   => "",
    TIMING_DIR => "$ENV{USER}/hw/layout/tsmc13/spar/synthesis/logic/mld/lib",
    BASE_DIR   => "$ENV{PWD}"
);

my @PVTS       = ("1.0_25_tt");
my @SLEW_RATES = ("14", "28", "44", "76", "138", "264", "516");
my @CAP_VALUES = ("0.00079e-12", "0.002054e-12", "0.00474e-12", "0.010112e-12",
                  "0.020856e-12", "0.042186e-12", "0.08532e-12");
my $TASK_NAME  = "";
my $CELL_NAME  = "";
my $GATE_NAME  = "";
my $ROOT_NAME  = "";
my $TS_FILE    = "";
my $LIB_NAME   = "";
my $STRENGTH   = 0;
my $NUM_INPUTS = 0;

#-------------------------------------------------------------------------------
#	Command-line parsing
#-------------------------------------------------------------------------------

sub read_env {
    my ($fileName) = @_;
    open(ENVFILE,"$VARS{BASE_DIR}/$fileName") ||
        die("*Error> cannot open the file \"$fileName\".\n"); 
    while (<ENVFILE>) {
        if ($_ =~ /^--cast-dir=(.*)$/) {
            $VARS{CAST_DIR} = $1;
        } elsif ($_ =~ /^--dfII-dir=(.*)$/) {
            $VARS{DFII_DIR} = $1;
        } elsif ($_ =~ /^--spec-dir=(.*)$/) {
            $VARS{SPEC_DIR} = $1;
        } elsif ($_ =~ /^--timing-spec-dir=(.*)$/) {
            $VARS{TIMING_DIR} = $1;
        } elsif ($_ =~ /^--target=(.*)$/) {
            $LIB_NAME = $1;
        } elsif ($_ =~ /^--width=(.*)$/) {
        } elsif ($_ =~ /^--height=(.*)$/) {
        } else {
            print STDERR "Unrecognized argument '$ARGV[0]'.\n";
            close(ENVFILE);
            usage_exit();
        }
    }
    close(ENVFILE);
}

while (@ARGV) {
    if ($ARGV[0] =~ /^--cast-dir=(.*)$/) {
        $VARS{CAST_DIR} = $1;
    } elsif ($ARGV[0] =~ /^--dfII-dir=(.*)$/) {
        $VARS{DFII_DIR} = $1;
    } elsif ($ARGV[0] =~ /^--base-dir=(.*)$/) {
        $VARS{BASE_DIR} = $1;
    } elsif ($ARGV[0] =~ /^--spec-dir=(.*)$/) {
        $VARS{SPEC_DIR} = $1;
    } elsif ($ARGV[0] =~ /^--timing-spec-dir=(.*)$/) {
        $VARS{TIMING_DIR} = $1;
    } elsif ($ARGV[0] =~ /^--task=(.*)$/) {
        $TASK_NAME = $1;
    } elsif ($ARGV[0] =~ /^--cell=(.*)$/) {
        #- generate fully qualified cell name.
        $GATE_NAME  = $1;
        $CELL_NAME  = "logic.mld.sizing.";
        $CELL_NAME .= "$GATE_NAME.10000";
    } elsif ($ARGV[0] =~ /^--inputs=(.*)$/) {
        $NUM_INPUTS = $1;
    } elsif ($ARGV[0] =~ /^--include=(.*)$/) {
        read_env($1);
    } elsif ($ARGV[0] =~ /^--pkg-dir=(.*)$/) {
        $VARS{PDK_DIR} = $1;
    } elsif ($ARGV[0] =~ /^--target=(.*)$/) {
        $LIB_NAME = $1;
    } elsif ($ARGV[0] =~ /^--cap=(.*)$/) {
        my $single_cap = $1;
        $single_cap .= "e-12";
        @CAP_VALUES = ($single_cap);
    } else {
        print STDERR "Unrecognized argument '$ARGV[0]'.\n";
        usage_exit();
    }
    shift;
}

usage_exit() unless ($TASK_NAME eq "debug"  ||
                     $TASK_NAME eq "image"  ||
                     $TASK_NAME eq "dual"   ||
                     $TASK_NAME eq "nand"); 
usage_exit() if (
    (($TASK_NAME eq "image") &&
     ($CELL_NAME eq "" || $VARS{SPEC_DIR} eq "" || $NUM_INPUTS == 0)) ||
    (($TASK_NAME eq "dual" || $TASK_NAME eq "nand") &&
     ($CELL_NAME eq "" || $VARS{SPEC_DIR} eq "" || $NUM_INPUTS == 0)));

#- TS_FILE:  timing sense file name -> gateName.ts (e.g. LOGIC2_1.ts)
#- GATE_NAME: library file name -> gateName with strength (e.g. LOGIC2_1_X1)

#- CELL_NAME: fqc (e.g. logic.mld.sizing.LOGIC2_1_DR_X1.10000)
#- GATE_NAME: pure gate name (e.g. LOGIC2_1)
#- STRENGTH : strength (e.g. 1,2,4 or 8)
#- CELL_NAME is the same as logic.mld.sizing.$GATE_NAME\_$type\_X$STRENGTH.10000.
#- type of image, dual and nand tasks is IMAGE, DR and C, respectively.
#- TS_FILE is $GATE_NAME.ts.
my @fullName = split("_X", $GATE_NAME);
$STRENGTH  = $fullName[1];
$ROOT_NAME = $fullName[0];
$ROOT_NAME =~ s/_DR// if (($TASK_NAME eq "dual") || ($TASK_NAME eq "image"));
$ROOT_NAME =~ s/_C//  if  ($TASK_NAME eq "nand");
$TS_FILE   = $ROOT_NAME . ".ts";

#-------------------------------------------------------------------------------
#	Define input and output pins.
#-------------------------------------------------------------------------------

#- return an array for all the outputs of cell to be tested.
sub get_outputs {
    @outpins   = ();
    if ($GATE_NAME =~ /^PC_AND/) {
        #- for PC_AND_SMALL
        if ($NUM_INPUTS < 5) {
            push   @outpins, "X";
            push   @outpins, "_X";
        #- for PC_AND_HF
        } else {
            push   @outpins, "X";
            push   @outpins, "_X2";
            push   @outpins, "_X1";
        }
        return @outpins;
    }
    push   @outpins, "X.0";
    push   @outpins, "X.1";
    push   @outpins, "_X.0";
    push   @outpins, "_X.1";
    push   @outpins, "V" if ($TASK_NAME eq "nand");
    return @outpins;
}

sub get_loadoutputs {
    @outpins   = ();
    if ($GATE_NAME =~ /^PC_AND/) {
        #- for PC_AND_SMALL
        if ($NUM_INPUTS < 5) {
            push   @outpins, "X";
        #- for PC_AND_HF
        } else {
            push   @outpins, "X";
        }
        return @outpins;
    }
    push   @outpins, "X.0";
    push   @outpins, "X.1";
    push   @outpins, "V" if ($TASK_NAME eq "nand");
    return @outpins;
}
sub get_inputs {
    my ($num_inputs) = @_;
    my @inpins = ();
    for ($i = 0; $i < $num_inputs; $i++) {
        push @inpins, "A[$i].0";
        push @inpins, "A[$i].1";
    }
    push   @inpins, "go" unless ($TASK_NAME eq "image");
    return @inpins;
}

my @nodes_withcap = get_loadoutputs();
my @nodes	  = get_outputs();

#-------------------------------------------------------------------------------
#	lve_extract - 
#-------------------------------------------------------------------------------

sub lve_extract {
    my $infile	   = "alint.in";
    my $localprops = "cell.localprops";
    my $todo       = "$VARS{BASE_DIR}/todo";

    write_infile($infile);
    write_props($localprops);
    write_todo($todo);

    my $lve  = "fulcrum --pdk tsmc13 lve ";
    $lve    .= "--dfII-dir=$VARS{DFII_DIR} --cast-dir=$VARS{CAST_DIR} ";
    $lve    .= "--spec-dir=$VARS{SPEC_DIR}/cast ";
    $lve    .= "--qsub=0 --jobs=1 --mem=1000M --verbose=1 ";
    $lve    .= "--sort=1 --estimated-view=floorplan --mode=estimated ";
    $lve    .= "--delayTau=20,40,64,110,199,381,745 ";
    $lve    .= "--delayCC=0 --bumpCC='' ";
    $lve    .= "--priority=-1 ";
    my $sge_arch="x86_64";
    chomp $sge_arch;
    my $qrsh = "qb";
    my $i    = 0;

    #- run LVE extract.
    foreach $cap (@CAP_VALUES) {
        foreach $pvt (@PVTS) {
            my $capfile = "alint_${cap}_${pvt}.asp";
            my $sweep   = "sweep_${cap}_${pvt}.sweep";
            my $dir     = "$VARS{BASE_DIR}/${cap}";
            my $pwd     = "$ENV{PWD}";

            write_capfile($capfile, $pvt);
            write_sweep($sweep, $pvt);

            my $lve_cmd  = "$lve ";
            $lve_cmd .= "--output-dir=$dir --task=extract ";
            $lve_cmd .= "--alint-asp=$pwd/$capfile --alint-in=$pwd/$infile ";
            $lve_cmd .= "--cell-localprops=$pwd/$localprops ";
            $lve_cmd .= "--include=$pwd/$sweep --include=$todo ";

            print "** Run LVE extract for $cap.\n";
            print "$lve_cmd.\n";
            if ($i % 3 == 0) {
                system("$qrsh $lve_cmd ");
            } else {
                system("$qrsh $lve_cmd & ");
            }
            $i++;
        }
    }

    print("lve_sweep is done!\n");
}

#-------------------------------------------------------------------------------
#	lve_alint - 
#-------------------------------------------------------------------------------

sub lve_alint {
    my $infile	   = "alint.in";
    my $localprops = "cell.localprops";
    my $todo       = "$VARS{BASE_DIR}/todo";

    write_infile($infile);
    write_props($localprops);
    write_todo($todo);

    my $lve  = "fulcrum --pdk tsmc13 lve ";
    $lve    .= "--dfII-dir=$VARS{DFII_DIR} --cast-dir=$VARS{CAST_DIR} ";
    $lve    .= "--spec-dir=$VARS{SPEC_DIR}/cast ";
    $lve    .= "--qsub=0 --jobs=1 --mem=1000M --verbose=1 ";
    $lve    .= "--sort=1 --estimated-view=floorplan --mode=estimated ";
    $lve    .= "--delayTau=20,40,64,110,199,381,745 ";
    $lve    .= "--delayCC=0 --bumpCC='' ";
    $lve    .= "--priority=-1 ";
    my $sge_arch="x86_64";
    chomp $sge_arch;
    my $qrsh = "qb";
    my $i    = 0;

    #- run LVE alint.
    foreach $cap (@CAP_VALUES) {
        foreach $pvt (@PVTS) {
            my $capfile = "alint_${cap}_${pvt}.asp";
            my $sweep   = "sweep_${cap}_${pvt}.sweep";
            my $dir     = "$VARS{BASE_DIR}/${cap}";
            my $pwd     = "$ENV{PWD}";

            if ($GATE_NAME =~ /^PC_AND/) {
                edit_spice_for_pcand($dir) 
            } else {
                edit_spice($dir) 
            }
            print "** Changed NL and NW of WEAK_2P in cell.spice.\n";

            write_capfile($capfile, $pvt);
            write_sweep($sweep, $pvt);

            my $lve_cmd  = "$lve ";
            $lve_cmd .= "--output-dir=$dir --task=jlvs,alint ";
            $lve_cmd .= "--alint-asp=$pwd/$capfile --alint-in=$pwd/$infile ";
            $lve_cmd .= "--cell-localprops=$pwd/$localprops ";
            $lve_cmd .= "--include=$pwd/$sweep --include=$todo ";

            print "** Run LVE alint for $cap.\n";
            print "$lve_cmd.\n";
            if ($i % 3 == 0) {
                system("$qrsh $lve_cmd ");
            } else {
                system("$qrsh $lve_cmd & ");
            }
            $i++;
        }
    }
    print("lve_sweep is done!\n");
}

#-------------------------------------------------------------------------------
#	edit_spice - 
#	edit_spice_for_pcand - 
#-------------------------------------------------------------------------------

sub edit_spice {
    my ($dir) = @_;

    my $jautoDir = get_jauto_dir($dir);

    #- find right R and C values from a temporary jauto directory.
    my ($newC0, $newR0, $newC1, $newR1) = get_newRC($jautoDir);
    $newC0 .= "E-15";
    $newC1 .= "E-15";
    print "newC0: $newC0  newR0: $newR0\n";
    print "newC1: $newC1  newR1: $newR1\n";

    #- find right PL and PW values for WEAK_2P from a temporary jauto directory.
    my $orgCast = "$jautoDir/logic/mld/sizing/$GATE_NAME/10000.cast";
    my $PL0 = 0;
    my $PW0 = 0;
    my $PL1 = 0;
    my $PW1 = 0;
    open(CAST_FILE, "$orgCast") ||
        die("*Error> cannot open the file \"$orgCast\".\n"); 
    while (<CAST_FILE>) {
        if ($_ =~ /_X.0 \/ gate.WEAK_2P/) {
            $PL0 = $1 if ($_ =~ /PL=(\d+\.\d+)/);
            $PW0 = $1 if ($_ =~ /PW=(\d+\.\d+)/);
        } elsif ($_ =~ /_X.1 \/ gate.WEAK_2P/) {
            $PL1 = $1 if ($_ =~ /PL=(\d+\.\d+)/);
            $PW1 = $1 if ($_ =~ /PW=(\d+\.\d+)/);
        }
    }
    close(CAST_FILE);
    $PL0 .= "e-06";
    $PW0 .= "e-06";
    $PL1 .= "e-06";
    $PW1 .= "e-06";

    my $spice_file = "$dir/logic/mld/sizing/$GATE_NAME/";
    $spice_file   .= "10000/floorplan/estimated/cell.spice";
    system ("cp $spice_file $spice_file.org");
    open(SPICE, ">$spice_file") ||
        die("*Error> cannot open spice \"$spice_file\".\n");
    open(ORG_SPICE, "$spice_file.org") ||
        die("*Error> cannot open spice \"$spice_file\".\n");
    while (<ORG_SPICE>) {
        if ($_ =~ /C_X.0/) {
            if ($_ =~ /(\d+\.\w+\-\d+)/) {
                print "C_X.0: $1 -> $newC0\n";
                s/$1/$newC0/g; 
            }
        } elsif ($_ =~ /R_X.0/) {
            if ($_ =~ /(\d+\.\w+)/) {
                print "R_X.0: $1 -> $newR0\n";
                s/$1/$newR0/g; 
            }
        } elsif ($_ =~ /C_X.1/) {
            if ($_ =~ /(\d+\.\w+\-\d+)/) {
                print "C_X.1: $1 -> $newC1\n";
                s/$1/$newC1/g; 
            }
        } elsif ($_ =~ /R_X.1/) {
            if ($_ =~ /(\d+\.\w+)/) {
                print "R_X.1: $1 -> $newR1\n";
                s/$1/$newR1/g; 
            }
        } elsif ($_ =~ /X.0:source \/ gate.WEAK_2P/) {
            if ($_ =~ /PL=(\d+\.\d+e-\d+) PW=(\d+\.\d+e-\d+)/) {
                print "PL: $1 -> $PL0  PW: $2 -> $PW0 \n";
                s/PL=$1 PW=$2/PL=$PL0 PW=$PW0/g;
            } elsif ($_ =~ /PL=(\d+e-\d+) PW=(\d+e-\d+)/) {
                print "PL: $1 -> $PL0  PW: $2 -> $PW0 \n";
                s/PL=$1 PW=$2/PL=$PL0 PW=$PW0/g;
            }
        } elsif ($_ =~ /X.1:source \/ gate.WEAK_2P/) {
            if ($_ =~ /PL=(\d+\.\d+e-\d+) PW=(\d+\.\d+e-\d+)/) {
                print "PL: $1 -> $PL0  PW: $2 -> $PW0 \n";
                s/PL=$1 PW=$2/PL=$PL0 PW=$PW0/g;
            } elsif ($_ =~ /PL=(\d+e-\d+) PW=(\d+e-\d+)/) {
                print "PL: $1 -> $PL0  PW: $2 -> $PW0 \n";
                s/PL=$1 PW=$2/PL=$PL0 PW=$PW0/g;
            }
        }
        print SPICE $_;
    }
    close(ORG_SPICE);
    close(SPICE);
}

sub edit_spice_for_pcand {
    my ($dir) = @_;

    my $jautoDir        = get_jauto_dir($dir);

    #- find right R and C values from a temporary jauto directory.
    my ($newC, $newR)   = get_newRC_for_pcand($jautoDir);
    $newC .= "E-15";
    print "newC: $newC  newR: $newR\n";

    #- find right PL and PW values for STATICIZER 
    #- from a temporary jauto directory.
    #- $PL and $PW are only for PC_AND2,3 and 4.
    #- $PL1,2 and $PW1,2 are for PC_AND5-8.
    my $orgCast = "$jautoDir/logic/mld/sizing/$GATE_NAME/10000.cast";
    my $PL  = 0;
    my $PW  = 0;
    my $PL1 = 0;
    my $PW1 = 0;
    my $PL2 = 0;
    my $PW2 = 0;
    open(CAST_FILE, "$orgCast") ||
        die("*Error> cannot open the file \"$orgCast\".\n"); 
    while (<CAST_FILE>) {
        if ($_ =~ /_X1 \/ gate.STATICIZER/) {
            $PL1 = $1 if ($_ =~ /PL=(\d+\.\d+)/);
            $PW1 = $1 if ($_ =~ /PW=(\d+\.\d+)/);
        } elsif ($_ =~ /_X2 \/ gate.STATICIZER/) {
            $PL2 = $1 if ($_ =~ /PL=(\d+\.\d+)/);
            $PW2 = $1 if ($_ =~ /PW=(\d+\.\d+)/);
        } elsif ($_ =~ /_X \/ gate.WEAK_2P/) {
            $PL  = $1 if ($_ =~ /PL=(\d+\.\d+)/);
            $PW  = $1 if ($_ =~ /PW=(\d+\.\d+)/);
        }
    }
    close(CAST_FILE);
    $PL  .= "e-06";
    $PW  .= "e-06";
    $PL1 .= "e-06";
    $PW1 .= "e-06";
    $PL2 .= "e-06";
    $PW2 .= "e-06";

    my $spice_file = "$dir/logic/mld/sizing/$GATE_NAME/";
    $spice_file   .= "10000/floorplan/estimated/cell.spice";
    system ("cp $spice_file $spice_file.org");
    open(SPICE, ">$spice_file") ||
        die("*Error> cannot open spice \"$spice_file\".\n");
    open(ORG_SPICE, "$spice_file.org") ||
        die("*Error> cannot open spice \"$spice_file\".\n");
    while (<ORG_SPICE>) {
        if ($_ =~ /C_X/) {
            if ($_ =~ /(\d+\.\w+\-\d+)/) {
                print "C_X: $1 -> $newC\n";
                s/$1/$newC/g; 
            }
        } elsif ($_ =~ /R_X/) {
            if ($_ =~ /(\d+\.\w+)/) {
                print "R_X: $1 -> $newR\n";
                s/$1/$newR/g; 
            }
        } elsif ($_ =~ /X:source \/ gate.WEAK_2P/) {
            if ($_ =~ /PL=(\d+\.\d+e-\d+) PW=(\d+\.\d+e-\d+)/) {
                print "PL: $1 -> $PL  PW: $2 -> $PW \n";
                s/PL=$1 PW=$2/PL=$PL PW=$PW/g;
            } elsif ($_ =~ /PL=(\d+e-\d+) PW=(\d+e-\d+)/) {
                print "PL: $1 -> $PL  PW: $2 -> $PW \n";
                s/PL=$1 PW=$2/PL=$PL PW=$PW/g;
            }
        } elsif ($_ =~ /X1:source \/ gate.STATICIZER/) {
            if ($_ =~ /PL=(\d+\.\d+e-\d+) PW=(\d+\.\d+e-\d+)/) {
                print "PL1: $1 -> $PL1  PW1: $2 -> $PW1 \n";
                s/PL=$1 PW=$2/PL=$PL1 PW=$PW1/g;
            } elsif ($_ =~ /PL=(\d+e-\d+) PW=(\d+e-\d+)/) {
                print "PL1: $1 -> $PL1  PW1: $2 -> $PW1 \n";
                s/PL=$1 PW=$2/PL=$PL1 PW=$PW1/g;
            }
        } elsif ($_ =~ /X2:source \/ gate.STATICIZER/) {
            if ($_ =~ /PL=(\d+\.\d+e-\d+) PW=(\d+\.\d+e-\d+)/) {
                print "PL2: $1 -> $PL2  PW2: $2 -> $PW2 \n";
                s/PL=$1 PW=$2/PL=$PL2 PW=$PW2/g;
            } elsif ($_ =~ /PL=(\d+e-\d+) PW=(\d+e-\d+)/) {
                print "PL2: $1 -> $PL2  PW2: $2 -> $PW2 \n";
                s/PL=$1 PW=$2/PL=$PL2 PW=$PW2/g;
            }
        }
        print SPICE $_;
    }
    close(ORG_SPICE);
    close(SPICE);
}

#-------------------------------------------------------------------------------
#	get_newRC - 
#	get_newRC_for_pcand -
#-------------------------------------------------------------------------------

sub get_newRC {
    my ($jauto) = @_;

    open(WIRES_DEBUG, "$jauto/wires.debug") ||
        die("*Error> Cannot open wires.debug in \"$jauto\".\n");
    while (<WIRES_DEBUG>) {
        if ($_ =~ / _X.1 (.*)$/) {
            $var_x1 = $1;
        } elsif ($_ =~ / _X.0 (.*)$/) {
            $var_x0 = $1;
        }
    }
    close(WIRES_DEBUG);

    #- wire_length:
    #- logic2: 4u, logic3/4/5/6: 5u
    #- logic2nand: 5u, logic3/4/5/6nand: 6u
    my $wireLength = 0;
    if ($NUM_INPUTS > 2) {
        $wireLength = 5;
    } else {
        $wireLength = ($NUM_INPUTS + 2);
    }
    $wireLength = ($wireLength + 1) if ($TASK_NAME eq "nand");

    my $varL  = 0;
    my $varC  = 0;
    my $varR  = 0;
    $varL  = $1 if ($var_x0 =~ /L=(\d+\.\d+)/);
    $varC  = $1 if ($var_x0 =~ /C=(\d+\.\d+)/);
    $varR  = $1 if ($var_x0 =~ /R=(\d+\.\d+)/);
    if ($varL == 0) {
        $varL  = $1 if ($var_x0 =~ /L=(\d+)/);
    }
    if ($varC == 0) {
        $varC  = $1 if ($var_x0 =~ /C=(\d+)/);
    }
    if ($varR == 0) {
        $varR  = $1 if ($var_x0 =~ /R=(\d+)/);
    }

    my $newC0 = $varC * $wireLength / ($varL * 2);
    my $newR0 = $varR * $wireLength / ($varL);

    $varL  = 0;
    $varC  = 0;
    $varR  = 0;
    $varL  = $1 if ($var_x1 =~ /L=(\d+\.\d+)/);
    $varC  = $1 if ($var_x1 =~ /C=(\d+\.\d+)/);
    $varR  = $1 if ($var_x1 =~ /R=(\d+\.\d+)/);
    if ($varL == 0) {
        $varL  = $1 if ($var_x1 =~ /L=(\d+)/);
    }
    if ($varC == 0) {
        $varC  = $1 if ($var_x1 =~ /C=(\d+)/);
    }
    if ($varR == 0) {
        $varR  = $1 if ($var_x1 =~ /R=(\d+)/);
    }

    my $newC1 = $varC * $wireLength / ($varL * 2);
    my $newR1 = $varR * $wireLength / ($varL);
 
    return($newC0,$newR0,$newC1,$newR1);
}

sub get_newRC_for_pcand {
    my ($jauto) = @_;

    open(WIRES_DEBUG, "$jauto/wires.debug") ||
        die("*Error> Cannot open wires.debug in \"$jauto\".\n");
    while (<WIRES_DEBUG>) {
        if ($_ =~ / _X (.*)$/) {
            $var = $1;
        } elsif ($_ =~ / _X1 (.*)$/) {
            $var = $1;
        }
    }
    close(WIRES_DEBUG);

    #- wire_length:
    #- logic2: 4u, logic3/4/5/6: 5u
    #- logic2nand: 5u, logic3/4/5/6nand: 6u
    my $wireLength = 0;
    if ($NUM_INPUTS > 2) {
        $wireLength = 5;
    } else {
        $wireLength = ($NUM_INPUTS + 2);
    }

    my $varL  = $1 if ($var =~ /L=(\d+\.\d+)/);
    my $varC  = $1 if ($var =~ /C=(\d+\.\d+)/);
    my $varR  = $1 if ($var =~ /R=(\d+\.\d+)/);
    if ($varL == 0) {
        $varL  = $1 if ($var =~ /L=(\d+)/);
    }
    if ($varC == 0) {
        $varC  = $1 if ($var =~ /C=(\d+)/);
    }
    if ($varR == 0) {
        $varR  = $1 if ($var =~ /R=(\d+)/);
    }

    my $newC = $varC * $wireLength / ($varL * 2);
    my $newR = $varR * $wireLength / ($varL);

    return($newC,$newR);
}

#-------------------------------------------------------------------------------
#	get_jauto_dir - 
#-------------------------------------------------------------------------------

sub get_jauto_dir {
    my ($dir) = @_;
    opendir(IMD, $dir) || die ("*Error> Cannot open directory!");
    my @files = readdir(IMD);
    closedir(IMD);

    foreach $logfile (@files) {
        my $found  = 0;
        my $jauto = "";
        if ($logfile =~ /^lve.log/) {
            open(JAUTO_LOG, "$dir/$logfile") ||
                die("*Error> Cannot open file \"$logfile\".\n");
            while (<JAUTO_LOG>) {
                if ($_ =~ /Cell name: (.*)$/) {
                    if ($1 eq $CELL_NAME) {
                        $found = 1;
                    }
                }
                $jauto = $1 if ($_ =~ /Output directory name: (.*)$/);
            }
            close(JAUTO_LOG);
        }
        if (($found == 1) && ($jauto ne "")) {
            return($jauto);
        }
    }
}

#-------------------------------------------------------------------------------
#	pathalyze - 
#       pathalyze_for_pcand -
#-------------------------------------------------------------------------------

sub pathalyze {
    my ($mode)   = @_;
    ($voltage, $temp, $corner) = split("_", $PVTS[0]);

    my $timingTable   = "$VARS{TIMING_DIR}/timing_arcs_table/";
    if ($GATE_NAME    =~ /^PC_AND/) {
        $timingTable .= "$GATE_NAME.spec";
    } else {
        $timingTable .= "AtoX$NUM_INPUTS\_$mode.spec";
    }
    my $cellDir = "logic/mld/sizing/$GATE_NAME/10000";

    my $command = "fulcrum pathalyze ";
    $command   .= "--spec-dir=$VARS{CAST_DIR}:$VARS{SPEC_DIR}/cast ";
    $command   .= "--view=floorplan --mode=estimated ";
    $command   .= "--voltage=$voltage --temp=$temp --corner=$corner ";

    foreach $cap (@CAP_VALUES) {
        foreach $input_slew (@SLEW_RATES) {
            my $lveDir  = "$VARS{BASE_DIR}/$cap";

            my $doneDir = "$lveDir/$cellDir/floorplan/estimated/alint/";
            $doneDir   .= "tt/1.0V/25C/";
            if (!-e "$doneDir/alint.done") {
                #- need a wait statement.
                print "*Error> cannot find \"alint.done\" in $doneDir\n";
            }

            my $pathalyze = "$command --slew --input-slew=$input_slew ";
            $pathalyze   .= "--path-dir=$cap/slew_$input_slew ";
            $pathalyze   .= "--root-subtype=$CELL_NAME ";
            $pathalyze   .= "--lve-dir=$lveDir ";
            print "$pathalyze $timingTable\n";
            system("$pathalyze $timingTable");
        }
    }

    print("pathalyze is done!\n");
}

#-------------------------------------------------------------------------------
#	gen_image_delay - 
#	for image cells
#-------------------------------------------------------------------------------

sub gen_image_delay {
    my (@timing_sense) = @_;
    for (my $i = 0; $i < $NUM_INPUTS; $i++) {
        my $pinName = "A$i\_X0_F";
        push @pin_names, $pinName;
        $pinName = "A$i\_X1_R";
        push @pin_names, $pinName;
    }
    my %DELAY_VAL   = ();
    my %SLEW_VAL    = ();

    #- get the delay and slew values from result files.
    foreach $input_slew (@SLEW_RATES) {
        foreach $cap (@CAP_VALUES) {
            my $base_name   = "$cap/slew_$input_slew";
            my $result_path = "$VARS{BASE_DIR}/$base_name/result";
            my ($delay_list,$slew_list) = get_result($result_path);
            my @delay_vals  = split(/\s+/, $delay_list);
            my @slew_vals   = split(/\s+/, $slew_list);
            my $k = 0;
            foreach $pin_name (@pin_names) {
                push @{$DELAY_VAL{$input_slew,$pin_name}}, $delay_vals[$k];
                push @{$SLEW_VAL{$input_slew,$pin_name}},  $slew_vals[$k];
                $k++;
            }
        }
    }

    my $index1 = get_slew_index(@SLEW_RATES);
    my $index2 = get_cap_index(@CAP_VALUES);

    my $pos = 0;
    for (my $j = 0; $j < $NUM_INPUTS; $j++, $pos++) {
        my $fileName = "$GATE_NAME.$j.out0";
        open(OUTFILE,">$fileName") || 
            die("*Error> couldn't open the file \"$fileName\".\n");
        print OUTFILE "    timing() {\n";
        print OUTFILE "      related_pin : \"A_L_$j\_R__D\" ;\n";
        print OUTFILE "      timing_sense : $timing_sense[$j] ;\n";
        print OUTFILE "      cell_rise(delay_template_7x7) {\n";
        print OUTFILE "        index_1 (\"$index1\");\n";
        print OUTFILE "        index_2 (\"$index2\");\n";
        print OUTFILE "        values ( \\\n";
        foreach $input_slew (@SLEW_RATES) {
            my $str1 = get_value(@{$DELAY_VAL{$input_slew,$pin_names[$pos]}});
            print OUTFILE ",	\\\n" unless ($input_slew == $SLEW_RATES[0]);
            print OUTFILE "          $str1";
        }
        print OUTFILE ");\n      }\n";
        print OUTFILE "      rise_transition(delay_template_7x7) {\n";
        print OUTFILE "        index_1 (\"$index1\");\n";
        print OUTFILE "        index_2 (\"$index2\");\n";
        print OUTFILE "        values ( \\\n";
        foreach $input_slew (@SLEW_RATES) {
            my $str2 = get_value(@{$SLEW_VAL{$input_slew,$pin_names[$pos]}});
            print OUTFILE ",	\\\n" unless ($input_slew == $SLEW_RATES[0]);
            print OUTFILE "          $str2";
        }
        print OUTFILE ");\n      }\n";
        $pos++;

        print OUTFILE "      cell_fall(delay_template_7x7) {\n";
        print OUTFILE "        index_1 (\"$index1\");\n";
        print OUTFILE "        index_2 (\"$index2\");\n";
        print OUTFILE "        values ( \\\n";
        foreach $input_slew (@SLEW_RATES) {
            my $str3 = get_value(@{$DELAY_VAL{$input_slew,$pin_names[$pos]}});
            print OUTFILE ",	\\\n" unless ($input_slew == $SLEW_RATES[0]);
            print OUTFILE "          $str3";
        }
        print OUTFILE ");\n      }\n";
        print OUTFILE "      fall_transition(delay_template_7x7) {\n";
        print OUTFILE "        index_1 (\"$index1\");\n";
        print OUTFILE "        index_2 (\"$index2\");\n";
        print OUTFILE "        values ( \\\n";
        foreach $input_slew (@SLEW_RATES) {
            my $string   = get_value(@{$SLEW_VAL{$input_slew,$pin_names[$pos]}});
            print OUTFILE ",	\\\n" unless ($input_slew == $SLEW_RATES[0]);
            print OUTFILE "          $string";
        }
        print OUTFILE ");\n      }\n";
        print OUTFILE "    }\n";
        close (OUTFILE);
    }
}

#-------------------------------------------------------------------------------
#	gen_dual_delay - 
#	for dual-rail cells
#	number of timing arcs
#	    (A0_X0_R, A1_X0_R, ...): NUM_INPUTS
#	    go_X0_R, go_X0_F       : 2
#	    (A0_X1_R, A1_X1_R, ...): NUM_INPUTS
#	    go_X1_R, go_X1_F       : 2
#	    (A0_V_R, A1_V_R, ...)  : NUM_INPUTS
#	    go_V_R, go_V_F         : 2
#       $nTimeArcs = 2 * $NUM_INPUTS + 4 if ($mode eq "dual");
#       $nTimeArcs = 3 * $NUM_INPUTS + 6 if ($mode eq "nand");
#-------------------------------------------------------------------------------

sub gen_dual_delay {
    my ($pos_func,$neg_func) = @_;
    my (@pin_names) = get_related_pin_list($TASK_NAME);
    my %DELAY_VAL   = ();
    my %SLEW_VAL    = ();

    foreach $input_slew (@SLEW_RATES) {
        foreach $cap (@CAP_VALUES) {
            my $base_name   = "$cap/slew_$input_slew";
            my $result_path = "$VARS{BASE_DIR}/$base_name/result";
            my ($delay_list,$slew_list) = get_result($result_path);
            my @delay_vals  = split(/\s+/, $delay_list);
            my @slew_vals   = split(/\s+/, $slew_list);
            my $i = 0;
            foreach $pin_name (@pin_names) {
                push @{$DELAY_VAL{$input_slew,$pin_name}}, $delay_vals[$i];
                push @{$SLEW_VAL{$input_slew,$pin_name}},  $slew_vals[$i];
                $i++;
            }
        }
    }

    my $index1 = get_slew_index(@SLEW_RATES);
    my $index2 = get_cap_index(@CAP_VALUES);

    my @out_pins = ("X_D_0", "X_D_1");
    push @out_pins, "V" if ($TASK_NAME eq "nand");
    my $i = 0;

    #- GATE_NAME.out0 for X_L_0_R__D_0 which includes param A$i_X0_R and go_X0_R/F
    #- GATE_NAME.out1 for X_L_0_R__D_1 which includes param A$i_X1_R and go_X1_R/F
    #- GATE_NAME.out2 for X_L_0_R__C which includes param A$i_V_R and go_V_R/F
    #- e.g. 2-input gates
    #- out0: param A0_X0_R, A1_X0_R, go_X0_R and go_X0_F
    #- out1: param A0_X1_R, A1_X1_R, go_X1_R and go_X1_F
    #- out2: param A0_V_R,  A1_V_R,  go_V_R  and go_V_F
    for (my $num_out = 0; $num_out <= $#out_pins; $num_out++) {
        open(OUTFILE,">$GATE_NAME.out$num_out") || 
            die("*Error> couldn't open the file \"$GATE_NAME.out$num_out\".\n");
        my $funcName = get_function($num_out,$pos_func,$neg_func);

        for ($j = 0; $j < $NUM_INPUTS; $j++, $i++) {
            my (@rpinList)  = get_pin_prop($pin_names[$i]);

            #- for dual-rail pins such as _0 and _1
            #- Bug-fix: The unateness of all logic inputs to all outputs 
            #  in the dual-rail library should be positive-unate
            #  as the rising transition of a pin can only cause
            #  the rising transition of an output.
            for (my $k = 0; $k < 2; $k++) {
                if (is_pos_unate($rpinList[$k],$funcName)) {
                    $pinName = $rpinList[$k];
                }
            }

            print OUTFILE "    timing() {\n";
            print OUTFILE "      related_pin : \"$pinName\" ;\n";
            print OUTFILE "      timing_sense : positive_unate ;\n";
            print OUTFILE "      cell_rise(delay_template_7x7) {\n";
            print OUTFILE "        index_1 (\"$index1\");\n";
            print OUTFILE "        index_2 (\"$index2\");\n";
            print OUTFILE "        values ( \\\n";
            foreach $input_slew (@SLEW_RATES) {
                my $s1 = get_value(@{$DELAY_VAL{$input_slew,$pin_names[$i]}});
                print OUTFILE ",	\\\n" unless ($input_slew == $SLEW_RATES[0]);
                print OUTFILE "          $s1";
            }
            print OUTFILE ");\n      }\n";
            print OUTFILE "      rise_transition(delay_template_7x7) {\n";
            print OUTFILE "        index_1 (\"$index1\");\n";
            print OUTFILE "        index_2 (\"$index2\");\n";
            print OUTFILE "        values ( \\\n";
            foreach $input_slew (@SLEW_RATES) {
                my $s2 = get_value(@{$SLEW_VAL{$input_slew,$pin_names[$i]}});
                print OUTFILE ",	\\\n" unless ($input_slew == $SLEW_RATES[0]);
                print OUTFILE "          $s2";
            }
            print OUTFILE ");\n      }\n";
            print OUTFILE "    }\n";
        }

        print OUTFILE "    timing() {\n";
        print OUTFILE "      related_pin : \"go\" ;\n";
        print OUTFILE "      cell_rise(delay_template_7x7) {\n";
        print OUTFILE "        index_1 (\"$index1\");\n";
        print OUTFILE "        index_2 (\"$index2\");\n";
        print OUTFILE "        values ( \\\n";
        foreach $input_slew (@SLEW_RATES) {
            my $string   = get_value(@{$DELAY_VAL{$input_slew,$pin_names[$i]}});
            print OUTFILE ",	\\\n" unless ($input_slew == $SLEW_RATES[0]);
            print OUTFILE "          $string";
        }
        print OUTFILE ");\n      }\n";
        print OUTFILE "      rise_transition(delay_template_7x7) {\n";
        print OUTFILE "        index_1 (\"$index1\");\n";
        print OUTFILE "        index_2 (\"$index2\");\n";
        print OUTFILE "        values ( \\\n";
        foreach $input_slew (@SLEW_RATES) {
            my $string   = get_value(@{$SLEW_VAL{$input_slew,$pin_names[$i]}});
            print OUTFILE ",	\\\n" unless ($input_slew == $SLEW_RATES[0]);
            print OUTFILE "          $string";
        }
        print OUTFILE ");\n      }\n";
        $i++;
        print OUTFILE "      cell_fall(delay_template_7x7) {\n";
        print OUTFILE "        index_1 (\"$index1\");\n";
        print OUTFILE "        index_2 (\"$index2\");\n";
        print OUTFILE "        values ( \\\n";
        foreach $input_slew (@SLEW_RATES) {
            my $string   = get_value(@{$DELAY_VAL{$input_slew,$pin_names[$i]}});
            print OUTFILE ",	\\\n" unless ($input_slew == $SLEW_RATES[0]);
            print OUTFILE "          $string";
        }
        print OUTFILE ");\n      }\n";
        print OUTFILE "      fall_transition(delay_template_7x7) {\n";
        print OUTFILE "        index_1 (\"$index1\");\n";
        print OUTFILE "        index_2 (\"$index2\");\n";
        print OUTFILE "        values ( \\\n";
        foreach $input_slew (@SLEW_RATES) {
            my $string   = get_value(@{$SLEW_VAL{$input_slew,$pin_names[$i]}});
            print OUTFILE ",	\\\n" unless ($input_slew == $SLEW_RATES[0]);
            print OUTFILE "          $string";
        }
        print OUTFILE ");\n      }\n";
        $i++;
        print OUTFILE "    }\n";
        close (OUTFILE);
    }
}

#-------------------------------------------------------------------------------
#	gen_delay_for_pcand - 
#-------------------------------------------------------------------------------

sub gen_delay_for_pcand {
    my (@timing_sense) = @_;
    for (my $i = 0; $i < $NUM_INPUTS; $i++) {
        my $pinName = "A$i\_X";
        push @pin_names, $pinName;
    }
    push @pin_names, "go_X";
    my %DELAY_VAL   = ();
    my %SLEW_VAL    = ();

    #- get the delay and slew values from result files.
    foreach $input_slew (@SLEW_RATES) {
        foreach $cap (@CAP_VALUES) {
            my $base_name   = "$cap/slew_$input_slew";
            my $result_path = "$VARS{BASE_DIR}/$base_name/result";
            my ($delay_list,$slew_list) = get_result($result_path);
            my @delay_vals  = split(/\s+/, $delay_list);
            my @slew_vals   = split(/\s+/, $slew_list);
            my $k = 0;
            foreach $pin_name (@pin_names) {
                push @{$DELAY_VAL{$input_slew,$pin_name}}, $delay_vals[$k];
                push @{$SLEW_VAL{$input_slew,$pin_name}},  $slew_vals[$k];
                $k++;
            }
        }
    }

    my $index1 = get_slew_index(@SLEW_RATES);
    my $index2 = get_cap_index(@CAP_VALUES);

    for (my $j = 0; $j < $NUM_INPUTS; $j++) {
        my $fileName = "$GATE_NAME.$j.out0";
        open(OUTFILE,">$fileName") || 
            die("*Error> couldn't open the file \"$fileName\".\n");
        print OUTFILE "    timing() {\n";
        print OUTFILE "      related_pin : \"A_L_$j\_R__D\" ;\n";
        print OUTFILE "      timing_sense : $timing_sense[$j] ;\n";
        print OUTFILE "      cell_rise(delay_template_7x7) {\n";
        print OUTFILE "        index_1 (\"$index1\");\n";
        print OUTFILE "        index_2 (\"$index2\");\n";
        print OUTFILE "        values ( \\\n";
        foreach $input_slew (@SLEW_RATES) {
            my $str1 = get_value(@{$DELAY_VAL{$input_slew,$pin_names[$j]}});
            print OUTFILE ",	\\\n" unless ($input_slew == $SLEW_RATES[0]);
            print OUTFILE "          $str1";
        }
        print OUTFILE ");\n      }\n";
        print OUTFILE "      rise_transition(delay_template_7x7) {\n";
        print OUTFILE "        index_1 (\"$index1\");\n";
        print OUTFILE "        index_2 (\"$index2\");\n";
        print OUTFILE "        values ( \\\n";
        foreach $input_slew (@SLEW_RATES) {
            my $str2 = get_value(@{$SLEW_VAL{$input_slew,$pin_names[$j]}});
            print OUTFILE ",	\\\n" unless ($input_slew == $SLEW_RATES[0]);
            print OUTFILE "          $str2";
        }
        print OUTFILE ");\n      }\n";
        print OUTFILE "    }\n";
    }
    print OUTFILE "    timing() {\n";
    print OUTFILE "      related_pin : \"go\" ;\n";
    print OUTFILE "      cell_rise(delay_template_7x7) {\n";
    print OUTFILE "        index_1 (\"$index1\");\n";
    print OUTFILE "        index_2 (\"$index2\");\n";
    print OUTFILE "        values ( \\\n";
    foreach $input_slew (@SLEW_RATES) {
        my $str = get_value(@{$DELAY_VAL{$input_slew,$pin_names[$NUM_INPUTS]}});
        print OUTFILE ",    \\\n" unless ($input_slew == $SLEW_RATES[0]);
        print OUTFILE "          $str";
    }
    print OUTFILE ");\n      }\n";
    print OUTFILE "      rise_transition(delay_template_7x7) {\n";
    print OUTFILE "        index_1 (\"$index1\");\n";
    print OUTFILE "        index_2 (\"$index2\");\n";
    print OUTFILE "        values ( \\\n";
    foreach $input_slew (@SLEW_RATES) {
        my $str = get_value(@{$SLEW_VAL{$input_slew,$pin_names[$NUM_INPUTS]}});
        print OUTFILE ",    \\\n" unless ($input_slew == $SLEW_RATES[0]);
        print OUTFILE "          $str";
    }
    print OUTFILE ");\n      }\n";
    print OUTFILE "    }\n";
    close (OUTFILE);
}

#-------------------------------------------------------------------------------
#	gen_area - 
#-------------------------------------------------------------------------------

sub gen_area {
    my $cast_dir = "$VARS{CAST_DIR}:$VARS{SPEC_DIR}/cast";
    my $query    = "fulcrum cast_query --cast-path=$cast_dir ";
    $query      .= "--cell=\"$CELL_NAME\"";

    my $query_cmd1  = "$query --task=transistors";
    my $query_cmd2  = "$query --task=density";
    my $area_result = "result.$GATE_NAME.area";

    system("$query_cmd1 >  $area_result");
    system("$query_cmd2 >> $area_result");

    open(AREA_RESULT,"$VARS{BASE_DIR}/$area_result") || 
        die("*Error> cannot open the file \"$area_result\".\n");
    my $area_val = 1000000000000;
    while (<AREA_RESULT>) {
        my @split_val = split(/\s+/, $_);
        $area_val = $area_val * $split_val[$#split_val];
    }

    return $area_val;
}

#-------------------------------------------------------------------------------
#	gen_capacitance - 
#-------------------------------------------------------------------------------

sub gen_capacitance {
    my @input_pins = get_inputs($NUM_INPUTS);
    my $outfile    = "result.cap";

    my ($cell_dir) = $CELL_NAME;
    $cell_dir =~ tr/\./\//;

    my $tsmc13_pdk = "/usr/local/fulcrum/pdk/fulcrum-tsmc13-pdk/59207";
    my $spice_file = "$VARS{BASE_DIR}/$CAP_VALUES[0]/logic/mld";
    $spice_file   .= "/sizing/$GATE_NAME/10000/";
    $spice_file   .= "floorplan/estimated/cell.spice";

    my $captally   = "fulcrum captally ";
    $captally     .= "--fulcrum-pdk-root $tsmc13_pdk ";
    $captally     .= "--cdl $spice_file ";
    system("$captally > $outfile");
    my @capval_list = get_cap_value($outfile);

    return @capval_list;
}

#-------------------------------------------------------------------------------
#	Subroutines- 
#-------------------------------------------------------------------------------

sub write_infile {
    my ($infile) = @_;
    my $str = "";
    $str   .= "bumpCC 0 1\n";
    $str   .= "delayCC 0\n";
    $str   .= "bumpTau 4e-11\n";
    $str   .= "delayTau 2e-11 4e-11 6.4e-11 11e-11 19.9e-11 38.1e-11 74.5e-11\n";
    foreach $node (@nodes) {
        $str .= "output \"$node.out\"\n";
        $str .= "outerr \"$node.err\"\n";
        $str .= "alint $node\n";
    }
    $str .= "output \"alint.done\"\n";
    $str .= "echo done\n";
    $str .= "output\n";
    $str .= "outerr\n";

    open  INFILE, ">$infile";
    print INFILE $str;
    close INFILE;
}

sub write_props {
    my ($localprops) = @_;
    my $str = "";
    foreach $node(@nodes) {
        $str .= "$node 0 1000 1000 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1\n";
    }
    open  PROPS, ">$localprops";
    print PROPS $str;
    close PROPS;
}

sub write_todo {
    my ($todo) = @_;
    open  TODO, ">$todo";
    print TODO $CELL_NAME;
    close TODO;
}

sub write_capfile {
    my ($capfile_name,$pvt_val) = @_;
    open  CAPFILE, ">$capfile_name";
    my ($true, $temp, $corner) = split("_", $pvt_val);
    $header  = "";
    $header .= ".true=$true;\n";
    $header .= ".temperature=$temp;\n";
    $header .= ".corner \"$corner\";\n";
    $header .= ".include \"tech.asp\";\n";
    $header .= ".include \"cell.aspice\";\n";
    $header .= ".include \"noprs.asp\";\n";
    $header .= ".poststep=5e-12;\n";
    $header .= ".max_bump_fanin_aggressors=2;\n";
    $header .= ".max_delay_fanin_aggressors=1;\n";
    $header .= ".prstau=4e-11;\n";

    $capstr  = "";
    foreach $node (@nodes_withcap) {
        $capstr .= "cap (\"$node\")($cap);\n";
    }

    print CAPFILE $header;
    print CAPFILE $capstr;
    close CAPFILE;
}

sub write_sweep {
    my ($sweepfile_name,$pvt_val) = @_;
    open  SWEEP, ">$sweepfile_name";
    ($true, $temp, $corner) = split("_", $pvt_val);
    print SWEEP "--corner=$corner\n";
    print SWEEP "--true=$true\n";
    print SWEEP "--temp=$temp\n";
    close SWEEP;
}

sub get_delay_info {
    my $path = shift;
    my $rise_delay = 0;
    my $fall_delay = 0;
    my $rise_slew  = 0;
    my $fall_slew  = 0;
    my $obj_name   = "";

    open(RSTFILE,"$path") || 
        die("*Error> cannot open the result file.\n");
    while (<RSTFILE>) {
        if (/delay/) {
            ($obj_name, $rise_delay, $fall_delay) = split(/\s+/, $_);
        } elsif (/slew/) {
            s/slew//g;
            ($obj_name, $rise_slew, $fall_slew)   = split(/\s+/, $_);
        }
    }
    close(RSTFILE);
    return($rise_delay,$fall_delay,$rise_slew,$fall_slew);
}

sub get_related_pin_list {
    my $mode = shift;
    my @pinList = ();

    #- X0 rise: A0_X0_R A1_X0_R ... go_X0_R
    for (my $i = 0; $i < $NUM_INPUTS; $i++) {
        my $pinName = "A$i\_X0_R";
        push @pinList, $pinName;
    }
    push @pinList, "go_X0_R";

    #- X0 fall: go_X0_F
    push @pinList, "go_X0_F";

    #- X1 rise: A0_X1_R A1_X1_R ... go_X1_R
    for ($i = 0; $i < $NUM_INPUTS; $i++) {
        push @pinList, "A$i\_X1_R";
    }
    push @pinList, "go_X1_R";

    #- X1 fall: go_X1_F
    push @pinList, "go_X1_F";

    #- V rise and fall: A0_V_R A1_V_R ... go_V_R go_V_F
    if ($mode eq "nand") {
        for (my $i = 0; $i < $NUM_INPUTS; $i++) {
            push @pinList, "A$i\_V_R";
        }
        push @pinList, "go_V_R";
        push @pinList, "go_V_F";
    }

    return @pinList;
}

sub get_slew_index {
    my (@slew_rates) = @_;
    my $index1   = "";
    foreach $input_slew (@slew_rates) {
        $index1 .= ", " unless ($index1 eq "");
        $input_slew = $input_slew / 1000;
        $index1 .= $input_slew;
    }
    return $index1;
}

sub get_cap_index {
    my (@cap_values) = @_;
    my $index2   = "";
    foreach $cap (@cap_values) {
        $index2 .= ", " unless ($index2 eq "");
        my ($cap_val,$scale_val) = split(/e-1/,$cap);
        $index2 .= $cap_val;
    }
 
    return $index2;
}

sub get_value {
    my (@values) = @_;
    my $val_string = "\"";
    my $prev_value = "";
    my $value = 0;

    for (my $i = 0; $i < $#values; $i++) {
        $value = $values[$i]/1000;
        $val_string .= "$value, ";
    }
    $value = $values[$#values]/1000;
    $val_string .= "$value\"";

    return $val_string;
}

sub get_index_val {
    my (@slew_rates,@cap_values) = @_;
    my $index1   = "";
    foreach $input_slew (@slew_rates) {
        $index1 .= ", " unless ($index1 eq "");
        $input_slew = $input_slew / 1000;
        $index1 .= $input_slew;
    }

    my $index2   = "";
    foreach $cap (@cap_values) {
        $index2 .= ", " unless ($index2 eq "");
        my ($cap_val,$scale_val) = split(/e-1/,$cap);
        $index2 .= $cap_val;
    }
 
    return($index1,$index2);
}

sub get_function {
    my ($order,$posFunc,$negFunc) = @_;
    if ($order == 0) {
        $funcName = $negFunc;
    } elsif ($order == 1) {
        $funcName = $posFunc;
    } else {
        $funcName = "$negFunc $posFunc";
    }
    return($funcName);
}

sub get_pin_prop {
    my $path = shift;
    my @pinName;

    if ($path =~ /A(\d+)/) {
        push @pinName, "A_L_$1\_R__D_0";
        push @pinName, "A_L_$1\_R__D_1";
    } elsif ($path =~ /go/) {
        push @pinName, "go";
    }

    return(@pinName);
}

#- CHECK 1118: non_unate!
#- There is no non-unate in dual-rail lib -> take a look at this more carefully.
sub is_pos_unate {
    my ($pinName,$funcName) = @_;

    if ($funcName =~ /$pinName/) {
        #- positive_unate
        return(1);
    } else {
        #- negative_unate
        return(0);
    }
}

sub get_result {
    my $path = shift;
    open(RSTFILE,"$path") || 
        die("*Error> cannot open intermediate timing arcs, \"$path\".\n");
    while (<RSTFILE>) {
        if (/related_pin/) {
#            s/related_pin //g;
#            s/^\s+//g;
#            $pinList = $_;
        } elsif (/delay/) {
            s/delay //g;
            s/^\s+//g;
            $delayList = $_;
        } elsif (/slew/) {
            s/slew //g;
            s/^\s+//g;
            $slewList = $_;
        }
    }
    close(RSTFILE);
    return($delayList,$slewList);
}

sub get_path_delay {
    my $path = shift;
    opendir(IMD, $path) || 
        die("*Error> cannot open directory $path!\n");
    @thefiles = readdir(IMD);
    closedir(IMD);

    my @fall_delay_list = ();
    my @rise_delay_list = ();
    foreach $infile (@thefiles) {
        unless ($infile eq "." || $infile eq "..") {
            $_ = $infile;
            open(INFILE,"$path/$infile") || 
                die("*Error> cannot open the file \"$infile\".\n");
            while (<INFILE>) {
                if (/X.0\+/) {
                    s/\n//g;
                    my @splitval = split(/\s+/, $_);
                    foreach $sval (@splitval) {
                        $delayval = $sval;
                    }
                    push @fall_delay_list, $delayval;
                } elsif (/X.1\+/) {
                    s/\n//g;
                    my @splitval = split(/\s+/, $_);
                    foreach $sval (@splitval) {
                        $delayval = $sval;
                    }
                    push @rise_delay_list, $delayval;
                }
            }
            close(INFILE);
        }
    }
    my $max_rise = 0;
    foreach $dval (@rise_delay_list) {
        $max_rise = $dval if ($dval > $max_rise);
    }
    my $max_fall = 0;
    foreach $dval (@fall_delay_list) {
        $max_fall = $dval if ($dval > $max_fall);
    }
    return($max_rise,$max_fall);
}

sub clean() {
    if ((-e "alint.in") || (-e "alint_0.00079e-12_1.1_90_tt.asp")) {
        system("rm -rf alint*");
    }
    if (-e "sweep_0.00079e-12_1.0_25_tt.sweep") {
        system("rm -rf sweep_*");
    }

    if (-e "$VARS{BASE_DIR}/cap_sweep_results") {
        system("rm -rf $VARS{BASE_DIR}/cap_sweep_results");
    }
    if (-e "$VARS{BASE_DIR}/cell.localprops") {
        system("rm -rf $VARS{BASE_DIR}/cell.localprops");
    }
    if (-e "$VARS{BASE_DIR}/result.cap") {
        system("rm -rf $VARS{BASE_DIR}/result.cap");
    }
    if (-e "$VARS{BASE_DIR}/result.area") {
        system("rm -rf $VARS{BASE_DIR}/result.area");
    }
    if (-e "$VARS{BASE_DIR}/cell.spice.flat") {
        system("rm -rf $VARS{BASE_DIR}/cell.spice.flat");
    }
}

sub get_cap_value {
    my ($capfile) = @_;
    open(CAPFILE,"$VARS{BASE_DIR}/$capfile") || 
        die("*Error> cannot open the file \"$capfile\".\n");
    my $go_cap   = "";
    my @splitval = "";
    my $value    = 0;
    while (<CAPFILE>) {
        if ($_ =~ /A\[/) {
            @splitval = split(/\s+/, $_);
            $value = $splitval[1];
            if ($value =~ /(\d+\.\d+)e-(\d+)/) {
                $value = $1 * 10**-$2 * 10**12;
                $value = round_cap($value,-10);
            }
            push @cap_val_list, $value;
        } elsif ($_ =~ /go/) {
            @splitval = split(/\s+/, $_);
            $value = $splitval[1];
            if ($value =~ /(\d+\.\d+)e-(\d+)/) {
                $value = $1 * 10**-$2 * 10**12;
                $go_cap = round_cap($value,-10);
            }
        }
    }
    close(CAPFILE);

    if (($GATE_NAME =~ /PC_AND/) || ($TASK_NAME ne "image")) {
        push @cap_val_list, $go_cap;
    }

    return @cap_val_list;
}

sub round_cap {
    my ($num,$digit) = @_;
    my $shifted_num = $num * 10**(-$digit);
    my $shifted_int = int($shifted_num);
    my $leftover = $shifted_num - $shifted_int;
    $shifted_int++ if ($leftover >= 0.5);
    return $shifted_int * 10**$digit;
}

sub get_inpin_name {
    my ($org_name) = @_;
    my ($root_name,$pin_name) = split(/\./, $org_name);
    my ($m) = ($root_name =~ /(\d)/);

    my $new_name = "A_L_$m\_R__D_$pin_name";
    return $new_name;
}

#-------------------------------------------------------------------------------
#	debug -
#-------------------------------------------------------------------------------

sub debug {
    exit 0;
}

#-------------------------------------------------------------------------------
#	gen_library
#-------------------------------------------------------------------------------

sub gen_library {
    pathalyze($TASK_NAME);

    #- get timing_sense and function from timing_sense '.ts' file.
    my @timing_sense = ();
    my $function = "";
    my $pos_func = "";
    my $neg_func = "";
    open(TSFILE, "$VARS{TIMING_DIR}/timing_sense_table/$TS_FILE") ||
        die("*Error> cannot open the file \"$TS_FILE\".\n");
    while (<TSFILE>) {
        last if ($_ =~ /^end/);
        if ($_ =~ /timing_sense : (.*)$/) {
            push @timing_sense, $1;
        } elsif ($_ =~ /function : (.*)$/) {
            $function = $1;
        } elsif ($_ =~ /pos_func : (.*)$/) {
            $pos_func = $1;
        } elsif ($_ =~ /neg_func : (.*)$/) {
            $neg_func = $1;
        }
    }
    close(TSFILE);

    print "Generate timing arcs ...\n";

    #- generate .info file.
    my $area_val = gen_area();
    my @cap_val  = gen_capacitance();

    #- generate timing file and .lib file.
    if ($GATE_NAME =~ /^PC_AND/) {
        gen_delay_for_pcand(@timing_sense);
        gen_library_for_pcand($function,$area_val,@cap_val);
    } elsif ($TASK_NAME eq "image") {
        gen_image_delay(@timing_sense);
        gen_image_library($function,$area_val,@cap_val);
    } else {
        gen_dual_delay($pos_func,$neg_func);
        gen_dual_library($pos_func,$neg_func,$area_val,@cap_val);
    }
}

#-------------------------------------------------------------------------------
#	gen_image_library
#-------------------------------------------------------------------------------

sub gen_image_library {
    my ($function,$area_val,@cap_val) = @_;
    my $libName = "$GATE_NAME.$TASK_NAME.lib";
    open(CELLLIB, ">$libName") ||    
        die("*Error> cannot open the file \"$libName\".\n");

    #- cell name is $cellName_IMAGE_X$strength, (e.g. LOGIC2_1_IMAGE_X1)
    #- so DR in $GATE_NAME should be replaced by IMAGE
    #- because $GATE_NAME is the same as a dual-rail name.
    my $cellName = $ROOT_NAME . "_IMAGE_X" . $STRENGTH;

    print CELLLIB "cell ($cellName) { \n";
    print CELLLIB "  area : $area_val\n";
    my $pos = 0;
    my $myCap = 0;
    for (my $i = 0; $i < $NUM_INPUTS; $i++, $pos = $pos+2) {
        #- find the maximum value of capacitance.
        if ($cap_val[$pos] > $cap_val[$pos+1]) {
            $myCap = $cap_val[$pos];
        } else {
            $myCap = $cap_val[$pos+1];
        }
        #- generate input pin list.
        print CELLLIB "  pin(A_L_$i\_R__D) {\n";
        print CELLLIB "    direction : input;\n";
        print CELLLIB "    capacitance : $myCap;\n";
        print CELLLIB "  }\n";
    }
    print CELLLIB "  pin(X_L_0_R__D) {\n";
    print CELLLIB "    direction : output;\n";
    print CELLLIB "    capacitance : 0.0;\n";
    print CELLLIB "    function : \"$function\" ;\n";
    close(CELLLIB);
    for (my $j = 0; $j < $NUM_INPUTS; $j++) {
        my $timingFile = "$GATE_NAME.$j.out0";
        system("cat $timingFile >> $libName");
    }
    open(CELLLIB, ">>$libName");
    print CELLLIB "  }\n";
    print CELLLIB "}\n";
    close(CELLLIB);

    system ("cat $libName >> $LIB_NAME");

    my $ncCellName = "";
    my $twistList = "";
    open(TSFILE, "$VARS{TIMING_DIR}/timing_sense_table/$TS_FILE") ||
        die("*Error> cannot open the file \"$TS_FILE\".\n");
    while (<TSFILE>) {
        if ($_ =~ /^NonCanonical (.*)$/) {
            $ncCellName = $1;
        } elsif ($_ =~ /twisted_pin: (.*)$/) {
            $twistList = $1;
        } elsif ($_ =~ /function : (.*)$/) {
            $function = $1;
        } elsif ($_ =~ /^end/) {
            if ($ncCellName ne "") {
                #- generate non-canonical gates.
                gen_noncanonical_gates($ncCellName,$twistList,$function) 
            }
            #- initialize.
            $ncCellName = "";
            $twistList  = "";
            $function   = "";
        }
    }
    close(TSFILE);
}

#-------------------------------------------------------------------------------
#	gen_noncanonical_gates
#-------------------------------------------------------------------------------

sub gen_noncanonical_gates {
    my ($cellName,$twistList,$function) = @_;
    my $canGate = "$GATE_NAME.$TASK_NAME.lib";
    my $nonCanGate = "$cellName\_X$STRENGTH.$TASK_NAME.lib";
    my $inverse = 0;
    open(REFFILE, "$VARS{BASE_DIR}/$canGate") ||
        die("*Error> cannot open the file \"$canGate\".\n");
    open(NCFILE, ">$VARS{BASE_DIR}/$nonCanGate") ||
        die("*Error> cannot open the file \"$nonCanGate\".\n");
    while (<REFFILE>) {
        if ($_ =~ /$ROOT_NAME/) {
            s/$ROOT_NAME/$cellName/g;
            print NCFILE $_;
        } elsif ($_ =~ /function :/) {
            print NCFILE "    function : \"$function\" ;\n";
        } elsif ($_ =~ /^  pin\((.*)$/) {
            my @pinName = split(/\)/,$1);
            $inverse = 1 if ($twistList =~ m/$pinName[0]/);
            print NCFILE $_;
        } elsif ($_ =~ /timing_sense (.*)$/) {
            if ($inverse == 1) {
                if ($_ =~ /negative_unate/) {
                    s/negative_unate/positive_unate/g;
                } elsif ($_ =~ /positive_unate/) {
                    s/positive_unate/negative_unate/g;
                }
                $inverse = 0;
            }
            print NCFILE $_;
        } else {
            print NCFILE $_;
        }
    }
    close(NCFILE);
    system ("cat $nonCanGate >> $LIB_NAME");
}

#-------------------------------------------------------------------------------
#	gen_dual_library
#-------------------------------------------------------------------------------

sub gen_dual_library {
    my ($pos_func,$neg_func,$area_val,@cap_val) = @_;
    my $libName = "$GATE_NAME.$TASK_NAME.lib";
    open(CELLLIB, ">$libName") ||    
        die("*Error> cannot open the file \"$libName\".\n");
 
    print CELLLIB "cell ($GATE_NAME) { \n";
    my $footPrint   = $ROOT_NAME;
    if ($TASK_NAME eq "dual") {
        $footPrint .= "_DR";
    } else {
        $footPrint .= "_C";
    }
    print CELLLIB "  cell_footprint : $footPrint\n";
    print CELLLIB "  area : $area_val\n";
    my $pos = 0;
    for (my $i = 0; $i < $NUM_INPUTS; $i++, $pos = $pos+2) {
        #- generate input pin list.
        print CELLLIB "  pin(A_L_$i\_R__D_0) {\n";
        print CELLLIB "    direction : input;\n";
        print CELLLIB "    capacitance : $cap_val[$pos];\n";
        print CELLLIB "  }\n";
        print CELLLIB "  pin(A_L_$i\_R__D_1) {\n";
        print CELLLIB "    direction : input;\n";
        print CELLLIB "    capacitance : $cap_val[$pos+1];\n";
        print CELLLIB "  }\n";
    }
    print CELLLIB "  pin(go) {\n";
    print CELLLIB "    direction : input;\n";
    print CELLLIB "    capacitance : $cap_val[$pos];\n";
    print CELLLIB "  }\n";

    print CELLLIB "  pin(X_D_0) {\n";
    print CELLLIB "    direction : output;\n";
    print CELLLIB "    capacitance : 0.0;\n";
    print CELLLIB "    function : \"$neg_func\" ;\n";
    close(CELLLIB);
    system("cat $GATE_NAME.out0 >> $libName");
    open(CELLLIB, ">>$libName");
    print CELLLIB "  }\n";

    print CELLLIB "  pin(X_D_1) {\n";
    print CELLLIB "    direction : output;\n";
    print CELLLIB "    capacitance : 0.0;\n";
    print CELLLIB "    function : \"$pos_func\" ;\n";
    close(CELLLIB);
    system("cat $GATE_NAME.out1 >> $libName");
    open(CELLLIB, ">>$libName");
    print CELLLIB "  }\n";

    if ($TASK_NAME eq "nand") {
        print CELLLIB "  pin(V) {\n";
        print CELLLIB "    direction : output;\n";
        print CELLLIB "    capacitance : 0.0;\n";
        print CELLLIB "    function : \"!$pos_func | !$neg_func\" ;\n";
        close(CELLLIB);
        system("cat $GATE_NAME.out2 >> $libName");
        open(CELLLIB, ">>$libName");
        print CELLLIB "  }\n";
    }

    print CELLLIB "}\n";
    close(CELLLIB);

    system ("cat $libName >> $LIB_NAME");
}

#-------------------------------------------------------------------------------
#       gen_library_for_pcand
#-------------------------------------------------------------------------------

sub gen_library_for_pcand {
    my ($function,$area_val,@cap_val) = @_;
    my $libName = "$GATE_NAME.lib";
    open(CELLLIB, ">$libName") ||    
        die("*Error> cannot open the file \"$libName\".\n");
    print CELLLIB "cell ($GATE_NAME) { \n";
    print CELLLIB "  area : $area_val\n";
    my $pos = 0;
    my $myCap = 0;
    for (my $i = 0; $i < $NUM_INPUTS; $i++) {
        #- generate input pin list.
        print CELLLIB "  pin(A_L_$i\_R__D) {\n";
        print CELLLIB "    direction : input;\n";
        print CELLLIB "    capacitance : $cap_val[$i];\n";
        print CELLLIB "  }\n";
    }
    print CELLLIB "  pin(go) {\n";
    print CELLLIB "    direction : input;\n";
    print CELLLIB "    capacitance : $cap_val[$NUM_INPUTS];\n";
    print CELLLIB "  }\n";
    print CELLLIB "  pin(X) {\n";
    print CELLLIB "    direction : output;\n";
    print CELLLIB "    capacitance : 0.0;\n";
    print CELLLIB "    function : \"$function\" ;\n";
    close(CELLLIB);
    for (my $i = 0; $i < $NUM_INPUTS; $i++) {
        my $timingFile = "$GATE_NAME.$i.out0";
        system("cat $timingFile >> $libName");
    }
    open(CELLLIB, ">>$libName");
    print CELLLIB "  }\n";
    print CELLLIB "}\n";
    close(CELLLIB);

    system ("cat $libName >> $LIB_NAME");
}

#-------------------------------------------------------------------------------
#	Program
#-------------------------------------------------------------------------------

debug()  if ($TASK_NAME eq "debug");

#- characterization using lve.
if ($LIB_NAME eq "") {
    lve_extract();
} elsif ($LIB_NAME eq "alint") {
    lve_alint();
#- generate library.
} else {
    gen_library(); 
}

