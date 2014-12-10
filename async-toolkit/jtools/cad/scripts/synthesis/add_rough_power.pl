#!/usr/bin/perl -w 

#-------------------------------------------------------------------------------
#	Program usage
#-------------------------------------------------------------------------------

sub usage_exit {
    print STDERR << "USAGE";
add_rough_power.pl
  [--lib=<libName>]   Dual-rail library to be modified 
                      (Default is 'mld_dual.lib' in //depot.)
  [--base-dir=<dir>]  Directory to output a new liberty (Default is './'.)
  [--round=<digit>]   Round digit to be generated (Default is 5.)

USAGE
    exit 1;
}

#-------------------------------------------------------------------------------
#	Global variables, paths and configuration
#-------------------------------------------------------------------------------

my %VARS = (
    LIB         => "/mnt/fulcrum/home/user/nkim/project/hw/layout/tsmc13/spar/synthesis/logic/mld/lib/mld_dual.lib",
    BASE_DIR    => "$ENV{PWD}",
    ROUND_DIGIT => 5
);

my @SLEW_RATE = ("0.014","0.028","0.044","0.076","0.138","0.264","0.516");
my @CAP_VALUE = ("0.00079", "0.002054", "0.00474", 
                 "0.010112", "0.020856", "0.042186", "0.08532");

#- the unit of these power value is 1fW, so they should muliply by 0.001
#- because 'leakage_power_unit' is 1pW in liberty files.
#- LOGIC_SLOPE and LOGIC_BASE sorted by slew rates
#- represent internal rise power for all logic input pins.
my %LOGIC_SLOPE = (
    0.014 => "0.309623 0.309721 0.310796 0.306219 0.305518 0.306378 0.357994",
    0.028 => "0.310483 0.311595 0.308154 0.307540 0.308836 0.306931 0.359715",
    0.044 => "0.313887 0.312578 0.307675 0.310096 0.309574 0.305518 0.353324",
    0.076 => "0.316400 0.318023 0.317685 0.307454 0.309819 0.315473 0.351788",
    0.138 => "0.331166 0.328536 0.324935 0.316745 0.320020 0.314305 0.347733",
    0.264 => "0.359100 0.354701 0.351327 0.337182 0.327270 0.329913 0.340543",
    0.516 => "0.428248 0.422084 0.415135 0.391268 0.371513 0.357318 0.346381"
);
my %LOGIC_BASE = (
    0.014 => "-1.61785 -0.937037 0.464886 3.36268 8.92468 19.8667 40.1884",
    0.028 => "-1.61317 -0.953005 0.516879 3.34018 8.86846 19.8563 40.1944",
    0.044 => "-1.66760 -0.987986 0.524934 3.29389 8.85229 19.8777 40.3341",
    0.076 => "-1.65844 -1.010290 0.390525 3.35488 8.86457 19.7410 40.4363",
    0.138 => "-1.74243 -1.086930 0.360777 3.26906 8.76018 19.8017 40.6737",
    0.264 => "-1.94264 -1.268250 0.126810 3.09011 8.75743 19.6594 41.1065",
    0.516 => "-2.56625 -1.89867 -0.490377 2.53926 8.31878 19.4436 41.5182"
);
#- GO_SLOPE and GO_BASE sorted by slew rates
#- represent internal fall power for 'go' pins.
my %GO_SLOPE = (
    0.014 => "0.852452 0.846731 0.834319 0.819166 0.806372 0.802507 0.788417",
    0.028 => "0.843800 0.837004 0.824622 0.809371 0.797407 0.792706 0.778315",
    0.044 => "0.839320 0.832475 0.819602 0.804682 0.792460 0.787127 0.769854",
    0.076 => "0.838313 0.830509 0.819153 0.802986 0.791293 0.787188 0.766081",
    0.138 => "0.845262 0.838313 0.826502 0.811552 0.799140 0.793751 0.768723",
    0.264 => "0.875058 0.867973 0.857798 0.843898 0.830724 0.824432 0.799127",
    0.516 => "0.977572 0.971740 0.960723 0.944808 0.927270 0.914041 0.891410"
);
my %GO_BASE = (
    0.014 => "-6.47351 -6.45882 -6.32595 -6.10899 -5.90715 -5.83875 -5.35318",
    0.028 => "-6.38576 -6.35329 -6.21938 -6.00434 -5.80955 -5.73090 -5.21588",
    0.044 => "-6.34505 -6.31204 -6.17169 -5.96207 -5.77518 -5.67465 -5.07511",
    0.076 => "-6.32540 -6.27928 -6.16160 -5.93680 -5.74151 -5.67658 -4.96460",
    0.138 => "-6.35469 -6.31540 -6.20144 -5.99985 -5.80298 -5.70771 -4.88259",
    0.264 => "-6.48758 -6.45805 -6.38143 -6.20784 -6.01404 -5.92140 -5.12259",
    0.516 => "-7.14352 -7.17636 -7.13830 -7.00041 -6.78257 -6.60802 -6.04918"
);

#-------------------------------------------------------------------------------
#	Command-line parsing
#-------------------------------------------------------------------------------

while (@ARGV) {
    if ($ARGV[0] =~ /^--lib=(.*)$/) {
        $VARS{LIB} = $1;
    } elsif ($ARGV[0] =~ /^--base-dir=(.*)$/) {
        $VARS{BASE_DIR} = $1;
    } elsif ($ARGV[0] =~ /^--round=(.*)$/) {
        $VARS{ROUND_DIGIT} = $1;
    } else {
        print STDERR "Unrecognized argument '$ARGV[0]'.\n";
        usage_exit();
    }
    shift;
}

#-------------------------------------------------------------------------------
#	generate_internal_power -
#	main function to generate internal rise power for all logic pins
#       and internal fall power for go.
#-------------------------------------------------------------------------------

sub generate_internal_power {
    my $libName   = "mld_dual.lib";
    open(ORG_LIB, "$VARS{LIB}") ||
        die("*Error> cannot open liberty \"$VARS{LIB}\".\n");
    open(NEW_LIB, "> $VARS{BASE_DIR}/$libName") ||
        die("*Error> cannot create a new liberty under $VARS{BASE_DIR}.\n");

    my $cellArea = 0;
    my $cellName = "";
    my $pinList  = "";
    my $values   = "";
    my $index1   = "";
    my $index2   = "";
    foreach my $i (0...$#SLEW_RATE) {
        $index1 .= "$SLEW_RATE[$i]";
        $index2 .= "$CAP_VALUE[$i]";
        unless ($i == $#SLEW_RATE) {
            $index1 .= ", ";
            $index2 .= ", ";
        }
    }
    while (<ORG_LIB>) {
        if ($_ =~ /cell \((.*)\)/) {
            $cellName = $1;
            $pinList  = get_pin_list($cellName);
            print NEW_LIB $_;
        } elsif ($_ =~ /area : (\d+\.\d+)/) {
            $cellArea = $1;
            print NEW_LIB $_;
        } elsif (($_ =~ /function :/) && ($pinList ne "")) {
            print NEW_LIB $_;
            print NEW_LIB "    internal_power() { \n";
            print NEW_LIB "      related_pin : \"$pinList\" ; \n";
            print NEW_LIB "      rise_power(energy_template_7x7) { \n";
            print NEW_LIB "        index_1 (\"$index1\"); \n";
            print NEW_LIB "        index_2 (\"$index2\"); \n";
            print NEW_LIB "        values ( \\ \n";
            foreach $slew (@SLEW_RATE) {
                $values = get_internal_power_for_logic($slew, $cellArea);
                print NEW_LIB "          $values\n";
            }
            print NEW_LIB "      } \n";
            print NEW_LIB "    } \n";
            print NEW_LIB "    internal_power() { \n";
            print NEW_LIB "      related_pin : \"go\" ; \n";
            print NEW_LIB "      fall_power(energy_template_7x7) { \n";
            print NEW_LIB "        index_1 (\"$index1\"); \n";
            print NEW_LIB "        index_2 (\"$index2\"); \n";
            print NEW_LIB "        values ( \\ \n";
            foreach $slew (@SLEW_RATE) {
                $values = get_internal_power_for_go($slew, $cellArea);
                print NEW_LIB "          $values\n";
            }
            print NEW_LIB "      } \n";
            print NEW_LIB "    } \n";
        } else {
            print NEW_LIB $_;
        }
    }
    close(ORG_LIB);
    close(NEW_LIB);
}

#-------------------------------------------------------------------------------
#	get_internal_power_for_logic -
#	get_internal_power_for_go -
#	power = (base + slope * area) * 0.001
#-------------------------------------------------------------------------------

sub get_internal_power_for_logic {
    my ($slewRate,$cellArea) = @_;
    my @slopeVals = split(/\s+/, $LOGIC_SLOPE{$slewRate});
    my @baseVals  = split(/\s+/, $LOGIC_BASE{$slewRate});
    my $powerVals = "";
    foreach my $i (0..$#slopeVals) {
        $powerVals .= ", " unless ($i == 0);
        my $result  = ($baseVals[$i] + $slopeVals[$i] * $cellArea) * 0.001;
        $powerVals .= round($result);
    }

    return "\"$powerVals\"); " if ($slewRate == $SLEW_RATE[$#SLEW_RATE]);
    return "\"$powerVals\", \\";
}

sub get_internal_power_for_go {
    my ($slewRate,$cellArea) = @_;
    my @slopeVals = split(/\s+/, $GO_SLOPE{$slewRate});
    my @baseVals  = split(/\s+/, $GO_BASE{$slewRate});
    my $powerVals = "";
    foreach my $i (0..$#slopeVals) {
        $powerVals .= ", " unless ($i == 0);
        my $result  = ($baseVals[$i] + $slopeVals[$i] * $cellArea) * 0.001;
        $powerVals .= round($result);
    }

    return "\"$powerVals\"); " if ($slewRate == $SLEW_RATE[$#SLEW_RATE]);
    return "\"$powerVals\", \\";
}

sub round {
    my ($num) = @_;
    my $shiftedNum = $num * 10**$VARS{ROUND_DIGIT};
    my $shiftedInt = int($shiftedNum);
    my $leftover   = $shiftedNum - $shiftedInt;
    $shiftedInt++ if ($leftover >= 0.5);
    my $rounded    = $shiftedInt * 10**(-$VARS{ROUND_DIGIT});
    return $rounded;
}

#-------------------------------------------------------------------------------
#	get_pin_list -
#	get related pin list based on the cell name
#	to make the related_pin all logic input pins.  
#-------------------------------------------------------------------------------

sub get_pin_list {
    my ($cellName) = @_;

    if ($cellName =~ /^BUF_DR_X/) {
        return "A_L_0_R__D_0 A_L_0_R__D_1";
    } elsif ($cellName =~ /^BUF_C_X/) {
        return "A_L_0_R__D_0 A_L_0_R__D_1";
    } elsif ($cellName =~ /^LOGIC(\d+)/) {
        my $nInputs    = $1;
        my $pinList    = "";
        for (my $i = 0; $i < $nInputs; $i++) {
            $pinList  .= "A_L_$i\_R__D_0 A_L_$i\_R__D_1 ";
        }
        return $pinList;
    }
    return "";
}

#-------------------------------------------------------------------------------
#	Program
#-------------------------------------------------------------------------------

usage_exit() if ($VARS{LIB}      eq "" ||
                 $VARS{BASE_DIR} eq "");

generate_internal_power()
