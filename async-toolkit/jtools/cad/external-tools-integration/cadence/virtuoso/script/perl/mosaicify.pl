#!/usr/intel/bin/perl

### This scripts takes in a file which is dumped with ( dbWriteSkill )
my $input = $ARGV[0];
my $output = $ARGV[1];
### and outputs dbMosiac calls that replace the instances, which are assumed
### to be aon a grid
my %grid = ();
###

open SKILL, "<$input" or die;
open OUT, ">$output" or die;

@gridSmall = (4.8,9.6);
@gridBig   = (9.6,9.6);

$grid{"globals.fill.POLY"} = \@gridSmall;
$grid{"globals.fill.M1"} = \@gridSmall;
$grid{"globals.fill.M2"} = \@gridSmall;
$grid{"globals.fill.M3"} = \@gridSmall;
$grid{"globals.fill.M4"} = \@gridSmall;
$grid{"globals.fill.M5"} = \@gridSmall;
$grid{"globals.fill.M6"} = \@gridBig;
$grid{"globals.fill.M7"} = \@gridBig;

$grid{"globals.fill.VIA12"} = \@gridSmall;
$grid{"globals.fill.VIA23"} = \@gridSmall;
$grid{"globals.fill.VIA34"} = \@gridSmall;
$grid{"globals.fill.VIA45"} = \@gridSmall;
$grid{"globals.fill.VIA56"} = \@gridBig;
$grid{"globals.fill.VIA67"} = \@gridBig;

@m1 = ("globals.fill.VIA12");
@m2 = ("globals.fill.VIA12","globals.fill.VIA23");
@m3 = ("globals.fill.VIA23","globals.fill.VIA34");
@m4 = ("globals.fill.VIA34","globals.fill.VIA45");
@m5 = ("globals.fill.VIA45");
@m6 = ("globals.fill.VIA67");
@m7 = ("globals.fill.VIA67");

$via{"globals.fill.M1"} = \@m1;
$via{"globals.fill.M2"} = \@m2;
$via{"globals.fill.M3"} = \@m3;
$via{"globals.fill.M4"} = \@m4;
$via{"globals.fill.M5"} = \@m5;
$via{"globals.fill.M6"} = \@m6;
$via{"globals.fill.M7"} = \@m7;

sub rownd{
    my $float = shift;
    return int($float + 0.5);
}

print OUT "CellView = ( geGetWindowCellView )\n";

@cells = ();

while( <SKILL> ) {
    my $line = $_;

    if( $line =~ /\"R0\"/ && defined($grid{$cell}) ) {
        my ($coords) = split(" ",$_);
        ($x,$y) = split(":",$coords);
        $I = rownd($x / $gridX);
        $J = rownd($y / $gridY);
        $M{$cell} = $I if($I > $M{$cell});
        $N{$cell} = $J if($J > $N{$cell});
        $T = $TT{$cell};

        $T->[$I][$J] = 1;
    }
    elsif( $line =~ /dbOpenCellViewByType\(\"(.*)\" \"(.*)\" \"(.*)\"\)/ ) {
        ($lib,$cell,$view) = ($1,$2,$3);
        ($gridX,$gridY) = @{$grid{$cell}};

        $M{$cell} = 0;
        $N{$cell} = 0;
        $lib{$cell} = $lib;
        $view{$cell} = $view;
        my @T = ();
        $TT{$cell} = \@T;
        
        if($gridX) {
            push @cells,$cell;
        }

        print "$cell, grid = $gridX $gridY\n";
    }
}

foreach $cell (@cells) {

    my $T = $TT{$cell};
    my ($gridX,$gridY) = @{$grid{$cell}};
    my $M = $M{$cell};
    my $N = $N{$cell};
    my $lib = $lib{$cell};
    my $view = $view{$cell};

    my @vias = @{$via{$cell}};


    foreach $viaCell (@vias) {
        print "Removing redundant cells where there is already $viaCell\n";
        my ($viaT) = @{$TT{$viaCell}};
        for($I=0;$I<=$M;$I++) {
            for($J=0;$J<=$N;$J++) {
                if( $viaT->[$I][$J] == 1 ||
                    $viaT->[$I][$J] == 2 ) {
                    $T->[$I][$J] = 0;
                }
            }
        }
    }

    print "Doing $M x $N raster of $lib $cell $view with $gridX x $gridY grid...\n";
    $I = 0;
    $J = 0;

    print OUT "Master = ( dbOpenCellViewByType \"$lib\" \"$cell\" \"$view\" nil \"r\" )\n";

    while($I<=$M) {
        print "$I\n" if(($I%500) == 0);

        while($J<=$N) {
            if( ($T->[$I][$J] == 1)) {
                $IE = $I;
                $JS = $J;
                $JE = $J;
                
                while( $T->[$I][$JE] == 1 && $JE <= $N ) {
                    $JE++;
                }
                $JE--;
                
                while($IE <= $M) {
                    my $columnGood = 1;
                    #print "IS = $IS IE = $IE JS = $JS JE = $JE\n";
                    for($JM=$JS;$JM<=$JE;$JM++) {
                        if($T->[$IE][$JM] != 1 ) {
                            #print "$IE $JM\n";
                            $columnGood = 0;
                            last;
                        }
                    }
                    if($columnGood == 1) {
                        for($JM=$JS;$JM<=$JE;$JM++) {
                            $T->[$IE][$JM] = 2;
                        }
                        $IE++;
                    } else {
                        $IE--;
                        my $columns = $IE - $I + 1;
                        my $rows = $JE - $JS + 1;
                        my $x = $I * $gridX;
                        my $y = $JS * $gridY;
                        #print "Got one!\n";
                        print OUT "( dbCreateSimpleMosaic CellView Master nil $x\:$y \"R0\" $rows $columns $gridY $gridX )\n";
                        
                        last;
                    }
                }
                $J = $JE;
            }
            $J++;
        }
        if($J >= $N) {
            $J = 0;
            $I++;
        }
    }   
}




close(OUT);
close(SKILL);

