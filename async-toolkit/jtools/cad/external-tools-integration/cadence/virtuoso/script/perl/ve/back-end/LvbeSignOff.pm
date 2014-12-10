# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id: //depot/sw/cad/external-tools-integration/cadence/virtuoso/main/script/perl/ve/back-end/LvbeSignOff.pm#2 $
# $DateTime: 2002/04/16 16:04:07 $
# $Author: chrisb $

# XXX: this back-end is not used for stage one.

# Verification back-end LvbeSignOff
#
# This back-end filters a DRC .err file through a list of signoff files.
# An error is filtered if there is a signoff for the rule,cell such that the
# bounding box of the signoff is contained in the bounding box of the .err violation.
# The following nvpair is used:
#
#	Name			Description
#
#       SignOff                 SignOff file
#       ErrFile                 .err file
#       OutFile                 .realerr file
#
# Insert the following lines in a Perl file to invoke this back-end:
#
# use LvbeSignOff;
# LvbeSignOff::verify();

use Lvfe;
package LvbeSignOff;

#
# Perform SignOff verification
#

sub verify {
    &Lvfe::assert_defined("ErrFile");
    &Lvfe::assert_defined("OutFile");
    
    my $errFile = Lvfe::get_nvpair("ErrFile");
    my $outFile = Lvfe::get_nvpair("OutFile");
    my @signoffFiles = Lvfe::get_nvpairs("SignOff");
    my $signed = parseSignOffs(@signoffFiles);
    &filterErrFile($signed,$errFile,$outFile);
}


sub filterErrFile {
    my ($signed,$errFile,$outFile) = @_;
    my @signedSets = ();
    my $rule;
    my $cell;
    my $shapes = 0;
    my $errors = 0;
    my $pass = 1;
    

    open OUT, ">$outFile" or die "Can't open $outFile for write";
    open ERR, "<$errFile" or die "Can't open $errFile for read";

    while ( <ERR> ) {
        my $line = $_;
        if( /^Rule[^:]*:\s*(.*)/ ) {
            $rule = $1;
        } elsif( /^Cell Name : (.*)/ ) {
            ($cell) = split(" ",$1);
            $shapes = 0;
            @signedSets = ();
            while( my ($cellRegEx,$signedSet) = each(%{$signed->{$rule}})) {
                if($cell =~ /$cellRegEx/ ) {
                    push @signedSets,$signedSet;
                }
            }
        } elsif( /^Shape/ ) {
            $shapes = 1;
        } elsif( /^\s+[0-9]+\s+.\s+([0-9-\.]+)\s+([0-9-\.]+)\s+([0-9-\.]+)\s+([0-9-\.]+)/) {
            if( $shapes==1 ) {
                $errors = 1;
                my @pt1 = ($1,$2);
                my @pt2 = ($3,$4);
                my @pts = (\@pt1,\@pt2);
                my $box = pointsToBBox(\@pts);
                my $printLine = 1;
                foreach my $signedSet (@signedSets) {
                    if(&isABoxInBox($signedSet,$box)) {
                        $printLine = 0;
                        last;
                    }
                }
                if ($printLine) {
                    $pass = 0;
                    print OUT "$rule: $cell: @{$box}\n";
                }
            }
        }
    }
    if($pass) {
        if($errors) {
            print OUT "SIGNOFF\n";
        }
        else {
            print OUT "PASS\n";
        }
    }
    else {
        print OUT "FAIL\n";
    }
}

sub parseSignOffs {
    my @signoffFiles = @_;
    my %signed = ();
    my @cells = ();

    foreach my $signfile (@signoffFiles) {
        open SIGN, "<$signfile" or die "Can't open $signfile for read";
        my $rule;
        while( <SIGN> ) {
            if( /^\s*\(\s*cell\s*\"(.*)\"\s*\)/ ) {
                my ($thisCell) = split(" ",$1);
                push @cells, $thisCell;
            }  elsif( /^\s*\(\s*rule\s*\"(.*)\"\s*\)/ ) {
                $rule = $1;
            } elsif( /^\s*\(\s*contour\s*(.*)\s*\)/ ) {
                my @points = map { &parsePoint($_) } split(" ",$1);
                my $box = &pointsToBBox(\@points);
                foreach $cell (@cells) {
                    push @{$signed{$rule}{$cell}}, $box;
                }
                @cells = ();
            }
        }
    }
    
    foreach my $rule (keys %signed) {
        foreach my $cell (keys %{$signed{$rule}}) {
            @{$signed{$rule}{$cell}} = sort { &boxCmp($a,$b) } @{$signed{$rule}{$cell}};
        }
    }
    return \%signed;
}

sub isABoxInBox {
    my ($boxes,$boundBox) = @_;
    my $low = &binary_search($boxes,$boundBox,\&boxCmp);
    for(my $k=$low; $k < @$boxes; $k++ ) {
        $box = $boxes->[$k];
        return 1 if( &boxInBox($box,$boundBox) );
        return 0 if( $box->[0] > $boundBox->[0]);
    }
    return 0;
}

sub binary_search {
     my ($array, $word, $cmp) = @_;
     my ($low, $high) = ( 0, @$array - 1 );
     my $try;
     while ( $low <= $high ) {              # While the window is open
         $try = int( ($low+$high)/2 );      # Try the middle element
         $low  = $try+1, next if &$cmp($array->[$try],$word) < 0; # Raise bottom
         $high = $try-1, next if &$cmp($array->[$try],$word) > 0; # Lower top
         return $try;
     }
     return $try;
}

sub boxCmp { 
    ($a,$b) = @_;
    for(my $k=0; $k<@$a; $k++) {
        $cmp = ($a->[$k] <=> $b->[$k]);
        return $cmp if ($cmp!=0);
    }
    return 0;
}

sub boxInBox {
    my($box1,$box2) = @_;
    my $ret = 0;
    if( $box1->[0] >= $box2->[0] &&
        $box1->[1] >= $box2->[1] &&
        $box1->[2] <= $box2->[2] &&
        $box1->[3] <= $box2->[3] ) {
        $ret = 1;
    }
    return $ret
}


sub pointsToBBox {
    my ($points) = @_;
    my ($left,$right) = &minmax( map { $_->[0]; } @{$points} );
    my ($bottom,$top) = &minmax( map { $_->[1]; } @{$points} );
    my @ret = ($left,$bottom,$right,$top);
    return \@ret;
}

sub parsePoint {
    my ($str) = @_;
    my @pt = split(":",$str);
    return \@pt;
}

sub minmax {
    my $min = pop(@_);
    my $max = $min;
    foreach $val (@_) {
        if($val < $min) { $min = $val;}
        if($val > $max) { $max = $val;}
    }
    return ($min,$max);
}

#
# Perl 'require' requires that a module return a true value.
#

1;
