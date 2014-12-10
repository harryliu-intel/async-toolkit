#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;
my $width=2.2;

my @lines=();
my $mode=0;
my %pincnt=();
my $indent="";
my $pin;
my $macro;
my %macro=();
my @macro=();
my %ispwr=();
while (<>) {
    chomp;
    if (/^\s*MACRO\s+(\S+)/) {
        $macro=$1;
        push @{$macro{$macro}},$_;
    }
    elsif (/^END/ and defined $macro) {
        push @{$macro{$macro}},$_;
        undef $macro;
        undef @macro;
        @macro=();
    }
    elsif (defined ($macro)) {
        push @{$macro{$macro}},$_;
    }
    if (/^\s*PIN\s+(\S+)/) {
        $pin=$1;
        $pincnt{$macro}++;
    }
    if (/^\s*USE\s+(ground|power)/i) {
        $ispwr{"$macro $pin"}=1;
        $pincnt{$macro}--;
    }
    push @lines, $_;
}
$mode = 0;
foreach my $line (@lines) {
    $_=$line;
    if ($mode == 0) {
        print;
        if (/^\s*MACRO\s+(\S+)/) {
            $macro=$1;
            $mode=1;
            domacro($macro);
        }
        next;
    }
    elsif (/^END/) {
        $mode=0;
    }
}

sub domacro {
    my ($macro)=@_;
    my @lns=@{$macro{$macro}};
    my $pincnt = $pincnt{$macro};
    my $validpin=1;
    my $delta = $width/($pincnt-1);
    my $start = $delta;
    foreach my $line (@lns) {
        $_=$line;
        if (/^END/) {
            print;
            last;
        }
        if (/^\s*CLASS/) {
            print;
            next;
        }
        if (/^\s*SYMMETRY/) {
            print;
            next;
        }
        if (/^(\s*)SIZE/ ) {
            print "${1}SIZE $width BY 2.2 ;";
            next;
        }
        if (/^(\s*)ORIGIN/) {
            print "${1}ORIGIN 0.0 0.0 ;";
            next;
        }
        if (/^(\s*)FOREIGN (\S+)/) {
            print "${1}FOREIGN $2 0.0 0.0 ;";
            next;
        }
        if (/^\s*PIN\s+(\S+)/) {
            $pin=$1;
            $validpin=0;
            $validpin=1 if ! $ispwr{"$macro $pin"};
            print if $validpin;
        }
        elsif (/^(\s*)DIRECTION/) {
            $indent=$1;
            print if $validpin;
        }
        elsif (/^\s*PORT/) {
            print $_ if $validpin;
        }
        elsif (/^(\s*)END \Q$pin\Q/) {
            my $indent=$1;
            if ($validpin) {
                print "${indent}    LAYER M2 ;";
                printf "${indent}       RECT %.2f %.2f %.2f %.2f ;\n",
                    $start, 2.0-$delta/2, $start+$delta/2, 2.0+$delta;
                print "${indent}    END";
                $start += $delta;
                print;
            }
        }
    }
}
