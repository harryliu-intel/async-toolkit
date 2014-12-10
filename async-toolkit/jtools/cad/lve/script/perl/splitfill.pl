#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;
use Getopt::Long;

my $has=0;
my $bgnstr="";
my $sname;
my $incell=0;
my $dropproperties=0;
my %out=();
my $cell;
my %cells=();
my $verbose=0;

sub usage {
    my ($msg) = @_;
    print STDERR "$msg" if defined($msg) and $msg ne "";
    print <<EU;
Usage: splitfill --cell=cellname <gdsfile>
EU
exit 1;
}

GetOptions (
    "cell=s" => \$cell,
    "verbose" => \$verbose,
) or usage;

my $ingds=$ARGV[0];
usage if ( ! -s "$ingds");
usage if $cell eq "";
# bind file to get the 'real' names, not to change the gds
open (P, "<$cell.bind") or usage "Cannot open $cell.bind";
while (<P>) {
    chomp;
    my ($out,$v,$gds)=split;
    $out{$gds}=$out;
}
close P;
open (P, "rdgds '$ingds' |");
open (Q, "| wrgds > $cell.gds");
open (M, ">/scratch/M.rdg");
open (R, ">/scratch/R.rdg");
select Q;
while (<P>) {
    chomp;
    next if ((/^PROP/) and $dropproperties);
    if (/BGNSTR/) {
        $bgnstr=$_;
    }
    if (/STRNAME $cell$/) {
        $incell=1;
    }
    if (/STRNAME/) {
        print STDERR "$_ $. $incell" if $verbose;
        my ($sn,$cl)=split;
        $cells{$cl} += 0;
    }
    if (/^[AS]REF/ and $incell) {
        my @temp=();
        push @temp, $_;
        my $ism=0;
        while (<P>) {
            chomp;
            push @temp, $_;
            if (/^ENDEL/) {
                if ($ism) {
                    print M join("\n", @temp);
                }
                else {
                    print R join("\n", @temp);
                }
                undef @temp;
                $ism=0;
                last;
            }
            if (/SNAME/) {
                $sname=$_;
                $sname =~ s/SNAME //;
                $cells{$sname}++;
                if (($out{$sname} =~ /^bfh/) or ($sname =~ /^FILL/)) {
                    $ism=1;
                }
            }
        }
        next;
    }
    if (/ENDSTR/and $incell) {
        sleep 1;
        $incell=0;
        my $has=1;
        close M;
        close R;
        if ($has) {
            $cells{"${cell}_misc"}=1;
            $cells{"${cell}_real"}=1;
            print "SREF";
            print "SNAME ${cell}_misc";
            print "STRANS 0";
            print "ANGLE 0.0";
            print "XY 1";
            print "0,0";
            print "ENDEL";
            print "SREF";
            print "SNAME ${cell}_real";
            print "STRANS 0";
            print "ANGLE 0.0";
            print "XY 1";
            print "0,0";
            print "ENDEL";
        }
        print "ENDSTR";
        if ($has) {
            print STDERR "Writing ${cell}_misc..." if $verbose;
            print "$bgnstr";
            print "STRNAME ${cell}_misc";
            local *S;
            local $_;
            open (S, "</scratch/M.rdg");
            while (<S>) {
                chomp;
                print;
            }
            close S;
            print "ENDSTR";
            print STDERR "Done" if $verbose;
            print STDERR "Writing ${cell}_real..." if $verbose;
            print "$bgnstr";
            print "STRNAME ${cell}_real";
            local *S;
            local $_;
            open (S, "</scratch/R.rdg");
            while (<S>) {
                chomp;
                print;
            }
            close S;
            print "ENDSTR";
            print STDERR "Done" if $verbose;
        }
        next;
    }
    print;
}
open (X, ">${cell}.cells.split") or
    usage "Cannot open ${cell}.cells.split file";
my $err=0;
foreach my $c (sort keys %cells) {
    print X "$c";
    if ( ! defined $out{$c}) {
        print STDERR "Error: Need bind entry for $c";
        $err++;
    }
}
close X;
unlink "/scratch/M.rdg";
unlink "/scratch/R.rdg";
$err;
