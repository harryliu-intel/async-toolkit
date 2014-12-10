#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

# purpose: to modify gds to Fulcrum/Intel standards

use strict;
my %valid=();
my $validcnt=0;
open (P, "<validpins");
while (<P>) {
    chomp;
    $valid{$_}=1;
    $validcnt++;
}
close P;
my $gdsfile=shift;
open (P, "rdgds '$gdsfile' |");
my $outfile=shift;
$outfile = "$gdsfile.mod" if ! defined $outfile;
open (Q, "| wrgds > '$outfile'");
select Q;
my @text=();
my $validstring=1;
my $validlayer=1;
my %definedpins=();
my $string;
while (<P>) {
    chomp;
    s/'VDD'/'Vdd'/g;
    s/'VSS'/'GND'/g;
    if (/^BGNSTR/ or /^STRNAME/) {
        foreach my $key (keys %definedpins) {
            $definedpins{$key}=0;
        }
    }
    if (/^TEXT$/) {
        @text=($_);
        $validstring=$validlayer=0;
        next;
    }
    if (@text) {
        if (/^MAG /) {
            $_="MAG 0.1";
        }
        push @text, $_;
        if (/^STRING/) {
            my @f=split(/ /,$_,2);
            $string=$f[1];
            $string =~ s/'//g;
            $validstring=1 if $valid{$string} or !$validcnt;
            $validstring=0 if $definedpins{$string} == 1; # no duplicates
            $validstring=1;
        }
        if (/^LAYER (\d+)$/) {
            my $l = $1;
            $validlayer = 1
                if (($l >= 31 and $l <= 40) or ($l >= 131 and $l <= 140) or $l == 63);
        }
        if (/ENDEL/) {
            if (($validlayer and $validstring) or ($string eq "GND")) {
                print join("\n", @text);
                $definedpins{$string}=1 if ($string ne "Vdd");
            }
            @text=();
        }
        next;
    }
    if (/^ENDEL/) {
        @text=();
    }
    print;
}
