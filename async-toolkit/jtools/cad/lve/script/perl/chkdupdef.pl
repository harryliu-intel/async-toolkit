#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;
my %nets=();
my %inst=();

print "Usage: chkdupdef <deffile>" if ! @ARGV or ! -f $ARGV[0];
if ($ARGV[0] =~ /\.gz$/) {
    open (P, "gunzip -c '$ARGV[0]' |");
}
else {
    open (P, "<$ARGV[0]");
}
my $isnet=0;
my $iscom=0;
my $seccnt=0;
while (<P>) {
    chomp;
    $isnet=1,$seccnt++ if /^NETS/;
    $iscom=1,$seccnt++ if /^COMPONENTS/;
    $isnet = $iscom = 0 if /^END/;
    last if $seccnt >= 2 and $isnet==0 and $iscom==0;
    if (/^-/) {
        my @f=split;
        my $name=$f[1];
        $name =~ s/\\//g;
        $nets{$name}++ if $isnet;
        $inst{$name}++ if $iscom;
    }
}
my $err=0;
foreach my $name (sort keys %nets) {
    if ($nets{$name} > 1) {
        print "NET $name";
        $err++;
    }
}
foreach my $name (sort keys %inst) {
    if ($inst{$name} > 1) {
        print "INST $name";
        $err++;
    }
}
$err;
