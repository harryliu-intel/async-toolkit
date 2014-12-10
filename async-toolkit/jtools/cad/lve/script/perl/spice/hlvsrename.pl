#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;
use Getopt::Long;
use IPC::Open2;

my $pidg=open2 (\*RDG, \*WRG, "rename --type=cell --to=cast --from=gds2");

sub ren {
    my ($name)=@_;
    return $name if $name =~ /^\s*$/;
    $name =~ s/^(\s*)//;
    my $ws=$1;
    print WRG "$name";
    my $out=<RDG>;
    chomp $out;
    $out=$name if $out eq "";
    $ws.$out;
}

while (<>) {
    chomp;
    if (/_D_/ and ! /^\s*run_details\//) {
        $_=ren($_);
    }
    print;
}
close WRG;
close RDG;
waitpid $pidg, 0;
