#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;
use Getopt::Long;

my $outfile;
my $force=0;
my $verbose=1;
my $help=0;

sub usage {
    my ($msg) = @_;
    print STDERR "$msg" if defined($msg) and $msg ne "";
    print <<EU;
Usage: fixnames cellname
    --force     : force overwrite of output
    --verbose
EU
exit 1;
}

GetOptions (
    "outfile=s" => \$outfile,
    "force" => \$force,
    "verbose" => \$verbose,
    "help" => \$help,
) or die;

usage() if $help;

$outfile=$ARGV[0]
    if (! defined ($outfile) or $outfile eq "") and $ARGV[0] ne "";

usage if ($outfile eq "");
usage "modules.bind required" if ! -e "modules.bind";
usage "$outfile.gz not found" if ! -e "$outfile.gz";

if ( -f "$outfile") {
    if ($force) {
        unlink $outfile;
    }
    else {
        usage "$outfile exists";
    }
}
my %new=();

open(P, "<modules.bind");
while (<P>) {
    chomp;
    my ($new,$old)=split;
    $new{$old}=$new;
}
close P;
open (P, "gunzip -c '$outfile.gz' |");
open (O, ">$outfile") or die "Cannot write $outfile";
select O;
while (<P>) {
    chomp;
    my @f=split;
    my $mod=0;
    if ($f[0] eq "module" and ! defined($new{$f[1]})) {
        print STDERR "WARNING: No translation for $f[1], update modules.bind";
    }
    foreach my $f (@f) {
        if (defined $new{$f}) {
            $f = $new{$f};
            $mod=1;
        }
    }
    $_ = join(" ", @f) if $mod;
    print;
}
close P;
close O;
