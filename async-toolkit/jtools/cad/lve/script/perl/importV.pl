#!/usr/intel/bin/perl -l
use strict;

use Getopt::Long;

my $cell;
my $verbose=1;

GetOptions (
    "cell=s" => \$cell,
    "verbose" => \$verbose,
) or die;

sub usage {
    my ($msg) = @_;
    print STDERR "$msg" if defined($msg) and $msg ne "";
    print <<EU;
Usage: importV cellname
EU
exit 1;
}

$cell=$ARGV[0] if (! defined ($cell) or $cell eq "") and $ARGV[0] ne "";
usage if ($cell eq "");

$|=1;
if ( -s "$cell.v" ) {
    printf "delete $cell.v? ";
    my $ans=<STDIN>;
    chomp $ans;
    if ($ans =~ /^y/i) {
        unlink "$cell.v";
    }
    else {
        exit 1;
    }
}
if ( -s "$cell.cast" ) {
    printf "delete $cell.cast? ";
    my $ans=<STDIN>;
    chomp $ans;
    if ($ans =~ /^y/i) {
        unlink "$cell.cast";
    }
    else {
        exit 1;
    }
}
my %modules;
open (P, "<modules.bind");
while (<P>) {
    chomp;
    my ($a,$b)=split;
    $modules{$b}=1;
}
close P;
open (P, "gunzip -c '$cell.v.gz' |");
my $err=0;
while (<P>) {
    chomp;
    if (/^module /) {
        my @f=split;
        if (! defined($modules{$f[1]})) {
            print STDERR "Need to update modules.bind";
            $err++;
        }
    }
}
close P;
if ($err) {
    print STDERR "Exit due to errors";
    exit 1;
}
print STDERR "Fixing names..." if $verbose;
system "fixnames $cell.v";
rename "$cell.v", "$cell.fix.v";
# cell.v now created
print STDERR "Rehierarchy 1 ..." if $verbose;
system "rehierverilog '$cell.fix.v' > '$cell.reh1.v'";
print STDERR "Rehierarchy 2 ..." if $verbose;
system "rehierverilog '$cell.reh1.v' > '$cell.v'";
unlink "$cell.fix.v";
unlink "$cell.reh1.v";
#print STDERR "vs2cast..." if $verbose;
#system "vs2cast '$cell.v' '$cell'";
exit 0;
print STDERR "distribute cast..." if $verbose;
system "splitcast --p4 '$cell.cast'";
print STDERR "Generate cdl and check cast..." if $verbose;
system "jflat --tool=cdl --cell='$cell' 2>bfhcdl.err 1>bfhcdl.cdl";
if ( -s "bfhcdl.err") {
    print STDERR "Error: jflat failed";
    system "cat 'bfhcdl.err' 1>\&2";
    exit 1;
}
exit 0;
