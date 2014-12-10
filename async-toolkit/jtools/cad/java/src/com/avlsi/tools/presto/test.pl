#!/usr/intel/bin/perl -w

use strict;
use vars qw($Dir $File @Cells $Logfile $GoodTidy);

use FindBin;

$Dir = $FindBin::Bin;

$File = "$Dir/test.cast";

# I laugh in the face of memo 28
$GoodTidy = "/home/user/ppelleti/local/bin/tidy";

@Cells = ();

open F, $File or die;
while (<F>) {
    push @Cells, $1 if (/^define\s+(\w+)/);
}
close F;

my $n = $#Cells + 1;
print STDERR "Doing $n cells\n";

$Logfile = "$Dir/log.html";
open F, ">$Logfile" or die;
print F "<html><head><title>PReSto test results</title></head><body>\n";
close F;

my $first = 1;
my $cell;
foreach $cell (@Cells) {
    if (not $first) {
        open F, ">>$Logfile" or die;
        print F "<hr>\n";
        close F;
    }
    system("$Dir/test-one-cell.pl --logfile=$Logfile $cell");
    $first = 0;
}

open F, ">>$Logfile" or die;
print F "</body></html>\n";
close F;

system("$GoodTidy -f /dev/null -mcq $Logfile");
