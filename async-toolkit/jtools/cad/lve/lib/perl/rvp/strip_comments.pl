#!/usr/bin/perl -w -I..

use rvp 7.54;


foreach my $f (@ARGV) {
    my $chunkRead = rvp->chunkReadInit($f,0);
    while ($chunk = rvp->chunkRead($chunkRead)) {
	print $chunk->{text} unless $chunk->{type} eq "comment";
    }
}


