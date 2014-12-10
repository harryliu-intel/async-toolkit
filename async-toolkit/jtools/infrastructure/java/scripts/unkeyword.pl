#!/usr/local/bin/perl -w
#
#      unkeyword.pl - remove the Perforce Id keyword from specified files
#
#      Copyright 2002 Fulcrum Microsystems, Inc.  All rights reserved.
#
#      $Id: //depot/sw/sdk/core/maelstrom/main/build/java/src/scripts/unkeyword.pl#1 $
#

# Expects a list of files (output of find) on stdin

while (<>) {
    chomp;
    my $file = $_;
    my $size = -s $file;
    $Files{$file} = $size if ($size);
}

@Files = sort { $Files{$a} <=> $Files{$b} } keys %Files;

open WORDS, "/usr/share/dict/words" or die;
$Word = "";

foreach $file (@Files) {
    open F, $file or die;
    my $s = join("", <F>);
    close F;
    my $s2 = $s;
    $s2 =~ s/\044Id:[ -\#\%-~]+\044/fillwithcrud($&)/seg;
    if ($s ne $s2) {
        open F, ">$file" or die;
        print F $s2;
        close F;
    }
}

sub fillwithcrud {
    my $s = $_[0];
    my $l = length($s);
    
    my $result = "";
    while (length($result) != $l) {
        if (length($result) + length($Word) < $l) {
            if ($result eq "") {
                $result = $Word;
            } else {
                $result .= " " . $Word;
            }
            getnewword();
        } else {
            my $n = $l - length($result);
            $result .= ' ' x $n;
        }
    }

    return $result;
}

sub getnewword {
    do {
        $Word = <WORDS>;
        chomp $Word;
    } while ($Word !~ /^[a-z]+$/);
}
