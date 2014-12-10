#!/usr/intel/bin/perl -w
# Copyright 2004 Fulcrum Microsystems.  All rights reserved.
# Authors: Kai He

use FileHandle;
use IPC::Open2;

$rename = "rename";

sub usage() {
    die "Usage: $0 --from=[cast,cadence,gds2] --to=[cast,cadence,gds2] --infile=[infilename] --outfile=[outfilename]\n";
}

while (defined $ARGV[0] && $ARGV[0] =~ /^--(.*)/) {
    ($flag,$value) = split('=',$1);
    if ($flag eq "to") {
        $to = $value;
        shift @ARGV;
    } elsif ($flag eq "from") {
        $from = $value;
        shift @ARGV;
    } elsif ($flag eq "infile") {
        $f_in = $value;
        shift @ARGV;
    } elsif ($flag eq "outfile") {
        $f_out = $value;
        shift @ARGV;
    } else { 
    shift @ARGV;
    }
}

defined $to or usage();
defined $from or usage();
defined $f_out or usage();
defined $f_in or usage();

$pid1=open2(*NODE_IN,*NODE_OUT,"$rename --type=node --from=$from --to=$to");
$pid2=open2(*CELL_IN,*CELL_OUT,"$rename --type=cell --from=$from --to=$to");

sub node_rename {
    print NODE_OUT $_[0]."\n";
    my $name=<NODE_IN>;
    chomp($name);
    return $name;
}
sub cell_rename {
    print CELL_OUT $_[0]."\n";
    my $name=<CELL_IN>;
    chomp($name);
    return $name;
}

open( SOURCE, "<$f_in") or die "Can't open '$f_in' for reading.\n";
open( TARGET, ">$f_out") or die "Can't open '$f_out' for writing.\n";

$. = 0;
$pin=0;
$macro=0;

while($line=<SOURCE>){
    if ($line=~/\\/){$line=~s/\\//g;}
    if ($line=~/^MACRO (\S+)\s*$/){
        $oldname=$1;
        $newname=cell_rename($oldname);
        $line=~s/^MACRO \Q$oldname\E/MACRO \Q$newname\E/;
        $macro=1;
    } elsif ( $line=~/^\s*FOREIGN (\S+)/){
        $oldname=$1;
        $newname=cell_rename($oldname);
        $line=~s/^(\s*FOREIGN) \Q$oldname\E/$1 \Q$newname\E/;
    } elsif ( $line=~/^\s*PIN (\S+)\s*$/){
        $oldname=$1;
        $newname=node_rename($oldname);
        $line=~s/^(\s*PIN) \Q$oldname\E/$1 \Q$newname\E/;
        $pin=1;
    } elsif ( $pin && $line=~/^\s*END (\S+)\s*$/){
        $oldname=$1;
        $newname=node_rename($oldname);
        $line=~s/^(\s*END) \Q$oldname\E/$1 \Q$newname\E/;
        $pin=0;
    } elsif ( $macro && $line=~/^\s*END (\S+)\s*$/){
        $oldname=$1;
        $newname=cell_rename($oldname);
        $line=~s/^(\s*END) \Q$oldname\E/$1 \Q$newname\E/;
        $macro=0;
    } 
    print TARGET $line;
}

close(SOURCE);
close(TARGET);

close(CELL_IN);
close(CELL_OUT);
close(NODE_IN);
close(NODE_OUT);
waitpid( $pid1, 0);
waitpid( $pid2, 0);

