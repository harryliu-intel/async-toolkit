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

$pid1=open2(*INST_IN,*INST_OUT,"$rename --type=instance --from=$from --to=$to");
$pid2=open2(*CELL_IN,*CELL_OUT,"$rename --type=cell --from=$from --to=$to");
$pid3=open2(*NODE_IN,*NODE_OUT,"$rename --type=node --from=$from --to=$to");

sub inst_rename {
    print INST_OUT $_[0]."\n";
    my $name=<INST_IN>;
    chomp($name);
    return $name;
}
sub cell_rename {
    print CELL_OUT $_[0]."\n";
    my $name=<CELL_IN>;
    chomp($name);
    return $name;
}
sub node_rename {
    print NODE_OUT $_[0]."\n";
    my $name=<NODE_IN>;
    chomp($name);
    return $name;
}

open( SOURCE, "<$f_in") or die "Can't open '$f_in' for reading.\n";
open( TARGET, ">$f_out") or die "Can't open '$f_out' for writing.\n";

$. = 0;
$components=0;
$nets=0;
$pins=0;
$thisNet=0;

while($line=<SOURCE>){
    if ($line=~/\\/){$line=~s/\\//g;}
    if ($line=~/^DESIGN (\S*)/){
        $oldcellname=$1;
        $newcellname=cell_rename($oldcellname);
        $line=~s/^DESIGN \Q$oldcellname\E/DESIGN \Q$newcellname\E/;

    } elsif ( $line=~/^COMPONENTS /){
        $components=1;
    } elsif ( $components && $line=~/^- (\S*) (\S*) /){
        $oldinstname=$1;
        $oldcellname=$2;
        $newinstname=inst_rename($oldinstname);
        $newcellname=cell_rename($oldcellname);
        $line=~s/^- \Q$oldinstname\E \Q$oldcellname\E/- \Q$newinstname\E \Q$newcellname\E/;

    } elsif ( $line=~/^END COMPONENTS$/){
        $components=0;

    } elsif ( $line=~/^PINS /){
        $pins=1;
    } elsif ( $pins && $line=~/- (\S*) \+ NET (\S*) /){
        $oldpinname=$1;
        $oldnetname=$2;
        $newpinname=node_rename($oldpinname);
        $newnetname=node_rename($oldnetname);
        $line=~s/^- \Q$oldpinname\E \+ NET \Q$oldnetname\E/- \Q$newpinname\E \+ NET \Q$newnetname\E/;
    } elsif ( $line=~/^END PINS$/){
        $pins=0;

    } elsif ( $line=~/^NETS /){
        $nets=1;
    } elsif ( $line=~/^SPECIALNETS /){
        $nets=1;
    } elsif ( $nets && $line=~/^- (\S*)/){
        $oldnetname=$1;
        $newnetname=node_rename($oldnetname);
        $line=~s/^- \Q$oldnetname\E/- \Q$newnetname\E/;
        $thisNet=1;
    } elsif ( $nets && $thisNet && $line=~/^(\s*)\( (\S*) (\S*) \)/){
        $newline=$1;
        chomp($line);
        @word=split(' ',$line);
        for( $i=0; $i<$#word; $i++){
            if($word[$i]eq"("){
                $i++; $oldinstname=$word[$i];
                $i++; $oldpinname=$word[$i];
                $i++; # get rid of ")"
                $newinstname=inst_rename($oldinstname);
                $newpinname=node_rename($oldpinname);
                $newline.="( $newinstname $newpinname )";
            }
            if($i<$#word-1){$newline.=" ";}
        }
        $line=$newline."\n";
    } elsif ( $nets && $thisNet && $line=~/^\s*\+/){
        $thisNet=0;
    } elsif ( $line=~/^END NETS/){
        $nets=0;
    } elsif ( $line=~/^END SPECIALNETS/){
        $nets=0;
    } 
    print TARGET $line;
}

close(SOURCE);
close(TARGET);

close(CELL_IN);
close(CELL_OUT);
close(INST_IN);
close(INST_OUT);
close(NODE_IN);
close(NODE_OUT);
waitpid( $pid1, 0);
waitpid( $pid2, 0);
waitpid( $pid3, 0);

