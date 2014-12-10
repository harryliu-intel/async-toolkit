#!/usr/intel/bin/perl
# Copyright 2005 Fulcrum Microsystems.  All rights reserved.
# Authors: Kai He

use strict;
use Getopt::Long;

sub usage() {
    my $usage  = "USAGE: $0 \n";
    $usage .= "    --swappin-file=<filename> \n";
    $usage .= "    --infile=<filename> \n";
    $usage .= "    [--topcell=<cellname> default()]\n";
    $usage .= "    [--outfile=<filename>] \n";
    die "$usage";
}

my $swappin_file;
my $f_in;
my $f_out;
my $topcell;

GetOptions(
    "swappin-file=s" => \$swappin_file,
    "infile=s" => \$f_in,
    "outfile=s" => \$f_out,
    "topcell=s" => \$topcell,
) || usage;

defined $f_in or usage();
defined $swappin_file or usage();
defined $topcell or $topcell="";
defined $f_out or $f_out="";

if( ! -e "$swappin_file" ){ 
    print "Error: $swappin_file does not exit. \n";
    usage();
}
if( ! -e "$f_in" ){ 
    print "Error: $f_in does not exit. \n";
    usage();
}
if( -z "$swappin_file" ){ 
    print "No Pin Swap found in $swappin_file. \n";
    exit(0);
}
my %nameMap;
open( PINSWAP, "<$swappin_file") || die "Error: Can not open $swappin_file for read.\n";
while(<PINSWAP>){
  chomp($_); 
  my @fields=split(' ', $_);
  $nameMap{$fields[0]}=$fields[1];
}
close( PINSWAP );

open( TARGET, ">${f_in}_tmp");
print TARGET "* Pinswap Fixed Deck\n";
replace_nodes("$f_in");
close( TARGET );

if( $f_out eq ""){
    system("mv -f '${f_in}' '${f_in}_org' && mv -f '${f_in}_tmp' '${f_in}' && mv '$swappin_file' '${swappin_file}_old'");
} else {
    system("mv -f '${f_in}_tmp' '${f_out}'");
}

############################### SUBROUTINES GO HERE ######################
sub convertInstName {
    my ($instName)=@_;
    chomp($instName);
    # there is no chagne in instance names
    return $instName;
}

sub convertNetName {
    my ($netName)=@_;
    chomp($netName);
    my $lastPart;
    if( $netName =~ /:/ ){ ($netName, $lastPart)=split(':', $netName); }
    # look up the net names in nameMap table.
    if( defined $nameMap{$netName} ){ $netName=$nameMap{$netName};}
    if( defined $lastPart ){ $netName="${netName}:$lastPart"; }
    return $netName;
}

sub replace_nodes {
    my ($f_in)=@_;
    local(*SOURCE, $_);

    open( SOURCE, "<$f_in") or die "Can't open '$f_in' for reading.\n";

    my $line = <SOURCE>;
    my $next_line;
    my $main_subckt=($topcell eq "")?1:0;
    $"=" ";
    while ($line) {
      $next_line = <SOURCE>;     
      if ($main_subckt==0 && $line =~ /^\.SUBCKT/i) {
          # concatenate any continued lines
          while (defined $next_line and $next_line =~ s/^\+/ /) {
            chomp $line;
            $line .= $next_line;
            $next_line = <SOURCE>;
          }
          $line =~s/[\n\+]/ /g; $line=~s/\s\// /g; $line=~s/\s+/ /g;
          if( $line =~/^\.SUBCKT \Q$topcell\E/)  { $main_subckt=1;}
          print TARGET "$line\n";
      } elsif($main_subckt==1 && $line =~ /^X/i){
          # concatenate any continued lines
          while (defined $next_line and $next_line =~ s/^\+/ /) {
            chomp $line;
            $line .= $next_line;
            $next_line = <SOURCE>;
          }
          $line =~s/[\n\+]/ /g; $line=~s/\s\// /g; $line=~s/\s+/ /g;
          my ($oldName, $temp)=split(/ /,$line, 2);
          $oldName=~s/^X//;
          my $newName=convertInstName( $oldName );
          my @pin_list=split(/ /, $temp);
          my $cellName=$pin_list[$#pin_list];
          print TARGET "X$newName\n";
          for(my $k=0; $k<$#pin_list; $k++){
            $pin_list[$k]=convertNetName($pin_list[$k]); 
            print TARGET "+$pin_list[$k]\n";
          }
          print TARGET "+$cellName\n";

      } elsif($main_subckt==1 && $line =~ /^M/i){
          # concatenate any continued lines
          while (defined $next_line and $next_line =~ s/^\+/ /) {
            chomp $line;
            $line .= $next_line;
            $next_line = <SOURCE>;
          }
          $line =~s/[\n\+]/ /g; $line=~s/\s+/ /g;

        # process transistor
          my @fields = split(' ',$line, 7);
          my $name =$fields[0];
          my $term1=$fields[1];
          my $term2=$fields[2];
          my $term3=$fields[3];
          my $term4=$fields[4];
          my $type =$fields[5];
          my $rest =$fields[6];
          $name=~s/^M//;
          $term1 =~ /^\S+/ or print STDERR "Transistor has no terminals\n";
          $term2 =~ /^\S+/ or print STDERR "Transistor has only one terminal\n";
          $term3 =~ /^\S+/ or print STDERR "Transistor has only two terminals\n";
          $term4 =~ /^\S+/ or print STDERR "Transsitor has only three terminal\n";
          $name = convertInstName( $name );
          $term1 = convertNetName( $term1 );
          $term2 = convertNetName( $term2 );
          $term3 = convertNetName( $term3 );
          $term4 = convertNetName( $term4 );
          print TARGET "M$name $term1 $term2 $term3 $term4 $type $rest\n";

      } elsif($main_subckt==1 && $line =~ /^D/i){
          # concatenate any continued lines
          while (defined $next_line and $next_line =~ s/^\+/ /) {
            chomp $line;
            $line .= $next_line;
            $next_line = <SOURCE>;
          }
          $line =~s/[\n\+]/ /g; $line=~s/\s+/ /g;
        # process diode
          my @fields = split(' ',$line, 5);
          my $name=$fields[0];
          my $term1=$fields[1];
          my $term2=$fields[2];
          my $device=$fields[3];
          my $rest=$fields[4];
          $name=~s/^D//;
          $term1 =~ /^\S+/ or print STDERR "Diode has no terminals\n";
          $term2 =~ /^\S+/ or print STDERR "Diode has only one terminal\n";
          $device =~ /^\S+/ or print STDERR "Diode has no name\n";
          $name = convertInstName( $name );
          $term1 = convertNetName( $term1 );
          $term2 = convertNetName( $term2 );
          print TARGET "D$name $term1 $term2 $device $rest\n";

      } elsif($main_subckt==1 && $line =~ /^C/i){
          # concatenate any continued lines
          while (defined $next_line and $next_line =~ s/^\+/ /) {
            chomp $line;
            $line .= $next_line;
            $next_line = <SOURCE>;
          }
          $line =~s/[\n\+]/ /g; $line=~s/\s+/ /g;

        # process capacitor
          my @fields = split(' ',$line, 4);
          my $name=$fields[0];
          my $term1=$fields[1];
          my $term2=$fields[2];
          my $rest=$fields[3];
          $term1 =~ /^\S+/ or print STDERR "Capacitor has no terminals\n";
          $term2 =~ /^\S+/ or print STDERR "Capacitor has only one terminal\n";
          $term1 = convertNetName( $term1 );
          $term2 = convertNetName( $term2 );
          print TARGET "$name $term1 $term2 $rest\n";

      } elsif($main_subckt==1 && $line =~ /^R/i){
          # concatenate any continued lines
          while (defined $next_line and $next_line =~ s/^\+/ /) {
            chomp $line;
            $line .= $next_line;
            $next_line = <SOURCE>;
          }
          $line =~s/[\n\+]/ /g; $line=~s/\s+/ /g;

        # process resistor
          my @fields = split(' ',$line, 4);
          my $name=$fields[0];
          my $term1=$fields[1];
          my $term2=$fields[2];
          my $rest=$fields[3];
          $term1 =~ /^\S+/ or print STDERR "Resistor has no terminals\n";
          $term2 =~ /^\S+/ or print STDERR "Resistor has only one terminal\n";
          $term1 = convertNetName( $term1 );
          $term2 = convertNetName( $term2 );
          print TARGET "$name $term1 $term2 $rest\n";

      } else {
        print TARGET $line;
      }
      $line = $next_line;
  }
}



