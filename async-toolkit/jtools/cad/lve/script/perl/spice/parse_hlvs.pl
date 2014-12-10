#!/usr/bin/perl

use strict;
my $line;
my $previousLine;
my $group;
my $net;
my $HLVS_FILE = "";

if ( $ARGV[0] ) { $HLVS_FILE = $ARGV[0]; }

open INFILE,"<$HLVS_FILE" or die "**ERROR**: Can't open file list $HLVS_FILE\n";
open OUTFILE,">$HLVS_FILE.summary" or die "**ERROR**: Can't open file list $HLVS_FILE.summary\n";
$line = <INFILE>;
while ($line)
{
    chomp $line;
    if ( $line =~ m/Comparison Result/ )
    {
        # Skip next three lines
        $line = <INFILE>;
        $line = <INFILE>;
        $line = <INFILE>;
        # Capture next 5
        my $line1 = <INFILE>;
        my $line2 = <INFILE>;
        my $line3 = <INFILE>;
        my $line4 = <INFILE>;
        my $line5 = <INFILE>;
        chomp $line1;
        chomp $line2;
        chomp $line3;
        chomp $line4;
        chomp $line5;
        # Check if pass
        my $pass = 0;
        if ($line1 eq "                       #####    ##    #####  #####" &&
            $line2 eq "                       #    #  #  #  #      #"      &&
            $line3 eq "                       #####  ######  ####   ####"  &&
            $line4 eq "                       #      #    #      #      #" &&
            $line5 eq "                       #      #    # #####  #####")
        {
            $pass = 1;
        }
        # Get the cell name
        $line = <INFILE>;
        $line = <INFILE>;
        chomp $line;
        $line =~ m/\[(\S+)/;
        # Make it nice
        my $cell = $1;
        $cell =~ s/_D_/./g;
        $cell =~ s/_C_/,/g;
        $cell =~ s/_l_/[/g;
        $cell =~ s/_r_/]/g;
        $cell =~ s/_U_/_/g;
        # Print output
        if ($pass)
        {
            printf OUTFILE ("\n===== PASS ===== $cell");
        }
        else
        {
            printf OUTFILE ("\n===== FAIL ===== $cell");
        }
    }
    elsif ( $line =~ m/^DIAGNOSTIC:/ )
    {
        printf OUTFILE "\n\n$line\n";
        $group = 0;
    }
    elsif ( $line =~ m/^\s+Group (\d+) of \d+/ )
    {
        $group = $1;
    }    
    elsif ( $line =~ m/^        (\S+)\s+:\s+\d+/ )
    {
        $net = $1;
        $net =~ s/_D_/./g;
        $net =~ s/_C_/,/g;
        $net =~ s/_l_/[/g;
        $net =~ s/_r_/]/g;
        $net =~ s/_U_/_/g;
        printf OUTFILE ("%4d: $net\n",$group);
    }
    elsif ( $line =~ m/^    (\S+)\s+:\s+\d+/ )
    {
        $net = $1;
        $net =~ s/_D_/./g;
        $net =~ s/_C_/,/g;
        $net =~ s/_l_/[/g;
        $net =~ s/_r_/]/g;
        $net =~ s/_U_/_/g;
        printf OUTFILE ("%4d: $net\n",$group);
    }
    elsif ( $previousLine =~ m/^    \S+\s+/ && $line =~ m/^      \S+\s+:\s+\d+/ )
    {        
        $previousLine =~ m/^    (\S+)\s+/;        
        $net = $1;
        $line =~ m/^      (\S+)\s+:\s+\d+/;
        $net = $net . $1;
        $net =~ s/_D_/./g;
        $net =~ s/_C_/,/g;
        $net =~ s/_l_/[/g;
        $net =~ s/_r_/]/g;
        $net =~ s/_U_/_/g;
        printf OUTFILE ("%4d: $net\n",$group);
    }
    $previousLine = $line;
    $line = <INFILE>;
}
printf OUTFILE "$group: $net";
close INFILE;
close OUTFILE;


