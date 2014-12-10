#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use Getopt::Long;

open (FILE, "list_3.txt");
open (FL, "list_4.txt");

while (my $se = <FILE>) {
chomp;
        $se =~ s/\./\//g;
        $se =~ s/[\s+]//g;
	$k=0;
	$m=0;

	$line = "RESET";
	if ($se=~/$line/){
		$k=1; 
	}
	$lin = "MBUF";
	if ($se=~/$lin/){
		$m=1; 
	}

if ($k==1)
{
$se ="spec/".$se;
 	$se =~ s/\./\//g;
	$se =~ s/[\s+]//g;
	$se.=".cast";



open (P, "<$se");
    my $out=$se;
    $out =~ s/\.cast//;
    $out .= "_hvt.cast";
    open (Q, ">$out");
    while (<P>) {
        chomp;
        if (/^define\s+"([^"]+)"/) {
            my $st=$1;
            s/"$st"/"${st}_hvt"/;
        }
        if (/\s(gate|stack)/) {
            s/\(1\)/(3)/;
            s/\(1,1\)/(3,3)/;
        }
        if (/synthesis\.qdi\..*\.(\d+)\s/) {
            my $st=$1;
            s/\.$st /\.${st}_hvt /;
        }
        if (/standard\.reset\..*\.(\d+)\s/) {
            my $st=$1;
            s/\.$st /\.${st}_hvt /;
        }
	if (!(/lib\.util\.spare\..*\.(\d+)\s/)) {
        if (/lib\..*\.(\d+)(.?)\s/) {
            my $st=$1.$2;
            s/\.$st /\.${st}_hvt /;
        }}
        if (/chip\..*\.(\d+)(.?)\s/) {
		my $st=$1.$2;
            s/\.$st /\.${st}_hvt /;
        }
        print Q;
        }

close P;
close Q;

}
if ($m==1)
{
$se ="spec/".$se;
 	$se =~ s/\./\//g;
	$se =~ s/[\s+]//g;
	$se.=".cast";



open (P, "<$se");
    my $out=$se;
    $out =~ s/\.cast//;
    $out .= "_svt.cast";
    open (Q, ">$out");
    while (<P>) {
        chomp;
        if (/^define\s+"([^"]+)"/) {
            my $st=$1;
            s/"$st"/"${st}_svt"/;
        }
        if (/\s(gate|stack)/) {
            s/\(2\)/(1)/;
            s/\(2,2\)/(1,1)/;
        }
        if (/synthesis\.qdi\..*\.(\d+)\s/) {
            my $st=$1;
            s/\.$st /\.${st}_hvt /;
        }
        if (/standard\.reset\..*\.(\d+)\s/) {
            my $st=$1;
            s/\.$st /\.${st}_hvt /;
        }
	if (!(/lib\.util\.spare\..*\.(\d+)\s/)) {
        if (/lib\..*\.(\d+)(.?)\s/) {
            my $st=$1.$2;
            s/\.$st /\.${st}_hvt /;
        }}
        if (/chip\..*\.(\d+)(.?)\s/) {
		my $st=$1.$2;
            s/\.$st /\.${st}_hvt /;
        }
        print Q;
        }

close P;
close Q;

}

}

while (my $see = <FL>) {
chomp;
        $see =~ s/\./\//g;
        $see =~ s/[\s+]//g;


$see ="spec/".$see;
 	$see =~ s/\./\//g;
	$see =~ s/[\s+]//g;
	$see.=".cast";



open (P, "<$see");
    my $out=$see;
    $out =~ s/\.cast//;
    $out .= "_svt.cast";
    open (Q, ">$out");
    while (<P>) {
        chomp;
        if (/^define\s+"([^"]+)"/) {
            my $st=$1;
            s/"$st"/"${st}_svt"/;
        }
        if (/\s(gate|stack)/) {
            s/\(2\)/(1)/;
            s/\(2,2\)/(1,1)/;
        }
        if (/standard\.reset\..*\.(\d+)\s/) {
            my $st=$1;
            s/\.$st /\.${st}_hvt /;
        }
	if (!(/lib\.util\.spare\..*\.(\d+)\s/)) {
        if (/^\s+(lib\.\S+)\s+([^+-\s]+);/) {
		@sw = split( /\s+/,$_);
		$found=0;
		open (FIL, "spec/SVT_skipcells.txt");
		while ($sss = <FIL>) {
		chomp $sss;
		$sss =~ s/[\s+]//g;

        		if ($sw[1]=~/$sss/){
        			$found=1;
				last;
			}
		}
		close FIL;
		if ($found!=1)
		{
			  my $st=$sw[1];
		          s/\ \Q$st\E /\ ${st}_svt /;

		}

        }
	}

        print Q;
        }

close P;
close Q;

}
