#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;
use Getopt::Long;

my %options;
my $verilog;
my $topcell;

GetOptions ( %options ) or die;
$topcell = shift;
$verilog = shift;

if ($verilog =~ /gz$/) {
    open (P, "gunzip -c '$verilog' |");
}
else {
    open (P, "<$verilog");
}
$/ = "\n";
my @assign=();
my @lines=();
my @statements=();
my $state=0;
while (<P>) {
    chomp;
    push @lines, $_;
    if (/^module.*$topcell/) {
        $state = 1;
    }
    if ($state == 1 and /^endmodule/) {
        last;
    }
}
close P;
my $statement="";
my $module=0;
foreach my $line (@lines) {
    if ($line =~ /^module.*$topcell/) {
        $module = 1;
    }
    if ($line =~ /^endmodule/) {
        $module=0;
    }
    if ($module) {
        $statement .= $line;
        if ($statement =~ /;\s*$/) {
            $statement =~ s/\s+/ /g;
            $statement =~ s/;\s*$//;
            $statement =~ s/^ //;
            push @statements, $statement;
            $statement="";
        }
    }
    else {
        $statement="";
    }
}
my %needassign = (
    "SCAN_OUT" => ["output [7:0]","SO%d"],
    "SCAN_MODE" => ["input","+TI"],
    "SCAN_IN" => ["input [7:0]","SI%d"],
    "SCAN_IDL" => ["input [7:0]","-TI"],
    "SCAN_EN" => ["input","+SE"],
    "MODULE_RESET_N" => ["input","+SC"],
    "CORE_RESET_N" => ["input","+SC"],
    "CLK" => ["input","-ES"],
    "CDC_RESET_N" => ["input","+SC"],
);
my $statement=$statements[0];
$statement =~ s/\(/ ( /g;
$statement =~ s/\)/ ) /g;
$statement =~ s/,\s*/ /g;
my @f=split(/ /,$statement);
shift @f; # module
shift @f; # topcell
shift @f; # lparen
pop @f;   # rparen
my %ports=();
foreach my $port (@f) {
    $ports{$port}=1;
}
my %trace=();
foreach my $statement (@statements) {
    $_ = $statement;
    if (/av_/ and (/buf/ or /delay/)) {
        s/\(/ ( /g;
        s/\)/ ) /g;
        s/\s+/ /g;
        my $out;
        my $in;
        my @f=split;
        foreach my $n (0..$#f) {
            if ($f[$n] eq ".Q") {
                $out = $f[$n+2];
            }
            elsif ($f[$n] eq ".A") {
                $in = $f[$n+2];
            }
        }
        $in =~ s/\s//g;
        $out =~ s/\s//g;
        $trace{$in}=$out;
    }
    if (/lib.dft.converter.SYNC_CONVERTER_ARRAY_8.1000/) {
        s/\n/ /g;
        s/\s+/ /g;
        s/^[^\(]*\(//;
        my @ports = split(/,/);
        my @pl=();
        my $line = "";
        for( my $n=0; $n <= $#ports; $n++) {
            my $assign=$ports[$n];
            $assign =~ s/^\s*//;
            $line = $assign;
            while (! ($line =~ /\)/)) {
                $n++;
                $line .= ", $ports[$n]";
            }
            push @pl, $line;
        }
        foreach my $pl (@pl) {
            my ($port,$wire)=split(/\(/,$pl);
            $port =~ s/^\.//;
            $port =~ s/\s+$//;
            $wire =~ s/\)+\s*$//g;
            s/  */ /g;
            push @assign, "  assign $port = $wire;" if ! ($port =~ /\\/) and defined($needassign{$port});
        }
    }
}
my @newports=();
foreach my $port (sort keys %needassign) {
    push @newports, $port if (! $ports{$port});
}
my $doassign=1;
$module=0;
for( my $n = 0; $n <= $#lines; $n++) {
    while ($lines[$n] =~ /^\s*$/) {
        $n++;
    }
    $_ = $lines[$n];
    if (/^module.*lib.synchronous/) {
        while (! ($lines[$n] =~ /^endmodule/)) {
            $n++;
        }
        next;
    }
    if (/^module.*$topcell/) {
        $module=1;
    }
    if ($module == 1 and /;/) {
        s/\);\s*$//;
        $_ .= ", ".join(" , ", @newports)." );";
        $module=2;
        print;
        foreach my $port (@newports) {
            print "   $needassign{$port}[0] $port;";
        }
        next;
    }
    if (/lib.dft.converter.SYNC_CONVERTER_ARRAY_8.1000/) {
        while ( ! ($lines[$n] =~ /;/)) {
            print "// $lines[$n]";
            $n++;
        }
        print "// $lines[$n]";
        next;
    }
    if ($doassign and $module == 2 and /\(/) {
        print join ("\n", @assign);
        print "";
        $doassign=0;
    }
    print;
}
# find valid scan
my @in;
my @out;
foreach my $as (@assign) {
    if ($as =~ /SCAN_IN/) {
        $as =~ s/,/ /g;
        $as =~ s/\s+/ /g;
        $as =~ s/\x7d;//;
        @in = split(/ /,$as);
        while (@in and $in[0] ne "\x7b") {
            shift @in;
        }
        shift @in;
    }
    if ($as =~ /SCAN_OUT/) {
        $as =~ s/,/ /g;
        $as =~ s/\s+/ /g;
        $as =~ s/\x7d;//;
        @out = split(/ /,$as);
        while (@out and $out[0] ne "\x7b") {
            shift @out;
        }
        shift @out;
    }
}
sub trace {
    my $in = $_[0];
    $in =~ s/\s//g;
    while (defined ($trace{$in})) {
        $in = $trace{$in};
    }
    $in;
}

my @valid;
foreach my $n (0..$#in) {
    my $out = trace($in[$n]);
    $valid[7-$n]=1 if $out ne $out[$n];
    $valid[7-$n]=0 if $out eq $out[$n];
}
open (P, ">x.pinassign");
select P;
foreach my $port (sort keys %needassign) {
    if ($needassign{$port}[0] =~ /:/) {
        foreach my $n (0..7) {
            printf "assign pin=$port\[%d\]\ttest_function= $needassign{$port}[1];\n",
                $n, $n if $valid[$n];
        }
    }
    else {
        print "assign pin=$port\ttest_function= $needassign{$port}[1]";
    }
}
