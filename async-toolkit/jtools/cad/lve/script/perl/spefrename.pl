#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use IPC::Open2;
use Getopt::Long;
use strict;

# comes directly from StarRC
my $from  = "gds2";
my $to = "cadence";
my $verbose = 0;
my $spefin;
my $spefout;

GetOptions(
    "verbose" => \$verbose,
    "from=s" => \$from,
    "to=s" => \$to,
) or die;

my $rename = "fulcrum rename";
$rename = "rename" if (`which rename` =~ m:/tools/:);
my $npid = open2(\*RDN,\*WRN, "$rename --type=node --from=$from --to=$to");
my $cpid = open2(\*RDC,\*WRC, "$rename --type=cell --from=$from --to=$to");
$spefin  = shift;
$spefout = shift;

sub nxl {
    my $n=@_[0];
    print WRN "$n";
    $n = <RDN>;
    chomp $n;
    $n;
}

sub cxl {
    my $n=@_[0];
    print WRC "$n";
    $n = <RDC>;
    chomp $n;
    $n;
}

my %ilookup=();
my %rcmap=();
my %nmap=();

sub ilookup {
    my $fn = @_[0];
    return $ilookup{$fn} if defined $ilookup{$fn};
    my $rname = $fn;
    my ($iname,$pname)=split(/:/,$fn);
    my $riname = $iname;
#    $pname =~ s/\\#/_U_/g;
    $pname = nxl($pname);
    $rname = "$iname:$pname";
    $ilookup{$fn}=$rname;
    $rname;
}

open (STDIN, "<$spefin") or die "Can't open $spefin: $!"
    if defined $spefin;
open (STDOUT, ">$spefout") or die "Can't open $spefout: $!"
    if defined $spefout;
my $cn=0;
my $mode="";

while (<STDIN>) {
    chomp;
    if (/^\*BUS_DELIMITER/) {
        $_ = "*BUS_DELIMITER <>";
    }
    if ($mode eq "namemap" and /^\*\d+ /) {
        my ($n,$name)=split;
        $name = nxl($name);
        $rcmap{$n}=$name;
        $nmap{$name}=$n;
        $_ = "$n $name";
    }
    if ($mode eq "conn" and /^\*I/) {
        my @f=split;
        $f[1] = ilookup($f[1]);
        $f[$#f] = cxl($f[$#f]);
        $_ = "@f";
    }
    if (($mode eq "res" or $mode eq "cap") and /^\d/) {
        my @f=split;
        for (my $i = 1; $i <= $#f and $i < 3; $i++) {
            if ($f[$i] =~ /:/) {
                $f[$i] = ilookup($f[$i]);
            }
        }
        $_ = "@f";
    }
    $mode = "namemap" if /^\*NAME_MAP/;
    $mode = "ports" if /^\*PORTS/;
    $mode = "dnet" if (/^\*D_NET/);
    $mode = "conn" if /^\*CONN/;
    $mode = "cap" if /^\*CAP/;
    $mode = "res" if /^\*RES/;
    $mode = "" if /^\*END/;
    print;
}
close STDIN;
close STDOUT;
