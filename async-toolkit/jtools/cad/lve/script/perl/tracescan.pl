#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;
use Getopt::Long;

my $spar_dir= "/mnt/fulcrum/scratch1/aubrey/alta/spar";
my @net;
my $file = "";
my $verbose=0;
my $summary=0;
my @defaultnets=();
@defaultnets=("RS.D.1");
for (my $n = 0; $n <= 7; $n++) {
    push @defaultnets, "RS[$n].D.1";
}
for (my $n = 0; $n <= 7; $n++) {
    push @defaultnets, "SCAN_OUT[$n]";
}

sub usage {
    my ($msg) = @_;
    print STDERR "$msg" if defined $msg;
    print STDERR <<EU;
Usage: tracescan [options] [list of scan outputs] flat-verilog-file.v
    --spar-dir=<dir>   : spar dir, defaults to $spar_dir"
    --verbose          :
    scan outputs defaults to:
        @defaultnets

Note: verilog MUST be flat to work, flatten with rc
EU
    exit 1;
}

GetOptions (
    "summary" => \$summary,
    "verbose" => \$verbose,
    "spar-dir=s" => \$spar_dir,
) or usage;

if (@ARGV) {
    $file = pop;
    @net = @ARGV;
}
if (! @net) {
    @net=@defaultnets;
}

usage if ( ! -f "$file");

my %scanlength=();
open (P, "<$spar_dir/vendor/avago/serdes/SerDes_cln65g_flip_chip/sd65Ckr_vco113_dfef5_01.ctl") or warn "Cannot open : $!";
my $cn="sd65Ckr_vco113_dfef5_01";
my $len=undef;
while (<P>) {
    chomp;
    s/\s+/ /g;
    s/^ //;
    s/ $//;
    if (/ScanLength\s+(\d+)/) {
        $len=$1;
    }
    elsif (/ScanOut\s+"(\S+)"/) {
        $scanlength{$cn}->{$1}=$len if defined $len;
        $len=undef;
    }
}
close P;

my %assign;
my %rassign;
my %found;
my %cell;
my %inst;
my %pin=();

open (P, "<$file") or die "Cannot open $file";
my $element;
$/ = ";";
$_=<P>; # get rid of module declaration, not used
my %instancepath=();
my %in=();
my %total=();
my $total=0;
my %count=();
my %inspin=();
while (<P>) {
    chomp;
    s/\\/ /g;
    s/\n/ /g;
    s/\s+/ /g;
    s/^ //;
    next if /^\s*\/\//;
    if (/^assign/) {
        s/^assign\s+//;
        s/\\/ /g;
        my ($nt,$va)=split(/=/, $_, 2);
        $nt =~ s/\s//g;
        $va =~ s/^\s+//;
        $va =~ s/\s+$//;
        if ($va =~ /\x7b/) {
            $va =~ s/\x7b/ /g;
            $va =~ s/\x7d/ /g;
            $va =~ s/[, ]+/ /g;
            my @va = split(/[ ,]/, $va);
            for (my $n = 0; $n <= $#va; $n++) {
                $va[$#va-$n] =~ s/\s+//g;
                $assign{"$nt\[$n\]"}=$va[$#va-$n] if $va[$#va-$n] ne "";
                $assign{$va[$#va-$n]}="$nt\[$n\]" if $va[$#va-$n] ne "";
            }
        }
        else {
            $assign{$nt}=$va;
            $assign{$va}=$nt;
        }
    }
    elsif (! /^(input|output|inout|wire|endmodule|module)\s*/) {
        # an instance, ignore all but delay and buf and dff and cdc's
        s/\. /./g;
        s/\(/ ( /g;
        s/\)/ ) /g;
        s/\s+/ /g;
        my ($cell,$inst,$names)=split(/ /, $_, 3);
        my $in = undef;
        my $out = undef;
        my @f = split(/[, ]/, $names);
        my $oname="Q";
        my $iname=undef;
        if ($cell eq "sd65Ckr_vco113_dfef5_01") {
            my @f = split(/[, ]/, $names);
            my %cnt=();
            %cnt=%{$scanlength{$cell}} if defined $scanlength{$cell};
            my @out=();
            my @in=();
            my @ipin=();
            my @opin=();
            foreach my $n (0..$#f) {
                if ($f[$n] =~ /\.TEST__SOUT(\d+)$/) {
                    my $x=$1;
                    $out[$x]=$f[$n+2];
                    $opin[$x]=$f[$n];
                }
                if ($f[$n] =~ /\.TEST__SIN(\d+)$/) {
                    my $x=$1;
                    $in[$x]=$f[$n+2];
                    $ipin[$x]=$f[$n];
                }
            }
            for my $n (0..$#in) {
                if (defined($in[$n]) and defined ($out[$n])) {
                    @{$instancepath{$inst}}=[$in[$n],$out[$n]];
                    $inst{$out[$n]}=$inst;
                    $cell{$out[$n]}=$cell;
                    $in{$out[$n]}=$in[$n];
                    $pin{$out[$n]}=$opin[$n];
                    my $cnt=$cnt{"TEST__SOUT$n"};
                    $count{$out[$n]}=$cnt;
                    $inspin{$out[$n]}="$inst TEST__SOUT$n";
                }
            }
        }
        elsif ($cell =~ /lib\.synchronous\.conversion\.v3/) {
            my @f = split(/[, ]/, $names);
            my $in = undef;
            my $out = undef;
            my $ipin;
            my $opin;
            foreach my $n (0..$#f) {
                if ($f[$n] eq ".SCAN.IN") {
                    $in = $f[$n+2];
                    $ipin=$f[$n];
                }
                if ($f[$n] eq ".SCAN.OUT") {
                    $out = $f[$n+2];
                    $opin=$f[$n];
                }
            }
            @{$instancepath{$inst}}=[$in,$out];
            $inst{$out}=$inst;
            $cell{$out}=$cell;
            $in{$out}=$in;
            $pin{$out}=$opin;
        }
        elsif (($cell =~ /(buf|delay|_rcvr_|inv)/) and ! ($cell =~ /^lib.buffer.half/)) {
            my @f = split(/[, ]/, $names);
            my $in = undef;
            my $out = undef;
            my $ipin;
            my $opin;
            foreach my $n (0..$#f) {
                if ($f[$n] eq ".A") {
                    $in = $f[$n+2];
                    $ipin=$f[$n];
                }
                if ($f[$n] eq ".Q") {
                    $out = $f[$n+2];
                    $opin=$f[$n];
                }
            }
            if (defined ($in) and defined ($out)) {
                @{$instancepath{$inst}}=[$in,$out];
                $inst{$out}=$inst;
                $cell{$out}=$cell;
                $in{$out}=$in;
                $pin{$out}=$opin;
            }
        }
        elsif ($cell =~ /dff/) {
            my @f = split(/[, ]/, $names);
            my $in = undef;
            my $out = undef;
            my $outn = undef;
            my $ipin;
            my $opin;
            my $opinn;
            foreach my $n (0..$#f) {
                if ($f[$n] eq ".SI") {
                    $in = $f[$n+2];
                    $ipin=$f[$n];
                }
                if ($f[$n] eq ".Q" ) {
                    $out = $f[$n+2];
                    $opin=$f[$n];
                }
                if ($f[$n] eq ".QN") {
                    $outn = $f[$n+2];
                    $opinn=$f[$n];
                }
            }
            @{$instancepath{$inst}}=[$in,$out];
            $inst{$out}=$inst;
            $cell{$out}=$cell;
            $in{$out}=$in;
            $pin{$out}=$opin;
            if (defined ($outn)) {
                $in{$outn}=$in;
                $pin{$outn}=$opinn;
                $inst{$outn}=$inst;
                $cell{$outn}=$cell;
            }
        }
        elsif ($cell =~ /ltch/) {
            my @f = split(/[, ]/, $names);
            my $in = undef;
            my $out = undef;
            my $ipin;
            my $opin;
            foreach my $n (0..$#f) {
                if ($f[$n] eq ".D") {
                    $in = $f[$n+2];
                    $ipin=$f[$n];
                }
                if ($f[$n] eq ".Q") {
                    $out = $f[$n+2];
                    $opin=$f[$n];
                }
            }
            @{$instancepath{$inst}}=[$in,$out];
            $inst{$out}=$inst;
            $cell{$out}=$cell;
            $in{$out}=$in;
            $pin{$out}=$opin;
        }
        elsif ($cell =~ /^lib.serial.scan.[RSM]BUF_ChanDft/) {
            my @f = split(/[, ]/, $names);
            my $in = undef;
            my $out = undef;
            my $ipin;
            my $opin;
            foreach my $n (0..$#f) {
                if ($f[$n] eq ".L.D.1") {
                    $in = $f[$n+2];
                    $ipin=$f[$n];
                }
                if ($f[$n] eq ".R.D.1") {
                    $out = $f[$n+2];
                    $opin=$f[$n];
                }
            }
            @{$instancepath{$inst}}=[$in,$out];
            $inst{$out}=$inst;
            $cell{$out}=$cell;
            $in{$out}=$in;
            $pin{$out}=$opin;
        }
        elsif ($cell =~ /^lib.buffer/ or $cell =~ /lib.serial.shift_register.SHIFT_REG/) {
            my @f = split(/[, ]/, $names);
            my $in = undef;
            my $out = undef;
            my $ipin;
            my $opin;
            foreach my $n (0..$#f) {
                if ($f[$n] eq ".L.1") {
                    $in = $f[$n+2];
                    $ipin=$f[$n];
                }
                if ($f[$n] eq ".R.1") {
                    $out = $f[$n+2];
                    $opin=$f[$n];
                }
            }
            @{$instancepath{$inst}}=[$in,$out];
            $inst{$out}=$inst;
            $cell{$out}=$cell;
            $in{$out}=$in;
            $pin{$out}=$opin;
        }
        elsif ($cell =~ /lib.serial.scan.SCAN_CONFIG/) {
            my @f = split(/[, ]/, $names);
            my $in = undef;
            my $out = undef;
            my $ipin;
            my $opin;
            foreach my $n (0..$#f) {
                if ($f[$n] eq ".LS.1") {
                    $in = $f[$n+2];
                    $ipin=$f[$n];
                }
                if ($f[$n] eq ".RS.1") {
                    $out = $f[$n+2];
                    $opin=$f[$n];
                }
            }
            @{$instancepath{$inst}}=[$in,$out];
            $inst{$out}=$inst;
            $cell{$out}=$cell;
            $in{$out}=$in;
            $pin{$out}=$opin;
        }
        elsif ($cell =~ /lib\.serial\.scan\.(R_SCAN_CELL_|SCAN_CELL_)/) {
            my @f = split(/[, ]/, $names);
            my $in = undef;
            my $out = undef;
            my $ipin;
            my $opin;
            foreach my $n (0..$#f) {
                if ($f[$n] eq ".LS.1") {
                    $in = $f[$n+2];
                    $ipin=$f[$n];
                }
                if ($f[$n] eq ".RS.1") {
                    $out = $f[$n+2];
                    $opin=$f[$n];
                }
            }
            @{$instancepath{$inst}}=[$in,$out];
            $inst{$out}=$inst;
            $cell{$out}=$cell;
            $in{$out}=$in;
            $pin{$out}=$opin;
        }
        elsif ($cell =~ /^lib.serial/) {
            my @f = split(/[, ]/, $names);
            my $in = undef;
            my $out = undef;
            my $ipin;
            my $opin;
            foreach my $n (0..$#f) {
                if ($f[$n] eq ".LS.D.1") {
                    $in = $f[$n+2];
                    $ipin=$f[$n];
                }
                if ($f[$n] eq ".RS.D.1") {
                    $out = $f[$n+2];
                    $opin=$f[$n];
                }
            }
            @{$instancepath{$inst}}=[$in,$out];
            $inst{$out}=$inst;
            $cell{$out}=$cell;
            $in{$out}=$in;
            $pin{$out}=$opin;
        }
        elsif (($cell =~ /SCAN_BUF_X/) or ($cell =~ /^SCAN_CONFIG/) or ($cell =~ /^SCAN_TOK_BUF/) or ($cell =~ /^SCAN_TOK_EDFF/)) {
            my @f = split(/[, ]/, $names);
            my $in = undef;
            my $out = undef;
            my $ipin;
            my $opin;
            foreach my $n (0..$#f) {
                if ($f[$n] eq ".LS") {
                    $in = $f[$n+2];
                    $ipin=$f[$n];
                }
                if ($f[$n] eq ".RS") {
                    $out = $f[$n+2];
                    $opin=$f[$n];
                }
            }
            @{$instancepath{$inst}}=[$in,$out];
            $inst{$out}=$inst;
            $cell{$out}=$cell;
            $in{$out}=$in;
            $pin{$out}=$opin;
        }
        elsif ($cell =~ /^TO_1of2/) {
            my @f = split(/[, ]/, $names);
            my $in = undef;
            my $out = undef;
            my $ipin;
            my $opin;
            foreach my $n (0..$#f) {
                if (($f[$n] eq ".L") or ($f[$n] eq "L[0]") or ($f[$n] eq "L[1]")) {
                    $in = $f[$n+2];
                    $ipin=$f[$n];
                }
                if ($f[$n] eq ".R.1") {
                    $out = $f[$n+2];
                    $opin=$f[$n];
                }
            }
            @{$instancepath{$inst}}=[$in,$out];
            $inst{$out}=$inst;
            $cell{$out}=$cell;
            $in{$out}=$in;
            $pin{$out}=$opin;
        }
        elsif ($cell =~ /^FROM_1of2/) {
            my @f = split(/[, ]/, $names);
            my $in = undef;
            my $out = undef;
            my $ipin;
            my $opin;
            foreach my $n (0..$#f) {
                if ($f[$n] eq ".L.1") {
                    $in = $f[$n+2];
                    $ipin=$f[$n];
                }
                if ($f[$n] eq ".R") {
                    $out = $f[$n+2];
                    $opin=$f[$n];
                }
            }
            @{$instancepath{$inst}}=[$in,$out];
            $inst{$out}=$inst;
            $cell{$out}=$cell;
            $in{$out}=$in;
            $pin{$out}=$opin;
        }
        elsif ($cell =~ /^BUF_X/) {
            my @f = split(/[, ]/, $names);
            my $in = undef;
            my $out = undef;
            my $ipin;
            my $opin;
            foreach my $n (0..$#f) {
                if ($f[$n] eq ".L") {
                    $in = $f[$n+2];
                    $ipin=$f[$n];
                }
                if ($f[$n] eq ".R") {
                    $out = $f[$n+2];
                    $opin=$f[$n];
                }
            }
            @{$instancepath{$inst}}=[$in,$out];
            $inst{$out}=$inst;
            $cell{$out}=$cell;
            $in{$out}=$in;
            $pin{$out}=$opin;
        }
        else {
            my @f = split(/[, ]/, $names);
            my $in = undef;
            my $out = undef;
            my $ipin;
            my $opin;
            foreach my $n (0..$#f) {
                if ($f[$n] eq ".LS.D.1") {
                    $in = $f[$n+2];
                    $ipin=$f[$n];
                }
                if ($f[$n] eq ".RS.D.1") {
                    $out = $f[$n+2];
                    $opin=$f[$n];
                }
            }
            @{$instancepath{$inst}}=[$in,$out];
            $inst{$out}=$inst;
            $cell{$out}=$cell;
            $in{$out}=$in;
            $pin{$out}=$opin;
        }
        $total{$cell}++;
        $total++;
    }
}
close P;
my %summarycells=();

sub getcount {
    my ($out)=@_;
    my $count=0;
    if ($cell{$out} =~ /lib.synchronous.conversion.v3.SCAN_[AS]2[SA]*-L(\d+)-R/) {
        $count += $1+2;
    }
    elsif (defined ($scanlength{$cell{$out}})) {
        $count += $count{$out};
    }
    else {
#        $count++ if $cell{$out} =~ /ltch/;
        $count++ if $cell{$out} =~ /dff/;
        $count++ if $cell{$out} =~ /av_2xsyn/;
        $count++ if $cell{$out} =~ /^SCAN_BUF/;
        $count++ if $cell{$out} =~ /^SCAN_TOK_BUF/;
        $count++ if $cell{$out} =~ /^TOK_BUF/;
        $count++ if $cell{$out} =~ /^TOK_EDFF/;
        $count++ if $cell{$out} =~ /^SCAN_TOK_EDFF/;
        $count++ if $cell{$out} =~ /^SCAN_CONFIG/;
    }
    $count;
}

my $chaincnt=0;
foreach my $net (sort @net) {
    my $out=$net;
    my $count=0;
    # First count the ff's
    while (! $found{$out} and defined ($assign{$out}) or defined ($in{$out})) {
        $found{$out}=1;
        if (defined($assign{$out}) and ! $found{$assign{$out}}) {
            $out = $assign{$out};
        }
        elsif (defined($in{$out})) {
            $out = $in{$out};
        }
        $count += getcount($out);
    }
    %found=();
    $out=$net;
    # then print the reverse count since we started from the end
    my $last="";
    my @list=();
    if (! $found{$out} and defined ($assign{$out}) or defined ($in{$out})) {
        push @list, "OUT $out $count" if (! $found{$out} and defined ($assign{$out}) or defined ($in{$out}));
        push @list, "$out $count $cell{$out} $inst{$out} ".substr($pin{$out},1);
        while (! $found{$out} and defined ($assign{$out}) or defined ($in{$out})) {
            my $iout=$out;
            $found{$out}=1;
            if (defined($assign{$out}) and ! $found{$assign{$out}}) {
                $out = $assign{$out};
            }
            elsif (defined($in{$out})) {
                $out = $in{$out};
            }
            $count -= getcount($out);
            push @list, "$out $count $cell{$out} $inst{$out} ".substr($pin{$out},1) if $last ne $out;
            $summarycells{$cell{$out}}++ if $cell{$out};
            $last=$out;
        }
    }
    $list[$#list] = "IN $list[$#list]" if $#list >= 0;
    print join "\n", @list if $#list >= 0 and ! $summary;
    $chaincnt++ if $#list >= 0;
}
if ($summary) {
    if ($verbose) {
        foreach my $cell (sort keys %total) {
            print "$cell $total{$cell}";
        }
        print "";
    }
    print "Summary of cell usage in $chaincnt scan chains";
    my $maxlen=0;
    my $maxdgt=0;
    foreach my $cell (sort keys %summarycells) {
        $maxlen = length($cell) if length($cell) > $maxlen;
        $maxdgt = length($summarycells{$cell})
            if length($summarycells{$cell}) > $maxdgt;
    }
    foreach my $cell (sort keys %summarycells) {
        printf "%-*.*s %*d\n", $maxlen,$maxlen,$cell,$maxdgt,$summarycells{$cell};
    }
    print "Total of $total cells used in design";
}
