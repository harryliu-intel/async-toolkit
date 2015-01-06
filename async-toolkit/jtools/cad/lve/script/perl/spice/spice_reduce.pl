#!/usr/intel/bin/perl -w
# $Id$
# $DateTime$
# $Author$
# Copyright 2004-2006 Fulcrum Microsystems.  All rights reserved.

use strict;
use Getopt::Long;

# check if a string is a legal real number
sub is_numeric {
    my ($parm) = @_;
    if ($parm =~ /^[-+]?[\d]+(\.[\d]+)?([eE]?[-+]?[\d]+)?$/ ) { return 1; }
    else { return 0; }
}

sub usage() {
    die "Usage: $0 --infile=[infilename] --outfile=[outfilename] [--mode=nvn|totem] \n";
}

my $macro_model = 0; # for macro-model transistors instead of primitives
my $f_out;
my $f_in;
my $mode="";
my $grayboxlist="";
my $cdlgds2="";
my $debug=0;
my $blackbox=1; # always, detected automatically

GetOptions(
    "infile=s" => \$f_in,
    "outfile=s" => \$f_out,
    "mode=s" => \$mode,
    "graybox-list=s" => \$grayboxlist,
    "cdlgds2-file=s" => \$cdlgds2,
    "blackbox" => \$blackbox,
    "debug" => \$debug,
) || usage;

my %grayboxlist=();
my %subcktlist=();

defined $f_out or usage();
defined $f_in or usage();

if ( -r "$grayboxlist") {
    open (P, "<$grayboxlist");
    while (<P>) {
        chomp;
        my @f=split;
        $grayboxlist{$f[$#f]}=1;
    }
    close P;
}

my %includedfiles;
$includedfiles{$f_in}=1;
open( TARGET, ">$f_out") or die "Can't open '$f_out' for writing.\n";
print TARGET "* Reduced deck\n";
my $level=-1;
my $topcell="";
my %portlist;
my $exitcode=0;
my $wraplimit=1020; # hspice has a 1024 char limit. Hsim too, I think

sub wrapline {
    my ($line)=@_;
    my $n=0;
    $line =~ s/\s+/ /g;
    $line =~ s/\s\/\s/ /g;
    $line =~ s/\s$//;
    $line =~ s/^\s//;
    my @f=split(/ /,$line);
    # comment out resistors
    if ($#f > 2 and ($f[0] =~ /^x/i) and ($f[3] =~ /^r/i) and
        (($#f == 3) or ($f[4] =~ /=/))) {
        $line = "*$line";
        print TARGET "$line\n";
        return;
    }
    while (length($line) > $wraplimit) {
        my $end = rindex($line, " ", $wraplimit);
        my $sub = $line;
        $sub = substr($line, 0, $end) if ($end > 0);
        $line = substr($line, length($sub));
        $sub =~ s/\s+$//;
        printf TARGET "+ " if $n;
        $n++;
        print TARGET "$sub\n";
        $line =~ s/^\s+//;
    }
    printf TARGET "+ " if $n;
    print TARGET "$line\n";
}

my %loaded=();
sub reduce {
    my ($f_in,$ftop)=@_;
    chomp $f_in;
    my $ends = "";
    return if $loaded{$f_in};
    $loaded{$f_in}=1;
    if ( ! -s $f_in ) {
        my $i_in=$f_in;
        my $t_in=$f_in;
        $i_in =~ s/spice_gds2/spice_include/;
        $t_in =~ s/spice_gds2/spice_topcell/;
        if ( -s $t_in) {
            reduce ($i_in,$ftop);
            reduce ($t_in,$ftop);
        }
    }
    else {
    print "Reading $f_in\n" if $debug;
    local(*SOURCE, $_,$.);
    my $localtopcell="";
    my %hastransistors=();

    if (! open( SOURCE, "<$f_in")) {
        print STDERR "Error: Can't open '$f_in' for reading from $ftop.\n";
        $exitcode=1;
        return;
    }
    # code to fix nodes named '0' (zero), see bug 11694
    my %znode=();
    my $zcnt=0;
    if ($mode eq "nvn") {
        my @hash=();
        my $mndx=-1;
        my %deleted=();
        my %group=();
        while (<SOURCE>) {
            if (/^R/i) {
                my ($r,$n1,$n2) = split;
                next if !($n1 =~ /^0:/ and $n2 =~ /^0:/);
                my $chg1=-1;
                my $chg2=-1;
                $zcnt++;
                if (defined($group{$n1})) {
                    $chg1 = $group{$n1};
                    my %x=%{$hash[$chg1]};
                    $x{$n2}=1;
                    if (defined($group{$n2}) and $group{$n2} != $chg1) {
                        $chg2=$group{$n2};
                        my %y=%{$hash[$chg2]};
                        foreach my $node (keys %y) {
                            $x{$node}=1;
                            $group{$node}=$chg1;
                        }
                        %y=();
                        %{$hash[$chg2]}=%y;
                        $deleted{$chg2}=1;
                    }
                    $group{$n2}=$chg1;
                    %{$hash[$chg1]}=%x;
                }
                elsif (defined($group{$n2})) {
                    $chg2 = $group{$n2};
                    my %x = %{$hash[$chg2]};
                    $x{$n1}=1;
                    %{$hash[$chg2]}=%x;
                    $group{$n1}=$chg2;
                }
                else {
                    $mndx++;
                    my %x=();
                    $x{$n1}=1;
                    $x{$n2}=1;
                    %{$hash[$mndx]} = %x;
                    $group{$n1}=$mndx;
                    $group{$n2}=$mndx;
                }
            }
        }
        close SOURCE;
        foreach my $n (0..$mndx) {
            if (! $deleted{$n}) {
                $zcnt++;
                my %x=%{$hash[$n]};
                my $key="";
                foreach my $node (sort keys %x) {
                    if ($key eq "") {
                        $key = $node;
                        $key =~ s/:/_/g;
                    }
                    $znode{$node}=$key;
                }
            }
        }
        open( SOURCE, "<$f_in");
    }
    $level++;
    $. = 0;
    my $line = <SOURCE>;
    my $next_line;
    $"=" ";
    while ($line) {
        $next_line = <SOURCE>;

        # concatenate any continued lines
        while (defined $next_line and $next_line =~ s/^\+/ /) {
            chomp $line;
            $line .= $next_line;
            $next_line = <SOURCE>;
        }

        if ($line =~ /^ *\.inc/i) {
            my ($x,$file)=split(/  */, $line);
            $file =~ s/'//g;
            $file =~ s/"//g;
            reduce($file,$f_in) if (! $includedfiles{$file});
            $includedfiles{$file}=1;
            $line = $next_line;
            next;
        }

        if ($mode eq "nvn") { $line =~ s/\//_S_/g; }
        $hastransistors{$localtopcell} = 1 if ($line =~ /^m/i);
        if ($mode eq "nvn" and $zcnt > 0) {
            my @f=split(/ /, $line);
            foreach my $node (@f) {
                $node = $znode{$node} if (defined ($znode{$node}));
            }
            $line = join (" ", @f);
        }
        if ($line =~ /^R/i and $mode ne "totem") {

            # process resistor to remove width/length/layer info if any
            $line =~ s/\s+/ /g;
            my @fields = split(' ',$line);
            my $name=$fields[0];
            my $term1=$fields[1];
            my $term2=$fields[2];
            $term1 =~ /^\S+/ or print STDERR "Resistor has no terminals";
            $term2 =~ /^\S+/ or print STDERR "Resistor has only one terminal";
            my $res=1e-5;
            if (is_numeric($fields[3])) { $res=$fields[3]; }
            elsif (is_numeric($fields[4])) { $res=$fields[4]; }
            else { $res=$fields[6]; $res =~ s/^.*MODEL=//i; }
            is_numeric($res)
                or print STDERR "Resistor has no resistance specifier";
            my $term1x = $term1;
            $term1x =~ s/:.*//;
            my $term2x = $term2;
            $term2x =~ s/:.*//;
            my $model=$fields[4];
            $model = "" if ! $model =~ /\.model/i;
            if ($mode eq "nvn" and $term1x ne $term2x) {
                print TARGET "$name $term1x $term2x $res $model\n";
            }
            elsif ($mode ne "nvn") {
                print TARGET "$name $term1 $term2 $res\n";
            }
        }
        elsif ($line =~ /^c/i and $mode eq "nvn") {
            # skip all capacitors
        }
        elsif ($line =~ /^.ends/i and $mode eq "nvn") {
            $ends = $line;
            $line = $next_line;
        }
        elsif ($line =~ /^.subckt/i and $mode eq "nvn") {
            print TARGET "$ends" if $ends ne "";
            my @f=split(/ /,$line,3);
            $subcktlist{$f[1]}=1;
            print "$f[0] $f[1]\n" if $debug;
            $topcell = $f[1] if $level == 0;
            $localtopcell = $f[1];
            $hastransistors{$localtopcell}=0;
            wrapline "$line";
            $ends = "";
            $line = $next_line;
        } else {
            $line =~ s/\s+/ /g;
            $line =~ s/^\s+//;
            if ($mode eq "nvn" and $line ne "\n" and $line ne "") {
                my @f=split(/ /,$line);
                chomp $f[$#f] if $#f >= 0;
                my $lf=0;
                foreach my $n (0..$#f) {
                    if ($f[$n] =~ /=/) {
                        last;
                    }
                    $lf=$n;
                    $f[$n] =~ s/:.*//;
                }
                if ($f[0] =~ /^\.subckt/i) {
                    $subcktlist{$f[1]}=1;
                    print "$f[0] $f[1]\n" if $debug;
                    $topcell = $f[1] if $level == 0;
                    $portlist{$f[1]} = [@f];
                }
                if ($f[0] =~ /^x/i) {
                    $subcktlist{$f[$lf]} |= 2;
                    if ($level == 0) {
                        my $subcell=$f[$lf];
                        print "TOP $subcell\n" if $debug;
                        if ($grayboxlist{$subcell}) {
                            $grayboxlist{$subcell} = 2;
                        }
                    }
                    else {
                        print "REF $f[$lf] ($lf) ($level)\n" if $debug;
                    }
                }
                $line = "@f";
            }
            # macro-model transistors
            # change M to X and force parameters to lower-case
            if ($mode ne "nvn" && $macro_model && $line =~ s/^M/X/gi) {
                my @fields = split(/\s+/,$line);
                for (my $f=0; $f<@fields; $f++) {
                    if ($fields[$f] =~ /(\S+)=(\S+)/ ) {
                        $fields[$f] = lc($1) . "=$2";
                    }
                }
                $line = join(" ",@fields) . "\n";
            }
            # pass through non-resistor lines, non blank
            wrapline "$line" if ($line ne "\n" and $line ne "");
        }
        $line = $next_line;
    }
    close(SOURCE);
    if ($mode eq "nvn") {
        my %need=();
        my $need=0;
        foreach my $cell (sort keys %grayboxlist) {
            if ($grayboxlist{$cell} == 1 and $level == 0 and
                                ! ($cell =~ /^SUBCELL_\d+$/)) {
                print STDERR "Warning: Graybox cell $cell not in $f_in\n";
                $need{$cell}=1;
                $need=1;
            }
        }
        foreach my $cell (sort keys %subcktlist) {
            if ( $subcktlist{$cell} == 1 and $cell ne $topcell ) {
                if ($level != 0) {
                    $need{$cell}=1;
                    $need=1;
                }
            }
        }
        if ($need and $blackbox and ! $hastransistors{$localtopcell}) {
            open (CDL, "<$cdlgds2") or warn "Cannot open cdl file $cdlgds2\n";
            my $line=<CDL>;
            my $nextline="";
            chomp $line;
            my $istopcell=0;
            while (<CDL>) {
                chomp;
                $nextline = $_;
                while ($nextline =~ s/^\+/ /) {
                    $line .= $nextline;
                    $nextline = <CDL>;
                    chomp $nextline;
                }
                $line =~ s/\s+/ /g;
                $_=$line;
                if (/^.subckt/i) {
                    $istopcell = 1 if (/ $localtopcell /);
                }
                $istopcell=0 if (/^.ends/i);
                if ($istopcell and /^x/i) {
                    s/\s+\/\s+/ /g;
                    my @f=split;
                    if ($need{$f[$#f]} and $#f == 3 and $f[1] eq "GND" and $f[2] eq "Vdd" ) {
                        print "Warning: Adding $f[0] $f[$#f] in $localtopcell\n";
                        wrapline "$_\n";
                    }
                }
                $line = $nextline;
            }
            close CDL;
        }
        print TARGET ".ENDS \$ added $f_in\n"
            if ! ($f_in =~ /\.spice_include/);
    }
    $level--;
    }
}

reduce ($f_in,"top");
close(TARGET);
exit($exitcode);

