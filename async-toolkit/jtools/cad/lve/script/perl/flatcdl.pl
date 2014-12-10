#!/usr/intel/bin/perl -w
# AAG
# $Id:$
# $DateTime:$

use Getopt::Long;
use IPC::Open2;
use strict;

# This does a hierarchical expansion of a spice deck

$|=1;
select STDERR;
$|=1;
select STDOUT;

my $sep=".";
my $verbose;
my $cdl;
my $library;
my $scale=1;
my $outputfile;
my $stripcolon=0;
my @subcktlist=();

my $topcell;
my $pdk="";
$|=1;
my %subckts;
my %called;

my %globals=(
#    "vss" => 1,
#    "vdd" => 1,
#    "vtt" => 1,
#    "vddA" => 1,
#    "0" => 1,
#    "gnd" => 1,
);

sub expandvalue {
    my ($value) = @_;
    $value =~ s/'//g;
    if ($value =~ /u$/i) {
        $value =~ s/u//i;
        $value = "(($value)*1e-6)";
    }
    elsif ($value =~ /n$/i) {
        $value =~ s/n//i;
        $value = "(($value)*1e-9)";
    }
    elsif ($value =~ /p$/i) {
        $value =~ s/p//i;
        $value = "(($value)*1e-12)";
    }
    eval("\$value = $value");
    $value;
}

sub isglobal {
    my ($name)=@_;
    foreach my $f (keys %globals) {
        return 1 if ($name =~ /^$f$/i);
    }
    0;
}

my $linelength=4096;
my $grayboxfile;
my %graybox_gds2;
my %graybox_cast;
my %graybox_cadence;
my $mode="cadence";
my $argmode;
my %graybox_list;


my %options = (
    "cdl=s" => \$cdl,
    "library=s" => \$library,
    "cell=s" => \$topcell,
    "verbose" => \$verbose,
    "scale=f" => \$scale,
    "linelength=i" => \$linelength,
    "graybox=s" => \$grayboxfile,
    "output=s" => \$outputfile,
    "mode=s" => \$argmode,
    "stripcolon" => \$stripcolon,
);

sub readgraybox {
    if (defined $grayboxfile and -r $grayboxfile and -f $grayboxfile) {
        local (*RDG, *WRG);
        local (*RDC, *WRC);
        local (*P, $_);
        my $pidg=open2 (\*RDG, \*WRG, "rename --type=cell --from=cast --to=gds2");
        my $pidc=open2 (\*RDC, \*WRC, "rename --type=cell --from=cast --to=cadence");
        open (P, "<$grayboxfile");
        while (<P>) {
            chomp;
            s/ //g;
            my $cast=$_;
            $graybox_cast{$cast}=1;
            print WRG "$cast\n";
            $_=<RDG>;
            chomp;
            $graybox_gds2{$_}=1;
            print WRC "$cast\n";
            $_=<RDC>;
            chomp;
            $graybox_cadence{$_}=1;
        }
        close P;
        close RDG;
        close WRG;
        close RDC;
        close WRC;
        waitpid $pidg, 0;
        waitpid $pidc, 0;
    }
}

sub writeline {
    my ($line,$inst)=@_;
    if ($line =~ /^\*/) {
        return;
    }
    my ($i,$l)=split(/  */,$line,2);
    my $p=substr($i,0,1);
    $inst = "" if !defined ($inst);
    $i = substr($i,1);
    $i = "$inst${sep}$i" if $inst ne "";
    $line = "$p$i";
    $line .= " $l" if defined $l;
    $line =~ s/  */ /g;
    my $cnt=0;
    my $ndx;
    my $out;
    while (length($line) > $linelength-1) {
        $ndx=rindex(substr($line,0,$linelength-2)," ");
        if ($ndx >= 0) {
            $out=substr($line,0,$ndx);
            $line = substr($line,$ndx+1);
        }
        else {
            $out = $line;
            $line = "";
        }
        print "+ " if $cnt;
        print "$out\n";
        $cnt++;
    }
    print "+ " if $cnt and length($line);
    print "$line\n" if (length ($line));
}

GetOptions ( %options ) or usage_exit();
$mode=$argmode if defined $argmode;
$cdl = $ARGV[0] if defined $ARGV[0] and ! defined $cdl;
usage_exit("CDL File not defined or not present") if (! defined ($cdl) or ( ! -f "$cdl" ));
usage_exit("Too many Args") if $#ARGV >= 0;

$mode = "gds2" if ! defined $argmode and $cdl =~ /gds2/;
if (defined($outputfile) and $outputfile ne $cdl) {
    open (STDOUT, ">$outputfile") or die "Cannot open $outputfile : $!";
}

readgraybox;
if ($mode eq "gds2") {
    $sep = "_D_";
    foreach my $c (keys %graybox_gds2) {
        $graybox_list{$c}=1;
    }
}
else {
    $sep = ".";
    foreach my $c (keys %graybox_cadence) {
        $graybox_list{$c}=1;
    }
}

sub usage_exit {
    my $msg = $_[0];
    print STDERR "$msg\n" if defined ($msg);
    print STDERR <<EU;
Usage: flatcdl [options] <cdl-file>
    --cdl=<cdlfile>       : alternate way to specify input
    --cell=<topcell>      : top cell if ambiguous in file
    --graybox=<filename>  : file with list of graybox cells
    --library=<filename>  : file with subckt definitions
    --linelength=<length> : line wrap length
    --mode=<mode>         : (gds2|cadence) automatic if not specified
    --output=<file>       : output filename if not stdout
    --scale=<scale>       : default 1, usually left alone
    --verbose             : extra output
    --stripcolon          : case of extracted netlists
EU
    exit 1;
}

sub read_file {
    my ($file)=@_;
    open (FILE, "<$file") or warn "Cannot open $file";
    print STDERR "Reading $file\n" if $verbose;
    my $subckt="";
    my @pins=();
    my @lines=();
    my @subckts=();
    my %localcalled=();
    my $ln=<FILE>;
    chomp $ln;
    while (<FILE>) {
        chomp;
        while (/^\+/) {
            s/^\+/ /;
            $ln .= $_;
            $_=<FILE>;
            chomp;
        }
        my $lx = $_;
        $_ = $ln;
        $ln = $lx;
        s/\s+\$.*//;
        s: / : :g;
        s/  */ /g;
        s/ *= */=/g;
        s/^  *//;
        next if (/^ *$/);
        s/:\S*/ /g if $stripcolon;
        my @f=split;
        next if (! defined ($f[0]));
        foreach my $f (@f) {
            if ($f =~ /^scale=/i) {
                $f =~ s/scale=//i;
                $scale=$f;
            }
        }
        if (/\.global/i) {
            shift @f;
            foreach my $f (@f) {
                $globals{$f}=1;
            }
        }
        if ($f[0] =~ /^\.subckt$/i ) {
            shift @f;
            $subckt=shift @f;
            push @subcktlist, $subckt;
            undef @pins;
            undef @lines;
            undef @subckts;
            undef %localcalled;
            my %params=();
            if (defined ($subckts{$subckt}) ) {
                warn "Duplicate subckt definition $subckt in $file";
            }
            foreach my $n (0..$#f) {
                last if ($f[$n] =~ /^\$/); # comment
                if ($f[$n] =~ /=/) {
                    my ($param,$val)=split(/=/,$f[$n]);
                    $val =~ s/'//g;
                    if ($val =~ /u$/i) {
                        $val =~ s/u$//i;
                        $val = "($val)*1e-6";
                    }
                    elsif ($val =~ /n$/i) {
                        $val =~ s/n$//i;
                        $val = "($val)*1e-9";
                    }
#                    $param =~ tr/A-Z/a-z/;
                    $params{$param}=$val;
                }
                else {
                    push @pins,$f[$n];
                }
            }
            $subckts{$subckt}->{lines}=[@lines];
            $subckts{$subckt}->{pins}=[@pins];
            $subckts{$subckt}->{subckts}=[@subckts];
            $subckts{$subckt}->{params}={%params};
        }
        elsif ($f[0] =~ /^\.ends/i) {
            $subckts{$subckt}->{pins}=[@pins];
            $subckts{$subckt}->{lines}=[@lines];
            $subckts{$subckt}->{subckts}=[@subckts];
            $subckts{$subckt}->{called}={%localcalled};
            $subckt="";
        }
        elsif ($subckt ne "") {
            if (/^x/i) {
                my $n;
                for ($n = $#f; $f[$n] =~ /=/ and $n > 0; $n--) {
                }
                $called{$f[$n]}++;
                $localcalled{$f[$n]}++;
            }
            push @lines,$_;
            push @subckts,$_ if (/^x/i);
        }
    }
    if ($subckt ne "") {
        $subckts{$subckt}->{pins}=[@pins];
        $subckts{$subckt}->{lines}=[@lines];
        $subckts{$subckt}->{subckts}=[@subckts];
        $subckts{$subckt}->{called}={%localcalled};
    }
    close FILE;
}

read_file ($cdl);
my $need=0;
foreach my $subckt (sort keys %called) {
    if (! defined ($subckts{$subckt})) {
        $need++;
    }
}
if ($need > 0 and defined $library and -f $library) {
    read_file ($library);
}
$need=0;
foreach my $subckt (sort keys %called) {
    if (! defined ($subckts{$subckt})) {
        $need++;
        print STDERR "Error: Need $subckt\n"
            if ! defined $graybox_list{$subckt};
    }
}
if ($need) {
    warn "Warning: Required subcircuits not defined.";
}
my $topcnt=0;
if (!defined ($topcell)) {
    foreach my $subckt (sort keys %subckts) {
        if (!defined ($called{$subckt})) {
            $topcell = $subckt;
            print STDERR "Alleged Topcell $subckt\n" if $verbose;
            $topcnt++;
        }
    }
}
else {
    $topcnt=1;
}
foreach my $subckt (sort keys %subckts) {
    if (!defined ($subckts{$subckt}->{subckts})) {
        print STDERR "Leaf cell $subckt\n" if $verbose;
    }
}
if ($topcnt == 1) {
    print STDERR "Top cell is $topcell\n" if $verbose;
}
elsif ($topcnt) {
    die "too many top cells found, $topcnt";
}
else {
    die "no top cell defined or found";
}

sub setvalue {
    my ($value,%param1,%param2)=@_;
    %param2=() if (!%param2);
    %param1=() if (!%param1);
    my $invalue=$value;
    my $pp;
    foreach my $param (sort {$b cmp $a} keys %param1) {
        $pp = $param1{$param};
        $pp = expandvalue ($pp);
        $value =~ s/$param/$pp/g;
    }
    foreach my $param (sort {$b cmp $a} keys %param2) {
        $pp = $param2{$param};
        $pp = expandvalue ($pp);
        $value =~ s/$param/$pp/g;
    }
    expandvalue ($value);
}

my $lvl=0;
sub writesubckt_no {
    my ($subckt,$inst,$line,@global)=@_;
    my @lines=();
    @lines = @{$subckts{$subckt}->{lines}}
        if defined $subckts{$subckt}->{lines};
    foreach my $line (@lines) {
        writeline ($line);
    }
}

sub writesubckt {
    my ($subckt,$inst,$line,@global)=@_;
    my @pins = @{$subckts{$subckt}->{pins}};
    my @lines = @{$subckts{$subckt}->{lines}};
    my %params = %{$subckts{$subckt}->{params}};
    my $multiplier=1;
    if (defined $line) {
#        print "*$line\n";
        my @l=split(/  */,$line);
        for( my $n=$#l; $n > 0 and $l[$n] =~ /=/; $n--) {
            my ($p,$v)=split(/=/,$l[$n],2);
#            $p =~ tr/A-Z/a-z/;
            $params{$p}=$v;
        }
    }
    my %nets=();
    if (@global and $#global eq $#pins) {
#        print "*S $subckt $inst\n";
        foreach my $n (0..$#pins) {
            $nets{$pins[$n]}=$global[$n];
#            print "*G $pins[$n]=$global[$n]\n";
        }
    }
    my @subckts=();
    @subckts=@{$subckts{$subckt}->{subckts}} if defined $subckts{$subckt}->{subckts};
    foreach my $line (@lines) {
        my $ndx = index($line," \$");
        my $ln=$line;
        my $en="";
        if ($ndx >= 0) {
            $ln = substr($line,0,$ndx);
            $en = substr($line,$ndx+1);
        }
        my $device=substr($line,0,1);
        $device =~ tr/A-Z/a-z/;
        my @f=split(/  */,$ln);
        my $devnr=0;
        $devnr = 5 if $device eq "m";
        $devnr = 3 if $device =~ /^[rdc]/;
        my $n;
        for ($n = $#f; $n > $devnr; $n--) {
            if ($f[$n] =~ /=/) {
                my ($name,$value)=split(/=/,$f[$n]);
                $name =~ tr/A-Z/a-z/;
                if ($device eq "m" or $device eq "d") {
                    $value=setvalue($value,%params);
                    if ($name =~ /^m$/i) {
                    }
                    elsif ($name =~ /^a/i) {
                        my $scalesq=$scale*$scale;
                        $value = "($value)*$scalesq";
                    }
                    else {
                        $value = "($value)*$scale";
                    }
                    eval "\$value=$value";
                    $f[$n] = "$name=$value";
                }
                elsif ($name =~ /^m$/) {
                    $multiplier=$value;
                }
            }
            elsif ($devnr > 0) {
                $f[$n] = setvalue($f[$n],%params);
            }
            else {
                last;
            }
        }
        my $name=$f[$n];
        $name = $f[3],$n=3 if substr($ln, 0, 1) =~ /[rcd]/i;
        $name = $f[5],$n=5 if substr($ln, 0, 1) =~ /m/i;
        for ($n--; $n > 0; $n--) {
            if (defined ($nets{$f[$n]})) {
                $f[$n] = $nets{$f[$n]};
            }
            elsif (! isglobal($f[$n])) {
                $f[$n] = "$inst${sep}$f[$n]" if $inst ne "";
            }
        }
        $line = "@f";
        $line .= " $en" if $en ne "";
        if (!($line =~ /^x/i)) {
            writeline $line,$inst;
        }
    }
    if (1) {
        foreach my $line (@lines) {
            my $ndx = index($line," \$");
            my $ln=$line;
            my $en="";
            if ($ndx >= 0) {
                $ln = substr($line,0,$ndx);
                $en = substr($line,$ndx+1);
            }
            my $device=substr($line,0,1);
            $device =~ tr/A-Z/a-z/;
            my @f=split(/  */,$ln);
            my $n;
            my $devnr=0;
            $devnr = 5 if $device eq "m";
            $devnr = 3 if $device =~ /^[rdc]/;
            for ($n = $#f; $n > $devnr; $n--) {
                if ($f[$n] =~ /=/) {
                    my ($name,$value)=split(/=/,$f[$n]);
                    $name =~ tr/A-Z/a-z/;
                    if ($device eq "m" or $device eq "d") {
                        $value=setvalue($value,%params);
                        if ($name =~ /^m$/i) {
                        }
                        elsif ($name =~ /^a/i) {
                            my $scalesq=$scale*$scale;
                            $value = "($value)*$scalesq";
                        }
                        else {
                            $value = "($value)*$scale";
                        }
                        eval "\$value=$value";
                        $f[$n] = "$name=$value";
                    }
                    elsif ($name =~ /^m$/) {
                        $multiplier=$value;
                    }
                }
                elsif ($devnr > 0) {
                    $f[$n] = setvalue($f[$n],%params);
                }
                else {
                    last;
                }
            }
            my $name=$f[$n];
            $name = $f[3],$n=3 if substr($ln, 0, 1) =~ /[rcd]/i;
            $name = $f[5],$n=5 if substr($ln, 0, 1) =~ /m/i;
            $line = "@f";
            $line .= " $en" if $en ne "";
            if ($line =~ /^x/i) {
                my $i=$f[0];
                my $n;
                for($n = $#f; $f[$n] =~ /=/ and $n >= 0; $n--) {
                }
                $name=$f[$n];
                my @glob=();
                for ($n--; $n > 0; $n--) {
                    $glob[$n-1]=$f[$n];
                }
                $i =~ s/^x//i;
                $i = "$inst${sep}$i" if $inst ne "";
                if ($graybox_list{$name}) {
                    $"=" ";
                    my @f=split(/ /,$line);
                    shift @f;
                    unshift @f, $i;
                    writeline "X@f", "";
                }
                else {
                    writesubckt($name,$i,$line,@glob);
                }
            }
        }
    }
    $lvl--;
}

my %needed;
sub findneeded {
    my ($cell) = @_;
    if (defined ($subckts{$cell})) {
        my %called=%{$subckts{$cell}->{called}};
        foreach my $c (keys %called) {
            findneeded($c) if ! $needed{$c};
            $needed{$c}++;
        }
    }
}

sub writeneeded {
    foreach my $subckt (@subcktlist) {
        if ($needed{$subckt}) {
            writeline ".SUBCKT $subckt @{$subckts{$subckt}->{pins}}","";
            writesubckt_no "$subckt", "";
            writeline ".ENDS", "";
        }
    }
}

my @pins=();
if (defined $subckts{$topcell}->{pins}) {
    @pins=@{$subckts{$topcell}->{pins}};
}
else {
    print STDERR "Warning: $topcell has no pins!\n";
}
foreach my $g (keys %globals) {
    print ".global $g\n";
}
print "\n";
foreach my $c (keys %graybox_list) {
    findneeded($c);
    $needed{$c}=1;
}
writeneeded;
writeline ".SUBCKT $topcell @pins","";
writesubckt $topcell,"";
writeline ".ENDS";
