#!/usr/intel/bin/perl -l
# AAG
# $Id: fixcdl.pl,v 1.1 2012/10/26 16:34:44 aagrey Exp $
# $DateTime$

my $resistors=<<ER;
.SUBCKT rm1s PLUS MINUS w=w l=l
ENDS rm1s

.SUBCKT rm1w PLUS MINUS w=w l=l
ENDS rm1w

.SUBCKT rm2l PLUS MINUS w=w l=l
ENDS rm2l

.SUBCKT rm2w PLUS MINUS w=w l=l
ENDS rm2w
ER

my %resistors=();

my $resistorsdone=0;

sub evalit {
    my ($value)=@_;
    $value =~ s/'//g;
    $value =~ s/(\d)u/$1*1e-6/ig;
    $value =~ s/(\d)n/$1*1e-9/ig;
    $value =~ s/(\d)p/$1*1e-12/ig;
    $value =~ s/(\d)f/$1*1e-15/ig;
    eval "\$value=$value" if ($value !~ /[a-z][a-z]/i) and ($value !~ /\*[a-z]/i);
    $value;
}

sub evaluate {
    my ($value)=@_;
    my ($n,$v)=split(/=/, $value);
    if (defined ($v)) {
        return "$n=".evalit($v);
    }
    return evalit($n);
}

sub sortNatural {
    return $a cmp $b if $a !~ /\d/ and $b !~ /\d/;
    my $sa=$a;
    $sa =~ s/(\d+)/_/;
    my $na=$1;
    $na = -1 if ! defined $na;
    my $sb=$b;
    $sb =~ s/(\d+)/_/;
    my $nb=$1;
    $nb = -1 if ! defined $nb;
    return $sa cmp $sb if $sa ne $sb;
    $na - $nb;
}

#tcsort stolen from http://ppt.perl.org/commands/tsort/index.html
# who stole it from Jon Bentley (I<More Programming Pearls>, pp. 20-23),
# who, in turn, stole it from Don Knuth
# (I<Art of Computer Programming, volume 1: Fundamental Algorithms>,
# Section 2.2.3)

sub tcsort {
    my @inlist=@_;
    my %pairs;	# all pairs ($l, $r)
    my %npred;	# number of predecessors
    my %succ;	# list of successors

    foreach my $element (@inlist) {
        $_=$element;
        my ($l, $r) = my @l = split;
        next unless @l == 2;
        next if defined $pairs{$l}{$r};
        $pairs{$l}{$r}++;
        $npred {$l} += 0;
        ++$npred{$r};
        push @{$succ{$l}}, $r;
    }

    # create a list of nodes without predecessors
    my @list = grep {!$npred{$_}} keys %npred;

    my @out=();
    while (@list) {
        $_ = pop @list;
        unshift @out,$_; # order backwards from original (AAG)
        foreach my $child (@{$succ{$_}}) {
            if ($opt_b) {	# breadth-first
                unshift @list, $child unless --$npred{$child};
            } else {	# depth-first (default)
                push @list, $child unless --$npred{$child};
            }

        }
    }
    @out;
}

# purpose : to modify Avago netlist to agree with Fulcrum/Intel conventions
my $outfile = $ARGV[1];
$outfile = "$ARGV[0].mod" if ! defined $outfile;
my %portmap=();
open (P, "<$ARGV[0]");
my $ln="";
while (<P>) {
    chomp;
#    s/\[//g;
#    s/\]//g;
#    s/\//_/g;
    s/</[/g;
    if (/^\+/) {
        s/^\+/ /;
        $ln .= $_;
        next;
    }
    my $lx=$_;
    $_=$ln;
    $ln=$lx;
    next if /^\*/;
    s/\s+/ /g;
    s/ \$.*//;
    s/\s+/ /g;
    if (/^\.subckt/i or /^\.ends/i) {
        my @f = split;
        for (my $n = 0; $n <= $#f; $n++) {
###            $f[$n] =~ tr/a-z/A-Z/ if $n != 1;
            $f[$n] = "Vdd" if $f[$n] =~ /^VDD$/i or $f[$n] =~ /\.VDD$/i;
            $f[$n] = "GND" if $f[$n] =~ /\.GND$/i or $f[$n] =~ /^VSS$/i;
        }
        my $pre=shift @f;
        while ($f[$#f] =~ /=/) {
            pop @f;
        }
        my $subckt = shift @f;
        my $ports=join(" ", @f);
        my $sortedports=join(" ", sort sortNatural @f);
        my $n=0;
        my %map=();
        foreach my $f (sort sortNatural @f) {
            $map{$f}=$n++;
        }
        $n=0;
        foreach my $f (@f) {
            $portmap{$subckt}->{$n}=$map{$f};
            $n++;
        }
    }
    elsif (/^m/i) {
        my @f = split;
        for (my $n = 0; $n <= 4 ; $n++) {
###            $f[$n] =~ tr/a-z/A-Z/;
            $f[$n] = "Vdd" if $f[$n] =~ /^VDD$/i or $f[$n] =~ /\.VDD$/i;
            $f[$n] = "GND" if $f[$n] =~ /\.GND$/i or $f[$n] =~ /^VSS$/i;
            $f[$n] =~ s/\./_/g;
        }
#        $f[5] =~ s/ch/mos/;
        for (my $n = 6; $n <= $#f; $n++) {
            if ($f[$n] =~ /^[lw]=/i) {
                $f[$n] = evaluate($f[$n]);
            }
            elsif ($f[$n] =~ /^m/i) {
                $f[$n] = "";
            }
        }
    }
    elsif (/^[rcd]/i) {
#        tr/A-Z/a-z/;
        my @f = split;
        for (my $n = 0; $n <= 2 ; $n++) {
###            $f[$n] =~ tr/a-z/A-Z/;
            $f[$n] = "Vdd" if $f[$n] =~ /^VDD$/i or $f[$n] =~ /\.VDD$/i;
            $f[$n] = "GND" if $f[$n] =~ /\.GND$/i or $f[$n] =~ /^VSS$/i;
            $f[$n] =~ s/\./_/g;
        }
        for (my $n = 3; $n <= $#f; $n++) {
            if ($f[$n] =~ /=/) {
                $f[$n] = evaluate($f[$n]);
            }
            elsif ($f[$n] =~ /^m/i) {
                $f[$n] = "";
            }
        }
    }
    elsif (/^x/i) {
        my @f = split;
        for (my $n = 0; $n < $#f ; $n++) {
###            $f[$n] =~ tr/a-z/A-Z/;
            if ($n == 3 and $f[$n] =~ /^R/ and $f[$n+1] =~ /=/) {
###                $f[$n] =~ tr/A-Z/a-z/;
                $resistors{$f[$n]}=1;
            }
            $f[$n] = "Vdd" if $f[$n] =~ /^VDD$/i or $f[$n] =~ /\.VDD$/i;
            $f[$n] = "GND" if $f[$n] =~ /\.GND$/i or $f[$n] =~ /^VSS$/i;
            if ($f[$n] =~ /=/) {
###                $f[$n] =~ tr/A-Z/a-z/;
                $f[$n] = evaluate($f[$n]);
            }
            else {
                $f[$n] =~ s/\./_/g;
            }
        }
        my $n = $#f;
        if ($f[$n] =~ /=/) {
            $f[$n] = evaluate($f[$n]);
###            $f[$n] =~ tr/A-Z/a-z/;
        }
    }
}
close P;
open (Q, ">$outfile");
select Q;
open (P, "<$ARGV[0]");
$ln="";
while (<P>) {
    chomp;
#    s/\[//g;
#    s/\]//g;
#    s/\//_/g;
    s/</[/g;
    s/>/]/g;
    s/\s+/ /g;
    s/\s$//;
    if (/^\+/) {
        s/^\+/ /;
        $ln .= $_;
        next;
    }
    my $lx=$_;
    $_=$ln;
    $ln=$lx;
    next if /^\*/;
    s/\s+/ /g;
    s/ \$.*//;
    if (/^\.subckt/i or /^\.ends/i) {
        if (! $resistorsdone ) {
            foreach my $r (sort keys %resistors) {
                print ".SUBCKT $r PLUS MINUS w=w l=l\n.ENDS $r\n";
            }
            $resistorsdone=1;
        }
        my @f = split;
        for (my $n = 0; $n <= $#f; $n++) {
###            $f[$n] =~ tr/a-z/A-Z/ if $n != 1;
            $f[$n] = "Vdd" if $f[$n] =~ /^VDD$/i or $f[$n] =~ /\.VDD$/i;
            $f[$n] = "GND" if $f[$n] =~ /\.GND$/i or $f[$n] =~ /^VSS$/i;
        }
        my $pre=shift @f;
        my $subckt = shift @f;
        my @ports=@f;
        my @suffix=();
        while ($ports[$#ports] =~ /=/) {
            push @suffix, pop @ports;
        }
        my $ports=join(" ", @ports);
        my $sortedports=join(" ", ((sort sortNatural @ports),@suffix));
        if ($pre =~ /subckt/i) {
            print "$pre $subckt $sortedports";
        } else {
            print join(" ", ($pre, $subckt, @f)) 
        }
    }
    elsif (/^m/i) {
        my @f = split;
        for (my $n = 0; $n <= 4 ; $n++) {
###            $f[$n] =~ tr/a-z/A-Z/;
            $f[$n] = "Vdd" if $f[$n] =~ /^VDD$/i or $f[$n] =~ /\.VDD$/i;
            $f[$n] = "GND" if $f[$n] =~ /\.GND$/i or $f[$n] =~ /^VSS$/i;
            $f[$n] =~ s/\./_/g;
        }
#        $f[5] =~ s/ch/mos/;
        for (my $n = 6; $n <= $#f; $n++) {
            if ($f[$n] =~ /^[lw]=/i) {
                $f[$n] = evaluate($f[$n]);
            }
            elsif ($f[$n] =~ /^m/i) {
                $f[$n] = "";
            }
        }
        print join(" ", @f);
    }
    elsif (/^[rcd]/i) {
#        tr/A-Z/a-z/;
        my @f = split;
        for (my $n = 0; $n <= 2 ; $n++) {
###            $f[$n] =~ tr/a-z/A-Z/;
            $f[$n] = "Vdd" if $f[$n] =~ /^VDD$/i or $f[$n] =~ /\.VDD$/i;
            $f[$n] = "GND" if $f[$n] =~ /\.GND$/i or $f[$n] =~ /^VSS$/i;
            $f[$n] =~ s/\./_/g;
        }
        for (my $n = 3; $n <= $#f; $n++) {
            if ($f[$n] =~ /=/) {
                $f[$n] = evaluate($f[$n]);
            }
            elsif ($f[$n] =~ /^m/i) {
                $f[$n] = "";
            }
        }
        print join(" ", @f);
    }
    elsif (/^x/i) {
        my @f = split;
        my $subcell=undef;
        for (my $n = 0; $n < $#f ; $n++) {
###            $f[$n] =~ tr/a-z/A-Z/;
            if ($n == 3 and $f[$n] =~ /^R/ and $f[$n+1] =~ /=/) {
                $f[$n] =~ tr/A-Z/a-z/;
            }
            $f[$n] = "Vdd" if $f[$n] =~ /^VDD$/i or $f[$n] =~ /\.VDD$/i;
            $f[$n] = "GND" if $f[$n] =~ /\.GND$/i or $f[$n] =~ /^VSS$/i;
            if ($f[$n] =~ /=/) {
                $f[$n] = evaluate($f[$n]);
###                $f[$n] =~ tr/A-Z/a-z/;
            }
            else {
                $f[$n] =~ s/\./_/g;
            }
        }
        my $n = $#f;
        while ($f[$n] =~ /=/) {
                $f[$n] = evaluate($f[$n]);
###            $f[$n] =~ tr/A-Z/a-z/;
            $n--;
        }
        $subcell=$f[$n];
        if (defined $portmap{$subcell}) {
            my $inst=shift @f;
            my @x=();
            my @x=@f;
            my $n=0;
            foreach my $f (@f) {
                last if $f eq "$subcell";
                $x[$portmap{$subcell}->{$n}] = $f;
                $n++;
            }
            @f=($inst, @x);
        }
        else {
            print STDERR "Portmap for $subcell not defined." if $subcell !~ /^rm/;
        }
        print join(" ", @f);
    }
    else {
        print;
    }
}
$_=$ln;
s/\s+/ /g;
if (/^\.ends/i) {
    print;
}
