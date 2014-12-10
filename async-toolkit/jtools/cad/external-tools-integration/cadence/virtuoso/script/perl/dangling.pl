#!/usr/intel/bin/perl -l
# AAG
# $Id: dangling,v 1.5 2006/04/21 04:10:44 aubrey Exp aubrey $
# $DateTime$

use strict;
use IPC::Open2;

my $subckt="";
my %definitions=();

my %graybox=();
my %nodecnt=();
my %node2inst=();
my %inst2cell=();

my $drd;
my $dwr;
my $dpid=0;

# for graybox list, which uses cast names
sub cast2cadence {
    my ($cell)=@_;
    if (! $drd ) {
        $dpid=open2 ($drd,$dwr,
            "fulcrum rename --from=cast --to=cadence --type=cell");
    }
    print $dwr "$cell";
    $cell=<$drd>;
    chomp $cell;
    $cell;
}

# does not work without this list
open (P, "<$ARGV[1]");
while (<P>) {
    chomp;
    s/ //g;
    $graybox{$_}=1;
}
close P;
if ($dwr) {
    close $dwr or die;
    close $drd or die;
}
waitpid $dpid,0 if $dpid;


my %global=(
    "GND" => 1,
    "Vdd" => 1,
    "0" => 1,
);

my %subckts=();
my %resistors=();
my %xrefs=();
my $top;

sub parsecdl {
    my ($file)=@_;
    local (*P,$_);
    open (P, "<$file");
    my $ln="";
    my $subckt="";
    my @resistors=();
    my @xrefs=();
    while (<P>) {
        chomp;
        if (/^\+/) {
            s/^\+/ /;
            $ln .= $_;
            next;
        }
        my $x = $_;
        $_=$ln;
        $ln = $x;
        s/  */ /g;
        if (/^\.subckt /i) {
            # collect subckt line
            my ($s,$n,@f)=split;
            $subckt=$n;
            $top=$n;
            my @pins=();
            @resistors=();
            @xrefs=();
            foreach my $f (@f) {
                if ($f =~ /=/) {
                    last;
                }
                $f =~ s/:.*//;
                push @pins,$f;
            }
            $subckts{$subckt}=[@pins];
        }
        elsif (/^\.ends/i) {
            # reset all the temp arrays and save
            $resistors{$subckt}=[@resistors];
            $xrefs{$subckt}=[@xrefs];
            $subckt="";
            @resistors=();
            @xrefs=();
        }
        elsif (/^r/i) {
            # resistors may connect same nets
            my @f=split;
            $f[1] =~ s/:.*//;
            $f[2] =~ s/:.*//;
            push @resistors, "$f[0] $f[1] $f[2] $f[3] $f[4]";
        }
        elsif (/^c/i) { # ignore capacitors for now
        }
        elsif (/^x/i) {
            # decode contained instances
            s/ \/ / /g;
            if (/=/) {
                s/=.*//;
                my $i=rindex($_," ");
                $_=substr($_,0,$i) if ($i > 0);
            }
            my @f=split;
            # dump the :'s but in cdl this is silly, really
            foreach my $f (0..$#f) {
                $f[$f] =~ s/:.*//;
            }
            # one array element for each subcell
            push @xrefs, "@f";
        }
        elsif (/^ *[dm]/i) {
        }
        elsif (/^ *\*/) {
        }
        elsif (/^$/) {
        }
        else {
            print STDERR "Warning: Unrecognized element $_";
        }
    }
    # for last subckt if not decoded above
    if( @resistors or @xrefs) {
        $resistors{$subckt}=[@resistors];
        $xrefs{$subckt}=[@xrefs];
    }
}

# called recursively to generate names and port lists
sub getnames {
    my ($cell,$inst,@pins)=@_;
    $inst .= '.' if $inst ne "";
    $inst2cell{$inst}=$cell;
    my @resistors=();
    my @xrefs=();
    my @nodes=();
    @nodes=@{$subckts{$cell}} if defined $subckts{$cell};
    my %nodes=();
    my $n = 0;
    foreach my $node (@nodes) {
        $nodes{$node}=$n;
        $n++;
    }
    @resistors=@{$resistors{$cell}} if defined $resistors{$cell};
    @xrefs=@{$xrefs{$cell}} if defined $xrefs{$cell};
    foreach my $xref (@xrefs) {
        my @f=split(/ /,$xref);
        my @spins=();
        my $sinst = $f[0];
        $sinst =~ s/^X//;
        $sinst = "${inst}$sinst";
        $inst2cell{$sinst}=$f[$#f];
        foreach my $n (1..$#f-1) {
            my @inodes;
            @inodes=@{$subckts{$f[$#f]}} if defined $subckts{$f[$#f]};
            if (defined $nodes{$f[$n]}) {
                push @spins,$pins[$nodes{$f[$n]}];
                $nodecnt{$pins[$nodes{$f[$n]}]}++;
                $nodecnt{$pins[$nodes{$f[$n]}]}++;
                $node2inst{$pins[$nodes{$f[$n]}]} = "$sinst:$inodes[$n-1]";
            }
            else {
                push @spins, "${inst}$f[$n]";
                $nodecnt{"${inst}$f[$n]"}++;
                $node2inst{"${inst}$f[$n]"} = "$sinst:$inodes[$n-1]";
            }
        }
        # descend if not a graybox cell
        getnames ($f[$#f],$sinst,@spins) if ( ! $graybox{$f[$#f]});
    }
    foreach my $resistor (@resistors) {
        my @f=split(/ /,$resistor);
        my $tr=$f[0];
        if ($f[1] ne $f[2]) {
            $nodecnt{"${inst}$f[1]"}++;
            $nodecnt{"${inst}$f[2]"}++;
        }
    }
}

# read the netlist entirely
parsecdl ($ARGV[0]);
# recursively find all of the names
getnames ($top, "",@{$subckts{$top}});

# just report the results
open(P, ">$ARGV[2]");
print( P "danglingNodeList=nil" ); 
foreach my $node (sort keys %nodecnt) {
    my ($inst,$pin)=split(/:/,$node2inst{$node});
    print( P "danglingNodeList=cons( list(\"$node\" \"$inst\" \"$pin\" \"$inst2cell{$inst}\") danglingNodeList)") if $nodecnt{$node}==1;
}
close(P);
