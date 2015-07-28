#!/usr/intel/bin/perl -w

#
#  Spice to Spice processing.  Optionally rename arbitrary node names
#  to hex ascii, pick a canonical node, or probe nodes.
#
# Copyright 2012 Intel
# Authors: Andrew Lines

# find relevant packaged tools and libraries
BEGIN {
    $lve_root = $0;
    $lve_root =~ s:/[^/]*$::;
    $lve_root =~ s:/[^/]*$::;
    @INC = ("$lve_root/lib/perl", @INC);
}
use FileHandle;
use IPC::Open2;
use POSIX;
use rename;

# Output record separator
$" = " ";

#
# parse arguments
#
my $top = "";   # top cell name
my $icf = 0;    # reverse translate output of ICF extract flow
my $rename = 0; # translate to ascii
my $flatten_top = 0; # flatten top cell
my $probe_top_ports = 0; # probe ports of top cell
my $probe_ports = 0;     # probe all ports of SUBCKT's
my $probe_gates = 0;     # probe base node names if they gate transistors
my $skip_parms = "x,y,llx,lly,urx,ury,si_l,si_w,size0,mulid0,angle"; # hack

# skip renaming estimated extraction primitives
my %primitive;
$primitive{"wirecap"} = 1;
$primitive{"wireres"} = 1;

# used to create an alias to one subnet
my %first_subnet;

# command line arguments
while (defined $ARGV[0] && $ARGV[0] =~ /^--(.*)=(.*)/) {
    if    ($1 eq "top") { $top = $2; }
    elsif ($1 eq "icf") { $icf = $2; }
    elsif ($1 eq "rename") { $rename = $2; }
    elsif ($1 eq "flatten-top") { $flatten_top = $2; }
    elsif ($1 eq "probe-top-ports") { $probe_top_ports = $2; }
    elsif ($1 eq "probe-ports") { $probe_ports = $2; }
    elsif ($1 eq "probe-gates") { $probe_gates = $2; }
    elsif ($1 eq "skip-parms") { $skip_parms = $2; }
    else { usage(); }
    shift @ARGV;
}
@ARGV == 2 or usage();
$f_in  = "$ARGV[0]";
$f_out = "$ARGV[1]";

open IN,  "<$f_in"  or die "Can't open '$f_in' for reading.\n";
open OUT, ">$f_out" or die "Can't open '$f_out' for writing.\n";

# skip unsupported transistor parameters
my %skip_parms;
foreach $parm (split(",",$skip_parms)) {
    $skip_parms{$parm} = 1;
}

#
# Save options in header
#

print OUT "* rename_spice --top=$top --icf=$icf --rename=$rename --flatten-top=$flatten_top --probe-top-ports=$probe_ports --probe-ports=$probe_ports --probe-gates=$probe_gates --skip-parms=$skip_parms $f_in $f_out\n";

#
# Do linewise translation of SPICE to SPICE
#

my $line = <IN>;
$. = 0;
my $cell = "";
while ($line) {
    $next_line = <IN>;
    while (defined $next_line && $next_line =~ s/^\+/ /) {
        chomp $line;
        $line .= $next_line;
        $next_line = <IN>;
    }
    my $full = $line;
    if ($line =~ s/^\.SUBCKT\s+//i) {

        #
        # Begin Subcircuit Definition
        #

        my @parameters = ();
        my @nodes = ();

        $line =~ s/(\S+)\s+//;
        $cell = $type = $1;
        my $flat_top = $flatten_top && ($cell eq $top);
        unless ($primitive{$type}) { my_list_to_spice($type); }
        print OUT "\n.SUBCKT $type" unless ($flat_top);
        while ($line =~ s/(\S+)\s*//) {
            $arg = $1;
            if ($arg =~ /(\S+)=(\S+)/) {
                # Parameter and default.
                # Default value saved in subc_arg will be used 
                # if an instance fails to provide all parameters
                print OUT " $1=$2" unless ($flat_top);
            } else {
                # Node argument
                my $node = $1;
                node_names($node);
                push @nodes, $node;
                print OUT " $node" unless ($flat_top);
            }
        }
        print OUT "\n" unless ($flat_top);

        if ($probe_ports && !($cell =~ /^stack/ || $cell =~ /^gate/) ||
            $probe_top_ports && $cell eq $top) {
            foreach my $node (@nodes) {
                print OUT ".probe v($node)\n";
            }
        }
            
    } elsif ($line =~ s/^\.ENDS//i) {

        %first_subnet = ();
        my $flat_top = $flatten_top && ($cell eq $top);
        print OUT ".ENDS\n" unless ($flat_top);

    } elsif ($line =~ s/^M//i) {

        #
        # MOSFET
        #

        my %parameters = ();
        my @nodes = ();

        $line =~s/^(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)//
            or error_msg("Missing arguments in mosfet: $full");
        my $name   = $1;
        my $type   = $6;
        my $drain  = $2;
        my $gate   = $3;
        my $source = $4;
        my $bulk   = $5;

        # create parameter list in aspice order
        while ($line =~ s/(\S+)=//) {
            my $p = $1;
            my $v;
            $line =~ s/(\S+)\s+//;
            $v = $1;
            $parameters{$p} = $v unless ($skip_parms{$p});
        }

        # emit
        my_list_to_spice($name);
        node_names($drain,$gate,$source,$bulk);
        print OUT "M${name} $drain $gate $source $bulk $type";
        foreach my $parm (keys %parameters) {
            print OUT " $parm=$parameters{$parm}";
        }
        print OUT "\n";

        # probe base name of gate
        if ($probe_gates) {
            my $n = $gate;
            if ($gate =~ /(.*):(.*)/ ) { $n = $1; }
            print OUT ".probe v($n)\n";
        }

    } elsif ($line =~ s/^X//i) {

        #
        # Call to Subcircuit
        #

        my %parms = ();
        my @parms = ();
        my @nodes = ();
        $line =~ s/^(\S+)\s*// or error_msg("Call lacks instance name: $full");
        $name = $1;
        while ($line =~ s/(\S+)=(\S+)\s*//) {
            $parms{$1}=$2;
            push @parms, $1;
        }
        $line =~ s/(\S+)\s*$// or error_msg("Call lacks SUBCKT name: $full");
        $type = $1;
        @nodes = split /\s+/, $line;
        my $slash = pop @nodes; # FIXME: find better way to remove last "/"
        if ($slash ne "/") { # slash is optional before subcircuit name
            push @nodes, $slash;
        }
        my_list_to_spice($name);
        unless ($primitive{$type}) { my_list_to_spice($type); }
        node_names(@nodes);
        print OUT "X$name @nodes $type";
        foreach my $parm (@parms) {
            print OUT " $parm=$parms{$parm}";
        }
        print OUT "\n";

    } elsif ($line =~ s/^C//i) {
	
        #
        # Capacitor
        #

        $line =~ s/^(\S*)\s*//;
        $name = $1;
        $line =~ s/^(\S+)\s*//
            or error_msg("Capacitor has no positive terminal: $full");
        $pos = "$1";
        $line =~ s/^(\S+)\s*//
            or error_msg("Capacitor has no negative terminal: $full");
        $neg = "$1";
        my_list_to_spice($name);
        node_names($pos,$neg);
        print OUT "C$name $pos $neg $line";

    } elsif ($line =~ s/^R//i) {

        #
        # Resistor
        #

        $line =~ s/^(\S*)\s*//;
        $name = $1;
        $line =~ s/^(\S+)\s*//
            or error_msg("Resistor has no positive terminal: $full");
        $pos = "$1";
        $line =~ s/^(\S+)\s*//
            or error_msg("Resistor has no negative terminal: $full");
        $neg = "$1";
        my_list_to_spice($name);
        node_names($pos,$neg);
        print OUT "R$name $pos $neg $line";

    } elsif ($line =~ m/^\*/) {
        # comment line, do nothing
    } elsif ($line =~ s/^\s*\n//i ) {
        # empty line with space, do nothing
    } else {
        # unknown line, error
        error_msg("Unknown line type: $line");
    }
    $line = $next_line;
}

# usage banner
sub usage {
    die "Usage: rename_spice\n" .
        " [--icf=$icf] [--rename=$rename]\n" .
        " [--top=$top] [--flatten-top=$flatten_top]\n" .
        " [--probe-top-ports=$probe_ports] [--probe-ports=$probe_ports] [--probe-gates=$probe_gates]\n" .
        " [--skip-parms=$skip_parms]\n" .
        " <in> <out>\n";
}

# reverse translation of a list of nodes from ICF's StarRC flow
sub my_from_spice {
    my ($node) = @_;
    $node =~ s/\]\[/,/g;
    $node =~ s/\|/:/g;
    my $new = "";
    while ($node =~ s/^X([^\/]*)\///g) {
        $new .= $1 . "/";
    }
    $new .= $node;
    return $new;
}

# rename a list of nodes
sub my_list_to_spice {
    if ($icf) {
        for (my $i=0; $i<@_; $i++) {
            $_[$i] = my_from_spice($_[$i]);
        }
    }
    if ($rename) { list_to_spice(@_); }
}

# convert node names, renaming the first subnet to be the real node
sub node_names {
    for (my $i=0; $i<@_; $i++) {
        my $node = $_[$i];
        $node = my_from_spice($_[$i]) if ($icf);
        if ($node =~ /(.*):(.*)/ ) { # has base:subnet format
            my $base = $1;
            my $subnet = $2;
            if (!defined($first_subnet{$base})) {
                $first_subnet{$base} = $subnet;
            }
            if ($first_subnet{$base} eq $subnet) {
                $node = $base; # strip off the subnet, make this the canonical node
            }
        } else { # not a subnet
            if (!defined($first_subnet{$node})) {
                $first_subnet{$node} = "";
            } elsif (!($first_subnet{$node} eq "")) {
                $node = "$node:"; # some other subnet got to be the base
            }
        }
        $_[$i] = $node;
    }
    if ($rename) { list_to_spice(@_); }
}

# report a fatal error
sub error_msg {
    unlink($f_out);
    die "$0: $f_in, line $.: $_[0]\n";
}

# check if a string is a legal real number
sub is_numeric {
    my ($parm) = @_;
    if ($parm =~ /^[-+]?[\d]+(\.[\d]+)?([eE]?[-+]?[\d]+)?$/ ) { return 1; }
    else { return 0; }
}
