#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

# not a true verilog parser. Makes assumptions about commas
# not being inside of a commented name. Makes assumptions about
# certain names with square brackets as well as 'curly' brackets.
# does not handle structures more complex than arrays.

use strict;

use Getopt::Long;
use IPC::Open2;
use IPC::Open3;
use IO::Select;

my $apid = open2(\*ARD, \*AWR, "rename --type=all 2>/dev/null");

# command line args
my $refineparent="PRIMITIVE";
my $progress=0;
my @addimport=("lib.synchronous.schannel");
# maybe not
@addimport=();
my $bindfile="";
my $castdir="";
my $castpath="";
my $specdir="";
my $library="";
my $subtype;
my $subtypedefined=0;
my $verilog_block="rtl";
my $propagate=1;
my $cast_and_spec=0;
my $wrap=0; # used to wrap the top level for canonicalization issues.
my $argwrap=0;
my $cdc_mode=0;
my $debug=0;
# mainly for non-verilog power ports
my @extra_ports=();
my @extra_implied=();

my $externalfile="";
my $outputfile="";
my $verbose=0;
my $quiet=0;
my $argparent;
my $p4client;
my $p4=0;
my $max_heap_size="512M";
my $extra_refine="";
my $portbase="";

# binding file related
my %bind=();           # result of reading vsbind file
my %bindinstlist=();   # these three track bind signals
my %bindnetlist=();
my %bindcelllist=();

# results from parse:
my %portdirection=();  # key = "module port", value = =|-|+-
my %portmap=();        # key = module, subkey is the port name, value in port order index (n)
my %moduleports=();    # key = module, value is array of [aref,portname]
my %wiretypes=();      # key = module, value is array of [aref,wirename] (not including ports)
my %inst=();           # key = module, value is array of instance names
my %assign=();         # key = module, value is array of assign statements (e.g. a=b)
                       # note itype is "module:inst"
my %signal=();         # key is itype, subkey is pin name, value is net connected to pin
my %pins=();           # key is itype, key is array of [pin=net] (same info as above, but ordered)
my %type=();           # key is itype, value is cell type of instance

                       # from generate_port_mapping of top module
my %verilog=();        # verilog name of top module cast port
my %castname=();       # cast name of top module verilog port

# from getports
my %castfound=();      # key=cell, array of cast lines representing the ports
my %ports=();          # key=cell, array of [ctype,name,dir]
my %implied=();        # key=cell, array of [ctype,name,dir]
my @aliasedports=();   # only for top cell, all aliases of each port
my %cpdirection=();    # key = cell, subkey=port, value=direction (-|+)
my %ecanonical=();     # key = cell, subkey=port, value=canonical port name
my %eicount=();        # number of inputs on canonical port
my %rcanonical=();     # reverse canonical lookup
my %revlookup=();      # convert .d[#] to .#
my %realchannel=();    # finds the root part of name of channel
my %realport=();       # list of ports actually connected to something, key=cell
my %channeldefs=();    # sub nodes of channels derived from cast_query, merge of all cell types
                       # for subcells from generate_port_mapping
my %v2c=();            # key is cell, subkey is verilog name, value is cast name
my %c2v=();            # key is cell, subkey is cast name, value is verilog name

my @module=();
my %nodes=();
my $portlist=1;
my $instlist=0;
my %specialnodes=();
my $top = "";
my $porttopcell="";
my $debugfile = "/dev/null";
my $warning=0;
my $warnings=0;
my $flat=0;
my %external=();       # list of external cells with array ports
my $profile=0;
my $errors=0;

my $mname="xxx";

sub ignore_cell {
    my ($cell)=@_;
    if(($cell =~ /_TieDown$/)
        or ($cell =~ /GNDShieldTieOff$/) or ($cell =~ /POWER_GRID_TIEOFF/)) {
        return 1;
    }
    0;
}

my $cast_server_pid=0;
my $SEL;

sub start_cast_server {
    my $cmd="cast_file_server --max-heap-size=$max_heap_size --cast-path=$castpath";
    $cast_server_pid=open3(*CCIn, *CCOut, *CCErr, $cmd);
    die "Cannot start cast server" if ! $cast_server_pid;
    $SEL = new IO::Select;
    $SEL->add(*CCOut);
    $SEL->add(*CCErr);
    print STDERR "Start cast server $cmd" if $verbose;
}

sub call_cast_server {
    my $cmd = shift;
    my $showerrs=shift;
    $showerrs=1 if ! defined $showerrs;
    start_cast_server if $cast_server_pid==0;
    my $done=0;
    my @ready;
    my $out_data;
    my $err_line="";
    my $return_code=0;
    my $err_data;
    my $n=0;
    print STDERR "CastServer: $cmd" if $debug;
    print CCIn "- - com.avlsi.tools.$cmd";
    my $return="";
    while (!$done and (@ready = $SEL->can_read())) {
        foreach my $fh (@ready) {
            if ($fh eq *CCOut) {
                if (!sysread CCOut, $out_data, 4096) {
                    print STDERR "Warning: Java cast server closed stdout.\n";
                    $warnings++;
                    $return_code = -2;
                }
                $return .= $out_data;
            }
            elsif ($fh eq *CCErr) {
                if (!sysread CCErr, $err_data, 80) {
                    print STDERR "Warning: Java cast server closed stderr.\n";
                    $warnings++;
                    $return_code = -2;
                    $done = 1;
                }
                my @lines = split /\n/, $err_data;
                my $leftover = pop @lines if ($err_data !~ /\n$/);
                my $starts_with_newline = ($err_data =~ /^\n/);
                while (@lines or $err_line ne "" and $err_data eq "\n") {
                    $err_line .= shift @lines unless ($starts_with_newline);
                    if ($err_line !~ /^CastFileServer:/) {
                        if ($showerrs and ! ($err_line =~ /^\s*$/)) {
                            print STDERR $cmd if $verbose;
                            print STDERR "Errline: $err_line";
                            $errors++;
                        }
                    }
                    elsif ($err_line eq "CastFileServer: EXCEPTION") {
                        $return_code = -2;
                        if ($showerrs) {
                            print STDERR "Error running $cmd";
                            $errors++;
                        }
                    }
                    elsif ($err_line =~ /^CastFileServer: EXIT (.*)$/) {
                        kill 9, $cast_server_pid;
                        waitpid $cast_server_pid, 0;
                        $cast_server_pid=0;
                        $return_code = -2 if ($1 != 0);
                    }
                    elsif ($err_line eq "CastFileServer: READY") {
                        $done = 1;
                    }
                    $err_line = ""; $err_data = "";
                    $starts_with_newline = 0;
                }
                $err_line .= $leftover if ($leftover);
            }
        }
    }
    $return_code = -2 if (!$done);
    # Drain stdout (not usually necessary)
    while ($SEL->can_read(0)) {
        last if (!sysread CCOut, $out_data, 4096);
        $return .= $out_data;
    }
    $return;
}

sub query_ports {
    my ($cell,$queryfile)=@_;
    start_cast_server if $cast_server_pid==0;
    my $cmd="jauto.CastQuery --no-header --task=external_nodes=di:im:xr,ports --cell=$cell";
    my $query=call_cast_server($cmd);
    if ($query ne "") {
        print STDERR "Writing $queryfile" if $debug;
        my $fqo;
        open ($fqo, ">$queryfile");
        printf $fqo "%s", $query;
        close $fqo;
    }
}

sub query_real_ports {
    my ($cell,$queryfile)=@_;
    start_cast_server if $cast_server_pid==0;
    my $cmd="jauto.CastQuery --no-header --task=external_nodes=im:xr:re --cell=$cell";
    my $query=call_cast_server($cmd);
    if ($query ne "") {
        print STDERR "Writing $queryfile" if $debug;
        my $fqo;
        open ($fqo, ">$queryfile");
        printf $fqo "%s", $query;
        close $fqo;
    }
    else { # if no real ports at all, just fake it
        $cmd="jauto.CastQuery --no-header --task=external_nodes=im:xr --cell=$cell";
        $query=call_cast_server($cmd);
        if ($query ne "") {
            print STDERR "Writing $queryfile" if $debug;
            my $fqo;
            open ($fqo, ">$queryfile");
            printf $fqo "%s", $query;
            close $fqo;
        }
    }
}

sub query_portmap {
    my ($cell,$queryfile,$verilogblock)=@_;
    start_cast_server if $cast_server_pid==0;
    $verilogblock = "rtl" if ! defined $verilogblock;
    my $cmd="prs2verilog.GeneratePortMapping --verilog-block=$verilogblock --cast-path=$castpath --cell=$cell";
    # ignore errors on generating port map
    my $query=call_cast_server($cmd, 0);
    if ($query ne "") {
        print STDERR "Writing $queryfile" if $debug;
        my $fqo;
        open ($fqo, ">$queryfile");
        printf $fqo "%s", $query;
        close $fqo;
    }
}

# convert 'b definitions to a list of Vdd and GND nodes
sub btolist {
    my ($signal)=@_;
    if ($signal =~ /'b/) {
        $signal =~ s/'b/ /;
        my ($cnt,$vals)=split(/ /,$signal);
        $signal="";
        my $v = substr($vals,0,1);
        $signal .= $v eq "1" ? "Vdd" : "GND"; 
        foreach my $i (1..length($vals)-1) {
            my $v = substr($vals,$i,1);
            $signal .= ", ";
            $signal .= $v eq "1" ? "Vdd" : "GND"; 
        }
    }
    $signal;
}

# read vsbind file which allows changing the port
# names, instance connections to override verilog
# mostly used for ports where some things just are
# not in the verilog
sub readbindfile {
    my ($file)=@_;
    local (*P,$_);
    if (open (P, "<$file") ) {
        print STDERR "Reading $file..." if $progress;
        while (<P>) {
            chomp;
            next if /^#/;
            next if /^\s*$/;
            my ($cell, $instance, $pin, $signal) = split;
            $bind{"$cell $instance $pin"}=$signal if defined $signal;
            $bindinstlist{$instance}=0 if $instance ne "*";
            $bindcelllist{$cell}=0;
            $bindnetlist{$signal}=0;
        }
    }
    else {
        print STDERR "Warning: bind file $file not readable";
        $warnings++;
    }
}

# traslate signal due to vsbind information collected above
sub getbindsignal {
    my ($cell,$inst,$port,$signal)=@_;
    if (defined $bind{"$cell $inst $port"}) {
        $signal = $bind{"$cell $inst $port"};
        $bindinstlist{$inst}=1;
        $bindcelllist{$cell}=1;
        $bindnetlist{$signal}=1;
    }
    elsif (defined $bind{"$cell * $port"}) {
        $signal = $bind{"$cell * $port"};
        $bindcelllist{$cell}=1;
        $bindnetlist{$signal}=1;
    }
    $signal;
}

# cadence cell name to cast name translation (using rename)
sub cellcad2cast {
    return $_[0] if ($_[0] =~ /\(/) or ($_[0] =~ /\$/) or ($_[0] eq "");
    print AWR "cell cadence cast $_[0]";
    my $new=<ARD>;
    chomp $new;
    if ($new =~ /\s/ or $new eq "") {
       $new = $_[0];
       close ARD;
       close AWR;
       kill $apid;
       waitpid $apid, 0;
       $apid = open2(\*ARD, \*AWR, "rename --type=all 2>/dev/null");
    }
    $new;
}

# cadence instance name to cast name translation (using rename)
sub instcad2cast {
    return $_[0] if ($_[0] =~ /\(/) or ($_[0] =~ /\$/) or ($_[0] eq "");
    print AWR "instance cadence cast $_[0]";
    my $new=<ARD>;
    chomp $new;
    if ($new =~ /\s/ or $new eq "") {
       $new = $_[0];
       close ARD;
       close AWR;
       kill $apid;
       waitpid $apid, 0;
       $apid = open2(\*ARD, \*AWR, "rename --type=all 2>/dev/null");
    }
    $new;
}

# cast instance name to cadence name translation (using rename)
sub instcast2cad {
    print AWR "instance cast cadence $_[0]";
    my $new=<ARD>;
    chomp $new;
    if ($new =~ /\s/ or $new eq "") {
       $new = $_[0];
       close ARD;
       close AWR;
       kill $apid;
       waitpid $apid, 0;
       $apid = open2(\*ARD, \*AWR, "rename --type=all 2>/dev/null");
    }
    $new;
}

# cadence node name to cast name translation (using rename)
sub nodecad2cast {
    return $_[0] if ($_[0] =~ /[\(\$']/) or ($_[0] eq "");
    print AWR "node cadence cast $_[0]";
    my $new=<ARD>;
    chomp $new;
    if ($new =~ /\s/ or $new eq "") {
       $new = $_[0];
       close ARD;
       close AWR;
       kill $apid;
       waitpid $apid, 0;
       $apid = open2(\*ARD, \*AWR, "rename --type=all 2>/dev/null");
    }
    $new;
}

# cast node name to cadence name translation (using rename)
sub nodecast2cad {
    return $_[0] if ($_[0] =~ /\(/) or ($_[0] =~ /\$/) or ($_[0] eq "");
    my $old=$_[0];
    print AWR "node cast cadence $_[0]";
    my $new=<ARD>;
    chomp $new;
    if ($new =~ /\s/ or $new eq "") {
       $new = $_[0];
       close ARD;
       close AWR;
       kill $apid;
       waitpid $apid, 0;
       $apid = open2(\*ARD, \*AWR, "rename --type=all 2>/dev/null");
    }
    $new;
}

sub special { # really double index arrays
    my ($name)=@_;
    $name =~ s/\[/_l_/g;
    $name =~ s/\]/_r_/g;
    $name;
}

# make names cast compatible
my %instxref=();
my %instlookup=();

sub fix_inst {
    my ($inst)=@_;
    return $instlookup{$inst} if defined $instlookup{$inst};
    my $base = "\\[(\\d+)\\]";
    my $expr = "$base";
    # look for arrays at end of instance name
    my $prefix=$inst;
    my @new=();
    if ($cast_and_spec) { # for proteus async, keep array syntax
        my $n=0;
        while ($inst =~ /$expr$/) {
            $expr .= $base;
            $n++;
        }
        $expr = substr($expr,0,$n*length($base));
        $inst =~ /(.*)$expr$/;
        $prefix=$1;
        # array all the integer indicies
        for (my $x = 2; $x <= $n+1; $x++) {
            eval "push \@new, \$$x;";
        }
    }
    # the prefix needs gdsii nameing
    $prefix =~ s/\[/_L_/g;
    $prefix =~ s/\]/_R_/g;
    $prefix =~ s/\./_D_/g;
    $prefix =~ s/\$/_/g;
    $prefix =~ s/,/_c_/g;
    # add on cast-like multi-dim array
    if (@new) {
        $prefix.= "[".join(",", @new)."]";
    }
    # check for duplicates
    $prefix =~ s/\//_slash_/g;
    if (defined ($instxref{$prefix}) and $instxref{$prefix} ne $inst) {
        print STDERR "Warning: apparent inadvertent duplicate instance name $prefix";
        $warnings++;
    }
    $instxref{$prefix} = $inst;
    $instlookup{$inst} = $prefix;
    $prefix;
}

my %nodexref=();
my %nodecheck=();
my $localmodule="";

sub setnodexref {
    my ($in,$out)=@_;
    if ($out ne $in and defined($nodecheck{$out}) and $nodecheck{$out} ne $in) {
        print STDERR "Warning: change in node renaming $out $nodecheck{$out} => $in";
    }
    $nodecheck{$out}=$in if $out ne $in;
    $nodexref{$in}=$out;
}

sub fix_node {
    my ($signal1, $ln)=@_;
    my $insignal=$signal1;
    my $doprint = 0;
#    $doprint = 1 if $signal1 =~ /\[/;
    return $nodexref{$signal1} if defined $nodexref{$signal1};
    $signal1 =~ s/\//_slash_/g;
    printf STDERR "$signal1 $ln" if $doprint;
    if (defined($ecanonical{$localmodule}->{$signal1})) {
        if (defined ($rcanonical{$localmodule}->{$signal1})) {
            setnodexref($signal1,$rcanonical{$localmodule}->{$signal1});
        }
        else {
            setnodexref($signal1,$signal1);
        }
        print STDERR " 1 $nodexref{$signal1}" if $doprint;
        return $nodexref{$signal1};
    }
    my $try=$signal1;
    $try =~ s/\]\[/,/g;
    if (defined($ecanonical{$localmodule}->{$try})) {
        if (defined ($rcanonical{$localmodule}->{$try})) {
            setnodexref($try,$rcanonical{$localmodule}->{$try});
        }
        else {
            setnodexref($try,$try);
        }
        print STDERR " 2 $nodexref{$try}" if $doprint;
        return $nodexref{$try};
    }
    my $in=$signal1;
    $signal1 =~ s/\$/_/g;
    $signal1 =~ s/\./_D_/g;
    $signal1 =~ s/\[/_l_/g;
    $signal1 =~ s/\]/_r_/g;
    $signal1 =~ s/,/_c_/g;
    if ($signal1 =~ /^\\/) {
        my ($sl,$sr)=split("_blank_", $signal1);
        $sr = "" if ! defined $sr;
        $sl =~ s/\\//;
        $sl=~ s/\[/_l_/g;
        $sl=~ s/\]/_r_/g;
        $sl=~ s/\./_D_/g;
        setnodexref($in,$sl.$sr);
        print STDERR " 3 $nodexref{$in}" if $doprint;
        return $sl.$sr;
    }
    if ($signal1 =~ /\]_blank_\[/) {
        my ($sl,$sr)=split("_blank_", $signal1);
        $sr = "" if ! defined $sr;
        $sl =~ s/\\//;
        $sl=~ s/\[/_l_/g;
        $sl=~ s/\]/_r_/g;
        $signal1 = $sl.$sr;
    }
    elsif ($signal1 =~ /_r__blank__l_/) {
        my ($sl,$sr)=split("_blank_", $signal1);
        $sr = "" if ! defined $sr;
        $sl =~ s/\\//;
        $sl=~ s/\[/_l_/g;
        $sl=~ s/\]/_r_/g;
        $sr=~ s/_l_/[/g;
        $sr=~ s/_r_/]/g;
        $signal1 = $sl.$sr;
    }
    elsif ($signal1 =~ m:\[\d+\]\[\d+\]:) {
        $signal1 =~ s/\[/_l_/g;
        $signal1 =~ s/\]/_r_/g;
    }
    if ($signal1 =~ / /) {
        $signal1 =~ s/ //;
    }
    if ($signal1 =~ /\]\[/) {
        $signal1 =~ s/\[/_l_/;
        $signal1 =~ s/\]/_r_/;
    }
    $signal1 =~ s/\[([^\d][^\]]*)\]/_l_${1}_r_/;
    if ($signal1 =~ /\]\[/) {
        $signal1 =~ s/_r_\[/_c_/;
        $signal1 =~ s/\]\[/_r_\[/;
    }
    $signal1 = $specialnodes{$signal1} if (defined $specialnodes{$signal1});
    $signal1 = $castname{$signal1} if defined $castname{$signal1};
    setnodexref($in,$signal1);
    print STDERR " 4 $signal1" if $doprint;
    $signal1;
}

# depends too much on globals here
sub domodule {
    my ($cast)=@_;
    my @mlines=();
    my $portlist=1;
    if (@module) {
        if ($debug) {
            printf STDERR "Module $module[0]";
            foreach my $nd (sort keys %nodes) {
                print STDERR " Node $nd $nodes{$nd}";
            }
        }
        my $mod=$module[0];
        chomp $mod;
        foreach my $ln (@module) {
            my $ok = 1;
            $portlist=0 if $ln =~ /</;
            $ln =~ s/\$/_/g;
            if (! $portlist and ($ln =~ /^ *node /) ) {
                my $lx = $ln;
                chomp $lx;
                $lx =~ s/^  *//;
                $lx =~ s/;//;
                my @f=split(/ /,$lx);
                $ok = 0 if (! ($f[1] =~ /\[/)) and defined($nodes{$f[1]}) and $nodes{$f[1]}==0;
                print STDERR "Unused $ln in $mod ($nodes{$f[1]})($f[1])" if ! $ok and $verbose;
            }
            print $ln if $ok and ! $propagate;
            push @mlines, $ln if $ok and $propagate;
        }
        if ($propagate and @mlines) {
            writecast(\@mlines,$cast ? $castdir : $specdir);
        }
        @module=();
        %nodes=();
    }
}

# generate datetime only once
my @datetime=localtime(time);

sub cast_header {
    my ($fh)=@_;
    my $year = $datetime[5]+1900;
    my $user=`whoami`;
    chomp $user;
    print $fh "/*";
    print $fh " * Copyright (C) 2002-$year Fulcrum Microsystems. All rights reserved.";
    print $fh " * \$\Id:\$";
    print $fh " * \$\DateTime:\$";
    print $fh " * \$\Author: $user \$";
    printf $fh " * Generated by import-cast (vs2cast) %02d/%02d/%04d %02d:%02d:%02d\n",
        $datetime[4]+1, $datetime[3], $datetime[5]+1900,
        $datetime[2], $datetime[1], $datetime[0];
    print $fh "*/\n";
}

sub writecast {
    my ($lines,$dir)=@_;
    my @lines=@{$lines};
    chomp @lines;
    my $module=shift @lines;
    # put a blank line after the module name
    unshift @lines, "";
    unshift @lines, $module;
    my @f=split(/ /, $module);
    $f[$#f] =~ s/;//;
    my $libpath=$f[$#f];
    $libpath =~ s/\./\//g;
    $libpath = "$dir/$libpath";
    my $st=$subtype;
    system "mkdir -p '$libpath' 2>/dev/null";
    foreach my $ln (@lines) {
        if ($ln =~ /^define/) {
            my @f=split(/"/,$ln);
            $st = $f[1];
            last;
        }
    }
    my $exists=0;
    my $writable=0;
    my $outfile="$libpath/$st.cast";
    $exists = 1 if -f $outfile;
    $writable = 1 if -w $outfile;
    my $p4edit = ($p4 and $exists and ! $writable);
    system "p4 -c $p4client edit '$outfile'"
        if $p4edit;
    my $fc;
    if (open ($fc, ">$outfile") ) {
        print STDERR "Writing $outfile" if $progress;
        cast_header($fc);
        print $fc join("\n", @lines);
        close $fc;
        system "p4 -c $p4client revert -a '$outfile'"
            if $p4edit;
        system "p4 -c $p4client add '$outfile'"
            if $p4 and ! $exists;
    }
    else {
        print STDERR "Failed to write '$outfile'";
        exit 1;
    }
}

# for profiling, does not work on compute servers
# if profile = 1
#use Time::HiRes qw( gettimeofday tv_interval);
# if profile = 0

sub gettimeofday {}
sub tv_interval {0;}

my %opts = (
    "add-import=s" => sub { push @addimport, $_[1];},
    "bindfile=s" => \$bindfile,
    "cast-and-spec" => \$cast_and_spec,
    "cast-dir=s" => \$castdir,
    "cast-path=s" => \$castpath,
    "cdc-mode" => \$cdc_mode,
    "client=s" => \$p4client,
    "debug" => \$debug,
    "external=s" => \$externalfile,
    "extra-implied=s" => sub {
        my @f=split(/,/,$_[1]);
        foreach my $f (@f) {
            my ($type,$dn)=split(/:/,$f,2);
            if ( ! defined ($dn) ) {
                $dn = $type;
                $type = "node";
            }
            $dn =~ s/^([-+]*)//;
            my $dir=$1;
            push @extra_implied, [$type,$dn,$dir];
        }},
    "extra-ports=s" => sub {
        my @f=split(/,/,$_[1]);
        foreach my $f (@f) {
            my ($type,$dn)=split(/:/,$f,2);
            if ( ! defined ($dn) ) {
                $dn = $type;
                $type = "node";
            }
            $dn =~ s/^([-+]*)//;
            my $dir=$1;
            push @extra_ports, [$type,$dn,$dir];
        }},
    "extra-refine=s" => \$extra_refine,
    "library=s" => \$library,
    "max-heap-size=s" => \$max_heap_size,
    "output=s" => \$outputfile,
    "p4" => \$p4,
    "parent=s" => \$argparent,
    "port-base=s" => \$portbase,
    "progress" => \$progress,
    "propagate" => \$propagate,
    "quiet" => \$quiet,
    "refineparent=s" => \$refineparent,
    "spec-dir=s" => \$specdir,
    "subtype=s" => sub { $subtype = $_[1]; $subtypedefined=1; },
    "verbose" => \$verbose,
    "verilog-block=s" => \$verilog_block,
    "wrap=i" => sub { $wrap = $argwrap = $_[1];},
);

sub usage {
    print STDERR "@_" if @_;
    print STDERR <<EU;
Usage: vs2cast [options] <verilog> <cellname>
    --cast-dir=<dir>      : cast directory 
    --library=<name>      : required library name
    --spec-dir=<dir>      : cast spec directory
    [--cast-path=<list>   : if not cast-dir:spec-dir
    [--add-import=<name>] : to be added to each module
    [--bindfile=<name>]   : external binding file
    [--client=<>]         : to invoke p4
    [--debug]             : generate lots of debug data
    [--external=<file>]   : read file to define external cells with arrays
    [--max-heap-size=<>   : set max heap size for sub-tasks
    [--output=<file>]     : output instead of stdout. (may not work right.)
    [--p4]                : to allow p4
    [--parent=<>]         : refinement parent of top cell
    [--progress]          : show progress
    [--propagate]         : actually populate the spec dir
    [--quiet]             : no info at all
    [--refineparent=<>]   : refinement parent of subcells
    [--subtype=#]         : default 1000
    [--verbose]           : generate less debug data
    [--verilog-bloc=<>]   : if not rtl
    [--warning]           : generate warning messges
    [--wrap=#]            : number of times to wrap the cast for canon names
    [--extra-refine=<>]   : extra refinement parent to be added
    [--port-base=<>]      : base cell for port names if not parent
    [--cdc-mode]          : used to create a cast of an routed cdc
EU
    exit 1;
}

select STDERR;
$|=1;
select STDOUT;
$|=1;
GetOptions ( %opts ) or usage;
$portbase = $argparent if $portbase eq "" and defined $argparent;
$subtype = 1000 if ! defined $subtype;
usage if ! defined $ARGV[1];
usage "Must define a library ($library)" if (! ($library =~ /\./)) or $library eq "";
`mkdir -p  "$castdir"` if $castdir ne "" and ! -d $castdir;
usage "Must define a cast-dir ($castdir)" if $castdir eq "" or ! -d $castdir;
`mkdir -p  "$specdir"` if $specdir ne "" and ! -d $specdir;
usage "Must define a spec-dir ($specdir)" if $specdir eq "" or ! -d $specdir;
$verbose=1 if $debug;
$warning=1 if $verbose;
$progress=1 if $verbose;
$verbose = $debug = $warning = $progress = 0 if $quiet;
my $verilog=$ARGV[0];
my $md5sum=`md5sum '$verilog'`;
chomp $md5sum;
$md5sum =~ s/\s.*//;
my $fulcrum_id=`head '$verilog' | awk '/FULCRUM ID/ {print \$4}'`;
chomp $fulcrum_id;
$fulcrum_id = 0 if ! $fulcrum_id =~ /^\d+$/;
my $proteus_tag="$md5sum $fulcrum_id";
my $extranodes=$verilog;
$extranodes =~ s/\.v$/.extranodes/;
my $directives_file=$verilog;
$directives_file =~ s/\.[^\.]+$/.directives/;
my $arg=$ARGV[1];
my $argcell=$arg;
$argcell =~ s/\.(\d+)$//;
my $argsubtype=$1;
if ($outputfile eq "") {
    $outputfile = $ARGV[0];
    $outputfile =~ s/\.v[^\.]*$//;
    $outputfile .= ".cast";
}
if ($bindfile eq "") {
    $bindfile = $arg;
    $bindfile .= ".vsbind";
}
readbindfile ($bindfile) if ($bindfile ne "" and -s $bindfile);

sub typerename {
    my ($type)=@_;
    if ($type eq "") {
        $type = "node";
    }
    elsif ($type =~ m=\[(\d+):(\d+)\]=) {
        my $b=$2;
        my $e=$1;
        if ($b == 0) {
            $e++;
            $type = "node\[$e\]";
        }
        else {
            $type = "[$b..$e]";
        }
    }
    $type;
}

$debugfile = "vs2cast.dbg" if $debug;
open (DBG, ">$debugfile");

# not in the verilog, but necessary
my %specialcases = (
    "QCSMS1_DS_13T" => ["VDD10=vdd","VSS10=GND"],
    "QCSMS1_MUX_13T" => ["VDD10=vddA","VSS10=GND"],
);

my %portnodes=();
my %hasarray=();
my @moduleports=();
my @ports=();
my %porttypes=();
my %wires=();
my %localnodes=();
my %module=();
my %modules=(); # just those in the file

# whole files goes into memory for ease of handling 'c' comments
sub readfile {
    my ($file) = @_;
    local(*P, $_, $/);
    undef $/;
    if (open (P, "<$file")) {
        printf STDERR "Reading $file..." if $progress;
    }
    else {
        die "Cannot open $file $!";
    }
    my $text = <P>;
    close P;
    $text =~ s/;/;\n/g;
    $text =~ s/\t/ /g;
    $text =~ s/  */ /g;
    $text =~ s/^ *//;
    $text =~ s/\n */\n/g;
    $text =~ s/\] \[/]_blank_[/g;
    # initial lines with comments
    while ($text =~ m:^//:) {
        $text =~ s:^//[^\n]*\n::;
    }
    $text =~ s/^ *//;
    # other 'rest of line' comments
    while (index ($text, "//") > 0) {
        $text =~ s: *//[^\n]*\n:\n:g;
        $text =~ s/\n\n/\n/g;
    }
    # remove blank lines
    while ($text =~ /\n\n/) {
        $text =~ s/\n\n/\n/g;
    }
    # remove 'c' type comments
    my $i1;
    my $i2;
    my $t;
    while (($i1 = index($text, "/*")) >= 0) {
        $i2 = index(substr($text,$i1),"*/");
        $t = substr($text,0,$i1).substr($text,$i1+$i2+2);
        $text=$t;
    }
    print STDERR "Done" if $progress;
    # added for slash separators
#    $text =~ s:/:_slash_:g;
    # make into an array of lines
    split(/\n/,$text);
}

sub vsplit {
    my ($string)=@_;
    my @array=();
    my $token="";
    my $esc=0;
    $string =~ s/\/\/.*//;
    $string =~ s/^\s+//;
    for (my $i = 0; $i < length($string); $i++) {
        my $s = substr($string,$i,1);
        if ($s =~ /\s/) {
            $esc=0;
            if (length($token)) {
                push @array, $token;
                $token="";
            }
        }
        elsif (($s =~ /[,\)\({}=]/) and ! $esc) {
            $esc=0;
            if (length($token)) {
                push @array, $token;
                $token="";
            }
            push @array, "$s";
        }
        elsif ($s eq "\\") {
            $esc=1;
        }
        elsif ($s eq ";") {
            if (length($token)) {
                push @array, $token;
                $token="";
            }
            push @array, ";";
            last;
        }
        else {
            $token .= $s;
        }
    }
    @array;
}

sub parse {
    my ($file, $external) = @_;
    my @lines = readfile ($file);
    printf STDERR "Parsing $file..." if $progress;
    my $lnr = 0;
    local ($_);
    my $module = "";
    %localnodes=();
    my %plookup=();
    my @plookup=();
    my @wlookup=();
    my @wires=();
    my @inst=();
    my $hasarray=0;
    my @mp=();
    my @assign=();
    while ($lnr <= $#lines) {
        $_=$lines[$lnr++];
        # skip blank lines
        s/^\s+//;
        next if (/^$/);
        # module definition until terminating ;
        if (/^module/) {
            @assign=();
            my $ln = $_;
            while (! ($ln =~ /;/) and $lnr <= $#lines) {
                $ln .= " $lines[$lnr++]";
            }
            my @mp = vsplit($ln);
            shift @mp;
            $module = cellcad2cast(shift @mp);
            $modules{$module}=1;
            $hasarray=0;
            @moduleports=();
            my $lp = shift @mp;
            print STDERR "Error: expected '(' got '$lp'" if $lp ne '(';
            foreach my $n (0..$#mp) {
                if ($mp[$n] eq ')' and $mp[$n+1] eq ';') {
                    last;
                }
                elsif ($mp[$n] eq ';' or $mp[$n] eq ')') {
                    print STDERR "Error: Unexpected '$mp[$n]'";
                }
                elsif ($mp[$n] ne ',') {
                    $moduleports[$n] = ["node", $mp[$n]];
                    $portmap{$module}->{$mp[$n]}=$n;
                    print DBG "HA1 $module $mp[$n]" if $debug;
                    $hasarray = 1 if ($mp[$n] =~ /\[/);
                }
            }
            $hasarray{$module}=$hasarray;
            undef %plookup;
            %plookup=();
            foreach my $n (0..$#mp) {
                $plookup{$mp[$n]}=$n;
            }
            @wires=();
            @wlookup=();
        }
        # port declarations
        elsif (/^output/ or /^input/ or /^inout/) {
            my $ln = $_;
            my $ln = $_;
            while (! ($ln =~ /;/) and $lnr <= $#lines) {
                $ln .= " $lines[$lnr++]";
            }
            my @tokens=vsplit($ln);
            my $io = shift @tokens;
            my $aref = shift @tokens;
            my $end=pop @tokens;
            print STDERR "Parse Error: '$end' should be ';'" if $end ne ';';
            if ($aref eq "wire") {
                $aref = shift @tokens;
            }
            if ($aref =~ /^\[\d+:\d+\]$/ ) {
                $hasarray{$module}=1;
            }
            else {
                unshift @tokens, $aref;
                $aref = "";
            }
            if ($io eq "input") {
                $io = "-";
            }
            elsif ($io eq "output") {
                $io = "+";
            }
            else {
                $io = "+-";
            }
            foreach my $port (@tokens) {
                if (! ($port =~ /^[,;]/)) {
                    $portdirection{"$module $port"}=$io;
                    if (defined $plookup{$port}) {
                        $moduleports[$plookup{$port}]=[$aref,$port];
                        $plookup[$plookup{$port}] = $aref;
                    }
                    else {
                        print STDERR "Warning: Undefined ref :$port: at $." if $port ne "";
                        $warnings++;
                    }
                }
            }
        }
        # local declarations, not necessarily all local nodes!
        elsif (/^wire/ or /^trireg/) {
            my $ln = $_;
            while (! ($ln =~ /;/) and $lnr <= $#lines) {
                $ln .= " $lines[$lnr++]";
            }
            my @tokens=vsplit($ln);
            my $io = shift @tokens;
            my $aref = shift @tokens;
            my $end=pop @tokens;
            print STDERR "Parse Error: '$end' should be ';'" if $end ne ';';
            if (! ($aref =~ /^\[\d+:\d+\]$/ ) ) {
                unshift @tokens, $aref;
                $aref = "";
            }
            foreach my $wire (@tokens) {
                if (! ($wire =~ /^[,;]/)) {
                    push @wlookup, [$aref,$wire] if ! defined $plookup{$wire};
                }
            }
        }
        # finish up module
        elsif (/^endmodule/) {
            print DBG "ENDMODULE $module $lnr" if $debug;
            $moduleports{$module}=[@moduleports];
            @moduleports=();
            $wiretypes{$module} = [@wlookup];
            @wlookup=();
            $external{$module}=1 if $external;
            $inst{$module}=[@inst];
            @inst=();
            @plookup = ();
            if (@assign) {
                $assign{$module}=[@assign];
            }
            $module="";
        }
        # assigns
        elsif (/^assign /) {
            my @a=vsplit($_);
            shift @a;
            my  $a = "";
            my $n=0;
            while ($n <= $#a and $a[$n] ne "=") {
                $a .= $a[$n];
                $n++;
            }
            my $b="";
            $n++;
            while ($n <= $#a and $a[$n] ne ";") {
                $b .= $a[$n];
                $n++;
            }
            $b = "GND" if $b eq "1'b0";
            $b = "Vdd" if $b eq "1'b1";
            push @assign, "$a=$b" if $a ne "obs";
        }
        # old assigns
        elsif (/^xyassign /) {
            my $l=$_;
            $l =~ s/ *;//;
            $l =~ s/[ ]+/ /g;
            $l =~ s/^assign *//;
            my ($a, $b)=split(/=/,$l);
            $b =~ s/ //g;
            $b = "GND" if $b eq "1'b0";
            $b = "Vdd" if $b eq "1'b1";
            my ($x,$y)=split(/ /,$a);
            $x = fix_node($x, __LINE__);
            if (defined ($y) and $y ne "") {
                $a = "$x$y";
            }
            else {
                $a = $x;
            }
            $a =~ s/ //g;
            ($x,$y)=split(/ /,$b);
            $x = fix_node($x, __LINE__);
            if (defined ($y) and $y ne "") {
                $b = "$x$y";
            }
            else {
                $b = $x;
            }
            $b =~ s/ //g;
            push @assign, "$a=$b" if $a ne "obs";
        }
        # assumes the rest is a list of instances, no logic
        else {
            my $ln = $_;
            while (! ($ln =~ /;/) and $lnr <= $#lines) {
                $ln .= " $lines[$lnr++]";
            }
            $_ = $ln;
            next if $ln eq "";
            my @ins = vsplit($ln);
            my $type = shift @ins;
            my $inst = shift @ins;
            my $itype = "$module:$inst";
            my $x = shift @ins;
            print STDERR "Error: Expected '(' got '$x' at line $lnr ($ln)" if $x ne '(';
            my @p=();
            for (my $n = 0; $n < $#ins; $n++) {
                my $pin = $ins[$n];
                print STDERR "Error: invalid pin form $pin" if ! $pin =~ /^\./;
                $pin =~ s/^\.//;
                my $net = "";
                my $i;
                for ($i = $n+2; $i <= $#ins and $ins[$i] ne ')'; $i++) {
                    $net .= " $ins[$i]";
                }
                $net =~ s/^\s//;
                my $p=nodecad2cast($pin);
                if ($net ne "") {
                    # maybe someday
                    #my $n=nodecad2cast($net);
                    $signal{$itype}->{$p}=$net;
                    push @p, "$p=$net";
                }
                $n = $i + 1;
            }
            $pins{$itype} = [@p];
            $type{$itype} = cellcad2cast($type);
            push @inst, $itype;
        }
    }
    print STDERR "Done" if $progress;
}

$castpath="$castdir:$specdir" if $castpath eq "";
my $topcell=$verilog;
$topcell =~ s/\.[^\.]+$//;
#$refineparent = "chip.bali.port.epl.EPL_CELL" if $topcell =~ /EPL/; # HACK
# get externals to define ports of sIPCell, for example
# cells without definitions are assumed single ports
parse ("$externalfile", 1) if -r $externalfile;
parse ($verilog, 0);
my $mapfile=$arg;
$mapfile .= ".vs2cast.portmap";
if ( -r $mapfile  and -s $mapfile ) {
    open (P, "<$mapfile");
    print STDERR "Reading $mapfile" if $progress;
}
else {
    my $cell=$argcell;
    print STDERR "Generating $mapfile from cast" if $progress;
    query_portmap ($cell,$mapfile,$verilog_block);
    open (P, "<$mapfile");
}
while (<P>) {
    chomp;
    my ($verilog,$cast)=split;
    $verilog{$cast}=$verilog;
    $castname{$verilog}=$cast;
    my %cmaps=();
    my %vmaps=();
    # probably should be more careful with this:
    if ($cast =~ /(.*)\[(\d+)\]$/) {
        my $castname=$1;
        my $castndx=$2;
        if ($verilog =~ /(.*)\[(\d+)\]$/) {
            my $vname=$1;
            my $vndx=$2;
            if ($vndx == $castndx) {
                $verilog{$castname}=$vname;
                $castname{$vname}=$castname;
                if (defined ($cmaps{$vname}) and $cmaps{$vname} ne $castname) {
                    print STDERR "Warning: Inconsistent v->c array mapping $vname : $castname vs. $cmaps{$vname}";
                }
                elsif (! defined $cmaps{$vname}) {
                    $cmaps{$vname}=$castname;
                }
                if (defined ($vmaps{$castname}) and $vmaps{$castname} ne $vname) {
                    print STDERR "Warning: Inconsistent c->v array mapping $castname : $vname vs. $vmaps{$castname}";
                }
                elsif (! defined ($vmaps{$castname})) {
                    $vmaps{$castname}=$vname;
                }
            }
        }
    }
}
close P;
if ( ! -r $mapfile or ! -s "$mapfile") {
    # ok generate a portmap from the verilog itself, no valid cast
    my $cell;
    $cell = $argcell if defined ($modules{$argcell});
    $cell = $arg if defined ($modules{$arg});
    if (defined ($cell)) {
        print STDERR "Generating $mapfile from verilog" if $progress;
        open (Q, ">$mapfile");
        my %map=%{$portmap{$cell}};
        foreach my $p (sort keys %map) {
            print Q "$p $p";
            $verilog{$p}=$p;
            $castname{$p}=$p;
        }
        close Q;
    }
}
if ( ! -r $mapfile or ! -s "$mapfile") {
    # this time it is a lost cause
    die "Error: $mapfile contains no data but is required";
}

my %sxref=();
my %castexists=();
# find all cells in the spec tree
# there are likely duplicates, especially with multiple subtypes
mkdir "cache";
my $cellxreffile="cache/cellxref.dat";
my @castdirs=split(/:/, $castpath);
if ( -s "$cellxreffile") {
    open (P, "<$cellxreffile");
    printf STDERR "Reading $cellxreffile..." if $progress;
    while (<P>) {
        chomp;
        my ($name,$cn)=split;
        $sxref{$name}=$cn;
        my $x=$cn;
        my $cds=$x;
        $cds =~ s:\.([^\.]+)$::;
        $cds =~ s/\./\//g;
        $x =~ s/\.[^\.]+$//;
        my $cd = $x;
        $cd =~ s:\.([^\.]+)$::;
        my $tail=$1;
        $cd =~ s/\./\//g;
        foreach my $dir (@castdirs) {
            if ( -f "$dir/$cds.cast" or -f "$dir/$cd.cast") {
                $castexists{$x}=1;
                $castexists{$cn}=1;
                last;
            }
        }
    }
    close P;
    print STDERR "Done" if $progress;
}
else {
    printf STDERR "Writing $cellxreffile..." if $progress;
    open (P, "find '$specdir' -name \\*.cast |");
    while (<P>) {
        chomp;
        s:$specdir/::;
        s/:/ /;
        s:/:.:g;
        s/\.cast$//;
        my $spec=$_;
        my $cast=$_;
        $cast =~ s/\.([^\.]+)$//;
        my $st = $1;
        my $st1 = -1;
        my $name = $cast;
        $name =~ s/.*\.//;
        next if ! ($st =~ /^\d+$/);
        if (defined ($sxref{$name})) {
            $st1 = $sxref{$name};
            $st1 =~ s/.*\.//;
        }
        $sxref{$name}=$spec if $st < $st1 or $st1 == -1;
    }
    close P;
    open (P, ">$cellxreffile") || die "Cannot write to $cellxreffile: $!";
    foreach my $name (sort keys %sxref) {
        print P "$name $sxref{$name}";
        my $cn = $sxref{$name};
        my $x=$cn;
        my $cds=$x;
        $cds =~ s:\.([^\.]+)$::;
        $cds =~ s/\./\//g;
        $x =~ s/\.[^\.]+$//;
        my $cd = $x;
        $cd =~ s:\.([^\.]+)$::;
        my $tail=$1;
        $cd =~ s/\./\//g;
        foreach my $dir (@castdirs) {
            if ( -f "$dir/$cds.cast" or -f "$dir/$cd.cast") {
                $castexists{$x}=1;
                $castexists{$cn}=1;
                last;
            }
        }
    }
    close P;
    print STDERR "Done" if $progress;
}

sub castexists {
    my $cell=shift;
    return $castexists{$cell} if defined $castexists{$cell};
    my $cc = $cell;
    $cc =~ s/\.\d[^\.]*$//;
    if (defined $castexists{$cc}) {
        $castexists{$cell} = $castexists{$cc};
        return $castexists{$cc};
    }
    my $rv=call_cast_server("jflat.JFlat --cast-path=$castpath --tool=check --cell=$cc", 0);
    $rv = ($rv =~ /Checked/) ? 1 : 0;
    $castexists{$cc} = $castexists{$cell} = $rv;
    return 0 if $rv == 0;
    if ($cc ne $cell) {
        $rv=call_cast_server("jflat.JFlat --tool=check --cell=$cell");
        $rv = ($rv =~ /Checked/) ? 1 : 0;
        $castexists{$cell} = $castexists{$cc} = $rv;
    }
    $castexists{$cc};
}

sub getports {
    my ($cell)=@_;
    my $castcell=$cell;
    $castcell =~ s/\.\d[^\.]*$//;
    if ($castcell =~ /\.$subtype$/) {
        $castcell =~ s/\.$subtype$//;
    }
    return 1 if ignore_cell($cell);
    return 1 if (defined ($ports{$cell}));
    if (defined ($ports{$castcell})) {
        $ports{$cell}=$ports{$castcell};
        $implied{$cell}=$implied{$castcell};
        return 1;
    }
    print STDERR "getports($cell)=$castcell" if $debug;
    my @ports=();
    my @portnodes=();
    my @implied;
    my @external=();
    my @castlines=();
    my %aliasedports=();
    # first priority, find port list from cast
    my $queryfile="cache/$castcell.query.ports";
    my $requeryfile="cache/$castcell.query.realports";
    my $mapqueryfile="cache/$castcell.portmap";
    if ($cell =~ /\./) { # cannot be cast file without .'s
#        if (castexists($castcell) or ($castcell =~ /\./)) { }
        if (castexists($castcell)) {
            query_ports($castcell,$queryfile);
            query_real_ports($castcell,$requeryfile);
            query_portmap($castcell,$mapqueryfile);
            if ( ! -s $queryfile) {
                print STDERR "Error: creating/opening $queryfile";
                $errors++;
            }
            open (P, "<$queryfile");
            while (<P>) {
                chomp;
                print STDERR "getports: query $cell" if $. == 1 and $debug;
                if (/^[01] /) {
                    my ($implied, $type, $name)=split;
                    my $dir = "";
                    my $ctype=$type;
                    if ($name =~ s/^([-+]*)//) {
                        $dir = $1;
                        if (length($dir) > 1) {
                            $dir="";
                        }
                    }
                    my $cname=$name;
                    if ($name =~ s/\[(\d+)\.\.(\d+)\]$//) {
                        my $lo=$1;
                        my $hi=$2;
                        $ctype = "$type\[$lo..$hi\]";
                    }
                    elsif ($name =~ s/\[(\d+)\.\.(\d+),(\d+)\.\.(\d+)\]$//) {
                        my $lo1=$1;
                        my $hi1=$2;
                        my $lo2=$3;
                        my $hi2=$4;
                        $ctype = "$type\[$lo1..$hi1,$lo2..$hi2\]";
                    }
                    if ( ! ($type =~ /^node/) and ! defined ($channeldefs{"$ctype"})) {
                        foreach my $ex (@external) {
                            my ($en) = split(/ /,$ex);
                            my $p=$en;
                            $p =~ s/\..*//;
                            $p =~ s/\[.*//;
                            if ($p eq $name) {
                                my $ch=$en;
                                $ch =~ s/$name//;
                                print STDERR "CHD $name $en $ch $cell $ctype" if $debug;
                                push @{$channeldefs{"$ctype"}}, $ch;
                            }
                        }
                    }
                    push @castlines, "define \"XXXX\"() (" if ! @castlines;
                    if ($implied) {
                        push @implied, [$ctype, $name, $dir];
                    }
                    else {
                        if (($type =~ /([^\]]+)\[(\d+)\.\.(\d+)\]$/) and 0) {
                            my $ltt=$1;
                            my $lxx=$type;
                            $lxx =~ s/$ltt//;
                            push @castlines, "    $ltt $dir$name$lxx";
                        }
                        else {
                            push @castlines, "    $type $dir$cname";
                        }
                        push @ports, [$ctype, $name, $dir];
                    }
                }
                else {  # this comes first from cast_query
                    s/^([-+]*)//;
                    my $d=$1;
                    $d="" if length($d) > 1;
                    push @external, $_;
                    my ($p,$a)=split;
                    my $isenable = $a =~ /\.e$/ ? 1 : 0;
                    $cpdirection{$cell}->{$p}=$d;
#                    $cpdirection{$cell}->{$a}=$d;
                    $cpdirection{$castcell}->{$p}=$d;
#                    $cpdirection{$castcell}->{$a}=$d;
                    $ecanonical{$cell}->{$p}=$a;
                    $ecanonical{$castcell}->{$p}=$a if $castcell ne $cell;
                    $ecanonical{$cell}->{$a}=$a;
                    $ecanonical{$castcell}->{$a}=$a if $castcell ne $cell;
                    $eicount{$castcell}->{$a} += ($d eq '-' ? 1 : 0);
                    $eicount{$cell}->{$a} += ($d eq '-' ? 1 : 0);
                    if ($cell eq $porttopcell) {
                        if (defined($aliasedports{$a}) ) {
                            $aliasedports{$a} .= " $p";
                        }
                        else {
                            $aliasedports{$a} = $p;
                        }
                    }
# commented out stuff in the next approx 50 lines may be needed
# later for better handling of canonical port channels.
#                    if ($p eq $a) {
#                        $realchannel{$castcell}->{$a}=$p;
#                        $realchannel{$cell}->{$a}=$p;
#                        $ecanonical{$castcell}->{$a}=$a;
#                        $ecanonical{$cell}->{$a}=$a;
#                    }
                    # convert .d[#] to .#
                    $revlookup{$castcell}->{$a}=$p;
                    $revlookup{$cell}->{$a}=$p;
                    if ($a =~ /\.\d+$/) {
                        $p =~ s/\.d\[(\d+)\]$/.$1/;
                    }
                    $rcanonical{$castcell}->{$a}=$p;
                    $rcanonical{$cell}->{$a}=$p;
#                    $realchannel{$castcell}->{$a}=$p;
#                    $realchannel{$cell}->{$a}=$p;
                    my $done=$isenable;
                    while (! $done and $p ne "" and $a ne "") {
                        my $ps="";
                        if ($p =~ s/\.([^\.]+)$//) {
                            $ps = $1;
                        }
                        my $as="";
                        if ($a =~ s/\.([^\.]+)$//) {
                            $as = $1;
                        }
                        if ($as eq $ps and $as ne "") {
                            $realchannel{$castcell}->{$a}=$p;
                            $realchannel{$cell}->{$a}=$p;
                            $realchannel{$castcell}->{$p}=$p;
                            $realchannel{$cell}->{$p}=$p;
#                            $ecanonical{$castcell}->{$a}=$a;
#                            $ecanonical{$cell}->{$a}=$a;
#                            $ecanonical{$castcell}->{$p}=$a;
#                            $ecanonical{$cell}->{$p}=$a;
                        }
                        else {
                            $done=1;
                        }
                    }
                    # arrays
#                    $p =~ s/(\[.*\])$//;
#                    my $ps=$1;
#                    $a =~ s/(\[.*\])$//;
#                    my $cs=$1;
#                    if ($cs eq $ps and $cs ne "") {
#                        $ecanonical{$castcell}->{$a}=$a;
#                        $ecanonical{$cell}->{$a}=$a;
#                        $ecanonical{$castcell}->{$p}=$a;
#                        $ecanonical{$cell}->{$p}=$a;
#                        $realchannel{$castcell}->{$a}=$p;
#                        $realchannel{$cell}->{$a}=$p;
#                        $realchannel{$castcell}->{$p}=$p;
#                        $realchannel{$cell}->{$p}=$p;
#                    }
                }
            }
            close P;
            open (P, "<$requeryfile");
            while (<P>) {
                chomp;
                print STDERR "getports: query $cell" if $. == 1 and $debug;
                if (! /^[01] /) {
                    my ($p,$a)=split;
                    $realport{$cell}->{$a}=1;
                    $realport{$castcell}->{$a}=1;
                    $realport{$cell}->{$p}=1;
                    $realport{$castcell}->{$p}=1;
                }
            }
            close P;
            open (P, "<$mapqueryfile");
            while (<P>) {
                chomp;
                my ($v,$c)=split;
                $v2c{$cell}->{$v}=$c;
                $v2c{$castcell}->{$v}=$c;
                $c2v{$cell}->{$c}=$v;
                $c2v{$castcell}->{$c}=$v;
                # convert verilog names of subcell to cast names of subcell
                if (defined $ecanonical{$cell}->{$c}) {
                    $ecanonical{$cell}->{$v}=$ecanonical{$cell}->{$c};
                    $ecanonical{$castcell}->{$v}=$ecanonical{$cell}->{$c} if $castcell ne $cell;
                }
                else {
                    print STDERR "Error: no defined canonical cast port $c from $mapqueryfile";
                }
            }
            close P;
        }
        else {
            print STDERR "Info: No cast cell $castcell, no query done"
                if $verbose;
            close P;
        }
        if (@ports || @implied) {
            $castfound{$cell}=[@castlines];
            $castfound{$castcell}=[@castlines] if $castcell ne $cell;
            $ports{$cell}=[@ports];
            $ports{$castcell}=[@ports] if $castcell ne $cell;
            $implied{$cell}=[@implied];
            $implied{$castcell}=[@implied];
            if ($cell eq $porttopcell) {
                foreach my $port (sort keys %aliasedports) {
                    push @aliasedports, $aliasedports{$port}
                        if $aliasedports{$port} =~ / /;
                }
            }
            print STDERR "getports (query) $cell" if $debug;
            return 1;
        }
    }
    my $shortname = $castcell;
    $shortname =~ s/.*\.//;
    # keep the port order from the verilog
    if (defined ($moduleports{$shortname})) {
        $ports{$cell}=$moduleports{$shortname};
        $ports{$castcell}=$ports{$cell} if $castcell ne $cell;
        print STDERR "getports (short) $cell" if $debug;
        return 1;
    }
    if (defined ($moduleports{$castcell})) {
        $ports{$cell}=$moduleports{$castcell};
        $ports{$castcell}=$ports{$cell} if $castcell ne $cell;
        print STDERR "getports (castcell) $cell" if $debug;
        return 1;
    }
    if (defined ($moduleports{$cell})) {
        $ports{$cell}=$moduleports{$cell};
        $ports{$castcell}=$ports{$cell} if $castcell ne $cell;
        print STDERR "getports (cell) $cell" if $debug;
        return 1;
    }
    print STDERR "Error: getports: No defined SUBCKT nor cast for $castcell";
    $errors++;
    0;
}

my @speclines=();
my @castlines=();

foreach my $top (sort keys %modules) {
    # do not do external modules
    next if $external{$top};
    my %isport=();
    my %iswire=();
    my $castcell="$library.$top";
    my $st = $subtype;
    if ($top =~ /\./) {
        $castcell = $top;
        $castcell =~ s/\.([^\.]+)$//;
        $st = $1;
        $st = $subtype if $subtypedefined and $cdc_mode;
    }
    elsif ("$library.$top" eq "$argcell") {
        $st = $argsubtype;
    }
    my $tc = "$castcell.$st";
    # dump the existing cdc modules
    next if !$cdc_mode and $tc =~ /^lib\.synchronous\.conversion\.v3\./;
    if ($cdc_mode) {
        my @castcell=split(/\./, $castcell);
        $castcell="$library.$castcell[$#castcell]";
    }
    push @speclines, "module $castcell;\n";
    push @castlines, "module $library;\n";
    foreach my $im (@addimport) {
        push @speclines, "import $im;";
        push @castlines, "import $im;";
    }
    push @speclines, "" if @addimport;
    push @castlines, "" if @addimport;
    my $ltc=$tc;
    $tc =~ /.*\.([^\.]+)\.([^\.]+)$/;
    if (defined ($2) and $2 eq $st) {
        $ltc = $1;
    }
    $castcell =~ /\.([^\.]+)$/;
    my $cname=$1;
    my $istop=0;
    if ($wrap <= 0 or ($ltc ne $arg and $tc ne $arg)) {
        push @speclines, "define \"$st\"() (";
        push @castlines, "define \"$cname\"() (";
        $istop=1;
    }
    else {
        push @speclines, "define \"a\"() (";
        push @castlines, "define \"${cname}_a\"() (";
    }
    my $ptc=$tc;
    $ptc = $argparent if defined $argparent;
    $ptc = $portbase if $portbase ne "";
    $porttopcell=$ptc;
    getports($ptc);
    $localmodule=$ptc;
    foreach my $mp (@{$ports{$ptc}},@{$implied{$ptc}}) {
        next if ! defined $mp;
        my ($t,$p)=@{$mp};
        if (defined($channeldefs{$t})) {
            foreach my $ch (@{$channeldefs{$t}}) {
                my $cha=$ch;
                $cha =~ s/\.d\[(\d+)\]$/.$1/;
                if (defined $verilog{$p.$cha}) {
                    my $v=$verilog{$p.$cha};
                    $v =~ s/\[.*//;
                    $v =~ s/\..*//;
                    print STDERR "ISP 1 $v" if $debug;
                    $isport{$v}=1;
                    print STDERR "ISP 2 $verilog{$p.$cha}" if $debug;
                    $isport{$verilog{$p.$cha}}=1;
                }
                elsif (defined ($verilog{$p.$ch})) {
                    my $v=$verilog{$p.$ch};
                    $v =~ s/\[.*//;
                    $v =~ s/\..*//;
                    print STDERR "ISP 2 $v" if $debug;
                    $isport{$v}=1;
                    print STDERR "ISP 4 $verilog{$p.$ch}" if $debug;
                    $isport{$verilog{$p.$ch}}=1;
                }
            }
        }
        elsif (defined ($verilog{$p})) {
            my $v=$verilog{$p};
            $v =~ s/\[.*//;
            $v =~ s/\..*//;
            print STDERR "ISP 5 $v" if $debug;
            $isport{$v}=1;
            print STDERR "ISP 6 $p" if $debug;
            $isport{$verilog{$p}}=1;
        }
        print STDERR "ISP 7 $p" if $debug;
        $isport{$p}=1;
    }
    if ($tc =~ /^$library/) {
        foreach my $mp (@extra_ports) {
            my ($t,$n,$d)=@{$mp};
            push @{$ports{$tc}}, $mp;
            print STDERR "ISP 8 $n" if $debug;
            $isport{$n}=1;
        }
        foreach my $mp (@extra_implied) {
            my ($t,$n,$d)=@{$mp};
            push @{$implied{$tc}}, $mp;
            print STDERR "ISP 9 $n" if $debug;
            $isport{$n}=1;
        }
        if (! $isport{Vdd}) {
            $isport{Vdd}=1;
            push @{$implied{$tc}}, ["node", "Vdd", "-"];
        }
        if (! $isport{GND}) {
            $isport{GND}=1;
            push @{$implied{$tc}}, ["node", "GND", "-"];
        }
    }
    undef %specialnodes;
    %specialnodes=();
    if (defined ($castfound{$ptc})) {
        # print cast header from cast information, most accurate
        my @plines=@{$castfound{$ptc}};
        foreach my $pn (1..$#plines-1) {
            push @speclines, "$plines[$pn];";
            push @castlines, "$plines[$pn];";
        }
        push @speclines, "$plines[$#plines]";
        push @castlines, "$plines[$#plines]";
        my $parent;
        if (defined ($argparent)) {
            $parent = $argparent;
        }
        else {
            $parent = $tc;
            $parent =~ s/\.[^\.]+$//;
        }
        if ($istop) {
            if ($cast_and_spec) {
                push @speclines, "  ) <+ routed <: $castcell \x7b";
            }
            elsif ($cdc_mode) {
                push @speclines, "  ) <+ routed <: $refineparent \x7b";
            }
            else {
                push @speclines, "  ) <+ routed <: $parent \x7b";
            }
        }
        else {
            if ($cast_and_spec) {
                push @speclines, "  ) <+ unrouted <: ${castcell}_a \x7b";
            }
            elsif ($cdc_mode) {
                push @speclines, "  ) <+ unrouted <: $refineparent \x7b";
            }
            else {
                push @speclines, "  ) <+ unrouted <: $parent \x7b";
            }
        }
        if ($istop and $extra_refine ne "") {
            push @castlines, "  ) <+ ".join(" <+ ", split(/,/,$extra_refine))." <: $parent \x7b";
        }
        else {
            push @castlines, "  ) <: $parent \x7b";
        }
    }
    else {
        # find ports from cdl, not accurate, but is all we have left.
        foreach my $mp (@{$ports{$ptc}}) {
            next if ! defined $mp;
            my ($type,$port,$d)=@{$mp};
            my $dir = "+-";
            $dir = $d if defined $d;
            $dir = $portdirection{"$top $port"}
                if defined ($portdirection{"$top $port"});
            # ports sorted by name and by array index
            my $nm = $port;
            $nm =~ s/\[.*//;
            my $nv = $port;
            $nv =~ s/.*\[//;
            $nv =~ s/\]//;
            $type = typerename($type);
            my $port1=fix_node($port, __LINE__);
            if ($type =~ /node\[/ and $port1 =~ /\[/) {
                $specialnodes{$port1} = special($port1);   
                $port1=$specialnodes{$port1};
            }
            if ($type =~ /^\[/) {
                push @speclines, "    node $dir$port1$type;";
                push @castlines, "    node $dir$port1$type;";
            }
            else {
                push @speclines, "    $type $dir$port1;";
                push @castlines, "    $type $dir$port1;";
            }
        }
        $speclines[$#speclines] =~ s/;$//;
        $castlines[$#castlines] =~ s/;$//;
        push @speclines, "  ) <+ synchronous <+ unrouted <: $refineparent \x7b";
        push @castlines, "  ) <+ synchronous <: $refineparent \x7b";
    }
    # subcells block
    if ($cast_and_spec) {
        push @speclines, "  subtypes \x7b";
        push @castlines, "  [~SlackerSkipProteusSubcells -> subcells \x7b";
    }
    else {
        push @speclines, "   subcells \x7b";
        push @castlines, "   subcells \x7b";
    }
    if ( -s "$extranodes" and $tc eq $arg) {
        open (XT, "<$extranodes");
        print STDERR "Reading $extranodes" if $progress;
        while (<XT>) {
            chomp;
            push @castlines, $_;
            push @speclines, $_ if ! $cast_and_spec;
        }
        close XT;
    }
    my @temp;
    my %localaliasedports=();
    foreach my $aliases (@aliasedports) {
        my (@f)=split(/ /, $aliases);
        my $canon=shift @f;
        $localaliasedports{$canon}=1;
        foreach my $name (@f) {
            $localaliasedports{$name}=1;
            push @temp, "  $name=$canon;";
        }
    }
    if ($cast_and_spec) {
        push @castlines, @temp;
    }
    else {
        push @speclines, @temp;
    }
    undef @temp;
    # node declarations
    foreach my $wd (@{$wiretypes{$top}}) {
        my ($type,$name)=@{$wd};
        $iswire{$name}=1 if ! $isport{$name};
        $type = typerename($type);
        $name=fix_node($name, __LINE__);
        if ($type =~ /node\[/ and $name =~ /\[/) {
            $specialnodes{$name} = special($name);   
            $name=$specialnodes{$name};
        }
        if ($type =~ /^\[/) {
            push @castlines, "  node $name$type;";
            push @speclines, "  node $name$type;" if ! $cast_and_spec;
        }
        else {
            push @castlines, "  $type $name;";
            push @speclines, "  $type $name;" if ! $cast_and_spec;
        }
    }
    if (defined $assign{$top}) {
        # the first part of this is to fix a situation
        # which should not occur in a final netlist
        my %declare=();
        my %arr=();
        foreach my $assign (sort @{$assign{$top}}) {
            my ($var,$val)=split(/=/,$assign);
            $var = fix_node($var, __LINE__);
            $val = fix_node($val, __LINE__);
            my $svar = $var;
            $svar =~ s/\[.*//;
            my $pvar=$var;
            $pvar =~ s/_l.*//;
            if (! $iswire{$svar} and ! $iswire{$var} and ! $isport{$var} and ! $isport{$svar} and ! $isport{$pvar}) {
                $declare{$var}=1;
            }
            $svar = "";
            my $osvar="";
            foreach $var (sort keys %declare) {
                next if (defined($ecanonical{$localmodule}->{$var}));
                $svar = $var;
                $svar =~ s/\[(\d+)\]$//;
                my $ndx = $1;
                if ($svar ne $var) {
                    if (defined ($arr{$svar})) {
                        $arr{$svar} = $ndx+1 if $arr{$svar} < $ndx+1;
                    }
                    else {
                        $arr{$svar} = $ndx+1;
                    }
                }
            }
        }
        foreach my $arr (sort keys %arr) {
            push @castlines, "  node[$arr{$arr}] $arr;";
            push @speclines, "  node[$arr{$arr}] $arr;" if ! $cast_and_spec;
        }
        foreach my $declare (sort keys %declare) {
            push @castlines, "  node $declare;"
                if (! $declare =~ /\[\d+\]$/);
            push @speclines, "  node $declare;"
                if (! $declare =~ /\[\d+\]$/) and ! $cast_and_spec;
        }
        # this is the normal part
        foreach my $assign (sort @{$assign{$top}}) {
            my ($a,$b)=split(/=/, $assign);
            $a = fix_node($a, __LINE__);
            $b = fix_node($b, __LINE__);
            my $psignal=$a;
            $psignal =~ s/_l_.*//;
            $psignal =~ s/\..*//;
            if ($isport{$psignal}) {
                print STDERR "FSP 1 $psignal" if $debug;
                my $signal2=$a;
                $signal2 =~ s/_l_/[/g;
                $signal2 =~ s/_r_/]/g;
                $signal2 =~ s/\]\[/,/g;
                # trap false port detection
                $signal2 =~ /\]([^\]]+)$/;
                $a = $signal2 if ! defined($1) or $1 eq "";
                $a=$castname{$a} if defined $castname{$a};
            }
            else {
                print STDERR "NSP 1 $psignal" if $debug;
            }
            $psignal=$b;
            $psignal =~ s/_l_.*//;
            $psignal =~ s/\..*//;
            if ($isport{$psignal}) {
                print STDERR "FSP 2 $psignal" if $debug;
                my $signal2=$b;
                $signal2 =~ s/_l_/[/g;
                $signal2 =~ s/_r_/]/g;
                $signal2 =~ s/\]\[/,/g;
                # trap false port detection
                $signal2 =~ /\]([^\]]+)$/;
                $b = $signal2 if ! defined($1) or $1 eq "";
                $b=$castname{$b} if defined $castname{$b};
            }
            else {
                print STDERR "NSP 2 $psignal" if $debug;
            }
            $assign = "$a=$b";
            push @castlines, "  $assign;";
            push @speclines, "  $assign;" if ! $cast_and_spec;
        }
    }
    # instance calls
    my $dummycnt=100001;
    my $dummystart=$dummycnt;
    my %dummytypes=();
    foreach my $itype (@{$inst{$top}}) {
        my ($mod,$vinst)=split(/:/,$itype);
        $dummystart=$dummycnt;
        my $inst = fix_inst($vinst);
        my $type = $type{$itype};
        next if ignore_cell($type);
        if (! ($type =~ /\./)) {
            if (defined ($sxref{$type})) {
                $type = $sxref{$type};
            }
            else {
                $type = "$library.$type.$subtype";
            }
        }
        getports($type);
        my %lsignal=();
        if (defined $v2c{$type}) {
            foreach my $key (keys %{$signal{$itype}}) {
                if (defined $v2c{$type}->{$key}) {
                    $lsignal{$itype}->{$v2c{$type}->{$key}}=$signal{$itype}->{$key};
                    print STDERR "Defined port $key in $type $v2c{$type}->{$key}" if $debug;
                }
                else {
                    # maybe it is a verilog array
                    my $n=0;
                    for ($n = 0; defined $v2c{$type}->{"$key\[$n\]"}; $n++) {
                        $lsignal{$itype}->{$v2c{$type}->{"$key\[$n\]"}} = "$signal{$itype}->{$key}\[$n\]";
                        print STDERR "Defined port $key\[$n\] in $type ".$v2c{$type}->{"$key\[$n\]"} if $debug;
                    }
                    if ($n == 0) { print STDERR "Undefined port $key in $type";}
                }
            }
            %{$signal{$itype}}=%{$lsignal{$itype}};
        }
        if ( ! defined $ports{$type} ) {
            print STDERR "Error: Cannot find port list for $type";
            $errors++;
        }
        my @portlines=();
        my $basetype = $type;
        $basetype =~ s/\.\d[^\.]*$//;
        if ($cast_and_spec) {
            push @portlines, "  $basetype $inst(";
            push @speclines, "  $basetype :>";
            push @speclines, "    $type $inst;";
        }
        else {
            push @portlines, "  $type $inst(";
        }
        # for debugging only
        my $test=0;
        my %dassign=();
        my %net=map { split /=/ } @{$pins{$itype}};
        my %aliasesadded=();
        foreach my $pl (@{$ports{$type}}) {
            my ($ty,$pt2,$d)=@{$pl};
            $pt2 =~ s/^[-+]//;
            $ty = typerename($ty);
            my $found=0;
            my $auxfound=0;
            my $pt = $pt2;
            if ($ty eq "node") { # could still be problems for channels
                if (defined ($ecanonical{$basetype}->{$pt2})) {
                    $pt = $ecanonical{$basetype}->{$pt2};
                }
                else {
                    print STDERR "Warning: no canonical name for $pt2 in $basetype";
                    $warnings++;
                }
            }
            my $s = getbindsignal($type, $inst, $pt2, "");
            if ($s ne "") {
                $found=1;
                $auxfound=1;
                $net{$pt}=$s;
                push @portlines, "    $net{$pt}, // $ty $pt";
            }
            if (! $found) {
                # order is important here, so use the array
                foreach my $p (@{$pins{$itype}}) {
                    my ($port,$signal)=split(/=/,$p);
                    my $pa = $port;
                    $pa =~ s/\[.*//;
                    $pa =~ s/\..*$//;
                    $net{$port}=$signal;
                    $auxfound = 1 if $port eq $pt or $pa eq $pt or $port eq $pt2 or $pa eq $pt2;
                    next if $port ne $pt and $port ne $pt2;
                    my $port1=fix_node($port, __LINE__);
                    $found=1;
                    print STDERR "CPT 1 $signal $isport{$signal}" if $debug;
                    if ($iswire{$signal}) {
                        $signal=fix_node($signal, __LINE__);
                    }
                    elsif (! ($signal =~ /\x7b/) and $tc eq $arg and defined ($castname{$signal})) {
                        if (($signal ne $castname{$signal}) and
                               (($isport{$signal} and $isport{$castname{$signal}}) or (! $isport{$signal} and ! $isport{$castname{$signal}}))) {
                            $signal = $castname{$signal};
                            print STDERR "CPT 1a $signal $isport{$signal}" if $debug;
                        }
                    }
                    $signal =~ s/^1'b1$/Vdd/g;
                    $signal =~ s/^1'b0$/GND/g;
                    if ($signal =~ /'b/) {
                        if ($signal =~ /\x7b/) {
                            my $sig=$signal;
                            $sig =~ s/[ ,\x7b\x7d]+/ /g;
                            $sig =~ s/^ //;
                            $sig =~ s/ $//;
                            $signal="\x7b";
                            my @s=split(/ /, $sig);
                            $signal .= btolist($s[0]);
                            foreach my $n (1..$#s) {
                                $signal .= ", ";
                                $signal .= btolist($s[$n]);
                            }
                            $signal .= " \x7d";
                        }
                        else {
                            $signal = "\x7b".btolist($signal)."\x7d";
                        }
                    }
                    # reverse order of arrays
                    if ($signal =~ /\x7b/) {
                        $signal =~ s/[ ,\x7b\x7d]+/ /g;
                        $signal =~ s/^ //;
                        $signal =~ s/ $//;
                        my @signal=();
                        my @s=split(/ /,$signal);
                        foreach my $s (@s) {
                            if ( $s =~ /(.*)\[(\d+):(\d+)\]$/) {
                                my $name=$1;
                                my $e=$2;
                                my $b=$3;
                                if ($b <= $e ) {
                                    for (my $n = $e; $n >= $b; $n--) {
                                        push @signal, "$name\[$n\]";
                                    }
                                }
                                else {
                                    for (my $n = $e; $n <= $b; $n++) {
                                        push @signal, "$name\[$n\]";
                                    }
                                }
                            }
                            else {
                                push @signal,$s;
                            }
                        }
                        foreach my $s (@signal) {
                            print STDERR "CPT 2 $s $isport{$s}" if $debug;
                            if ($iswire{$s}) {
                                $s = fix_node($s, __LINE__);
                            }
                            elsif (! ($s =~ /\x7b/) and $tc eq $arg and defined ($castname{$s})) {
                                if (($s ne $castname{$s}) and
                                        (($isport{$s} and $isport{$castname{$s}}) or (! $isport{$s} and ! $isport{$castname{$s}}))) {
                                    $s = $castname{$s};
                                    print STDERR "CPT 2a $s $isport{$s}" if $debug;
                                }
                                $s = $castname{$s};
                            }
                        }
                        my $fxsig;
                        if ($#signal > 0) {
                            $fxsig = fix_node($signal[$#signal], __LINE__);
                            push @portlines, "    \x7b $fxsig, ";
                            for (my $i = $#signal-1; $i > 0; $i--) {
                                $fxsig = fix_node($signal[$i], __LINE__);
                                push @portlines, "      $fxsig,";
                            }
                            $fxsig = fix_node($signal[0], __LINE__);
                            push @portlines, "      $fxsig \x7d, // $ty $port1";
                        }
                        else {
                            $fxsig = fix_node($signal[0], __LINE__);
                            push @portlines, "    \x7b $fxsig \x7d, // $ty $port1";
                        }
                    }
                    else {
                        my $signal1=fix_node($signal, __LINE__);
                        my $psignal=$signal1;
                        $psignal =~ s/_l_.*//;
                        $psignal =~ s/\..*//;
                        print STDERR "CPT 3 $psignal $isport{$psignal}" if $debug;
                        if ($isport{$psignal}) {
                            my $signal2=$signal1;
                            $signal2 =~ s/_l_/[/g;
                            $signal2 =~ s/_r_/]/g;
                            $signal2 =~ s/\]\[/,/g;
                            # trap false port detection
                            $signal2 =~ /\]([^\]]+)$/;
                            $signal1 = $signal2 if ! defined($1) or $1 eq "";
                            $signal1=$castname{$signal1} if defined $castname{$signal1};
                        }
                        if ($signal1 =~ /(.*)\[(\d+):(\d+)\]$/) {
                            my $name=$1;
                            my $start=$3;
                            my $end=$2;
                            my @arr=();
                            for (my $n = $start; $n <= $end; $n++) {
                                push @arr, "$name\[$n\]";
                            }
                            push @portlines, "    \x7b";
                            push @portlines, "     ".join(",\n     ", @arr);
                            push @portlines, "    \x7d, // $port1";
                        }
                        else {
                            push @portlines, "    $signal1, // $port1";
                        }
                    }
                    last;
                }
            }
            if (! $found) {
                if ($pt eq "VDD10") {
                    push @portlines, "    Vdd, // $ty $pt";
                }
                elsif ($pt eq "VSS10") {
                    push @portlines, "    GND, // $ty $pt";
                }
                else {
                    my $pt1=fix_node($pt2, __LINE__);
                    my $tysuffix="";
                    my $typrefix=$ty;
                    if ($typrefix =~ s/\[(\d+)\.\.(\d+)\]$//) {
                        $tysuffix="[$1..$2]";
                    }
                    elsif ($typrefix =~ s/\[(\d+)\.\.(\d+),(\d+)\.\.(\d+)\]$//) {
                        $tysuffix="[$1..$2,$3..$4]";
                    }
                    my $portchannelcount=0;
                    my $portchannelconnect=0;
                    my $portchannel="";
                    my $print = 0;
                    if (defined($channeldefs{$ty})) {
                        my $match=0;
                        my $chcnt=0;
                        my @chlines=();
                        my $sigroot="";
                        foreach my $ch (@{$channeldefs{$ty}}) {
                            $chcnt++;
                            my $lm=0;
                            $portchannelcount++;
                            my $portmatch=0;
                            my $lastsignal;
                            my $cha = $ch;
                            $cha =~ s/\.d$/.0/;
                            $cha =~ s/\.d\[(\d+)\]$/.$1/;
                            my $ctype=$type;
                            $ctype=~s/\.[^\.]+$//;
                            my $ec=$ecanonical{$ctype}->{"$pt2$ch"};
                            $ec = "" if ! defined $ec;
                            my $eca=$ecanonical{$ctype}->{"$pt2$cha"};
                            $eca = "" if ! defined $eca;
                            my $signal=undef;
                            my $port=undef;
                            if (defined($signal{$itype}->{$pt2.$ch})) {
                                $port=$pt2.$ch;
                            }
                            elsif (defined($signal{$itype}->{$pt2.$cha})) {
                                $port=$pt2.$cha;
                            }
                            elsif (defined($signal{$itype}->{$ec})) {
                                $port=$ec;
                            }
                            elsif (defined($signal{$itype}->{$eca})) {
                                $port=$eca;
                            }
                            if (defined($port)) {
                                $signal = $signal{$itype}->{$port};
                                $signal = "Vdd" if $signal eq "1'b1";
                                $signal = "GND" if $signal eq "1'b0";
                                print STDERR "CPT 4 $signal $isport{$signal}" if $debug;
                                if ($iswire{$signal}) {
                                    $signal = fix_node($signal, __LINE__);
                                }
                                elsif (! ($signal =~ /\x7b/) and ! $isport{$signal} and $tc eq $arg) {
                                    $signal=$castname{$signal} if defined $castname{$signal};
                                    $signal=$castname{"$signal\[0\]"} if defined $castname{"$signal\[0\]"};
                                }
                                $lastsignal=$signal;
                                $sigroot="" if (! defined ($sigroot));
                                my $chroot=$signal;
                                $chroot =~ s/\.[^\.]+$//;
                                $chroot =~ s/\]\[/,/g;
                                if ($sigroot eq "") {
                                    $sigroot = substr($signal,0,length($signal)-length($ch));
                                }
                                if (substr($signal,0,length($signal)-length($ch)) eq $sigroot) {
                                    $match++;
                                }
                                if (($portchannel eq "" or $portchannel eq $chroot)) {
                                    $portchannelconnect++;
                                    $portchannel = $chroot if $portchannel eq "";
                                    $portmatch=1;
                                }
                                $signal=fix_node($signal, __LINE__);
                                $signal = $castname{$signal} if defined $castname{$signal};
                                push @chlines, "    dummy$dummycnt$ch=$signal;";
                                $lm=1;
                            }
                            my $ps=$lastsignal;
                            $ps =~ s/\.[^\.]+$//;
                            if (! $lm) {
                                if ($ps eq $portchannel) {
                                    if ($lastsignal eq "") {
                                        my $sig="dummy$dummycnt$ch";
                                        push @chlines, "//  unconnected $sig";
                                        push @chlines, "    $sig=GND;" if ! defined $aliasesadded{$sig};
                                        $aliasesadded{$sig}=1;
                                    }
                                    else {
                                        push @chlines, "//  unconnected $lastsignal $portchannel";
                                        push @chlines, "    $lastsignal=GND;" if ! defined $aliasesadded{$lastsignal};
                                        $aliasesadded{$lastsignal}=1;
                                    }
                                }
                                elsif ((! defined($realport{$ctype}->{"$pt2$ch"}) and $eicount{$ctype}->{$ec} == 0) or $cpdirection{$ctype}->{"$pt2$ch"} eq "-") {
                                    push @chlines, "//  unconnected node $pt2$ch of $basetype";
                                    # only for inputs, bidi and outputs ignored, bug 14646
                                    push @chlines, "    dummy$dummycnt$ch=GND;";
                                }
                                elsif ( $cpdirection{$ctype}->{"$pt2$ch"} ne "-") {
                                    push @chlines, "//  unconnected node $pt2$ch of $basetype";
                                }
                                else {
                                    push @chlines, "//  unconnected node $pt2$ch of $basetype";
                                    print STDERR "Error: unconnected $pt2$ch of $basetype";
                                    $errors++;
                                }
                            }
                        }
                        print STDERR "CDEF $ty $match=$chcnt $sigroot ".join(',',@{$channeldefs{$ty}}) if $debug;
                        if ($portchannelcount != $portchannelconnect or 1) {
                            push @castlines, "    $typrefix dummy$dummycnt$tysuffix;";
                            push @speclines, "    $typrefix dummy$dummycnt$tysuffix;" if ! $cast_and_spec;
                            push @castlines, @chlines;
                            push @speclines, @chlines if ! $cast_and_spec;
                            push @portlines, "    dummy$dummycnt, // $ty $pt1";
                            $auxfound=1;
                        }
                        else {
                            push @portlines, "    $portchannel, // $ty $pt1";
                            $dummycnt--;
                        }
                    }
                    elsif ( $ty =~ /^node\[(\d+)(?:\.\.(\d+))?\]$/ ) {
                        # todo try to match up node arrays to ports
                        push @portlines, "    dummy$dummycnt, // $ty $pt1";
                        push @castlines, "    $typrefix dummy$dummycnt$tysuffix;";
                        push @speclines, "    $typrefix dummy$dummycnt$tysuffix;" if ! $cast_and_spec;
                        $auxfound=1;
                        my ($al, $ah) = ($1, $2);
                        if (!defined($ah)) {
                            $ah = $al - 1;
                            $al = 0;
                        }
                        for (my $n = $al; $n <= $ah; $n++) {
                            my $ch = "\[$n\]";
                            foreach my $p (@{$pins{$itype}}) {
                                my ($port,$signal)=split(/=/,$p);
                                $signal = "Vdd" if $signal eq "1'b1";
                                $signal = "GND" if $signal eq "1'b0";
                                print STDERR "CPT 5 $signal $isport{$signal}" if $debug;
                                if ($iswire{$signal}) {
                                    $signal=fix_node($signal, __LINE__);
                                }
                                elsif (! ($signal =~ /\x7b/) and ! $isport{$signal} and $tc eq $arg) {
                                    $signal=$castname{$signal} if defined $castname{$signal};
                                    $signal=$castname{"$signal\[0\]"} if defined $castname{"$signal\[0\]"};
                                }
                                if ($port eq "$pt2$ch") {
                                    my $fxname=fix_node($signal, __LINE__);
                                    $fxname =~ s/\$/_/g;
                                    $fxname = $castname{$fxname} if defined $castname{$fxname};
                                    push @castlines, "    dummy$dummycnt$ch=$fxname;";
                                    push @speclines, "    dummy$dummycnt$ch=$fxname;" if ! $cast_and_spec;
                                    last;
                                }
                            }
                        }
                    }
                    elsif (! ($ty =~ /^node$/)) {
                        # todo try to match up nodes to ports, but may already be done!
                        push @portlines, "    dummy$dummycnt, // $ty $pt1";
                        push @castlines, "    $typrefix dummy$dummycnt$tysuffix;";
                        push @speclines, "    $typrefix dummy$dummycnt$tysuffix;" if ! $cast_and_spec;
                        print STDERR "NDEF1 $ty $itype $inst" if $debug;
                        $auxfound=1;
                    }
                    else {
                        push @portlines, "    dummy$dummycnt, // $ty $pt1";
                        push @castlines, "    $typrefix dummy$dummycnt$tysuffix;";
                        push @speclines, "    $typrefix dummy$dummycnt$tysuffix;" if ! $cast_and_spec;
                        print STDERR "NDEF2 $ty $itype $inst" if $debug;
                        $auxfound=1;
                    }
                    $dummycnt++;
                }
            }
            print STDERR "Warning: port $pt2 may not be connected in $type:$itype"
                if $warning and ! $auxfound;
            $warnings++ if ! $auxfound;
        }
        $portlines[$#portlines] =~ s/,\s/ /;
        # need implied ports
        if (defined ($implied{$type})) {
            my @imp=();
            my $allmatch=1;
            foreach my $ip (@{$implied{$type}}) {
                my ($it, $in1, $id)=@{$ip};
                $it = typerename($it);
                $in1 = "aclk" if ($in1 eq "debug_ack"); #HACK
                # implied ports may be explictily connected
                my $in = $in1;
                if ($it eq "node") {
                    if (defined ($ecanonical{$basetype}->{$in1})) {
                        $in = $ecanonical{$basetype}->{$in1};
                    }
                    else {
                        print STDERR "Warning: no canonical name for $in1 in $basetype";
                        $warnings++;
                    }
                }
                my $s = getbindsignal($type, $inst, $in1, "");
                my $bound=0;
                if ($s ne "") {
                    $net{$in}=$s;
                    $bound=1;
                }
                print STDERR "CPT 6 $in $isport{$in}" if $debug;
                if (defined $net{$in}) {
                    if ($bound) {
                        push @imp, $net{$in};
                    }
                    else {
                        my $signal1=$net{$in};
                        if ($signal1 =~ /(.*)\[(\d+)\]\.(.*)/) {
                            $signal1 = "${1}_l_${2}_r__D_${3}";
                        }
                        elsif ($signal1 =~ /(.*)\[(\d+)\](.*)/) {
                            $signal1 = "${1}_l_${2}_r_${3}";
                        }
                        $signal1 = $castname{$signal1}
                            if (defined ($castname{$signal1}));
                        push @imp, fix_node($signal1, __LINE__);
                    }
                    $allmatch=0;
                }
                # implied ports are connected by name
                elsif ($isport{$in} or $iswire{$in}) {
                    $in = $castname{$in}
                        if (defined ($castname{$in}));
                    push @imp, fix_node($in, __LINE__);
                }
                # verilog does not have power pins
                elsif ($in eq "Vdd" or $in eq "GND") {
                    push @imp, $in;
                }
                # special case power names
                elsif ($in eq "VDD") {
                    push @imp, "Vdd";
                    $allmatch=0;
                }
                elsif ($in eq "vss" or $in eq "VSS") {
                    push @imp, "GND";
                    $allmatch=0;
                }
                # everything else
                else {
                    $allmatch=0;
                    push @castlines, "    $it dummy$dummycnt;";
                    push @speclines, "    $it dummy$dummycnt;" if ! $cast_and_spec;
                    push @imp, "dummy$dummycnt /* $in */";
                    $dummycnt++;
                }
            }
            if ($allmatch) {
                push @portlines, "  );";
            }
            else {
                push @portlines, "  )(".join(", ",@imp).");";
            }
        }
        else {
            push @portlines, "  );";
        }
        if ($cast_and_spec) {
            push @castlines,@portlines;
        }
        else {
            push @speclines,@portlines;
        }
        @portlines=();
    }
    # end subcells/subtypes
    my @subcells_directives_data=();
    my @cell_directives_data=();
    my $fh;
    if (open ($fh, "<$directives_file")) {
        print STDERR "Reading $directives_file" if $verbose;
        while (<$fh>) {
            chomp;
            s/\\//g;
            if (/^\s*(extra_delay|slew_signoff|skew_signoff)\(\s*(.*)\)\s*=\s*(.*)\s*;$/ ) {
                my $directive=$1;
                my $node = $2;
                my $delay = $3;
                $node =~ s/\\//g;
                $node =~ s/\s//g;
                if ($node =~ /\//) {
                    $node =~ m/(.*)\/(.*)/;
                    my $tail=$2;
                    my $inst=$1;
                    # the directive instances are not proper verilog instances
                    $inst =~ s/,/][/g;
                    $inst = fix_inst($inst);
                    $node = $inst.".".$tail;
                    my $inode=nodecad2cast($node);
                    $node = $inode if $inode ne "";
                }
                else {
                    $node=fix_node($node, __LINE__);
                    my $inode=nodecad2cast($node);
                    $node = $inode if $inode ne "";
                }
                push @subcells_directives_data, "$directive($node)=$delay;";
            }
            elsif (/^\s*delaybias\s*\(\s*(\S+)\s*\)\s*=\s*(\S+)\s*;/) {
                my $inst = fix_inst($1);
                my $value= $2;
                push @subcells_directives_data, "delaybias($inst)=$value;"
                    if $value != 1;
            }
            elsif (/^\s*slacker_leaf\s*=\s*(\S+)\s*;/) {
                push @cell_directives_data, "slacker_leaf=$1;"
                    if $istop;
            }
            elsif (/^\s*(slacker_time|slacker_alignment|slacker_ignore)\s*\(\s*(\S+)\s*\)\s*=\s*(\S+)\s*;/) {
                my $dir=$1;
                my $channel=$2;
                my $value=$3;
                $channel =~ s/\]\[/,/g;
                $channel = $realchannel{$localmodule}->{$channel} if defined $realchannel{$localmodule}->{$channel};
                push @cell_directives_data, "$dir($channel)=$value;"
                    if $istop;
            }
        }
        close $fh;
    }
    if (@subcells_directives_data) {
        if ($cast_and_spec) {
            push @castlines, "    directives \x7b";
            foreach my $line (@subcells_directives_data) {
                push @castlines, "      $line";
            }
            push @castlines, "    \x7d";
        }
        else {
            push @speclines, "    directives \x7b";
            foreach my $line (@subcells_directives_data) {
                push @speclines, "      $line";
            }
            push @speclines, "    \x7d";
        }
    }
    push @speclines, "  \x7d";
    if ($cast_and_spec) {
        push @castlines, "  \x7d]";
    }
    else {
        push @castlines, "  \x7d";
    }
    # end module
    push @cell_directives_data, "proteus_tag = $proteus_tag;";
    if (@cell_directives_data) {
        if ($cast_and_spec) {
            push @castlines, "  directives \x7b";
            foreach my $line (@cell_directives_data) {
                push @castlines, "    $line";
            }
            push @castlines, "  \x7d";
            if ($wrap == 0) {
                push @speclines, "  directives \x7b";
                push @speclines, "    fixed_size = true;";
                push @speclines, "  \x7d";
            }
        }
        else {
            push @speclines, "  directives \x7b";
            push @speclines, "    fixed_size = true;";
            foreach my $line (@cell_directives_data) {
                push @speclines, "    $line";
            }
            push @speclines, "  \x7d";
        }
    }
    push @speclines, "\x7d";
    push @castlines, "\x7d";
}
## wrappers for canonicalization issues
my $char = 97;
for(; $wrap > 0; $wrap--) {
    # do not do external modules
    my $top = $arg;
    $char++;
    my %isport=();
    my %iswire=();
    my $castcell="$library.$top";
    my $st = $subtype;
    if ($top =~ /\./) {
        $castcell = $top;
        $castcell =~ s/\.([^\.]+)$//;
        $st = $1;
    }
    elsif ("$library.$top" eq "$argcell") {
        $st = $argsubtype;
    }
    my $tc = "$castcell.$st";
    # dump the existing cdc modules
    push @speclines, "module $castcell;\n";
    push @castlines, "module $library;\n";
    foreach my $im (@addimport) {
        push @speclines, "import $im;";
        push @castlines, "import $im;";
    }
    push @speclines, "" if @addimport;
    push @castlines, "" if @addimport;
    $castcell =~ /\.([^\.]+)$/;
    my $cname = $1;
    my $istop=0;
    if ($wrap == 1) {
        push @speclines, "define \"$st\"() (";
        push @castlines, "define \"$cname\"() (";
        $istop=1;
    }
    else {
        push @speclines, sprintf "define \"%c\"() (", $char;
        push @castlines, sprintf "define \"${cname}_%c\"() (", $char;
    }
    my $ptc=$tc;
    $ptc = $argparent if defined $argparent;
    $ptc = $portbase if $portbase ne "";
    getports($ptc);
    foreach my $mp (@{$ports{$ptc}}) {
        my ($t,$p,$d)=@{$mp};
        print STDERR "ISP 9 $p" if $debug;
        $isport{$p}=1;
    }
    foreach my $mp (@{$implied{$ptc}}) {
        my ($t,$p,$d)=@{$mp};
        print STDERR "ISP 10 $p" if $debug;
        $isport{$p}=1;
    }
    if ($tc =~ /^$library/) {
        if (! $isport{Vdd}) {
            $isport{Vdd}=1;
            $isport{GND}=1;
            push @{$implied{$tc}}, ["node", "Vdd", "-"];
            push @{$implied{$tc}}, ["node", "GND", "-"];
        }
    }
    if (defined ($castfound{$ptc})) {
        print STDERR "Cast Found for $ptc" if $verbose;
        # print cast header from cast information, most accurate
        my @plines=@{$castfound{$ptc}};
        foreach my $pn (1..$#plines-1) {
            push @speclines, "$plines[$pn];";
            push @castlines, "$plines[$pn];";
        }
        push @speclines, "$plines[$#plines]";
        push @castlines, "$plines[$#plines]";
        my $parent;
        if ( defined ($argparent) ) {
            $parent = $argparent;
        }
        else {
            $parent = $tc;
            $parent =~ s/\.[^\.]+$//;
        }
        if ($wrap == 1) {
            if ($cast_and_spec) {
                push @speclines, "  ) <+ routed <: $castcell \x7b";
            }
            else {
                push @speclines, "  ) <+ routed <: $parent \x7b";
            }
        }
        else {
            if ($cast_and_spec) {
                push @speclines, sprintf "  ) <+ unrouted <: ${castcell}_%c \x7b", $char;
            }
            else {
                push @speclines, "  ) <+ unrouted <: $parent \x7b";
            }
        }
        if ($istop and $extra_refine ne "") {
            push @castlines, "  ) <+ ".join(" <+ ", split(/,/,$extra_refine))." <: $parent \x7b";
        }
        else {
            push @castlines, "  ) <: $parent \x7b";
        }
    }
    else {
        # find ports from cdl, not accurate, but is all we have left.
        foreach my $mp (@{$ports{$tc}}) {
            my ($type,$port,$d)=@{$mp};
            my $dir = "+-";
            $dir = $d if defined $d;
            $dir = $portdirection{"$top $port"}
                if defined ($portdirection{"$top $port"});
            # ports sorted by name and by array index
            my $nm = $port;
            $nm =~ s/\[.*//;
            my $nv = $port;
            $nv =~ s/.*\[//;
            $nv =~ s/\]//;
            $type = typerename($type);
            my $port1=fix_node($port, __LINE__);
            if ($type =~ /node\[/ and $port1 =~ /\[/) {
                $specialnodes{$port1} = special($port1);   
                $port1=$specialnodes{$port1};
            }
            if ($type =~ /^\[/) {
                push @speclines, "    node $dir$port1$type;";
                push @castlines, "    node $dir$port1$type;";
            }
            else {
                push @speclines, "    $type $dir$port1;";
                push @castlines, "    $type $dir$port1;";
            }
        }
        $speclines[$#speclines] =~ s/;$//;
        $castlines[$#castlines] =~ s/;$//;
        push @speclines, "  ) <+ synchronous <+ unrouted <: $refineparent \x7b";
        push @castlines, "  ) <+ synchronous <: $refineparent \x7b";
    }
    # subcells block
    if ($cast_and_spec) {
        push @speclines, "  subtypes \x7b";
    }
    else {
        push @speclines, "  subcells \x7b";
    }
    push @castlines, "  subcells \x7b";
    if ($cast_and_spec) {
        push @castlines, sprintf "    ${castcell}_%c z(", $char-1;
        push @speclines, sprintf "    ${castcell}_%c :> \n".
                                 "       ${castcell}.%c z;", $char-1, $char-1;
    }
    else {
        push @speclines, sprintf "    $castcell.%c z(", $char-1;
    }
    my @plines=();
    @plines=@{$castfound{$ptc}} if defined ($castfound{$ptc});
    if (@plines) {
        my $ln;
        foreach my $pn (1..$#plines-1) {
            $ln = $plines[$pn];
            $ln =~ s/^ *//;
            $ln =~ s/^[a-z].* //;
            $ln =~ s/[-+]//;
            $ln =~ s/\[[^\]]+\]//;
            push @speclines, "        $ln,"
                if ! $cast_and_spec;
            push @castlines, "        $ln,";
        }
        $ln = $plines[$#plines];
        $ln =~ s/^ *//;
        $ln =~ s/^[a-z].* //;
        $ln =~ s/[-+]//;
        $ln =~ s/\[[^\]]+\]//;
        push @speclines, "        $ln"
            if ! $cast_and_spec;
        push @castlines, "        $ln";
    }
    else {
        # find ports from cdl, not accurate, but is all we have left.
        my @pl=();
        foreach my $mp (@{$ports{$tc}}) {
            my ($type,$port,$d)=@{$mp};
            my $dir = "+-";
            $dir = $d if defined $d;
            $dir = $portdirection{"$top $port"}
                if defined ($portdirection{"$top $port"});
            # ports sorted by name and by array index
            my $nm = $port;
            $nm =~ s/\[.*//;
            my $nv = $port;
            $nv =~ s/.*\[//;
            $nv =~ s/\]//;
            $type = typerename($type);
            my $port1=fix_node($port, __LINE__);
            if ($type =~ /node\[/ and $port1 =~ /\[/) {
                $specialnodes{$port1} = special($port1);   
                $port1=$specialnodes{$port1};
            }
            push @pl, $port1;
        }
        push @speclines, "        ".join(",\n        ",@pl);
        push @castlines, "        ".join(",\n        ",@pl);
    }
    push @speclines, "      );" if ! $cast_and_spec;
    push @castlines, "      );";
    # end subcells
    my @subcells_directives_data=();
    my @cell_directives_data=();
    my $fh;
    if (open ($fh, "<$directives_file")) {
        print STDERR "Reading $directives_file" if $verbose;
        while (<$fh>) {
            chomp;
            s/\\//g;
            if (/^\s*(extra_delay|slew_signoff|skew_signoff)\(\s*(.*)\)\s*=\s*(.*)\s*;$/ ) {
                my $directive=$1;
                my $node = $2;
                my $delay = $3;
                $node =~ s/\\//g;
                $node =~ s/\s//g;
                if ($node =~ /\//) {
                    $node =~ m/(.*)\/(.*)/;
                    my $tail=$2;
                    my $inst=$1;
                    # the directive instances are not proper verilog instances
                    $inst =~ s/,/][/g;
                    $inst = fix_inst($inst);
                    $node = $inst.".".$tail;
                    my $inode=nodecad2cast($node);
                    $node = $inode if $inode ne "";
                }
                else {
                    $node=fix_node($node, __LINE__);
                    my $inode=nodecad2cast($node);
                    $node = $inode if $inode ne "";
                }
                push @subcells_directives_data, "$directive($node)=$delay;";
            }
            elsif (/^\s*delaybias\s*\(\s*(\S+)\s*\)\s*=\s*(\S+)\s*;/) {
                my $inst = fix_inst($1);
                my $value= $2;
                push @subcells_directives_data, "delaybias($inst)=$value;"
                    if $value != 1;
            }
            elsif (/^\s*slacker_leaf\s*=\s*(\S+)\s*;/) {
                push @cell_directives_data, "slacker_leaf=$1;"
                    if $istop;
            }
            elsif (/^\s*(slacker_time|slacker_alignment|slacker_ignore)\s*\(\s*(\S+)\s*\)\s*=\s*(\S+)\s*;/) {
                my $dir=$1;
                my $channel=$2;
                my $value=$3;
                $channel =~ s/\]\[/,/g;
                $channel = $realchannel{$localmodule}->{$channel} if defined $realchannel{$localmodule}->{$channel};
                push @cell_directives_data, "$dir($channel)=$value;"
                    if $istop;
            }
        }
        close $fh;
    }
    if (@subcells_directives_data) {
        if ($cast_and_spec) {
            push @castlines, "    directives \x7b";
            foreach my $line (@subcells_directives_data) {
                push @castlines, "      $line";
            }
            push @castlines, "    \x7d";
        }
        else {
            push @speclines, "    directives \x7b";
            foreach my $line (@subcells_directives_data) {
                push @speclines, "      $line";
            }
            push @speclines, "    \x7d";
        }
    }
    push @speclines, "  \x7d";
    push @castlines, "  \x7d";
    # end module
    if (@cell_directives_data) {
        if ($cast_and_spec) {
            if (@cell_directives_data) {
                push @castlines, "  directives \x7b";
                foreach my $line (@cell_directives_data) {
                    push @castlines, "    $line";
                }
                push @castlines, "  \x7d";
                if ($wrap == 1) {
                    push @speclines, "  directives \x7b";
                    push @speclines, "    fixed_size = true;";
                    push @speclines, "  \x7d";
                }
            }
            else {
                push @speclines, "  directives \x7b";
                push @speclines, "    fixed_size = true;";
                foreach my $line (@cell_directives_data) {
                    push @speclines, "    $line";
                }
                push @speclines, "  \x7d";
            }
        }
    }
    push @speclines, "\x7d";
    push @castlines, "\x7d";
}
## end wrappers
if ($debug) {
    my $fd;
    open ($fd, ">castlines.cast");
    print $fd join("\n", @castlines);
    close $fd;
    open ($fd, ">speclines.cast");
    print $fd join("\n", @castlines);
    close $fd;
}

## create cast file
if ($cast_and_spec) {
    if ($castlines[0] =~ /^module\s+(.*)\s*;/) {
        my $dir=$1;
        $dir =~ s/\./\//g;
        $dir =~ s/\/([^\/]+)$//;
        $dir = "$castdir/$dir";
        my $file = $1.".cast";
        system("mkdir","-p",$dir) if ! -d $dir;
        my $fh;
        if (open ($fh, ">$dir/$file")) {
            print STDERR "Writing $dir/$file" if $verbose;
            cast_header ($fh);
            print $fh "$castlines[0]\n";
            my $abstractlibrary=$argparent;
            $abstractlibrary =~ s/\.[^\.]+$//;
            if ($abstractlibrary eq $library) {
                my $fx;
                $outputfile =~ /\.([^\.]+)\.cast$/;
                my $cell=$1;
                my $cmd= "image2cast";
                $cmd .= " --cell='$cell'";
                $cmd .= " --base='ABSTRACT_$cell'";
                $cmd .= " '${cell}.qdi_image.v'";
                print STDERR $cmd if $verbose;
                open $fx, "$cmd |";
                while (<$fx>) {
                    chomp;
                    print $fh $_;
                }
                close $fx;
                print $fh "";
            }
            for (my $n = 1; $n <= $#castlines; $n++) {
                print $fh $castlines[$n] if ! ($castlines[$n] =~ /^module/);
            }
            close $fh;
        }
        else {
            print STDERR "Error: failed to create $dir/$file";
            $errors++;
        }
    }
    else {
        print STDERR "Error: internal error: poorly formed castlines";
        $errors++;
    }
}
foreach my $line (@speclines) {
#    $line =~ s/_r__l_/_c_/g;
    $_=$line;
    if (/^module/) {
        domodule(0);
        $mname = $_;
        chomp $mname;
        $mname =~ s/;$//;
        $mname =~ s/.* //;
    }
    $_=$line;
    $portlist = 0 if /</;
    $instlist = 1 if /\(/;
    if (! $portlist and /^ *node /) {
        my $lx = $_;
        chomp $lx;
        $lx =~ s/^  *//;
        $lx =~ s/;//;
        my @f=split(/ /,$lx);
    }
    if ($instlist and (/=/ or /,/ or (/ \/\//))) {
        if (/=/) {
            my $lx = $_;
            chomp $lx;
            $lx =~ s/[ ;]//g;
            my ($sl,$sr)=split(/=/, $lx);
            $nodes{$sl}++ if defined $nodes{$sl};
            $nodes{$sr}++ if defined $nodes{$sr};
        }
        elsif ((/,/) or (/ \/\//)) {
            my $lx = $_;
            chomp $lx;
            $lx =~ s/\/\*.*\*\///g;
            $lx =~ s/ *\/\/.*//;
            $lx =~ s/[ ,{}\(\);]+/ /g;
            $lx =~ s/^ //;
            $lx =~ s/ $//;
            my @lx = split(/ /,$lx);
            foreach my $n (@lx) {
                $nodes{$n}++ if defined $nodes{$n};
            }
        }
    }
    push @module, "$_";
}
domodule(0);
foreach my $inst (sort keys %bindinstlist) {
    print STDERR "Warning instance $inst from vsbind not in verilog"
        if ! $bindinstlist{$inst};
    $warnings++ if ! $bindinstlist{$inst};
}
foreach my $cell (sort keys %bindcelllist) {
    print STDERR "Warning cell $cell from vsbind not in verilog"
        if ! $bindcelllist{$cell};
    $warnings++ if ! $bindcelllist{$cell};
}
my $prefix="";
for (my $n = 0; $n < $argwrap; $n++) {
    $prefix = $prefix."z.";
}
my $fxr;
my $fxril;
my $xcell="$library.$argcell.$subtype";
$xcell = $argcell if $argcell =~ /\./;
open ($fxr, ">$xcell.instxref");
mkdir "cast2skill";
open ($fxril, ">cast2skill/$xcell.instmap.il");
print STDERR "Writing cast2skill/$xcell.instmap.il" if $verbose;
print STDERR "Writing $xcell.instxref" if $verbose;
my $ic=0;
my $ci=100000;
foreach my $inst (sort keys %instlookup) {
    if ($ci > 4000) {
        if ($ic > 0) {
            print $fxril "    Table ) )\n";
        }
        $ic++;
        print $fxril "(defun PinInstXrefTable$ic ( )";
        print $fxril "  (let (";
        print $fxril "    ( Table ( makeTable `pd nil ) ) )";
        $ci=0;
    }
    my $ixref=instcast2cad($prefix.$instlookup{$inst});
    print $fxr "$inst $prefix$instlookup{$inst}";
    print $fxril "          ( setarray Table \"$inst\" \"$ixref\" )";
    $ci++;
}
close $fxr;
print $fxril "    Table ) )\n";
print $fxril "(defun PinInstXrefTable ( )";
print $fxril "  (let ( Key map";
print $fxril "        ( Table ( makeTable `pd nil ) ) )";
for (my $n = 1; $n <= $ic; $n++) {
    print $fxril "        map = PinInstXrefTable$n( )";
    print $fxril "        foreach( Key map";
    print $fxril "            ( setarray Table Key arrayref( map Key ) ) )";
}
print $fxril "    Table ) )";
close $fxril;
open ($fxr, ">$xcell.nodexref");
open ($fxril, ">cast2skill/$xcell.nodemap.il");
print STDERR "Writing $xcell.nodexref" if $verbose;
print STDERR "Writing cast2skill/$xcell.nodemap.il" if $verbose;
my $ic=0;
my $ci=100000;
foreach my $node (sort keys %nodexref) {
    if ($ci > 4000) {
        if ($ic > 0) {
            print $fxril "    Table ) )\n";
        }
        $ic++;
        print $fxril "(defun PinNodeXrefTable$ic ( )";
        print $fxril "  (let (";
        print $fxril "    ( Table ( makeTable `pd nil ) ) )";
        $ci=0;
    }
    # cannot translate these names to cadence names because they have '$' in them!
    my $n1=$node;
    my $n2=$nodexref{$node};
    print $fxr "$node $nodexref{$node}";
    print $fxril "          ( setarray Table \"$n1\" \"$n2\" )";
    $ci++;
}
close $fxr;
print $fxril "    Table ) )\n";
print $fxril "(defun PinNodeXrefTable ( )";
print $fxril "  (let ( Key map";
print $fxril "        ( Table ( makeTable `pd nil ) ) )";
for (my $n = 1; $n <= $ic; $n++) {
    print $fxril "        map = PinNodeXrefTable$n( )";
    print $fxril "        foreach( Key map";
    print $fxril "            ( setarray Table Key arrayref( map Key ) ) )";
}
print $fxril "    Table ) )";
close $fxril;

open ($fxr, ">$porttopcell.rcanon");
open ($fxril, ">cast2skill/$porttopcell.rcanon.il");
print STDERR "Writing $porttopcell.rcanon" if $verbose;
print STDERR "Writing cast2skill/$porttopcell.rcanon.il" if $verbose;
my $ic=0;
my $ci=100000;
my %x=();
my $ptc=$porttopcell;
$ptc =~ s/\.\d[^\.]*$//;
%x=%{$revlookup{$porttopcell}} if defined $revlookup{$ptc};
foreach my $node (sort keys %x) {
    if ($ci > 4000) {
        if ($ic > 0) {
            print $fxril "    Table ) )\n";
        }
        $ic++;
        print $fxril "(defun PinRevCanonicalTable$ic ( )";
        print $fxril "  (let (";
        print $fxril "    ( Table ( makeTable `pd nil ) ) )";
        $ci=0;
    }
    my $n1=nodecast2cad($node);
    my $n2=nodecast2cad($x{$node});
    print $fxr "$node $x{$node}";
    print $fxril "          ( setarray Table \"$n1\" \"$n2\" )";
    $ci++;
}
close $fxr;
print $fxril "    Table ) )\n";
print $fxril "(defun PinRevCanonicalTable ( )";
print $fxril "  (let ( Key map";
print $fxril "        ( Table ( makeTable `pd nil ) ) )";
for (my $n = 1; $n <= $ic; $n++) {
    print $fxril "        map = PinRevCanonicalTable$n( )";
    print $fxril "        foreach( Key map";
    print $fxril "            ( setarray Table Key arrayref( map Key ) ) )";
}
print $fxril "    Table ) )";
close $fxril;

END {
    if ($cast_server_pid) {
        close CCin;
        close CCOut;
        close CCErr;
        kill 9, $cast_server_pid;
#        waitpid $cast_server_pid, 0;
    }
    if ($apid) {
        kill 9, $apid;
#        waitpid $apid, 0;
    }
    print STDERR "Warning count: $warnings" if $warnings;
    print STDERR "Error count: $errors" if $errors;
    exit $errors;
}
exit $errors;
