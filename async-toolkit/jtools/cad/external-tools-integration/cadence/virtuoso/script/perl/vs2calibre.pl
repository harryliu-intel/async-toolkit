#!/usr/intel/bin/perl -lw
# AAG
# $Id$
# $DateTime$

# not a true verilog parser. Makes assumptions about commas
# not being inside of a commented name. Makes assumptions about
# certain names with square brackets as well as 'curly' brackets.
# does not handle structures more complex than arrays.

use strict;

use Getopt::Long;

# for profiling, does not work on compute servers
my $top = "";
my $externalfile="";
my $debug=0;
my $debugfile = "/dev/null";
my $portmap = "";
my $outputfile="";
my $verbose=0;
my $progress=1;
my $flat=0;
my %external=();   # list of external cells with array ports

my $profile=0;
# if profile = 1
#use Time::HiRes qw( gettimeofday tv_interval);
# if profile = 0
sub gettimeofday {}
sub tv_interval {0;}

my %opts = (
    "top=s" => \$top,
    "external=s" => \$externalfile,
    "debug" => \$debug,
    "portmap=s" => \$portmap,
    "output=s" => \$outputfile,
    "verbose" => \$verbose,
    "progress" => \$progress,
    "flat" => \$flat,
);

sub usage {
    print STDERR "@_" if @_;
    print STDERR <<EU;
Usage: vs2calibre --top <top module> [options] <verilog-file(s)>
    --external=<file>   : read file to define external cells with arrays
    --debug             : generate lots of debug data
    --verbose           : generate less debug data
    --portmap           : renames ports, but should not use here
    --output=<file>     : output instead of stdout. (may not work right.)
    --flat              : make a flat spice file
EU
    exit 1;
}

select STDERR;
$|=1;
select STDOUT;
$|=1;
GetOptions ( %opts ) or usage;
usage if ! defined $ARGV[0];
$verbose=1 if $debug;
$progress=1 if $verbose;
usage("No top module defined") if $top eq "" and $flat;
if ($outputfile eq "") {
    if ($top ne "") {
        $outputfile = $top;
    }
    else {
        $outputfile = $ARGV[0];
        $outputfile =~ s/\.v[^\.]*$//;
    }
    $outputfile .= ".sp";
}
if ($outputfile ne "-" and $outputfile ne "") {
    if (open (OUT, ">$outputfile")) {
        select OUT;
        $|=1;
    }
    else {
        print STDERR "Cannot open $outputfile";
    }
}
my $iofile = $outputfile;
$iofile =~ s/\.[^\.]*$//;
$iofile .= ".ports";
open(IO, ">$iofile");

my %ver=();
my %cast=();

sub readportmap {
    my ($file) = @_;
    local (*P,$_);
    return if ! -r "$file";
    open (P, "<$file") or warn "Cannot open $file: $!";
    print STDERR "Reading $file" if $progress;
    while (<P>) {
        chomp;
        my ($ver,$cast)=split;
        $ver{$cast}=$ver;
        $cast{$ver}=$cast;
    }
}


$debugfile = "vs2calibre.dbg" if $debug;
open (DBG, ">$debugfile");

readportmap ($portmap);

# not in the verilog, but necessary
my %specialcases = (
    "QCSMS1_DS_13T" => ["VDD10=vdd","VSS10=GND"],
    "QCSMS1_MUX_13T" => ["VDD10=vddA","VSS10=GND"],
);

my $wraplimit = 4096;
#$wraplimit = 8192; # no wrap

sub wrap {
    my ($str) = @_;
    my $orig=$str;
    if ($wraplimit > 2000) {
        print "$str";
        return;
    }
    my $b = 0;
    while (length ($str) > $wraplimit) {
        my $e = rindex($str," ",$wraplimit);
        $e = index($str," ") if ($e < 0);
        $e = length($str) if ($e < 0);
        my $s1 = substr($str, 0, $e);
        printf "+ " if $b;
        print $s1;
        if (length($str) < $e+1) {
            $str="";
        }
        else {
            $str = substr($str,$e+1);
        }
        $b = 2;
    }
    if (length($str) > 0) {
        printf "+ " if $b;
        print $str;
    }
}
my %pins=();
my %type=();
my %ports=();
my %portmap=();
my %hasarray=();
my @ports=();
my %porttypes=();
my %wiretypes=();
my %wires=();
my %localnodes=();
my %module=();
my %modules=(); # just those in the file

# whole files goes into memory for ease of handling 'c' comments
sub readfile {
    my ($file) = @_;
    printf STDERR "Reading $file..." if $progress;
    local(*P, $_, $/);
    undef $/;
    open (P, "<$file") or die "Cannot open $file $!";
    my $text = <P>;
    close P;
    $text =~ s/;/;\n/g;
    $text =~ s/\t/ /g;
    $text =~ s/  */ /g;
    $text =~ s/^ *//;
    $text =~ s/\n */\n/g;
    $text =~ s/\\//g;
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
    # make into an array of lines
    split(/\n/,$text);
}

sub parse {
    my ($file, $external) = @_;
    my @lines = readfile ($file);
    printf STDERR "Parsing $file..." if $progress;
    my $lnr = 0;
    local ($_);
    my $module = "";
    %localnodes=();
    my @ln = ();
    my %plookup=();
    my @plookup=();
    my @wlookup=();
    my @wires=();
    my $hasarray=0;
    while ($lnr <= $#lines) {
        $_=$lines[$lnr++];
        # skip blank lines
        next if (/^[ \t]*$/);
        # module definition until terminating ;
        if (/^module/) {
            my @f = split;
            $module = $f[1];
            print IO "MODULE $module";
            $modules{$module}=1;
            print DBG "Module $f[1]";
            my $ln = $_;
            while (! ($ln =~ /;/) and $lnr <= $#lines) {
                $_ = $lines[$lnr++];
                $ln .= " $_";
            }
            $ln =~ s/^[^\(]*\(//;
            $ln =~ s/,/ /g;
            $ln =~ s/  */ /g;
            $ln =~ s/^ //;
            $ln =~ s/ $//;
            $ln =~ s/ *;.*//;
            $ln =~ s/\)[^\)]*$//;
            undef @ports;
            @ports = split(/ /,$ln);
            $ports{$module}=[@ports];
            $hasarray=0;
            foreach my $n (0..$#ports) {
                $portmap{$module}->{port}{$ports[$n]}=$n;
                print DBG "HA1 $module $ports[$n]";
                $hasarray = 1 if ($ports[$n] =~ /\[/);
            }
            $hasarray{$module}=$hasarray;
            undef %plookup;
            %plookup=();
            foreach my $n (0..$#ports) {
                $plookup{$ports[$n]}=$n;
            }
            undef @ln;
            @wires=();
            @wlookup=();
            @ln=();
        }
        # port declarations
        elsif (/^output/ or /^input/ or /^inout/) {
            my $ln = $_;
            while (! (/;/) and $lnr <= $#lines) {
                $_ = $lines[$lnr++];
                $ln .= $_;
            }
            $_ = $ln;
            s/  */ /g;
            my ($io,$aref,$rest) = split (/ /,$_,3);
            if (! defined ($rest)) {
                $rest = $aref;
                $aref = "";
            }
            elsif (! ($aref =~ /\[/ ) or ! ($aref =~ /:/)) {
                $rest = "$aref $rest";
                $aref = "";
            }
            $rest =~ s/\t/ /g;
            $rest =~ s/ *;.*//;
            $rest =~ s/,/ /g;
            $rest =~ s/  */ /g;
            $rest =~ s/ $//;
            $hasarray{$module}=1 if $aref =~ /\[/;
            foreach my $port (split (/ /,$rest)) {
                print IO "$io $port";
                if (defined $plookup{$port}) {
                    $plookup[$plookup{$port}] = $aref;
                }
                else {
                    print STDERR "Undefined ref :$port: at $." if $port ne "";
                }
            }
        }
        # finish up module
        elsif (/^endmodule/) {
            print DBG "ENDMODULE $module $lnr";
            foreach my $n (0..$#ports) {
                print STDERR "Undefined type for $module $ports[$n] in $file"
                    if ! defined $plookup[$n];
            }
            $porttypes{$module} = [@plookup];
            $wiretypes{$module} = [@wlookup];
            $wires{$module} = [@wires];
            foreach my $n (0..$#wires) {
                $portmap{$module}->{wire}{$wires[$n]}=$n;
            }
            $localnodes{$module} = [@ln];
            $external{$module}=1 if $external;
            @ln = ();
            @plookup = ();
            $module="";
        }
        # local declarations, not necessarily all local nodes!
        elsif (/^wire/) {
            my $ln = $_;
            while (! (/;/) and $lnr <= $#lines) {
                $_ = $lines[$lnr++];
                $ln .= " $_";
                $_ = $ln;
            }
            s/,/ /g;
            s/  */ /g;
            s/ *; *$//;
            my @l = split;
            shift @l;
            push @ln, @l;
            s/  */ /g;
            my ($io,$aref,$rest) = split (/ /,$_,3);
            if (! defined ($rest)) {
                $rest = $aref;
                $aref = "";
            }
            elsif (! ($aref =~ /\[/ ) ) {
                $rest = "$aref $rest";
                $aref = "";
            }
            $rest =~ s/\t/ /g;
            $rest =~ s/ *;.*//;
            $rest =~ s/,/ /g;
            $rest =~ s/  */ /g;
            $rest =~ s/ $//;
            foreach my $port (split (/ /,$rest)) {
                push @wires, $port;
                push @wlookup, $aref;
            }
        }
        # assigns
        elsif (/^assign/) {
        }
        # assumes the rest is a list of instances, no logic
        else {
            my $ln = $_;
            while (! (/;/) and $lnr <= $#lines) {
                $_ = $lines[$lnr++];
                $ln .= $_;
            }
            $_ = $ln;
            s/  */ /g;
            s/^ //;
            s/ $//;
            next if length($_) == 0;
            my ($type,$inst,$list)=split(/ /,$_,3);
            if ($inst =~ /\(/) {
                s/([^ ])\(/ (/;
                my $l=$1;
                ($type,$inst,$list)=split(/ /,$_,3);
                $inst = $inst.$l;
            }
            $_ = $list;
            print DBG "L1 $type $inst $_";
            s/^ *\( *//;
            s/ *\) *; *$//;
            s/ *\)/)/g;
            s/\( */(/g;
            print DBG "L2 $type $inst $_";
            my $x = $_;
            my @p=();
            my $orig=$_;
            while (length($_)) {
                my $pin = $_;
                $pin =~ s/\(.*//;
                $pin =~ s/^\.//;
                print DBG "L3 $inst $pin : $_";
                print DBG "$pin : $_";
                $_ = substr($_,length($pin)+1);
                $pin =~ s/ //g;
                if (substr($_,0,1) ne "(") {
                    print STDERR "Warning: $type $inst $x =>";
                    print STDERR "Syntax pin $pin $_ $ln";
                    exit 1;
                }
                my $l = 1;
                my $net = "";
                $_ = substr($_,1);
                while ($l >= 1 or (substr($_,0,1) ne ")" and length($_))) {
                    if ($l == 1 and substr($_,0,1) eq ")") {
                        $net = "";
                        if (length($_) == 1) {
                            $_="";
                        }
                        else {
                            $_=substr($_,1);
                        }
                        last;
                    }
                    $net .= substr ($_,0,1);
                    $_ = substr($_,1);
                    $l++ if (substr($_,0,1) eq "(");
                    $l-- if (substr($_,0,1) eq ")");
                }
                while (substr($_,0,1) ne "," and length($_)) {
                    $_ = substr($_,1);
                }
                if (length($_)>0) {
                    $_ = substr($_,1);
                }
                else {
                    $_ = "";
                }
                while (substr ($_,0,1) eq " ") {
                    $_ = substr($_,1);
                }
                $net =~ s/, */, /g;
                $net =~ s/ *$//;
                print DBG "L4 $inst:$pin:$net";
                if (length ($net) ) {
                    push @p,"$pin=$net";
                    print DBG "P $module:$inst $#p $pin=$net";
                }
            }
            print DBG "PX2 $module:$inst $#p $type : ",join(";",@p);
            $pins{"$module:$inst"} = [@p];
            $type{"$module:$inst"} = $type;
        }
    }
    print STDERR "Done" if $progress;
}

# get externals to define ports of sIPCell, for example
# cells without definitions are assumed single ports
parse ("$externalfile", 1) if -r $externalfile;
foreach my $file (@ARGV) {
    parse ($file, 0);
}
# find which cells are 'leaf' cells and make module lookup
my %leaf = ();
foreach my $k (sort keys %type) {
    my ($module,$inst)=split(/:/,$k);
    push @{$module{$module}},$inst;
    if ($external{$type{$k}} or ! defined ($ports{$type{$k}})) {
        $leaf{$k}=1;
        $leaf{$type{$k}}=1;
    }
}
printf STDERR "Expanding...\n" if $progress;
# now expand all of the ports to make it easier later
foreach my $k (keys %pins) {
    my ($module,$inst)=split(/:/,$k);
    my $type = $type{$k};
    printf DBG "Expanding $module $inst $type\n";
    my $t0 = [gettimeofday] if $profile;
    my @iv=();
    my @wh=();
    my @p=();
    my $ivlast=0;
    foreach my $pd (@{$pins{$k}}) {
        my ($port,$signal)=split(/=/,$pd);
        print DBG "EX1 $inst $port $signal";
        my $t1=time;
#        print DBG "   $module $inst $pd $type";
        if ((! defined ($hasarray{$type}) or ! $hasarray{$type})) {
            print DBG "EX2 $inst $pd";
            if ($profile) {
                my $iv = tv_interval($t0,[gettimeofday])*1.e6 - $ivlast;
                printf DBG "PET %.0f noexpand $pd\n", $iv - $ivlast;
                $ivlast = $iv;
            }
            push @p, $pd;
        }
        else {
            print DBG "EX3 $inst $port";
            my @port = expandport($type, $port);
            if ($profile) {
                my $iv = tv_interval($t0,[gettimeofday])*1.e6 - $ivlast;
                printf DBG "PET %.0f expandport1 $pd\n", $iv - $ivlast;
                $ivlast = $iv;
            }
            my @s1 = arrayify ($signal);
            if ($profile) {
                my $iv = tv_interval($t0,[gettimeofday])*1.e6 - $ivlast;
                printf DBG "PET %.0f arrayify $pd\n", $iv - $ivlast;
                $ivlast = $iv;
            }
            my @signal=();
            foreach my $s1 (@s1) {
                my $ta = [gettimeofday] if $profile;
                my @x = expandport($module,$s1);
                printf DBG "PE2 %.0f $s1 $module\n",
                    tv_interval($ta,[gettimeofday])*1.e6 if $profile;
                $x[0] = $s1 if ($#x == -1);
                push @signal, @x;
            }
            if ($profile) {
                my $iv = tv_interval($t0,[gettimeofday])*1.e6 - $ivlast;
                printf DBG "PET %.0f expandport2 $pd\n", $iv - $ivlast;
                $ivlast = $iv;
            }
            if ($#signal != $#port) {
                print STDERR "Error: $#signal $#s1 $#port $module $inst $type $pd";
                print STDERR "  @signal";
                print STDERR "  @port";
                exit 1;
            }
            foreach my $n (0..$#port) {
                print DBG "EX4 $inst $port[$n]=$signal[$n]";
                push @p, "$port[$n]=$signal[$n]";
            }
        }
        if ($profile) {
            my $iv = tv_interval($t0,[gettimeofday])*1.e6 - $ivlast;
            printf DBG "PET %.0f pushport $pd\n", $iv - $ivlast;
            $ivlast = $iv;
        }
        my $t2=time-$t1;
        print DBG "EPT $t2 $inst $pd";
    }
    if ($profile) {
        my $iv = tv_interval($t0,[gettimeofday])*1.e6 - $ivlast;
        printf DBG "PET %.0f end1\n", $iv - $ivlast;
        $ivlast = $iv;
    }
    # replace old list with new list
    $pins{$k}=[sort @p];
    if ($profile) {
        my $iv = tv_interval($t0,[gettimeofday])*1.e6 - $ivlast;
        printf DBG "PET %.0f end2\n", $iv - $ivlast;
        $ivlast = $iv;
    }
}
my @expandedports=();

print STDERR "Done expanding" if $progress;

my @stack=();

sub writespfile {
    $|=1;
    @stack=();
    print STDERR "Writing! $top" if $progress;
    @expandedports = expandports ($top);
    foreach my $n (0..$#expandedports) {
        if (defined ($cast{$expandedports[$n]})) {
            $expandedports[$n] = $cast{$expandedports[$n]};
        }
    }
    wrap ".SUBCKT $top @expandedports";
    my @stack = ();
    printinst ($top, "", ());
    print ".ENDS\n";
    0;
}

if ($flat) {
    writespfile();
}
else {
    foreach my $module (sort keys %modules) {
        $top = $module;
        writespfile() if ! $external{$top};
    }
}
print STDERR "Done!" if $progress;
0;

# function expands an array into it's components
sub expandf {
    my ($name,$s,$e) = @_;
    my $t1 = time;
    my @epl;
    my $t0 = [gettimeofday];
    if (defined ($s) and $e =~ /^\d+$/) {
        if ($s >= $e) {
            for (my $p = $s; $p >= $e; $p--) {
                push @epl, "$name\[$p]";
            }
        }
        else {
            for (my $p = $s; $p <= $e; $p++) {
                push @epl, "$name\[$p]";
            }
        }
        printf DBG "EF1 $name $s $e $#epl %.0f\n", 1e6*tv_interval( $t0, [gettimeofday]);
    }
    else {
        $epl[0] = $name;
        printf DBG "EF0 $name - - $#epl %.0f\n", 1e6*tv_interval( $t0, [gettimeofday]);
    }
    my $t2 = time - $t1;
    $s = "" if ! defined $s;
    $e = "" if ! defined $e;
    print DBG "EXFT $t2 $name:$s:$e";
    @epl;
}

# expand a port into it's comps by name e.g. abC[16:0] -> abC[16],abC[15]...
sub expand {
    my ($name)=@_;
    $name =~ s/\[(\d+):(\d+)\]//;
    my @epl = expandf($name,$1,$2);
    @epl;
}

# take list with {..} and make into a node list
sub arrayify {
    my ($s) = @_;
    my @arr=();
    if ($s =~ /^{/) { # }
        my @s;
        $s =~ s/{ *//;
        $s =~ s/}$//;
        $s =~ s/, / /g;
        $s =~ s/  */ /g;
        $s =~ s/^ //;
        $s =~ s/ $//;
        @s = split (/ /,$s);
        foreach my $s (@s) {
            push @arr, expand($s);
        }
    }
    else {
        $arr[0] = $s;
    }
    @arr;
}

# expands a specific port. Requires the module to get the order and range
# correctly extracted
sub expandport {
    my ($top,$name) = @_;
    my $t1=[gettimeofday];
    my @epl = ();
    print DBG "DY $top $name";
    if (! defined ($ports{$top})) {
        print DBG "DX No ports for $top";
        push @epl,$name;
        printf DBG "EP1 $top $name %.0f\n", tv_interval( $t1, [gettimeofday])*1e6;
        return @epl;
    }
    if ($name =~ /\[\d+\]$/) {
        push @epl, $name;
        printf DBG "EP2 $top $name %.0f\n", tv_interval( $t1, [gettimeofday])*1e6;
        return @epl;
    }
    if ($name =~ /\[(\d+):(\d+)\]$/) {
        @epl=expand($name);
        printf DBG "EP3 $top $name %.0f\n", tv_interval( $t1, [gettimeofday])*1e6;
        return @epl;
    }
    printf DBG "DY2 $#ports $top %.0f\n", tv_interval( $t1, [gettimeofday])*1e6;
    my $n = 4;
    if (defined $portmap{$top}->{port}{$name}) {
        my $n = $portmap{$top}->{port}{$name};
        my $porttype = ${@{$porttypes{$top}}}[$n];
        if ($porttype =~ /\[/) {
            $porttype =~ m.\[(\d+):(\d+)\].;
            push @epl, expandf ($name,$1,$2);
        }
        else {
            push @epl, "$name";
        }
    }
    printf DBG "DY3 $#ports $top %.0f\n", tv_interval( $t1, [gettimeofday])*1e6;
    if (! @epl) {
        if (defined $portmap{$top}->{wire}{$name}) {
            my $n = $portmap{$top}->{wire}{$name};
            my $porttype=${@{$wiretypes{$top}}}[$n];
            if ($porttype =~ /\[/) {
                $porttype =~ m.\[(\d+):(\d+)\].;
                push @epl, expandf ($name,$1,$2);
            }
            else {
                push @epl, "$name";
            }
        }
    }
    printf DBG "DY4 $#ports $top %.0f\n", tv_interval( $t1, [gettimeofday])*1e6;
    push @epl, $name if ! @epl;
    print DBG "DY5 $top $#epl $name @epl";
    printf DBG "EP$n $top $name %.0f ($#ports $#epl) @epl\n", tv_interval( $t1, [gettimeofday])*1e6;
    @epl;
}

# list of all ports expanded into wires for a module
sub expandports {
    my ($top) = @_;
    print DBG "DYS:$top:";
    my $t1 = time;
    my @epl = ();
    my @ports = ();
    my @porttypes = ();
    if ( defined $ports{$top} and defined $porttypes{$top}) {
        @ports = @{$ports{$top}};
        @porttypes = @{$porttypes{$top}};
    }
    else {
        print DBG "DYSE : no ports for $top";
        print STDERR "Error: No ports for $top in expandports";
    }
    foreach my $n (0..$#ports) {
        if ($porttypes[$n] =~ /\[/) {
            $porttypes[$n] =~ m.\[(\d+):(\d+)\].;
            push @epl, expandf ($ports[$n],$1,$2);
        }
        else {
            push @epl, "$ports[$n]";
        }
    }
    my $t2=time-$t1;
    print DBG "DYT $t2 $top";
    @epl;
}

# globals for printinst
my $count=0;
my $isleaf=0;

sub printinst {
    my ($top,$cinst,@connections)=@_;
    push @stack,$top;
    print DBG "PI $top,$cinst,@connections";
    my %map = ();
    my @list = expandports ($top);
    my %list=();
    # make a hash of the port names
    foreach my $list (@list) {
        $list{$list}=1;
    }
    # map the upper level signal names to the internal port names
    foreach my $n (0..$#connections) {
        print DBG "C $top:$cinst $connections[$n]";
        my ($p,$s) = split(/=/,$connections[$n]);
        $map{$p} = $s;
        if (defined ($list{$p})) {
            # this may not handle full arrays correctly
            my @p = expandport($top,$p);
            my @s = arrayify ($s);
            foreach my $n (0..$#p) {
                $map{$p[$n]} = $s[$n];
                print DBG "MP1x $cinst $top $p[$n] $s[$n]";
            }
            print DBG "MP1 $cinst $top $p $map{$p} $#p $#s";
        }
        else { # bad thing here, should always be ok except for power
            $"="->";
            print STDERR "No port $p in $top $cinst @stack"
                unless $p eq "Vdd" or $p eq "GND";
            $"=" ";
        }
    }
    # test for all ports mapped, assign like local nodes if not
    # it is an error for this if the cell is not the top cell
    foreach my $port (@list) {
        print DBG "PRT $top $port";
        if (! defined ($map{$port})) {
            print DBG "No portmap for $port in $cinst $top" if $cinst ne "";
#            $map{$port} = $port;
        }
    }
    # now do explicit local nodes (wires)
    @list=();
    @list = @{$localnodes{$top}} if defined $localnodes{$top};
    my $ninst = $cinst;
    $ninst .= '/' if $cinst ne "";
    foreach my $l (@list) {
        $map{$l} = "$ninst$l";
        print DBG "ML $cinst $top $l $map{$l}";
    }
    # record all pre-mappings in debug file
    foreach my $node (sort keys %map) {
        print DBG "M $cinst $top $node $map{$node}";
    }
    my $k;
    if (defined ($module{$top})) {
        foreach my $inst (sort @{$module{$top}}) {
            # key for hashes below
            $k = "$top:$inst";
            my @p = @{$pins{$k}};
            my @mappins=();
            foreach my $n (0..$#p) {
                print DBG "MX1 $k $p[$n]";
                my ($p,$s) = split(/=/,$p[$n]);
                my @exp = expandport($type{$k},$p);
                my @p = arrayify ($s);
                print DBG "PY $type{$k} $k [$p $s] $#exp $#p" if $#exp != $#p;
                if ($s =~ /^{/) { # }
                    if ($#exp == $#p) {
                        foreach my $n (0..$#p) {
                            $map{$p[$n]} = "$ninst$p[$n]"
                                if ! defined $map{$p[$n]};
                            push @mappins, "$exp[$n]=$map{$p[$n]}";
                            print DBG "MX2 $cinst $top $exp[$n] $map{$p[$n]} $p[$n]";
                        }
                    }
                    elsif ($#p == 0) {
                        foreach my $n (0..$#exp) {
                            my $name = $exp[$n];
                            $name =~ s/.*\[/[/;
                            $name = $p[0].$name;
                            $map{$name} = "$ninst$name"
                                if ! defined $map{$name};
                            push @mappins, "$exp[$n]=$map{$name}";
                            print DBG "MX3 $cinst $top $exp[$n] $map{$name} $name";
                        }
                    }
                    else {
                        print DBG "Err Port count mismatch $#exp $#p";
                    }
                }
                else {
                    $map{$s} = "$ninst$s"
                        if ! defined $map{$s};
                    push @mappins, "$p=$map{$s}";
                    print DBG "MX4 $cinst $top $s $map{$s} $p";
                }
            }
            my @s=(sort @mappins);
            if ($leaf{$k} or ! $flat) {
                print DBG "PX1 X$ninst$inst $type{$k} \$PINS @s\n @p";
                # fix the port list for printing only
                my @ss=();
                foreach my $n (0..$#s) {
                    if ($s[$n] =~ /1'[bh]([01])/) {
                        my $sex = $1;
                        my ($name,$x) = split(/=/,$s[$n]);
                        $s[$n] = "$name=Vdd" if $sex;
                        $s[$n] = "$name=GND" if ! $sex;
                    }
                    if ($s[$n] =~ /^VDD=/ and $s[$n] =~ /=GND/) {
                        $s[$n] =~ s/=GND/=Vdd/;
                    }
                    elsif ($s[$n] =~/^V[DG][DG]/ and $s[$n] =~ /=GND/) {
                        my ($name,$x) = split(/=/,$s[$n]);
                        $s[$n] = "$name=$name";
                    }
                    push @ss, $s[$n]
                        if $s[$n] ne "Vdd=Vdd" and $s[$n] ne "GND=GND" and
                            ! ($s[$n] =~ /^Rref=/) and ! ($s[$n] =~ /^Vtt=/);
                }
                push @ss,@{$specialcases{$type{$k}}} if defined $specialcases{$type{$k}};
                if (! $isleaf) {
                    foreach my $n (0..$#ss) {
                        my ($p,$s)=split(/=/,$ss[$n]);
                        if ($s =~ /^1'[bh]([01])$/) {
                            my $sex = $1;
                            $ss[$n] = "$p=Vdd" if $sex;
                            $ss[$n] = "$p=GND" if ! $sex;
                        }
                        elsif (defined ($cast{$s})) {
                            $s = $cast{$s};
                            $ss[$n]="$p=$s";
                        }
                        if ($p eq "VDD" and $s =~ /^1/) {
                            $ss[$n]="$p=Vdd";
                        }
                        elsif ($p =~/^V[DG][DG]/ and $s =~ /^1/) {
                            $ss[$n] = "$p=$p";
                        }
                    }
                }
                wrap "X$ninst$inst $type{$k} \$PINS @ss";
                $count++;
            }
            else {
                $isleaf++;
                printinst($type{$k},"$ninst$inst",@s);
                $isleaf--;
            }
        }
    }
    else {
        print STDERR "Skipping $top as undefined";
    }
    print DBG "PIX $top";
    pop @stack;
}
