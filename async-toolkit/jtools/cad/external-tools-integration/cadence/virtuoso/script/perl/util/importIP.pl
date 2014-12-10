#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;
use Getopt::Long;

my $cdl=undef;
my $verbose=0;
my $vdd="VDD";
my $gnd="VSS";
my $pdk=undef;

my $refinement_parent=undef;
my $lib_name=undef;
my $sub_type=0;
my $bind_rul_in="/dev/null";
my $output_dir=undef;
my $all_cells=0;
my $lib_file=undef;
my $topcell="";
my $cdl2cast=0;
my %options=();
my $cast_path=undef;

sub usage {
    my ($msg)=@_;
    print STDERR "Usage: importPR [options]";
    foreach my $opt (sort keys %options) {
        my $r=$options{$opt};
        my $ref=ref($options{$opt});
        if ($opt =~ /=/) {
            my $o=$opt;
            $o =~ s/=.*//;
            if ($ref eq "SCALAR") {
                print STDERR "   --$o=[".$$r."]";
            }
            else {
                print STDERR "   --$o=$o";
            }
        }
        else {
            if ($ref eq "SCALAR") {
                print STDERR "   --$opt [$$r]";
            }
            else {
                print STDERR "   --$opt [$opt]";
            }
        }
    }
    print STDERR "$msg" if defined $msg;
    exit 1;
}

sub mergecast {
    my @files=@_;
    my $fh;
    my @header=();
    my %cells=();
    my $header=0;
    my $cell="";
    foreach my $file (@files) {
        if (open($fh, "<$file")) {
            while (<$fh>) {
                chomp;
               if (/^define "(\S+)"/) {
                    $cell=$1;
                    @{$cells{$cell}}=();
                    $header=1;
                }
                elsif (/^define (\S+)\(/) {
                    $cell=$1;
                    @{$cells{$cell}}=();
                    $header=1;
                }
                push @{$cells{$cell}}, $_ if $cell ne "";
                push @header, $_ if $header==0;
                if (/^\x7d/) {
                    push @{$cells{$cell}}, "" if $cell ne "";
                    $cell="";
                }
            }
        }
        else {
            print STDERR "Cannot open $file";
        }
    }
    my @out=@header;
    my $cnt=0;
    foreach my $cell (sort keys %cells) {
        push @out, @{$cells{$cell}};
        $cnt++;
    }
    @out;
}

sub evalit {
    my ($value)=@_;
    $value =~ s/'//g;
    $value =~ s/(\d)u/$1*1e-6/ig;
    $value =~ s/(\d)n/$1*1e-9/ig;
    $value =~ s/(\d)p/$1*1e-12/ig;
    $value =~ s/(\d)f/$1*1e-15/ig;
    $value =~ s/[\(\)]//g;
    print STDERR $value if $value =~ /\*/ and $value =~ /[a-df-z]/;
    eval "\$value=$value";
    print STDERR $value if $value =~ /\*/ and $value =~ /[a-df-z]/;
    $value;
}

sub evaluate {
    my ($value)=@_;
    return $value;
    my ($n,$v)=split(/=/, $value);
    if (defined ($v)) {
        return "$n=".evalit($v);
    }
    return evalit($n);
}

# purpose : to modify Avago netlist to agree with Fulcrum/Intel conventions
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

my %direction=();
my %isbus=();
my %dirlookup=("input" => "-", "output" => "+", "inout" => "-+", "" => "-+", "internal" => "i");

sub getdirection {
    my ($cell,$pin) = @_;
    my %dir=();
    %dir=%{$direction{$cell}} if defined $direction{$cell};
    if (! defined ($dir{$pin})) {
        my $x = $pin;
        $x =~ s/\[.*//;
        $dir{$pin} = $dir{$x} if defined $dir{$x};
        $dir{$pin} = "-" if ! defined($dir{$pin}) and $pin =~ /VDD/i;
        $dir{$pin} = "-" if ! defined($dir{$pin}) and $pin =~ /GND$/i;
        $dir{$pin} = "-+" if ! defined $dir{$pin};
    }
    $dir{$pin};
}

sub parselibfile {
    my ($file) = @_;
    open (P, "<$file");
    print STDERR "Parsing $file" if $verbose;
    my $cell;
    my $pin;
    my $bus;
    my $bus_type;
    my %bit_width;
    my %bit_from;
    my %bit_to;
    my $direction;
    while (<P>) {
        chomp;
        if (/^\s*cell\s*\(\s*(.*)\s*\)/) {
            $cell=$1;
            $cell =~ s/\s//g;
            $cell =~ s/"//g;
            $cell =~ s/'//g;
        }
        if (/^\s*type\s*\(\s*(\S+)\s*\)/) {
            $bus_type = $1;
            while (<P>) {
                chomp;
                if (/^\s*(\S+)\s*:\s*(\S+)\s*;/) {
                    if ($1 eq "bit_width") {
                        $bit_width{$bus_type}=$2;
                    }
                    if ($1 eq "bit_from") {
                        $bit_from{$bus_type}=$2;
                    }
                    if ($1 eq "bit_to") {
                        $bit_to{$bus_type}=$2;
                    }
                }
                if (/^\s*\x7d/) {
                    last;
                }
            }
        }
        if (/^\s*pin\s*\(\s*(.*)\s*\)/) {
            $pin = $1;
#            $pin =~ tr/a-z/A-Z/;
            $direction{$cell}->{$pin}="undefined";
            $direction="undefined";
            undef $bus;
        }
        if (/^\s*bus\s*\(\s*(.*)\s*\)/) {
            $bus = $1;
            $isbus{$cell}->{$bus}=1;
            undef $pin;
        }
        if (/^\s*bus_type\s*:\s*(\S+)\s*;/) {
            $bus_type = $1;
            print STDERR "BUS TYPE :$bus_type: not defined" if ! defined $bit_width{$bus_type};
        }
        if (/^\s*direction\s*:\s*(.*)\s*;/) {
            $direction=$1;
            $direction =~ s/\s//g;
            $direction = $dirlookup{$direction};
            if (defined ($pin)) {
                $direction{$cell}->{$pin}=$direction;
                if ($pin =~ /\[/) {
                    my $base=$pin;
                    $base =~ s/\[.*//;
                    $direction{$cell}->{$base}=$direction;
                }
            }
            elsif (defined ($bus) and defined($bit_width{$bus_type})) {
                my $to = $bit_to{$bus_type};
                my $from = $bit_from{$bus_type};
                my $width = $bit_width{$bus_type};
                if (defined ($to) and defined ($from) and abs($from-$to)+1 != $width) {
                    print STDERR "Bus definition may be inconsistent for $bus_type";
                }
                if ($to < $from) {
                    my $x = $from;
                    $from = $to;
                    $to = $x;
                }
                $direction{$cell}->{"$bus"}=$direction;
                for (my $n = $from; $n <= $to; $n++) {
                    $direction{$cell}->{"$bus\[$n\]"}=$direction;
                }
            }
        }
    }
}

sub fixcastfile {
    my ($file)=@_;
    open (P, "<$file");
    my $module;
    my @lines=();
    while (<P>) {
        chomp;
        if (/^module\s+(\S+)\s*;/) {
            $module=$1;
            push @lines, $_;
        }
        elsif (/^define\s+"(\S+)"\(\)\((.*)\)(.*)/) {
            my $cell=$1;
            my $nodes=$2;
            my $rest=$3;
            my $lookupcell=$cell;
            if ($cell eq "0") {
                $lookupcell=$module;
                $lookupcell =~ s/.*\.//;
            }
            if (defined ($direction{$lookupcell})) {
                my %dir=%{$direction{$lookupcell}};
                my @nodes=();
                my $lastbus="";
                my $a=-1;
                my $b=-1;
                foreach my $pin (sort sortNatural keys %dir) {
                    next if $dir{$pin} eq "i";
                    my $node=$pin;
                    my $bus="";
                    next if $isbus{$lookupcell}->{$pin};
                    if ( $pin =~ /(\S+)\[(\d+)\]$/) {
                        $bus=$1;
                        my $n=$2;
                        if ($bus ne $lastbus) {
                            if ($lastbus ne "") {
                                push @nodes, "$dir{$pin}$lastbus\[$a..$b\]";
                            }
                            $lastbus=$bus;
                            $a=$b=$n;
                            next;
                        }
                        else {
                            $a = $a > $n ? $n : $a;
                            $b = $b < $n ? $n : $b;
                        }
                        next;
                    }
                    elsif ($lastbus ne "") {
                        push @nodes, "$dir{$pin}$lastbus\[$a..$b\]";
                        $lastbus="";
                    }
                    $dir{$pin}="-" if $dir{$pin} eq "";
                    push @nodes, "$dir{$pin}$pin" if $dir{$pin} ne "undefined";
                }
                $nodes="node ".join(", ", @nodes);
            }
            push @lines, "define \"$cell\"()($nodes)$rest";
        }
        else {
            s/_L_/[/g;
            s/_R_/]/g;
            push @lines, $_;
        }
    }
    close P;
    open (P, ">out.cast");
    open (P, ">$file");
    print P join("\n", @lines);
    close P;
}

sub fixcdl {
    my ($infile,$outfile)=@_;
    $outfile = "$infile.mod" if ! defined $outfile;
    my %portmap=();
    open (P, "<$infile");
    my $ln="";
    my @lines=();
    my %resistors=();
    my $resistorsdone=0;
    my %called=();
    while (<P>) {
        chomp;
        s/\//_/g;
        if (/^\+/) {
            s/^\+/ /;
            $ln .= $_;
            next;
        }
        my $lx=$_;
        $_=$ln;
        $ln=$lx;
        s/\s+/ /g;
        push @lines, $_;
        if (/^\.subckt/i) {
            my @f = split;
            for (my $n = 0; $n <= $#f; $n++) {
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
            $called{$subckt} += 0;
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
            my $ckt=$f[$#f];
            for (my $n = 0; $n <= $#f ; $n++) {
    ###            $f[$n] =~ tr/a-z/A-Z/;
                if ($n == 3 and $f[$n] =~ /^R/ and $f[$n+1] =~ /=/) {
    ###                $f[$n] =~ tr/A-Z/a-z/;
                    $resistors{$f[$n]}=1;
                }
                $f[$n] = "Vdd" if $f[$n] eq "VDD" or $f[$n] =~ /\.VDD$/i;
                $f[$n] = "GND" if $f[$n] =~ /\.GND$/i;
                if ($f[$n] =~ /=/) {
    ###                $f[$n] =~ tr/A-Z/a-z/;
                    $f[$n] = evaluate($f[$n]);
                }
                else {
                    $f[$n] =~ s/\./_/g;
                    $ckt=$f[$n];
                }
            }
            $called{$ckt}++;
            my $n = $#f;
            if ($f[$n] =~ /=/) {
                $f[$n] = evaluate($f[$n]);
    ###            $f[$n] =~ tr/A-Z/a-z/;
            }
        }
    }
    $ln =~ s/\s+/ /;
    push @lines, $ln;
    close P;
    open (Q, ">$outfile");
    foreach my $line (@lines) {
        $_=$line;
        s/\//_/g;
        s/\s+/ /g;
        s/\s$//;
        if (/^\.subckt/i or /^\.ends/i) {
#            if (! $resistorsdone ) {
#                foreach my $r (sort keys %resistors) {
#                    print Q ".SUBCKT $r PLUS MINUS w=w l=l\n.ENDS $r\n";
#                }
#                $resistorsdone=1;
#            }
            my @f = split;
            for (my $n = 0; $n <= $#f; $n++) {
    ###            $f[$n] =~ tr/a-z/A-Z/ if $n != 1;
                $f[$n] = "Vdd" if $f[$n] eq "VDD" or $f[$n] =~ /\.VDD$/i;
                $f[$n] = "GND" if $f[$n] =~ /\.GND$/i;
            }
            my $pre=shift @f;
            my $subckt = shift @f;
            my $ports=join(" ", @f);
            my $sortedports=join(" ", sort sortNatural @f);
            if ($pre =~ /subckt/i) {
                print Q "$pre $subckt $sortedports";
            } else {
                print Q join(" ", ($pre, $subckt, @f)) 
            }
        }
        elsif (/^m/i) {
            my @f = split;
            for (my $n = 0; $n <= 4 ; $n++) {
    ###            $f[$n] =~ tr/a-z/A-Z/;
                $f[$n] = "Vdd" if $f[$n] eq "VDD" or $f[$n] =~ /\.VDD$/i;
                $f[$n] = "GND" if $f[$n] =~ /\.GND$/i;
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
            print Q join(" ", @f);
        }
        elsif (/^[rcd]/i) {
    #        tr/A-Z/a-z/;
            my @f = split;
            for (my $n = 0; $n <= 2 ; $n++) {
    ###            $f[$n] =~ tr/a-z/A-Z/;
                $f[$n] = "Vdd" if $f[$n] eq "VDD" or $f[$n] =~ /\.VDD$/i;
                $f[$n] = "GND" if $f[$n] =~ /\.GND$/i;
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
            print Q join(" ", @f);
        }
        elsif (/^x/i) {
            my @f = split;
            my $subcell=undef;
            for (my $n = 0; $n < $#f ; $n++) {
    ###            $f[$n] =~ tr/a-z/A-Z/;
                if ($n == 3 and $f[$n] =~ /^R/ and $f[$n+1] =~ /=/) {
                    $f[$n] =~ tr/A-Z/a-z/;
                }
                $f[$n] = "Vdd" if $f[$n] eq "VDD" or $f[$n] =~ /\.VDD$/i;
                $f[$n] = "GND" if $f[$n] =~ /\.GND$/i;
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
            print Q join(" ", @f);
        }
        else {
            print Q;
        }
    }
    my $top=undef;
    foreach my $cell (keys %called) {
        if ($called{$cell}==0) {
            if (defined ($top)) {
                print STDERR "Too many top cells $cell $top\n";
            }
            else {
                $top=$cell;
            }
        }
    }
    $top;
}

%options=(
    "refinement-parent=s" => \$refinement_parent,
    "lib-name=s" => \$lib_name,
    "sub-type=s" => \$sub_type,
    "output-dir=s" => \$output_dir,
    "all-cells" => \$all_cells,
    "vdd-node=s" => \$vdd,
    "gnd-node=s" => \$gnd,
    "cdl=s" => \$cdl,
    "fulcrum-pdk-root=s" => \$pdk,
    "lib-file=s" => \$lib_file,
    "cell=s" => \$topcell,
    "cdl2cast" => \$cdl2cast,
    "cast-path=s" => \$cast_path,
);

GetOptions (
    %options
) or usage;

usage "Bad lib-name" if ! defined $lib_name or $lib_name !~ /\./;
usage "No output-dir specified" if ! defined $output_dir;
usage "No refinement-parent" if ! defined $refinement_parent;
`mkdir -p "$output_dir"`;
usage "Could not create output-dir" if ! -d $output_dir;
usage "CDL not specified" if ! defined $cdl or ! -s $cdl;
my $spec_dir="$output_dir/spec";
my $cast_dir="$output_dir/cast";
my $name_table="$output_dir/skill";
`mkdir -p $output_dir`;
`mkdir -p $spec_dir`;
`mkdir -p $output_dir`;
my $mod="$cdl.modw";
$mod =~ s:.*/::;
my $rv=system("fixcdl",$cdl, $mod);
die "Bad return for fixcdl" if $rv != 0;
parselibfile($lib_file);
my $libdir=$lib_name;
$libdir=~ s:\.:/:g;
my @cmd=();
if ($cdl2cast) { # used for stdcells so you get PRS
    push @cmd, "cdl2cast";
    push @cmd, "--max-heap-size=2G";
    push @cmd, "--cdl-file=$mod";
    push @cmd, "--output-spec=$spec_dir";
    push @cmd, "--output-cast=$cast_dir";
    push @cmd, "--name-table-dir=$name_table";
    push @cmd, "--vdd-node=$vdd";
    push @cmd, "--gnd-node=$gnd";
    push @cmd, "--refinement-parent=$refinement_parent";
    push @cmd, "--lib-name=$lib_name";
    push @cmd, "--sub-type=$sub_type";
    push @cmd, "--meters-per-input-unit=1";
    push @cmd, "--layout-to-cdl-bind-rul=/dev/null";
    push @cmd, "--output-cdl-to-layout-bind-rul=$output_dir/cdl2layoutbind.rul";
    push @cmd, "--output-cast-cells=$output_dir/allcells.txt";
    push @cmd, "--bind-rul-header=$pdk/share/Fulcrum/assura/bind.rul";
    push @cmd, "--all-cells" if $all_cells;
    push @cmd, "--bind-rul-in=bind.rul.combined";
    my $rv=system(@cmd);
    die "cdl2cast fails with return code $rv" if $rv != 0;
}
else {
    push @cmd, "cdl2cast2";
    push @cmd, "--lib=vendor.avago.mem";
    push @cmd, "--sub-type=0";
    push @cmd, "--gnd-node=GND";
    push @cmd, "--vdd-node=Vdd";
    push @cmd, "--cell=$topcell";
    push @cmd, "--libfile=$lib_file";
    push @cmd, "--refinement-parent=$refinement_parent";
    push @cmd, "--output-dir=$output_dir";
    push @cmd, "$mod";
    my $rv=system(@cmd);
    print STDERR "cdl2cast2 returns $rv" if $rv != 0;
    exit $rv if $rv != 0;
    # use jflat to check the cast syntax
    @cmd=("jflat");
    push @cmd, "--cast-path=$output_dir/cast:$output_dir/spec:$cast_path";
    push @cmd, "--cell=$lib_name.$topcell.$sub_type";
    push @cmd, "--tool=check,cdl";
    push @cmd, "--output-dir=$output_dir";
    $rv=system(@cmd);
    print STDERR "jflat returns $rv" if $rv != 0;
    exit $rv if $rv != 0;
    my ($cast_dir,$spec_dir)=split(/:/,$cast_path);
    my @spec=`find $output_dir/spec/$libdir -name '*.cast'`;
    chomp @spec;
    my $writeerr=0;
    foreach my $spec (@spec) {
        $spec =~ s:$output_dir/spec:$spec_dir:;
        if ( ! -w $spec and -e $spec ) {
            print STDERR "Cannot write to $spec";
            $writeerr++;
        }
    }
    system("rsync", "-a", "$output_dir/spec/$libdir/", "$spec_dir/$libdir") if ! $writeerr;
    my @merged=mergecast( "$output_dir/cast/$libdir.cast", "$cast_dir/$libdir.cast");
    if ( ( -w "$cast_dir/$libdir.cast" or ! -f "$cast_dir/$libdir.cast") and open (P, ">$cast_dir/$libdir.cast")) {
        print P join("\n", @merged);
        close P;
    }
    else {
        print STDERR "Cannot write to $cast_dir/$libdir.cast";
    }
    exit 0;
}

fixcastfile("$output_dir/cast/$libdir.cast");
open (P, "<$output_dir/allcells.txt"); # generated by cdl2cast
while (<P>) {
    chomp;
    next if /CBLK/;
    my $topcell=$_;
    my @jflat=();
    push @jflat, "jflat";
    push @jflat, "--tool=cdl";
    push @jflat, "--cell=$topcell";
    push @jflat, "--output-dir=$output_dir";
    push @jflat, "--cast-path=/p/work/aagrey/rrc/cast:$cast_dir:$spec_dir";
    my $rv=system(@jflat);
}
