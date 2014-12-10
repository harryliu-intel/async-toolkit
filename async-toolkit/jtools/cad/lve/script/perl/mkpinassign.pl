#!/usr/intel/bin/perl -l
# AAG
# $Id: mkpinassign.pl,v 1.1 2011/01/07 19:44:37 aubrey Exp aubrey $
# $DateTime$
# No SC on outputs

use strict;
use Getopt::Long;

my $verbose=0;
my $debug=0;
my $cast_path="";
my $cell;
my $verilog;
my $workdir="atpg2";
my $cellname;
my $pdk_root=$ENV{FULCRUM_PDK_ROOT};
my $spar_dir="/home/local/checkouts/alta/alta/layout/tsmc65/spar";
my $prefix="TOP_RPREFIX_";
my %cdcs=();
my %assign=();
my %used=();
my $prefix="";
my $clk="clk";

sub usage {
    print STDERR <<EU;
Usage: mkpinassign [options]
    --verbose
    --debug  not implemented
    --cell=<mid cell name> 
    --spar-dir <spar-dir>
    --verilog <verilog source>
    --work-dir <usually atpg>
    --fulcrum-pdk-root <pdk>
    --prefix <from SYNC_PREFIX in proteus>
    --clk=<name>
EU
    exit 1;
}

GetOptions (
    "verbose" => \$verbose,
    "debug" => \$debug,
    "cell=s" => \$cell,
    "spar-dir=s" => \$spar_dir,
    "verilog=s" => \$verilog,
    "work-dir=s" => \$workdir,
    "fulcrum-pdk-root=s" => \$pdk_root,
    "prefix=s" => \$prefix,
    "clk=s" => \$clk,
) or usage;

if ( ! -d $pdk_root) {
    $pdk_root=`fulcrum --path --pdk tsmc65 pdk`;
    chomp $pdk_root;
}
my $atpgverilog=$verilog;
$atpgverilog =~ s/\.v$//;
$atpgverilog = "$workdir/$atpgverilog.atpg.v";
if ( -s "$atpgverilog") {
    printf STDERR "$atpgverilog exists, continue? [y/n] ";
    my $ans=<STDIN>;
    if (! ($ans =~ /^y/i)) {
        exit 1;
    }
}

sub set {
    my ($pin,$dir)=@_;
    if ($pin =~ /^SCAN_(IN|OUT|IDL|EN|MODE)/) {
        my $ty=$1;
        my $ndx=0;
        if ($pin =~ /\[(\d+)\]/) {
            $ndx=$1;
        }
        if ($ty eq "IDL") {
            $assign{$pin}="-TI";
        }
        elsif ($ty eq "OUT") {
            $assign{$pin}="SO$ndx";
        }
        elsif ($ty eq "IN") {
            $assign{$pin}="SI$ndx";
        }
        elsif ($ty eq "EN") {
            $assign{$pin}="+SE";
        }
        elsif ($ty eq "MODE") {
            $assign{$pin}="+TI";
        }
    }
    elsif (($pin =~ /reset/i) and ($dir eq "input")) {
        my $neg=0;
        $neg=1 if $pin =~ /_n$/i;
        $assign{$pin}=sprintf "%sSC", $neg ? "+" : "-";
    }
    elsif ($pin eq "CLK") {
        $assign{$pin}="-ES";
    }
}

$cell=shift if @ARGV;
my %cdcs=();
my %refcells=();
my %notacell=(
    "module" => 1,
    "input" => 1,
    "inout" => 1,
    "output" => 1,
    "assign" => 1,
    "wire" => 1,
    "endmodule" => 1,
    "//" => 1,
    "" => 1,
);

my %macro=();
my %ports=();
my %connects=();
my %midports=();

if (defined ($verilog)) {
    if ( -s $verilog ) {
        local( *P,$/);
        $/=";";
        open (P, "<$verilog") or die;
        my $last=0;
        my $topmod=0;
        while (<P>) {
            my $x=$_;
            s/(\S+)lib\.synchronous/\\lib.synchronous/g;
            my $pfx=$1;
            $pfx =~ s/\\//;
            s/\s+/ /g;
            s/^ //;
            s/ $//;
            s/\\//g;
            print STDERR "Warning: apparent prefix mismatch: $pfx vs $prefix" if $pfx ne "" and $pfx ne $prefix;
            if ($topmod) {
                if ( ! (/wire/ or /output/ or /input/)) {
                    my ($macro,$inst,$rest)=split(/ /, $_, 3);
                    $macro{$inst}=$macro;
                    $rest =~ s/^\(//;
                    $rest =~ s/\)$//;
                    $rest =~ s/\s*\);$//;
                    my @rest=split(/,/, $rest);
                    foreach my $pl (@rest) {
                        $pl =~ /\s*(\S+)\s*\(\s*(.+)\s*\)/;
                        my $signal=$2;
                        my $pin=$1;
                        $pin =~ s/^\.//;
                        $pin =~ s/\s//g;
                        $signal =~ s/\s//g;
                        $ports{$inst}->{$pin}=$signal;
                        if ($inst eq "mid" and $signal ne "") {
                            $midports{$signal}=1;
                        }
                        push @{$connects{$signal}}, "$inst $pin";
                    }
                }
            }
            $topmod=1 if /^\s*endmodule/ and $last;
            if (! $topmod) {
                my $ln1=$_;
                my ($cn1)=split(/ /,$ln1);
                $cn1 =~ s/\\//g;
                $refcells{$cn1} |= 1 if ! (($notacell{$cn1}) or ($cn1 =~ /^(av_|lib\.synchronous\.conversion\.v3\.|vendor\.avago\.svt\.gates)/));
            }
            if ( ! $topmod) {
                if (/\s*module\s/) {
                    s/.*\smodule\s//;
                    s/([^\(]+)\(//;
                    $cellname=$1;
                    $cellname =~ s/[\\\s]//g;
                    $refcells{$cellname} |= 2;
                    s/\s\)\s*//;
                    s/;\s*$//;
                    s/\\//g;
                    s/[,\s]+/ /g;
                    $last=1 if $cellname eq $cell;
                    $cdcs{$cellname} = 1 if $cellname =~ /lib\.synchronous\.conversion\.v3\.SCAN_(A2S|S2A)-L/;
                }
                if ($cellname eq $cell) {
                    if (/(in|out)put/) {
                        my $dir=$1;
                        s/[\s,]+/ /g;
                        s/^ //;
                        s/ *;$//;
                        my @f=split;
                        my $dir=shift @f;
                        my $b=-1;
                        my $e=-1;
                        if ($f[0] =~ /^\[(\d+):(\d+)\]$/) {
                            $b=$2;
                            $e=$1;
                            shift @f;
                        }
                        foreach my $pin (@f) {
                            if ($b >= 0) {
                                for (my $n = $b; $n <= $e; $n++) {
                                    set("$pin\[$n\]",$dir);
                                }
                            }
                            else {
                                set($pin,$dir);
                            }
                        }
                    }
                    elsif (/\swire\s/ or /\sreg\s/) {
                    }
                    else {
                        my $ln=$_;
                        while ($ln =~ /SCAN_(IN)\[(\d+)\]/) {
                            my $ty=$1;
                            my $ndx=$2;
                            $used{"SCAN_$ty\[$ndx\]"}=1;
                            $ln =~ s/SCAN_(IN)\[(\d+)\]//;
                        }
                        my $ln=$_;
                        while ($ln =~ /SCAN_(OUT)\[(\d+)\]/) {
                            my $ty=$1;
                            my $ndx=$2;
                            if ($ln !~ /assign.*SCAN_OUT/) {
                                $used{"SCAN_$ty\[$ndx\]"}=1;
                            }
                            $ln =~ s/SCAN_(OUT)\[(\d+)\]//;
                        }
                        my $ln=$_;
                        while ($ln =~ /SCAN_(IDL)\[(\d+)\]/) {
                            my $ty=$1;
                            my $ndx=$2;
                            $used{"SCAN_$ty\[$ndx\]"}=1;
                            $ln =~ s/SCAN_(IDL)\[(\d+)\]//;
                        }
                    }
                }
            }
        }
        close P;
    }
    else {
        print STDERR "Cannot find verilog file $verilog.";
        usage;
    }
}
else {
    usage;
}
mkdir $workdir;

my %found=();
sub searchconnect {
    my $signal=$_[0];
    $found{$signal}=1;
    foreach my $connect (@{$connects{$signal}}) {
        my ($inst,$pin)=split(/ /, $connect);
        my %sigs=%{$ports{$inst}};
        foreach my $pn (sort keys %sigs) {
            next if $pn ne "Q";
            next if $pn eq $pin;
            print "INVERTED" if ! $macro{$inst} =~ /buf/;
            searchconnect($sigs{$pn});
        }
    }
}
searchconnect("$clk");
my @clks=();
foreach my $sig (sort keys %found) {
    my %pl=%{$ports{mid}};
    foreach my $pin (sort keys %pl) {
        if ( "$pl{$pin}" eq "$sig") {
            $assign{$pin}=$assign{CLK};
            push @clks, $pin;
        }
    }
}
open (P, ">$workdir/$cell.FULLSCAN.pinassign");
my %target=();
foreach my $pin (sort keys %assign) {
    my $assign=$assign{$pin};
    $assign =~ s/^[-+]//;
    next if ($pin =~ /^SCAN_(IN|OUT|IDL)\[/) and ! $used{$pin};
    push @{$target{$assign}}, $pin;
}
foreach my $assign (sort keys %target) {
    my $n=0;
    my $dir="";
    my $fpin="";
    foreach my $pin (@{$target{$assign}}) {
        if ($n == 0 or ($assign ne "SC" and $assign ne "ES")) {
            print P "assign pin=$pin test_function= $assign{$pin};";
            $dir=$assign{$pin};
            $fpin=$pin;
        }
        else {
            if ($dir eq $assign{$pin}) {
                print P "correlate $pin + $fpin;";
            }
            else {
                print P "correlate $pin - $fpin;";
            }
        }
        $n++;
    }
}
#foreach my $pin (sort keys %assign) {
#    next if ($pin =~ /^SCAN_(IN|OUT|IDL)\[/) and ! $used{$pin};
#    my $assign=$assign{$pin};
#    if ($pin !~ /^[a-zA-Z]/) {
#        $pin = "\"$pin\"";
#    }
#    print P "assign pin=$pin  test_function= $assign;";
#    $used{$pin}=1;
#}
close P;

my $pwd=`pwd`;
chomp $pwd;
my $techlibextra="";
my %file;
open (X, "<vendorlist") or open (X, "<../vendorlist");
while (<X>) {
    chomp;
    my ($file,$cell)=split;
    $file{$cell}=$file;
}
close X;
my %skip=();
foreach my $cell (sort keys %refcells) {
    if ($refcells{$cell}==1) {
        my $cx = $cell;
        $cx = "vendor/artisan/memory/$cell/$cell.v" if ($cell =~ /^REG/) or ($cell =~ /^SRAM/);
        $cx = "$file{$cell}" if defined $file{$cell};
        print STDERR "Warning: Cannot locate verilog for $cell $spar_dir/$cx" if ! -s "$spar_dir/$cx";
        $cx = "\$SPAR_ROOT/$cx";
        $skip{$cx}=1;
        $techlibextra .= ",$cx";
    }
}
my $template =<<ET;
#!/bin/ksh
# Generated by proteus
# to run: cd $pwd
#         et et -e $workdir/runet.atpg

export WORKDIR=$workdir
export CELL=$cell
export PDK_ROOT=$ENV{FULCRUM_PDK_ROOT}
export MODE=FULLSCAN
export SPAR_ROOT=/home/user/aubrey/alta/spar

if [ -z "\$STAGE" ]; then
if [ -e \$WORKDIR/tbdata ] ; then
  rm -rf \$WORKDIR/tbdata
fi

if [ -e \$WORKDIR/testresults ] ; then
  rm -rf \$WORKDIR/testresults
fi
fi

if [ -z "\$STAGE" -o "\$STAGE" = "build_model" ]; then
build_model \\
    cell=\$CELL \\
    defaultinouts=no \\
    allowincomplete=yes blackboxoutputs=z \\
    DESIGNSOURCE=$atpgverilog \\
    TECHLIB=\$SPAR_ROOT/vendor/avago/svt/gates/verilog/av65gp_svt_atpg.v,\$SPAR_ROOT/vendor/avago/svt/gates/verilog/av65gp_ck_svt_atpg.v,\$SPAR_ROOT/vendor/avago/lvt/gates/verilog/av65gp_lvt_atpg.v,\$SPAR_ROOT/vendor/avago/hvt/gates/verilog/av65gp_hvt_atpg.v$techlibextra \\
	industrycompatible=yes
     # vlogparser=IEEEstandard
# Uncomment to use the NC Verilog Parser; default: ET parser
fi

if [ -z "\$STAGE" -o "\$STAGE" = "build_fault_model" ]; then
build_faultmodel \\
    includedynamic=no \\
	cellfaults=yes
fi

if [ -z "\$STAGE" -o "\$STAGE" = "build_testmode" ]; then
build_testmode \\
    TESTMODE=\$MODE \\
    ASSIGNFILE=\$WORKDIR/\$CELL.\$MODE.pinassign \\
    MODEDEF=\$MODE 
fi

if [ -z "\$STAGE" -o "\$STAGE" = "report_faults" ]; then
report_faults \\
 workdir=\$WORKDIR \\
  testmode=\$MODE
fi

if [ -z "\$STAGE" -o "\$STAGE" = "report_fault_statistics" ]; then
report_fault_statistics
 workdir=\$WORKDIR \\
  testmode=\$MODE
fi

if [ -z "\$STAGE" -o "\$STAGE" = "report_test_structures" ]; then
report_test_structures \\
  testmode=\$MODE 
fi

if [ -z "\$STAGE" -o "\$STAGE" = "verify_test_structures" ]; then
verify_test_structures \\
  testmode=\$MODE
fi

if [ -z "\$STAGE" -o "\$STAGE" = "create_scanchain_tests" ]; then
create_scanchain_tests \\
 workdir=\$WORKDIR \\
  experiment=chain_test \\
    testmode=\$MODE \\
    contentionreport=hard \\
   contentionremove=yes
fi

if [ -z "\$STAGE" -o "\$STAGE" = "commit_tests" ]; then
commit_tests \\
 workdir=\$WORKDIR \\
  inexperiment=chain_test \\
    testmode=\$MODE
fi

if [ -z "\$STAGE" -o "\$STAGE" = "create_logic_tests" ]; then
create_logic_tests \\
 workdir=\$WORKDIR \\
  experiment=logic \\
    testmode=\$MODE \\
	effort=high \\
    contentionreport=hard \\
   contentionremove=yes
fi

if [ -z "\$STAGE" -o "\$STAGE" = "commit_tests" ]; then
commit_tests \\
 workdir=\$WORKDIR \\
  inexperiment=logic \\
    testmode=\$MODE
fi

if [ -z "\$STAGE" -o "\$STAGE" = "report_fault_statistics2" ]; then
report_fault_statistics
 workdir=\$WORKDIR \\
  testmode=\$MODE
fi

if [ -z "\$STAGE" -o "\$STAGE" = "write_vectors" ]; then
write_vectors \\
 workdir=\$WORKDIR \\
    testmode=\$MODE \\
	language=wgl
fi
ET

open (P, ">$workdir/runet.atpg");
print P $template;
close P;
chmod 0755,"$workdir/runet.atpg";
open (P, "<$verilog");
open (Q, ">$atpgverilog");
while (<P>) {
    chomp;
    if (/^\/\//) {
        print Q;
    }
    else {
        print Q "";
        last;
    }
}
close P;
my $last=0;
print Q "";
my %printed;
foreach my $cdc (sort keys %cdcs) {
    my $cdcname=$cdc;
    $cdcname =~ s/$prefix//;
    $cdcname =~ s/\.500_\d+$//;
    $cdcname =~ s/\.500$//;
    $cdcname .= ".500";
    $printed{$cdcname}=1;
    if ( ! $skip{$cdcname}) {
        my $name=$cdcname;
        $name =~ s/-L/(/;
        $name =~ s/-R/)/;
        $name =~ s/^$prefix//;
        $name = "lib/synchronous/conversion/v3/atpg/$name.v";
        if (-s "$spar_dir/$name") {
            open (P, "<$spar_dir/$name");
            while (<P>) {
                chomp;
                next if /^$/;
                print Q;
            }
            close P;
            $skip{$cdcname}=1;
        }
        else {
            print STDERR "Warning: Cannot locate verilog for $cdcname";
        }
    }
}

open (P, "<$verilog");
my $skip=0;
my $module="";
my $inst="";
my %badports=();
while (<P>) {
    chomp;
    next if /^\/\//;
    next if /^\s*$/;
    s/\S+lib\.synchronous/\\lib.synchronous/g;
    if (/lib\.synchronous/) {
        s/500_\d+/500/;
    }
    if (/^module\s+(\S+)/) {
        $module=$1;
        $module =~ s/\($//;
        $module =~ s/\\//;
        if ($module =~ /lib\.sync/) {
            $module =~ s/500_\d+$/500/;
        }
        $skip = $printed{$module} ? 1 : 0;
        $skip = 1 if $last;
        $printed{$module}=1;
        $last = 1 if ($module eq $cell);
    }
    elsif (/lib\.synchronous/) {
        m/(lib.sync\S+)/;
        $inst=$1;
    }
    if ((/SCAN\.OUT/) and ($inst =~ /lib\.sync/) and (/\),\s*$/)) {
        s/\),\s*$/\)\);/;
        if (! $skip) {
#            print STDERR "Port list incorrect for $inst ($_)";
            print Q;
            while (<P>) {
                chomp;
                #print STDERR "  $_";
                my $ln = $_;
                $ln =~ s/[,\.\(\)\;\s]+/ /g;
                $ln =~ s/^ //;
                $ln =~ s/ $//;
                foreach my $p (split(/ /, $ln)) {
                    $badports{$p}=1;
                }
                last if /;/;
            }
        }
        next;
    }
    if ((/cdc_reset_n.*cdc_reset_n/) and ($inst =~ /lib\.sync/)) {
        $inst="" if (/;/);
        next;
    }
    $inst="" if (/;/);
    print Q if (! $skip);
}
close P;
close Q;
open P, ">runme.sh";
print P <<ER;
#!/bin/bash
log=$workdir/runet.log
if [ -s "\$log" ]; then
   n=0
   while [ -s "$workdir/runet.log\$n" ]; do
      n=`expr \$n + 1`
   done
   log="$workdir/runet.log\$n"
fi
/p/rrc/tools/bin/et et -e $workdir/runet.atpg 2>&1 | tee \$log
ER
close P;
chmod 0755, "runme.sh";
if (0) { # just for fun
open (P, "<$verilog");
my $eg=join("|", sort keys %badports);
my %printed=();
print "";
my $module;
while (<P>) {
    chomp;
    s/\s+/ /g;
    if (/^\s*module\s+(\S+)/) {
        $module=$1;
        if (($module =~ /mid/) or ($module =~ /lib\.sync/)) {
            $printed{$module}=0;
        }
        else {
            $printed{$module}=2;
        }
    }
    if ((/($eg)/) and $printed{$module} < 2) {
        if ($printed{$module} == 0) {
            print $module;
            $printed{$module}=1;
        }
        print;
    }
}
}
