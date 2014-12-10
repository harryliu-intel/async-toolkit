#!/usr/intel/bin/perl
# Copyright 2005 Fulcrum Microsystems.  All rights reserved.
# Authors: Kai He

use strict;

use FileHandle;
use IPC::Open2;
use File::Temp qw/ tempfile tempdir /;

# Find relevant packaged tools
our $package_root = $0;
my $exe = $package_root;
$exe =~ s:.*/::;
if (! ($package_root =~ m:^/:)) {
    my $pwd = `pwd`;
    chomp $pwd;
    $package_root = $pwd;
    $package_root .= "/$0";
    $package_root =~ s:$exe$::;
    $package_root =~ s://:/:g;
    chdir $package_root;
    $package_root = `pwd`;
    chomp $package_root;
    chdir $pwd;
}
else {
    $package_root =~ s:/bin/$exe::;
}

my $rename = "rename";
my $mkcdswd =  "mkcdswd";
my $createAbstract =  "createAbstract";
my $cds_sh_lib="$package_root/share/script/sh/util";

my $output_dir      = "$ENV{PWD}";
my $cast_dir        = "$ENV{HOME}/hw/cast";
my $spec_dir        = "$ENV{HOME}/hw/layout/tsmc13/spec";
my $dfII_dir        = "$ENV{HOME}/hw/layout/tsmc13/dfII";
my $fulcrum_pdk_root = "$ENV{FULCRUM_PDK_ROOT}";
my $maxheapsize     = "1.5G";
my $verbose=0;


sub usage() {
    my $usage  = "USAGE: $0 \n";
    $usage .= "    --fulcrum-pdk-root=<path name> default()\n";
    $usage .= "    --dfII-dir=<path name> (used for instantiator views and for mkcdswd)\n";
    $usage .= "    --cell=<cellname> default()\n";
    $usage .= "    --lef-output=<filename> \n";
    $usage .= "    [--cds-log=<filename>] \n";
    $usage .= "    [--abstract-log=<filename>] \n";
    $usage .= "    [--working-dir=(default /scratch/XXXX)] \n";
    $usage .= "    [--extracted-view=layout] \n";
    $usage .= "    [--verbose] \n";
    $usage .= "    [--cast-dir=<path>] \n";
    $usage .= "    [--spec-dir=<path>] \n";
    $usage .= "    [--max-heap-size=memsize] \n";
    die "$usage";
}

my $cell;
my $lef;
my $working_dir;
my $view;
my $cds_log;
my $abstract_log;

while (defined $ARGV[0] && $ARGV[0] =~ /^--(.*)/) {
    my ($flag,$value) = split('=',$1);
    $value = 1 if ! defined $value;
    if ($flag eq "fulcrum-pdk-root") {
        $fulcrum_pdk_root = $value;
        shift @ARGV;
    } elsif ($flag eq "max-heap-size") {
        $maxheapsize = $value;
        shift @ARGV;
    } elsif ($flag eq "dfII-dir") {
        $dfII_dir = $value;
        shift @ARGV;
    } elsif ($flag eq "cast-dir") {
        $cast_dir = $value;
        shift @ARGV;
    } elsif ($flag eq "spec-dir") {
        $spec_dir = $value;
        shift @ARGV;
    } elsif ($flag eq "cell") {
        $cell = $value;
        shift @ARGV;
    } elsif ($flag eq "lef-output") {
        $lef = $value;
        shift @ARGV;
    } elsif ($flag eq "working-dir") {
        $working_dir = $value;
        shift @ARGV;
    } elsif ($flag eq "extracted-view") {
        $view = $value;
        shift @ARGV;
    } elsif ($flag eq "cds-log") {
        $cds_log = $value;
        shift @ARGV;
    } elsif ($flag eq "abstract-log") {
        $abstract_log = $value;
        shift @ARGV;
   } else { 
    shift @ARGV;
   }
}

defined $fulcrum_pdk_root or usage();
defined $dfII_dir or usage();
defined $lef or usage();
defined $abstract_log or $abstract_log="abstract.log";
defined $view or $view="abstract";
my $abstract_view=$view;

my $blank_lib="$fulcrum_pdk_root/share/Fulcrum/blank-library";

my $pid1=open2(*CELL_IN,*CELL_OUT,"$rename --type=cell --from=cast --to=cadence");

sub cell_rename {
    print CELL_OUT $_[0]."\n";
    my $name=<CELL_IN>;
    chomp($name);
    return $name;
}


defined $working_dir or
    $working_dir  = tempdir( CLEANUP => 1, DIR => "/scratch" );
if (defined ($ENV{LVE_DIRMODE})) {
    chmod $ENV{LVE_DIRMODE}, "$working_dir";
    if (defined($ENV{LVE_SGID}) and $ENV{LVE_SGID} == 1) {
        `chgrp $ENV{LVE_GID} "$working_dir" &>/dev/null`;
    }
}
else {
    chmod 0755, $working_dir;
}

my $cellname=cell_rename($cell);
my @segments=split('\.', $cellname);
my $libname=$segments[0];
for(my $i=1; $i<=$#segments-2; $i++){
        $libname.=".".$segments[$i];
}

my $cadencename=$cellname;
$cadencename =~ s/\./#2e/g;
$cadencename =~ s/-/#2d/g;
$cadencename =~ s/\$/#24/g;
my $libpath=$libname;
$libpath =~ s/\./\//g;
my $abstractcdb="$dfII_dir/$libpath/$cadencename/$abstract_view/layout.oa";
if (! -s $abstractcdb ) {
    print STDERR "Warning: LEF Out for $cell, no $abstract_view view, fallback to layout view.\n";
    $abstract_view="layout";
    $abstractcdb="$dfII_dir/$libpath/$cadencename/$abstract_view/layout.oa";
    if (! -s $abstractcdb ) {
        print STDERR "Error: LEF Out for $cell, no $abstract_view view.\n";
        exit 1;
    }
}

my $cast_path = "$cast_dir:$spec_dir";
my $cmd="$mkcdswd --target-dir=$working_dir --fulcrum-pdk-root=$fulcrum_pdk_root --cast-path=$cast_path --force --dfII-dir=$dfII_dir";
print "Creating temp dir ($working_dir)...\n" if $verbose;
system($cmd);

open( SOURCE, "<$working_dir/cds.lib");
open( TARGET, ">$working_dir/cds.lib_tmp");
while(my $line=<SOURCE>){
    $line=~s/\$\{FULCRUM_PDK_ROOT\}/$fulcrum_pdk_root/g;
    print TARGET $line;  
}
close(TARGET);
close(SOURCE);
system("mv $working_dir/cds.lib_tmp $working_dir/cds.lib");
my $tempfile = tempfile( CLEANUP => 1, DIR => "/scratch" );

open (GENLEF, ">$working_dir/genlef.il");
print GENLEF <<EG;
load("$package_root/share/skill/layout/lefdef/lefdef.il")
pout=outfile("$tempfile")
lefInstanceOut( pout "$libname" "$cellname" "$abstract_view" ?Verbose nil )
close(pout)
exit
EG
close GENLEF;

my $cmd = "layout -nograph -log '$working_dir/lef.log' -replay '$working_dir/genlef.il'";

chdir $working_dir or die "Cannot chdir to $working_dir";
system ($cmd);

my $ckpin = 
"    PIN CK
        DIRECTION INPUT ;
        PORT
        LAYER M1 ;
        RECT 0.12 0.12 0.36 0.36 ;
        END
    END CK
";

my %dir=();
if( -s "$tempfile") {
    if ( $cast_path ne ":") {
        my @ports = `cast_query --max-heap-size='$maxheapsize' --cast-path='$cast_path' --task=external_nodes=di:im --no-header --cell='$cell'`;
        chomp @ports;
        foreach my $port (@ports) {
            my $dir = substr($port,0,1);
            if ($dir =~ /[-+]/) {
                $port = substr($port,1);
                $dir = $dir eq "+" ? "OUTPUT" : "INPUT";
            }
            else {
                $dir = "INOUT";
            }
            $dir{$port}=$dir;
        }
    }
    open (X, "<$tempfile");
    open (Y, ">$lef");
    my $lefvc = $lef;
    $lefvc =~ s/\.lef/_vc.lef/;
    open (V, ">$lefvc");
    my $pin;
    my $done=0;
    print V "\nBUSBITCHARS \"<>\" ;\n\n";
    while (<X>) {
        chomp;
        if (/^\s*PIN\s+([^\s]+)/) {
            $pin = $1;
        }
        elsif (/^(\s*)DIRECTION\s+INOUT/ and defined ($dir{$pin})) {
            $_ = "${1}DIRECTION $dir{$pin} ;";
        }
        if ((/^END / or /^\s*OBS/) and ! $done) {
            print V $ckpin;
            $done = 1;
        }
        s/-0\.000\s/0.000 /g;
        print Y "$_\n";
        if (/^\s*SITE\s*CoreSite/) {
            next;
        }
        elsif (/^(\s*)CLASS\s/) {
            print V "${1}CLASS BLOCK ;\n";
            next;
        }
        elsif (/^\s*SYMMETRY\s/) {
            $_ =~ s/;/R90 ;/;
        }
        print V "$_\n";
    }
    close X;
    close Y;
    close V;
    unlink "$tempfile";
    print "Success: lefOut file is $lef.\n" if $verbose;
} else {
    system("rm -f '$lef'");
    print STDERR "Error: LEF Out Failed for $cell.\n";
    exit 1;
}
exit 0;
