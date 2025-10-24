#!/usr/intel/bin/perl
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

#
use warnings;
use strict;

use Getopt::Long;
use Data::Dumper;
use FileHandle;

## voltage mapping
my %voltage_pin_mapping;
my %bump_pins_mapping;

my $pin_map;
my $out_dir;
my $io_list;
my $help;

GetOptions (
        "pin_map=s",      \$pin_map,
        "io_list=s",      \$io_list,
        "out_dir=s",      \$out_dir,
        "help",           \$help,
        "h",              \$help
);

if (defined $help || !defined $pin_map || !defined $io_list || !defined $out_dir) {
    print <<HELP;
usage $0 :
    REQUIRED:
    -pin_map <File with pin to voltage mapping>
    -io_list <File with list of IO to map>
    -out_dir <Directory where supply port mapping for collage need to written>

HELP
    exit 1;
}

## read the contents of pin_bump_mapping.txt
my %family_2_rail_mapping;

my $fh = new FileHandle ($pin_map, "r") ||
die "Could not open file $pin_map for reading \n";

my @array = <$fh>;
foreach my $line (@array) {
    chomp($line);
    my @values = split(/ /, $line);
    my $family = $values[0];
    chomp($family);
    my $rail_info = undef;
    for (my $count = 1; $count < $#values+1; $count++) {
        $rail_info .= "$values[$count] ";   
    }
    chomp($rail_info);
    $rail_info =~ s/\s\+//g;
    print ">>>$rail_info<<<<\n";
    if (exists $family_2_rail_mapping{$family}) { 
        push (@{$family_2_rail_mapping{$family}}, $rail_info );
    }
    else { 
        @{$family_2_rail_mapping{$family}} = $rail_info; 
    }
}

## print Dumper (\%family_2_rail_mapping);

## get a list of all .lib files to run the command on
# first, get the cell name to .lib mapping list
#$fh = new FileHandle ($io_list, "r") || die "Could not open file $io_list for reading \n";
my %lib_cell_mapping;
#$lib_cell_mapping{"c73p1giolbggpiogppafamily"} = `echo $ENV{MODEL_ROOT}/subIP/HIP_gpio_lib/c73p1giolbggpiogppafamily_0.75-0.94-tttt-70\.lib`;
#$lib_cell_mapping{"c73p1giolbggpiogppbfamily"} = `echo $ENV{MODEL_ROOT}/subIP/HIP_gpio_lib/c73p1giolbggpiogppbfamily_0.75-0.94-tttt-70\.lib`;

open(FILE17,"<$io_list") || die "File '$io_list' does not exist";
while (<FILE17>) {
    my $line17 = $_;
    chomp ($line17);
    if ($line17 =~ /^\s*(\S+)\s+(\S+)/){} 
    else {die"Can not process line $line17 in $io_list";}
    my $libber = $1;
#RJS    my $libber_name = $ENV{MODEL_ROOT} . "/" . $2;
    my $libber_name = $2;
    $lib_cell_mapping{$libber} = $libber_name;
} #while



##print Dumper (\%lib_cell_mapping);

$fh = new FileHandle("$out_dir/generate_collage_mapping.csh", "w") ||die "Could not write to file $out_dir/generate_collage_mapping.csh\n";
print "We are writing to fh which is in out_dir $out_dir\n";
print $fh "#!/bin/csh \n";
print $fh "\n";
print $fh "setenv IDEALTOOLS /p/cse/asic/upf_tools/ideal-II/d10q2ww16b/bin\n";
print $fh "\n";
print $fh "unalias rm \n";
print $fh "unalias mv \n";
print $fh "unalias cp \n";
print $fh "unalias grep \n";
print $fh "\n";

while (my($cell, $lib) = each %lib_cell_mapping) {
    chomp ($cell);
    chomp ($lib);
    $cell =~ s/\s//g;
    print "cell name = >>$cell<<\n and the corresponding lib is $lib\n";
    my $cmd;
    my $ext_lib_supply_ground_cmd;

    next unless (exists $family_2_rail_mapping{$cell});

    my $arr = $family_2_rail_mapping{$cell};
    $ext_lib_supply_ground_cmd = join(" ", @{$family_2_rail_mapping{$cell}});

 
    ## strip timing information from MPHY .lib file as this is causing idlsh to run out of memory
    if ($cell =~ m/c73p1wmhybridx26familyew/) {
        print $fh "cp ${lib} .\n";

        my @arr = split("/", $lib);
        my $local_lib = pop @arr;

        print $fh "chmod 755 ${local_lib}\n";
        print $fh "$ENV{MODEL_ROOT}/tools/upf/scripts/strip_timing_from_lib.pl -lib_name ${local_lib}\n";
        print $fh "rm -f ${local_lib}\n";
        print $fh "mv ${local_lib}.strip ${local_lib}\n";
        print $fh "rm -f ${local_lib}.strip\n";

        $lib = $local_lib;
    }


    if ($cell =~ m/c73p1wmgen3x20familyew/) {
        print $fh "cp ${lib} .\n";

        my @arr = split("/", $lib);
        my $local_lib = pop @arr;

        print $fh "chmod 755 ${local_lib}\n";
        print $fh "$ENV{MODEL_ROOT}/tools/upf/scripts/strip_timing_from_lib.pl -lib_name ${local_lib}\n";
        print $fh "rm -f ${local_lib}\n";
        print $fh "mv ${local_lib}.strip ${local_lib}\n";
        print $fh "rm -f ${local_lib}.strip\n";

        $lib = $local_lib;
    }

    $cmd = "/p/cse/asic/upf_tools/ext_lib_supply_ground/run.csh $cell $lib \"$ext_lib_supply_ground_cmd\" \n";
    print "$cmd\n";
    print $fh "$cmd \n";

}

close ($fh);

## run
my $cmd = "cd $out_dir";
chdir $out_dir || die "Could not cd into $out_dir\n";
system ("pwd");
system("chmod +x generate_collage_mapping.csh");
system("./generate_collage_mapping.csh | tee $out_dir/log.map ");
system("find . -name \"\*\.tcl\" -exec rm \-rf \'\{\}\' \\;");
system("find . -name \"\*\.lib\" -exec rm \-rf \'\{\}\' \\;");
