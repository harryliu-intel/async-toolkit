#!/usr/intel/bin/perl -w

# find relevant packaged tools and libraries
BEGIN {
    $lve_root = $0;
    $lve_root =~ s:/[^/]*$::;
    $lve_root =~ s:/[^/]*$::;
    @INC = ("$lve_root/lib/perl", @INC);
    $cad_dir = $lve_root;
    $cad_dir =~s:/[^/]*$::;
    @INC = ("$cad_dir/lib/perl", @INC);
}
use LveStatus;
use LveUtil;
use Getopt::Long;
use strict;
use File::stat;

my $lve_dir;
my $cell_list;
my $dfII_dir;
my $mode="typ";
sub usage {
    my $usage_str = "USAGE: $0 \n";
    $usage_str .=   "  --lve-dir=<: separated string of lve dirs>\n";
    $usage_str .=   "  --cells=<file specifying cells in the format: <FQCN> <level> >\n";
    $usage_str .=   "  --dfII-dir=path to dfII client <assumes nomodtime option>\n";
    $usage_str .=   "  --extract-mode=[typ|rcb|rcw|cwo|cbe]\n";
    die "$usage_str";
}
GetOptions (
    "lve-dir=s"   => \$lve_dir,
    "cells=s"     => \$cell_list,
    "dfII-dir=s"    => \$dfII_dir,
    "extract-mode=s" => \$mode,
    ) or usage;

usage() unless (defined($cell_list) && defined($lve_dir));

my @cells = ();
my %paths = ();
my @lve_dirs = split(":", $lve_dir);
@cells = get_lines($cell_list);

my $p;
foreach my $d (@lve_dirs) {
    foreach my $l (@cells) {
        my @split_l = split(" ", $l);
        my ($c, $level) = ($split_l[0], $split_l[1]);
        $p = "$d/";
        $p .= fqcn_to_path($c);
        if (-d $p) {
            $paths{$c} = $p;
        }
    }
}

my @views = ("custom_tag", "layout");
my $found_raw = 0;
my $extract_dir = "extracted";
unless ($mode eq "typ") {
    $extract_dir .= "-$mode";
}

foreach my $c (keys %paths) {
    my $dfII_path = dfIIDir($dfII_dir, $c);
    $dfII_path =~ s/\(/#2dL/g;
    $dfII_path =~ s/\)/#2dR/g;
    $dfII_path =~ s/,/_/g;
    foreach my $v (@views) {
        my $raw_file = "$paths{$c}/$v/$extract_dir/extract.raw";
        my $layout_oa = "$dfII_path/$v/layout.oa";
        if (-e $raw_file && -e $layout_oa) {
            my $extract_mtime = (stat($raw_file))->mtime;
            my $dfII_mtime = (stat($layout_oa))->mtime;
            if ($dfII_mtime > $extract_mtime) {
                print "$c\n";
            }
        }
    }
}
