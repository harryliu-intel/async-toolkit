#!/usr/intel/bin/perl
use strict;
use warnings;
use Getopt::Long;

my ($mem, $cell);
my $path = '.';
my $output = '/dev/stdout';
my @defines = ();

GetOptions("cast-path=s" => \$path,
           "cell=s" => \$cell,
           "define=s" => \@defines,
           "max-heap-size=s" => \$mem,
           "output=s" => \$output) || die "Can't parse arguments: $!";

if (!defined($cell)) {
    print STDERR <<EOF;
Usage: verilog_filelist --cell=<cosim spec> (cell to generate filelist for)
                        [--cast-path=<path> (CAST path, defaults to .)]
                        [--max-heap-size=<mem>]
                        [--define=<CAST define> ...]
                        [--output=<file> (defaults to /dev/stdout)]
EOF
    exit(2);
}

my @cmd = ('cast2verilog',
           "--cast-path=$path",
           "--cell=$cell",
           '--output-file=/dev/null',
           "--file-list=$output",
           map { "--define=$_" } @defines);
push @cmd, "--max-heap-size=$mem" if defined $mem;

exec(@cmd);

exit(1); # in case cast2verilog cannot be executed
