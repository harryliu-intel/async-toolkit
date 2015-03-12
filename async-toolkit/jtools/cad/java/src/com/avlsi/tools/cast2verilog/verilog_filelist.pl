#!/usr/intel/bin/perl
use strict;
use warnings;
use Getopt::Long;
use File::Spec::Functions;

my ($mem, $cell);
my $path = '.';
my $output = '/dev/stdout';
my @defines = ();
my @verilog_dirs = ();
my $use_dcdft = 0;

sub try_dft {
    my ($file) = @_;
    if ($use_dcdft) {
        if ($file !~ /dc_dft\.v/ && $file =~ /(.*)(\.v(?:\.gz)?)/) {
            my $full = "$1.dc_dft$2";
            return $full if -e $full;
        }
    }
    return -e $file ? $file : undef;
}

sub find_file {
    my ($dirs, $file) = @_;
    if (file_name_is_absolute($file)) {
        my $full = try_dft($file);
        return $full if $full;
    } else {
        foreach my $dir (@{$dirs}) {
            my $full = try_dft(catfile($dir, $file));
            return $full if $full;
        }
    }
    return undef;
}

GetOptions("cast-path=s" => \$path,
           "cell=s" => \$cell,
           "define=s" => \@defines,
           "max-heap-size=s" => \$mem,
           "output=s" => \$output,
           "use-dc-dft!" => \$use_dcdft,
           "verilog-dir=s" => sub { @verilog_dirs = split /:/, $_[1] })
   || die "Can't parse arguments: $!";

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

my $rawout = @verilog_dirs ? '/dev/stdout' : $output;
my @cmd = ('cast2verilog',
           "--cast-path=$path",
           "--cell=$cell",
           '--output-file=/dev/null',
           "--file-list=$rawout",
           map { "--define=$_" } @defines);
push @cmd, "--max-heap-size=$mem" if defined $mem;

if (@verilog_dirs) {
    my @found = ();
    my @missing = ();
    local $" = "\n";
    open(my $fh, '-|', @cmd) or die "Can't execute @cmd: $!";
    while(<$fh>) {
        chomp;
        next if /^#/ || /^\s*$/;
        my ($switch, $file) = ('', $_);
        if (/^(-[vf] )(\S+)/) {
            ($switch, $file) = ($1, $2);
        }
        $file = find_file(\@verilog_dirs, $file);
        if (defined($file)) {
            push @found, "$switch$file";
        } else {
            push @missing, $_;
        }
    }
    close($fh);
    if (@missing) {
        die "Can't resolve the following files: @missing\n";
    }
    open(my $oh, '>', $output) or die "Can't open $output: $!";
    print $oh "@found\n";
    close($oh);
} else {
    exec(@cmd);

    exit(1); # in case cast2verilog cannot be executed
}
