#!/usr/intel/bin/perl -l

use strict;

use Getopt::Long;

my $include="== 0";

sub usage {
    print STDERR "Usage: genconfig [--include] arg list";
    print STDERR "    arg list is either arg or arg=value";
    print STDERR "   --include makes config for functions USING this arg";
    print STDERR "     otherwise for those NOT using this arg";
    print STDERR "     gets default value from existing .fulcrum.config file";
    print STDERR "     unless specified as arg=value";
    exit 1;
}

GetOptions (
    "include" => sub { $include="!= 0";},
) or usage;

usage if ! @ARGV;

my $packageRoot=`fulcrum --latest --path lve`;
chomp $packageRoot;
$packageRoot =~ s:/bin/lve::;
my @args=@ARGV;
my %args=();
my %keys=();
my $hasconfig=0;
if ( -f ".fulcrum.config") {
   open (P, "<.fulcrum.config") or die;
   $hasconfig=1;
}
foreach my $n (0..$#args) {
    my ($arg,$argvalue)=split(/=/, $args[$n], 2);
    $argvalue="" if ! defined $argvalue;
    my $key=1 << $n;
    my $cmd="egrep -c '\"$arg(\"|=)|app_name' '$packageRoot/bin/'* | awk -F: '\$2$include {print \$1}'";
    my @list=`$cmd`;
    chomp @list;
    printf "- " if $include eq "== 0";
    foreach my $file (@list) {
#        if ( ! -l $file ) {
            $file =~ s:.*/::;
            printf "$file ";
#        }
    }
    if ($argvalue eq "") {
        seek P, 0, 0;
        while (<P>) {
            chomp;
            s/^\s+//;
            s/\s+$//;
            if (/^--$arg(\s+|=)(\S+)/) {
                $argvalue=$2;
                last;
            }
        }
    }
    print "\x7b\n--$arg=$argvalue\n\x7d";
}
#print join("\n", @{$args{"dfII-dir"}});
