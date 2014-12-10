#!/usr/intel/bin/perl -l
# AAG
# $Id: rehierverilog.pl,v 1.1 2007/07/26 18:46:08 aubrey Exp aubrey $
# $DateTime$

use strict;
use Getopt::Long;
use Digest::MD5 qw/ md5_hex /;

my $verbose=0;

GetOptions (
    "verbose" => \$verbose,
) or die;

sub usage {
    my ($msg) = @_;
    print STDERR "$msg" if defined($msg) and $msg ne "";
    print <<EU;
Usage: rehierverilog <input> > <output>
EU
exit 1;
}

my $input = $ARGV[0];

my %md5=();
my %reducedmodules=();
my $total=0;
my $reduced=0;
my %content=();

open (P, "<$input") or usage "Cannot open $input";
my $module;
my $content="";
while (<P>) {
    if (/^module/) {
        my @f=split;
        $module=$f[1];
        $content="";
        next;
    }
    if ($module ne "") {
        $content .= $_;
    }
    if (/^endmodule/) {
        $content{$module}=$content;
        my $md5 = md5_hex ($content);
        $md5{$module}=$md5;
        if (defined ($reducedmodules{$md5})) {
            if ($content{$reducedmodules{$md5}} ne $content) {
                print STDERR "Bogus Md5 result $module vs $reducedmodules{$md5}";
            }
            if (length ($module) < length($reducedmodules{$md5})) {
                $reducedmodules{$md5}=$module;
            }
        }
        else {
            $reducedmodules{$md5}=$module;
            $reduced++;
        }
        $total++;
        $module="";
        $content="";
    }
}
print STDERR "Input modules: $total Reduced modules: $reduced" if $verbose;
foreach my $md5 (sort keys %reducedmodules) {
    my $module = $reducedmodules{$md5};
    print "module $module (";
    my @f=split("\n", $content{$module});
    foreach my $f (@f) {
        my @g = split(" ", $f);
        foreach my $g (@g) {
            if (defined ($md5{$g})) {
                $g = $reducedmodules{$md5{$g}};
            }
        }
        $f = join(" ", @g);
    }
    print join("\n", @f);
}
