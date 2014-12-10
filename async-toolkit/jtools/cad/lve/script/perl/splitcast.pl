#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;
use Getopt::Long;

my $spec_dir="";
my @lines;
my $verbose=0;
my $p4=0;
my $p4client="";

sub usage {
    my ($msg)=@_;
    print STDERR $msg
        if (defined ($msg) and $msg ne "");
    print STDERR <<EU;
Usage: splitcast [options] <castfile>
    <castfile>           : is output of vs2cast
    options:
    --client=<p4 client> : required if --p4 specified
    --p4                 : does automatic checkout to make files writable
    --spec-dir=<dir>     : the cast spec directory root
EU
exit 1;
}

usage if $#ARGV < 0;

GetOptions (
    "verbose" => \$verbose,
    "p4" => \$p4,
    "client=s" => \$p4client,
    "spec-dir=s" => \$spec_dir,
) or usage;

usage "No spec-dir specified" if $spec_dir eq "" or ! -d $spec_dir;
usage "no p4 client specified" if $p4 and $p4client eq "";
usage "Input file $ARGV[0] does not exist" if ! -e $ARGV[0];
usage if ! defined ($ARGV[0]) or $ARGV[0] eq "";

sub writefile {
    my @f=split(/ /, $lines[0]);
    $f[$#f] =~ s/;//;
    my $cn = $f[$#f];
    my $libpath=$cn;
    $libpath =~ s/\./\//g;
    $libpath = "$spec_dir/$libpath";
    my $subtype=0;
    system "mkdir -p '$libpath' 2>/dev/null";
    foreach my $ln (@lines) {
        if ($ln =~ /^define/) {
            my @f=split(/"/,$ln);
            $subtype = $f[1];
            last;
        }
    }
    my $exists=0;
    my $writable=0;
    $exists = 1 if -f "$libpath/$subtype.cast";
    $writable = 1 if -w "$libpath/$subtype.cast";
    my $p4edit = ($p4 and $exists and ! $writable);
    system "p4 -c $p4client edit '$libpath/$subtype.cast'"
        if $p4edit;
    if (open (X, ">$libpath/$subtype.cast") ) {
        print STDERR "Writing '$libpath/$subtype.cast" if $verbose;
        print X join("\n", @lines);
        close X;
        system "p4 -c $p4client revert -a '$libpath/$subtype.cast'"
            if $p4edit;
        system "p4 -c $p4client add '$libpath/$subtype.cast'"
            if $p4 and ! $exists;
    }
    else {
        print STDERR "Failed '$libpath/$subtype.cast";
    }
}

open (P, "<$ARGV[0]");
while (<P>) {
    chomp;
    if (/^module/) {
        writefile if @lines;
        @lines=();
    }
    push @lines, $_;
}
writefile if @lines;
