#!/usr/bin/env perl
# vim:et:sw=4:ts=4:tw=79:

use strict;
use warnings;

use File::Find;
use FindBin qw($Bin);
use Getopt::Long;
use IO::File;
use Pod::Usage;

###############################################################################
# Constants
###############################################################################

use constant EMPTY_STRING => "";

###############################################################################
# Global Variables
###############################################################################

our $local = 1;
our $numFails = 0;
our $pattern = undef;
our $verbose = 0;

our $class = "com.fulcrummicro.util.properties.PropReader";
our ($classpath);

our $java = "java";
our $p4 = "p4";

###############################################################################
# Local Functions
###############################################################################

sub dprintf
{
    if ($verbose)
    {
        printf(shift(@_), @_);
    }

}   # end dprintf


sub run
{
    return if (defined($pattern) and !/$pattern/);

    my $reader = "$java -classpath $classpath $class";
    my ($args, $expected, $optfile, $test);

    if (/^(t\d+)\.test$/)
    {
        $args = "$1.args";
        $expected = "$1.expected";
        $optfile = "$1.xml";
        $test = "$1.test";
    }
    elsif (/^(t\d+)$/)
    {
        $args = "$1/args";
        $expected = "$1/expected";
        $optfile = "$1/t.xml";
        $test = "$1/test";
    }
    else
    {
        return;
    }

    print("*** $File::Find::name ***\n");
    $optfile = EMPTY_STRING if (!-e($optfile));
    my $cmd = "$reader common.xml $optfile --config=$test --topdir=. --platform=test";
    if (-e($args))
    {
        my $fh = IO::File->new($args, "r") or die("can't open $args");
        while (my $line = $fh->getline())
        {
            chomp($line);
            $cmd .= " $line";
        }
        $fh->close();
    }
    dprintf("  * $cmd\n");
    my @output = readpipe("$cmd | sort | diff $expected -");
    dprintf("@output");
    if ($? != 0)
    {
        print("FAIL: test $File::Find::name\n");
        $numFails++;
    }

}   # end run


sub trim
{
    my ($s) = @_;

    $s =~ s/^\s+//;
    $s =~ s/\s+$//;
    return $s;

}   # end trim


###############################################################################
# Public Functions
###############################################################################

sub main
{
    my $status = GetOptions
    (
        'java:s'    => \$java,
        'help'      => sub { pod2usage(0) },
        'local!'    => \$local,
        'p4:s'      => \$p4,
        'verbose!'  => \$verbose
    );
    if (!$status)
    {
        pod2usage(2);
    }

    $pattern = $ARGV[0] || '.';

    # Retrieve the root of the regression tree.
    chomp(my @output = readpipe("$p4 -s client -o"));
    my ($p4root) = grep {m/info:\s+Root:/} @output;
    $p4root = trim((split(/:/, $p4root))[-1]);
    (my $root = $Bin) =~ s,($p4root/reg-\w+).*,$1,;

    # Configure the Java class path.
    my $jars = "$root/shared/jars";
    $classpath = $local ? "$root/shared/java/classes" : "$jars/util.jar";
    $classpath .= ":$jars/jep-2.4.1.jar";

    find(\&run, '.');
    if ($numFails > 0)
    {
        print("$numFails tests failed\n");
    }
    else
    {
        print "PASS\n";
    }

}   # end main

main();

__END__

=head1 NAME

test.pl - runs Java XML property reader unit tests

=head1 SYNOPSIS

test.pl [F<-nolocal> F<-verbose>] [F<test>]

=head1 OPTIONS

=over 4

=item B<-nolocal>

Use the XML property reader archived in util.jar.

=item B<-verbose>

Verbose mode. Verbose mode can be disabled using B<-noverbose>.

=back

=head1 DESCRIPTION


=head1 AUTHOR

Written by Kevin Duncklee

INTEL CONFIDENTIAL
Copyright 2008 - 2013 Intel Corporation
All Rights Reserved.
