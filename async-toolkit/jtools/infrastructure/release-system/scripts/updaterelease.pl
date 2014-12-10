#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use Getopt::Long;

sub usage {
    print STDERR <<H;
Usage: updaterelease
    updates all of the fulcrum release database (and others) after changing
    the release definition from the perforce depot.
    You must be the build user.
H
exit 1;
}

$ENV{PATH}="/usr/intel/bin:$ENV{PATH}";
my $fulcrumdefault="/usr/local/fulcrum";
my $fulcrum=$fulcrumdefault;
my $tmp = "tmp";
$sname=`uname -s`;
chomp $sname;
$mname=`uname -m`;
chomp $mname;
# latest and all
my %options = (
    "help" => \$arghelp,
    "verbose" => \$verbose,
);

GetOptions ( %options ) or usage;

usage if $arghelp;

$uname=`uname`;
chomp $uname;
if ($uname ne "SunOS") {
    chdir ("$fulcrum/config") or die "Cannot chdir to $fulcrum";
    $out=`P4CONFIG=.p4.config p4 sync 2>&1`;
    if ($out =~ m/up-to-date/) {
        print "No update required";
        exit 0;
    }
    system "/home/local/common/fulcrum/bin/updatefmdb";
    system "qrsh -l a=solaris64 -now n updaterelease";
}
else {
    system "cp -p /home/local/fedora/fulcrum/config/release $fulcrum/config";
    system "/home/local/common/fulcrum/bin/updatefmdb";
}
