#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$
use strict;

use Getopt::Long qw(:config require_order);
use DB_File;
$ENV{PATH}="/usr/intel/bin:$ENV{PATH}";
my $fulcrum="/p/rrc/tools/fulcrum";
my $branch="";
GetOptions (
    "toolhome=s" => \$fulcrum,
    "branch=s" => \$branch,
);
$fulcrum =~ s:/$::;
my $aname=`uname -sm`;
chomp $aname;
$aname =~ s/ /-/;
my $configdir = "$fulcrum/config/$aname";
$configdir = "$fulcrum/config/$branch/$aname" if $branch ne "";
my $tmpreldb = "$configdir/releasedbtmp";
my $tmplatdb = "$configdir/latestdbtmp";
my $reldb = "$configdir/releasedb";
my $release = "$configdir/releasedb";
my $latdb = "$configdir/latestdb";
my $latest = "$configdir/latestdb";
my $prefdb = "$configdir/prefdb";
my $alldb = "$configdir/alldb";
my $betadb = "$configdir/betadb";
my %db;
my $db = "reldb";
$db = $ARGV[0] if defined ($ARGV[0]);
eval "\$db = \$$db";
if ( ! -r "$db" and ! -r "$db.pag" and ! ($db =~ /db$/) ) {
    $db .= "db";
    eval "\$db = \$$db";
}
dbmopen (%db, "$db", undef) or die "Cannot locate $db";
foreach my $tool (sort keys %db) {
    print "$tool $db{$tool}";
}
dbmclose %db;
