#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use strict;
use Getopt::Long;

select STDERR;
$|=1;
select STDOUT;
$|=1;

sub usage {
    my $prog=$0;
    $prog =~ s:.*/::;
    print STDERR "$prog [--toolhome \<dir\>] [--verbose]";
    exit 1;
}

# specific paths
my $binhome=$0;
# find cononical path to this executable
if ( $binhome =~ m:^/:) {
    $binhome =~ s:/[^/]+$::;
}
else {
    my $pwd = `pwd`;
    chomp $pwd;
    if ( $binhome =~ m:/: ) {
        $binhome =~ s:/[^/]+$::;
        chdir $binhome;
        $binhome=`pwd`;
        chomp $binhome;
        chdir $pwd;
    }
    else {
        $binhome=$pwd;
    }
}

$binhome = "/p/rrc/tools/bin"
    if ! -x "$binhome/updatefmdb.pl";

my $toolhome="";
my $verbose=0;
my $targetarch="Linux-x86_64";
my $branch="";
my $synconly=0;
my %options = (
    "toolhome=s" => \$toolhome,
    "verbose" => \$verbose,
    "target-arch=s" => \$targetarch,
    "branch=s" => \$branch,
    "synconly" => \$synconly,
);

GetOptions ( %options ) or usage;

my $toolhomearg="";
if ( "$toolhome" ne "" ) {
    if ( -d "$toolhome" ) {
        $toolhomearg="--toolhome=$toolhome";
    }
    else {
        print STDERR "$toolhome is not a directory";
        usage;
    }
}
$toolhome =~ s:/$::;
if ($toolhome =~ m:/p/rrc/tools/: or $toolhome eq "") {
    die "You must be tsbuild user to update global db's"
        if ( ! -w "/p/rrc/tools/fulcrum/config" );
    foreach my $arch ( "intel-x86_64") {
       system "P4PORT=10.232.36.77:1666 P4USER=system P4CONFIG= P4CLIENT=system-$arch-fulcrum p4 sync 2>/dev/null";
    }
}

die "You do not have write permission to $toolhome"
    if ( $toolhome ne "" and ! -w "$toolhome" );
my $aname=`uname -sm`;
chomp $aname;
$aname =~ s/ /-/;
my %archname = (
#    "lx24-x86" => "Linux-i686",
    "lx24-amd64" => "Linux-x86_64",
);

my @targetarch=split(/,/,$targetarch);
my %targetarch=();
foreach my $a (keys %archname) {
    $targetarch{$archname{$a}}=0;
}
foreach my $a (@targetarch) {
    if (defined $targetarch{$a}) {
        $targetarch{$a}=1;
    }
    else {
        print STDERR "Unknown Arch $a";
    }
}
my $verbosearg="&>/dev/null";
$verbosearg="--verbose" if $verbose;
my %pid;
my $pid;
foreach my $arch (keys %archname) {
    if ($targetarch{$archname{$arch}}) {
        print STDERR "Starting $archname{$arch}" if $verbose;
        my $cmd="$binhome/updatefmdb.pl $toolhomearg $verbosearg";
        $cmd .= " --branch $branch" if $branch ne "";
        if ($pid = fork ) {
            $pid{$pid}=$archname{$arch};
        }
        else {
            exec "$cmd";
        }
    }
}
print "Waiting..." if $verbose;
while (($pid = wait) != -1) {
    print "Finished $pid{$pid}" if $verbose;
}
