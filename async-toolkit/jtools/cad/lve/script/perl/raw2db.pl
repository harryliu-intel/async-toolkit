#!/usr/intel/bin/perl -wl

use POSIX;
use Getopt::Long;
use File::stat;
use DBI;
use strict;

our $root;

BEGIN {
    my $lve_root = $0;
    $lve_root =~ s:/[^/]*$::;
    $lve_root =~ s:/[^/]*$::;
    @INC = ("$lve_root/lib/perl", @INC);
}

use LveUtil;
use LveDB;

my $filelist=undef;
my $verbose=0;
my $prefix="";

GetOptions (
    "root=s" => \$root,
    "filelist=s" => \$filelist,
    "verbose" => \$verbose,
    "db-prefix=s" => \$prefix,
);

my $user=`whoami`;
chomp $user;

my $dbh;
$prefix=lve_db_getprefix_from_root($root,$prefix);
if (( -f "$root/lvedb.db" and -w "$root/lvedb.db") or $prefix ne "") {
    $dbh=lve_db_connect($root, $prefix);
    if ( ! $dbh  or ! $dbh->{dbh} ) {
        print STDERR "Connect failed";
        exit 1;
    }
    print STDERR "Connected" if $dbh and $verbose;
}
else {
    print STDERR "DB does not exist or is not writable" if $verbose;
    exit 0;
}

if ( defined $filelist and -f "$filelist") {
    open (P, "<$filelist");
    while (<P>) {
        chomp;
        push @ARGV, $_;
    }
    close P;
}
foreach my $rawfile (@ARGV) {
    lve_db_raw2db($dbh,$root,$rawfile,$verbose);
}
lve_db_disconnect($dbh);
exit 0;
