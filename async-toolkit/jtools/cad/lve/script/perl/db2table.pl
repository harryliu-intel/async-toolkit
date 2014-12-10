#!/usr/intel/bin/perl -wl

use POSIX;
use Getopt::Long qw(:config pass_through);
use File::stat;
use DBI;
use strict;

our $lve_root;
our $root;
$root=`pwd`;
chomp $root;
my $verbose=0;
my $cell=undef;
my $prefix="";
my $brief=0;
BEGIN {
    $lve_root = $0;
    $lve_root =~ s:/[^/]*$::;
    $lve_root =~ s:/[^/]*$::;
    @INC = ("$lve_root/lib/perl", @INC);
}

use LveUtil;
use LveDB;

GetOptions (
    "root=s" => \$root,
    "verbose" => \$verbose,
    "cell=s" => \$cell,
    "brief" => \$brief,
);

my $user=`whoami`;
chomp $user;

my $dbh;
$prefix=lve_db_getprefix_from_root($root,$prefix);
if (( -f "$root/lvedb.db" and -w "$root/lvedb.db") or $prefix ne "") {
    $dbh=lve_db_connect($root, $prefix);
}
else {
    print STDERR "No database" if $verbose;
    exit 0;
}

my $raw="raw";
$prefix=$dbh->{prefix};
$raw = "${prefix}_raw" if $prefix ne "";
my $cells="cells";
$cells = "${prefix}_cells" if $prefix ne "";

my %status=();
my $sth;
if (defined ($cell)) {
    $sth=$dbh->{dbh}->prepare("select * from $cells where fqcn='$cell'");
}
else {
    $sth=$dbh->{dbh}->prepare("select * from $cells order by fqcn");
}
$sth->execute;
while (my $fst=$sth->fetchrow_hashref) {
    if ($brief) {
        printf "$fst->{fqcn}";
    } else {
        printf "CELL:=$fst->{fqcn}";
    }
    foreach my $key (sort keys %$fst) {
        if ($key ne "fqcn" and $key ne "args" and $key ne "datetime") {
            $status{$fst->{fqcn}}->{$key}=$fst->{$key};
            my $ukey=$key;
            $ukey =~ tr/a-z/A-Z/;
            if ($brief) {
                printf " $ukey=".substr($fst->{$key},0,1) if $fst->{$key} ne "NOT_TESTED";
            } else {
                printf " $ukey:=$fst->{$key}";
            }
        }
    }
    print "";
}
$sth->finish;
