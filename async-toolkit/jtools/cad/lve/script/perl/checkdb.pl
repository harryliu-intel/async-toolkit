#!/usr/intel/bin/perl -wl

use POSIX;
use Getopt::Long;
use File::stat;
use DBI;
use strict;

our $lve_root;
our $root;
my $verbose=0;
my $cell=undef;
my $fix=0;
my $prefix="";
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
    "fix" => \$fix,
    "db-prefix=s" => \$prefix,
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
my %viewdepth = (
    "alint" => 5,
    "antenna" => 0,
    "aspice" => 8,
    "asta" => 5,
    "drc" => 0,
    "extract" => 1,
    "frc" => 0,
    "hdrc" => 0,
    "hlvs" => 0,
    "hsim" => 7,
    "hspice" => 7,
    "jlvs" => -1,
    "lib" => 5,
    "lvs" => 0,
    "slint" => 5,
    "totem" => 7,
    "hspice" => 8,
    "hsim" => 8,
    "rte" => 6,
);

my %status=();
my $sth;
if (defined ($cell)) {
    $sth=$dbh->{dbh}->prepare("select * from $cells where fqcn='$cell'");
}
else {
    $sth=$dbh->{dbh}->prepare("select * from $cells");
}
$sth->execute;
while (my $fst=$sth->fetchrow_hashref) {
    foreach my $key (sort keys %$fst) {
        if ($key ne "fqcn") {
            $status{$fst->{fqcn}}->{$key}=$fst->{$key};
        }
    }
}
$sth->finish;

print STDERR "cells read" if $verbose;
if (defined ($cell)) {
    $sth=$dbh->{dbh}->prepare("select * from $raw where fqcn='$cell'");
}
else {
    $sth=$dbh->{dbh}->prepare("select * from $raw order by fqcn");
}
$sth->execute;
my %raw=();
my $cnt=0;
my @remove=();
while (my $rawline=$sth->fetchrow_hashref) {
    my $fqcn=$rawline->{fqcn};
    my $task=$rawline->{task};
    my $path=$rawline->{path};
    my $result=$rawline->{result};
    my $file = "$root/$path/$task.raw";
    if ( ! -e "$file" and ! -e "$file.gz" ) {
        print "Error: $file has been removed.";
        push @remove, "delete from $raw where fqcn='$fqcn' and path='$path' and task='$task'";
        print "   fixed" if $fix;
    }
    else {
        if ( -e "$file" ) {
            open (P, "<$file");
        }
        else {
            open (P, "gunzip -c '$file.gz' |");
        }
        $cnt++;
        print $cnt if $cnt % 1000 == 0;
        my %stat=();
        while (<P>) {
            chomp;
            my @f=split;
            $raw{$fqcn}->{$task}->{$f[0]}++;
            $stat{$f[0]}++;
        }
        close P;
        my $thisstatus=summarizeStatus(\%stat);
        if ($thisstatus ne $result) {
            printf "$path/$task.raw $rawline->{result} should be $thisstatus";
            if ($fix) {
                lve_db_do($dbh, "update $raw set result='$thisstatus' where fqcn='$fqcn' and task='$task' and path='$path'");
                print " fixed";
            }
            else {
                print "";
            }
        }
    }
}
$sth->finish;
print STDERR "raw read" if $verbose;
if ($fix) {
    foreach my $line (@remove) {
        lve_db_do($dbh, $line);
    }
}

foreach my $fqcn (sort keys %status) {
    my $remove=1;
    foreach my $task (sort keys %viewdepth) {
        my $status;
        if (defined $raw{$fqcn}->{$task}) {
            $status=summarizeStatus(\%{$raw{$fqcn}->{$task}});
        }
        else {
            $status="NOT_TESTED";
        }
        $remove=0 if $status ne "NOT_TESTED";
        if ($verbose or $status{$fqcn}->{$task} ne $status) {
            printf "$fqcn $task $status{$fqcn}->{$task} should be $status";
            if ($fix and ($status{$fqcn}->{$task} ne $status)) {
                lve_db_do($dbh, "update $cells set $task='$status' where fqcn='$fqcn'");
                print " fixed";
            }
            else {
                print "";
            }
        }
    }
    if ($remove) {
        my $celldir=$fqcn;
        $celldir =~ s/\./\//g;
        if (! -d "$root/$celldir") {
            printf "$fqcn should be removed.";
            if ($fix) {
                lve_db_do($dbh, "delete from $cells where fqcn='$fqcn'");
                print " fixed";
            }
            else {
                print "";
            }
        }
    }
}
