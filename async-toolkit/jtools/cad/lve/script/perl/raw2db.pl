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

my %viewdepth = (
    "alint" => 5,
    "asta" => 5,
    "lib" => 5,
    "antenna" => 0,
    "drc" => 0,
    "lvs" => 0,
    "frc" => 0,
    "hdrc" => 0,
    "hlvs" => 0,
    "jlvs" => -1,
    "aspice" => 8,
    "hspice" => 7,
    "hsim" => 7,
    "totem" => 7,
    "extract" => 1,
    "rte" => 6,
);

if ( defined $filelist and -f "$filelist") {
    open (P, "<$filelist");
    while (<P>) {
        chomp;
        push @ARGV, $_;
    }
    close P;
}
my $filecnt=0;
foreach my $rawfile (@ARGV) {
    my $opened=0;
    if (($rawfile =~ /\.gz$/) and open (P, "gunzip -c '$rawfile' |")) {
        $opened=1;
    }
    elsif (open (P, "<$rawfile")) {
        $opened=1;
    }
    if ($opened) {
        print STDERR "Processing $rawfile" if $verbose;
        my $rawname = $rawfile;
        $rawname =~ s/\.gz$//;
        $filecnt++;
        my @path=split(/\//, $rawname);
        my $task = $path[$#path];
        $task =~ s/\.raw$//;
        print STDERR "Skipping $rawfile" if ! defined $viewdepth{$task};
        next if ! defined $viewdepth{$task};
        my $viewdepth=$viewdepth{$task};
        my %allstatus=();
        my %alllstatus=();
        my $cnt=0;
        my ($status,$tsk,$cell,$path);
        while (<P>) {
            chomp;
            ($status, $tsk, $cell, $path)=split(/ /, $_);
            next if ! defined $path;
            next if $tsk ne $task;
            $allstatus{$status}++;
            $cnt++;
        }
        if ($cnt > 0) {
            my $status=summarizeStatus(\%allstatus);
            my @path=split(/\//, $path);
            my $view;
            my $mode;
            if ($viewdepth >= 0) {
                $view=$path[$#path-$viewdepth];
            }
            else {
                $view="NA";
            }
            if ($viewdepth >= 1) {
                $mode=$path[$#path-$viewdepth+1];
            }
            else {
                $mode="NA";
            }
            $rawfile =~ s:^$root/::;
            $rawname =~ s:^$root/::;
            if ( $rawname eq "$path/$task.raw" ) {
                my $stat=stat("$root/$rawfile");
                my @dt=localtime($stat->mtime);
                my $datetime=sprintf("%04d-%02d-%02d %02d:%02d:%02d",
                    $dt[5]+1900, $dt[4]+1, $dt[3], $dt[2], $dt[1], $dt[0]);
                lve_db_raw($dbh, $cell,$task,$view,$mode,$status,$path,$datetime);
            }
            else {
                print STDERR "$rawfile does not have correct path\n  $rawfile\n  $path/$task.raw.";
            }
        }
    }
    else {
        print STDERR "Cannot open $rawfile: $!";
    }
}
exit 0;
