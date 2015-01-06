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
my $rebuild=0;
my $prefix="";
my $help=0;
my $create_db=0;
my $pdk_root = "$ENV{FULCRUM_PDK_ROOT}";

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
    "help" => \$help,
    "rebuild" => \$rebuild,
    "db-prefix=s" => \$prefix,
);
die usage() if (! -d $root);
die usage() if ($help);
my $user=`whoami`;
chomp $user;

my $dbh;
my $db_file="lvedb.db";
$prefix=lve_db_getprefix_from_root($root,$prefix);
if (( -f "$root/lvedb.db" and -w "$root/lvedb.db") or $prefix ne "") {
my @CHARS = (qw/ A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
	         a b c d e f g h i j k l m n o p q r s t u v w x y z
	         0 1 2 3 4 5 6 7 8 9 _
	     /);
    $db_file="lvedb.XXXX.db";
    $db_file =~ s/X/$CHARS[ int( rand( $#CHARS ) ) ]/ge;
    system("cp", "-f","$root/lvedb.db","$root/$db_file");
    $dbh=lve_db_connect($root, $prefix,$db_file);
}
else {
  if($rebuild){
    $create_db=1;
  }else{
  print STDOUT "Error: No database found. You can use --rebuild to build lve database";
  exit 0;
  }
}
######################
# create database
######################
my $raw="raw";
my $cells="cells";
if($create_db){
    $prefix=lve_create_db($root, $pdk_root, $prefix);
    my $lvedbfile="$root/lvedb.db";
    if ( -w "$lvedbfile" or $prefix ne "") {
        if ( -w "$lvedbfile") {
            my $stat=stat("$lvedbfile");
            my $mode=$stat->mode;
            $mode |= 020;
            chmod $mode, "$lvedbfile";
        }
    }
}else{
    $prefix=$dbh->{prefix};
}
$raw = "${prefix}_raw" if $prefix ne "";
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
    "totem" => 7,
    "hsim" => 8,
    "rte" => 6,
    "xa" => 7,
);

#########################
# rebuild database
#########################
if($rebuild){
  $dbh=lve_db_connect($root, $prefix,$db_file);
  if ( ! $dbh  or ! $dbh->{dbh} ) {
        print STDERR "Error: Connect failed 0";
        exit 1;
  }

  lve_db_fix($dbh); # add raw column to database
  #reset database
  lve_db_do($dbh, "delete from $cells");
  lve_db_do($dbh, "delete from $raw");
  #find all raw files
  print "Information: Searching *.raw files to build database...";
  my @raw_files=`find $root \\! -empty -follow -type f -name \\*.raw`;
  if(scalar(@raw_files)==0){
    print "Information: No valid raw files to create lve database";
  }
  foreach my $f (@raw_files){
      chomp($f);
      print "Information: Update database for $f";
      lve_db_raw2db($dbh,$root,$f,$verbose);
  }
  lve_db_disconnect($dbh);
  system("cp","-f","$root/$db_file","$root/lvedb.tmp.db");
  system("mv","-f","$root/$db_file","$root/lvedb.db") if($db_file ne "lvedb.db");
  exit;
}
#########################
# fix database
#########################
my @cell=();
if(defined $cell){
  if(-e $cell){
    #cell is a file  
    open(F,"<$cell") or die "CANNOT open file $cell";
    @cell=<F>;
    chomp(@cell);
  }else{
    @cell=split(":",$cell);
  }
}
print join("\n",@cell);
my %status=();
my $sth;
my $fqcn_str=undef;
#if (defined ($cell)) {
if (scalar(@cell)>0) {
  my @fqcn_str= map {"fqcn=\'$_\'"} @cell;
  $fqcn_str=join(" or ", @fqcn_str);
}
if (defined $fqcn_str) {
    #$sth=$dbh->{dbh}->prepare("select * from $cells where fqcn='$cell'");
    $sth=$dbh->{dbh}->prepare("select * from $cells where $fqcn_str");
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

#if (defined ($cell)) {
if (defined $fqcn_str) {
    #$sth=$dbh->{dbh}->prepare("select * from $raw where fqcn='$cell'");
    $sth=$dbh->{dbh}->prepare("select * from $raw where $fqcn_str");
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
    }else{
        my $thisstatus=lve_db_raw2db($dbh,$root,$file,$verbose,1); #only get status from raw, dont update db now.
        $raw{$fqcn}->{$task}->{$thisstatus}=1;
        $raw{$fqcn}->{'path'}->{$path}=1;
        if (defined $thisstatus and $thisstatus ne $result) {
            printf "Information: $fqcn $path/$task.raw $rawline->{result} should be $thisstatus";
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

#check if there is any newly added .raw file.
if(defined $cell){
  foreach my $c (@cell){
    #add cell if not exists in database
    my $cell_path=$c; $cell_path=~s/\./\//g;
    my @raw_files=`find \'$root/$cell_path\' \\! -empty -follow -type f -name \\*.raw`;
    foreach my $file (@raw_files){
       chomp($file);
       my @path=split(/\//, $file);
       my $task = $path[$#path];
       $task =~ s/\.(raw|raw\.gz)$//;
       my $file_path = $file; $file_path=~s/^$root\///;
       my $path = $file_path; $path=~s/\/$task\.(raw|raw\.gz)$//;
       print STDERR "Skipping $file" if ! defined $viewdepth{$task};
       next if ! defined $viewdepth{$task};
       if(not defined $raw{$c}->{'path'}->{$path}){
           my $skipdb=1;  $skipdb=0 if $fix;
          my $status=lve_db_raw2db($dbh,$root,$file,$verbose,$skipdb);
          $raw{$c}->{$task}->{$status}=1 if defined $status;
          print "Information: $c should insert new result $file" if (defined $status);
          if ($fix and defined $status) {
              print " fixed";
          }
       }
    }
  }  
}else{
    my @raw_files=`find \'$root\' \\! -empty -follow -type f -name \\*.raw`;
    foreach my $file (@raw_files){
       chomp($file);
       my $c;
       my @path=split(/\//, $file);
       my $task = $path[$#path];
       $task =~ s/\.(raw|raw\.gz)$//;
       my $file_path = $file; $file_path=~s/^$root\///;
       my $path = $file_path; $path=~s/\/$task\.(raw|raw\.gz)$//;
       print STDERR "Skipping $file" if ! defined $viewdepth{$task};
       next if ! defined $viewdepth{$task};
       my $file_t=$file;
       my $opened=0;
       if (($file_t =~ /\.gz$/) and open (P, "gunzip -c '$file_t' |")) {
          $opened=1;
       }
       elsif (open (P, "<$file_t")) {
          $opened=1;
       }
       next if (!$opened);
       while(<P>){
            chomp;
            (my $status, my $tsk, $c, my $p)=split(/\s+/, $_);
            last;
       }
       if(not defined $raw{$c}->{'path'}->{$path}){
          my $skipdb=1;  $skipdb=0 if $fix;
          my $status=lve_db_raw2db($dbh,$root,$file,$verbose,$skipdb);
          $raw{$c}->{$task}->{$status}=1 if defined $status;
          print "Information: $c should insert new result $file" if (defined $status);
          if ($fix and defined $status) {
              print " fixed";
          }
       }
    }

}
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
            printf "Information: $fqcn $task $status{$fqcn}->{$task} should be $status";
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
            printf "Information: $fqcn should be removed.";
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

  system("cp","-f","$root/$db_file","$root/lvedb.tmp.db");
  system("mv","-f","$root/$db_file","$root/lvedb.db") if($db_file ne "lvedb.db");

sub usage {
    local $"=",";
    my $usages = <<ET;

Check/Fix/Rebuild lve database. 

USAGE: $0 -root=<lve root> [options] 

  Options:
    --help (display this usage manual)
    --cell=<cellname> (lve cell fqcn. User can specify a particular cell to check.)
    --fix  (fix database if a cell has any update result)
    --rebuild (rebuild whole database. The difference between fix and rebuild is fix will only fix the cell exists in the database.
               rebuild will search all *.raw files to rebuild the database)
    --db-prefix=<> (db prefix. Only for tsyvweb03.ts.intel.com lve database)
    --verbose
ET
   return $usages;
}

