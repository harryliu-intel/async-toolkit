#!/usr/intel/pkgs/perl/5.14.1-threads/bin/perl -l

BEGIN {
    push @INC, ("/usr/intel/pkgs/perl/5.14.1-threads/lib64/module/r1/x86_64-linux-thread-multi","/usr/intel/pkgs/perl/5.14.1-threads/lib64/module/r1");
}

use POSIX;
use DBI;
use File::stat;
use strict;

my $debug=0;
my $cmd="select fqcn from cells";
my @entries=();
foreach my $dbfile (`find /nfs/site/disks/*/a0/work_root/*/ -maxdepth 6 -name lvedb.db -size +0 2>/dev/null`) {
    chomp $dbfile;
    next if ! -r $dbfile;
    next if $dbfile =~ m:/share/Fulcrum/lve/lvedb.db:;
    next if $dbfile =~ /\.bak/;
    next if $dbfile =~ /\.bkk/;
    next if $dbfile =~ /\.bad/;
    next if $dbfile =~ /rrcwd\/abc/;
    next if $dbfile =~ /elaineou/;
    next if $dbfile =~ /ahasan1/;
    my $db=DBI->connect("dbi:SQLite:dbname=$dbfile", { AutoCommit => 1, sqlite_use_immediate_transaction => 1 });
    if ( !$db) {
        print STDERR "Cannot open $dbfile";
        next;
    }
    next if ! defined ($db);
    my $sth=$db->prepare($cmd);
    if (! defined ($sth) or !$sth) {
        print STDERR "Prepare failed in $dbfile";
        $db->disconnnect();
        next;
    }
    my $rv=$sth->execute;
    if ( ! defined ($rv) or ! $rv ) {
        print STDERR "Query failed in $dbfile";
        $db->disconnect();
        next;
    }
    my $cnt=0;
    if (! $sth->fetchrow_hashref) {
        print STDERR "No cells in $dbfile" if $debug;
        $sth->finish;
        $db->disconnect();
        next;
    }
    $sth->finish;
    $db->disconnect;
    my $entry=$dbfile;
    $entry =~ s:/nfs/site/disks/::;
    $entry =~ s:/lvedb.db::;
    push @entries, $entry;
}
open (OUT, ">/p/rrc/tools/apache2/htdocs/lvedirsSC.tmp");
select OUT;
print join ("\n", sort @entries);
close OUT;
rename "/p/rrc/tools/apache2/htdocs/lvedirsSC.tmp", "/p/rrc/tools/apache2/htdocs/lvedirsSC.txt";
