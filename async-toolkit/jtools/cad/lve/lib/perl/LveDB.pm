
package LveDB;

use POSIX;
use LveStatus;
use DBI;
use File::stat;
use Fcntl qw(:flock);

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT = qw( 
        &lve_create_db &lve_db_connect &lve_db_disconnect &lve_db_do
        &lve_db_filename &lve_db_exists &lve_db_select &lve_db_raw
        &lve_db_hashref &lve_db_fix &lve_db_getprefix_from_root
        &lve_db_raw2db
    );
}

use strict;
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
    "xa" => 7,
);

my $dbfilename="lvedb.db";
my @cell_table_entry=(
    "fqcn primary key",
    "datetime default NULL", 
    "alint default 'NOT_TESTED'", 
    "antenna default 'NOT_TESTED'", 
    "aspice default 'NOT_TESTED'", 
    "asta default 'NOT_TESTED'", 
    "drc default 'NOT_TESTED'", 
    "extract default 'NOT_TESTED'", 
    "frc default 'NOT_TESTED'", 
    "hdrc default 'NOT_TESTED'", 
    "hlvs default 'NOT_TESTED'", 
    "hsim default 'NOT_TESTED'", 
    "xa default 'NOT_TESTED'", 
    "hspice default 'NOT_TESTED'", 
    "jlvs default 'NOT_TESTED'", 
    "lib default 'NOT_TESTED'", 
    "lvs default 'NOT_TESTED'", 
    "slint default 'NOT_TESTED'", 
    "totem default 'NOT_TESTED'", 
    "'rte' default 'NOT_TESTED'", 
    "args default ''"
);

sub lve_db_filename {
    my ($root)=@_;
    "$root/$dbfilename";
}

sub lve_db_exists {
    my ($root)=@_;
    my $prefix=lve_db_getprefix_from_root($root,"");
    (-f "$root/$dbfilename" and -w "$root/$dbfilename") or $prefix ne "";
}

sub lve_db_getprefix_from_root {
    my ($root, $prefix)=@_;
    my $dbt;
    $dbt=DBI->connect("dbi:mysql:lve:host=tsyvweb03.ts.intel.com", "lve", "lve");
    my $ref=$dbt->selectrow_hashref("SELECT prefix FROM root where root='$root'");
    $prefix=$ref->{prefix} if (defined($ref->{prefix}));
    $dbt->disconnect;
    $prefix;
}

sub lve_create_db {
    my ($root, $pdk_root, $prefix)=@_;
    local($\)="\n";
    my $error=0;
    $prefix="" if ! defined $prefix;
    $prefix=lve_db_getprefix_from_root($root, $prefix);
    my $dbh;
    if ($prefix eq "" and -d "$pdk_root") {
        $dbh->{dbh}=DBI->connect("dbi:SQLite:dbname=$root/$dbfilename", { AutoCommit => 1, sqlite_use_immediate_transaction => 1 });
    }
    elsif ($prefix ne "") {
        $dbh->{dbh}=DBI->connect("dbi:mysql:lve:host=tsyvweb03", "lve", "lve");
    }
    else {
        $dbh=0;
    }
    my $fh;
    if ($dbh and $dbh->{dbh}) {
        if ($prefix eq "") {
            if (open ($fh, "<$pdk_root/share/Fulcrum/lve/lvedb.dump")) {
                while (<$fh>) {
                    chomp;
                    $dbh->{dbh}->do($_);
                }
                close $fh;
            }
            else {
                print STDERR "Error: Cannot open $pdk_root/share/Fulcrum/lve/lvedb.dump.";
            }
        }
        else {
            $dbh->{prefix}=$prefix;
            my $ref=$dbh->{dbh}->selectrow_hashref("SELECT root FROM root where prefix='$prefix'");
            my $error=0;
            if (defined ($ref->{root}) and $ref->{root} ne $root) {
                print STDERR "This database is already in use here $ref->{root}.";
                $error=1;
            }
            if (! defined ($ref->{root})) {
                if (open ($fh, "<$pdk_root/share/Fulcrum/lve/lvedb.dump")) {
                    while (<$fh>) {
                        chomp;
                        my @f=split;
                        if ($f[0] eq "DROP") {
                            $f[4] = "${prefix}_$f[4]";
                        }
                        elsif ($f[0] eq "CREATE") {
                            for (my $n = 1; $n <= $#f; $n++) {
                                if ($f[$n] =~ /^\(/) {
                                    $f[$n-1] = "${prefix}_$f[$n-1]";
                                    last;
                                }
                            }
                        }
                        $dbh->{dbh}->do(join(" ", @f));
                    }
                    close $fh;
                    $dbh->{dbh}->do("insert into root values ('$root','$prefix')");
                }
                else {
                    print STDERR "Error: Cannot open $pdk_root/share/Fulcrum/lve/lvedb.dump.";
                    $error=1;
                }
            }
        }
        $dbh->{dbh}->disconnect;
    }
    else {
        print STDERR "Error: failed to create database.";
    }
    $prefix;
}

sub lve_db_connect {
    my ($root,$prefix,$dbfile)=@_;
    local($\)="\n";
    $dbfilename=$dbfile if(defined $dbfile);
    my $dbh;
    $prefix="" if ! defined $prefix;
    $prefix=lve_db_getprefix_from_root($root,$prefix);
    if (-s "$root/$dbfilename" and $prefix eq "") {
#        print STDERR "Using sqlite3.";
        $dbh->{dbh}=DBI->connect("dbi:SQLite:dbname=$root/$dbfilename", { AutoCommit => 1, sqlite_use_immediate_transaction => 1 });
        $dbh->{prefix}=$prefix;
        $dbh->{root}=$root;
        return $dbh;
    }
    elsif ($prefix ne "") {
        $dbh->{dbh}=DBI->connect("dbi:mysql:lve:host=tsyvweb03.ts.intel.com", "lve", "lve");
        $dbh->{prefix}=$prefix;
        $dbh->{root}=$root;
        my $ref=$dbh->{dbh}->selectrow_hashref("SELECT root FROM root where prefix='$prefix'");
        if ($ref->{root} ne $root) {
            print STDERR "Wrong root for this mysql set of tables, this prefix is for $root".
                " not $ref->{root}.";
            return 0;
        }
        return $dbh;
    }
    return 0;
}

sub lve_db_do {
    my ($dbh, $cmd)=@_;
    local($\)="\n";
    my $errstr="";
    my $rv=$dbh->{dbh}->do($cmd);
    $errstr=$dbh->{dbh}->errstr if defined $dbh->{dbh}->errstr;
    $rv = 0 if ! defined $rv;
    my $cnt=0;
    while ($rv ne "1" and $rv ne "0E0" and $errstr =~ /locked/) {
        sleep rand(5)+1;
        $rv=$dbh->{dbh}->do($cmd);
        $errstr=$dbh->{dbh}->errstr if defined $dbh->{dbh}->errstr;
        $rv = 0 if ! defined $rv;
        $cnt++;
        last if $cnt > 10;
    }
    printf STDERR "Error: %s for $cmd $errstr ",$rv if $rv eq "0";
    $rv;
}

sub lve_db_disconnect {
    my ($dbh)=@_;
    $dbh->{dbh}->disconnect;
}

sub lve_db_select {
    my ($dbh, $columns, $table, $where)=@_;
    $table = $dbh->{prefix}."_".$table if $dbh->{prefix} ne "";
    $dbh->{dbh}->selectall_arrayref("SELECT $columns FROM $table where $where");
}

sub lve_db_hash {
    my ($dbh, $columns, $table, $where)=@_;
    $table = $dbh->{prefix}."_".$table if $dbh->{prefix} ne "";
    if ($where ne "") {
        $dbh->{dbh}->selectrow_hashref("SELECT $columns FROM $table where $where");
    }
    else {
        $dbh->{dbh}->selectrow_hashref("SELECT $columns FROM $table");
    }
}

sub lve_db_raw {
    my ($dbh, $fqcn, $task, $view, $mode, $status, $path, $datetime, $user) = @_;
    return if ! $dbh->{dbh};
    local($\)="\n";
    print STDERR "Error: invalid call to lve_db_raw."
        if ! defined $path;
    my @dt=localtime(time);
    $datetime=sprintf("%04d-%02d-%02d %02d:%02d:%02d",
        $dt[5]+1900, $dt[4]+1, $dt[3], $dt[2], $dt[1], $dt[0]) if ! defined $datetime;
    my $utask=$task;
    $utask =~ tr/a-z/A-Z/;
    if ( ! defined ($user)) {
        $user=`whoami`;
        chomp $user;
    }
    my $ref=lve_db_hash($dbh, "fqcn,$task", "cells", "fqcn='$fqcn'");
    my %stat=();
    my $cells="cells";
    $cells = $dbh->{prefix}."_cells" if $dbh->{prefix} ne "";
    if (! defined ($ref->{fqcn})) {
        lve_db_do($dbh, "insert into $cells (fqcn) values ('$fqcn')");
    }
    my $raw="raw";
    $raw = $dbh->{prefix}."_raw" if $dbh->{prefix} ne "";
    lve_db_do($dbh, "delete from $raw where fqcn='$fqcn' and task='$task' and path='$path'");
    lve_db_do($dbh, "insert into $raw ( fqcn, task, view, mode, datetime, result, path, user ) values ( '$fqcn', '$task', '$view', '$mode', '$datetime', '$status', '$path', '$user')");
    my $sth=$dbh->{dbh}->selectall_arrayref("SELECT result FROM $raw where fqcn='$fqcn' and task='$task'");
    foreach my $row (@$sth) {
        my @x=@$row;
        $stat{$x[0]}++;
    }
    my $globalstatus=LveUtil::summarizeStatus(\%stat);
    lve_db_do($dbh, "update $cells set $utask='$globalstatus',datetime='$datetime' where fqcn='$fqcn'");
}

sub lve_db_fix {
    my ($dbh)=@_;
    # this prevents command from sending to STDERR
    open (OLDERR, ">&STDERR");
    printf OLDERR "";
    open (STDERR, ">>/dev/null");
    my $rte="rte";
    $rte = $dbh->{prefix}."_rte" if $dbh->{prefix} ne "";
    my $sth=$dbh->{dbh}->selectall_arrayref("SELECT $rte FROM cells");
    # this restores normal STDERR behavior
    close STDERR;
    open (STDERR, ">&OLDERR");
    if ( $sth ) {
        return 0;
    }
    my $cells="cells";
    $cells = $dbh->{prefix}."_cells" if $dbh->{prefix} ne "";
    $sth=$dbh->{dbh}->selectall_arrayref("SELECT * FROM $cells");
    my $needfix=1;
    my %cells=();
    foreach my $row (@$sth) {
        $needfix=0 if ($#$row == $#cell_table_entry);
        if ($needfix) {
            my @x=@$row;
            $cells{$x[0]}=[@x];
        }
        else {
            last;
        }
    }
    if ($needfix and $dbh->{prefix} eq "") {
        # only sql files (for now) need to be fixed.
        lve_db_do($dbh, "drop table cells");
        my $cmd="CREATE TABLE cells ( ".join(", ", @cell_table_entry).");";
        lve_db_do($dbh, $cmd);
        foreach my $fqcn (sort keys %cells) {
            my @x=@{$cells{$fqcn}};
            my $cmd="insert into cells VALUES(";
            for (my $n = 0; $n < scalar(@cell_table_entry) - 2; $n++) {
                if (! defined ($x[$n])) {
                    $cmd .= "NULL,";
                }
                else {
                    $cmd .= "'$x[$n]',";
                }
            }
            $cmd .= "'NOT_TESTED',";
            $cmd .= "'$x[$#cell_table_entry]')";
            lve_db_do($dbh, $cmd);
        }
    }
    $needfix;
}

sub lve_db_raw2db{
    my ($dbh,$root,$rawfile,$verbose,$skipdb)=@_;
    my $opened=0;
    my $status_sum="NOT_TESTED";
    chomp($rawfile);
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
        my @path=split(/\//, $rawname);
        my $task = $path[$#path];
        $task =~ s/\.raw$//;
        print STDERR "Skipping $rawfile" if ! defined $viewdepth{$task};
        return undef if ! defined $viewdepth{$task};
        my $viewdepth=$viewdepth{$task};
        my %allstatus=();
        my $cnt=0;
        my ($status,$tsk,$cell,$path);
        my $onlyNewBumpFail=undef;
        my %fail_type;
        $fail_type{OTHER}=0;
        $fail_type{FAIL_MNOISE}=0;
        $fail_type{FAIL_NEWBUMP}=0;
        while (<P>) {
            chomp;
            ($status, $tsk, $cell, $path)=split(/ /, $_);
            next if ! defined $path;
            next if $tsk ne $task;
            if($tsk eq "alint" and $status =~ "FAIL"){
              if($_ =~ /fail_type=/){
                $fail_type{FAIL_NEWBUMP}=1;
              }elsif($_ =~ /multi_noise_resp/){
                $fail_type{FAIL_MNOISE}=1;
              }else{
                $fail_type{OTHER}=1;
              }
            }
            $allstatus{$status}++;
            $cnt++;
        }
        if ($cnt > 0) {
            $status_sum=LveUtil::summarizeStatus(\%allstatus,\%fail_type);
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
                lve_db_raw($dbh, $cell,$task,$view,$mode,$status_sum,$path,$datetime) if(not defined $skipdb or $skipdb==0);
            }
            else {
                $status_sum=undef;
                print STDERR "$rawfile does not have correct path\n  $rawfile\n  $path/$task.raw.";
            }
        }
    }
    else {
        $status_sum=undef;
        print STDERR "Cannot open $rawfile: $!";
    }
    return $status_sum;
}
1;
