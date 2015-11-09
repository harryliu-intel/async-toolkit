#!/usr/intel/bin/perl
use strict;
use warnings;
sub write_script {
    my ($command) = @_;
    my $script = "$$.sh";
    open my $fh, ">$script" || die;
    print $fh "#!/bin/sh\n";
    print $fh "touch $script.started\n";
    print $fh "$command\n";
    print $fh "rm $script\n";  # delete self
    print $fh "sleep 86400\n";
    close $fh;
    chmod 0755, $script;
    return $script;
}
my ($host, $cmd) = ($ARGV[0], $ARGV[1]);
my $temp = write_script($cmd);
my $started = "$temp.started";
unlink $started;
my @args = ('nbjob', 'prun', '--host', $host, "./$temp");
my $jobid;
open my $fh, '-|', @args;
while(<$fh>) {
    $jobid = $1 if (/JobID (\d+)/);
}
close $fh;
for my $i (1..10) {
    if (-f $started) {
        unlink $started;
        last;
    }
    sleep 15;
}
