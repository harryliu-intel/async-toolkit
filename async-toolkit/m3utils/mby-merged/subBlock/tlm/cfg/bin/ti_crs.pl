#!/usr/intel/bin/perl
use strict;

die "missing turnin number argument\n" if ( $ARGV[0] !~ /\d+/ );
my $tiNum = $ARGV[0];
my $workrepo =
  defined $ENV{GK_WORKREPO}
  ? $ENV{GK_WORKREPO}
  : "/nfs/iil/proj/vlvsc/gk_scratch/gk/hda/workrepos";
my @tiDir = `/bin/ls ${workrepo}`;

# finding the turnin suffix number
my $suffix_num = -1;
foreach my $ti_dir (@tiDir) {
    $suffix_num = $1 if ( $ti_dir =~ /turnin${tiNum}\.(\d+)/ );
}
die "can't find turnin${tiNum} directory inside $workrepo\n"
  if ( $suffix_num == -1 );
my $tiWork = "${workrepo}/turnin${tiNum}\.${suffix_num}";
die "can't locate ${tiWork}/GATEKEEPER/logfiles/gencode.log"
  unless ( -e "${tiWork}/GATEKEEPER/logfiles/gencode.log" );
my $nbflow_cmd = `grep nbflow ${tiWork}/GATEKEEPER/logfiles/gencode.log`;
my @Cmd        = split( "\n", $nbflow_cmd );
my $cmd        = $Cmd[0];
chomp $cmd;
die
"please wait 1 min till GK will create nbfeeder cmd, if the problem isn't resolve within 5 min, please refer to DA team\n"
  unless ($cmd);
print "running: $cmd\n";
my $rc = system($cmd);
$rc = $rc >> 8;

if ($rc) {
    print "FAIL to execute nbflow\n";
    exit 1;
}
else {
    print "execute nbflow successfully\n";
    exit 0;
}
