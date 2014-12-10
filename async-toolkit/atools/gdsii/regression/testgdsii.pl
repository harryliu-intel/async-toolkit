#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

use File::Temp qw(tempdir);
use Getopt::Long;

my $verbose = 0;

sub logit {
    my ($msg) = @_;
    if ($verbose) {
        print "$msg";
    }
}

my $erv = 0;

sub testcmd {
    my ($cmd, $msg) = @_;
    my $rv;
    if ($verbose) {
        $rv = system "( $cmd )";
    }
    else {
        $rv = system "( $cmd ) >/dev/null 2>\&1";
    }
    $rv = ($rv - ($rv % 256)) / 256;
    if ($rv && defined ($msg)) {
        logit ($msg);
    }
    $erv |= $rv;
    $rv;
}

GetOptions (
    "verbose" => \$verbose,
);

# test of the gdsii package
# install package
#my $dir=tempdir ("/scratch/gdsii.XXXXXX", CLEANUP => 1);
my $dir = shift @ARGV;
if ( ! -d $dir ) {
    logit ("$dir is not a directory or does not exist");
    print "FAIL";
    exit 1;
}
open (P, "find $dir -name .fulcrum-package-root |");
$package_root=<P>;
close P;
chomp $package_root;
$package_root =~ s/.fulcrum-package-root//;
$uname=`uname -s`;
chomp $uname;
my $osname=$uname;
$uname .= "-".`uname -m`;
chomp $uname;
my $regressiondir="$package_root/regression";
$package_root .= "$uname/bin";
$ENV{PATH} = "$package_root:/usr/intel/bin:/usr/bin:/bin";
logit "Bin dir: $package_root";
$testgdsfile="testgdsii.gds";
$outgds="/tmp/test$$.gds";
unlink $outgds;
$erv |= ($rv = ( -f $outgds ));
if ($rv) {logit "Failed to remove $outgds";}
logit "Testing rdgds and wrgds";
testcmd "rdgds $regressiondir/$testgdsfile | wrgds > $outgds", "Failed to create $outgds";
testcmd "cmp $regressiondir/$testgdsfile $outgds","Compare failed";
unlink $outgds;
logit "testing scalegds";
testcmd "scalegds $regressiondir/$testgdsfile $outgds","scalegds failed";
unlink $outgds;
logit "testing aaggds";
testcmd "aaggds 2>\&1 $regressiondir/$testgdsfile <<!
t core_nexus_shared_crossbar_tail_TAIL_SPLIT_ENV_1_1000
wh $outgds
q
!
","aaggds Failed";
system "rdgds $regressiondir/testaaggds.gds > /tmp/testaaggds$$.rdg";
system "rdgds $outgds > ${outgds}$$.rdg";
open (P, "diff /tmp/testaaggds$$.rdg ${outgds}$$.rdg | wc -l |");
$rslt=<P>;
chomp $rslt;
while (<P>) {};
close P;
$rslt =~ s/ //g;
if ($rslt > 4) {
    $rv = $rslt - 4;
}
else {
    $rv = 0;
}
$erv |= $rv;
if ($rv) {logit "aaggds Failed";}
unlink $outgds;
unlink "/tmp/testaaggds$$.rdg";
unlink "${outgds}$$.rdg";
logit "testing gifdwg";
testcmd "gifdwg -p $regressiondir/testdwg.dwg > /tmp/test$$.gif", "gifdwg fails";
testcmd "cmp $regressiondir/test$osname.gif /tmp/test$$.gif", "gifdwg fails";
unlink "/tmp/test$$.gif";
logit "testing dwg";
testcmd "dwg -o /tmp/test$$.ps $regressiondir/testdwg.dwg", "dwg fails";
testcmd "diff $regressiondir/test$osname.ps /tmp/test$$.ps", "dwg fails";
unlink "/tmp/test$$.ps";
if (defined ($ENV{DISPLAY})) { # dont test when there is no DISPLAY
logit "testing xdwg";
testcmd "xdwg $regressiondir/testdwg.dwg", "xdwg fails";
testcmd "pkill xdwg","xdwg fails";
}
print $erv ? "FAIL" : "PASS";
exit $erv;

