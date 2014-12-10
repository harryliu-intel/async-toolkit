#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

# Note and Warning: this can still fail due to asynchonous issues since
# there is no locking mechanism to prevent a license being grabbed after
# this program checks the license.

use strict;
use Getopt::Long;
my $licenses;
my @licenses;
my %licenses;
if ( !defined ($ENV{LM_LICENSE_FILE})) {
    $ENV{LM_LICENSE_FILE} =
        "/usr/local/flexlm/licenses/license.80e5b6d8.dat".
        ":/usr/local/flexlm/licenses/license.0048546B71E0.dat";
}

sub usage {
    print @_;
    print <<U;
Usage: lmwait [--wait] --licenses <list> <command line>
    --wait       : do not ask to wait
    --licenses   : [required] list of licenses to check
    command line : the command to execute when license available
U
exit 1;
}

sub checklicense {
    local (*P);
    local ($_);
    if ($#licenses == 0) {
        open (P, "/usr/local/cadence/bin/ic lmstat -f $licenses[0] |");
    }
    else {
        open (P, "/usr/local/cadence/bin/ic lmstat -a |");
    }
    my $nok=0;
    while (<P>) {
        chomp;
        if (/^Users of /) {
            s/:/ /g;
            my @f=split;
            if ($licenses{$f[2]}) {
                $licenses{$f[2]} = 2;
                if ($f[5] <= $f[10]) {
                    $nok = 1;
                }
            }
        }
    }
    close P;
    foreach my $l (keys %licenses) {
        if ($licenses{$l} == 1) {
            $nok = 2;
            print STDERR "License $l not found, possible spelling error.";
        }
    }
    if ($nok == 2) {
        die;
    }
    $nok;
}

my $noask = 0;
GetOptions (
    "licenses=s" => \$licenses,
    "wait" => \$noask,
) or usage 1;

if (defined ($licenses) ) {
    @licenses = split(/[ ,]/, $licenses);
}

usage $licenses if $#licenses < 0;
foreach my $l (@licenses) {
    $licenses{$l}=1;
}

my $istty = (defined ($ENV{TERM}));
my $user="";

$|=1;
my $ex = shift (@ARGV);
if ( ! defined ($ex)) {
    usage 3;
}
unshift (@ARGV, $ex);
my $nok = checklicense;
if ($istty and $nok and ! $noask) {
    printf "License(s) in use, do you want to wait? ";
    my $ans=<>;
    if ($ans =~ /^n/i) {
        exit 1;
    }
}
my $cnt=0;
while ($nok) {
    sleep 1;
    $nok = checklicense;
    if (! $cnt) {
        print "Waiting for a license";
    }
    $cnt++;
}
$"=" ";
print "running: @ARGV";
exec @ARGV;
