#!/usr/local/bin/perl
# vim:et:sw=4:ts=4:tw=79:

use strict;
use warnings;

use File::Find;

sub trim
{
    my ($s) = @_;

    $s =~ s/^\s+//;
    $s =~ s/\s+$//;
    return $s;

}   # end trim

my $LOCAL = 1;

my $JAVA = "/usr/local/java/bin/java";
my $P4   = "/usr/local/bin/p4";

my ($root) = grep {m/info:\s+Root:/} split(/\n/, readpipe("$P4 -s client -o"));
$root = trim((split(/:/, $root))[-1]);

my $JARS      = "$root/reg-main/shared/jars";
my $CLASSES   = "$root/reg-main/shared/java/classes";
my $CLASSPATH = $LOCAL ? $CLASSES : "$JARS/util.jar";
$CLASSPATH    .= ":$JARS/dsim.jar:$JARS/jep-2.4.1.jar";
my $CLASS     = "com.fulcrummicro.util.properties.PropReader";
my $JAVACMD   = "$JAVA -classpath $CLASSPATH $CLASS";

my $fail = 0;
my $find = $ARGV[0];
if(!defined $find) {
    $find = '.';
}

find(\&find_tests, '.');
if($fail) {
    print "$fail tests failed\n";
} else {
    print "PASS\n";
}

sub find_tests {
    if((defined $find) and !(/$find/)) {
        return;
    }

    if(/^(t\d+)\.test$/) {
        my $name = $File::Find::name;
        my $args = "";
        print "*** $name ***\n";
        if(-e "$1.args") {
            open(ARGS, "<", "$1.args") or die "failed to open $1.args\n";
            while(<ARGS>) {
                chomp;
                $args .= " $_";
            }
        }
        my $optfile = "";
        if(-e "$1.xml") {
            $optfile = "$1.xml";
        }
        my $cmd = "$JAVACMD common.xml $optfile --config=$1.test --topdir=. $args";
        #print "  * $cmd\n";
        ##print `$cmd`;
        if(system("$cmd | sort | diff $1.expected -") != 0) {
            print "FAIL: test $name\n";
            $fail++;
        }
    } elsif(/^(t\d+)$/) {
        my $name = $File::Find::name;
        my $args = "";
        print "*** $name ***\n";
        if(-e "$1/args") {
            open(ARGS, "<", "$1/args") or die "failed to open $1/args\n";
            while(<ARGS>) {
                chomp;
                $args .= " $_";
            }
        }
        my $optfile = "";
        if(-e "$1/t.xml") {
            $optfile = "$1/t.xml";
        }
        my $cmd = "$JAVACMD common.xml $optfile --config=$1/test --topdir=. $args";
        #print "  * $cmd\n";
        ##print `$cmd`;
        if(system("$cmd | sort | diff $1/expected -") != 0) {
            print "FAIL: test $name\n";
            $fail++;
        }
    }
}
