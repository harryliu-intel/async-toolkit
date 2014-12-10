#!/usr/local/bin/perl -w
#
#      make_obfuscated_jar.pl - obfuscate a jar, and remove jcast cruft
#
#      Copyright 2002 Fulcrum Microsystems, Inc.  All rights reserved.
#
#      $Id: //depot/sw/sdk/core/maelstrom/main/build/java/src/scripts/make_obfuscated_jar.pl#1 $
#

use Getopt::Long;
use FindBin;
use POSIX;

$AntlrAllJar = "/usr/local/jars/antlrall.jar";
$RetroguardJar = "/home/group/maelstrom/jars/retroguard.jar";
$BcelJar = "/home/group/maelstrom/jars/bcel.jar";
$LogFile = tmpnam();

$Input = undef;
$Output = undef;

GetOptions("antlralljar=s" => \$AntlrAllJar,
           "retroguardjar=s" => \$RetroguardJar,
           "bceljar=s" => \$BcelJar, "logfile=s" => \$LogFile,
           "input=s" => \$Input, "output=s" => \$Output) or die;

if (not defined $Input or not defined $Output) {
    die "input and output must be specified\n";
}

# First step: run Retroguard on whole jar file

$RgsFile = "$FindBin::Bin/f16-sdk.rgs";

$ENV{'CLASSPATH'} = "$AntlrAllJar:$BcelJar:$Input:$RetroguardJar";

dsystem("java RetroGuard $Input $Output $RgsFile $LogFile");

# Second step: parse log file to create translation table

open LOGFILE, $LogFile or die;
while (<LOGFILE>) {
    if (/^\.package_map\s+([^\s]+)\s+([^\s]+)$/) {
        $ObMap{$1} = $2;
    } elsif (/^\.class_map\s+([^\s]+)\s+([^\s]+)$/) {
        $ObMap{$1} = $2;
        push @Classes, $1;
    }
}
close LOGFILE;

foreach $class (@Classes) {
    $ReverseOb{lookup($class)} = $class;
}

# Third step: read the contents of the jar and make a list of
# classes we don't want

@DontWant = ();
open UNZIP, "unzip -l $Output |" or die;
while (<UNZIP>) {
    if (/^\s*\S+\s+\S+\s+\S+\s+(.*)\.class\s*$/) {
        my $class = $1;
        my $unobfuscated = $class;
        $unobfuscated = $ReverseOb{$class} if (exists $ReverseOb{$class});
        if (not wanted($unobfuscated)) {
            push @DontWant, $class;
        }
    }
}
close UNZIP;

# Last step: remove unwanted classes from the jar

open ZIP, "| zip -d -\@ $Output" or die;
foreach $class (@DontWant) {
    print ZIP "$class.class\n";
}
close ZIP;

sub wanted {
    my $class = $_[0];

    if ($class =~ m%^com/avlsi/tools/sigscan% or
        $class =~ m%^com/avlsi/tools/dsim% or
        $class =~ m%^com/avlsi/tools/tsim/[^/]+$% or
        $class =~ m%^com/avlsi/tools/cosim% or
        $class =~ m%^com/avlsi/util/container% or
        $class =~ m%^com/avlsi/util/text% or
        $class =~ m%^com/avlsi/util/exception% or
        $class =~ m%^com/avlsi/util/debug% or
        $class =~ m%^com/avlsi/util/math% or
        $class =~ m%^com/avlsi/io% or
        $class =~ m%^com/fulcrummicro% or
        $class eq 'com/avlsi/util/cmdline/Signal$Handler' or
        $class eq 'com/avlsi/file/common/InvalidHierNameException' or
        $class eq 'com/avlsi/util/bool/AbstractBooleanExpression' or
        $class eq 'com/avlsi/util/bool/BooleanExpressionInterface' or
        $class eq 'com/avlsi/util/bool/HierNameAtomicBooleanExpression' or
        $class eq 'com/avlsi/util/bool/AbstractAtomicBooleanExpression' or
        $class eq 'com/avlsi/util/bool/OrBooleanExpressionInterface' or
        $class eq 'com/avlsi/util/bool/AndBooleanExpressionInterface' or
        $class eq 'com/avlsi/cast/CastFileParser' or
        $class eq 'com/avlsi/cell/CellInterface' or
        $class eq 'com/avlsi/tools/jflat/JFlat$CellProcessor') {
        return 1;
    } else {
        return 0;
    }
}

# Given a full unobfuscated name, return the full obfuscated name
sub lookup {
    my $unobfuscated = $_[0];
    my $first = "";
    my $sep = "";
    my $last = $unobfuscated;
    if ($unobfuscated =~ /^(.*)(\$)(.*?)$/) {
        $first = $1;
        $sep = $2;
        $last = $3;
    } elsif ($unobfuscated =~ /^(.*)(\/)(.*?)$/) {
        $first = $1;
        $sep = $2;
        $last = $3;
    }
    return $unobfuscated if (not exists $ObMap{$unobfuscated});
    return lookup($first) . $sep . $ObMap{$unobfuscated};
}

# procedure to execute a given command
sub dsystem {
    my $cmd = $_[0];
    print "$cmd\n";
    my $result = system($cmd);
    my $retcode = $? / 256;
    die "$cmd returned $retcode" if ($retcode);
    return $result;
}
