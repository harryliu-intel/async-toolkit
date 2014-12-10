#!/usr/intel/bin/perl -w

use strict;
use vars qw($Logfile $Cell $Dir $TmpDir $CastPath $Version $PrsBlock
            $HtmlFile $Transitions $Rules);

use FindBin;
use Getopt::Long;
use POSIX;

# On stdout, outputs tuple of:
# version|cellname|status|transitions|rules

$CastPath = "/home/user/ppelleti/clients/ppelleti_microarch/hw/cast"; # FIXME!
$Logfile = "/dev/null";

GetOptions("logfile=s", \$Logfile) or die;

$Cell = $ARGV[0];
$Dir = $FindBin::Bin;

$TmpDir = POSIX::tmpnam();

mkdir $TmpDir or die;

open S, "$Dir/test.cast" or die;
open D, ">$TmpDir/test.cast" or die;

$Version = "unknown";
$PrsBlock = "";

my $foundit = 0;
while(<S>) {
    $foundit = 1 if (/^define\s+$Cell\W/);
    if ($foundit and /^\s*env\s+\{/) {
        open P, "presto --cast-path=$Dir:$CastPath --cell=test.$Cell |" or die;
        my $old_ = $_;
        while (<P>) {
            print D $_;
            $PrsBlock .= $_;
            $Version = $1 if (/PReSto of (.*?)\./);
        }
        close P;
        $_ = $old_;
        print D "\n";
        $foundit = 0;
    }
    print D $_;
}

close D;
close S;

open P, "jdsim-auto --cast-path=$TmpDir:$CastPath --cell=test.$Cell --basedir=$TmpDir 2>&1 |" or die;

my $lastline = "error";
while (<P>) {
    chomp;
    $lastline = $_;
}
close P;

$HtmlFile = "$TmpDir/modules/test/$Cell.html";

open S, "$HtmlFile" or die;
open D, ">>$Logfile" or die;

print D "<h2>PRS for $Cell</h2><pre>\n";
$PrsBlock =~ s/\&/\&amp;/g;
$PrsBlock =~ s/\</\&lt;/g;
$PrsBlock =~ s/\>/\&gt;/g;
print D $PrsBlock;
print D "</pre>\n";

my $output = 0;
while (<S>) {
    $output = 0 if (m%</body>%i);
    s/<a href[^>]+>//i;
    s%</a>%%i;
    print D $_ if ($output);
    $output = 1 if (/ END OF NAVBAR /);
}

close D;
close S;

$Transitions = "unknown";
$Rules = "unknown";

open P, "lynx -dump -nolist $HtmlFile |" or die;
while (<P>) {
    $Transitions = $& if (/[\d\.]+t/);
    $Rules = $1 if (/\#Rules\s+(\d+)/);
}
close P;

unlink $HtmlFile or die;
unlink "$TmpDir/test.cast" or die;
rmdir "$TmpDir/modules/test" or die;
rmdir "$TmpDir/modules" or die;
rmdir $TmpDir or die;

print "$Version|$Cell|$lastline|$Transitions|$Rules\n";
