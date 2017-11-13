#!/usr/intel/bin/perl
use strict;
use warnings;
use Getopt::Long;
use File::Temp qw/tempfile tempdir/;
use File::Spec::Functions;

my $output = "dsim.jar";
GetOptions("output=s" => \$output) || die;

my $package_root = $ENV{'FULCRUM_PACKAGE_ROOT'};
my $java = catdir($package_root, 'share', 'java');

my $dir = tempdir(CLEANUP => 1);

my @jars = ('all.jar', 'antlr-2.7.2.jar', 'concurrent.jar', 'stringtemplate.jar');

my $xfcmd = "cd '$dir' && " . join(' && ', map { "jar xf '" . catfile($java, $_) . "'" } @jars);

system($xfcmd) == 0 || die "Can't execute $xfcmd: $!";

my ($mh, $mf) = tempfile();

print $mh <<EOF;
Manifest-Version: 1.0
Created-By: 1.4.2_05 (Apple Computer, Inc.)
Main-Class: com.avlsi.tools.dsim.DSimMain
EOF

close $mh;

my $cfcmd = "jar cfm '$output' $mf -C '$dir' .";
system($cfcmd) == 0 || die "Can't execute $cfcmd: $!";

print <<EOF
$output generated successfully, run DSim with:
	java -jar -Xmx512m $output --no-readline <other standard arguments>
EOF
