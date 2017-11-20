#!/usr/intel/pkgs/perl/5.14.1/bin/perl
use UsrIntel::R1;
#--------------------------------------------------------------------------------
# INTEL CONFIDENTIAL
#
# Copyright (June 2005)2 (May 2008)3 Intel Corporation All Rights Reserved. 
# The source code contained or described herein and all documents related to the
# source code ("Material") are owned by Intel Corporation or its suppliers or
# licensors. Title to the Material remains with Intel Corporation or its
# suppliers and licensors. The Material contains trade secrets and proprietary
# and confidential information of Intel or its suppliers and licensors. The
# Material is protected by worldwide copyright and trade secret laws and treaty
# provisions. No part of the Material may be used, copied, reproduced, modified,
# published, uploaded, posted, transmitted, distributed, or disclosed in any way
# without Intels prior express written permission.
#
# No license under any patent, copyright, trade secret or other intellectual
# property right is granted to or conferred upon you by disclosure or delivery
# of the Materials, either expressly, by implication, inducement, estoppel or
# otherwise. Any license under such intellectual property rights must be express
# and approved by Intel in writing.
#
#--------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Ignores certain simulator options that experience has shown to be unaccepted
# by vericom.
# Debussy accepts "all simulator options" (their claim) - so we shouldn't hide
# anything from it as it might need those options. However, of course, they
# don't really accept all options and error out on some - so must compensate for
# that by putting them here.
#-------------------------------------------------------------------------------
use strict;
use Getopt::Long;
use lib "$ENV{ACE_HOME}/lib";
use Ace::OptionsTranslator;
Getopt::Long::config("no_auto_abbrev");
Getopt::Long::config("no_ignore_case");
Getopt::Long::config("pass_through");

# Don't delete these comments - they form the unit test for this script.
# Start UnitTest
# Input: -ntb_opts "dtm+pcs" -unknown +unknown2+=boo -liblist lib1 lib2 -2001 -ams -applog -cricket a+b +define+MACTP -L unitA -L unitB -f that.f -work whatever -R o,m,g -smartinc
# Expected: -L lib1 lib2 -2001 -ams -applog a+b +define+MACTP -L unitA -L unitB -f that.f -work whatever o,m,g -smartinc
# End UnitTest
my %translator = (                   
                         -mti => {	  
                                 },
                         -vcs => {
                                  -suppress => [("", -1, ',')],
				 '-ntb_opts'           => [("", 1, ' ')],
                                 '-power_aware'        => [('', 1, ' ')],
                                 '-pa_initial_on'      => [('', 1, ' ')],
                                 '-pa_random_corrupt'  => [('', 1, ' ')], 
                                 '-Xzqi=0x100'         => [('', 1, ' ')],
                                 '+pathpulse'          => [('', 1, ' ')],
                                 '-sep_cmp'            => [('', 1, ' ')],
                                 '-lca'                => [('', 1, ' ')],
                                 '-liblist'            => \&liblist,
                                 #'-liblist'            => [('-L', 1, ' ')],
                                 '-ntb_vl'             => [('', 1, ' ')],
                                },
                        );
# This compiler's own options
my @myoptions = qw(
		-2001 -ams -applog -autoendcelldef -comment_transoff_regions
		-define -errormax -extractRTL -incdir -help -h -lib -libmap -mentor
                -logdir -nclib -nlog +nlog -ny -nv -psl_inline -psl_inline_append
		-quiet -rcFile -replace -rep -silent -stdout -smartinc -ssv -ssy
                -ssz -sv -sverilog -sv_pragma -vc -view -work -assert -syntaxerrormax
                +libverbose +spiceExt+ +v2k -2001genblk -2005 -2009 -skipClass -cunit
                );
# Other simulators options accepted by vericom
my @othersoptions  = qw (
                -y -v -u -uppercase -f +define+ +incdir+ +libext+ +veraext+
                +systemverilogext+ +verilog2001ext+ -L
		);                
# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------
my $ot = new Ace::OptionsTranslator(-translator => \%translator, -myoptions => \@myoptions, -othersoptions => \@othersoptions);

my $cmd = "vericom";
my $tmp_dir = `mktemp -u `;
chomp $tmp_dir;
$cmd .= $ot->translate(@ARGV);
$cmd .= " -cunit ";
$cmd .= " -cunit -2012 +rmkeyword+checker ";
$cmd .= "-2012",
$cmd .= " +define+VERDICOMP+NOVAS_UPF_PKG ";
$cmd .= " -ssy -ssv -ssz ";
$cmd  =~ s/\-timescale=\S+\s/ /g;
$cmd  .= " -logdir $tmp_dir/$ENV{USER}/verdi ";
 $cmd .= "-2012 " ;

system ("/bin/mkdir -p  $tmp_dir/$ENV{USER}/verdi");
print "\n\n$cmd\n";
unless (defined $ENV{ACE_OPTIONS_TRANSLATOR_DEBUG_MODE_ON}) {
    my $rc = $ot->execute_cmd($cmd);
    unless ($rc) {
	system ("/bin/rm -rf  $tmp_dir");
    }
    exit $rc;
}
# ------------------------------------------------------------------------------
# Converts
#        -liblist A+B+C
# to
#        -L A -L B -L C
# ------------------------------------------------------------------------------
sub liblist {
  my ($i, $ARGV) = @_;
  my @trans;
  $$i++;
  if ($ARGV->[$$i] =~ /\+/) {
    my @ary = split /\+/, $ARGV->[$$i];
    foreach my $l (@ary) {
        push @trans, "-L $l"
    }
  }
  else {
    push @trans, "-L $ARGV->[$$i]";
  }
  return join ' ', @trans;
}
