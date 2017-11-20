#!/usr/intel/pkgs/perl/5.8.7/bin/perl
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
#--------------------------------------------------------------------------------
# $Author: vromaso $
# $Date: 2011/03/15 21:48:47 $
# $Revision: 1.17 $
# $Source: /nfs/ace/CVS/ace/bin/ace_vlog.pl,v $
#--------------------------------------------------------------------------------
# Converts VLOGAN (vcs) options to VLOG (modelsim) options
#--------------------------------------------------------------------------------
use strict;
use Getopt::Long;
use lib "$ENV{ACE_HOME}/lib";
use Ace::OptionsTranslator;
Getopt::Long::config("no_auto_abbrev");
Getopt::Long::config("no_ignore_case");
Getopt::Long::config("pass_through");

# Start UnitTest
# -sv -2001 -timescale=1pw/2ns -nc -ntb_opts dtm+pcs-hoho -unkown1 +unknown=2 -sverilog -R rta1 -rta2 - chicken -L lib7 -override_timescale=3ps/4ns
# Expected: +unknown=2 - -L -R -override_timescale=3ps/4ns -rta2 -sv -unkown1 -vlog01compat chicken lib7 rta1
# End UnitTest
my %translator = (
    -vcs => {
        '-timescale='         => [('',1)],   # [H]v should translate to -timescale=
        '+define+'            => [('',1)],   
        '-sverilog'           => [('-sv', 0)], 
        '-override_timescale=' => [('-override_timescale',1, ' ')], 
        '-liblist'            => [('-L',1, ' ')], 
        '+verilog2001ext'     => [('',0)], 
        '-power_aware'        => [('',1)], 
        '-pa_initial_on'      => [('',1)], 
        '-pa_random_corrupt'  => [('',1)], 
        '-Xzqi='              => [('',1)], 
        '-ntb_opts'           => [('',1)],
        '+pathpulse'          => [('',1)],
        '-nc'                 => [('',0)],
        '-sep_cmp'            => [('',0)],
        '-lca'                => [('',0)],
        '-ntb_vl'             => [('',0)],
        '-assert'             => [('',1)],
	'-Xkeyopt='        => [('',1)],
    },
    -debusyy => {
        -2001 => [('-vlog01compat',0)],
        -skipClass => [('', 0)],
    }
    
);
my @myoptions = qw(-93 -compat -compile_uselibs -cuname +define+ +delay_mode_distributed
                   +delay_mode_path +delay_mode_unit +delay_mode_zero -dpiheader -error -f -fatal -gen_xml -hazards -help 
                   +incdir+ -incr -isymfile +libext+ -libmap -libmap_verbose +librescan -line -lint +maxdelays +mindelays
		   -mfcu -noincr +nolibcell -nologo +nospecify -note +notimingchecks +nowarn -nowarn -O1 -quiet -R -refresh 
		   -source -sv -suppress -time -timescale +typdelays -u -v -version -vlog01compat -vlog95compat -warning -work  
		   -y +acc= -novopt -vopt -L -Lf -cover -coverAll -coverExcludeDefault -coverGenerate -nodebug -nodebug=ports
		   -nodebug=pli +protect -fast=
                   
                   +acc +nocheck +opt+ - -0in -0in_options -O0 -O4 -O5 -fast -forcecode -keep_delta -nodebug=pli -nopsl -pslfile
                   );
my @othersoptions = ();

#-------------------------------------------------------------------------------
# Main
#-------------------------------------------------------------------------------
my $ot = new Ace::OptionsTranslator(-translator => \%translator, -block_unknown_options => 0,
                                    -myoptions => \@myoptions, -othersoptions => \@othersoptions);
$ENV{MODELSIM} = $ENV{MTI_MODELSIM_INI};
print "\n# MODELSIM=$ENV{MODELSIM}\n";
my $cmd = "$ENV{MTI_HOME}/bin/vlog" . $ot->translate(@ARGV);
   $cmd = $ot->uniquify($cmd);

print "\n\n$cmd\n";
unless (defined $ENV{ACE_OPTIONS_TRANSLATOR_DEBUG_MODE_ON}) {
    my $rc = $ot->execute_cmd($cmd);
    exit $rc;
}
