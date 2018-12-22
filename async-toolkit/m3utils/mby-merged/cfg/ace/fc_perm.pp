# -*-perl-*---------------------------------------------------------------------
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
{
################################################################################
# Note: All regular expressions must be placed with single quotes '/example/i'
#  instead of double quotes "/example/i"
################################################################################
# If this variable is set, the config files in the list are first read in the
#   order listed (so it is possible to overwrite information from an ealier
#   cfgfile.)  Then the remainder of this file is parsed so that the information
#   contained within this file has the highest precedence.
#
# Note: Only one level of hierarchy can exist (so a file listed here cannot then
#   call an additional base config file.)
#
################################################################################

@BASE_CFG_FILES = (
);

%PATTERNS_DEF = (
    # Each of the defined "modes" are checked inside of postsim.pl.  If no modes
    #   are ever "turned on" the test is automatically a fail.
    Modes => {&validate_hash(

        # ---------------------------------------------------------------------
        # Filters that apply to post VCS simulation phase (eg Moat)
        # ---------------------------------------------------------------------
        FcPostSim => {
            Required     => 0,
            StartString  => '/FC_FILTER: Running Post Simulation Check/',
            EndString    => '/# ACE SIM OUTPUT END/',

            okErrors => [

                # akchandr edit
                # Owner: 
                # Date :  
                # HSD  :  
                # Info :
                
                # dteo edit
                # Owner: 
                # Date :  
                # HSD  :  
                # Info :
                
                # gerards edit
                # Owner: 
                # Date :  
                # HSD  :  
                # Info :
                
                # gkaragat edit
                # Owner: 
                # Date :  
                # HSD  :  
                # Info :
               
                # jjperry1 edit
                # Owner: 
                # Date :  
                # HSD  :  
                # Info :
                
                # klee18 edit
                # Owner: Patrick Lee
                # Date : 2/28/2017
                # HSD  : N/A 
                # Info : errors from tracker outputs which are not deemed as real error
                '/.*all_ral_registers_after_start_trk.out.*/',
                '/.*SIP_.*.out.*PER =\s+Parity Error.*/',
                
                # mdsibbel edit
                # Owner: 
                # Date :  
                # HSD  :  
                # Info :
                
                # mmdhanif edit
                # Owner: 
                # Date : 
                # HSD  : 
                # Info :
 
                # rkganti edit
                # Owner: 
                # Date :  
                # HSD  :  
                # Info :
                
                # sgogula edit
                # Owner: 
                # Date :  
                # HSD  :  
                # Info :
                
                # ssnanal edit
                # Owner: 
                # Date :  
                # HSD  :  
                # Info :
                
            ],


            okErrors_multiLine => [

                # Owner: 
                # Date :  
                # HSD  :  
                # Info :  

             ]

        },

        # ----------------------------------------------------------------------------------
        # Filters that apply after UVM start_of_simulation phase till end of VCS simulation
        # ----------------------------------------------------------------------------------
        FcSim => {
            Required     => 0,
            StartString  => '/# fc_test start_of_simulation/',
            EndString    => '/FC_FILTER: Running Post Simulation Check/',

# TODO - Enable this after the `TEST_COMPLETED` string is enabled in flush seq
#             RequiredText => [
#                 '/TEST_COMPLETED/',
#             ],

            okErrors => [

                # akchandr edit
                # Owner: 
                # Date :  
                # HSD  :  
                # Info :
                
                # dteo edit
                # Owner: 
                # Date :  
                # HSD  :  
                # Info :
                
                # gerards edit
                # Owner: 
                # Date :  
                # HSD  :  
                # Info :
                
                # gkaragat edit
                # Owner: 
                # Date :  
                # HSD  :  
                # Info :
               
                # jjperry1 edit
                # Owner: 
                # Date :  
                # HSD  :  
                # Info :
                
                # klee18 edit
                # Owner: 
                # Date :  
                # HSD  :  
                # Info :
                
                # mdsibbel edit
                # Owner: 
                # Date :  
                # HSD  :  
                # Info :
                
                # mmdhanif edit
                # Owner: 
                # Date : 
                # HSD  : 
                # Info : 
                
                # rkganti edit
                # Owner: 
                # Date :  
                # HSD  :  
                # Info :
                
                # sgogula edit
                # Owner: 
                # Date :  
                # HSD  :  
                # Info :
                
                # ssnanal edit
                # Owner: 
                # Date :  
                # HSD  :  
                # Info :
                
                # tsharke edit
                # Owner: 
                # Date :  
                # HSD  :  
                # Info :

            ],


            okErrors_multiLine => [

                # Owner: 
                # Date :  
                # HSD  :  
                # Info :  

             ]

        },

        # -------------------------------------------------------------------------------------------
        # Filters that apply at the beginning of simulation until when UVM start_of_simulation begin
        # -------------------------------------------------------------------------------------------
        FcTime0 => {
            Required     => 0,
            StartString  => '/# ACE SIM OUTPUT START/',
            EndString    => '/# fc_test start_of_simulation/',

            okErrors => [

                # akchandr edit
                # Owner: 
                # Date :  
                # HSD  :  
                # Info :
                
                # dteo edit
                # Owner: 
                # Date :  
                # HSD  :  
                # Info :
                
                # gerards edit
                # Owner: 
                # Date :  
                # HSD  :  
                # Info :
                
                # gkaragat edit
                # Owner: 
                # Date :  
                # HSD  :  
                # Info :
               
                # jjperry1 edit
                # Owner: 
                # Date :  
                # HSD  :  
                # Info :
                
                # klee18 edit
                # Owner: 
                # Date :  
                # HSD  :  
                # Info :
                
                # mdsibbel edit
                # Owner: 
                # Date :  
                # HSD  :  
                # Info :
                
                # mmdhanif edit
                # Owner: 
                # Date :  
                # HSD  :  
                # Info :
                
                # rkganti edit
                # Owner: 
                # Date :  
                # HSD  :  
                # Info :
                
                # sgogula edit
                # Owner: 
                # Date :  
                # HSD  :  
                # Info :
                
                # ssnanal edit
                # Owner: 
                # Date :  
                # HSD  :  
                # Info :
                
                # tsharke edit
                # Owner: 
                # Date :  
                # HSD  :  
                # Info :
                
            ],


            okErrors_multiLine => [

                # Owner: 
                # Date :  
                # HSD  :  
                # Info :  

             ]

        },

    )},

    # List of classified errors to look for.
    # The parser searchs for 'All' first. Then tries to classify
    # For instance,
    # # ** Error: static memory checker error : C17 : - SRAM - ....
    # The above error is matches first with the 'All' regular expression.
    # Then it matches with the '1 Static_Mem' classification.
    # The Number in front of classification is used to order the
    # error types, ie, 1 is more serious than 2.
    #
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    # NOTE: These errors are only matched when one of the above "modes" is active,
    #  otherwise they are IGNORED!!!
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    Errors => [
        # Error regular expr                    Category    Severity
        [ '/Error/i',                            "ALL",      1       ],
        [ '/\$finish at simulation time\s*0/',   "ALL",      1       ],
        [ '/ failed at /',                       "ALL",      2       ],
    ],

    # Timeout strings which result in a postsim.fail with status of "Timeout"
    TimeoutErrors => [
        '/Simulation TIMEOUT reached/i',
    ],

    # This is a list of errors which are to be considered FATAL errors regardless of
    # whether they show up before or after the "StartOn" or "EndOn" conditions.
    FatalErrors => [
        '/Fatal error:/',
        '/Error\: OS signal 7 \(bus error\) received/i',
        '/Error-\[VFS_IOE\] VCS virtual file system IO error/i',
        '/Failed\: Input\/output error/i',
    ],

    # Defines a list of warnings to look for
    Warnings => [
        '/warning/i',
    ],

    # Defines a list of errors which are 'safe' to ignore
    #
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    # NOTE: These errors will be ignored globally (ie. for any of the "modes")
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    okErrors => [

        ################################################################################################
        # This section of filters are permanent 
        ################################################################################################
        # These are expected to always be present
        # Must filter UVM_ERROR and UVM_FATAL count summary
        '/^\s*UVM_ERROR\s*:\s*\d+\s*$/',
        '/^\s*UVM_FATAL\s*:\s*0\s*$/',
        '/Number of demoted UVM_ERROR reports  :    0/',
        '/Number of caught UVM_ERROR reports   :    0/',
        '/Number of demoted UVM_FATAL reports  :    0/',
        '/Number of caught UVM_FATAL reports   :    0/',
        '/\$finish at simulation time\s*0/',

        #The license manager is set to print a moderate number of diagnostic messages.
        #Unfortunately, a normal testrun will have diagnostics which include the string 'error'.
        '/^.*Run..lic_error LMC-\d+..for more information.*$/',
        '/^.*ERROR \(LMC-\d+\): License call failed.*$/',
        '/^.*(FLEXnet|FlexNet) Licensing checkout error\:.*$/',
        '/^.*(FLEXnet|FlexNet) Licensing error\:.*$/',
        # Filter out LP info messages with the word 'ERROR' in them as they are just info messages for NLP simulations.
        '/(.*)INFO(.*)LP_(.*)\(ERROR\)(.*)/',
        '/.*INFO = .*, WARNING = .*, ERROR = .*, FATAL = .*/',
    ],

    okErrors_multiLine => [
        ##[<N lines per error message>, <match pattern for first line>, ..   , <line N-1 pattern> , <line N pattern ],
        #[3, "/ERROR.*\(introduced error\)/", "/At time/", "/ERROR - example multi-line error/"],
    ],

    # Any additional information that is required from the logfile can be entered here:
    #
    # This is what is already extracted:
    #   Test name:
    #   Test type:  (Verilog procedural or Xscale assembly)
    #   Status:
    #   Cause: (for fails only)
    #   Error Count:
    #   Warning Count:
    #   SIMPLI Error Count:
    #   SIMPLI Check Count:

    ## Default test type is "proc" procedural.  Gets changed to "asm" for assembly if the
    ##  following regular expression matches in the transcript.
    TestType => {
        Default => {
            regex   => undef,
            keyword => "Proc",
        },
        Assembly => {
            regex   => '/Reading test memory image/',
            keyword => "Asm",
        },
    },

    TestInfo => [
        # Use this to add to, or overwrite the standards
        # defined in $ACE_HOME/udf/ace_test_info.pp
    ],

    # Simple calulations based on contents of the 'TestInfo' array
    Calculations => {
        SimRate => '($TestInfo{Runtime} != 0) ? $TestInfo{Cycles}/$TestInfo{Runtime} : 0',
    },
 );
 };


sub validate_hash {
    # This is an auxillary function which detects 2 problems in a hash
    #  1. duplicated keys
    #  2. a missing key or value
    # When import into the postsim, it will only print warnings.
    # When called with the environment variable VTE_FLOW_DEBUG it dies.
    if (@_ % 2) {
        print "Odd number of elements in $0\n";
        exit 1 if defined $ENV{VTE_FLOW_DEBUG};
    }
    my %h;
    while ( my ( $key, $new_val ) = splice @_, 0, 2 ) {
        if ( exists $h{$key} ) {
            print "Duplicated key in $0:\n\t$key => $h{$key}\n\t$key => $new_val\n";
            exit 1 if defined $ENV{VTE_FLOW_DEBUG};
        }
        $h{$key} = $new_val;
    }
    return %h;
}

