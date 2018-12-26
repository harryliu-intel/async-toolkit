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
    "$ENV{MODEL_ROOT}/cfg/ace/fc_perm.pp"
);

%PATTERNS_DEF = (
    # Each of the defined "modes" are checked inside of postsim.pl.  If no modes
    #   are ever "turned on" the test is automatically a fail.
    Modes => {&validate_hash(

        # ---------------------------------------------------------------------
        # Filters that apply to post VCS simulation phase (eg Moat)
        # ---------------------------------------------------------------------
        FcTempPostSim => {
            Required     => 0,
            StartString  => '/FC_FILTER: Running Post Simulation Check/',
            EndString    => '/# ACE SIM OUTPUT END/',

            okErrors => [
                # akchandr edit
                # Owner: Chandran, Ajay K
                # Date : 
                # HSD  : 
                # Info : 
 
                # cmtanbon edit

                # dteo edit

                # gerards edit

                # gkaragat edit

                # jjperry1 edit

                # klee18 edit
 
                # mdsibbel edit

                # mmdhanif edit

                # rkganti edit

                # sgogula edit

                # ssnanal edit
            ],


            okErrors_multiLine => [

                # Info :

             ]

        },

        # ----------------------------------------------------------------------------------
        # Filters that apply after UVM start_of_simulation phase till end of VCS simulation
        # ----------------------------------------------------------------------------------
        FcTempSim => {
            Required     => 0,
            StartString  => '/# fc_test start_of_simulation/',
            EndString    => '/FC_FILTER: Running Post Simulation Check/',

            okErrors => [

                # akchandr edit
                # NSS specific filters
                #'/\*Denali\* Error.*nss_tb_env_inst.hif_env.ARcPEpEnv0.passiveEp.*$/',  
                #'/\*Denali\* Error.*nss_tb_env_inst.hif_env.ARcPEpEnv0.activeRc0.*$/',  

                # Lot of Denali errors reported, skip for now
                #'/UVM_ERROR.*FLUSH_PHASE.*errors reported by Cadence BFMs/',

                # rgudla edit
                #'/^.*fc_hdl_top.dut.parimc.parimcsbbfuse.sbbv4_i1.u_mpe_ccu_core.u_ccu_progcntr.unnamed.*$/',
                
                # dteo edit

                # gerards edit

                # jtnovack edit

                # gkaragat edit
                
                # jjperry1 edit

                # klee18 edit

                # mdsibbel edit

                # mmdhanif edit

                # msheetz edit

                # rkganti edit

                # sgogula edit

                # ssnanal edit
        
                # akau edit

                # cholland edit

                # dobrien2 edit

                # jtholiyi edit

                # jtnovack edit

                # jwbarz edit

                # rjstach edit

                # rkoganti edit

                # ssurendi edit
                
],

            okErrors_multiLine => [

                # Owner: Mat Khir Md Haniffa 
                # Date : 08/14/2017
                # HSD  : bt 
                # Info : Parity not enabled for SATA PSF instances (PSF7/8/9)
             ]

        },

        # -------------------------------------------------------------------------------------------
        # Filters that apply at the beginning of simulation until when UVM start_of_simulation begin
        # -------------------------------------------------------------------------------------------
        FcTempTime0 => {
            Required     => 0,
            StartString  => '/# ACE SIM OUTPUT START/',
            EndString    => '/# fc_test start_of_simulation/',

            okErrors => [

                # akchandr edit
                # NSS specific filters
                #'/\*Denali\* Error.*nss_tb_env_inst.pcieSve0.*/',  
                #'/UVM_WARNING.*TPRGED.*$/',

                # Denali error counter
                #'/for Error-Count enabled for Denali verification suite/',

                # akchandr edit
                # Filters for PHYSS errors
                #'/UVM_WARNING.*WAKE_ERROR_COUNTER_inst/',
                #'/\*Denali\*.*physs_integ_subenv.*ENET_XT CONFIG.*ERROR.*$/',

                # cmtanbon edit
                
                # dteo edit

                # gerards edit

                # gkaragat edit
                
                # jjperry1 edit

                # klee18 edit

                # mdsibbel edit

                # mmdhanif edit

                # rkganti edit

                # sgogula edit

                # ssnanal edit

                # akau edit

                # cholland edit

                # dobrien2 edit

                # jtholiyi edit

                # jtnovack edit

                # jwbarz edit

                # rjstach edit

                # rkoganti edit


            ],


            okErrors_multiLine => [

                # akchandr edit

                # cmtanbon edit
                
                # dteo edit

                # gerards edit

                # gkaragat edit

                # jjperry1 edit

                # klee18 edit

                # mdsibbel edit

                # mmdhanif edit

                # rkganti edit

                # sgogula edit

                # ssnanal edit


             ]

        },

        #FcDebugPostSim => {
        #    # Set this attribute to totally ignore the mode
        #    Ignore      => 1,
        #    Required     => 0,
        #    StartString  => '/# ACE SIM OUTPUT START/',
        #    EndString    => '/# ACE SIM OUTPUT END/',
#
#            okErrors => [
#                '/^.*RX_errorCbPort.*$/',
#                '/^.*TX_errorCbPort.*$/',
#                '/^.*errorCbPort.*$/',
#            ],
#
#
#            okErrors_multiLine => [
#                # Info :
#             ]
#        },

        # ---------------------------------------------------------------------
        # Filters that apply to UPF enabled simulation
        # Owner : Ravindra Ganti
        # Date  : 03/07/2071
        # Info  : A set of test filters catered for UPF-enabled only simulation
        # ---------------------------------------------------------------------

        UPFMode=> {
            Required     => 0,
            StartString  => '/ACE_ERR_FILTER: Enable UPF Simulation filters/',
            EndString    => '/ACE SIM OUTPUT END/',

            okErrors => [

                # akchandr edit

                # dteo edit

                # gerards edit

                # gkaragat edit

                # jjperry1 edit

                # klee18 edit

                # mdsibbel edit

                # mmdhanif edit

                # rkganti edit
                # Owner: Ravindra Ganti
                # Date : 
                # HSD  :
                # Info : Filter out LP info messages with the word 'ERROR' in them as they are just info messages.
                #'/(.*)INFO(.*)LP_(.*)\(ERROR\)(.*)/',
                #'/.*INFO = .*, WARNING = .*, ERROR = .*, FATAL = .*/',

                # Owner: Ravindra Ganti
                # Date : 06/30/2017
                # HSD  : 217280
                # Info : Assertion failures at time zero of NLP simulations are spurious and can be ignored. 
                #'/^.*: started at 0fs failed at 0fs.*/',
                #'/^.*: at time 0 fs.*/',
                #'/^.*.vcc_assertion at 0.0.*/',
                # Owner: Ravindra Ganti
                # Date : 06/30/2017
                # HSD  : 217281 
                # Info : Assertion failures during power down -> power up as the corrupted signals change from 'X' to a known value (0 or 1). These are OK to ignore in the context of power transition times only. 
                #'/^.*.known_driven_rd_addr_p0:.*/',
                #'/^.*.known_driven_rd_en_p0:.*/',
                #'/^.*.known_driven_wr_addr_p0:.*/',
                #'/^.*.known_driven_wr_en_p0:.*/',
                #'/^.*.L_0in_known_driven.*/',
                #'/^.*.ASSERT_CFMIA_GATE_CLOCK_ERR1.*/',
                #'/^.*.ASSERT_CFMIA_GATE_CLOCK_ERR2.*/',
                #'/^.*.ASSERT_NEVER_Input_Vecotor_Mutex.*/',
                #'/^.*.ASSERT_NEVER_hconfig_write_ambiguity.*/',
                #'/^.*.ASSERT_NEVER_hconfig_read_ambiguity.*/',
                #'/^.*.Output Gated Clock is toggling when not required.*/',
                #'/^.*.Output Gated Clock should toggle while in Reset.*/',
                #'/^.*.ASSERT_ONE_HOT_currAvailEntries.*/',
                #'/^.*.ASSERT_ONE_HOT_currEmptyEntries.*/',
                #'/^.*.ASSERT_LRU_BotOfTLB_ONEHOT.*/',
                #'/^.*.ASSERT_LRU_TopOfQueue_ONEHOT.*/',
                #'/^.*.ASSERT_NEVER_att_size_ext_hit.*/',
                #'/^.*.ASSERT_NEVER_att_size_ext_0.*/',
                #'/^.*.ASSERT_NEVER_att_size_ext_1.*/',
                #'/^.*.Input Packed Vector is not Soft-Mutex.*/',
                #'/^.*.currAvailEntriesMutex should always be MUTEX \{Weak MUTEX\}.*/',
                #'/^.*.currEmptyEntriesMutex should always be MUTEX \{Weak MUTEX\}.*/',
                #'/^.*.BotOfTLB pointer have no bit asserted.*/',
                #'/^.*.TopOfQueue pointer have no bit asserted.*/',
                #'/^.*.Overall Hit Vector for 128 Enteris must always be Soft-Mutex.*/',
                #'/^.*.Attempted write to more than one CR at once. CR Address Decoding error.*/',
                #'/^.*.Attempted read from more than one CR at once. CR Address Decoding error.*/',
                #'/^.*.Write cannot only be enabled for High & Low on same cycle. Decoding Error.*/',
                #'/^.*.Transaction cannot hit both Linear & Physical regions. Decoding Error.*/',
                #'/^.*.Invalid arc.*/',
                #'/^.*.Write enable should not be X.*/',
                #'/^.*.Read enable should not be X.*/',
                #'/^.*.Write address should not be X.*/',
                #'/^.*.Read address should not be X.*/',
                #'/^.*.hnqual should be asserted in the cycle before hclk\/nclk common edge.*/',
                #'/^.*.assert_rf_rd_before_wr:.*/',
                #'/^.*.assert_rf_rdwr:.*/',
                #'/^.*.ASSERT_NEVER_hnqual_assert:.*/',
                #'/^.*.addr_bound_chck.*/',
                #'/^.*.Not all Non-Posteds were completed by the end of simulation.*/',
                #'/^.*.tracker file is EMPTY.*/',
                #'/^.*.\:: Error count may be high due to empty trackers.*/',
                #'/^.*.\:: Error count may be high due to missing trackers.*/',
                #'/^.*._TRK.out NOT found.*/',
                #'/^.*.Postsim run failed for.*/',

                # sgogula edit

                # ssnanal edit

            ],


            okErrors_multiLine => [

                # akchandr edit

                # dteo edit

                # gerards edit

                # gkaragat edit

                # jjperry1 edit

                # klee18 edit

                # mdsibbel edit

                # mmdhanif edit

                # rkganti edit

                # sgogula edit

                # ssnanal edit

             ]

        },

        # ---------------------------------------------------------------------
        # Filters that apply only to DFT tests
        # ---------------------------------------------------------------------
        DftTempFilter => {
            Required     => 0,
            StartString  => '/target.dft_/',
            EndString    => '/# ACE SIM OUTPUT END/',

            okErrors => [

              # Mark edit
              # Owner: Mark Sibbel (mdsibbel)
              # Date : 03/09/2017
              # HSD  : No HSD Filed ... test debug output with error message
              # Info : Test debug output with error

              # George edit

              # Cathy edit

             ]
        },

        # ---------------------------------------------------------------------
        # Filters that apply to Visa XML Parser
        # Owner :   Cathy Tanbonliong
        # Date:     4/29/2016
        # Bug:      No bug
        #Info :     This mode is to support visa parser runs
        # ---------------------------------------------------------------------
        VisaParserMode => {
            Required     => 0,
            StartString  => '/ACE_ERR_FILTER\: VISA PARSER FLOW/',
            RequiredText => [
            ],

            okErrors => [
                
            ],
        },


                 

        # -------------------------------------------------------------------------------------------
        # TEST CASE SPECIFIC FILTER section begins
        # -------------------------------------------------------------------------------------------
        # <Short description of filter, example: Filters that apply for reset tests>
        # Owner:
        # Date :
        # HSD  : <Include HSD information here or within the body of the filter mode>
        # Info :
#        MODE_HSD123 => {
#            Required     => 0,
#            StartString  => '/ACE_ERR_FILTER: TEST_CASE_SPECIFIC BEGIN/',
#            EndString    => '/ACE_ERR_FILTER: TEST_CASE_SPECIFIC END/',
#
#            okErrors => [
#
#                # Owner:
#                # Date :
#                # HSD  :
#                # Info :
#
#            ],
#
#        },
        # <Short description of filter, example: Filters that apply for reset tests>


    "AceLintra_default" => {
             Ignore      => 1,
             Required     => 0,
             StartString  => '/ACE SIM OUTPUT START/',
             EndString    => '/ACE SIM OUTPUT END/',
             RequiredText => [
                              '/RC = 0/',
                              '/Lint status PASSED/',
                             ],
             okErrors     => [
                              '/severity: Error/',
                             ],
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
        [ '/FATAL/',                             "ALL",      3       ],
        [ '/ failed at /',                       "ALL",      2       ],
        [ '/NON-ZERO return code/',              "ALL",      2       ],
        #[ '/FATAL/',                             "ALL",      3       ],
    ],

    #okErrors => [                                                       
    #    '/^.*exclusive_monitor_error_control_reg.*$/',                  
    #    '/^.*error_response_policy.*$/',                                
    #    '/^.*allow_cache_update_on_coherent_error_response.*$/',        
    #    '/^.*flag_cacheline_and_memory_coherency_check_per_xact_as_error.*$/',
    #    '/^.*svt_axi_decode_error_response_ictest_sequence.*$/',
    #    '/^.*excl_mon_error_control_reg.*$/',
    #    '/^.*Error: .*fc_hdl_top.dut.parimc.parimcdfxleg.cltapc_mdu.cltapc_inst.i_cltapc_exi.chk_tappris_trst_exi_routing_mode0: at .*$/',
    #    '/^.*fc_hdl_top.dut.parimc.parimcdfxleg.cltapc_mdu.cltapc_inst.i_cltapc_exi.chk_tappris_trst_exi_routing_mode0: started at .*$/',
    #    '/^.*imc_cfg_trk.out:.*$/',
    # ],

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
