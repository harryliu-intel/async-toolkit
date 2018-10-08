#!/usr/intel/bin/perl

# ---------------------------------*-perl-*-------------------------------------
# Intel Top Secret, Copyright 2018 Intel Corporation, All Rights Reserved
#
# Purpose:          This file creates a list of associative arrayss for storing IP specific libraries &
#                   vlog opts that in turn will be used in HDL UDF file for model/library definition.
#                   Arrays available:
#                   %sip_rtl_libs        = stores SIP RTL libraries
#                   %sip_verif_libs      = stores SIP Verification libraries
#                   @HIP_RTL_LIBS        = stores HIP RTL libraries
#                   %sip_rtl_synth_opts  = stores SIP RTL specific synthesis define , use for backend synthesis
#                   %sip_rtl_vlog_opts   = stores SIP RTL specific vlog options
#                   %sip_verif_vlog_opts = stores SIP Verification specific vlog options
#                   @SIP_SHARED_LIB      = stores SIP shared verification sublibs
#
#                   This file also contains a section of code which allow IPs to be dynamically added
#                   to the basic boot components for defining 'lite' model (example lbgsd_pch_lite in
#                   LBG). This is achieved by adding -add_ip to the Ace input argument.
#                   Example:
#
#                   simbuild -s all +s ace -ace_args ace -cc -add_ip=boot,sata,gbe -ace_args-
#                   (in this case only three major components of the IPs are constructed: boot+sata+gbe)
#
#                   Separate lists are maintained for IPs, HIPs and the shared validation libs
#                   (sip_shared_lib sublibs).
#
# How to populate this file with new IPs :
#                   1. Section A: if applicable, populate %sip_list with a new IP key
#                   2. Section B: populate the hash table with a list of IP rtl libraries
#                   3. Section C: populate IP rtl related vlog opts. Example SATA_RTL_ENABLE. This option will
#                                 be useful in constructing model with stubbed RTL
#                   4. Section D: populate RTL synthesis related opts if applicable
#                   5. Section E: populate IP verification libraries. Example test island / verif env libraries
#                   6. Section F: populate verification related vlog opts. Example SATA_ENV_ENABLE
#                   7. Section G: sip_shared_lib declaration
#                   8. Section H: populate HIP RTL related libraries
#                   9. Section I: populate SIP specific ELAB OPTS
#                  10. Section J: initializing a set of arrays which captured information as populated from Section A
#                                 through section F. These arrays will ultimately be used during FC compilation
# ------------------------------------------------------------------------------
package fc_global_vars;

use Exporter;
use lib "$ENV{RTL_PROJ_BIN}/perllib";
use ToolConfig;
use Switch;
use common::ace_lib_utils;

@ISA = qw(Exporter);
@EXPORT = qw(get_sip_rtl_libs get_all_sip_rtl_libs get_all_sip_verif_libs get_sip_verif_libs get_all_sip_rtl_vlogopts get_all_sip_verif_vlogopts get_all_sip_elab_opts get_func_simv_opts get_hip_liblist get_all_sip_rtl_synthopts get_top_vrf_vlogopts get_nlp_elab_opts get_nlp_sim_opts);
my $debug = 0;

$VT_TYPE = &ToolConfig::get_facet("vt_type");
$CTECH_LIB_NAME = ($VT_TYPE eq "") ?  "CTECH_v_rtl_lib" : "CTECH_p1274_ec0_$VT_TYPE"."_rtl_lib";
$CTECH_EXP_LIB_NAME = ($VT_TYPE eq "") ?  "CTECH_EXP_v_rtl_lib" : "CTECH_EXP_p1274_ec0_$VT_TYPE"."_rtl_lib";

$IP_STUB_LIB = "soc_ip_stub_lib";
$COLLAGE_LIB = (&ToolConfig::get_facet("dut_type") eq "upf") ? "soc_collage_assemble_upf_lib" : "soc_collage_assemble_lib";

#################################################################
# Section A:
# <<< Hash table whose key defines a list of IP groups supported
#################################################################
my %sip_list = (
    #'boot'       => ['amba','axi','apb','chi', 'igr'],
    'boot'       => ['igr'],

);
# >>>

#############################################################
# Section B:
# <<< SIP RTL LIBRARIES
#############################################################
# if adding new keys, make sure they are added to all hashes
# HIP related lib should be move to HIP_RTL_LIBS (NON-SYNTH related)
# SIP rtl lib should contain only synthesizable code (SYNTH related )
my %sip_rtl_libs = (
    'boot'               => [
                            ],
);

sub get_sip_rtl_libs {
    my @libs = ();
    my @rtl_libs = ();
    foreach my $ip (@_) {
      print "$ip: @{$sip_rtl_libs{$ip}}\n" if ($debug);
      push(@libs, @{$sip_rtl_libs{$ip}});
    }
    return @libs;
}

## >>> SIP RTL LIBRARIES

#############################################################
# Section C:
# <<< RTL VLOG OPTS
#############################################################
my %sip_rtl_vlog_opts = (
    'boot'               => ["+define+PMU_RTL_ENABLE",
                             ],
); 

sub get_rtl_vlog_opts {
    my @rtl_vlog_opts = ();
    foreach my $ip (@_) {
      print "$RTL vlog_opts: @{$sip_rtl_vlog_opts{$ip}}\n" if ($debug);
      push(@rtl_vlog_opts, @{$sip_rtl_vlog_opts{$ip}});
    }
    return @rtl_vlog_opts;
}

# >>> RTL VLOG OPTS

#############################################################
# Section D:
# <<< RTL SYNTHESIS OPTS
# SVA_OFF , NO_PWR_PINS and DC will be on by default.
#############################################################
my %sip_rtl_synth_opts = (
    'boot'               => [
                             ],
);

sub get_rtl_synth_opts {
    my @rtl_synth_opts = ();
    foreach my $ip (@_) {
      print "$RTL synth_opts: @{$sip_rtl_synth_opts{$ip}}\n" if ($debug);
      push(@rtl_synth_opts, @{$sip_rtl_synth_opts{$ip}});
    }
    return @rtl_synth_opts;
}

sub gen_liblist {
    my %VISA_LIB;
    system("rm $ENV{MODEL_ROOT}/cfg/ace/fc_liblist.svh");
    open(LIBLIST,">>$ENV{MODEL_ROOT}/cfg/ace/fc_liblist.svh");
    open(VISA,"$ENV{MODEL_ROOT}/cfg/ace/visa_inserted_library.cfg");
    while(<VISA>){
        next if $_ =~/^#/;
        next if $_ =~/^$/;
        chomp($_);
        ($ori_name,$visa_name)=split(/,/,$_);
        $VISA_LIB{$ori_name}=$visa_name;
    }
    foreach $IP (keys %{sip_rtl_libs}) {
        print LIBLIST "\n`define $IP\_LIBLIST";
        #sorting lib , PUNI lib has higher priorty
        @PUNI=();
        @NONPUNI=();
        @TMP=();
        #workaround for USBX library order in spyglass
        if($IP =~ /usb/){
            @TMP=reverse(@{$sip_rtl_libs{$IP}});
        }else{
            @TMP=@{$sip_rtl_libs{$IP}};
        }
        foreach $library (@TMP){
            if ($library =~ /PUNI/i){
                push(@PUNI,$library);
            }else{
                push(@NONPUNI,$library);
            }
        }
        foreach $library (@PUNI){
            if(defined $VISA_LIB{$library}){
                print LIBLIST " $VISA_LIB{$library} $library";
            }else{
                print LIBLIST " $library";
            }
        }
        foreach $library (@NONPUNI){
            if(defined $VISA_LIB{$library}){
                print LIBLIST " $VISA_LIB{$library} $library";
            }else{
                print LIBLIST " $library";
            }
        }
    }
    print LIBLIST "\n`define global_LIBLIST fc_top_lib";
    foreach $IP (keys %{sip_rtl_libs}) {
        print LIBLIST " `$IP\_LIBLIST";
    }
    close(LIBLIST);
}

# >>> RTL SYNTHESIS OPTS

#############################################################
# Section E:
# <<< SIP VERIF LIBRARIES
#############################################################
my %sip_verif_libs = (
    'boot'               => ["mby_ingress_ti_lib", "mby_ingress_env_lib"
                             ],
);

sub get_sip_verif_libs {
    my @libs = ();
    foreach my $ip (@_) {
      print "$ip: @{$sip_verif_libs{$ip}}\n" if ($debug);
      push(@libs, @{$sip_verif_libs{$ip}});
    }
    return @libs;
}
# >>> SIP VERIF LIBRARIES

#############################################################
# Section F:
# <<< SIP VERIF VLOG OPTS
#############################################################
my %sip_verif_vlog_opts = (
    'boot'               => [ 
                              "+define+PMU_ENV_ENABLE",
                             ],
);

sub get_verif_vlog_opts {
    my @verif_vlog_opts = ();
    foreach my $ip (@_) {
      print "$vlog_opts: @{$sip_verif_vlog_opts{$ip}}\n" if ($debug);
      push(@verif_vlog_opts, @{$sip_verif_vlog_opts{$ip}});
    }
    return @verif_vlog_opts;
}
# >>> SIP VERIF VLOG OPTS

############################################################
# Section G:
# <<< SIP_SHARED_LIB
############################################################
our @SIP_SHARED_LIB = (
);

sub get_sip_shared_lib_list {
    return (@SIP_SHARED_LIB),
}

# >>> SIP_SHARED_LIB

#############################################################
# Section H:
# <<< HIP RTL LIBRARIES
#############################################################
our @HIP_RTL_LIBS = (
);

sub get_hip_liblist {
    return (@HIP_RTL_LIBS),
}

sub gen_hip_liblist {
    system("rm $ENV{REPO_ROOT}/cfg/ace/fc_hip_liblist.svh");
    open(LIBLIST,">>$ENV{REPO_ROOT}/cfg/ace/fc_hip_liblist.svh");
    my $liblist ="";
    foreach $hip (@HIP_RTL_LIBS){
        $liblist .= "$hip ";
    }
    print LIBLIST "`define global_HIP_LIBLIST $liblist";
    close(LIBLIST);
}

# >>> HIP RTL LIBRARIES

#############################################################
## Section I:
## <<< SIP specific ELAB OPTS
##############################################################
my %sip_elab_opts = (
#   'pcie'              => [
#                              # required .so file for pcie_bfm
#                              ("-iosf_pri_enable" ~~ @ARGV) ? "" : &ToolConfig::ToolConfig_get_tool_path('ipconfig/pcie_bfm_sv')."/results/vcs_lib/pcie_bfm_sv_lib/pcie_bfm_sv/Linux-SuSE/x86-64/libpcie.so",
#                          ],                           
);

sub get_sip_elab_opts {
    my @libs = ();
    my @rtl_libs = ();
    foreach my $ip (@_) {
      print "$ip: @{$sip_elab_opts{$ip}}\n" if ($debug);
      push(@libs, @{$sip_elab_opts{$ip}});
    }
    return @libs;
}
# >>> SIP specific ELAB OPTS

############################################################
# Section J:
# <<< Process to add IPs for compilation
############################################################
#Determine the IP to be added
our @ace_add_ips = &add_ip();

print ("INFO: IPs to add = @ace_add_ips \n");
#Parse the list against the known IPs:

## Collect list of IPs to stub out from the command line
our @ace_stub_ips = &ip_to_stub();
print ("INFO: IPs to stub = @ace_stub_ips \n") if (@ace_stub_ips[0] ne "no_stub");

## Identify the stub mode
our $stub_mode = &get_stub_mode();
print ("INFO: stub mode = $stub_mode \n");

my @ALL_SIP_RTL_LIBLIST = ();
my @ALL_SIP_ELAB_OPTS = ();
my @ALL_SIP_RTL_VLOGOPTS = ();
my @ALL_SIP_RTL_SYNTHOPTS = ();
my @ALL_SIP_VERIF_LIBLIST = ();
my @ALL_SIP_VERIF_VLOGOPTS = ();
# IP whose test island/env is enabled - mainly for FC moat script consumption
our @enabled_sip_list = ();

# Populate the bootable RTL/verif libraries & vlogopts

foreach our $ace_ip_to_add (@ace_add_ips) {

    switch ($ace_ip_to_add) {

        case ["boot"] {
                        # if RTL stub is not specified, ensure all RTLs are compiled
                        if (@ace_stub_ips[0] eq "no_stub" && $stub_mode eq "val") {
                            foreach $ip_key (keys %sip_list) {
                                populate_ip_rtl_attr("$ip_key");
                            }
                        } else {
                            populate_ip_rtl_attr("boot");
                        }
                        populate_ip_verif_attr("boot");
        }

        case ["mp0"] {
                        push (@ALL_SIP_RTL_VLOGOPTS, "+define+MPP_8 +define+EPC_8 +define+NO_SERDES",);

        }

        # Integrates all IPs
        # add new arrays here as well
        case ["all" , "no_stub"] {
                        foreach $ip_key (keys %sip_list) {
                            populate_ip_rtl_attr("$ip_key");
                            populate_ip_syn_attr("$ip_key");
                            populate_ip_verif_attr("$ip_key");
                        }
        }
        else {
            if (exists($sip_list{$ace_ip_to_add})) {
                populate_ip_rtl_attr("$ace_ip_to_add");
                populate_ip_verif_attr("$ace_ip_to_add");
            } else {
                print "IP_TO_ADD: ERROR - ip_to_add $ace_ip_to_add not found in switch statement in fc_global_var.pm. Check your spelling or add the IP to the sip_list\n";
            }
        }

    } # switch

    if ($stub_mode eq "rtl") {
        # Add pch_stub_lib option whenever RTL stub is attempted
        push (@ALL_SIP_RTL_LIBLIST, "$IP_STUB_LIB");
    }
} # foreach

sub get_all_sip_rtl_libs {
    return @ALL_SIP_RTL_LIBLIST;
}

sub get_all_sip_rtl_vlogopts {
    return @ALL_SIP_RTL_VLOGOPTS;
}

sub get_all_sip_rtl_synthopts {
    return @ALL_SIP_RTL_SYNTHOPTS;
}

sub get_all_sip_verif_libs {
    return @ALL_SIP_VERIF_LIBLIST;
}

sub get_all_sip_verif_vlogopts {
    return @ALL_SIP_VERIF_VLOGOPTS;
}

sub get_all_sip_elab_opts {
    return @ALL_SIP_ELAB_OPTS;
}

sub populate_ip_rtl_attr {
    my @ip_tmp = @_;
    my @tmp_arry = ();
    @tmp_arry = get_sip_rtl_libs(@ip_tmp);
    push (@ALL_SIP_RTL_LIBLIST, @tmp_arry);
    @tmp_arry = get_rtl_vlog_opts(@ip_tmp);
    push (@ALL_SIP_RTL_VLOGOPTS, @tmp_arry);
    @tmp_arry = get_sip_elab_opts(@ip_tmp);
    push (@ALL_SIP_ELAB_OPTS, @tmp_arry);
}


sub populate_ip_syn_attr {
    my @ip_tmp = @_;
    my @tmp_arry = ();
    @tmp_arry = get_rtl_synth_opts(@ip_tmp);
    push (@ALL_SIP_RTL_SYNTHOPTS, @tmp_arry);
}

sub populate_ip_verif_attr {
    my @ip_tmp = @_;
    my @tmp_arry = ();
    @tmp_arry = (@ip_tmp);
    @tmp_arry = get_sip_verif_libs(@ip_tmp);
    push (@ALL_SIP_VERIF_LIBLIST, @tmp_arry);
    @tmp_arry = get_verif_vlog_opts(@ip_tmp);
    push (@ALL_SIP_VERIF_VLOGOPTS, @tmp_arry);
    foreach my $ip (@_) {
           # Only the enabled SIP TI should have the moat check enabled
           push(@enabled_sip_list, @{$sip_list{$ip}});
    }
}

#####################################################################################
# IPs TO STUB
# Process ip_to_add switch to add IPs back into the Boot model  (Note: assumes Boot model)
# When adding IPs, add the corresponding RTL and verification libraries
#####################################################################################

#our @ip_list_to_stub = ();
#our @ip_vlog_to_stub = ();
our @hip_list_to_stub = ();

# ip_to_stub & add_ip are mutually exclusive
if (@ace_stub_ips[0] ne "no_stub" && @ace_add_ips[0] eq "no_stub")
{
    #if (@ace_stub_ips[0] eq "all_but_boot") {
    #    @ace_stub_ips = qw(csme cse_top cfmia ie rlink hdas usb sata smb gbe pcie dunit dts mcsmb hpsmb);
    #    foreach our $ace_ip_to_add (@ace_add_ips) {
    #        &remove_element_from_array($ace_ip_to_add,\@ace_stub_ips);
    #    }
    #    print "Final list of IPs : @ace_stub_ips >>>";
    #}


    foreach our $ace_ip_to_stub (@ace_stub_ips)
    {
        our @ip_rtl_list_to_stub  = get_sip_rtl_libs($ace_ip_to_stub);
        our @ip_rtl_vlog_to_stub = get_rtl_vlog_opts($ace_ip_to_stub);
        our @ip_vrf_list_to_stub  = get_sip_verif_libs($ace_ip_to_stub);
        our @ip_vrf_vlog_to_stub = get_verif_vlog_opts($ace_ip_to_stub);
        our @ip_elab_opts_to_stub = get_sip_elab_opts($ace_ip_to_stub);

        # Only the enabled SIP TI should have the moat check enabled
        &remove_element_from_array($ace_ip_to_stub,\@enabled_sip_list);

        foreach my $rtl_lib_to_remove (@ip_rtl_list_to_stub)
        {
            &remove_element_from_array($rtl_lib_to_remove,\@ALL_SIP_RTL_LIBLIST);
        }
        foreach my $rtl_vlog_to_remove (@ip_rtl_vlog_to_stub)
        {
            &remove_element_from_array($rtl_vlog_to_remove,\@ALL_SIP_RTL_VLOGOPTS);
        }
        foreach my $elab_opts_to_remove (@ip_elab_opts_to_stub)
        {
            &remove_element_from_array($elab_opts_to_remove,\@ALL_SIP_ELAB_OPTS);
        } 
        # No support to remove HIP
        #foreach our $hip_to_remove (@hip_list_to_stub)
        # {
        #     &remove_element_from_array($hip_to_remove,\@fc_boot_hip_list);
        #     &remove_element_from_array($hip_to_remove,\@fc_non_boot_hip_list);
        # }
        foreach my $vrf_lib_to_remove (@ip_vrf_list_to_stub)
        {
            &remove_element_from_array($vrf_lib_to_remove,\@ALL_SIP_VERIF_LIBLIST);
        }
        foreach my $vrf_vlog_to_remove (@ip_vrf_vlog_to_stub)
        {
            &remove_element_from_array($vrf_vlog_to_remove,\@ALL_SIP_VERIF_VLOGOPTS);
        }

    } #foreach ace_ip_to_stub

    # Add pch_stub_lib option whenever RTL stub is attempted
    push (@ALL_SIP_RTL_LIBLIST, "$IP_STUB_LIB");

}
# >>> Process to add IPs for compilation

# <<< Additional FC verificaton opts
our $hsopt = &get_hsopt();

our @TOP_VRF_VLOGOPTS = (
#klee18    $hsopt ? ():"+define+SIDEBAND_SB_ENABLE",
#    $hsopt ? ():"+define+EN_SB_VENUS",
);

sub get_top_vrf_vlogopts {
    return (@TOP_VRF_VLOGOPTS),
}

## <<< NLP (Native Low Power) Options

## NLP Elaboration Options
my @nlp_elab_opts = ();

if ($ENV{ENABLE_UPF_SIM}) {
    ## Write MV Report File for Debug
    push (@nlp_elab_opts, "-power=write_mvinfo");
    push (@nlp_elab_opts, "-power=attributes_on");
}

sub get_nlp_elab_opts {
    return (@nlp_elab_opts),
}

## NLP Simulation Options
my @nlp_sim_opts = ();

if ($ENV{ENABLE_UPF_SIM}) {
    ## Placeholder
    push (@nlp_sim_opts, "-power $ENV{MODEL_ROOT}/cfg/vcs/ebg_nlp.cfg");
}

sub get_nlp_sim_opts {
    return (@nlp_sim_opts),
}
## >>> NLP Options

# >>> Additional FC verificaton opts

#
#  ----------------
#    End of file
#  ----------------
#
# <<< VIM SETTINGS
# vim: ts=4 et
# >>>
1;
