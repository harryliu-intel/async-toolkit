##############################################################################
# Configuration values for various options
##############################################################################

#############################
# Name of design / top module
#############################
set design "soc"
set design_inst "soc"
set ::env(COLLAGE_DESIGN) ${design}

##############################################################################
global psf_names sbr_names
set psf_names "psf0 psf1"
set sbr_names "sbr0 sbr1 sbr2 sbr3 sbr4 sbr5 sbr6 sbrproxy "
set ::collage_tb::sbr_subsystems "iosf_sb"
set ::collage_tb::sbr_subsystems_prefix(iosf_sb) "chs"

##############################################################################
set ::collage_fabric::psf_version 2

# This enables the N:1 flow for mapping multiple RTL interfaces to one SV interface
# This must be set for both RTL and TB assembly
set ::collage_tb::tb_use_one_to_n 1

set chassis_config_id "$::env(MODEL_ROOT)" 

# This enables the N:1 flow for mapping multiple RTL interfaces to one SV interface
# This must be set for both RTL and TB assembly
set ::collage_tb::tb_use_one_to_n 1

set ::soc_collage_dir [file join $::env(MODEL_ROOT) "tools/collage/configs/config_soc"]
set ::soc_subsys_specs_dir [file join $soc_collage_dir "subsystems"]
set ::soc_repo_dir [file join $::env(MODEL_ROOT) "tools/collage/configs/config_soc"]
set ::soc_integ_specs_dir [file join $::soc_repo_dir "integ_specs"]
set ::soc_tb_specs_dir [file join $soc_repo_dir "tb_specs"]
set ::soc_integ_suffix ""

# <subsystem name> <subsystem dir, relative or absolute>
# subsystem name is useless, what is it for????
set ::soc_subsystems {
    epc	        epc
    cup         cup
    glbl_mgmt   glbl_mgmt
    mpp         mpp
    msh         msh
    rcf         rcf
#    collage collage
}

#System Verilog construct support
set ::use_vcs_parser 1

################################################################################
# TB configuration
################################################################################
# Need to have newer IOSF 1.1 signals supported in TB generation.
collage_tb_set_bfm_ver 1.1

#Disable IP test-bench mode as we are integrating IPs
#This generates the assigns for D/M only for fabric interfaces
set collage_tb::ip_tb 0

#This is needed to load the pmi_ifc Corekit instead of req_if Corekit
set collage_tb::pmi_ver "2.5"

# Auto install interface corekits
set ::collage_tb::disable_auto_install 0

# to make hierarchical connections on, set to 1 
# else to ip, set to 0
set ::collage_tb_hier_intf 0

# To add an include file per ti
# Only used for genereate blocks to avoid hier diff
set ::collage_tb::add_ti_adhoc_file 1

set ::collage_tb::emu_psf_ver "1.1"

# set the list pairs "<ip_interface> <intermediate_hier_intf>"
set ::collage_tb_user_mod_list {}

#To choose PSF2
set ::collage_fabric::psf_version "2" ; 

# Use "generate" instead of ifdef in the testbench
set ::collage_tb::use_generate 0

# Enable usage of Wdu to add wires to DUT pins
set ::collage_tb::use_custom_dut_writer 1
set ::collage_tb::sbr_version 1.21
set ::collage_tb::gen_param_file 0

################################################################################
# Global options for sbx assembler
################################################################################
#set ::soc_insert_idv_chains 1
#set ::soc_insert_scan_clock 1
#set ::soc_propagate_io_constraints 1

