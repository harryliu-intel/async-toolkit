#!/usr/intel/bin/tclsh8.4

## -------------------------------------------------------------------------
## -- Project: Dunlap Creek (DLC)
## -- Author:  rjstach
## -- Intel Proprietary
## -- Copyright (C) 2017 Intel Corporation
## -- All Rights Reserved
##
## DLC FC Collage UPF configuration file. 
## All assumptions and issues listed as TBD below
## Notes:
## 
##
##
## -------------------------------------------------------------------

##--------------------------------------------------------------------##
## source DLC upf configuration
## The cfg file contains all cell mappings, voltage and supply port names
##--------------------------------------------------------------------##
source $::env(MODEL_ROOT)/tools/collage/configs/config_soc/integ_specs/upf/soc_upf.cfg

##--------------------------------------------------------------------##
## SOC power domain power island
## All IP are supposed to isolated from their gated domain. 
## All IP which are in SOC power domain are listed below as Collage need port power rail information for set_related_supply
##--------------------------------------------------------------------##
set fw_en ""
power_island vinf_island "$SOC_VINF_PORT {ext_off $SOC_VINF_NOM}" "" "" $fw_en $fw_en $fw_en $fw_en {
ec_top
#  par_clk               
ccu_top                 
cgu
#par_dfxmdu                
dfxagg_top
#par_dts
tsanadigldo_0
tsanadigldo_1
tsanadigldo_2
tsanadigldo_3
#par_fuse                 
fuse_hip
fuse_top
#par_gpio                
gpcommstunit_wrapper
#par_npk
northpeak
sip_iosf_tam_top_wrapper1
#par_pcie0                
#par_pcie1                
#par_pcie2                 
#par_pcie3                 
#par_psfnorth
psf0
#par_psfsouth
psf1
#par_sbr               
sbr_gp0
sbr_gp1
sbr_gp2
#paremp               
pmu_wrapper1
}

##++++++++++++++++++++++++++++++++++++++++++##
## SOC supply port information
## Information extracted from DLC pin specification spreadsheet
## soc_supply_ground.txt
##++++++++++++++++++++++++++++++++++++++++++##
pin_power_spec \
soc \
hip \
$::env(MODEL_ROOT)/tools/collage/configs/config_soc/integ_specs/upf/soc_pin_power_spec.txt
##++++++++++++++++++++++++++++++++++++++++++##
default_island dlc          vinf_island
default_island soc          vinf_island
default_island parcgu       vinf_island
default_island parfuse      vinf_island
default_island pargpio      vinf_island
default_island parpcie      vinf_island
default_island parfivr      vinf_island
default_island parchannel   vinf_island
default_island parmain      vinf_island


##--------------------------------------------------------------------------##
## DSW power island
##--------------------------------------------------------------------------##
## int_dsw_pok to be used as the enable (available in parleg)
## This section captures the isolation control signals
## PRIM to RTC uses int_pwell_pok_rtc
## DSW to RTC uses int_dsw_pok
#proc isolation_override_hook {parname from_supply to_supply} {
#   ::upf::print_msg "### Debug -- Ray: $parname $from_supply $to_supply"
#
#   if {$parname == "parleg"} {
#      if {($from_supply == "vnnprim") && ($to_supply == "vccdsw_1p0")} {
#          return "int_pwell_pok_dsw"
#      }
#   }
#   if {$parname == "parrtc"} {
#      if {($from_supply == "vnnprim") && ($to_supply == "vccrtcwell_1p5")} {
#          return "parrtc_rtcwell_wrapper/pmcrtcunit_wrapper1/int_pwell_pok_rtc"
#      }
#      if {($from_supply == "vccdsw_1p0")  && ($to_supply == "vccrtcwell_1p5")} {
#          return "parrtc_rtcwell_wrapper/pmcrtcunit_wrapper1/int_dsw_pok"
#      }
#   }
#}
#
#set fw_en ""
#power_island pd_dsw "$DSW_SUPPLY_PORT {ext_off $DSW_SUPPLY_NOM}" "" "" $fw_en $fw_en $fw_en $fw_en {
#   parleg_dswwell_wrapper
#}

##--------------------------------------------------------------------------##
## RTC power island
##--------------------------------------------------------------------------##
#set fw_en "pmcrtcunit_wrapper1/int_pwell_pok_rtc"
#power_island pd_rtc "$RTC_SUPPLY_PORT {ext_off $RTC_SUPPLY_NOM}" "" "" $fw_en $fw_en $fw_en $fw_en {
#parrtc_module1
#parrtc_module2
#etc...
#}

##--------------------------------------------------------------------------##
## <SOC>_IO_WRAPPER power island
##--------------------------------------------------------------------------##
#power_island pd_rtc_3p3 "vccprtc_3p3 {ext_off 3.09}" "" "" "" "" "" "" {
#
#}

##--------------------------------------------------------------------------##
## Virtual IP is to move up ELS + LS insertion to wrapper levels instead of
## individual DSW/RTC IP to avoid conflicts + multiple well fanout
## signal resolution
##--------------------------------------------------------------------------##


##--------------------------------------------------------------------------##
## RTC - DSW - PRIMARY
## RTC - Deepest (Last to be turned off)
## PRIMARY - Shallowest
##--------------------------------------------------------------------------##
isolation_precedence "$RTC_SUPPLY_PORT $DSW_SUPPLY_PORT $SOC_SUPPLY_PORT"

##--------------------------------------------------------------------------##
# This is for all the signals being driven out of rtcwell
# isolation_location driving_partition driving_supply receiving_partition receiving_supply [self|parent]_[driver|receiver]
##--------------------------------------------------------------------------##
#isolation_location     parrtc $RTC_SUPPLY_PORT * * self_driver
#level_shifter_location parrtc $RTC_SUPPLY_PORT * * self_driver

# This is for all the signals being driven into rtcwell
#isolation_location     * * parrtc $RTC_SUPPLY_PORT self_receiver
#level_shifter_location * * parrtc $RTC_SUPPLY_PORT self_receiver
##--------------------------------------------------------------------------##

##--------------------------------------------------------------------------##
# This is for all the signals being driven out of dswwell
# isolation_location driving_partition driving_supply receiving_partition receiving_supply [self|parent]_[driver|receiver]
##--------------------------------------------------------------------------##
#isolation_location     * $DSW_SUPPLY_PORT * $SOC_SUPPLY_PORT self_driver
#level_shifter_location * $DSW_SUPPLY_PORT * $SOC_SUPPLY_PORT self_driver

# This is for all the signals being driven into dswwell
#isolation_location     * $SOC_SUPPLY_PORT * $DSW_SUPPLY_PORT self_receiver
#level_shifter_location * $SOC_SUPPLY_PORT * $DSW_SUPPLY_PORT self_receiver

# Duplicate the level iso/ls above for vccdsw_1p0
##isolation_location     * vccdsw_1p0 parleg $SOC_SUPPLY_PORT self_driver
##level_shifter_location * vccdsw_1p0 parleg $SOC_SUPPLY_PORT self_driver
##isolation_location     parleg $SOC_SUPPLY_PORT * vccdsw_1p0 self_receiver
##level_shifter_location parleg $SOC_SUPPLY_PORT * vccdsw_1p0 self_receiver
##--------------------------------------------------------------------------##

##--------------------------------------------------------------------------##
## List all variables that will be seen by Collage and substituted inside partition UPF files
##--------------------------------------------------------------------------##
set UPF_GLOBAL_VARS {
    PS_CELL
    PS_CELL_UNGATED_IN
    PS_CELL_GATED_OUT
    PS_CELL_ENABLE
    PS_CELL_ACK
    LS_HIGH_TO_LOW_THRESHOLD
    LS_LOW_TO_HIGH_THRESHOLD
    LS_BOTH_THRESHOLD
    FW_AND_CELL
    FW_OR_CELL
    FW_CLOCK_CELL
    FW_CLOCK_PW_AND_CELL
    LS_LH_AND_S_CELL
    LS_LH_AND_D_CELL
    LS_HL_AND_S_CELL
    LS_HL_AND_D_CELL
    LS_LH_OR_S_CELL
    LS_LH_OR_D_CELL
    LS_HL_OR_S_CELL
    LS_HL_OR_D_CELL
    LS_LH_BUF_S_CELL
    LS_LH_BUF_D_CELL
    LS_HL_BUF_S_CELL
    LS_HL_BUF_D_CELL
    LS_LH_CLK_S_CELL
    LS_LH_CLK_D_CELL
    LS_HL_CLK_S_CELL
    LS_HL_CLK_D_CELL
}
## +++++++++++++ PST OVERRIDES ++++++++++++++++++++++++##

##--------------------------------------------------------------------------##
## Override PST table of PARRTC that is generated by default through Collage
## This is done to keep the combinations to only the legal ones and nothing else.
## Also, includes vccio_1p05 supply added to power intermediate LS
## vccio_1p05 is needed inside parrtc to related supply of set_related_supply_net
##--------------------------------------------------------------------------##
##proc generate_pst_parrtc {par fp} {
##    puts $fp [exec cat $::env(MODEL_ROOT)/tools/collage/configs/$::env(CHASSIS_ID)/soc/integ_specs/upf/parrtc_pst_table_override.txt]
##}

##--------------------------------------------------------------------------##
## Override PST table of PARLEG that is generated by default through Collage
## This is done to keep the combinations to only the legal ones and nothing else.
## Also, includes vccio_1p05 supply added to power intermediate LS
## vccio_1p05 is needed inside parrtc to related supply of set_related_supply_net
##--------------------------------------------------------------------------##
##proc generate_pst_parleg {par fp} {
##    puts $fp [exec cat $::env(MODEL_ROOT)/tools/collage/configs/$::env(CHASSIS_ID)/soc/integ_specs/upf/parleg_pst_table_override.txt]
##}

##--------------------------------------------------------------------------##
## Override PST table of spt_io_wrapper to include all the rails and legal cases only
## Done to allow splitting of PST table to enable review and ease of debug
##--------------------------------------------------------------------------##
##proc generate_pst_ebg_io_wrapper { par fp } {
##    puts $fp [exec cat $::env(MODEL_ROOT)/tools/collage/configs/$::env(CHASSIS_ID)/soc/integ_specs/upf/ebg_io_wrapper_pst_table_override.txt]
##}

##--------------------------------------------------------------------------##
## Override PST table of pch to include all the rails and legal cases only
## Done to allow splitting of PST table to enable review and ease of debug
## pch PST table and spt_io_wrapper PST table will be similar except for couple of exceptions
##--------------------------------------------------------------------------##
proc generate_pst_mst {top fp} {
    puts $fp [exec cat $::env(MODEL_ROOT)/tools/collage/configs/config_soc/integ_specs/upf/soc_pst_table_override.txt]
}


## +++++++++++++ PRE HOOKS ++++++++++++++++++++++++##
##--------------------------------------------------------------------------##
## Adds b12 library overrides for PARRTC. Rest of the partitions use b05 libraries.
## Also adds vccio_1p05 supply port to the UPF file.
## NOTE:
## 1. Only create_* commands are added through this step
## 2. add_port_state and create_pst addition of vccio_1p05 is done through
##    a. RTC: post hook in this file
##    b. overriding of PST for parleg during merge
##--------------------------------------------------------------------------##
#proc generate_par_pre_hook {par fp} {
#  set par_rtc_idx [lsearch -exact $par "parrtc"]
#  set par_leg_idx [lsearch -exact $par "parleg"]
#
#  if { ([lindex $par ${par_rtc_idx}] == "parrtc") } {
#      puts $fp [exec cat $::env(MODEL_ROOT)/tools/collage/configs/config_ebg/soc/integ_specs/upf/parrtc_global_defines.txt]
#  }
#}
#  if { ([lindex $par ${par_leg_idx}] == "parleg") } {
#      puts $fp [exec cat $::env(MODEL_ROOT)/tools/collage/configs/config_ebg/soc/integ_specs/upf/parleg_global_defines.txt]
#  }
#}

## +++++++++++++ POST HOOKS ++++++++++++++++++++++++##

##--------------------------------------------------------------------------##
## Controls turning off all assertions inside PGD across all partitions when associated domains are powered off.
## This list needs to be reviewed everytime domain names change
##
## Also, adds SNPS_reinit to pch.upf file which allows re-initialization of initial blocks on power on
## Especially needed for IO and EBB and not so much for the core.
##--------------------------------------------------------------------------##
##proc generate_top_post_hook {top fp} {
##  set top_idx [lsearch -exact $top "pch"]
##  if { ([lindex $top ${top_idx}] == "pch") } {
##      puts $fp [exec cat $::env(MODEL_ROOT)/tools/collage/configs/$::env(CHASSIS_ID)/soc/integ_specs/upf/pch_design_attributes.txt]
##  }
##}

## +++++++++++++ IP UPF specs +++++++++++++++++++++++++++++++++++++++++++++++##
## Provide IP vcc pin mapping and path to UPF for each partition IP
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
##
## SIP example
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
##
## SIP Example Start ##
## ip_upf_file \
## <ip_name> \
## "" \
## "$SOC_SUPPLY_PORT {ext_off $SOC_SUPPLY_NOM}  $SOC_SUPPLY_PORT
##  $SOC_GROUND_PORT {$GND_SUPPLY_NOM}          $SOC_GROUND_PORT
##  $SOC_SRAM_PORT   {ext_off 1.05}             $SOC_SRAM_PORT
## " \
## "" \
## $::env(MODEL_ROOT)/subIP/<subIP>_ROOT/tools/upf/<ip>.upf \
## ""
##
## SIP Example End ##

## HIP Example
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
## HIP Example Start##
##
## c73p1wmhybridx26familyew
## ##Load UPF
##ip_upf_file \
##hip_wm26 \
##"" \
##"vccmphy_1p05 {ext_off 0.94} vccaaon_1p05 
## vccapcie2pll_1p05 {ext_off 0.94} vcccl0on_1p05
## vccapcie3pll_1p05 {ext_off 0.94} vcccl1on_1p05
## vccasatapll_1p05 {ext_off 0.94} vcccl2on_1p05
## vccapcie2pllebb_1p05 {ext_off 0.94} vccclpllebb_1p05
## vnnprim {ext_off 0.75} vccsoc_aon_lv 
## $SOC_GROUND_PORT {$GND_SUPPLY_NOM} vss 
##" \
##"" \
##$::env(MODEL_ROOT)/subIP/hip_wm26_ROOT/rtl/upf/c73p1wmhybridx26familyew.upf \
##"hip"

## ##Load IP interface file
##pin_power_spec \
##hip_wm26 \
##hip \
##$::env(MODEL_ROOT)/source/gen/upf/hip_power_pins/c73p1wmhybridx26familyew/c73p1wmhybridx26familyew_supply_ground.txt
##++++++++++++++++++++++++++++++++++++++++++##
##
## HIP Example END ##


###############################################################################
## par_clk
###############################################################################

#cgu -- HIP  -- wait for drop....

ip_upf_file \
ccu_top \
"" \
"$SOC_VINF_PORT {ext_off $SOC_VINF_NOM}  $CCU_SUSCLK_AON1_SUPPLY
 $SOC_GROUND_PORT {$GND_SUPPLY_NOM}      $SOC_GROUND_PORT
" \
"" \
$::env(ccu_ROOT)/tools/upf/ccu_top.upf \
""


###############################################################################
## par_dfxmdu
###############################################################################

ip_upf_file \
dfxagg_top \
"" \
"$SOC_SUPPLY_PORT {ext_off $SOC_SUPPLY_NOM}  vcc
 $SOC_GROUND_PORT {$GND_SUPPLY_NOM} $SOC_GROUND_PORT
" \
"" \
$::env(dfx_agg_ROOT)/tools/upf/inputs/dfxagg/dfxagg.upf \
""

###############################################################################
## par_dts
###############################################################################

## tsanadigmld_0
## 
ip_upf_file \
tsanadigmld_0 \
"" \
"$SOC_SUPPLY_PORT {ext_off $SOC_SUPPLY_NOM} vccdig 
 $SOC_VSFR_PORT   {ext_off $SOC_VSFR_NOM}   vcc_ana
 $SOC_VINF_PORT   {ext_off $SOC_VINF_NOM}   vccaon
 $SOC_GROUND_PORT {$GND_SUPPLY_NOM}         vss  
" \
"" \
$::env(power_delivery_ROOT)/tools/rtl/upf/tsanadigmld.upf \
"hip"

## ##Load IP interface file
#pin_power_spec \
#tsanadigmld_0 \
#hip \
#$::env(MODEL_ROOT)/source/gen/upf/hip_power_pins/tsanadigmld/tsanadigmld_supply_ground.txt

##=============================================================================

## tsanadigmld_1
## 
ip_upf_file \
tsanadigmld_1 \
"" \
"$SOC_SUPPLY_PORT {ext_off $SOC_SUPPLY_NOM} vccdig 
 $SOC_VSFR_PORT   {ext_off $SOC_VSFR_NOM}   vcc_ana
 $SOC_VINF_PORT   {ext_off $SOC_VINF_NOM}   vccaon
 $SOC_GROUND_PORT {$GND_SUPPLY_NOM}         vss
" \
"" \
$::env(power_delivery_ROOT)/tools/rtl/upf/tsanadigmld.upf \
"hip"

## ##Load IP interface file
#pin_power_spec \
#tsanadigmld_1 \
#hip \
#$::env(MODEL_ROOT)/source/gen/upf/hip_power_pins/tsanadigmld/tsanadigmld_supply_ground.txt

##=============================================================================

## tsanadigmld_2
## 
ip_upf_file \
tsanadigmld_2 \
"" \
"$SOC_SUPPLY_PORT {ext_off $SOC_SUPPLY_NOM} vccdig 
 $SOC_VSFR_PORT   {ext_off $SOC_VSFR_NOM}   vcc_ana
 $SOC_VINF_PORT   {ext_off $SOC_VINF_NOM}   vccaon
 $SOC_GROUND_PORT {$GND_SUPPLY_NOM}         vss  
" \
"" \
$::env(power_delivery_ROOT)/tools/rtl/upf/tsanadigmld.upf \
"hip"

## ##Load IP interface file
#pin_power_spec \
#tsanadigmld_2 \
#hip \
#$::env(MODEL_ROOT)/source/gen/upf/hip_power_pins/tsanadigmld/tsanadigmld_supply_ground.txt

##=============================================================================

## tsanadigmld_3
## 
ip_upf_file \
tsanadigmld_3 \
"" \
"$SOC_SUPPLY_PORT {ext_off $SOC_SUPPLY_NOM} vccdig 
 $SOC_VSFR_PORT   {ext_off $SOC_VSFR_NOM}   vcc_ana
 $SOC_VINF_PORT   {ext_off $SOC_VINF_NOM}   vccaon
 $SOC_GROUND_PORT {$GND_SUPPLY_NOM}         vss 
" \
"" \
$::env(power_delivery_ROOT)/tools/rtl/upf/tsanadigmld.upf \
"hip"

## ##Load IP interface file
#pin_power_spec \
#tsanadigmld_3 \
#hip \
#$::env(MODEL_ROOT)/source/gen/upf/hip_power_pins/tsanadigmld/tsanadigmld_supply_ground.txt


###############################################################################
## par_fuse
###############################################################################
## 
ip_upf_file \
fuse_hip \
"" \
"$SOC_FUSE_PORT   {ext_off $SOC_FUSE_NOM}   $VCCSTL 
 $SOC_FUSE_PORT   {ext_off $SOC_FUSE_NOM}   $VCCSTG0
 $SOC_FUSEHV_PORT {ext_off $SOC_FUSEHV_NOM} $VCCFPGM0
 $SOC_FUSEHV_PORT {ext_off $SOC_FUSEHV_NOM} $VCCFPGM1
 $SOC_GROUND_PORT {$GND_SUPPLY_NOM}         $SOC_GROUND_PORT  
" \
"" \
$::env(fuse_hip_ROOT)/rtl/upf/fuse_hip_wrapper.upf \
"hip"


## ##Load IP interface file
#pin_power_spec \
#fuse_hip_wrapper \
#hip \
#$::env(MODEL_ROOT)/source/gen/upf/hip_power_pins/fuse_hip_wrapper/fuse_hip_wrapper_supply_ground.txt

##=============================================================================

ip_upf_file \
fuse_top \
"" \
"$SOC_VINF_PORT   {ext_off $SOC_VINF_NOM}   $FUSE_TOP_AON_PORT
 $SOC_SUPPLY_PORT {ext_off $SOC_SUPPLY_NOM} $FUSE_TOP_VNN_PORT
 $SOC_GROUND_PORT {$GND_SUPPLY_NOM}         $SOC_GROUND_PORT
" \
"" \
$::env(fuse_ROOT)/tools/upf/TGL/fuse_top.upf \
""

###############################################################################
## par_gpio
###############################################################################
#NO UPF AVAILABLE:10/27/17

ip_upf_file \
gpcommstunit_wrapper \
"" \
"$SOC_VINF_PORT   {ext_off $SOC_VINF_NOM} $SOC_VINF_PORT
 $SOC_GROUND_PORT {$GND_SUPPLY_NOM}       $SOC_GROUND_PORT
" \
"" \
"" \
""


###############################################################################
## par_npk
###############################################################################

ip_upf_file \
northpeak \
"" \
"$SOC_SUPPLY_PORT {ext_off $SOC_SUPPLY_NOM} $VCCNPK
 $SOC_GROUND_PORT {$GND_SUPPLY_NOM}         $SOC_GROUND_PORT
" \
"" \
$::env(npk_ROOT)/tools/upf/$::env(NPK_SIP_VARIATION)/northpeak.upf \
""

##==========================
#NO UPF AVAILABLE: 10/27/17
ip_upf_file \
sip_iosf_tam_top_wrapper1 \
"" \
"$SOC_SUPPLY_PORT {ext_off $SOC_SUPPLY_NOM} $SOC_SUPPLY_PORT
 $SOC_GROUND_PORT {$GND_SUPPLY_NOM}         $SOC_GROUND_PORT
" \
"" \
"" \
""

###############################################################################
## par_pcie0
###############################################################################
 ##stub as of 10/27/17
##==========================
##==========================

###############################################################################
## par_pcie1
###############################################################################
 ##stub as of 10/27/17
##==========================
##==========================

###############################################################################
## par_pcie2
###############################################################################
 ##stub as of 10/27/17
##==========================
##==========================

###############################################################################
## par_pcie3
###############################################################################
 ##stub as of 10/27/17
##==========================
##==========================

###############################################################################
## par_psfnorth
###############################################################################

ip_upf_file \
psf0 \
"" \
"$SOC_SUPPLY_PORT {ext_off $SOC_SUPPLY_NOM} $SOC_SUPPLY_PORT
 $SOC_GROUND_PORT {$GND_SUPPLY_NOM}         $SOC_GROUND_PORT
" \
"" \
$::env(psf_ROOT)/tools/upf/configs/dlc/psf20_dlc_psf0.upf \
""

##==========================
##==========================

###############################################################################
## par_psfsouth
###############################################################################

ip_upf_file \
psf1 \
"" \
"$SOC_SUPPLY_PORT {ext_off $SOC_SUPPLY_NOM} $SOC_SUPPLY_PORT
 $SOC_GROUND_PORT {$GND_SUPPLY_NOM}         $SOC_GROUND_PORT
" \
"" \
$::env(psf_ROOT)/tools/upf/configs/dlc/psf20_dlc_psf1.upf \
""

##==========================
##==========================

###############################################################################
## par_sbr
###############################################################################

ip_upf_file \
sbr_gp0 \
"" \
"$SOC_SUPPLY_PORT {ext_off $SOC_SUPPLY_NOM} $SOC_SUPPLY_PORT
 $SOC_GROUND_PORT {$GND_SUPPLY_NOM}         $SOC_GROUND_PORT
" \
"" \
$::env(sbr_ROOT)/tools/upf/router/sbr_config_ipxact_sbr_network_workspace_sbr0_gp_top.upf \
""

##=============================================================================

ip_upf_file \
sbr_gp1 \
"" \
"$SOC_SUPPLY_PORT {ext_off $SOC_SUPPLY_NOM} $SOC_SUPPLY_PORT
 $SOC_GROUND_PORT {$GND_SUPPLY_NOM}         $SOC_GROUND_PORT
" \
"" \
$::env(sbr_ROOT)/tools/upf/router/sbr_config_ipxact_sbr_network_workspace_sbr1_gp_top.upf \
""

##=============================================================================

ip_upf_file \
sbr_gp2 \
"" \
"$SOC_SUPPLY_PORT {ext_off $SOC_SUPPLY_NOM} $SOC_SUPPLY_PORT
 $SOC_GROUND_PORT {$GND_SUPPLY_NOM}         $SOC_GROUND_PORT
" \
"" \
$::env(sbr_ROOT)/tools/upf/router/sbr_config_ipxact_sbr_network_workspace_sbr2_gp_top.upf \
""


###############################################################################
## paremp
###############################################################################
ip_upf_file \
pmu_wrapper1 \
"" \
"$SOC_VINF_PORT   {ext_off $SOC_VINF_NOM}   vnnaon
 $SOC_GROUND_PORT {$GND_SUPPLY_NOM}         $SOC_GROUND_PORT
" \
"" \
$::env(pmu_ROOT)/tools/upf/pmu_top.upf \
""

##==========================
##==========================

###############################################################################
## <partition?>
###############################################################################

##==========================
##==========================


#####################################################################################################
## Miscellaneous Collage UPF Configs
#####################################################################################################

## Name file containing set_related_supply_nets as <BLOCK>_collage_upf_extension.tcl
rename_set_voltage 1

### Create IP_ENABLE and CSN_ENABLE Configurations
enable_csn 1
set UPF_GEN_DC_IP_ENABLE 1

## Disable set_voltage commands
disable_set_voltage

## Not using upf_global_defs.tcl
set UPF_GLOBAL_VARS ""

## TOP pin_power_spec is the highest priority when determining TOP and PARTITION SRSNs
## (consider removing when all HIP .lib files are available)
top_pin_spec_priority 1
