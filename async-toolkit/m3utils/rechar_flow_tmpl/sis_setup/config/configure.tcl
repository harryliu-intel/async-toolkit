#++++++++++++++++++++++++++++++++
# configure.tcl 
# User Configuration File
#++++++++++++++++++++++++++++++++




#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# 0: Flow|Variable Initialization
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
    set corners		$::env(corner)
    
    set io_supplies	$::env(io_supplies)
    set io_grounds	$::env(io_grounds)

    if {[info exists ::env(core_supplies)]} {set core_supplies $::env(core_supplies)} else {set core_supplies ""}
    if {[info exists ::env(core_grounds)]}  {set core_grounds  $::env(core_grounds)}  else {set core_grounds  ""}

source $::env(SILICONSMART_ROOT_DIR)/tcl/base_procs.tcl

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# 1: Set 'Corner(s)' Info
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
    # Extract supply|ground Info from 'corner(s)'
    foreach corner $corners {
    	extract_corner_info $corner 

    	# Declare
    	create_operating_condition $corner
    	# P
	if {[info exists ::env(INTEL_PDK)]} {
	  set_opc_process $corner [subst {
	    {.lib '$::env(PROJECT_HSP_FILE)' $skew}
	    {.lib '$::env(PROJECT_HSP_INCLUDE)' $skew}
	    {$hspiceOptionsContent}
	  }]
	} else {
	  set_opc_process $corner [subst {
	    {.lib '$::env(PROJECT_HSP_FILE)' $skew}
	    {.inc $esd_model_lib_file}
	  }]
	}
	
    	# V
    	foreach core_supply	$core_supplies	{add_opc_supplies $corner $core_supply $vcccore}
    	foreach core_ground	$core_grounds	{add_opc_grounds  $corner $core_ground 0}
    	foreach io_supply	$io_supplies	{add_opc_supplies $corner $io_supply $vccio}
    	foreach io_ground	$io_grounds	{add_opc_grounds  $corner $io_ground 0}
    	# T
    	set_opc_temperature $corner $temp
    }

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# 2: Global Configuration File
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#if {[info exists ::env(INTEL_PDK)]} { source $::env(SILICONSMART_ROOT_DIR)/tcl/spf_post.tcl ; }
source $env(SILICONSMART_ROOT_DIR)/tcl/configure_init.tcl

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# 3: Global Cards [User Input Section]
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
define_parameters -append default {

    #--------------------------------------------------------------------------------------------------------#
    # Simulation Archive Controls
    set archive_condition_on_success yes		;# [yes|no] yes=if simulations pass, save them
    set archive_condition_on_failure yes		;# [yes|no] no=if simulations fail, save them
    #set remove_runtime_files 1				;# Set to 0 if archive_condition_on_success=yes

    #--------------------------------------------------------------------------------------------------------#
    #Choose Operating Condition(s)
    set active_pvts $::env(corner)

    #--------------------------------------------------------------------------------------------------------#
    # Supplies/Grounds to be Measured
    set power_meas_supplies [concat $io_supplies $core_supplies]
    set power_meas_grounds  [concat $io_grounds $core_grounds]
    if {[info exists ::env(INTEL_PDK)]} {
      set model_exclude_supplies {vss!}
    }

    #--------------------------------------------------------------------------------------------------------#
    # Job Scheduler Controls
    set job_scheduler standalone			;# [nb|standalone] nb=NetBatch
    set run_list_maxsize 16				;# '16' Tasks in Parallel
    # NetBatch Cards ('nbjob run' syntax) (Used if job_scheduler=nb)
    set normal_queue [subst { --target '$::env(NBPOOL)' --class '$::env(NBCLASS)' --qslot '$::env(NBQSLOT)' --class-reservation 'fVM=8000' --mail 10 --log-file /dev/null }]

    #--------------------------------------------------------------------------------------------------------#
    # Power/Leakage Controls

    #--------------------------------------------------------------------------------------------------------#
    # Capacitance Controls
    set enable_pwr_pincap 0				;# =1 generates *capacitance_power attributes

    #--------------------------------------------------------------------------------------------------------#
    # Constraint Controls
    set smc_constraint_style relative-degradation	;# [relative-degradation|pass-fail|relative-slew-degradation]
    #set_config_opt -type {hold removal} smc_constraint_style relative-slew-degradation
    set smc_degrade 0.01				;# =1% relative delay-degradation
    set smc_degrade_absolute 5e-12			;# =5ps absoulte delay-degradation
    #set smc_slew_degrade 0.5				;# =50% relative slew-degradation
    #set smc_slew_degrade_absolute 0			;# =0 absoulte slew-degradation
    set constraint_resolution 1e-12			;# =1ps Minimum Resolution of Search Window

    #--------------------------------------------------------------------------------------------------------#
    # CCSN Controls

    #--------------------------------------------------------------------------------------------------------#
    # LVF and Statistical Controls 
	# always use SBA method for constraint LVF generation
	set_config_opt -type { constraint mpw } lvf_ml_mode 0

    #--------------------------------------------------------------------------------------------------------#
    # Liberty Attributes

    #--------------------------------------------------------------------------------------------------------#
    # IBIS Controls
    set ibis_version 5				;# IBIS Version [4.1|4.2|5.0]
    set ibis_isso 1					;# =1 Generate ISSO (IBIS 5.0)
    set ibis_composite_current 1			;# =1 Generate Composite Current (IBIS 5.0)
    set ibis_clamping_iv_analysis_mode_dc 1		;# =1 DC Analysis for IV/Clamping Simulations; =0 Use Tran
    set ibischk_cmd "$::env(SILICONSMART_ROOT_DIR)/ibischk/linux_ubuntu_64/ibischk6_64"	;# Path to Golden Parser

    #--------------------------------------------------------------------------------------------------------#
    # Specific Requirements
    set subtract_pin_capacitance 0			;# =0 Dont Add PAD Capacitance to index_2 of ->PAD Arcs

    #--------------------------------------------------------------------------------------------------------#
    # Simulator Options
    # [Pre-SET in Global Configuration File, Un-comment and Modify if Needed]
    # set simulator_options {
    #     "common,finesim: finesim_mode=spicead finesim_method=gear cmiflag=1 cmiusrflag=3 pdmi=1 finesim_exitwarn="DC not converged""
    #     "common,hspice: runlvl=5 accurate=1 method=gear cmiflag=1 cmiusrflag=3 pdmi=1"
    # }

    #--------------------------------------------------------------------------------------------------------#
    # Simulation Resolution
    # [Pre-SET in Global Configuration File, Un-comment and Modify if Needed]
    # set time_res_high 1e-12				;# Simulator TSTEP

    #--------------------------------------------------------------------------------------------------------#
    # Functional Recognition/CCSN Pre-requisite
    # [Pre-SET in Global Configuration File, Un-comment and Modify if Needed]
    # set_config_opt bjt_model_names			[join "my_bjt [get_parameter bjt_model_names]"]
    # set_config_opt cap_model_names			[join "my_cap [get_parameter cap_model_names]"]
    # set_config_opt dio_model_names			[join "my_dio [get_parameter dio_model_names]"]
    # set_config_opt res_model_names			[join "my_res [get_parameter res_model_names]"]
    # set_config_opt nmos_model_names			[join "my_nmos [get_parameter nmos_model_names]"]
    # set_config_opt pmos_model_names			[join "my_pmos [get_parameter pmos_model_names]"]
    # set_config_opt nmos_drn_src_shorted_model_names	[join "my_nmos_drn_src_shorted [get_parameter nmos_drn_src_shorted_model_names]"]
    # set_config_opt pmos_drn_src_shorted_model_names	[join "my_pmos_drn_src_shorted [get_parameter pmos_drn_src_shorted_model_names]"]
    # set_config_opt nmos_drn_gate_shorted_model_names	[join "my_nmos_drn_gate_shorted [get_parameter nmos_drn_gate_shorted_model_names]"]
    # set_config_opt pmos_drn_gate_shorted_model_names	[join "my_pmos_drn_gate_shorted [get_parameter pmos_drn_gate_shorted_model_names]"]
    # set_config_opt nmos_gate_src_shorted_model_names	[join "my_nmos_gate_src_shorted [get_parameter nmos_gate_src_shorted_model_names]"]
    # set_config_opt pmos_gate_src_shorted_model_names	[join "my_pmos_gate_src_shorted [get_parameter pmos_gate_src_shorted_model_names]"]

    set enable_multi_threshold_receiver_cap 2
    set receiver_capacitance_rise_threshold_pct {0 20 50 60 70 80 90 95 100}
    set receiver_capacitance_fall_threshold_pct {100 80 50 40 30 20 10 5 0}
    set lvf_ml_lvl 1
    set lvf_format both
    set lvf_moment_model_mode 3
    set lvf_constraint_moment_model_mode 1
    set lvf_moment_legacy_mode 1
    set lvf_skewness_factor 1
    set lvf_std_dev_factor 1
    set statistical_enable_mpw_sensitivity 1
    set model_pin_cap_calc ave
    set lvf_constraint_models {setup hold recovery removal}
    set statistical_constraint_screening_points {1 5 21 25}
    set statistical_reduction_factor 1
    set_config_opt -type statistical_mpw statistical_reduction_factor 1
}


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# 4: Liberty Cards
#    Parameters that appear in the Liberty 'as-is'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
define_parameters -append liberty_model {
    #--------------------------------------------------------------------------------------------------------#
    # Header
    set revision {0.1}					;# Library Revision
    set comment {Intel Confidential - PDS}		;# Custom Comment
    set default_operating_conditions [get_parameter active_pvts]
}


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# 5: Pintype Configuration [User Input Section]
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
pintype default {
    #--------------------------------------------------------------------------------------------------------#
    # Logic Thresholds
    if {[string trim $io_supplies] eq ""} { if {[string trim $core_supplies] ne ""} { set logic_high_name [lindex $core_supplies 0] }
    } else { set logic_high_name [lindex $io_supplies 0] }
    if {[string trim $io_grounds] eq ""} { if {[string trim $core_grounds] ne ""} { set logic_low_name [lindex $core_grounds 0] }
    } else { set logic_low_name [lindex $io_grounds 0] }

    if {[info exists ::env(INTEL_PDK) ]} {
    	## For Intel's block
    	set logic_high_threshold 0.8 ;
    	set logic_low_threshold 0.2 ;
    } else {
    	## For Tsmc's block
	set logic_high_threshold 0.7 ;
	set logic_low_threshold 0.3 ;
	#set slew_derate_upper_threshold 0.9 ; # 0.9 is already default from the flow
	#set slew_derate_lower_threshold 0.1 ; # 0.1 is already default from the flow
    }
    
    set prop_delay_level 0.5				;# 50% Delay Threshold
    set glitch_high_threshold 0.7			;# 70% Glitch Threshold for Ports
    set glitch_low_threshold 0.3			;# 30% Glitch Threshold for Ports
    set prop_delay_current 0.1				;# 10% Three-State-Disable Current Threshold
    set cin_high_threshold 0.955			;# 95.5% Gate-Capacitance Integration Upper Threshold
    set cin_low_threshold 0.01				;# 01% Gate-Capacitance Integration Lower Threshold
    set cin_high_threshold_pwr 0.955			;# Similar to cin_high_threshold, but for power pin cap
    set cin_low_threshold_pwr 0.01			;# Similar to cin_low_threshold, but for power pin cap

    #--------------------------------------------------------------------------------------------------------#

 if {[info exists ::env(INTEL_PDK) ]} {
   
    # Slews for Intel's block
    set numsteps_slew 8					;# Need '7' Slew Indices
    set constraint_numsteps_slew 5			;# Need '5' Slew Indices, for Constraints and MPW
    set sweep_method_slew log				;# Use [log|log2x|linear2x|polynomial] Distribution
    set smallest_slew 3e-12				
    set default_slew 15e-12
    set largest_slew 400e-12

    #--------------------------------------------------------------------------------------------------------#
    # Loads for Intel's block
    set numsteps_load 8					;# Need '7' Load Indices
    set sweep_method_load log				;# Use [log|log2x|linear2x|polynomial] Distribution
    set smallest_load 5e-16
    set default_load 14e-15
    set largest_load 80e-15
    set default_load_mode position
    set default_load_index_position 7
    set default_load_index_position_mode max

    #--------------------------------------------------------------------------------------------------------#
    # Load Autoranging for Intel's block
    set autorange_load off				;# Load Auto-Ranging Granularity (MaxCap Determination)
    set_config_opt -type capload explicit_points_slew 80e-12	;# Input Tran for Load Auto-Ranging
    set max_tout 400e-12					;# Output MaxTran Goal for Load Auto-Ranging
    set opt_load_low 1e-16				;# Output Cap Sweep Range - Lower Bound
    set opt_load_high 30e-12				;# Output Cap Sweep Range - Upper Bound
    set maxload_tout_resolution 1e-12

  } else {

    # Slews for Tsmc's block
    set numsteps_slew 7					;# Need '7' Slew Indices
    set constraint_numsteps_slew 5			;# Need '5' Slew Indices, for Constraints and MPW
    set sweep_method_slew log				;# Use [log|log2x|linear2x|polynomial] Distribution
    set smallest_slew 6e-13				
    set default_slew  6e-12
    set largest_slew  2e-10

    #--------------------------------------------------------------------------------------------------------#
    # Loads for Tsmc's block
    set numsteps_load 7					;# Need '7' Load Indices
    set sweep_method_load log				;# Use [log|log2x|linear2x|polynomial] Distribution
    set smallest_load 1.7e-16
    set default_load  1e-15
    set largest_load  4.5e-14

    #--------------------------------------------------------------------------------------------------------#
    # Load Autoranging for Tsmc's block
    set autorange_load pin				;# Load Auto-Ranging Granularity (MaxCap Determination)
    set max_tout 1.57509e-10				;# Output MaxTran Goal for Load Auto-Ranging
    set opt_load_low 1e-16				;# Output Cap Sweep Range - Lower Bound
    set opt_load_high 30e-12				;# Output Cap Sweep Range - Upper Bound
    set maxload_tout_resolution 1e-13

  }


    #--------------------------------------------------------------------------------------------------------#
    # Driver
    set driver_mode emulated				;# [pwl|emulated|active|active-waveform] pwl=ramp, emulated=ccs-pre-driver
    set driver pwl					;# Specify Name of Driver Cell if driver_mode=active*

    #--------------------------------------------------------------------------------------------------------#
    # Global Loads and Slews
    if {[info exists ::env(INTEL_PDK) ]} {
    	## For INTEL's Block
    	set explicit_points_slew            	{3e-12 6e-12 12e-12 24e-12 49e-12 99e-12 199e-12 400e-12}
    	set explicit_points_load            	{5e-15 10e-15 20e-15 30e-15 40e-15 50e-15}
    	set constraint_explicit_points_slew	{3e-12 6e-12 12e-12 24e-12 49e-12 99e-12 199e-12 400e-12}
    } else {
    	## For TSMC's Block
    	set explicit_points_slew		{ 6e-13 2.15e-12 5.3e-12 1.16e-11 2.415e-11 4.925e-11 9.95e-11 2e-10 }
    	set explicit_points_load		{ 1.7e-16 5.9e-16 1.42e-15 3.09e-15 6.43e-15 1.31e-14 2.645e-14 5.314e-14 }
    	set constraint_explicit_points_slew 	{ 6e-13 7.25e-12 2.05e-11 4.7e-11 1e-10 }
    }
    
    
}

pintype pt_core->default {
    #--------------------------------------------------------------------------------------------------------#
    # A New Pintye for 'Core' Powered Pins
    set pt_core_logic_high_name [lindex $io_supplies 0]
    set pt_core_logic_low_name  [lindex $io_grounds  0]
    if {([info exists core_supplies]) && (${core_supplies} != "")} {set pt_core_logic_high_name [lindex $core_supplies 0]}
    if {([info exists core_grounds])  && (${core_grounds}  != "")} {set pt_core_logic_low_name  [lindex $core_grounds  0]}

    set logic_high_name $pt_core_logic_high_name
    set logic_low_name  $pt_core_logic_low_name
}

pintype pt_int_node->default {
    #--------------------------------------------------------------------------------------------------------#
    # A New Pintye for All Internal Nodes - For Hold Measurements
    #set smc_constraint_style relative-degradation	;# [relative-degradation|pass-fail|relative-slew-degradation]
    set logic_high_threshold 0.7			;# 70% Upper Slew Threshold for Internal Nodes
    set logic_low_threshold 0.3				;# 30% Lower Slew Threshold for Internal Nodes
    set glitch_high_threshold 0.7			;# 70% Glitch Threshold for Internal Nodes
    set glitch_low_threshold 0.3			;# 30% Glitch Threshold for Internal Nodes
    #set_config_opt autofix_glitch_high_threshold 0.75	;# Try 75% if 70% fails
    #set_config_opt autofix_glitch_low_threshold 0.25	;# Try 25% if 30% fails
    #set smc_degrade 0    				;# =1% relative delay-degradation
    #set smc_degrade_absolute 1e-09			;# =5ps absoulte delay-degradation
    #set smc_slew_degrade 0  				;# =50% relative slew-degradation
    #set smc_slew_degrade_absolute 0       		;# =0.5ps absoulte slew-degradation
}

pintype pt_int_node_passfail->pt_int_node {
    set smc_constraint_style pass-fail	;# [relative-degradation|pass-fail|relative-slew-degradation]
}

pintype pt_clock->default {
    #--------------------------------------------------------------------------------------------------------#
    # A New Pintye for All Clock Pins
    set smc_constraint_style relative-degradation	;# [relative-degradation|pass-fail|relative-slew-degradation]
    # Global Loads and Slews for Clock Pins
    if {[info exists ::env(INTEL_PDK) ]} {
    	## For INTEL Blocks
    	set explicit_points_slew             {1.5e-12 4e-12 9e-12 16e-12 30e-12 55e-12 100e-12}
    	set constraint_explicit_points_slew  {10e-12 20e-12 30e-12 40e-12 50e-12 60e-12 70e-12}
	
    } else {
    	## For TSMC Blocks
    	set explicit_points_slew		{ 6e-13 2.15e-12 5.3e-12 1.16e-11 2.415e-11 4.925e-11 9.95e-11 2e-10 }
    	set constraint_explicit_points_slew	{ 6e-13 7.25e-12 2.05e-11 4.7e-11 1e-10 }
    }
    
    
    # Pin Attributes
    define_parameters liberty_model {
        set clock true
    }
    set autorange_load off
}

#--------------------------------------------------------------------------------------------------------#
# New Auto-Pintyes for Power/Ground Combinations
foreach __pt_supply [lsearch -all -inline -not -exact [lsort -unique [concat $io_supplies $core_supplies]] {}] {
	foreach __pt_ground [lsearch -all -inline -not -exact [lsort -unique [concat $io_grounds $core_grounds]] {}] {
		set __pt_name pt_${__pt_supply}_${__pt_ground}
		regsub -all {!} ${__pt_name} {bar} __pt_name
		log_info "Corner Parsing:: Creating Auto Pintype ${__pt_name}"
		pintype ${__pt_name}->default {
		    set logic_high_name ${__pt_supply}
		    set logic_low_name  ${__pt_ground}
		}
	}
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# 6: Additional Global Commands
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# In-Situ Liberty Post Processor
if {[llength [get_parameter active_pvts]] == 1} {
    set_config_opt liberty_cell_postprocess [subst {$::env(SILICONSMART_ROOT_DIR)/tcl/liberty_cell_postprocess.tcl}]
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# 7: CCS_NOISE Specific Commands :
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#Please follow the below sets of available options for ccs_noise, if the default setting is not working for your circuit

##### Case1:
##* a. Std-cells
##* b. Or Number of transistors count< 400
##* c. Or Lots of timing arcs; Then enable below command
set_config_opt ccsn_advanced_flow 1 ;# (or 0 may also works)

##### Case2:
##* a. Custom std-cells/ IOs or Pass-gate
##* b. Or Number of transistors count: 400 < # <1000
##* c. Or Few Timing arcs; Then enable below command
#set_config_opt ccsn_advanced_flow 0 ;# (or 1 may also works)

##### Case3: 
##* a. Cell is Macro/ AIP
##* b. Or Number of transistors count > 1000
##* c. Or there are no Timing arc; Then enable below command
#set_config_opt ccb_partition_pin_based 1

### Other options: (As per req.)
#set_config_opt ccsn_flatten_netlist 1
#set_config_opt ccsn_exclude_pin {<list of space separated analog pins>}
#* May required to enable both options ccsn_advanced_flow and ccb_partition_pin_based in the above


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# 8: Voltage rails overriding from corners.xml file to cell specific power pin : (please enable and edit as per req.)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# add_opc_supplies $corner <supply_pin_name> $ivar($corner+<power_rail_name_in_xml>)
#* Example:
# add_opc_supplies $corner vccx $ivar($corner+vcc_tgl)
#* where vccx supply pin of the circuit and vcc_tgl present in the xml file.
#* and the value of vcc_tgl will be assigned to vccx

