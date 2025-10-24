#++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++
# configure_init.tcl 
# Global Configuration Defaults 
# CLT-Sourced, Tailored for IDS
# Works best with >= 2020.03
#++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# 0: Macro|Mcpu Variable Processing
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
   # Version Checks
   set versionInfo [get_version_info -verbose]
   set versionMajor [string range [lindex $versionInfo 1] 2 5]

   # Exit if SiliconSmart < 2020
   if {$versionMajor < "2020"} {
     log_warning "Important Notes: This configure.tcl is best supported with SiliconSmart Q-2020.03 or higher version"
     #exit
   }
   
   regexp {embedded-Finesim (.*)} [lindex $versionInfo 3] matched versionFse
   
   ## HSPICE simulator path:
   if {[info exists ::env(HSPICE_DIR)]} {
   
	set hspicePath "$::env(HSPICE_DIR)/hspice/bin/hspice";
	
   } elseif {[info exists ::env(HSPICE_HOME)]} {
   
   	set hspicePath "$::env(HSPICE_HOME)/hspice/bin/hspice";
	
   } else {
   
   	log_warning "Missing environment variable HSPICE_HOME or HSPICE_DIR"
	log_info "Using HSPICE simulator path: /p/hdk/cad/hspice/Q-2020.03/hspice/bin/hspice"
   	set hspicePath "/p/hdk/cad/hspice/Q-2020.03/hspice/bin/hspice"
   
   }
   
   ## FINESIM simulator path:
   if {[info exists ::env(FINESIM_VER)]} {
	set mappedFinesimVersion $::env(FINESIM_VER);
   } else {
   	log_error "Missing environment variable FINESIM_VER"
	log_error "Can't construct: /p/hdk/cad/finesim/{FINESIM_VER}/finesim/bin/finesim"
	exit
   }
   
   ## Library compiler path:
   if {[info exists ::env(LIBRARYCOMPILER_DIR)]} {
   	set lc_shellPath "$::env(LIBRARYCOMPILER_DIR)/bin/lc_shell";
	set mappedLcVersion $::env(LIBRARYCOMPILER_DIR);
   } elseif {[info exists ::env(SYNOPSYS_LC_ROOT)]} {
   	set lc_shellPath "$::env(SYNOPSYS_LC_ROOT)/bin/lc_shell";
	set mappedLcVersion $::env(SYNOPSYS_LC_ROOT);
   } elseif {[info exists ::env(LIBRARYCOMPILER_VER)]} {
   	log_warning "Missing environment variable LIBRARYCOMPILER_DIR or SYNOPSYS_LC_ROOT"
	log_info "Considering user defined LIBRARYCOMPILER_VER=$::env(LIBRARYCOMPILER_VER)"
	set lc_shellPath "/p/hdk/cad/librarycompiler/$::env(LIBRARYCOMPILER_VER)/bin/lc_shell";
	set mappedLcVersion "/p/hdk/cad/librarycompiler/$::env(LIBRARYCOMPILER_VER)"
   } else {
   	log_warning "Missing environment variable LIBRARYCOMPILER_DIR or SYNOPSYS_LC_ROOT"
	log_info "setting lc_shellPath to:/p/hdk/cad/librarycompiler/P-2019.03-SP5/bin/lc_shell";
   	set lc_shellPath "/p/hdk/cad/librarycompiler/P-2019.03-SP5/bin/lc_shell";
	set mappedLcVersion "/p/hdk/cad/librarycompiler/P-2019.03-SP5";
   }
   
   ## To consider PT shell version similiar as Library compiler version:
   regsub -all {( )} $mappedLcVersion "" mappedLcVersion
   regsub -all {(//)} $mappedLcVersion "/" mappedLcVersion
   regsub {(/$)} $mappedLcVersion "" mappedLcVersion
   regsub {(.*/)} $mappedLcVersion "" mappedLcVersion
   
   # Modes
   create_parameter ibis_mode
   if {![info exists ::env(ibis_mode)]} {set_config_opt ibis_mode 0} else {set_config_opt ibis_mode $env(ibis_mode)}
   create_parameter macro_mode
   if {![info exists ::env(macro_mode)]} {set_config_opt macro_mode 0} else {set_config_opt macro_mode $env(macro_mode)}
   create_parameter lvf_mode
   if {![info exists ::env(lvf_mode)]} {set_config_opt lvf_mode sba} else {set_config_opt lvf_mode $env(lvf_mode)}
   create_parameter use_simulator
   if {![info exists ::env(use_simulator)]} {set_config_opt use_simulator 0} else {set_config_opt use_simulator $env(use_simulator)}

   if {![info exists ::env(finesim_mode)]} {set finesim_mode spicead} else {set finesim_mode $env(finesim_mode)}
        
   # SPF Netlist Post-Processing
   #if {[info exists ::env(INTEL_PDK)]} {
     # source $::env(SILICONSMART_ROOT_DIR)/tcl/spf_post.tcl
   #}
   
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# 1: Global Cards
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
define_parameters default {

    #--------------------------------------------------------------------------------------------------------#
    # Early Calls
    disable_status
    suppress_message {SMC-260}

    #--------------------------------------------------------------------------------------------------------#
    # Simulator Options
    set simulator_options [subst {"common,finesim: finesim_mode=${finesim_mode} finesim_method=egear finesim_vpwltol=0 finesim_pwl_acc=1 finesim_exitwarn="DC not converged" finesim_exitwarn="FINESIM_TUNIT" finesim_no_swap=1"}]
    set_config_opt -type ccs_noise simulator_options {"common,finesim: finesim_mode=spicead finesim_method=egear finesim_vpwltol=0 finesim_pwl_acc=1 finesim_exitwarn="DC not converged" finesim_exitwarn="FINESIM_TUNIT" finesim_no_swap=1"}
    #--------------------------------------------------------------------------------------------------------#
    # Simulation Resolution
    set time_res_high 1e-12
    set time_res_low 5e-09

    #--------------------------------------------------------------------------------------------------------#
    # Liberty Power Format
    set liberty_multi_rail_format v2			;# v2=related_pg_pin syntax; v1=power_supply snytanx

    #--------------------------------------------------------------------------------------------------------#
    # Liberty Units
    set liberty_cap_unit "1ff"
    set liberty_current_unit "1mA"
    set liberty_flavor "2010.03"
    set liberty_leakage_power_unit "1uW"
    set liberty_time_unit "1ps"
    set model_significant_digits 8			;# Need '8' Significant Digits in Library Data
    set pulling_resistance_unit "1kohm"


if {! [info exists ::env(INTEL_PDK) ]} {
    ## For TSMC
    set liberty_cap_unit "1pf"
    set liberty_leakage_power_unit "1nW"
    set liberty_time_unit "1ns"
    set model_significant_digits 6			;# Need '8' Significant Digits in Library Data
}
    #--------------------------------------------------------------------------------------------------------#
    # Liberty Attributes, Groups and Modeling Choices
    set calculate_max_transition 1
    set liberty_max_transition 1			;# =1 Model max_transition Attribute
    set liberty_minimize_groups 0
    set liberty_minimize_timing_when 0
    set liberty_power_down_function 1			;# =1 Model power_down_function Attribute
    set import_sco_nowhens 1
    set import_liberty_ndw 0
    set import_liberty_ndw_mode 1
    set import_constraint_mode add_direction

    #--------------------------------------------------------------------------------------------------------#
    # Job Scheduler Controls
    set auto_fix 1					;# Re-run failed tasks '1' time(s)
    
    
    ## Changing it to 0 (on 22-02-2022)
    set cdpl_alt_submission 0
    
    
    set secure_mode 1
    set simulation_tmpdir /tmp				;# Simulation 'tmp' dir
    set cache_include_files 3
    set use_simulator_licenses 1
    #--------------------------------------------------------------------------------------------------------#
    set scheduler_poll_time 60				;# Update Log every '60' seconds
    ##Update it 10min-->5min (30May)
    set cdpl_worker_timeout 300				;# Exit a Worker/Job if idle for 300 sec (5min)
    set cdpl_long_task_alert 3600			;# Warn if a simulation runs>3600 sec (1hr)
    set cdpl_task_max_lifespan 2880			;# Kill if a simulation runs>2880 min (2days)
    set char_engine_max_lifespan 2880			;# Renew NetBatch Job after 2880 min (2days)
    set char_engine_hard_interrupt none
    set cdpl_log_filter 2
    set cdpl_task_max_retry 1
    set cdpl_tasks_log 1
    set cdpl_save_crashes 1
    set cdpl_submission_timeout 72000
    set cdpl_disable_worker_state_check 1
    set io_retry 4

    #--------------------------------------------------------------------------------------------------------#
    # Functional Recognition/CCSN Pre-requisite
    if {[info exists ::env(INTEL_PDK)]} {
    	set nmos_model_names			{n Macro_AnalogCell nhpulvt nhpculvt nulp ntglv nhphvt nhpchvt nhpsvt nhpcsvt nhplvt nhpclvt nlplvt nlllvt nll nhp nelllvt nal120 nal160 nal74 navt nclp nhvt nlp nlvt nslvt nsrhdc nsrhpc nsrlvc nsvt nsvtclp ntg nulvt nell ntgmv  nhpbulvt nhpblvt nhpbsvt nhpbhvt}
    	set pmos_model_names			{p Macro_StackGate mosfet_StackSeries_pcell_0 mosfet_StackSeries_pcell_1 mosfet_StackSeries_pcell_13 mosfet_StackSeries_pcell_14 mosfet_StackSeries_pcell_15 mosfet_StackSeries_pcell_17 mosfet_StackSeries_pcell_18 mosfet_StackSeries_pcell_19 mosfet_StackSeries_pcell_20 mosfet_StackSeries_pcell_22 mosfet_StackSeries_pcell_24 mosfet_StackSeries_pcell_26 mosfet_StackSeries_pcell_27 mosfet_StackSeries_pcell_2 phpulvt phpculvt pulp ptglv phphvt phpchvt phpsvt phpcsvt phplvt phpclvt plvt plplvt plp plllvt pll php pelllvt pal120 pal160 pal74 pavt pclp phvt pslvt psrhdc psrhpc psrlvc psvt psvtclp ptg pulvt pell ptgmv phpbulvt phpblvt phpbsvt phpbhvt}
    	set nmos_drn_src_shorted_model_names    {}
    	set pmos_drn_src_shorted_model_names    {}

    } else {
    	set nmos_model_names			{n analog_macro nch_18_mac nch_12_mac Macro_AnalogCell nhpulvt nch_ulvtll nch_ulvtll_mac nch_elvt_mac nch_lvt_mac nch_svt_mac nch_ulvt_mac nulp ntglv nlplvt nlllvt nll nhp nelllvt nal120 nal160 nal74 navt nclp nhvt nlp nlvt nslvt nsrhdc nsrhpc nsrlvc nsvt nsvtclp ntg nulvt nell ntgmv}
    	set pmos_model_names			{p pch_18_mac pch_12_mac Macro_StackGate phpulvt pch_ulvtll pch_ulvtll_mac pch_elvt_mac pch_lvt_mac pch_svt_mac pch_ulvt_mac pulp ptglv plvt plplvt plp plllvt pll php pelllvt pal120 pal160 pal74 pavt pclp phvt pslvt psrhdc psrhpc psrlvc psvt psvtclp ptg pulvt pell ptgmv}
    	set nmos_drn_src_shorted_model_names    {nch_18_mac_pode nch_mpodelvt_mac nch_mpodesvt_mac npode_elvt_mac nch_mpodeulvt_mac nch_mpodeulvtll_mac npode_ulvtll_mac npode_18_mac npode_18ud12_mac npode_18ud15_mac npode_lvt15_mac npode_lvt15ud12_mac npode_lvt_mac npode_svt_mac npode_ulvt_mac}
    	set pmos_drn_src_shorted_model_names    {pch_18_mac_pode pch_mpodelvt_mac pch_mpodesvt_mac ppode_elvt_mac ppode_ulvtll_mac ppode_12_mac pch_mpodeulvt_mac pch_mpodeulvtll_mac ppode_18_mac ppode_18ud12_mac ppode_18ud15_mac ppode_lvt15_mac ppode_lvt15ud12_mac ppode_lvt_mac ppode_svt_mac ppode_ulvt_mac}
    	
    }
    
    set bjt_model_names				{e8xbgdiode_prim pnp_i2_mac}
    set cap_model_names				{crtmom_wo nmoscap_18 nmoscap_12 nmoscap_18 rfad_unitcap_5p3p_m9 nmoscap ljpll_srlpfcap2 crtmom_2t crc4ce1 crc4ce2 crc4ce3 crc4ce3c4s crc4f crc4m0 crc4m1 crc4m10 crc4m2 crc4m3 crc4m4 crc4m5 crc4m6 crc4m7 crc4m8 crc4m9 crc4nb crc4nd crc4p crc4pd crc4tm0 crc4tm1 crc4tm1c4s crce1f crce1fc4 crce1ftm1 crce1m0 crce1m0c4 crce1m0tm1 crce1m1 crce1m10 crce1m10c4 crce1m10tm1 crce1m1c4 crce1m1tm1 crce1m2 crce1m2c4 crce1m2tm1 crce1m3 crce1m3c4 crce1m3tm1 crce1m4 crce1m4c4 crce1m4tm1 crce1m5 crce1m5c4 crce1m5tm1 crce1m6 crce1m6c4 crce1m6tm1 crce1m7 crce1m7c4 crce1m7tm1 crce1m8 crce1m8c4 crce1m8tm1 crce1m9 crce1m9c4 crce1m9tm1 crce1nb crce1nbc4 crce1nbtm1 crce1nd crce1ndc4 crce1ndtm1 crce1p crce1pc4 crce1pd crce1pdc4 crce1pdtm1 crce1ptm1 crce1tm0 crce1tm0c4 crce1tm0tm1 crce2f crce2fc4 crce2ftm1 crce2m0 crce2m0c4 crce2m0tm1 crce2m1 crce2m10 crce2m10c4 crce2m10tm1 crce2m1c4 crce2m1tm1 crce2m2 crce2m2c4 crce2m2tm1 crce2m3 crce2m3c4 crce2m3tm1 crce2m4 crce2m4c4 crce2m4tm1 crce2m5 crce2m5c4 crce2m5tm1 crce2m6 crce2m6c4 crce2m6tm1 crce2m7 crce2m7c4 crce2m7tm1 crce2m8 crce2m8c4 crce2m8tm1 crce2m9 crce2m9c4 crce2m9tm1 crce2nb crce2nbc4 crce2nbtm1 crce2nd crce2ndc4 crce2ndtm1 crce2p crce2pc4 crce2pd crce2pdc4 crce2pdtm1 crce2ptm1 crce2tm0 crce2tm0c4 crce2tm0tm1 crce3f crce3fc4 crce3ftm1 crce3m0 crce3m0c4 crce3m0tm1 crce3m1 crce3m10 crce3m10c4 crce3m10tm1 crce3m1c4 crce3m1tm1 crce3m2 crce3m2c4 crce3m2tm1 crce3m3 crce3m3c4 crce3m3tm1 crce3m4 crce3m4c4 crce3m4tm1 crce3m5 crce3m5c4 crce3m5tm1 crce3m6 crce3m6c4 crce3m6tm1 crce3m7 crce3m7c4 crce3m7tm1 crce3m8 crce3m8c4 crce3m8tm1 crce3m9 crce3m9c4 crce3m9tm1 crce3nb crce3nbc4 crce3nbtm1 crce3nd crce3ndc4 crce3ndtm1 crce3p crce3pc4 crce3pd crce3pdc4 crce3pdtm1 crce3ptm1 crce3tm0 crce3tm0c4 crce3tm0tm1 crm0f crm0fc4 crm0fce1 crm0fce2 crm0fce3 crm0fm1 crm0fm10 crm0fm2 crm0fm3 crm0fm4 crm0fm5 crm0fm6 crm0fm7 crm0fm8 crm0fm9 crm0ftm0 crm0ftm1 crm0nb crm0nbc4 crm0nbce1 crm0nbce2 crm0nbce3 crm0nbm1 crm0nbm10 crm0nbm2 crm0nbm3 crm0nbm4 crm0nbm5 crm0nbm6 crm0nbm7 crm0nbm8 crm0nbm9 crm0nbtm0 crm0nbtm1 crm0nd crm0ndc4 crm0ndce1 crm0ndce2 crm0ndce3 crm0ndm1 crm0ndm10 crm0ndm2 crm0ndm3 crm0ndm4 crm0ndm5 crm0ndm6 crm0ndm7 crm0ndm8 crm0ndm9 crm0ndtm0 crm0ndtm1 crm0p crm0pc4 crm0pce1 crm0pce2 crm0pce3 crm0pd crm0pdc4 crm0pdce1 crm0pdce2 crm0pdce3 crm0pdm1 crm0pdm10 crm0pdm2 crm0pdm3 crm0pdm4 crm0pdm5 crm0pdm6 crm0pdm7 crm0pdm8 crm0pdm9 crm0pdtm0 crm0pdtm1 crm0pm1 crm0pm10 crm0pm2 crm0pm3 crm0pm4 crm0pm5 crm0pm6 crm0pm7 crm0pm8 crm0pm9 crm0ptm0 crm0ptm1 crm0tcn crm0tcnm1 crm0tcnm2 crm0tcnm3 crm0tcnm4 crm10f crm10fc4 crm10fce1 crm10fce2 crm10fce3 crm10ftm0 crm10ftm1 crm10m0 crm10m0c4 crm10m0ce1 crm10m0ce2 crm10m0ce3 crm10m0tm0 crm10m0tm1 crm10m1 crm10m1c4 crm10m1ce1 crm10m1ce2 crm10m1ce3 crm10m1tm0 crm10m1tm1 crm10m2 crm10m2c4 crm10m2ce1 crm10m2ce2 crm10m2ce3 crm10m2tm0 crm10m2tm1 crm10m3 crm10m3c4 crm10m3ce1 crm10m3ce2 crm10m3ce3 crm10m3tm0 crm10m3tm1 crm10m4 crm10m4c4 crm10m4ce1 crm10m4ce2 crm10m4ce3 crm10m4tm0 crm10m4tm1 crm10m5 crm10m5c4 crm10m5ce1 crm10m5ce2 crm10m5ce3 crm10m5tm0 crm10m5tm1 crm10m6 crm10m6c4 crm10m6ce1 crm10m6ce2 crm10m6ce3 crm10m6tm0 crm10m6tm1 crm10m7 crm10m7c4 crm10m7ce1 crm10m7ce2 crm10m7ce3 crm10m7tm0 crm10m7tm1 crm10m8 crm10m8c4 crm10m8ce1 crm10m8ce2 crm10m8ce3 crm10m8tm0 crm10m8tm1 crm10m9 crm10m9c4 crm10m9ce1 crm10m9ce2 crm10m9ce3 crm10m9tm0 crm10m9tm1 crm10nb crm10nbc4 crm10nbce1 crm10nbce2 crm10nbce3 crm10nbtm0 crm10nbtm1 crm10nd crm10ndc4 crm10ndce1 crm10ndce2 crm10ndce3 crm10ndtm0 crm10ndtm1 crm10p crm10pc4 crm10pce1 crm10pce2 crm10pce3 crm10pd crm10pdc4 crm10pdce1 crm10pdce2 crm10pdce3 crm10pdtm0 crm10pdtm1 crm10ptm0 crm10ptm1 crm1f crm1fc4 crm1fce1 crm1fce2 crm1fce3 crm1fm10 crm1fm2 crm1fm3 crm1fm4 crm1fm5 crm1fm6 crm1fm7 crm1fm8 crm1fm9 crm1ftm0 crm1ftm1 crm1m0 crm1m0c4 crm1m0ce1 crm1m0ce2 crm1m0ce3 crm1m0m10 crm1m0m2 crm1m0m3 crm1m0m4 crm1m0m5 crm1m0m6 crm1m0m7 crm1m0m8 crm1m0m9 crm1m0tm0 crm1m0tm1 crm1nb crm1nbc4 crm1nbce1 crm1nbce2 crm1nbce3 crm1nbm10 crm1nbm2 crm1nbm3 crm1nbm4 crm1nbm5 crm1nbm6 crm1nbm7 crm1nbm8 crm1nbm9 crm1nbtm0 crm1nbtm1 crm1nd crm1ndc4 crm1ndce1 crm1ndce2 crm1ndce3 crm1ndm10 crm1ndm2 crm1ndm3 crm1ndm4 crm1ndm5 crm1ndm6 crm1ndm7 crm1ndm8 crm1ndm9 crm1ndtm0 crm1ndtm1 crm1p crm1pc4 crm1pce1 crm1pce2 crm1pce3 crm1pd crm1pdc4 crm1pdce1 crm1pdce2 crm1pdce3 crm1pdm10 crm1pdm2 crm1pdm3 crm1pdm4 crm1pdm5 crm1pdm6 crm1pdm7 crm1pdm8 crm1pdm9 crm1pdtm0 crm1pdtm1 crm1pm10 crm1pm2 crm1pm3 crm1pm4 crm1pm5 crm1pm6 crm1pm7 crm1pm8 crm1pm9 crm1ptm0 crm1ptm1 crm1tcn crm1tcnm2 crm1tcnm3 crm1tcnm4 crm1tcnm5 crm2f crm2fc4 crm2fce1 crm2fce2 crm2fce3 crm2fm10 crm2fm3 crm2fm4 crm2fm5 crm2fm6 crm2fm7 crm2fm8 crm2fm9 crm2ftm0 crm2ftm1 crm2m0 crm2m0c4 crm2m0ce1 crm2m0ce2 crm2m0ce3 crm2m0m10 crm2m0m3 crm2m0m4 crm2m0m5 crm2m0m6 crm2m0m7 crm2m0m8 crm2m0m9 crm2m0tm0 crm2m0tm1 crm2m1 crm2m1c4 crm2m1ce1 crm2m1ce2 crm2m1ce3 crm2m1m10 crm2m1m3 crm2m1m4 crm2m1m5 crm2m1m6 crm2m1m7 crm2m1m8 crm2m1m9 crm2m1tm0 crm2m1tm1 crm2nb crm2nbc4 crm2nbce1 crm2nbce2 crm2nbce3 crm2nbm10 crm2nbm3 crm2nbm4 crm2nbm5 crm2nbm6 crm2nbm7 crm2nbm8 crm2nbm9 crm2nbtm0 crm2nbtm1 crm2nd crm2ndc4 crm2ndce1 crm2ndce2 crm2ndce3 crm2ndm10 crm2ndm3 crm2ndm4 crm2ndm5 crm2ndm6 crm2ndm7 crm2ndm8 crm2ndm9 crm2ndtm0 crm2ndtm1 crm2p crm2pc4 crm2pce1 crm2pce2 crm2pce3 crm2pd crm2pdc4 crm2pdce1 crm2pdce2 crm2pdce3 crm2pdm10 crm2pdm3 crm2pdm4 crm2pdm5 crm2pdm6 crm2pdm7 crm2pdm8 crm2pdm9 crm2pdtm0 crm2pdtm1 crm2pm10 crm2pm3 crm2pm4 crm2pm5 crm2pm6 crm2pm7 crm2pm8 crm2pm9 crm2ptm0 crm2ptm1 crm2tcn crm2tcnm3 crm2tcnm4 crm2tcnm5 crm2tcnm6 crm3f crm3fc4 crm3fce1 crm3fce2 crm3fce3 crm3fm10 crm3fm4 crm3fm5 crm3fm6 crm3fm7 crm3fm8 crm3fm9 crm3ftm0 crm3ftm1 crm3m0 crm3m0c4 crm3m0ce1 crm3m0ce2 crm3m0ce3 crm3m0m10 crm3m0m4 crm3m0m5 crm3m0m6 crm3m0m7 crm3m0m8 crm3m0m9 crm3m0tm0 crm3m0tm1 crm3m1 crm3m1c4 crm3m1ce1 crm3m1ce2 crm3m1ce3 crm3m1m10 crm3m1m4 crm3m1m5 crm3m1m6 crm3m1m7 crm3m1m8 crm3m1m9 crm3m1tm0 crm3m1tm1 crm3m2 crm3m2c4 crm3m2ce1 crm3m2ce2 crm3m2ce3 crm3m2m10 crm3m2m4 crm3m2m5 crm3m2m6 crm3m2m7 crm3m2m8 crm3m2m9 crm3m2tm0 crm3m2tm1 crm3nb crm3nbc4 crm3nbce1 crm3nbce2 crm3nbce3 crm3nbm10 crm3nbm4 crm3nbm5 crm3nbm6 crm3nbm7 crm3nbm8 crm3nbm9 crm3nbtm0 crm3nbtm1 crm3nd crm3ndc4 crm3ndce1 crm3ndce2 crm3ndce3 crm3ndm10 crm3ndm4 crm3ndm5 crm3ndm6 crm3ndm7 crm3ndm8 crm3ndm9 crm3ndtm0 crm3ndtm1 crm3p crm3pc4 crm3pce1 crm3pce2 crm3pce3 crm3pd crm3pdc4 crm3pdce1 crm3pdce2 crm3pdce3 crm3pdm10 crm3pdm4 crm3pdm5 crm3pdm6 crm3pdm7 crm3pdm8 crm3pdm9 crm3pdtm0 crm3pdtm1 crm3pm10 crm3pm4 crm3pm5 crm3pm6 crm3pm7 crm3pm8 crm3pm9 crm3ptm0 crm3ptm1 crm3tcn crm3tcnm4 crm3tcnm5 crm3tcnm6 crm3tcnm7 crm4f crm4fc4 crm4fce1 crm4fce2 crm4fce3 crm4fm10 crm4fm5 crm4fm6 crm4fm7 crm4fm8 crm4fm9 crm4ftm0 crm4ftm1 crm4m0 crm4m0c4 crm4m0ce1 crm4m0ce2 crm4m0ce3 crm4m0m10 crm4m0m5 crm4m0m6 crm4m0m7 crm4m0m8 crm4m0m9 crm4m0tm0 crm4m0tm1 crm4m1 crm4m1c4 crm4m1ce1 crm4m1ce2 crm4m1ce3 crm4m1m10 crm4m1m5 crm4m1m6 crm4m1m7 crm4m1m8 crm4m1m9 crm4m1tm0 crm4m1tm1 crm4m2 crm4m2c4 crm4m2ce1 crm4m2ce2 crm4m2ce3 crm4m2m10 crm4m2m5 crm4m2m6 crm4m2m7 crm4m2m8 crm4m2m9 crm4m2tm0 crm4m2tm1 crm4m3 crm4m3c4 crm4m3ce1 crm4m3ce2 crm4m3ce3 crm4m3m10 crm4m3m5 crm4m3m6 crm4m3m7 crm4m3m8 crm4m3m9 crm4m3tm0 crm4m3tm1 crm4nb crm4nbc4 crm4nbce1 crm4nbce2 crm4nbce3 crm4nbm10 crm4nbm5 crm4nbm6 crm4nbm7 crm4nbm8 crm4nbm9 crm4nbtm0 crm4nbtm1 crm4nd crm4ndc4 crm4ndce1 crm4ndce2 crm4ndce3 crm4ndm10 crm4ndm5 crm4ndm6 crm4ndm7 crm4ndm8 crm4ndm9 crm4ndtm0 crm4ndtm1 crm4p crm4pc4 crm4pce1 crm4pce2 crm4pce3 crm4pd crm4pdc4 crm4pdce1 crm4pdce2 crm4pdce3 crm4pdm10 crm4pdm5 crm4pdm6 crm4pdm7 crm4pdm8 crm4pdm9 crm4pdtm0 crm4pdtm1 crm4pm10 crm4pm5 crm4pm6 crm4pm7 crm4pm8 crm4pm9 crm4ptm0 crm4ptm1 crm5f crm5fc4 crm5fce1 crm5fce2 crm5fce3 crm5fm10 crm5fm6 crm5fm7 crm5fm8 crm5fm9 crm5ftm0 crm5ftm1 crm5m0 crm5m0c4 crm5m0ce1 crm5m0ce2 crm5m0ce3 crm5m0m10 crm5m0m6 crm5m0m7 crm5m0m8 crm5m0m9 crm5m0tm0 crm5m0tm1 crm5m1 crm5m1c4 crm5m1ce1 crm5m1ce2 crm5m1ce3 crm5m1m10 crm5m1m6 crm5m1m7 crm5m1m8 crm5m1m9 crm5m1tm0 crm5m1tm1 crm5m2 crm5m2c4 crm5m2ce1 crm5m2ce2 crm5m2ce3 crm5m2m10 crm5m2m6 crm5m2m7 crm5m2m8 crm5m2m9 crm5m2tm0 crm5m2tm1 crm5m3 crm5m3c4 crm5m3ce1 crm5m3ce2 crm5m3ce3 crm5m3m10 crm5m3m6 crm5m3m7 crm5m3m8 crm5m3m9 crm5m3tm0 crm5m3tm1 crm5m4 crm5m4c4 crm5m4ce1 crm5m4ce2 crm5m4ce3 crm5m4m10 crm5m4m6 crm5m4m7 crm5m4m8 crm5m4m9 crm5m4tm0 crm5m4tm1 crm5nb crm5nbc4 crm5nbce1 crm5nbce2 crm5nbce3 crm5nbm10 crm5nbm6 crm5nbm7 crm5nbm8 crm5nbm9 crm5nbtm0 crm5nbtm1 crm5nd crm5ndc4 crm5ndce1 crm5ndce2 crm5ndce3 crm5ndm10 crm5ndm6 crm5ndm7 crm5ndm8 crm5ndm9 crm5ndtm0 crm5ndtm1 crm5p crm5pc4 crm5pce1 crm5pce2 crm5pce3 crm5pd crm5pdc4 crm5pdce1 crm5pdce2 crm5pdce3 crm5pdm10 crm5pdm6 crm5pdm7 crm5pdm8 crm5pdm9 crm5pdtm0 crm5pdtm1 crm5pm10 crm5pm6 crm5pm7 crm5pm8 crm5pm9 crm5ptm0 crm5ptm1 crm6f crm6fc4 crm6fce1 crm6fce2 crm6fce3 crm6fm10 crm6fm7 crm6fm8 crm6fm9 crm6ftm0 crm6ftm1 crm6m0 crm6m0c4 crm6m0ce1 crm6m0ce2 crm6m0ce3 crm6m0m10 crm6m0m7 crm6m0m8 crm6m0m9 crm6m0tm0 crm6m0tm1 crm6m1 crm6m1c4 crm6m1ce1 crm6m1ce2 crm6m1ce3 crm6m1m10 crm6m1m7 crm6m1m8 crm6m1m9 crm6m1tm0 crm6m1tm1 crm6m2 crm6m2c4 crm6m2ce1 crm6m2ce2 crm6m2ce3 crm6m2m10 crm6m2m7 crm6m2m8 crm6m2m9 crm6m2tm0 crm6m2tm1 crm6m3 crm6m3c4 crm6m3ce1 crm6m3ce2 crm6m3ce3 crm6m3m10 crm6m3m7 crm6m3m8 crm6m3m9 crm6m3tm0 crm6m3tm1 crm6m4 crm6m4c4 crm6m4ce1 crm6m4ce2 crm6m4ce3 crm6m4m10 crm6m4m7 crm6m4m8 crm6m4m9 crm6m4tm0 crm6m4tm1 crm6m5 crm6m5c4 crm6m5ce1 crm6m5ce2 crm6m5ce3 crm6m5m10 crm6m5m7 crm6m5m8 crm6m5m9 crm6m5tm0 crm6m5tm1 crm6nb crm6nbc4 crm6nbce1 crm6nbce2 crm6nbce3 crm6nbm10 crm6nbm7 crm6nbm8 crm6nbm9 crm6nbtm0 crm6nbtm1 crm6nd crm6ndc4 crm6ndce1 crm6ndce2 crm6ndce3 crm6ndm10 crm6ndm7 crm6ndm8 crm6ndm9 crm6ndtm0 crm6ndtm1 crm6p crm6pc4 crm6pce1 crm6pce2 crm6pce3 crm6pd crm6pdc4 crm6pdce1 crm6pdce2 crm6pdce3 crm6pdm10 crm6pdm7 crm6pdm8 crm6pdm9 crm6pdtm0 crm6pdtm1 crm6pm10 crm6pm7 crm6pm8 crm6pm9 crm6ptm0 crm6ptm1 crm7f crm7fc4 crm7fce1 crm7fce2 crm7fce3 crm7fm10 crm7fm8 crm7fm9 crm7ftm0 crm7ftm1 crm7m0 crm7m0c4 crm7m0ce1 crm7m0ce2 crm7m0ce3 crm7m0m10 crm7m0m8 crm7m0m9 crm7m0tm0 crm7m0tm1 crm7m1 crm7m1c4 crm7m1ce1 crm7m1ce2 crm7m1ce3 crm7m1m10 crm7m1m8 crm7m1m9 crm7m1tm0 crm7m1tm1 crm7m2 crm7m2c4 crm7m2ce1 crm7m2ce2 crm7m2ce3 crm7m2m10 crm7m2m8 crm7m2m9 crm7m2tm0 crm7m2tm1 crm7m3 crm7m3c4 crm7m3ce1 crm7m3ce2 crm7m3ce3 crm7m3m10 crm7m3m8 crm7m3m9 crm7m3tm0 crm7m3tm1 crm7m4 crm7m4c4 crm7m4ce1 crm7m4ce2 crm7m4ce3 crm7m4m10 crm7m4m8 crm7m4m9 crm7m4tm0 crm7m4tm1 crm7m5 crm7m5c4 crm7m5ce1 crm7m5ce2 crm7m5ce3 crm7m5m10 crm7m5m8 crm7m5m9 crm7m5tm0 crm7m5tm1 crm7m6 crm7m6c4 crm7m6ce1 crm7m6ce2 crm7m6ce3 crm7m6m10 crm7m6m8 crm7m6m9 crm7m6tm0 crm7m6tm1 crm7nb crm7nbc4 crm7nbce1 crm7nbce2 crm7nbce3 crm7nbm10 crm7nbm8 crm7nbm9 crm7nbtm0 crm7nbtm1 crm7nd crm7ndc4 crm7ndce1 crm7ndce2 crm7ndce3 crm7ndm10 crm7ndm8 crm7ndm9 crm7ndtm0 crm7ndtm1 crm7p crm7pc4 crm7pce1 crm7pce2 crm7pce3 crm7pd crm7pdc4 crm7pdce1 crm7pdce2 crm7pdce3 crm7pdm10 crm7pdm8 crm7pdm9 crm7pdtm0 crm7pdtm1 crm7pm10 crm7pm8 crm7pm9 crm7ptm0 crm7ptm1 crm8f crm8fc4 crm8fce1 crm8fce2 crm8fce3 crm8fm10 crm8fm9 crm8ftm0 crm8ftm1 crm8m0 crm8m0c4 crm8m0ce1 crm8m0ce2 crm8m0ce3 crm8m0m10 crm8m0m9 crm8m0tm0 crm8m0tm1 crm8m1 crm8m1c4 crm8m1ce1 crm8m1ce2 crm8m1ce3 crm8m1m10 crm8m1m9 crm8m1tm0 crm8m1tm1 crm8m2 crm8m2c4 crm8m2ce1 crm8m2ce2 crm8m2ce3 crm8m2m10 crm8m2m9 crm8m2tm0 crm8m2tm1 crm8m3 crm8m3c4 crm8m3ce1 crm8m3ce2 crm8m3ce3 crm8m3m10 crm8m3m9 crm8m3tm0 crm8m3tm1 crm8m4 crm8m4c4 crm8m4ce1 crm8m4ce2 crm8m4ce3 crm8m4m10 crm8m4m9 crm8m4tm0 crm8m4tm1 crm8m5 crm8m5c4 crm8m5ce1 crm8m5ce2 crm8m5ce3 crm8m5m10 crm8m5m9 crm8m5tm0 crm8m5tm1 crm8m6 crm8m6c4 crm8m6ce1 crm8m6ce2 crm8m6ce3 crm8m6m10 crm8m6m9 crm8m6tm0 crm8m6tm1 crm8m7 crm8m7c4 crm8m7ce1 crm8m7ce2 crm8m7ce3 crm8m7m10 crm8m7m9 crm8m7tm0 crm8m7tm1 crm8nb crm8nbc4 crm8nbce1 crm8nbce2 crm8nbce3 crm8nbm10 crm8nbm9 crm8nbtm0 crm8nbtm1 crm8nd crm8ndc4 crm8ndce1 crm8ndce2 crm8ndce3 crm8ndm10 crm8ndm9 crm8ndtm0 crm8ndtm1 crm8p crm8pc4 crm8pce1 crm8pce2 crm8pce3 crm8pd crm8pdc4 crm8pdce1 crm8pdce2 crm8pdce3 crm8pdm10 crm8pdm9 crm8pdtm0 crm8pdtm1 crm8pm10 crm8pm9 crm8ptm0 crm8ptm1 crm9f crm9fc4 crm9fce1 crm9fce2 crm9fce3 crm9fm10 crm9ftm0 crm9ftm1 crm9m0 crm9m0c4 crm9m0ce1 crm9m0ce2 crm9m0ce3 crm9m0m10 crm9m0tm0 crm9m0tm1 crm9m1 crm9m1c4 crm9m1ce1 crm9m1ce2 crm9m1ce3 crm9m1m10 crm9m1tm0 crm9m1tm1 crm9m2 crm9m2c4 crm9m2ce1 crm9m2ce2 crm9m2ce3 crm9m2m10 crm9m2tm0 crm9m2tm1 crm9m3 crm9m3c4 crm9m3ce1 crm9m3ce2 crm9m3ce3 crm9m3m10 crm9m3tm0 crm9m3tm1 crm9m4 crm9m4c4 crm9m4ce1 crm9m4ce2 crm9m4ce3 crm9m4m10 crm9m4tm0 crm9m4tm1 crm9m5 crm9m5c4 crm9m5ce1 crm9m5ce2 crm9m5ce3 crm9m5m10 crm9m5tm0 crm9m5tm1 crm9m6 crm9m6c4 crm9m6ce1 crm9m6ce2 crm9m6ce3 crm9m6m10 crm9m6tm0 crm9m6tm1 crm9m7 crm9m7c4 crm9m7ce1 crm9m7ce2 crm9m7ce3 crm9m7m10 crm9m7tm0 crm9m7tm1 crm9m8 crm9m8c4 crm9m8ce1 crm9m8ce2 crm9m8ce3 crm9m8m10 crm9m8tm0 crm9m8tm1 crm9nb crm9nbc4 crm9nbce1 crm9nbce2 crm9nbce3 crm9nbm10 crm9nbtm0 crm9nbtm1 crm9nd crm9ndc4 crm9ndce1 crm9ndce2 crm9ndce3 crm9ndm10 crm9ndtm0 crm9ndtm1 crm9p crm9pc4 crm9pce1 crm9pce2 crm9pce3 crm9pd crm9pdc4 crm9pdce1 crm9pdce2 crm9pdce3 crm9pdm10 crm9pdtm0 crm9pdtm1 crm9pm10 crm9ptm0 crm9ptm1 crp crpfc4 crpfce1 crpfce2 crpfce3 crpfm0 crpfm0_tcn crpfm1 crpfm10 crpfm1_tcn crpfm2 crpfm2_tcn crpfm3 crpfm4 crpfm5 crpfm6 crpfm7 crpfm8 crpfm9 crpftm0 crpftm1 crpfxndm0_tcn crpfxndm0_tcnxtcn crpfxndm1_tcn crpfxndm1_tcnxtcn crpfxndm2_tcn crpfxndm2_tcnxtcn crpnb crpnbc4 crpnbce1 crpnbce2 crpnbce3 crpnbm0 crpnbm0_tcn crpnbm1 crpnbm10 crpnbm1_tcn crpnbm2 crpnbm2_tcn crpnbm3 crpnbm4 crpnbm5 crpnbm6 crpnbm7 crpnbm8 crpnbm9 crpnbtm0 crpnbtm1 crpnbxpdm0_tcn crpnbxpdm0_tcnxtcn crpnbxpdm1_tcn crpnbxpdm1_tcnxtcn crpnbxpdm2_tcn crpnbxpdm2_tcnxtcn crpnd crpndc4 crpndce1 crpndce2 crpndce3 crpndm0 crpndm0_tcn crpndm1 crpndm10 crpndm1_tcn crpndm2 crpndm2_tcn crpndm3 crpndm4 crpndm5 crpndm6 crpndm7 crpndm8 crpndm9 crpndtm0 crpndtm1 crppd crppdc4 crppdce1 crppdce2 crppdce3 crppdm0 crppdm0_tcn crppdm1 crppdm10 crppdm1_tcn crppdm2 crppdm2_tcn crppdm3 crppdm4 crppdm5 crppdm6 crppdm7 crppdm8 crppdm9 crppdtm0 crppdtm1 crtcnf crtcnfm0 crtcnfm0_p crtcnfm1 crtcnfm1_p crtcnfm2 crtcnfm2_p crtcnfm3 crtcnnb crtcnnbm0 crtcnnbm0_p crtcnnbm1 crtcnnbm1_p crtcnnbm2 crtcnnbm2_p crtcnnbm3 crtcnnd crtcnndm0 crtcnndm0_p crtcnndm1 crtcnndm1_p crtcnndm2 crtcnndm2_p crtcnndm3 crtcnpd crtcnpdm0 crtcnpdm0_p crtcnpdm1 crtcnpdm1_p crtcnpdm2 crtcnpdm2_p crtcnpdm3 crtm0f crtm0fc4 crtm0fce1 crtm0fce2 crtm0fce3 crtm0ftm1 crtm0m0 crtm0m0c4 crtm0m0ce1 crtm0m0ce2 crtm0m0ce3 crtm0m0tm1 crtm0m1 crtm0m10 crtm0m10c4 crtm0m10ce1 crtm0m10ce2 crtm0m10ce3 crtm0m10tm1 crtm0m1c4 crtm0m1ce1 crtm0m1ce2 crtm0m1ce3 crtm0m1tm1 crtm0m2 crtm0m2c4 crtm0m2ce1 crtm0m2ce2 crtm0m2ce3 crtm0m2tm1 crtm0m3 crtm0m3c4 crtm0m3ce1 crtm0m3ce2 crtm0m3ce3 crtm0m3tm1 crtm0m4 crtm0m4c4 crtm0m4ce1 crtm0m4ce2 crtm0m4ce3 crtm0m4tm1 crtm0m5 crtm0m5c4 crtm0m5ce1 crtm0m5ce2 crtm0m5ce3 crtm0m5tm1 crtm0m6 crtm0m6c4 crtm0m6ce1 crtm0m6ce2 crtm0m6ce3 crtm0m6tm1 crtm0m7 crtm0m7c4 crtm0m7ce1 crtm0m7ce2 crtm0m7ce3 crtm0m7tm1 crtm0m8 crtm0m8c4 crtm0m8ce1 crtm0m8ce2 crtm0m8ce3 crtm0m8tm1 crtm0m9 crtm0m9c4 crtm0m9ce1 crtm0m9ce2 crtm0m9ce3 crtm0m9tm1 crtm0nb crtm0nbc4 crtm0nbce1 crtm0nbce2 crtm0nbce3 crtm0nbtm1 crtm0nd crtm0ndc4 crtm0ndce1 crtm0ndce2 crtm0ndce3 crtm0ndtm1 crtm0p crtm0pc4 crtm0pce1 crtm0pce2 crtm0pce3 crtm0pd crtm0pdc4 crtm0pdce1 crtm0pdce2 crtm0pdce3 crtm0pdtm1 crtm0ptm1 crtm1ce1 crtm1ce1c4 crtm1ce2 crtm1ce2c4 crtm1ce3 crtm1ce3c4 crtm1f crtm1fc4 crtm1m0 crtm1m0c4 crtm1m1 crtm1m10 crtm1m10c4 crtm1m1c4 crtm1m2 crtm1m2c4 crtm1m3 crtm1m3c4 crtm1m4 crtm1m4c4 crtm1m5 crtm1m5c4 crtm1m6 crtm1m6c4 crtm1m7 crtm1m7c4 crtm1m8 crtm1m8c4 crtm1m9 crtm1m9c4 crtm1nb crtm1nbc4 crtm1nd crtm1ndc4 crtm1p crtm1pc4 crtm1pd crtm1pdc4 crtm1tm0 crtm1tm0c4}
    set dio_model_names				{pdio_hia18_mac ndio_hia18_mac ndio_mac pdio_hia12_mac esd6phdjwrx017u_pcell_8 esd6phdjwrx060u_pcell_9 djn djnw djp e8xdjnesd_prim e8xdjpesd_prim}
    set res_model_names				{tfr_prim rmsp rhim n6rtc_oscunit n6rtc_reshmeg n6rtc_esd n6rtc_in n6rtc_sparebuff b88tfrbody_prim g80xlptfrdac_prim rc4ce1 rc4ce2 rc4ce3 rc4ce3c4s rc4f rc4m0 rc4m1 rc4m10 rc4m2 rc4m3 rc4m4 rc4m5 rc4m6 rc4m7 rc4m8 rc4m9 rc4nb rc4nd rc4p rc4pd rc4tm0 rc4tm1 rc4tm1c4s rce1f rce1fc4 rce1ftm1 rce1m0 rce1m0c4 rce1m0tm1 rce1m1 rce1m10 rce1m10c4 rce1m10tm1 rce1m1c4 rce1m1tm1 rce1m2 rce1m2c4 rce1m2tm1 rce1m3 rce1m3c4 rce1m3tm1 rce1m4 rce1m4c4 rce1m4tm1 rce1m5 rce1m5c4 rce1m5tm1 rce1m6 rce1m6c4 rce1m6tm1 rce1m7 rce1m7c4 rce1m7tm1 rce1m8 rce1m8c4 rce1m8tm1 rce1m9 rce1m9c4 rce1m9tm1 rce1nb rce1nbc4 rce1nbtm1 rce1nd rce1ndc4 rce1ndtm1 rce1p rce1pc4 rce1pd rce1pdc4 rce1pdtm1 rce1ptm1 rce1tm0 rce1tm0c4 rce1tm0tm1 rce2f rce2fc4 rce2ftm1 rce2m0 rce2m0c4 rce2m0tm1 rce2m1 rce2m10 rce2m10c4 rce2m10tm1 rce2m1c4 rce2m1tm1 rce2m2 rce2m2c4 rce2m2tm1 rce2m3 rce2m3c4 rce2m3tm1 rce2m4 rce2m4c4 rce2m4tm1 rce2m5 rce2m5c4 rce2m5tm1 rce2m6 rce2m6c4 rce2m6tm1 rce2m7 rce2m7c4 rce2m7tm1 rce2m8 rce2m8c4 rce2m8tm1 rce2m9 rce2m9c4 rce2m9tm1 rce2nb rce2nbc4 rce2nbtm1 rce2nd rce2ndc4 rce2ndtm1 rce2p rce2pc4 rce2pd rce2pdc4 rce2pdtm1 rce2ptm1 rce2tm0 rce2tm0c4 rce2tm0tm1 rce3f rce3fc4 rce3ftm1 rce3m0 rce3m0c4 rce3m0tm1 rce3m1 rce3m10 rce3m10c4 rce3m10tm1 rce3m1c4 rce3m1tm1 rce3m2 rce3m2c4 rce3m2tm1 rce3m3 rce3m3c4 rce3m3tm1 rce3m4 rce3m4c4 rce3m4tm1 rce3m5 rce3m5c4 rce3m5tm1 rce3m6 rce3m6c4 rce3m6tm1 rce3m7 rce3m7c4 rce3m7tm1 rce3m8 rce3m8c4 rce3m8tm1 rce3m9 rce3m9c4 rce3m9tm1 rce3nb rce3nbc4 rce3nbtm1 rce3nd rce3ndc4 rce3ndtm1 rce3p rce3pc4 rce3pd rce3pdc4 rce3pdtm1 rce3ptm1 rce3tm0 rce3tm0c4 rce3tm0tm1 rm0f rm0fc4 rm0fce1 rm0fce2 rm0fce3 rm0fm1 rm0fm10 rm0fm2 rm0fm3 rm0fm4 rm0fm5 rm0fm6 rm0fm7 rm0fm8 rm0fm9 rm0ftm0 rm0ftm1 rm0nb rm0nbc4 rm0nbce1 rm0nbce2 rm0nbce3 rm0nbm1 rm0nbm10 rm0nbm2 rm0nbm3 rm0nbm4 rm0nbm5 rm0nbm6 rm0nbm7 rm0nbm8 rm0nbm9 rm0nbtm0 rm0nbtm1 rm0nd rm0ndc4 rm0ndce1 rm0ndce2 rm0ndce3 rm0ndm1 rm0ndm10 rm0ndm2 rm0ndm3 rm0ndm4 rm0ndm5 rm0ndm6 rm0ndm7 rm0ndm8 rm0ndm9 rm0ndtm0 rm0ndtm1 rm0p rm0pc4 rm0pce1 rm0pce2 rm0pce3 rm0pd rm0pdc4 rm0pdce1 rm0pdce2 rm0pdce3 rm0pdm1 rm0pdm10 rm0pdm2 rm0pdm3 rm0pdm4 rm0pdm5 rm0pdm6 rm0pdm7 rm0pdm8 rm0pdm9 rm0pdtm0 rm0pdtm1 rm0pm1 rm0pm10 rm0pm2 rm0pm3 rm0pm4 rm0pm5 rm0pm6 rm0pm7 rm0pm8 rm0pm9 rm0ptm0 rm0ptm1 rm0tcn rm0tcnm1 rm0tcnm2 rm0tcnm3 rm0tcnm4 rm10f rm10fc4 rm10fce1 rm10fce2 rm10fce3 rm10ftm0 rm10ftm1 rm10m0 rm10m0c4 rm10m0ce1 rm10m0ce2 rm10m0ce3 rm10m0tm0 rm10m0tm1 rm10m1 rm10m1c4 rm10m1ce1 rm10m1ce2 rm10m1ce3 rm10m1tm0 rm10m1tm1 rm10m2 rm10m2c4 rm10m2ce1 rm10m2ce2 rm10m2ce3 rm10m2tm0 rm10m2tm1 rm10m3 rm10m3c4 rm10m3ce1 rm10m3ce2 rm10m3ce3 rm10m3tm0 rm10m3tm1 rm10m4 rm10m4c4 rm10m4ce1 rm10m4ce2 rm10m4ce3 rm10m4tm0 rm10m4tm1 rm10m5 rm10m5c4 rm10m5ce1 rm10m5ce2 rm10m5ce3 rm10m5tm0 rm10m5tm1 rm10m6 rm10m6c4 rm10m6ce1 rm10m6ce2 rm10m6ce3 rm10m6tm0 rm10m6tm1 rm10m7 rm10m7c4 rm10m7ce1 rm10m7ce2 rm10m7ce3 rm10m7tm0 rm10m7tm1 rm10m8 rm10m8c4 rm10m8ce1 rm10m8ce2 rm10m8ce3 rm10m8tm0 rm10m8tm1 rm10m9 rm10m9c4 rm10m9ce1 rm10m9ce2 rm10m9ce3 rm10m9tm0 rm10m9tm1 rm10nb rm10nbc4 rm10nbce1 rm10nbce2 rm10nbce3 rm10nbtm0 rm10nbtm1 rm10nd rm10ndc4 rm10ndce1 rm10ndce2 rm10ndce3 rm10ndtm0 rm10ndtm1 rm10p rm10pc4 rm10pce1 rm10pce2 rm10pce3 rm10pd rm10pdc4 rm10pdce1 rm10pdce2 rm10pdce3 rm10pdtm0 rm10pdtm1 rm10ptm0 rm10ptm1 rm1f rm1fc4 rm1fce1 rm1fce2 rm1fce3 rm1fm10 rm1fm2 rm1fm3 rm1fm4 rm1fm5 rm1fm6 rm1fm7 rm1fm8 rm1fm9 rm1ftm0 rm1ftm1 rm1m0 rm1m0c4 rm1m0ce1 rm1m0ce2 rm1m0ce3 rm1m0m10 rm1m0m2 rm1m0m3 rm1m0m4 rm1m0m5 rm1m0m6 rm1m0m7 rm1m0m8 rm1m0m9 rm1m0tm0 rm1m0tm1 rm1nb rm1nbc4 rm1nbce1 rm1nbce2 rm1nbce3 rm1nbm10 rm1nbm2 rm1nbm3 rm1nbm4 rm1nbm5 rm1nbm6 rm1nbm7 rm1nbm8 rm1nbm9 rm1nbtm0 rm1nbtm1 rm1nd rm1ndc4 rm1ndce1 rm1ndce2 rm1ndce3 rm1ndm10 rm1ndm2 rm1ndm3 rm1ndm4 rm1ndm5 rm1ndm6 rm1ndm7 rm1ndm8 rm1ndm9 rm1ndtm0 rm1ndtm1 rm1p rm1pc4 rm1pce1 rm1pce2 rm1pce3 rm1pd rm1pdc4 rm1pdce1 rm1pdce2 rm1pdce3 rm1pdm10 rm1pdm2 rm1pdm3 rm1pdm4 rm1pdm5 rm1pdm6 rm1pdm7 rm1pdm8 rm1pdm9 rm1pdtm0 rm1pdtm1 rm1pm10 rm1pm2 rm1pm3 rm1pm4 rm1pm5 rm1pm6 rm1pm7 rm1pm8 rm1pm9 rm1ptm0 rm1ptm1 rm1tcn rm1tcnm2 rm1tcnm3 rm1tcnm4 rm1tcnm5 rm2f rm2fc4 rm2fce1 rm2fce2 rm2fce3 rm2fm10 rm2fm3 rm2fm4 rm2fm5 rm2fm6 rm2fm7 rm2fm8 rm2fm9 rm2ftm0 rm2ftm1 rm2m0 rm2m0c4 rm2m0ce1 rm2m0ce2 rm2m0ce3 rm2m0m10 rm2m0m3 rm2m0m4 rm2m0m5 rm2m0m6 rm2m0m7 rm2m0m8 rm2m0m9 rm2m0tm0 rm2m0tm1 rm2m1 rm2m1c4 rm2m1ce1 rm2m1ce2 rm2m1ce3 rm2m1m10 rm2m1m3 rm2m1m4 rm2m1m5 rm2m1m6 rm2m1m7 rm2m1m8 rm2m1m9 rm2m1tm0 rm2m1tm1 rm2nb rm2nbc4 rm2nbce1 rm2nbce2 rm2nbce3 rm2nbm10 rm2nbm3 rm2nbm4 rm2nbm5 rm2nbm6 rm2nbm7 rm2nbm8 rm2nbm9 rm2nbtm0 rm2nbtm1 rm2nd rm2ndc4 rm2ndce1 rm2ndce2 rm2ndce3 rm2ndm10 rm2ndm3 rm2ndm4 rm2ndm5 rm2ndm6 rm2ndm7 rm2ndm8 rm2ndm9 rm2ndtm0 rm2ndtm1 rm2p rm2pc4 rm2pce1 rm2pce2 rm2pce3 rm2pd rm2pdc4 rm2pdce1 rm2pdce2 rm2pdce3 rm2pdm10 rm2pdm3 rm2pdm4 rm2pdm5 rm2pdm6 rm2pdm7 rm2pdm8 rm2pdm9 rm2pdtm0 rm2pdtm1 rm2pm10 rm2pm3 rm2pm4 rm2pm5 rm2pm6 rm2pm7 rm2pm8 rm2pm9 rm2ptm0 rm2ptm1 rm2tcn rm2tcnm3 rm2tcnm4 rm2tcnm5 rm2tcnm6 rm3f rm3fc4 rm3fce1 rm3fce2 rm3fce3 rm3fm10 rm3fm4 rm3fm5 rm3fm6 rm3fm7 rm3fm8 rm3fm9 rm3ftm0 rm3ftm1 rm3m0 rm3m0c4 rm3m0ce1 rm3m0ce2 rm3m0ce3 rm3m0m10 rm3m0m4 rm3m0m5 rm3m0m6 rm3m0m7 rm3m0m8 rm3m0m9 rm3m0tm0 rm3m0tm1 rm3m1 rm3m1c4 rm3m1ce1 rm3m1ce2 rm3m1ce3 rm3m1m10 rm3m1m4 rm3m1m5 rm3m1m6 rm3m1m7 rm3m1m8 rm3m1m9 rm3m1tm0 rm3m1tm1 rm3m2 rm3m2c4 rm3m2ce1 rm3m2ce2 rm3m2ce3 rm3m2m10 rm3m2m4 rm3m2m5 rm3m2m6 rm3m2m7 rm3m2m8 rm3m2m9 rm3m2tm0 rm3m2tm1 rm3nb rm3nbc4 rm3nbce1 rm3nbce2 rm3nbce3 rm3nbm10 rm3nbm4 rm3nbm5 rm3nbm6 rm3nbm7 rm3nbm8 rm3nbm9 rm3nbtm0 rm3nbtm1 rm3nd rm3ndc4 rm3ndce1 rm3ndce2 rm3ndce3 rm3ndm10 rm3ndm4 rm3ndm5 rm3ndm6 rm3ndm7 rm3ndm8 rm3ndm9 rm3ndtm0 rm3ndtm1 rm3p rm3pc4 rm3pce1 rm3pce2 rm3pce3 rm3pd rm3pdc4 rm3pdce1 rm3pdce2 rm3pdce3 rm3pdm10 rm3pdm4 rm3pdm5 rm3pdm6 rm3pdm7 rm3pdm8 rm3pdm9 rm3pdtm0 rm3pdtm1 rm3pm10 rm3pm4 rm3pm5 rm3pm6 rm3pm7 rm3pm8 rm3pm9 rm3ptm0 rm3ptm1 rm3tcn rm3tcnm4 rm3tcnm5 rm3tcnm6 rm3tcnm7 rm4f rm4fc4 rm4fce1 rm4fce2 rm4fce3 rm4fm10 rm4fm5 rm4fm6 rm4fm7 rm4fm8 rm4fm9 rm4ftm0 rm4ftm1 rm4m0 rm4m0c4 rm4m0ce1 rm4m0ce2 rm4m0ce3 rm4m0m10 rm4m0m5 rm4m0m6 rm4m0m7 rm4m0m8 rm4m0m9 rm4m0tm0 rm4m0tm1 rm4m1 rm4m1c4 rm4m1ce1 rm4m1ce2 rm4m1ce3 rm4m1m10 rm4m1m5 rm4m1m6 rm4m1m7 rm4m1m8 rm4m1m9 rm4m1tm0 rm4m1tm1 rm4m2 rm4m2c4 rm4m2ce1 rm4m2ce2 rm4m2ce3 rm4m2m10 rm4m2m5 rm4m2m6 rm4m2m7 rm4m2m8 rm4m2m9 rm4m2tm0 rm4m2tm1 rm4m3 rm4m3c4 rm4m3ce1 rm4m3ce2 rm4m3ce3 rm4m3m10 rm4m3m5 rm4m3m6 rm4m3m7 rm4m3m8 rm4m3m9 rm4m3tm0 rm4m3tm1 rm4nb rm4nbc4 rm4nbce1 rm4nbce2 rm4nbce3 rm4nbm10 rm4nbm5 rm4nbm6 rm4nbm7 rm4nbm8 rm4nbm9 rm4nbtm0 rm4nbtm1 rm4nd rm4ndc4 rm4ndce1 rm4ndce2 rm4ndce3 rm4ndm10 rm4ndm5 rm4ndm6 rm4ndm7 rm4ndm8 rm4ndm9 rm4ndtm0 rm4ndtm1 rm4p rm4pc4 rm4pce1 rm4pce2 rm4pce3 rm4pd rm4pdc4 rm4pdce1 rm4pdce2 rm4pdce3 rm4pdm10 rm4pdm5 rm4pdm6 rm4pdm7 rm4pdm8 rm4pdm9 rm4pdtm0 rm4pdtm1 rm4pm10 rm4pm5 rm4pm6 rm4pm7 rm4pm8 rm4pm9 rm4ptm0 rm4ptm1 rm5f rm5fc4 rm5fce1 rm5fce2 rm5fce3 rm5fm10 rm5fm6 rm5fm7 rm5fm8 rm5fm9 rm5ftm0 rm5ftm1 rm5m0 rm5m0c4 rm5m0ce1 rm5m0ce2 rm5m0ce3 rm5m0m10 rm5m0m6 rm5m0m7 rm5m0m8 rm5m0m9 rm5m0tm0 rm5m0tm1 rm5m1 rm5m1c4 rm5m1ce1 rm5m1ce2 rm5m1ce3 rm5m1m10 rm5m1m6 rm5m1m7 rm5m1m8 rm5m1m9 rm5m1tm0 rm5m1tm1 rm5m2 rm5m2c4 rm5m2ce1 rm5m2ce2 rm5m2ce3 rm5m2m10 rm5m2m6 rm5m2m7 rm5m2m8 rm5m2m9 rm5m2tm0 rm5m2tm1 rm5m3 rm5m3c4 rm5m3ce1 rm5m3ce2 rm5m3ce3 rm5m3m10 rm5m3m6 rm5m3m7 rm5m3m8 rm5m3m9 rm5m3tm0 rm5m3tm1 rm5m4 rm5m4c4 rm5m4ce1 rm5m4ce2 rm5m4ce3 rm5m4m10 rm5m4m6 rm5m4m7 rm5m4m8 rm5m4m9 rm5m4tm0 rm5m4tm1 rm5nb rm5nbc4 rm5nbce1 rm5nbce2 rm5nbce3 rm5nbm10 rm5nbm6 rm5nbm7 rm5nbm8 rm5nbm9 rm5nbtm0 rm5nbtm1 rm5nd rm5ndc4 rm5ndce1 rm5ndce2 rm5ndce3 rm5ndm10 rm5ndm6 rm5ndm7 rm5ndm8 rm5ndm9 rm5ndtm0 rm5ndtm1 rm5p rm5pc4 rm5pce1 rm5pce2 rm5pce3 rm5pd rm5pdc4 rm5pdce1 rm5pdce2 rm5pdce3 rm5pdm10 rm5pdm6 rm5pdm7 rm5pdm8 rm5pdm9 rm5pdtm0 rm5pdtm1 rm5pm10 rm5pm6 rm5pm7 rm5pm8 rm5pm9 rm5ptm0 rm5ptm1 rm6f rm6fc4 rm6fce1 rm6fce2 rm6fce3 rm6fm10 rm6fm7 rm6fm8 rm6fm9 rm6ftm0 rm6ftm1 rm6m0 rm6m0c4 rm6m0ce1 rm6m0ce2 rm6m0ce3 rm6m0m10 rm6m0m7 rm6m0m8 rm6m0m9 rm6m0tm0 rm6m0tm1 rm6m1 rm6m1c4 rm6m1ce1 rm6m1ce2 rm6m1ce3 rm6m1m10 rm6m1m7 rm6m1m8 rm6m1m9 rm6m1tm0 rm6m1tm1 rm6m2 rm6m2c4 rm6m2ce1 rm6m2ce2 rm6m2ce3 rm6m2m10 rm6m2m7 rm6m2m8 rm6m2m9 rm6m2tm0 rm6m2tm1 rm6m3 rm6m3c4 rm6m3ce1 rm6m3ce2 rm6m3ce3 rm6m3m10 rm6m3m7 rm6m3m8 rm6m3m9 rm6m3tm0 rm6m3tm1 rm6m4 rm6m4c4 rm6m4ce1 rm6m4ce2 rm6m4ce3 rm6m4m10 rm6m4m7 rm6m4m8 rm6m4m9 rm6m4tm0 rm6m4tm1 rm6m5 rm6m5c4 rm6m5ce1 rm6m5ce2 rm6m5ce3 rm6m5m10 rm6m5m7 rm6m5m8 rm6m5m9 rm6m5tm0 rm6m5tm1 rm6nb rm6nbc4 rm6nbce1 rm6nbce2 rm6nbce3 rm6nbm10 rm6nbm7 rm6nbm8 rm6nbm9 rm6nbtm0 rm6nbtm1 rm6nd rm6ndc4 rm6ndce1 rm6ndce2 rm6ndce3 rm6ndm10 rm6ndm7 rm6ndm8 rm6ndm9 rm6ndtm0 rm6ndtm1 rm6p rm6pc4 rm6pce1 rm6pce2 rm6pce3 rm6pd rm6pdc4 rm6pdce1 rm6pdce2 rm6pdce3 rm6pdm10 rm6pdm7 rm6pdm8 rm6pdm9 rm6pdtm0 rm6pdtm1 rm6pm10 rm6pm7 rm6pm8 rm6pm9 rm6ptm0 rm6ptm1 rm7f rm7fc4 rm7fce1 rm7fce2 rm7fce3 rm7fm10 rm7fm8 rm7fm9 rm7ftm0 rm7ftm1 rm7m0 rm7m0c4 rm7m0ce1 rm7m0ce2 rm7m0ce3 rm7m0m10 rm7m0m8 rm7m0m9 rm7m0tm0 rm7m0tm1 rm7m1 rm7m1c4 rm7m1ce1 rm7m1ce2 rm7m1ce3 rm7m1m10 rm7m1m8 rm7m1m9 rm7m1tm0 rm7m1tm1 rm7m2 rm7m2c4 rm7m2ce1 rm7m2ce2 rm7m2ce3 rm7m2m10 rm7m2m8 rm7m2m9 rm7m2tm0 rm7m2tm1 rm7m3 rm7m3c4 rm7m3ce1 rm7m3ce2 rm7m3ce3 rm7m3m10 rm7m3m8 rm7m3m9 rm7m3tm0 rm7m3tm1 rm7m4 rm7m4c4 rm7m4ce1 rm7m4ce2 rm7m4ce3 rm7m4m10 rm7m4m8 rm7m4m9 rm7m4tm0 rm7m4tm1 rm7m5 rm7m5c4 rm7m5ce1 rm7m5ce2 rm7m5ce3 rm7m5m10 rm7m5m8 rm7m5m9 rm7m5tm0 rm7m5tm1 rm7m6 rm7m6c4 rm7m6ce1 rm7m6ce2 rm7m6ce3 rm7m6m10 rm7m6m8 rm7m6m9 rm7m6tm0 rm7m6tm1 rm7nb rm7nbc4 rm7nbce1 rm7nbce2 rm7nbce3 rm7nbm10 rm7nbm8 rm7nbm9 rm7nbtm0 rm7nbtm1 rm7nd rm7ndc4 rm7ndce1 rm7ndce2 rm7ndce3 rm7ndm10 rm7ndm8 rm7ndm9 rm7ndtm0 rm7ndtm1 rm7p rm7pc4 rm7pce1 rm7pce2 rm7pce3 rm7pd rm7pdc4 rm7pdce1 rm7pdce2 rm7pdce3 rm7pdm10 rm7pdm8 rm7pdm9 rm7pdtm0 rm7pdtm1 rm7pm10 rm7pm8 rm7pm9 rm7ptm0 rm7ptm1 rm8f rm8fc4 rm8fce1 rm8fce2 rm8fce3 rm8fm10 rm8fm9 rm8ftm0 rm8ftm1 rm8m0 rm8m0c4 rm8m0ce1 rm8m0ce2 rm8m0ce3 rm8m0m10 rm8m0m9 rm8m0tm0 rm8m0tm1 rm8m1 rm8m1c4 rm8m1ce1 rm8m1ce2 rm8m1ce3 rm8m1m10 rm8m1m9 rm8m1tm0 rm8m1tm1 rm8m2 rm8m2c4 rm8m2ce1 rm8m2ce2 rm8m2ce3 rm8m2m10 rm8m2m9 rm8m2tm0 rm8m2tm1 rm8m3 rm8m3c4 rm8m3ce1 rm8m3ce2 rm8m3ce3 rm8m3m10 rm8m3m9 rm8m3tm0 rm8m3tm1 rm8m4 rm8m4c4 rm8m4ce1 rm8m4ce2 rm8m4ce3 rm8m4m10 rm8m4m9 rm8m4tm0 rm8m4tm1 rm8m5 rm8m5c4 rm8m5ce1 rm8m5ce2 rm8m5ce3 rm8m5m10 rm8m5m9 rm8m5tm0 rm8m5tm1 rm8m6 rm8m6c4 rm8m6ce1 rm8m6ce2 rm8m6ce3 rm8m6m10 rm8m6m9 rm8m6tm0 rm8m6tm1 rm8m7 rm8m7c4 rm8m7ce1 rm8m7ce2 rm8m7ce3 rm8m7m10 rm8m7m9 rm8m7tm0 rm8m7tm1 rm8nb rm8nbc4 rm8nbce1 rm8nbce2 rm8nbce3 rm8nbm10 rm8nbm9 rm8nbtm0 rm8nbtm1 rm8nd rm8ndc4 rm8ndce1 rm8ndce2 rm8ndce3 rm8ndm10 rm8ndm9 rm8ndtm0 rm8ndtm1 rm8p rm8pc4 rm8pce1 rm8pce2 rm8pce3 rm8pd rm8pdc4 rm8pdce1 rm8pdce2 rm8pdce3 rm8pdm10 rm8pdm9 rm8pdtm0 rm8pdtm1 rm8pm10 rm8pm9 rm8ptm0 rm8ptm1 rm9f rm9fc4 rm9fce1 rm9fce2 rm9fce3 rm9fm10 rm9ftm0 rm9ftm1 rm9m0 rm9m0c4 rm9m0ce1 rm9m0ce2 rm9m0ce3 rm9m0m10 rm9m0tm0 rm9m0tm1 rm9m1 rm9m1c4 rm9m1ce1 rm9m1ce2 rm9m1ce3 rm9m1m10 rm9m1tm0 rm9m1tm1 rm9m2 rm9m2c4 rm9m2ce1 rm9m2ce2 rm9m2ce3 rm9m2m10 rm9m2tm0 rm9m2tm1 rm9m3 rm9m3c4 rm9m3ce1 rm9m3ce2 rm9m3ce3 rm9m3m10 rm9m3tm0 rm9m3tm1 rm9m4 rm9m4c4 rm9m4ce1 rm9m4ce2 rm9m4ce3 rm9m4m10 rm9m4tm0 rm9m4tm1 rm9m5 rm9m5c4 rm9m5ce1 rm9m5ce2 rm9m5ce3 rm9m5m10 rm9m5tm0 rm9m5tm1 rm9m6 rm9m6c4 rm9m6ce1 rm9m6ce2 rm9m6ce3 rm9m6m10 rm9m6tm0 rm9m6tm1 rm9m7 rm9m7c4 rm9m7ce1 rm9m7ce2 rm9m7ce3 rm9m7m10 rm9m7tm0 rm9m7tm1 rm9m8 rm9m8c4 rm9m8ce1 rm9m8ce2 rm9m8ce3 rm9m8m10 rm9m8tm0 rm9m8tm1 rm9nb rm9nbc4 rm9nbce1 rm9nbce2 rm9nbce3 rm9nbm10 rm9nbtm0 rm9nbtm1 rm9nd rm9ndc4 rm9ndce1 rm9ndce2 rm9ndce3 rm9ndm10 rm9ndtm0 rm9ndtm1 rm9p rm9pc4 rm9pce1 rm9pce2 rm9pce3 rm9pd rm9pdc4 rm9pdce1 rm9pdce2 rm9pdce3 rm9pdm10 rm9pdtm0 rm9pdtm1 rm9pm10 rm9ptm0 rm9ptm1 rp rpfc4 rpfce1 rpfce2 rpfce3 rpfm0 rpfm0_tcn rpfm1 rpfm10 rpfm1_tcn rpfm2 rpfm2_tcn rpfm3 rpfm4 rpfm5 rpfm6 rpfm7 rpfm8 rpfm9 rpftm0 rpftm1 rpnb rpnbc4 rpnbce1 rpnbce2 rpnbce3 rpnbm0 rpnbm0_tcn rpnbm1 rpnbm10 rpnbm1_tcn rpnbm2 rpnbm2_tcn rpnbm3 rpnbm4 rpnbm5 rpnbm6 rpnbm7 rpnbm8 rpnbm9 rpnbtm0 rpnbtm1 rpnd rpndc4 rpndce1 rpndce2 rpndce3 rpndm0 rpndm0_tcn rpndm1 rpndm10 rpndm1_tcn rpndm2 rpndm2_tcn rpndm3 rpndm4 rpndm5 rpndm6 rpndm7 rpndm8 rpndm9 rpndtm0 rpndtm1 rppd rppdc4 rppdce1 rppdce2 rppdce3 rppdm0 rppdm0_tcn rppdm1 rppdm10 rppdm1_tcn rppdm2 rppdm2_tcn rppdm3 rppdm4 rppdm5 rppdm6 rppdm7 rppdm8 rppdm9 rppdtm0 rppdtm1 rtcnf rtcnfm0 rtcnfm0_p rtcnfm1 rtcnfm1_p rtcnfm2 rtcnfm2_p rtcnfm3 rtcnnb rtcnnbm0 rtcnnbm0_p rtcnnbm1 rtcnnbm1_p rtcnnbm2 rtcnnbm2_p rtcnnbm3 rtcnnd rtcnndm0 rtcnndm0_p rtcnndm1 rtcnndm1_p rtcnndm2 rtcnndm2_p rtcnndm3 rtcnpd rtcnpdm0 rtcnpdm0_p rtcnpdm1 rtcnpdm1_p rtcnpdm2 rtcnpdm2_p rtcnpdm3 rtm0f rtm0fc4 rtm0fce1 rtm0fce2 rtm0fce3 rtm0ftm1 rtm0m0 rtm0m0c4 rtm0m0ce1 rtm0m0ce2 rtm0m0ce3 rtm0m0tm1 rtm0m1 rtm0m10 rtm0m10c4 rtm0m10ce1 rtm0m10ce2 rtm0m10ce3 rtm0m10tm1 rtm0m1c4 rtm0m1ce1 rtm0m1ce2 rtm0m1ce3 rtm0m1tm1 rtm0m2 rtm0m2c4 rtm0m2ce1 rtm0m2ce2 rtm0m2ce3 rtm0m2tm1 rtm0m3 rtm0m3c4 rtm0m3ce1 rtm0m3ce2 rtm0m3ce3 rtm0m3tm1 rtm0m4 rtm0m4c4 rtm0m4ce1 rtm0m4ce2 rtm0m4ce3 rtm0m4tm1 rtm0m5 rtm0m5c4 rtm0m5ce1 rtm0m5ce2 rtm0m5ce3 rtm0m5tm1 rtm0m6 rtm0m6c4 rtm0m6ce1 rtm0m6ce2 rtm0m6ce3 rtm0m6tm1 rtm0m7 rtm0m7c4 rtm0m7ce1 rtm0m7ce2 rtm0m7ce3 rtm0m7tm1 rtm0m8 rtm0m8c4 rtm0m8ce1 rtm0m8ce2 rtm0m8ce3 rtm0m8tm1 rtm0m9 rtm0m9c4 rtm0m9ce1 rtm0m9ce2 rtm0m9ce3 rtm0m9tm1 rtm0nb rtm0nbc4 rtm0nbce1 rtm0nbce2 rtm0nbce3 rtm0nbtm1 rtm0nd rtm0ndc4 rtm0ndce1 rtm0ndce2 rtm0ndce3 rtm0ndtm1 rtm0p rtm0pc4 rtm0pce1 rtm0pce2 rtm0pce3 rtm0pd rtm0pdc4 rtm0pdce1 rtm0pdce2 rtm0pdce3 rtm0pdtm1 rtm0ptm1 rtm1ce1 rtm1ce1c4 rtm1ce2 rtm1ce2c4 rtm1ce3 rtm1ce3c4 rtm1f rtm1fc4 rtm1m0 rtm1m0c4 rtm1m1 rtm1m10 rtm1m10c4 rtm1m1c4 rtm1m2 rtm1m2c4 rtm1m3 rtm1m3c4 rtm1m4 rtm1m4c4 rtm1m5 rtm1m5c4 rtm1m6 rtm1m6c4 rtm1m7 rtm1m7c4 rtm1m8 rtm1m8c4 rtm1m9 rtm1m9c4 rtm1nb rtm1nbc4 rtm1nd rtm1ndc4 rtm1p rtm1pc4 rtm1pd rtm1pdc4 rtm1tm0 rtm1tm0c4}
    set three_term_cap_model_names		{}
    set three_term_res_model_names		{}	
    set nmos_drn_gate_shorted_model_names	{}
    set pmos_drn_gate_shorted_model_names	{}
    set nmos_gate_src_shorted_model_names	{}
    set pmos_gate_src_shorted_model_names	{}

    foreach model_type {bjt_model_names cap_model_names dio_model_names nmos_model_names pmos_model_names res_model_names three_term_cap_model_names three_term_res_model_names nmos_drn_src_shorted_model_names pmos_drn_src_shorted_model_names nmos_drn_gate_shorted_model_names pmos_drn_gate_shorted_model_names  nmos_gate_src_shorted_model_names pmos_gate_src_shorted_model_names} {
    	set $model_type "[get_parameter $model_type] [string toupper [get_parameter $model_type]]"
    }

    #--------------------------------------------------------------------------------------------------------#
    # Power/Leakage Controls
    set enable_dc_leakage 1				;# =1 DC Leakage Estimation, =0 Tran
    set enable_cell_leakage_power_modeling 1		;# =1 Model cell_leakage_power Attribute
    set gate_leakage_time_scaling_factor 10		;# Use a larger window for Transient Leakage Estimation
    set liberty_fill_out_power_with zero		;# No Power Data? Fill Zero, Dont Copy Opposite Edge
    set model_default_power_arc 0			;# No Default Power Arcs
    set model_input_leakage_current 1			;# =1 Include Gate Leakage in NLPM Leakage Power
    set auto_fix_leakage_current 0
    set multicycle_leakage_initialization 1

    #--------------------------------------------------------------------------------------------------------#
    # Capacitance Controls
    #corner_type maxmin_type
    switch -regexp -matchvar capModeVar -- ${maxmin_type} {
      {min.*max} {
                        set model_pin_cap_calc ave
                        log_info "Corner Parsing:: maxmin=${capModeVar}, 'capacitance' Attribute forming metric='average'"
                }
      {max.*min} {
                        set model_pin_cap_calc ave
                        log_info "Corner Parsing:: maxmin=${capModeVar}, 'capacitance' Attribute forming metric='average'"
                }
      {min|max} {
                        set model_pin_cap_calc ${capModeVar}
                        log_info "Corner Parsing:: maxmin=${capModeVar}, 'capacitance' Attribute forming metric='${capModeVar}'"
                }
      default   {
                        set model_pin_cap_calc ave
                        log_info "Corner Parsing:: maxmin=${maxmin_type}, 'capacitance' Attribute metric='average'"
                }
    }
    set model_rise_fall_capacitance 1			;# =1 Model {rise|fall}_capacitance Attribute
    set model_rise_fall_capacitance_range 1		;# =1 Model {rise|fall}_capacitance_range Attribute
        
    #--------------------------------------------------------------------------------------------------------#
    ## Truning on Intel specific Max-cap methodology from Native SiliconSmart engine (Supported from SiliconSmart 2019 version)
    # Intel specific max-cap method
    if {[info exists ::env(INTEL_PDK)]} {
      set maxcap_from_autorange 1
    }

    #--------------------------------------------------------------------------------------------------------#
    # Constraint/MPW Controls
    set constraint_mode independent			;# Capture setup|hold independently
    set legacy_capload 0
    set max_constraint_iterations 100
    set model_default_constraints max
    set model_neg_constraint_chk 0
    set model_neg_constraint_sum 0			;# =0 Enable setup+hold>=0 Check, =1 Disable Check
    set model_neg_constraint_sum_margin 2e-12		
    set model_neg_constraint_sum_threshold 200e-12
    set constraint_pulse_cratering 1		
    set constraint_simulated_seed 0
    set mpw_table_dimensions 1				;# =1 1-D MPW Table, =2 2-D MPW Table, =0 Attribute
    set mpw_rail_threshold  0.9
    set simulator_bisection 1				;# =1 FineSim Native Bisection

    #--------------------------------------------------------------------------------------------------------#
    # CCST Controls
    set enable_hidden_ccs_cap 1
    set extrapolate_ccs_cin_slew 1
    set model_arc_and_pin_cap 1
    set ccst_glitch_check 1

    #--------------------------------------------------------------------------------------------------------#
    # CCSN Controls
    set liberty_ccsn_format v1				;# =v1 Current CCSN Format, =v2 Advanced CCSN Format
    set ccb_partition_pin_based 0			;# =1 Pin-Based Models for Custom/IO/HIP/Analog
    set ccsn_advanced_flow 0				;# =0 Regular Flow
    set ccsn_cmiller_check_mode 3
    set ccsn_model_passgate_ccb pin
    set ccsn_numsteps_voltage 29
    set ccsn_single_output_stage 1
    set ccsn_truncate_long_ccb_name 1
    set ccbs_for_input_driving_passgate 1
    set configure_all_states_for_ccb 1
    set model_is_propagating 1
    set ccsn_model_default_pin_based_models 1
    set ccsn_pin_based_model_mode 0
    set ccsn_enable_user_defined_ccb 1

    #--------------------------------------------------------------------------------------------------------#
    # LVF (Delay,Slew And Constraints) and Statistical Controls
    set lvf_format v1					;# =v1 Classical Format, =v2 Moment-Based Format
    set lvf_model_slew 1				;# =1 Need Transition LVF
    set statistical_enable_constraint_sensitivity 1	;# =1 Need Consraint LVF
    set statistical_enable_mpw_sensitivity 0		;# =1 Need MPW LVF
    set statistical_simulator_bisection 1
    set lvf_constraint_models {setup hold recovery removal asynch_recovery asynch_removal nochange_setup nochange_hold}
    set lvf_external_sampling 1
    set enable_mc_sweeps 1
    # Optimizations
    set enable_netlist_pruning 1
    set netlist_pruning_rc_method 1
    set lvf_param_abs_threshold 2.5e-13
    set lvf_param_rel_threshold 0.0025
    set statistical_avoid_screening_acquisition 0
    # LVF Delay/slew
    set statistical_two_sided_screening 1
    set statistical_screening_points {}
    set statistical_screening_tolerance 0.03
    set statistical_reduction_factor 0.6
    set model_interpolation_method legacy
    # LVF Constraint
    ##check after setting the boundry
    set statistical_constraint_screening_points {}
    set statistical_constraint_screening_tolerance 2e-12
    set_config_opt -type statistical_constraint statistical_two_sided_screening 0
    set_config_opt -type statistical_constraint constraint_resolution 2e-12
    set_config_opt -type statistical_constraint statistical_reduction_factor 1
    set_config_opt -type statistical_constraint model_interpolation_method nonlinear
    # LVF Mpw
    set statistical_mpw_screening_points 1
    set statistical_mpw_screening_tolerance 2e-12
    set_config_opt -type statistical_mpw statistical_two_sided_screening 0
    set_config_opt -type statistical_mpw constraint_resolution 2e-12
    set_config_opt -type statistical_mpw statistical_reduction_factor 0.5
    set_config_opt -type statistical_mpw model_interpolation_method linear
    # Moments
    set lvf_moment_model_mode 1
    # Others
    set lvf_sigma_scaling 0
    set lvf_check_output_log 0
    set lvf_zero_sigma_value 1e-14
    set lvf_zero_sigma_min 0
    set lvf_zero_sigma_auto_fix 2
    set lvf_ignore_moscap_devices 1
    set lvf_ignore_global_variation 1
    set lvf_union_parameters 1
    set_config_opt -type lvf enable_parallel_sweeps 1
    #set mc_file_simulator {$env(SILICONSMART_ROOT_DIR)/tcl/finesim_lvf_mc.sh}
    #set statistical_constraint_screening_points {7 56}
    #--------------------------------------------------------------------------------------------------------#
    # LVF to x-OCV Conversion
    set lvf_to_ocv_method mean
    set lvf_to_ocv_slew_indices {1 2 3 4}
    set lvf_to_aocv_clock_load_indices {5 6 7 8}
    set lvf_to_aocv_data_load_indices {5 6 7 8}
    set lvf_to_ocv_arc_info_comment 1
    #--------------------------------------------------------------------------------------------------------#
    # AOCV 
    set aocv_set_version 2
    set aocv_early_sigma -1
    set aocv_late_sigma 1
    set aocv_num_stages 50
    
    #--------------------------------------------------------------------------------------------------------#
    # IBIS Controls
    set ibis_c_comp_ac 0
    set ibis_c_comp_reorder 0
    set ibis_vt_curve_make_monotonic 1			;# =1 Make VT Curves Monotonic
    set ibis_clamping_curve_make_monotonic 1		;# =1 Make Clamp Curves Monotonic
    set ibis_above_rail_multiplier 1
    set ibis_below_rail_multiplier 1
    set ibis_rail_extrapolate_linear 0
    set ibis_enable_parallel_pvt 1
    set ibis_clamping_iv_num_points 90
    set ibis_nsamples 200
    set ibis_diff_pin_voltage_mode 0
    set ibis_source "SiliconSmart [lindex [get_version_info] 1]"
    set ibis_copyright {Copyright 1999-2020, Intel Corporation, All Rights Reserved.}
    set ibis_manufacturer {Intel Corporation}
    set ibis_disclaimer \
    {               This IBIS model is provided in connection with Intel
                products. No license, express or implied, by estoppel or
                otherwise, to any intellectual property rights is granted
                by this document. Except as provided in Intel's Terms and
                Conditions of Sale for such products, Intel assumes no
                liability whatsoever, and Intel disclaims any express or
                implied warranty, relating to sale and/or use of Intel
                products including liability or warranties relating to
                fitness for a particular purpose, merchantability, or
                infringement of any patent, copyright or other
                intellectual property right. Intel products are not
                intended for use in medical, life saving, or life
                sustaining applications.
    }
    set model_ecsm_threshold_pct 0

    #--------------------------------------------------------------------------------------------------------#
    # BUS/Bundle Controls
    set downto 1					;# =1 Model BUS Format downto=true
    set liberty_bus_pin_order increasing
    set model_bundle_bit_level 0
    set model_bus_bit_level 0
    set import_pins_as_bus all

    #--------------------------------------------------------------------------------------------------------#
    # Internal Node Arcs
    set configure_internal_node_arcs 1

    #--------------------------------------------------------------------------------------------------------#
    # Customize Instance Files
    set enable_custom_settings 0

    #--------------------------------------------------------------------------------------------------------#
    # Misc
    set archive_results 0
    set check_model_file 1
    set check_pins_in_netlist 0
    set enable_cache 1
    set enable_cache_auto_index 1
    set enable_fse_log 1
    set enable_status 0
    set gzip_init_files 1
    set sis_gzip_enable_for_lib 1 
    set read_gzip_netlist 1
    set model_extra_comments 1
    set model_failed_cells_in_lib 0
    set propagate_warnings 1
    set total_slew_multiplier 2
    set update_cache_last 0
    set port_info_in_deck 1
    set error_invalid_finesim_so_path 1
    set expand_pwl_with_parallel_sweeps 1
    set monitor_secondary_output_for_delay 0
    set aus_cleanup_redundant_states 1
    set mpp_simulator [subst {${hspicePath}}]

    #--------------------------------------------------------------------------------------------------------#
    # Specific Requirements
    set phased_inputs_mode abs
    set min_disk_space 200
    set initialization_cycles 1
    set separate_cell_initialization ic
    set external_separate_cell_initialization 1
    set_config_opt -type input_capacitance initialization_cycles 0
    set_config_opt -type min_period state_partitions none

    #--------------------------------------------------------------------------------------------------------#
    # lvf_mode 
    switch -regexp -matchvar lvfModeVar -- [get_config_opt lvf_mode] {
      {^\s*sba\s*$} 	{
      			log_info "In-case of lvf, the sba method will be used."
                        set_config_opt statistical_model_sigma_montecarlo 0
                        set_config_opt lvf_ml_mode 0
    			set_config_opt update_cache_last 1
                        }
      {^\s*sba_opt\s*$} {
      			log_info "lvf_mode:: Detected lvf_mode=${lvfModeVar}. Optimized SBA method"
                        set_config_opt statistical_model_sigma_montecarlo 0
                        set_config_opt lvf_ml_mode 0
    			set_config_opt update_cache_last 1
			set_config_opt enable_mc_sweeps 0
			set_config_opt statistical_model_sigma_montecarlo 0
			set_config_opt lvf_external_sampling 1
			set_config_opt statistical_avoid_screening_acquisition 0
			set_config_opt enable_netlist_pruning 1
			set_config_opt statistical_reduction_factor 0.6
			set_config_opt enable_parallel_sweeps 1
			set_config_opt -type {lvf} netlist_max_sweeps 50                                  ;# Change as per requirement

                        }
      {^\s*ml\s*$} 	{
      			log_info "lvf_mode:: Detected lvf_mode=${lvfModeVar}"
                        set_config_opt statistical_model_sigma_montecarlo 0
                        set_config_opt lvf_ml_mode 1
    			set_config_opt update_cache_last 1
                        }
      {^\s*ml_opt\s*$} 	{
      			log_info "lvf_mode:: Detected lvf_mode=${lvfModeVar}. Optimized ML method"
                        set_config_opt statistical_model_sigma_montecarlo 0
                        set_config_opt lvf_ml_mode 1
    			set_config_opt update_cache_last 1
			set_config_opt enable_mc_sweeps 0
			set_config_opt statistical_model_sigma_montecarlo 0
			set_config_opt lvf_external_sampling 1
			set_config_opt statistical_avoid_screening_acquisition 0
			set_config_opt enable_netlist_pruning 1
			set_config_opt statistical_reduction_factor 0.6
			set_config_opt enable_parallel_sweeps 1
			set_config_opt -type {lvf} netlist_max_sweeps 50                                  ;# Change as per requirement
                        }
      {^\s*ml_adaptive\s*$} 	{
      			log_info "lvf_mode:: Detected lvf_mode=${lvfModeVar}. Adaptive ML method"
                        set_config_opt statistical_model_sigma_montecarlo 0
                        set_config_opt lvf_ml_mode 1
			set_config_opt lvf_ml_adaptive 1 ;# Adaptive ML commands
    			set_config_opt update_cache_last 1
			set_config_opt enable_mc_sweeps 0
			set_config_opt statistical_model_sigma_montecarlo 0
			set_config_opt lvf_external_sampling 1
			set_config_opt statistical_avoid_screening_acquisition 0
			set_config_opt enable_netlist_pruning 1
			set_config_opt statistical_reduction_factor 0.6
			set_config_opt enable_parallel_sweeps 1
			set_config_opt -type {lvf} netlist_max_sweeps 50                                  ;# Change as per requirement
                        }
      {^\s*ml_hsphy\s*$} {
      			log_info "lvf_mode:: Detected lvf_mode=${lvfModeVar} for high speed circuits."
                        set_config_opt statistical_model_sigma_montecarlo 0
                        set_config_opt lvf_ml_mode 1
    			set_config_opt update_cache_last 1
			set_config_opt statistical_constraint_screening_tolerance 2e-13
			set_config_opt -type statistical_constraint constraint_resolution 5e-13
			set_config_opt lvf_zero_sigma_auto_fix 0
			set_config_opt enable_mc_sweeps 0
			set_config_opt statistical_model_sigma_montecarlo 0
			set_config_opt lvf_external_sampling 1
			set_config_opt statistical_avoid_screening_acquisition 0
			set_config_opt enable_netlist_pruning 1
			set_config_opt statistical_reduction_factor 0.6
			set_config_opt enable_parallel_sweeps 1
			set_config_opt -type {lvf} netlist_max_sweeps 50                                  ;# Change as per requirement
                        }			
      {^\s*mc5000\s*$} 	{
      			log_info "lvf_mode:: Detected lvf_mode=${lvfModeVar} : Montecarlo with 5000 sample and 3 sigma variation target"
			set_config_opt lvf_external_sampling 0
                        set_config_opt statistical_model_sigma_montecarlo 1
                        set_config_opt lvf_ml_mode 0
    			set_config_opt update_cache_last 1
    			set_config_opt min_disk_space 800
                        set_config_opt statistical_montecarlo_method percentile
                        set_config_opt statistical_montecarlo_percentile_sigma 3
                        set_config_opt statistical_montecarlo_sample_size 5000
                        set_config_opt -type {statistical_constraint statistical_mpw} statistical_montecarlo_sample_size 1000
    			set_config_opt cdpl_long_task_alert 36000
    			set_config_opt cdpl_task_max_lifespan 40000
    			set_config_opt cdpl_worker_timeout 900
    			set_config_opt char_engine_max_lifespan 40000
    			set_config_opt enable_netlist_pruning 1
    			set_config_opt statistical_avoid_screening_acquisition 1
			
			## Added to speedup the simulation)
			set_config_opt enable_parallel_sweeps 1
			set_config_opt enable_mc_sweeps 1
			set_config_opt netlist_max_sweeps 1
						
                        }
      {^\s*mc2500\s*$} 	{
      			log_info "lvf_mode:: Detected lvf_mode=${lvfModeVar} : Montecarlo with 2500 sample and 3 sigma variation target"
			set_config_opt lvf_external_sampling 0
                        set_config_opt statistical_model_sigma_montecarlo 1
                        set_config_opt lvf_ml_mode 0
    			set_config_opt update_cache_last 1
    			set_config_opt min_disk_space 800
                        set_config_opt statistical_montecarlo_method percentile
                        set_config_opt statistical_montecarlo_percentile_sigma 3
                        set_config_opt statistical_montecarlo_sample_size 2500
                        set_config_opt -type {statistical_constraint statistical_mpw} statistical_montecarlo_sample_size 1000
    			set_config_opt cdpl_long_task_alert 36000
    			set_config_opt cdpl_task_max_lifespan 40000
    			set_config_opt cdpl_worker_timeout 900
    			set_config_opt char_engine_max_lifespan 40000
    			set_config_opt enable_netlist_pruning 1
    			set_config_opt statistical_avoid_screening_acquisition 1
			
			## Added to speedup the simulation)
			set_config_opt enable_parallel_sweeps 1
			set_config_opt enable_mc_sweeps 1
			set_config_opt netlist_max_sweeps 1
						
                        }
			
      {^\s*mc1000\s*$} 	{
      			
			log_info "lvf_mode:: Detected lvf_mode=${lvfModeVar} : Montecarlo with 1000 sample and 3 sigma variation target"
			set_config_opt lvf_external_sampling 0
                        set_config_opt statistical_model_sigma_montecarlo 1
                        set_config_opt lvf_ml_mode 0
    			set_config_opt update_cache_last 1
    			set_config_opt min_disk_space 800
                        set_config_opt statistical_montecarlo_method percentile
                        set_config_opt statistical_montecarlo_percentile_sigma 3
                        set_config_opt statistical_montecarlo_sample_size 1000
                        set_config_opt -type {statistical_constraint statistical_mpw} statistical_montecarlo_sample_size 1000
    			set_config_opt cdpl_long_task_alert 36000
    			set_config_opt cdpl_task_max_lifespan 40000
    			set_config_opt cdpl_worker_timeout 900
    			set_config_opt char_engine_max_lifespan 40000
    			set_config_opt enable_netlist_pruning 1
    			set_config_opt statistical_avoid_screening_acquisition 1
			
			## Added to speedup the simulation)
			set_config_opt enable_parallel_sweeps 1
			set_config_opt enable_mc_sweeps 1
			set_config_opt netlist_max_sweeps 1
			set_config_opt statistical_reduction_factor 0.6
			set_config_opt statistical_two_sided_screening 0
			
			}
			
      {^\s*mc500\s*$} 	{
      			log_info "lvf_mode:: Detected lvf_mode=${lvfModeVar} : Montecarlo with 500 sample and 1 sigma variation target"
			
			set_config_opt lvf_ml_mode 0
    			set_config_opt update_cache_last 1
			##
			set_config_opt lvf_external_sampling 0
			set_config_opt statistical_model_sigma_montecarlo 1
			set_config_opt statistical_montecarlo_sample_size 500
			set_config_opt enable_mc_sweeps 1
			set_config_opt statistical_reduction_factor 0.5
			set_config_opt statistical_two_sided_screening 0
			##
			set_config_opt enable_parallel_sweeps 1
			set_config_opt netlist_max_sweeps 1

			}
			
      {^\s*mc250\s*$} 	{
      			log_info "lvf_mode:: Detected lvf_mode=${lvfModeVar} : Montecarlo with 250 sample and 1 sigma variation target"
			
			set_config_opt lvf_ml_mode 0
    			set_config_opt update_cache_last 1
			##
			set_config_opt lvf_external_sampling 0
			set_config_opt statistical_model_sigma_montecarlo 1
			set_config_opt statistical_montecarlo_sample_size 250
			set_config_opt enable_mc_sweeps 1
			set_config_opt statistical_reduction_factor 0.5
			set_config_opt statistical_two_sided_screening 0
			##
			set_config_opt enable_parallel_sweeps 1
			set_config_opt netlist_max_sweeps 1

			}
			
      {^\s*mcref\s*$} 	{
                        # Same as "mc"
      			log_info "lvf_mode:: Detected lvf_mode=${lvfModeVar}"
                        set_config_opt statistical_model_sigma_montecarlo 1
                        set_config_opt lvf_ml_mode 0
    			set_config_opt update_cache_last 1
    			set_config_opt min_disk_space 800
                        set_config_opt statistical_montecarlo_method percentile
                        set_config_opt statistical_montecarlo_percentile_sigma 3
                        set_config_opt statistical_montecarlo_sample_size 5000
			set_config_opt -type {statistical_constraint statistical_mpw} statistical_montecarlo_sample_size 1000
    			set_config_opt cdpl_long_task_alert 36000
    			set_config_opt cdpl_task_max_lifespan 40000
    			set_config_opt cdpl_worker_timeout 900
    			set_config_opt char_engine_max_lifespan 40000
    			set_config_opt enable_netlist_pruning 1
    			set_config_opt statistical_avoid_screening_acquisition 1
                        # Additional
    			set_config_opt statistical_reduction_factor 1
    			set_config_opt lvf_zero_sigma_value 0
    			set_config_opt lvf_zero_sigma_min 0
    			set_config_opt lvf_zero_sigma_auto_fix 0
                        }
      default   	{
			log_warning "lvf_mode:: Unknown Argument lvf_mode=[get_config_opt lvf_mode]"
			}
    }

    #--------------------------------------------------------------------------------------------------------#
    # macro_mode 
    switch -regexp -matchvar macroModeVar -- [get_config_opt macro_mode] {
      {^\s*[0]\s*$} 	{ }
      {^\s*[1]\s*$} 	{
      			log_info "macro_mode:: Detected macro_mode=${macroModeVar}"
    			set_config_opt simulator_options [subst {"common,finesim: finesim_mode=${finesim_mode} finesim_method=egear finesim_vpwltol=0 finesim_pwl_acc=1 finesim_exitwarn="DC not converged" finesim_exitwarn="FINESIM_TUNIT" finesim_no_swap=1 finesim_rcred=1 finesim_fast_corner_sweep=1 finesim_adaptive_mt=1"}]
			set_config_opt -type ccs_noise simulator_options {"common,finesim: finesim_mode=spicead finesim_method=egear finesim_vpwltol=0 finesim_pwl_acc=1 finesim_exitwarn="DC not converged" finesim_exitwarn="FINESIM_TUNIT" finesim_no_swap=1"}
			set_config_opt ccsn_advanced_flow 0
			set_config_opt ccb_partition_pin_based 1
			set_config_opt ccb_max_input_count 10
			set_config_opt ccb_max_mosfet_count 300
			set_config_opt ccb_single_fanout 1
    			set_config_opt cdpl_long_task_alert 3600
    			set_config_opt cdpl_task_max_lifespan 2880
    			set_config_opt cdpl_worker_timeout 900
    			set_config_opt char_engine_max_lifespan 2880
    			set_config_opt enable_dc_leakage 0
    			set_config_opt min_disk_space 500
    			set_config_opt initialization_cycles 1
    			set_config_opt -type input_capacitance initialization_cycles 0
    			set_config_opt scheduler_poll_time 120
    			set_config_opt separate_cell_initialization nodeset
    			#set_config_opt time_res_high 1e-12
			}
      {^\s*[2]\s*$} 	{
      			log_info "macro_mode:: Detected macro_mode=${macroModeVar}"
    			set_config_opt simulator_options [subst {"common,finesim: finesim_mode=${finesim_mode} finesim_method=egear finesim_vpwltol=0 finesim_pwl_acc=1 finesim_exitwarn="DC not converged" finesim_exitwarn="FINESIM_TUNIT" finesim_no_swap=1 finesim_rcred=2 finesim_fast_corner_sweep=1 finesim_adaptive_mt=1"}]
			set_config_opt -type ccs_noise simulator_options {"common,finesim: finesim_mode=spicead finesim_method=egear finesim_vpwltol=0 finesim_pwl_acc=1 finesim_exitwarn="DC not converged" finesim_exitwarn="FINESIM_TUNIT" finesim_no_swap=1"}
			set_config_opt ccsn_advanced_flow 0
			set_config_opt ccb_partition_pin_based 1
			set_config_opt ccb_max_input_count 10
			set_config_opt ccb_max_mosfet_count 300
			set_config_opt ccb_single_fanout 2
			set_config_opt ccb_single_fanout_bit 1
    			set_config_opt cdpl_long_task_alert 36000
    			set_config_opt cdpl_task_max_lifespan 14400
    			set_config_opt cdpl_worker_timeout 900
    			set_config_opt char_engine_max_lifespan 14400
    			set_config_opt configure_all_states_for_ccb 0
    			set_config_opt enable_dc_leakage 0
    			set_config_opt min_disk_space 500
    			set_config_opt initialization_cycles 1
    			set_config_opt -type input_capacitance initialization_cycles 0
    			set_config_opt scheduler_poll_time 120
    			set_config_opt separate_cell_initialization nodeset
    			set_config_opt time_res_high 2e-12
    			set_config_opt -type ccs_noise time_res_high 1e-12
			}
      {^\s*[3]\s*$} 	{
      			log_info "macro_mode:: Detected macro_mode=${macroModeVar}. Invoking FineSim Pro"
    			set_config_opt simulator_options {"common,finesim: finesim_mode=prohd finesim_method=egear finesim_vpwltol=0 finesim_pwl_acc=1 finesim_exitwarn="DC not converged" finesim_exitwarn="FINESIM_TUNIT" finesim_no_swap=1 finesim_fast_corner_sweep=1 finesim_adaptive_mt=1"}
			set_config_opt -type ccs_noise simulator_options {"common,finesim: finesim_mode=spicead finesim_method=egear finesim_vpwltol=0 finesim_pwl_acc=1 finesim_exitwarn="DC not converged" finesim_exitwarn="FINESIM_TUNIT" finesim_no_swap=1"}
			set_config_opt ccsn_advanced_flow 0
			set_config_opt ccb_partition_pin_based 1
			set_config_opt ccb_max_input_count 10
			set_config_opt ccb_max_mosfet_count 300
			set_config_opt ccb_single_fanout 2
			set_config_opt ccb_single_fanout_bit 1
    			set_config_opt cdpl_long_task_alert 36000
    			set_config_opt cdpl_task_max_lifespan 14400
    			set_config_opt cdpl_worker_timeout 900
    			set_config_opt char_engine_max_lifespan 14400
    			set_config_opt configure_all_states_for_ccb 0
    			set_config_opt enable_dc_leakage 0
    			set_config_opt min_disk_space 500
    			set_config_opt initialization_cycles 1
    			set_config_opt -type input_capacitance initialization_cycles 0
    			set_config_opt scheduler_poll_time 120
    			set_config_opt separate_cell_initialization nodeset
    			set_config_opt time_res_high 5e-12
    			set_config_opt -type ccs_noise time_res_high 1e-12
			}
      {^stdcell$} 	{
      			log_info "macro_mode:: Detected macro_mode=${macroModeVar}"
    			set_config_opt simulator_options {"common,finesim: finesim_mode=spicead finesim_method=egear finesim_vpwltol=0 finesim_pwl_acc=1 finesim_exitwarn="DC not converged" finesim_exitwarn="FINESIM_TUNIT" finesim_no_swap=1"}
			set_config_opt constraint_initial_delay_mode 1
			set_config_opt ccsn_advanced_flow 1
			set_config_opt ccsn_flatten_netlist 1
			set_config_opt ccsn_ccb_partition_mode 1
			set_config_opt ccsn_add_second_stage_ccb 1
			set_config_opt ccb_partition_pin_based 0
			set_config_opt ccb_single_fanout 0
    			set_config_opt cdpl_long_task_alert 3600
    			set_config_opt cdpl_task_max_lifespan 2880
    			set_config_opt cdpl_worker_timeout 600
    			set_config_opt char_engine_max_lifespan 2880
    			set_config_opt configure_all_states_for_ccb 0
    			set_config_opt enable_dc_leakage 1
    			set_config_opt min_disk_space 200
    			set_config_opt initialization_cycles 0
    			set_config_opt -type input_capacitance initialization_cycles 0
    			set_config_opt scheduler_poll_time 60
    			set_config_opt separate_cell_initialization ic
    			set_config_opt time_res_high 5e-12
    			set_config_opt -type ccs_noise time_res_high 1e-12
    			set_config_opt update_cache_last 1
			}
      default   	{
			log_warning "macro_mode:: Unknown Argument macro_mode=[get_config_opt macro_mode]"
			}
    }

    #--------------------------------------------------------------------------------------------------------#
    # use_simulator 
    switch -regexp -matchvar useSimulatorVar -- [get_config_opt use_simulator] {
      {0|embedded} 	 {
                         set_config_opt simulator finesim_embedded
                         set_config_opt simulator_cmd [subst {/p/hdk/cad/finesim/${mappedFinesimVersion}/bin/finesim -spice <input_deck> -o <listing_file>}]
                         }
      {finesim_embedded} {
                         set_config_opt simulator finesim_embedded
                         set_config_opt simulator_cmd [subst {/p/hdk/cad/finesim/${mappedFinesimVersion}/bin/finesim -spice <input_deck> -o <listing_file>}]
                         }
      {1|finesim}	 {
      			 log_info "use_simulator:: Detected use_simulator=${useSimulatorVar}. Using simulator=finesim (FineSim Standalone 1-CPU)"
      			 set_config_opt simulator finesim
			 set_config_opt enable_external_simulator_pruning 1
                         set_config_opt simulator_cmd [subst {/p/hdk/cad/finesim/${mappedFinesimVersion}/bin/finesim -spice <input_deck> -o <listing_file>}]
			 }
      {2|mt2}		 {
      			 log_info "use_simulator:: Detected use_simulator=${useSimulatorVar}. Using simulator=finesim (FineSim Standalone 2-CPU MT)"
      			 log_info "use_simulator:: Please ensure corresponding NetBatch MT Options in parameter='normal_queue'"
      			 set_config_opt simulator finesim
			 set_config_opt enable_external_simulator_pruning 1
                         set_config_opt simulator_cmd [subst {/p/hdk/cad/finesim/${mappedFinesimVersion}/bin/finesim -spice -no_netcheck -np 2 <input_deck> -o <listing_file>}]
      			 set_config_opt -type ccs_noise simulator finesim_embedded
			 }
      {4|mt4}		 {
      			 log_info "use_simulator:: Detected use_simulator=${useSimulatorVar}. Using simulator=finesim (FineSim Standalone 4-CPU MT)"
      			 log_info "use_simulator:: Please ensure corresponding NetBatch MT Options in parameter='normal_queue'"
      			 set_config_opt simulator finesim
			 set_config_opt enable_external_simulator_pruning 1
                         set_config_opt simulator_cmd [subst {/p/hdk/cad/finesim/${mappedFinesimVersion}/bin/finesim -spice -no_netcheck -np 4 <input_deck> -o <listing_file>}]
      			 set_config_opt -type ccs_noise simulator finesim_embedded
			 }
      {8|mt8}		 {
      			 log_info "use_simulator:: Detected use_simulator=${useSimulatorVar}. Using simulator=finesim (FineSim Standalone 8-CPU MT)"
      			 log_info "use_simulator:: Please ensure corresponding NetBatch MT Options in parameter='normal_queue'"
      			 set_config_opt simulator finesim
			 set_config_opt enable_external_simulator_pruning 1
                         set_config_opt simulator_cmd [subst {/p/hdk/cad/finesim/${mappedFinesimVersion}/bin/finesim -spice -no_netcheck -np 8 <input_deck> -o <listing_file>}]
      			 set_config_opt -type ccs_noise simulator finesim_embedded
			 }
      default		 {
                         set_config_opt simulator finesim_embedded
                         set_config_opt simulator_cmd [subst {/p/hdk/cad/finesim/${mappedFinesimVersion}/bin/finesim -spice <input_deck> -o <listing_file>}]
                         }
    }

    #--------------------------------------------------------------------------------------------------------#
    # ibis_mode 
        switch -regexp -matchvar ibisModeVar -- [get_config_opt ibis_mode] {
          {0} 	{ }
          {1} 	{
          			log_info "ibis_mode:: Detected ibis_mode=${ibisModeVar}"
          			set archive_condition_on_success yes
    				set scheduler_poll_time 120
    				set total_slew_multiplier 1
          			set separate_cell_initialization nodeset
          			set_config_opt -type ibis initialization_cycles 1
          			set_config_opt -type {ibis_c_comp ibis_c_comp_ac} initialization_cycles 0
    				set_config_opt -type initialization simulator_options [subst {"common,finesim: finesim_mode=spicexd finesim_method=egear finesim_vpwltol=0 finesim_pwl_acc=1 finesim_exitwarn="DC not converged" finesim_exitwarn="FINESIM_TUNIT" finesim_no_swap=1"}]
    				set_config_opt -type ibis_vt simulator_options [subst {"common,finesim: finesim_mode=spicehd finesim_method=egear finesim_vpwltol=0 finesim_pwl_acc=1 finesim_exitwarn="DC not converged" finesim_exitwarn="FINESIM_TUNIT" finesim_no_swap=1"}]
            	}
          {2} 	{
          			log_info "ibis_mode:: Detected ibis_mode=${ibisModeVar}"
          			set archive_condition_on_success yes
    				set scheduler_poll_time 120
    				set total_slew_multiplier 1
          			set separate_cell_initialization nodeset
          			set_config_opt -type ibis initialization_cycles 1
          			set_config_opt -type {ibis_c_comp ibis_c_comp_ac} initialization_cycles 0

    				set_config_opt simulator_options [subst {"common,finesim: finesim_mode=${finesim_mode} finesim_method=egear finesim_vpwltol=0 finesim_pwl_acc=1 finesim_exitwarn="DC not converged" finesim_exitwarn="FINESIM_TUNIT" finesim_no_swap=1 finesim_rcred=1 finesim_fast_corner_sweep=1 finesim_adaptive_mt=1"}]
    				set_config_opt -type initialization simulator_options [subst {"common,finesim: finesim_mode=spicexd finesim_method=egear finesim_vpwltol=0 finesim_pwl_acc=1 finesim_exitwarn="DC not converged" finesim_exitwarn="FINESIM_TUNIT" finesim_no_swap=1 finesim_rcred=1 finesim_fast_corner_sweep=1 finesim_adaptive_mt=1"}]
    				set_config_opt -type ibis_vt simulator_options [subst {"common,finesim: finesim_mode=spicehd finesim_method=egear finesim_vpwltol=0 finesim_pwl_acc=1 finesim_exitwarn="DC not converged" finesim_exitwarn="FINESIM_TUNIT" finesim_no_swap=1 finesim_rcred=1 finesim_fast_corner_sweep=1 finesim_adaptive_mt=1"}]
            	}
          default   {		
          			set archive_condition_on_success yes
    				set scheduler_poll_time 120
            	}
        }

    #--------------------------------------------------------------------------------------------------------#
    # node_cap_only
    if {[info exists ::env(node_cap_only)]} {
      set nodeCapOnlyVar $::env(node_cap_only)
      if {$nodeCapOnlyVar == "1"} {
        log_info "node_cap_only:: Detected node_cap_only=1"
        set configure_from_function 0
        set configure_enable_optimization 1
        set fast_cell_initialization 1
        set_config_opt -type input_capacitance state_partitions one
      } elseif {$nodeCapOnlyVar == "2"} {
        log_info "node_cap_only:: Detected node_cap_only=2"
        set configure_from_function 0
        set configure_enable_optimization 1
        set fast_cell_initialization 1
        set input_cap_char_method 1
        set_config_opt -type input_capacitance state_partitions one
      } else {
        log_warning "node_cap_only:: Unknown Argument node_cap_only=${nodeCapOnlyVar}"
      }
    }

}


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# 2: Liberty Cards
#    Parameters that appear in the Liberty 'as-is'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
define_parameters liberty_model {
    #--------------------------------------------------------------------------------------------------------#
    # Header
    set technology cmos					;# CMOS Technology: Mandatory Attribute
    set delay_model "table_lookup"			;# Lookup Table: Mandatory Attribute
    set date [date]					;# Date of Library Generation
    # Default Values
    set default_fanout_load 1.0
    set default_inout_pin_cap 0.0
    set default_input_pin_cap 0.0
    set default_output_pin_cap 0.0
    set default_cell_leakage_power 0.0
    set default_leakage_power_density 0.0
    set in_place_swap_mode match_footprint
    set pulling_resistance_unit "1kohm"
}


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# 3: Pintype Configuration
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

pintype default {
    if {[info exists ::env(INTEL_PDK) ]} {
#    #--------------------------------------------------------------------------------------------------------#
#    # Logic Thresholds
#    set prop_delay_current 0.1				;# 10% Three-State-Disable Current Threshold
#    set cin_high_threshold 0.955			;# 95.5% Gate-Capacitance Integration Upper Threshold
#    set cin_low_threshold 0.01				;# 01% Gate-Capacitance Integration Lower Threshold####
#
#    #--------------------------------------------------------------------------------------------------------#
    # NLDM Noise
    set autorange_height 1
    set numsteps_width 5
    set numsteps_height 8
    set numsteps_voltage 25
    set smallest_width 50e-12
    set largest_width 1e-09
    set limit_noise_pulse_range 1
    }
}

#--------------------------------------------------------------------------------------------------------#
# New Pintyes for BUSes
# pt_bus_2b  to pt_bus_300b
# pt_bus_2ba to pt_bus_300ba
foreach __pt_bit [__range 2 301 1] {
	pintype pt_bus_${__pt_bit}b->default {
	    set bus_width ${__pt_bit}
	    set bus_to  0
	    set bus_from    [expr (${__pt_bit}-1)]
	    set liberty_pin_groups [__range [expr (${__pt_bit}-1)] -1 -1]
	}
	pintype pt_bus_${__pt_bit}ba->default {
	    set bus_width ${__pt_bit}
	    set bus_to  1
	    set bus_from    ${__pt_bit}
	    set liberty_pin_groups [__range ${__pt_bit} 0 -1]
	}
}


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# 4: Validation
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# if {! [info exists ::env(LIBRARYCOMPILER_DIR)]} {
#        set LC_VER [exec getTv librarycompiler]
#        set ::env(LIBRARYCOMPILER_DIR) "$env(CAD_ROOT)/librarycompiler/${LC_VER}"
#    } 

define_parameters validation {
    #--------------------------------------------------------------------------------------------------------#
    # Comparison Tolerances
    set enable_total_power_comparison	1

    set absolute_tolerance		0.005
    set relative_tolerance		0.01
    set delay_absolute_tolerance	0.01
    set delay_relative_tolerance	0.05
    set slew_absolute_tolerance		0.01
    set slew_relative_tolerance		0.05
    set setup_absolute_tolerance	0.01
    set setup_relative_tolerance	0.05
    set hold_absolute_tolerance		0.01
    set hold_relative_tolerance		0.05
    set recovery_absolute_tolerance	0.01
    set recovery_relative_tolerance	0.05
    set removal_absolute_tolerance	0.01
    set removal_relative_tolerance	0.05
    set mpw_absolute_tolerance		0.01
    set mpw_relative_tolerance		0.05
    set energy_absolute_tolerance	5.00E-05
    set energy_relative_tolerance	0.1
    set leakage_absolute_tolerance	5.00E-05
    set leakage_relative_tolerance	0.1

    #--------------------------------------------------------------------------------------------------------#
    # Compare Library
    set compare_library_top_failures	100
    set compare_library_inout_load_mode	1

    #--------------------------------------------------------------------------------------------------------#
    # Qualify Library
    puts "Using pt_shell path for qualification: /p/hdk/cad/primetime/${mappedLcVersion}/bin/pt_shell"
    set qualification_pt_shell		[subst {/p/hdk/cad/primetime/${mappedLcVersion}/bin/pt_shell}]
    set qualification_lc_shell		[subst {${lc_shellPath}}]
    set qualification_finesim		[subst {/p/hdk/cad/finesim/${mappedFinesimVersion}/bin/finesim}]
    set qualification_cleanup		0
    set qualification_10nm_mode		1
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# 5: slew derate setting for tsmc (As per recomendation) 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

if {! [info exists ::env(INTEL_PDK) ]} {
    set_config_opt slew_derate_upper_threshold 0.9
    set_config_opt slew_derate_lower_threshold 0.1
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# 6: Suggested by synopsis
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
set_config_opt cache_include_files 0
set_config_opt report_worker_peak_memory 1
