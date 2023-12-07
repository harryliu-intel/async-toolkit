#--------------------------------------------------------------------------------------------------------#
# Environment|Flow|Tool Variables (starts with a 'set')
#--------------------------------------------------------------------------------------------------------#
#set env(corner) 	{0.750-0.950-tttt-70-max}

if {![info exists ::env(io_supplies)]} { set env(io_supplies) {vcc} }
if {![info exists ::env(io_grounds)]} { set env(io_grounds) {vssx} }

set env(core_supplies)	{}
set env(core_grounds)	{}

set required_views {-timing -ccs -lvf -ccs_noise -power}		;# Characterization Models
#set required_views {-timing}		;# Characterization Models

#--------------------------------------------------------------------------------------------------------#
# Customize-able Env Vars (Uncomment when needed)
#--------------------------------------------------------------------------------------------------------#

# set env(macro_mode) 0				;# Macro Mode [set 1 for AIP and 0 for Custom/ STD. cell] (default:0)
 
# set env(lvf_mode) sba				;# LVF Mode [sba|sb_opt|ml|ml_opt|ml_hsphy|mc500|mc1000|mc2500] (default:sba)

# set env(finesim_mode) spiecad			;# FineSim Mode [spicead:p|spicead|spicead:s|spicehd] (default:spicead)

set env(use_simulator) finesim 			;# Simulator Mode [finesim_embedded|finesim|mt2|mt4] (default:finesim_embedded)

# set env(node_cap_only) 1			;# node_cap_only [0|1|2] (default:0)
 
#--------------------------------------------------------------------------------------------------------#
# Create and Set WorkArea
#--------------------------------------------------------------------------------------------------------#
set_replay_file -record_configure -record_inst replay.tcl
set env(CDPL_SUBMIT_FAILURE_THRESHOLD) 10
set env(CDPL_GRID_CHECK_NOREPLY_OKAY) 1
set env(FINESIM_LICENSE_MODE) 1
create [pwd]
set_location [pwd]
__set_log_file siliconsmart.log

#----------------------------#
# Job Setup
#----------------------------#
set job_scheduler standalone		;# [nb|standalone] nb=NetBatch
set run_list_maxsize 16				;# Max. parallel jobs
if {[info exists ::env(job_scheduler)]} { set job_scheduler $env(job_scheduler) }
if {[info exists ::env(run_list_maxsize)]} { set run_list_maxsize $env(run_list_maxsize) }
set_config_opt job_scheduler $job_scheduler
set_config_opt run_list_maxsize $run_list_maxsize
set_config_opt -type {lvf} netlist_max_sweeps 128
 
#----------------------------#
# Import
#----------------------------#
if {![info exists ::env(skip_import)]} {
    set_config_opt model_bundle_bit_level 1
#    set lib lib764_g1i_210h_50pp

    if {![info exists ::env(fulllib)]} {
        error "fulllib not defined"
    }
    set lib $::env(fulllib)

    if {![info exists ::env(extract_pvt)]} {
        error "extract_pvt not defined"
    }
    set extract_pvt $::env(extract_pvt)
    set lib_pvt tttt_0p550v_100c_tttt_cmax_ccslnt
    if {[info exists ::env(lib_pvt)]} {
        set lib_pvt $::env(lib_pvt)
	# is this right??
        error "lib_pvt not defined"
    }
    set stdcells $::env(stdcell_dir)
    if {![info exists ::env(bundle)]} {
        error "bundle not defined"
    }
    set bundle $::env(bundle)
    set spfdir $stdcells/$bundle/spf/${lib}_${bundle}_${extract_pvt}
    if {[info exists ::env(spfdir)]} {
        set spfdir $::env(spfdir)
    }
    set liberty $stdcells/$bundle/lib/${lib}_${bundle}_${lib_pvt}.lib.gz
    if {[info exists ::env(lib_file)]} {
        set liberty $::env(lib_file)
    }
    if {[info exists ::env(instance_dir)]} {
        set_config_opt master_instance_script "master_instance.tcl"
    }
    set args [list -liberty $liberty \
                   -extension .spf \
                   -netlist_dir $spfdir \
                   -rechar -fast]
    if {[info exists ::env(skeleton)]} {
        lappend args -skeleton
    }
    import {*}$args

    if {[info exists ::env(import_only)]} {
        puts "Exiting after import"
        exit
    }
}

set cells [get_cells]

#__set_debug_mode

#--------------------------------------------------------------------------------------------------------#
# Useful parameter overrides (Uncomment when needed)
#--------------------------------------------------------------------------------------------------------#
# Mandatory field for Macro blocks (Ignore if the block is custom standard cell)
# set_liberty_attribute -cell $cells is_macro_cell true

# Use a reduced-RC netlist for CCSN
# set_config_opt -cell $cells -type ccs_noise cut_netlist "[get_location]/netlists/${cells}_ccsn.spf"

# Use a reduced-RC netlist for LVF
# set_config_opt -cell $cells -type lvf cut_netlist "[get_location]/netlists/${cells}_lvf.spf"

# Intel-specific maxcap derivation. Default [1 for Intel | 0 for Tsmc]
# set_config_opt maxcap_from_autorange 0

#---------------------------------#
# User Specified Overrides (if any)
#(starts with a 'set_config_opt')
#---------------------------------#
set_config_opt update_cache_last 0
set_config_opt model_failed_cells_in_lib 1

# the following suggested by Synopsys support re: finesim crashes
set_config_opt enable_netlist_pruning 0


#--------------------------------------------------------------------------------------------------------#
# Flow Preset| Over-rides (Do Not Touch)
#--------------------------------------------------------------------------------------------------------#
## To include -ccs_noise when noise=yes in the corners.xml (Also for intel it excludes -lvf when noise; Plz customize if req.)
if {[regexp {noise} $corner_type]} {
    set required_views [join "$required_views -ccs_noise"]
    if {[info exists ::env(INTEL_PDK)]} { set required_views [regsub -all "\\-lvf" $required_views ""] }
}

if {![regexp {lvf} $required_views]} { set_config_opt lvf_external_sampling 0}
#--------------------------------------------------------------------------------------------------------#
# BEGIN: Trap: Do Not Touch
set catch_trap [catch {
##---------------------------------------##
if {[get_parameter -quiet -default v1 liberty_ccsn_format] != "v1"} {
# liberty_ccsn_format=v2|both requires 'generate_auto_index' 
  __generate_auto_index $cells
}
##---------------------------------------##
## Characterization
##---------------------------------------##
configure {*}$required_views $cells
characterize $cells

set model_args $required_views
if {[info exists ::env(skeleton)]} {
    lappend model_args -skeleton
}
model {*}$model_args $cells

##---------------------------------------##
# Lib Qualification
##---------------------------------------##
__report_sim_stats
#__qualify_library  -check {sensitivity index_spacing load_index lvf upf hazard pg_pin max_tran} -cells $cells [get_location]/models/liberty/liberty_${corner}.lib

##---------------------------------------##
# END : Trap: Do Not Touch
} catch_err]
if {($catch_err != 0) && ($catch_trap != 0)} {
  log_error "$catch_err"
  log_error "$errorInfo"
  log_error "Caught an Error during the characterization phase. Exiting."
  exit
}
#--------------------------------------------------------------------------------------------------------#
