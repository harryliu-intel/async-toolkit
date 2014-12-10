#!/usr/intel/bin/perl -lw
# AAG
# $Id$
# $DateTime$

# return status is 0 for available license
# return status is >0 for no available licenses

use strict;
use Getopt::Long;

# these aliases correspond, in some cases, with
# the grid ticket names

my $cnt=1;
my @licensefiles=();
my %licensefiles=();


my $lmstat = "/p/rrc/tools/bin/lmstat";
my $licsource = "/p/rrc/tools/bin/license";

my %serverlist = (
   "canyon_se" => "APACHEDA_LICENSE_FILE",
   "nspice_apl" => "APACHEDA_LICENSE_FILE",
   "nspice_basic" => "APACHEDA_LICENSE_FILE",
   "nspice_blsn" => "APACHEDA_LICENSE_FILE",
   "nspice_dsm" => "APACHEDA_LICENSE_FILE",
   "nspice_eyed" => "APACHEDA_LICENSE_FILE",
   "nspice_hicap" => "APACHEDA_LICENSE_FILE",
   "nspice_nport" => "APACHEDA_LICENSE_FILE",
   "nspice_passchk" => "APACHEDA_LICENSE_FILE",
   "nspice_prbs" => "APACHEDA_LICENSE_FILE",
   "nspice_psi" => "APACHEDA_LICENSE_FILE",
   "nspice_sv" => "APACHEDA_LICENSE_FILE",
   "nspice_ubs" => "APACHEDA_LICENSE_FILE",
   "oview" => "APACHEDA_LICENSE_FILE",
   "oviewfull" => "APACHEDA_LICENSE_FILE",
   "pathfinder_soc" => "APACHEDA_LICENSE_FILE",
   "redhawk_alp" => "APACHEDA_LICENSE_FILE",
   "redhawk_apl" => "APACHEDA_LICENSE_FILE",
   "redhawk_asv2" => "APACHEDA_LICENSE_FILE",
   "redhawk_caching" => "APACHEDA_LICENSE_FILE",
   "redhawk_chip_power_model" => "APACHEDA_LICENSE_FILE",
   "redhawk_fao" => "APACHEDA_LICENSE_FILE",
   "redhawk_gds2def" => "APACHEDA_LICENSE_FILE",
   "redhawk_inductance" => "APACHEDA_LICENSE_FILE",
   "redhawk_key" => "APACHEDA_LICENSE_FILE",
   "redhawk_lef2def" => "APACHEDA_LICENSE_FILE",
   "redhawk_lowpower" => "APACHEDA_LICENSE_FILE",
   "redhawk_mpr" => "APACHEDA_LICENSE_FILE",
   "redhawk_netlisting" => "APACHEDA_LICENSE_FILE",
   "redhawk_passchk" => "APACHEDA_LICENSE_FILE",
   "redhawk_psi" => "APACHEDA_LICENSE_FILE",
   "redhawk_pwrcalc" => "APACHEDA_LICENSE_FILE",
   "redhawk_pwrplan" => "APACHEDA_LICENSE_FILE",
   "redhawk_sig_em" => "APACHEDA_LICENSE_FILE",
   "redhawk_sim0" => "APACHEDA_LICENSE_FILE",
   "redhawk_sim1" => "APACHEDA_LICENSE_FILE",
   "redhawk_sim2" => "APACHEDA_LICENSE_FILE",
   "redhawk_sim3" => "APACHEDA_LICENSE_FILE",
   "redhawk_sim_cpm" => "APACHEDA_LICENSE_FILE",
   "redhawk_spice_pkg" => "APACHEDA_LICENSE_FILE",
   "redhawk_token" => "APACHEDA_LICENSE_FILE",
   "redhawk_tpm" => "APACHEDA_LICENSE_FILE",
   "redhawk_vcd" => "APACHEDA_LICENSE_FILE",
   "resistance_calculation" => "APACHEDA_LICENSE_FILE",
   "sentinel_psi" => "APACHEDA_LICENSE_FILE",
   "sentinel_psi_ac" => "APACHEDA_LICENSE_FILE",
   "sentinel_psi_dc" => "APACHEDA_LICENSE_FILE",
   "sentinel_psi_emi" => "APACHEDA_LICENSE_FILE",
   "sentinel_psi_sconvert" => "APACHEDA_LICENSE_FILE",
   "sentinel_psi_trans" => "APACHEDA_LICENSE_FILE",
   "sptartist" => "APACHEDA_LICENSE_FILE",
   "sptdirectdotlib" => "APACHEDA_LICENSE_FILE",
   "sptgafsplit" => "APACHEDA_LICENSE_FILE",
   "spthdl" => "APACHEDA_LICENSE_FILE",
   "sptipwizard" => "APACHEDA_LICENSE_FILE",
   "sptmixedlanguage" => "APACHEDA_LICENSE_FILE",
   "sptmixedvt" => "APACHEDA_LICENSE_FILE",
   "sptmultipletestbenches" => "APACHEDA_LICENSE_FILE",
   "sptmultiplevoltages" => "APACHEDA_LICENSE_FILE",
   "sptpowergating" => "APACHEDA_LICENSE_FILE",
   "sptrtlrewrite" => "APACHEDA_LICENSE_FILE",
   "sptsoc" => "APACHEDA_LICENSE_FILE",
   "sptspefcapacitance" => "APACHEDA_LICENSE_FILE",
   "sptsynlib2alf" => "APACHEDA_LICENSE_FILE",
   "sptsystemverilog" => "APACHEDA_LICENSE_FILE",
   "spttbengine" => "APACHEDA_LICENSE_FILE",
   "sptvectorless" => "APACHEDA_LICENSE_FILE",
   "sptvectorselector" => "APACHEDA_LICENSE_FILE",
   "sptvwengine" => "APACHEDA_LICENSE_FILE",
   "sptwsengine" => "APACHEDA_LICENSE_FILE",
   "sptwwalflint" => "APACHEDA_LICENSE_FILE",
   "sptwwengine" => "APACHEDA_LICENSE_FILE",
   "sptwwgaf" => "APACHEDA_LICENSE_FILE",
   "sptwwsim" => "APACHEDA_LICENSE_FILE",
   "sptwwui" => "APACHEDA_LICENSE_FILE",
   "sptwwvcdplus" => "APACHEDA_LICENSE_FILE",
   "sutil" => "APACHEDA_LICENSE_FILE",
   "totem" => "APACHEDA_LICENSE_FILE",
   "totem_apl" => "APACHEDA_LICENSE_FILE",
   "totem_caching" => "APACHEDA_LICENSE_FILE",
   "totem_digital" => "APACHEDA_LICENSE_FILE",
   "totem_gds" => "APACHEDA_LICENSE_FILE",
   "totem_hc" => "APACHEDA_LICENSE_FILE",
   "totem_inductance" => "APACHEDA_LICENSE_FILE",
   "totem_ipg" => "APACHEDA_LICENSE_FILE",
   "totem_mpr" => "APACHEDA_LICENSE_FILE",
   "totem_nx" => "APACHEDA_LICENSE_FILE",
   "totem_pwrcalc" => "APACHEDA_LICENSE_FILE",
   "totem_sim" => "APACHEDA_LICENSE_FILE",
   "totem_vcd" => "APACHEDA_LICENSE_FILE",
   "111" => "CDSLMD_LICENSE_FILE",
   "21400" => "CDSLMD_LICENSE_FILE",
   "276" => "CDSLMD_LICENSE_FILE",
   "300" => "CDSLMD_LICENSE_FILE",
   "32140" => "CDSLMD_LICENSE_FILE",
   "32150" => "CDSLMD_LICENSE_FILE",
   "32760" => "CDSLMD_LICENSE_FILE",
   "33301" => "CDSLMD_LICENSE_FILE",
   "34500" => "CDSLMD_LICENSE_FILE",
   "34510" => "CDSLMD_LICENSE_FILE",
   "570" => "CDSLMD_LICENSE_FILE",
   "940" => "CDSLMD_LICENSE_FILE",
   "945" => "CDSLMD_LICENSE_FILE",
   "960" => "CDSLMD_LICENSE_FILE",
   "994" => "CDSLMD_LICENSE_FILE",
   "adv_encrypt_std_64bit" => "CDSLMD_LICENSE_FILE",
   "adv_package_designer_expert" => "CDSLMD_LICENSE_FILE",
   "advanced_package_designer" => "CDSLMD_LICENSE_FILE",
   "affirma_ams_distrib_processing" => "CDSLMD_LICENSE_FILE",
   "affirma_ams_simulator" => "CDSLMD_LICENSE_FILE",
   "affirma_nc_simulator" => "CDSLMD_LICENSE_FILE",
   "affirma_sim_analysis_env" => "CDSLMD_LICENSE_FILE",
   "allegro_design_editor_620" => "CDSLMD_LICENSE_FILE",
   "allegro_design_expert" => "CDSLMD_LICENSE_FILE",
   "allegro_design_publisher" => "CDSLMD_LICENSE_FILE",
   "allegro_pcb_design_gxl" => "CDSLMD_LICENSE_FILE",
   "allegro_pcb_editor_gxl" => "CDSLMD_LICENSE_FILE",
   "allegro_pcb_global_route_env" => "CDSLMD_LICENSE_FILE",
   "allegro_pcb_intercon_feas" => "CDSLMD_LICENSE_FILE",
   "allegro_pcb_partitioning" => "CDSLMD_LICENSE_FILE",
   "allegro_pcb_router_610" => "CDSLMD_LICENSE_FILE",
   "allegro_studio" => "CDSLMD_LICENSE_FILE",
   "allegro_teamdesign_auth_option" => "CDSLMD_LICENSE_FILE",
   "allegro_viewer_plus" => "CDSLMD_LICENSE_FILE",
   "allegroslps" => "CDSLMD_LICENSE_FILE",
   "ams_environment" => "CDSLMD_LICENSE_FILE",
   "analog_design_environment_gxl" => "CDSLMD_LICENSE_FILE",
   "analog_design_environment_l" => "CDSLMD_LICENSE_FILE",
   "analog_design_environment_xl" => "CDSLMD_LICENSE_FILE",
   "artist_optimizer" => "CDSLMD_LICENSE_FILE",
   "artist_statistics" => "CDSLMD_LICENSE_FILE",
   "assura_drc" => "CDSLMD_LICENSE_FILE",
   "assura_lvs" => "CDSLMD_LICENSE_FILE",
   "assura_ui" => "CDSLMD_LICENSE_FILE",
   "base_verilog_lib" => "CDSLMD_LICENSE_FILE",
   "c_to_silicon_compiler_l" => "CDSLMD_LICENSE_FILE",
   "cadence_chip_assembly_router" => "CDSLMD_LICENSE_FILE",
   "capture_cis_studio" => "CDSLMD_LICENSE_FILE",
   "concept_hdl_expert" => "CDSLMD_LICENSE_FILE",
   "conformal_asic" => "CDSLMD_LICENSE_FILE",
   "conformal_constraint_dsgnr_xl" => "CDSLMD_LICENSE_FILE",
   "conformal_custom" => "CDSLMD_LICENSE_FILE",
   "conformal_eco" => "CDSLMD_LICENSE_FILE",
   "conformal_low_power" => "CDSLMD_LICENSE_FILE",
   "conformal_low_power_gxl" => "CDSLMD_LICENSE_FILE",
   "conformal_ultra" => "CDSLMD_LICENSE_FILE",
   "corners_analysis" => "CDSLMD_LICENSE_FILE",
   "dfm_core_technology" => "CDSLMD_LICENSE_FILE",
   "digital_mixed_signal_option" => "CDSLMD_LICENSE_FILE",
   "encounter_adv_node_gxl" => "CDSLMD_LICENSE_FILE",
   "encounter_c" => "CDSLMD_LICENSE_FILE",
   "encounter_digital_impl_sys_xl" => "CDSLMD_LICENSE_FILE",
   "encounter_low_power_gxl" => "CDSLMD_LICENSE_FILE",
   "encounter_power_system_xl" => "CDSLMD_LICENSE_FILE",
   "encounter_qrc_extraction_gxl" => "CDSLMD_LICENSE_FILE",
   "encounter_qrc_extraction_xl" => "CDSLMD_LICENSE_FILE",
   "encounter_test_architect" => "CDSLMD_LICENSE_FILE",
   "encounter_timing_system_gxl" => "CDSLMD_LICENSE_FILE",
   "encounter_timing_system_xl" => "CDSLMD_LICENSE_FILE",
   "encounter_true_time" => "CDSLMD_LICENSE_FILE",
   "expgen" => "CDSLMD_LICENSE_FILE",
   "extended_verilog_lib" => "CDSLMD_LICENSE_FILE",
   "first_encounter_vip" => "CDSLMD_LICENSE_FILE",
   "incisive_desktop_manager" => "CDSLMD_LICENSE_FILE",
   "incisive_enterprise_simulator" => "CDSLMD_LICENSE_FILE",
   "incisive_hdl_simulator" => "CDSLMD_LICENSE_FILE",
   "incisive_p2c_methodology" => "CDSLMD_LICENSE_FILE",
   "incisive_specman_interactive" => "CDSLMD_LICENSE_FILE",
   "incisive_verif_engine" => "CDSLMD_LICENSE_FILE",
   "incisive_verif_environ" => "CDSLMD_LICENSE_FILE",
   "leapfrog-cv" => "CDSLMD_LICENSE_FILE",
   "oasis_rfde" => "CDSLMD_LICENSE_FILE",
   "oasis_simulation_interface" => "CDSLMD_LICENSE_FILE",
   "pas_pcell_generator" => "CDSLMD_LICENSE_FILE",
   "pcb_design_expert" => "CDSLMD_LICENSE_FILE",
   "pcb_design_studio" => "CDSLMD_LICENSE_FILE",
   "pcb_librarian_expert" => "CDSLMD_LICENSE_FILE",
   "pcomp" => "CDSLMD_LICENSE_FILE",
   "pdk_test_fe" => "CDSLMD_LICENSE_FILE",
   "phys_ver_sys_const_validator" => "CDSLMD_LICENSE_FILE",
   "phys_ver_sys_drc_xl" => "CDSLMD_LICENSE_FILE",
   "phys_ver_sys_lvs_xl" => "CDSLMD_LICENSE_FILE",
   "phys_ver_sys_results_mgr" => "CDSLMD_LICENSE_FILE",
   "physical_verification_sys" => "CDSLMD_LICENSE_FILE",
   "physical_verification_sys_deb" => "CDSLMD_LICENSE_FILE",
   "placebase_all" => "CDSLMD_LICENSE_FILE",
   "plotversa" => "CDSLMD_LICENSE_FILE",
   "pspicestudio" => "CDSLMD_LICENSE_FILE",
   "qrc_advanced_analysis" => "CDSLMD_LICENSE_FILE",
   "qrc_advanced_modeling" => "CDSLMD_LICENSE_FILE",
   "relxpert" => "CDSLMD_LICENSE_FILE",
   "routemvia_all" => "CDSLMD_LICENSE_FILE",
   "rtl_compiler_l" => "CDSLMD_LICENSE_FILE",
   "rtl_compiler_physical" => "CDSLMD_LICENSE_FILE",
   "rtl_compiler_ultra" => "CDSLMD_LICENSE_FILE",
   "rtl_compiler_ultra_ii_option" => "CDSLMD_LICENSE_FILE",
   "rtl_compiler_verification" => "CDSLMD_LICENSE_FILE",
   "skilldev" => "CDSLMD_LICENSE_FILE",
   "specctra_expert" => "CDSLMD_LICENSE_FILE",
   "specctra_expert_system" => "CDSLMD_LICENSE_FILE",
   "specctra_pcb" => "CDSLMD_LICENSE_FILE",
   "specctraquest" => "CDSLMD_LICENSE_FILE",
   "specctraquest_si_expert" => "CDSLMD_LICENSE_FILE",
   "specctraquest_signal_expert" => "CDSLMD_LICENSE_FILE",
   "spectre_burst_allegrosi" => "CDSLMD_LICENSE_FILE",
   "test_design_analysis" => "CDSLMD_LICENSE_FILE",
   "test_design_generation" => "CDSLMD_LICENSE_FILE",
   "test_design_verification" => "CDSLMD_LICENSE_FILE",
   "test_extension_language" => "CDSLMD_LICENSE_FILE",
   "uet" => "CDSLMD_LICENSE_FILE",
   "ultrasim" => "CDSLMD_LICENSE_FILE",
   "verilog-xl" => "CDSLMD_LICENSE_FILE",
   "virtuoso_aps_mmsim_lk" => "CDSLMD_LICENSE_FILE",
   "virtuoso_layout_suite_gxl" => "CDSLMD_LICENSE_FILE",
   "virtuoso_layout_suite_l" => "CDSLMD_LICENSE_FILE",
   "virtuoso_layout_suite_xl" => "CDSLMD_LICENSE_FILE",
   "virtuoso_multi_mode_simulation" => "CDSLMD_LICENSE_FILE",
   "virtuoso_power_system_l" => "CDSLMD_LICENSE_FILE",
   "virtuoso_power_system_xl" => "CDSLMD_LICENSE_FILE",
   "virtuoso_qrc_extraction_gxl" => "CDSLMD_LICENSE_FILE",
   "virtuoso_qrc_extraction_l" => "CDSLMD_LICENSE_FILE",
   "virtuoso_qrc_extraction_xl" => "CDSLMD_LICENSE_FILE",
   "virtuoso_schematic_editor_l" => "CDSLMD_LICENSE_FILE",
   "virtuoso_schematic_editor_xl" => "CDSLMD_LICENSE_FILE",
   "virtuoso_spectre" => "CDSLMD_LICENSE_FILE",
   "virtuoso_spectre_gxl_mmsim_lk" => "CDSLMD_LICENSE_FILE",
   "virtuoso_spectre_rf" => "CDSLMD_LICENSE_FILE",
   "virtuoso_visual_analysis_xl" => "CDSLMD_LICENSE_FILE",
   "virtuoso_xl" => "CDSLMD_LICENSE_FILE",
   "acs" => "SNPSLMD_LICENSE_FILE",
   "aiu_foundation" => "SNPSLMD_LICENSE_FILE",
   "apacm" => "SNPSLMD_LICENSE_FILE",
   "apatd" => "SNPSLMD_LICENSE_FILE",
   "apcs" => "SNPSLMD_LICENSE_FILE",
   "aphpo" => "SNPSLMD_LICENSE_FILE",
   "apollo" => "SNPSLMD_LICENSE_FILE",
   "apsolar" => "SNPSLMD_LICENSE_FILE",
   "apsolarii" => "SNPSLMD_LICENSE_FILE",
   "aptime" => "SNPSLMD_LICENSE_FILE",
   "apxtalk" => "SNPSLMD_LICENSE_FILE",
   "artistif" => "SNPSLMD_LICENSE_FILE",
   "astro" => "SNPSLMD_LICENSE_FILE",
   "astro_dfm" => "SNPSLMD_LICENSE_FILE",
   "astroexp" => "SNPSLMD_LICENSE_FILE",
   "astroxtalk" => "SNPSLMD_LICENSE_FILE",
   "basic_analyses" => "SNPSLMD_LICENSE_FILE",
   "batch_measure" => "SNPSLMD_LICENSE_FILE",
   "batt_tool" => "SNPSLMD_LICENSE_FILE",
   "boa-brt" => "SNPSLMD_LICENSE_FILE",
   "cadence_frameway" => "SNPSLMD_LICENSE_FILE",
   "cadence_netlister" => "SNPSLMD_LICENSE_FILE",
   "cdsaawaves" => "SNPSLMD_LICENSE_FILE",
   "cktcheck" => "SNPSLMD_LICENSE_FILE",
   "cocentric-sys-davis" => "SNPSLMD_LICENSE_FILE",
   "cocentric-sys-designcenter" => "SNPSLMD_LICENSE_FILE",
   "cocentric-sys-hwflow" => "SNPSLMD_LICENSE_FILE",
   "cocentric-sys-hwsimif" => "SNPSLMD_LICENSE_FILE",
   "cocentric-sys-rdk" => "SNPSLMD_LICENSE_FILE",
   "cocentric-sys-simif-matlab" => "SNPSLMD_LICENSE_FILE",
   "cocentric-sys-simulator" => "SNPSLMD_LICENSE_FILE",
   "cocentric-sys-virsim" => "SNPSLMD_LICENSE_FILE",
   "corenl_char" => "SNPSLMD_LICENSE_FILE",
   "cosmos_scope" => "SNPSLMD_LICENSE_FILE",
   "cosmos_vo" => "SNPSLMD_LICENSE_FILE",
   "cpi-100" => "SNPSLMD_LICENSE_FILE",
   "ctv-interface" => "SNPSLMD_LICENSE_FILE",
   "dc-expert" => "SNPSLMD_LICENSE_FILE",
   "dc-explorer-shell" => "SNPSLMD_LICENSE_FILE",
   "dc-extension" => "SNPSLMD_LICENSE_FILE",
   "dc-fpga-features" => "SNPSLMD_LICENSE_FILE",
   "dc-sdf-interface" => "SNPSLMD_LICENSE_FILE",
   "dc-ultra-features" => "SNPSLMD_LICENSE_FILE",
   "dc-ultra-opt" => "SNPSLMD_LICENSE_FILE",
   "design-analyzer" => "SNPSLMD_LICENSE_FILE",
   "design-budgeting" => "SNPSLMD_LICENSE_FILE",
   "design-compiler" => "SNPSLMD_LICENSE_FILE",
   "design-vision" => "SNPSLMD_LICENSE_FILE",
   "designware" => "SNPSLMD_LICENSE_FILE",
   "designware-amba-vip" => "SNPSLMD_LICENSE_FILE",
   "designware-basic" => "SNPSLMD_LICENSE_FILE",
   "designware-ethernet-vip" => "SNPSLMD_LICENSE_FILE",
   "designware-fpga-basic" => "SNPSLMD_LICENSE_FILE",
   "designware-pci-vip" => "SNPSLMD_LICENSE_FILE",
   "designware-pciexpress-vip" => "SNPSLMD_LICENSE_FILE",
   "designware-regression" => "SNPSLMD_LICENSE_FILE",
   "designware-usb-vip" => "SNPSLMD_LICENSE_FILE",
   "designware-vera" => "SNPSLMD_LICENSE_FILE",
   "diode_tool" => "SNPSLMD_LICENSE_FILE",
   "drcycle_tool" => "SNPSLMD_LICENSE_FILE",
   "dw-developer" => "SNPSLMD_LICENSE_FILE",
   "edif2e" => "SNPSLMD_LICENSE_FILE",
   "edif_netlister" => "SNPSLMD_LICENSE_FILE",
   "encrypt" => "SNPSLMD_LICENSE_FILE",
   "enterprise_vo" => "SNPSLMD_LICENSE_FILE",
   "espcv" => "SNPSLMD_LICENSE_FILE",
   "esps2v" => "SNPSLMD_LICENSE_FILE",
   "ev-access" => "SNPSLMD_LICENSE_FILE",
   "ev-access_util" => "SNPSLMD_LICENSE_FILE",
   "expt_editor" => "SNPSLMD_LICENSE_FILE",
   "expt_editor_plus" => "SNPSLMD_LICENSE_FILE",
   "fastspice_xa" => "SNPSLMD_LICENSE_FILE",
   "floorplan-management" => "SNPSLMD_LICENSE_FILE",
   "formality" => "SNPSLMD_LICENSE_FILE",
   "formality-e1" => "SNPSLMD_LICENSE_FILE",
   "formality-esp" => "SNPSLMD_LICENSE_FILE",
   "fpga-compiler" => "SNPSLMD_LICENSE_FILE",
   "fullhspice" => "SNPSLMD_LICENSE_FILE",
   "fuse_char" => "SNPSLMD_LICENSE_FILE",
   "fusionvantagelmcinterface" => "SNPSLMD_LICENSE_FILE",
   "galaxy-advcts" => "SNPSLMD_LICENSE_FILE",
   "galaxy-advtech" => "SNPSLMD_LICENSE_FILE",
   "galaxy-common" => "SNPSLMD_LICENSE_FILE",
   "galaxy-dft" => "SNPSLMD_LICENSE_FILE",
   "galaxy-dfy" => "SNPSLMD_LICENSE_FILE",
   "galaxy-fp" => "SNPSLMD_LICENSE_FILE",
   "galaxy-fp-advcts" => "SNPSLMD_LICENSE_FILE",
   "galaxy-fp-advtech" => "SNPSLMD_LICENSE_FILE",
   "galaxy-fp-mv" => "SNPSLMD_LICENSE_FILE",
   "galaxy-icc" => "SNPSLMD_LICENSE_FILE",
   "galaxy-iu" => "SNPSLMD_LICENSE_FILE",
   "galaxy-multiroute4" => "SNPSLMD_LICENSE_FILE",
   "galaxy-multiroute8" => "SNPSLMD_LICENSE_FILE",
   "galaxy-mv" => "SNPSLMD_LICENSE_FILE",
   "galaxy-pnr" => "SNPSLMD_LICENSE_FILE",
   "galaxy-power" => "SNPSLMD_LICENSE_FILE",
   "galaxy-prototype" => "SNPSLMD_LICENSE_FILE",
   "galaxy-psyn" => "SNPSLMD_LICENSE_FILE",
   "galaxyconstraint" => "SNPSLMD_LICENSE_FILE",
   "gentech" => "SNPSLMD_LICENSE_FILE",
   "hdl" => "SNPSLMD_LICENSE_FILE",
   "hdl-compiler" => "SNPSLMD_LICENSE_FILE",
   "hercules-crypt_xref_data" => "SNPSLMD_LICENSE_FILE",
   "hercules-dp_mt" => "SNPSLMD_LICENSE_FILE",
   "hercules-explorer_drc" => "SNPSLMD_LICENSE_FILE",
   "hercules-explorer_filters" => "SNPSLMD_LICENSE_FILE",
   "hercules-explorer_lvs" => "SNPSLMD_LICENSE_FILE",
   "hercules-netlist" => "SNPSLMD_LICENSE_FILE",
   "hercules-run_tran" => "SNPSLMD_LICENSE_FILE",
   "hercules-xref_data" => "SNPSLMD_LICENSE_FILE",
   "hercules_debugger" => "SNPSLMD_LICENSE_FILE",
   "hercules_device" => "SNPSLMD_LICENSE_FILE",
   "hercules_drc" => "SNPSLMD_LICENSE_FILE",
   "hercules_erc" => "SNPSLMD_LICENSE_FILE",
   "hercules_hlpe" => "SNPSLMD_LICENSE_FILE",
   "hercules_lvs" => "SNPSLMD_LICENSE_FILE",
   "hercules_manager" => "SNPSLMD_LICENSE_FILE",
   "hercules_mask" => "SNPSLMD_LICENSE_FILE",
   "hercules_rce" => "SNPSLMD_LICENSE_FILE",
   "hercules_vue" => "SNPSLMD_LICENSE_FILE",
   "hsim" => "SNPSLMD_LICENSE_FILE",
   "hsim-cosim" => "SNPSLMD_LICENSE_FILE",
   "hsim-mra" => "SNPSLMD_LICENSE_FILE",
   "hsim-ms" => "SNPSLMD_LICENSE_FILE",
   "hsim-plx" => "SNPSLMD_LICENSE_FILE",
   "hsim-pra" => "SNPSLMD_LICENSE_FILE",
   "hsim-pvm" => "SNPSLMD_LICENSE_FILE",
   "hsim-sc" => "SNPSLMD_LICENSE_FILE",
   "hsim-spr" => "SNPSLMD_LICENSE_FILE",
   "hsim-sra" => "SNPSLMD_LICENSE_FILE",
   "hsim-xl" => "SNPSLMD_LICENSE_FILE",
   "hspice" => "SNPSLMD_LICENSE_FILE",
   "hspice3des" => "SNPSLMD_LICENSE_FILE",
   "hspice_model_library" => "SNPSLMD_LICENSE_FILE",
   "hspicecmidev" => "SNPSLMD_LICENSE_FILE",
   "hspicecmirt" => "SNPSLMD_LICENSE_FILE",
   "hspicerf" => "SNPSLMD_LICENSE_FILE",
   "hspiceva" => "SNPSLMD_LICENSE_FILE",
   "hspicewin" => "SNPSLMD_LICENSE_FILE",
   "icvalidator2-compareengine" => "SNPSLMD_LICENSE_FILE",
   "icvalidator2-geometryengine" => "SNPSLMD_LICENSE_FILE",
   "icvalidator2-manager" => "SNPSLMD_LICENSE_FILE",
   "icwb_plus" => "SNPSLMD_LICENSE_FILE",
   "icwbev_plus" => "SNPSLMD_LICENSE_FILE",
   "identdebugger" => "SNPSLMD_LICENSE_FILE",
   "identdebugger_pr" => "SNPSLMD_LICENSE_FILE",
   "identifyinstrumentor_encrypt" => "SNPSLMD_LICENSE_FILE",
   "identinstrumentor" => "SNPSLMD_LICENSE_FILE",
   "identinstrumentor_pr" => "SNPSLMD_LICENSE_FILE",
   "iqbus_author" => "SNPSLMD_LICENSE_FILE",
   "iqbus_lib" => "SNPSLMD_LICENSE_FILE",
   "juaopas" => "SNPSLMD_LICENSE_FILE",
   "juaplan" => "SNPSLMD_LICENSE_FILE",
   "juatime" => "SNPSLMD_LICENSE_FILE",
   "juaud" => "SNPSLMD_LICENSE_FILE",
   "jupiter" => "SNPSLMD_LICENSE_FILE",
   "juplan" => "SNPSLMD_LICENSE_FILE",
   "jutime" => "SNPSLMD_LICENSE_FILE",
   "juvs" => "SNPSLMD_LICENSE_FILE",
   "leda_checker" => "SNPSLMD_LICENSE_FILE",
   "leda_specifier" => "SNPSLMD_LICENSE_FILE",
   "library-compiler" => "SNPSLMD_LICENSE_FILE",
   "link_matlab" => "SNPSLMD_LICENSE_FILE",
   "lmcswift_net" => "SNPSLMD_LICENSE_FILE",
   "load_tool" => "SNPSLMD_LICENSE_FILE",
   "lsa" => "SNPSLMD_LICENSE_FILE",
   "lsim_parser" => "SNPSLMD_LICENSE_FILE",
   "ltl-100_gds" => "SNPSLMD_LICENSE_FILE",
   "ltl-100_ndw" => "SNPSLMD_LICENSE_FILE",
   "magellan" => "SNPSLMD_LICENSE_FILE",
   "magellan-cdc" => "SNPSLMD_LICENSE_FILE",
   "magellan-gui" => "SNPSLMD_LICENSE_FILE",
   "magellan-shell" => "SNPSLMD_LICENSE_FILE",
   "magellan-sim" => "SNPSLMD_LICENSE_FILE",
   "magellan-tb" => "SNPSLMD_LICENSE_FILE",
   "mce-base" => "SNPSLMD_LICENSE_FILE",
   "mdataprep" => "SNPSLMD_LICENSE_FILE",
   "mdynlink" => "SNPSLMD_LICENSE_FILE",
   "measure" => "SNPSLMD_LICENSE_FILE",
   "mentor_frameway" => "SNPSLMD_LICENSE_FILE",
   "mentor_netlister" => "SNPSLMD_LICENSE_FILE",
   "metaencrypt3des" => "SNPSLMD_LICENSE_FILE",
   "metawaves" => "SNPSLMD_LICENSE_FILE",
   "metawaveswin" => "SNPSLMD_LICENSE_FILE",
   "milkyway" => "SNPSLMD_LICENSE_FILE",
   "milkyway-interface" => "SNPSLMD_LICENSE_FILE",
   "model_encryption" => "SNPSLMD_LICENSE_FILE",
   "model_synthesis" => "SNPSLMD_LICENSE_FILE",
   "mos_tool" => "SNPSLMD_LICENSE_FILE",
   "motor_dcpm_tool" => "SNPSLMD_LICENSE_FILE",
   "mvcmp" => "SNPSLMD_LICENSE_FILE",
   "mvdbgen" => "SNPSLMD_LICENSE_FILE",
   "mverilog" => "SNPSLMD_LICENSE_FILE",
   "mvsim" => "SNPSLMD_LICENSE_FILE",
   "mwapiaccess" => "SNPSLMD_LICENSE_FILE",
   "mwapidev" => "SNPSLMD_LICENSE_FILE",
   "nanosim" => "SNPSLMD_LICENSE_FILE",
   "nanosim/ace_cosim" => "SNPSLMD_LICENSE_FILE",
   "nanosim/gui" => "SNPSLMD_LICENSE_FILE",
   "nanosim/power" => "SNPSLMD_LICENSE_FILE",
   "nanosim/vlog_a" => "SNPSLMD_LICENSE_FILE",
   "nanosim_starsimxt" => "SNPSLMD_LICENSE_FILE",
   "nanotime" => "SNPSLMD_LICENSE_FILE",
   "nanotime-ultra" => "SNPSLMD_LICENSE_FILE",
   "ncx" => "SNPSLMD_LICENSE_FILE",
   "net-tran" => "SNPSLMD_LICENSE_FILE",
   "opt_template_lib" => "SNPSLMD_LICENSE_FILE",
   "pathmill" => "SNPSLMD_LICENSE_FILE",
   "pathmill/dsx" => "SNPSLMD_LICENSE_FILE",
   "pathmill/pfx" => "SNPSLMD_LICENSE_FILE",
   "pathmill/sfx" => "SNPSLMD_LICENSE_FILE",
   "power-optimization" => "SNPSLMD_LICENSE_FILE",
   "primerail" => "SNPSLMD_LICENSE_FILE",
   "primerail-adv" => "SNPSLMD_LICENSE_FILE",
   "primerail-static" => "SNPSLMD_LICENSE_FILE",
   "primerail_hsim" => "SNPSLMD_LICENSE_FILE",
   "primetime" => "SNPSLMD_LICENSE_FILE",
   "primetime-px" => "SNPSLMD_LICENSE_FILE",
   "primetime-si" => "SNPSLMD_LICENSE_FILE",
   "primetime-vx" => "SNPSLMD_LICENSE_FILE",
   "primeyield_device" => "SNPSLMD_LICENSE_FILE",
   "primeyield_drc" => "SNPSLMD_LICENSE_FILE",
   "primeyield_erc" => "SNPSLMD_LICENSE_FILE",
   "primeyield_manager" => "SNPSLMD_LICENSE_FILE",
   "primeyield_mask" => "SNPSLMD_LICENSE_FILE",
   "primeyield_vue" => "SNPSLMD_LICENSE_FILE",
   "rtl-exploration" => "SNPSLMD_LICENSE_FILE",
   "rtl-power-analysis" => "SNPSLMD_LICENSE_FILE",
   "saber_frameway" => "SNPSLMD_LICENSE_FILE",
   "saber_guide" => "SNPSLMD_LICENSE_FILE",
   "saber_modelsim_mm" => "SNPSLMD_LICENSE_FILE",
   "saber_modelsim_plus_mm" => "SNPSLMD_LICENSE_FILE",
   "saber_modelsim_vhdl_mm" => "SNPSLMD_LICENSE_FILE",
   "saber_netlister" => "SNPSLMD_LICENSE_FILE",
   "saber_rt_mm" => "SNPSLMD_LICENSE_FILE",
   "saber_scope" => "SNPSLMD_LICENSE_FILE",
   "saber_simulator" => "SNPSLMD_LICENSE_FILE",
   "saber_sketch" => "SNPSLMD_LICENSE_FILE",
   "saber_sketch_netlister" => "SNPSLMD_LICENSE_FILE",
   "saber_verilog_mm" => "SNPSLMD_LICENSE_FILE",
   "saber_viewsim_mm" => "SNPSLMD_LICENSE_FILE",
   "saberrt_adi" => "SNPSLMD_LICENSE_FILE",
   "saberrt_fe" => "SNPSLMD_LICENSE_FILE",
   "saberrt_hil" => "SNPSLMD_LICENSE_FILE",
   "saberrt_nc" => "SNPSLMD_LICENSE_FILE",
   "scan_tool" => "SNPSLMD_LICENSE_FILE",
   "simif" => "SNPSLMD_LICENSE_FILE",
   "snps-assertions" => "SNPSLMD_LICENSE_FILE",
   "solarii" => "SNPSLMD_LICENSE_FILE",
   "sp2dspf" => "SNPSLMD_LICENSE_FILE",
   "spectral" => "SNPSLMD_LICENSE_FILE",
   "spice_parser" => "SNPSLMD_LICENSE_FILE",
   "sss" => "SNPSLMD_LICENSE_FILE",
   "ssst" => "SNPSLMD_LICENSE_FILE",
   "star-ascii" => "SNPSLMD_LICENSE_FILE",
   "star-cr" => "SNPSLMD_LICENSE_FILE",
   "star-ex" => "SNPSLMD_LICENSE_FILE",
   "star-ex_erc" => "SNPSLMD_LICENSE_FILE",
   "star-filter" => "SNPSLMD_LICENSE_FILE",
   "star-gazer" => "SNPSLMD_LICENSE_FILE",
   "star-ld2ltl" => "SNPSLMD_LICENSE_FILE",
   "star-r" => "SNPSLMD_LICENSE_FILE",
   "star-rc2" => "SNPSLMD_LICENSE_FILE",
   "star-rc2-aeo" => "SNPSLMD_LICENSE_FILE",
   "star-rc2-db" => "SNPSLMD_LICENSE_FILE",
   "star-rc2-distrib10" => "SNPSLMD_LICENSE_FILE",
   "star-rc2-inductance" => "SNPSLMD_LICENSE_FILE",
   "star-rc2-netlist" => "SNPSLMD_LICENSE_FILE",
   "star-rc2-prober" => "SNPSLMD_LICENSE_FILE",
   "star-rc2-tcad" => "SNPSLMD_LICENSE_FILE",
   "star-rc2-viewer" => "SNPSLMD_LICENSE_FILE",
   "star-rc2-xstor1" => "SNPSLMD_LICENSE_FILE",
   "star-rc2-xstor2" => "SNPSLMD_LICENSE_FILE",
   "star-rc2-xtr-aeo" => "SNPSLMD_LICENSE_FILE",
   "star-rc2-xtract" => "SNPSLMD_LICENSE_FILE",
   "star-rc2_manager" => "SNPSLMD_LICENSE_FILE",
   "star-rc2_ultra_manager" => "SNPSLMD_LICENSE_FILE",
   "star-rcxt-vx" => "SNPSLMD_LICENSE_FILE",
   "star-syn2star" => "SNPSLMD_LICENSE_FILE",
   "star-tcad" => "SNPSLMD_LICENSE_FILE",
   "star-xref" => "SNPSLMD_LICENSE_FILE",
   "synlib-eval" => "SNPSLMD_LICENSE_FILE",
   "synopsyscustomviewer" => "SNPSLMD_LICENSE_FILE",
   "synplifypremier" => "SNPSLMD_LICENSE_FILE",
   "synplifypro" => "SNPSLMD_LICENSE_FILE",
   "synplifypro_xilinx" => "SNPSLMD_LICENSE_FILE",
   "techviewer" => "SNPSLMD_LICENSE_FILE",
   "test-accelerate-max" => "SNPSLMD_LICENSE_FILE",
   "test-analysis" => "SNPSLMD_LICENSE_FILE",
   "test-atpg-max" => "SNPSLMD_LICENSE_FILE",
   "test-compile" => "SNPSLMD_LICENSE_FILE",
   "test-compile-share" => "SNPSLMD_LICENSE_FILE",
   "test-compiler" => "SNPSLMD_LICENSE_FILE",
   "test-compression-atpg" => "SNPSLMD_LICENSE_FILE",
   "test-compression-synthesis" => "SNPSLMD_LICENSE_FILE",
   "test-core-integration" => "SNPSLMD_LICENSE_FILE",
   "test-core-wrapper" => "SNPSLMD_LICENSE_FILE",
   "test-ctl-model" => "SNPSLMD_LICENSE_FILE",
   "test-delay" => "SNPSLMD_LICENSE_FILE",
   "test-dftc-tmax" => "SNPSLMD_LICENSE_FILE",
   "test-diagnosis" => "SNPSLMD_LICENSE_FILE",
   "test-fault-max" => "SNPSLMD_LICENSE_FILE",
   "test-faultsim" => "SNPSLMD_LICENSE_FILE",
   "test-iddq" => "SNPSLMD_LICENSE_FILE",
   "test-ieee-std-1149-1" => "SNPSLMD_LICENSE_FILE",
   "test-lbist-atpg" => "SNPSLMD_LICENSE_FILE",
   "test-lbist-integration" => "SNPSLMD_LICENSE_FILE",
   "test-lbist-synthesis" => "SNPSLMD_LICENSE_FILE",
   "test-map" => "SNPSLMD_LICENSE_FILE",
   "test-mbist-integration" => "SNPSLMD_LICENSE_FILE",
   "test-physical" => "SNPSLMD_LICENSE_FILE",
   "test-power" => "SNPSLMD_LICENSE_FILE",
   "test-rtl-check" => "SNPSLMD_LICENSE_FILE",
   "test-rtl-tristate" => "SNPSLMD_LICENSE_FILE",
   "test-scanroute" => "SNPSLMD_LICENSE_FILE",
   "test-sdd-timing" => "SNPSLMD_LICENSE_FILE",
   "test-stdvr" => "SNPSLMD_LICENSE_FILE",
   "test-validate" => "SNPSLMD_LICENSE_FILE",
   "testify" => "SNPSLMD_LICENSE_FILE",
   "testify_netlister" => "SNPSLMD_LICENSE_FILE",
   "therm_tool" => "SNPSLMD_LICENSE_FILE",
   "tlu_tool" => "SNPSLMD_LICENSE_FILE",
   "tr_analysis" => "SNPSLMD_LICENSE_FILE",
   "vcsamscompiler_net" => "SNPSLMD_LICENSE_FILE",
   "vcsamsruntime_net" => "SNPSLMD_LICENSE_FILE",
   "vcscompiler_net" => "SNPSLMD_LICENSE_FILE",
   "vcsmxruntime_net" => "SNPSLMD_LICENSE_FILE",
   "vcspostprocdebugger_net" => "SNPSLMD_LICENSE_FILE",
   "vcsruntime_net" => "SNPSLMD_LICENSE_FILE",
   "vcsruntimelimited_net" => "SNPSLMD_LICENSE_FILE",
   "vcstools_net" => "SNPSLMD_LICENSE_FILE",
   "vehicle_electric_lib" => "SNPSLMD_LICENSE_FILE",
   "vendor_library" => "SNPSLMD_LICENSE_FILE",
   "verilog_compiler" => "SNPSLMD_LICENSE_FILE",
   "vhdl-analyzer" => "SNPSLMD_LICENSE_FILE",
   "vhdl-compiler" => "SNPSLMD_LICENSE_FILE",
   "vhdl-cycle-sim" => "SNPSLMD_LICENSE_FILE",
   "vhdl-elaborator" => "SNPSLMD_LICENSE_FILE",
   "vhdl-event-sim" => "SNPSLMD_LICENSE_FILE",
   "vhdl-tools" => "SNPSLMD_LICENSE_FILE",
   "vhdl-virsim" => "SNPSLMD_LICENSE_FILE",
   "vhdl_compiler" => "SNPSLMD_LICENSE_FILE",
   "viewlogic_frameway" => "SNPSLMD_LICENSE_FILE",
   "viewlogic_net" => "SNPSLMD_LICENSE_FILE",
   "vlog2e" => "SNPSLMD_LICENSE_FILE",
   "vt_assertions" => "SNPSLMD_LICENSE_FILE",
   "vt_assertionsruntime" => "SNPSLMD_LICENSE_FILE",
   "vt_coverage" => "SNPSLMD_LICENSE_FILE",
   "vt_coverageruntime" => "SNPSLMD_LICENSE_FILE",
   "vt_coverageurg" => "SNPSLMD_LICENSE_FILE",
   "vt_testbench" => "SNPSLMD_LICENSE_FILE",
   "vt_testbenchruntime" => "SNPSLMD_LICENSE_FILE",
   "vt_vcs_beta_program" => "SNPSLMD_LICENSE_FILE",
   "vt_vcs_congruency" => "SNPSLMD_LICENSE_FILE",
   "vt_vcs_congruencyruntime" => "SNPSLMD_LICENSE_FILE",
   "vt_vcs_native_lp" => "SNPSLMD_LICENSE_FILE",
   "vt_vcs_nlp_signals" => "SNPSLMD_LICENSE_FILE",
   "vt_vcs_xprop" => "SNPSLMD_LICENSE_FILE",
   "vt_vcs_xpropruntime" => "SNPSLMD_LICENSE_FILE",
   "vt_visual" => "SNPSLMD_LICENSE_FILE",
   "wanr_synplify_global" => "SNPSLMD_LICENSE_FILE",
   "wanr_synplifypremier_global" => "SNPSLMD_LICENSE_FILE",
   "wanr_synplifypremierdp_global" => "SNPSLMD_LICENSE_FILE",
   "wanr_synplifypro_global" => "SNPSLMD_LICENSE_FILE",
   "wf_api" => "SNPSLMD_LICENSE_FILE",
   "wf_api_hspice" => "SNPSLMD_LICENSE_FILE",
   "wf_api_starsim" => "SNPSLMD_LICENSE_FILE",
   "xsim" => "SNPSLMD_LICENSE_FILE",
   "xvcsdebugger" => "SNPSLMD_LICENSE_FILE",
);

my %alias = (
    "verilog" => ["verilog-xl","affirma_nc_simulator"],
    "drc" => ["assura_drc"],
    "lvs" => ["assura_lvs"],
    "assuradrc" => ["assura_drc"],
    "assuralvs" => ["assura_lvs"],
    "nano" => ["encounter_digital_impl_sys_xl"],
    "rtl" => ["rtl_compiler_verification"],
    "ccar" => ["cadence_chip_assembly_router"],
    "vxl" => ["virtuoso_xl"],
    "herc" => ["hercules_manager","hercules_device","hercules_mask","milkyway"],
    "star" => ["star-rc2_manager","star-rc2-db","star-rc2","star-r"],
);

my $verbose=0;
my $wait=0;
my $sleep=2;

my $usage_string = <<ES;
    --count      Number needed (default 1)
    --verbose
    --wait       Wait for a licenses to be available
ES

my %license;

sub usage {
    print <<EF;
Usage: checklic [options] <licenses>
$usage_string
EF
if ($verbose) {
    &readlicense;
    print "Available License Options:";
    foreach my $lic (sort ((keys %license),(keys %alias))) {
       print "  $lic";
    }
}
# when this fails, still tell it there is a license to prevent
# hanging for invalid input
exit 0;
}


sub getlicensefiles {
    # a random file with all licenses
    open (DIR, "<$licsource");
    my @f;
    my %lic=();
    while (<DIR>) {
        chomp;
        if (/export ([A-Z]+_LICENSE_FILE)=(\S+)/) {
            my $var=$1;
            my $serv=$2;
            my @fixserv=();
            foreach my $lic (split(/:/, $serv)) {
                $lic{$lic}=1 if $lic =~ /@/;
                if ($lic =~ /^\$/) {
                    my $xlic = $lic;
                    $xlic =~ s/^\$//;
                    $lic=$licensefiles{$xlic};
                }
                push @fixserv, $lic;
            }
            $licensefiles{$var}=join(":", @fixserv);
        }
    }
    close DIR;
    foreach my $f (sort keys %lic) {
        push @licensefiles, $f;
    }
}

sub checklic {
    my @lic=@_;
    my $err=0;
    foreach my $lic (@lic) {
        if (defined ($license{$lic})) {
            print "chk $lic $license{$lic}" if $verbose;
            $err += ($license{$lic} < $cnt);
        }
        else {
            print STDERR "No license $lic" if $verbose;
            exit 1;
        }
    }
    $err;
}

sub readlicense {
    my (@lic)=@_;
    local (*P,$_);
    my @f;
    foreach my $lic (@lic) {
        my $licfile=$licensefiles{LM_LICENSE_FILE};
        $licfile=$licensefiles{$serverlist{$lic}} if defined $serverlist{$lic};
        open (P, "LM_LICENSE_FILE= $lmstat -c $licfile -f $lic 2>\&1 |");
        while (<P>) {
            if (/^Users/) {
                chomp;
                @f=split;
                if (defined ($f[10]) and $f[10] =~ /^\d+$/ and
                        $f[5] =~ /^\d+$/) {
                    my $total=$f[5];
                    my $used=$f[10];
                    my $lic=$f[2];
                    $lic =~ s/://;
                    $lic =~ tr/A-Z/a-z/;
                    $license{$lic} += $total-$used;
                }
            }
        }
        close P;
    }
    if ($verbose) {
        foreach my $lic (sort keys %license) {
            print "$lic $license{$lic}";
        }
    }
}

GetOptions (
    "verbose|v" => \$verbose,
    "wait|w" => \$wait,
    "count|cnt|c=n" => \$cnt,
) or usage;

usage if ! defined ($ARGV[0]);

getlicensefiles;
@licensefiles=($licensefiles{'SNPSLMD_LICENSE_FILE'});
#if ($verbose) {
#    foreach my $lic (keys %alias) {
#        foreach my $sub (@{$alias{$lic}}) {
#            print "Illegal alias $sub" if (! defined ($license{$sub}) and $verbose);
#        }
#    }
#}

my @lic;
foreach my $lic (@ARGV) {
    $lic =~ tr/A-Z/a-z/;
    if (defined ($alias{$lic})) {
        foreach my $l (@{$alias{$lic}}) {
            push @lic,$l;
        }
    }
    else {
        push @lic,$lic;
    }
}

readlicense(@lic);
$cnt=1 if $cnt <= 0;
$"=" ";
if (defined ($license{$lic[0]})) {
    my $err=checklic(@lic);
    print "err @lic $err" if $verbose;
    exit ($err) if (! $wait or ! $err);
    while ($err) {
        sleep $sleep;
        readlicense;
        $err=checklic(@lic);
        print "err @lic $err" if $verbose;
    }
}
else {
    print STDERR "No license $lic[0]" if $verbose;
    exit 1;
}
# always ok for undefined license
exit 0;
