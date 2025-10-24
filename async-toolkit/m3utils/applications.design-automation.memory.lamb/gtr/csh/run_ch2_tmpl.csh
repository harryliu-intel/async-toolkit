#!/usr/bin/csh -f

if ( $#argv == 0 ) then
    echo Error: Usage: run.csh block_name
    exit
endif

set block=$1

echo Running Block:${block}

Ifc_shell -B $block -F apr_fc -output_log_file logs/fc.import_design.log -f $ward/global/snps/apr_fc/run_import_design.tcl
Ifc_shell -B $block -F apr_fc -output_log_file logs/fc.read_upf.log -f $ward/global/snps/apr_fc/run_read_upf.tcl
Ifc_shell -B $block -F apr_fc -output_log_file logs/fc.redefine.log -f $ward/global/snps/apr_fc/run_redefine.tcl
Ifc_shell -B $block -F apr_fc -output_log_file logs/fc.init_floorplan.log -f $ward/global/snps/apr_fc/run_init_floorplan.tcl
Ifc_shell -B $block -F apr_fc -output_log_file logs/fc.setup_timing.log -f $ward/global/snps/apr_fc/run_setup_timing.tcl
Ifc_shell -B $block -F apr_fc -output_log_file logs/fc.initial_map.log -f $ward/global/snps/apr_fc/run_initial_map.tcl
Ifc_shell -B $block -F apr_fc -output_log_file logs/fc.floorplan.log -f $ward/global/snps/apr_fc/run_floorplan.tcl
Ifc_shell -B $block -F apr_fc -output_log_file logs/fc.logic_opto.log -f $ward/global/snps/apr_fc/run_logic_opto.tcl
Ifc_shell -B $block -F apr_fc -output_log_file logs/fc.insert_dft.log -f $ward/global/snps/apr_fc/run_insert_dft.tcl
Ifc_shell -B $block -F apr_fc -output_log_file logs/fc.compile_initial_opto.log -f $ward/global/snps/apr_fc/run_compile_initial_opto.tcl
Ifc_shell -B $block -F apr_fc -output_log_file logs/fc.compile_final_opto.log -f $ward/global/snps/apr_fc/run_compile_final_opto.tcl
Ifc_shell -B $block -F apr_fc -output_log_file logs/fc.ctmesh.log -f $ward/global/snps/apr_fc/run_ctmesh.tcl
Ifc_shell -B $block -F apr_fc -output_log_file logs/fc.cts.log -f $ward/global/snps/apr_fc/run_cts.tcl
Ifc_shell -B $block -F apr_fc -output_log_file logs/fc.clock_route_opt.log -f $ward/global/snps/apr_fc/run_clock_route_opt.tcl
Ifc_shell -B $block -F apr_fc -output_log_file logs/fc.route_auto.log -f $ward/global/snps/apr_fc/run_route_auto.tcl
Ifc_shell -B $block -F apr_fc -output_log_file logs/fc.route_opt.log -f $ward/global/snps/apr_fc/run_route_opt.tcl
Ifc_shell -B $block -F apr_fc -output_log_file logs/fc.fill.log -f $ward/global/snps/apr_fc/run_fill.tcl
Ifc_shell -B $block -F apr_fc -output_log_file logs/fc.finish.log -f $ward/global/snps/apr_fc/run_finish.tcl

echo Finished Running Block:${block}
