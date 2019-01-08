# Begin_DVE_Session_Save_Info
# DVE full session
# Saved on Sun Nov 4 18:26:18 2018
# Designs open: 1
#   V1: vcdplus.vpd
# Toplevel windows open: 2
# 	TopLevel.1
# 	TopLevel.2
#   Source.1: TB_IGR_TOP.top.epl_shim_0.segs0.seg0
#   Wave.1: 96 signals
#   Group count = 2
#   Group Group1 signal count = 19
#   Group Group2 signal count = 77
# End_DVE_Session_Save_Info

# DVE version: N-2017.12-SP1_Full64
# DVE build date: Mar  4 2018 21:33:51


#<Session mode="Full" path="/nfs/sc/disks/slx_1130/edwardro/mby/work_root/mby.edr.1/subBlock/mbyc/src/igr/sandbox/edrsim/session.vcdplus.vpd.tcl" type="Debug">

gui_set_loading_session_type Post
gui_continuetime_set

# Close design
if { [gui_sim_state -check active] } {
    gui_sim_terminate
}
gui_close_db -all
gui_expr_clear_all

# Close all windows
gui_close_window -type Console
gui_close_window -type Wave
gui_close_window -type Source
gui_close_window -type Schematic
gui_close_window -type Data
gui_close_window -type DriverLoad
gui_close_window -type List
gui_close_window -type Memory
gui_close_window -type HSPane
gui_close_window -type DLPane
gui_close_window -type Assertion
gui_close_window -type CovHier
gui_close_window -type CoverageTable
gui_close_window -type CoverageMap
gui_close_window -type CovDetail
gui_close_window -type Local
gui_close_window -type Stack
gui_close_window -type Watch
gui_close_window -type Group
gui_close_window -type Transaction



# Application preferences
gui_set_pref_value -key app_default_font -value {Helvetica,10,-1,5,50,0,0,0,0,0}
gui_src_preferences -tabstop 8 -maxbits 24 -windownumber 1
#<WindowLayout>

# DVE top-level session


# Create and position top-level window: TopLevel.1

if {![gui_exist_window -window TopLevel.1]} {
    set TopLevel.1 [ gui_create_window -type TopLevel \
       -icon $::env(DVE)/auxx/gui/images/toolbars/dvewin.xpm] 
} else { 
    set TopLevel.1 TopLevel.1
}
gui_show_window -window ${TopLevel.1} -show_state normal -rect {{59 444} {2852 1481}}

# ToolBar settings
gui_set_toolbar_attributes -toolbar {TimeOperations} -dock_state top
gui_set_toolbar_attributes -toolbar {TimeOperations} -offset 0
gui_show_toolbar -toolbar {TimeOperations}
gui_hide_toolbar -toolbar {&File}
gui_set_toolbar_attributes -toolbar {&Edit} -dock_state top
gui_set_toolbar_attributes -toolbar {&Edit} -offset 0
gui_show_toolbar -toolbar {&Edit}
gui_hide_toolbar -toolbar {CopyPaste}
gui_set_toolbar_attributes -toolbar {&Trace} -dock_state top
gui_set_toolbar_attributes -toolbar {&Trace} -offset 0
gui_show_toolbar -toolbar {&Trace}
gui_hide_toolbar -toolbar {TraceInstance}
gui_hide_toolbar -toolbar {BackTrace}
gui_hide_toolbar -toolbar {&Scope}
gui_set_toolbar_attributes -toolbar {&Window} -dock_state top
gui_set_toolbar_attributes -toolbar {&Window} -offset 0
gui_show_toolbar -toolbar {&Window}
gui_set_toolbar_attributes -toolbar {Signal} -dock_state top
gui_set_toolbar_attributes -toolbar {Signal} -offset 0
gui_show_toolbar -toolbar {Signal}
gui_set_toolbar_attributes -toolbar {Zoom} -dock_state top
gui_set_toolbar_attributes -toolbar {Zoom} -offset 0
gui_show_toolbar -toolbar {Zoom}
gui_set_toolbar_attributes -toolbar {Zoom And Pan History} -dock_state top
gui_set_toolbar_attributes -toolbar {Zoom And Pan History} -offset 0
gui_show_toolbar -toolbar {Zoom And Pan History}
gui_set_toolbar_attributes -toolbar {Grid} -dock_state top
gui_set_toolbar_attributes -toolbar {Grid} -offset 0
gui_show_toolbar -toolbar {Grid}
gui_hide_toolbar -toolbar {Simulator}
gui_hide_toolbar -toolbar {Interactive Rewind}
gui_hide_toolbar -toolbar {Testbench}

# End ToolBar settings

# Docked window settings
set HSPane.1 [gui_create_window -type HSPane -parent ${TopLevel.1} -dock_state left -dock_on_new_line true -dock_extent 817]
catch { set Hier.1 [gui_share_window -id ${HSPane.1} -type Hier] }
gui_set_window_pref_key -window ${HSPane.1} -key dock_width -value_type integer -value 817
gui_set_window_pref_key -window ${HSPane.1} -key dock_height -value_type integer -value -1
gui_set_window_pref_key -window ${HSPane.1} -key dock_offset -value_type integer -value 0
gui_update_layout -id ${HSPane.1} {{left 0} {top 0} {width 816} {height 746} {dock_state left} {dock_on_new_line true} {child_hier_colhier 683} {child_hier_coltype 127} {child_hier_colpd 0} {child_hier_col1 0} {child_hier_col2 1} {child_hier_col3 -1}}
set DLPane.1 [gui_create_window -type DLPane -parent ${TopLevel.1} -dock_state left -dock_on_new_line true -dock_extent 337]
catch { set Data.1 [gui_share_window -id ${DLPane.1} -type Data] }
gui_set_window_pref_key -window ${DLPane.1} -key dock_width -value_type integer -value 337
gui_set_window_pref_key -window ${DLPane.1} -key dock_height -value_type integer -value 654
gui_set_window_pref_key -window ${DLPane.1} -key dock_offset -value_type integer -value 0
gui_update_layout -id ${DLPane.1} {{left 0} {top 0} {width 336} {height 746} {dock_state left} {dock_on_new_line true} {child_data_colvariable 204} {child_data_colvalue 56} {child_data_coltype 76} {child_data_col1 0} {child_data_col2 1} {child_data_col3 2}}
set DriverLoad.1 [gui_create_window -type DriverLoad -parent ${TopLevel.1} -dock_state bottom -dock_on_new_line false -dock_extent 174]
gui_set_window_pref_key -window ${DriverLoad.1} -key dock_width -value_type integer -value 150
gui_set_window_pref_key -window ${DriverLoad.1} -key dock_height -value_type integer -value 174
gui_set_window_pref_key -window ${DriverLoad.1} -key dock_offset -value_type integer -value 0
gui_update_layout -id ${DriverLoad.1} {{left 0} {top 0} {width 2793} {height 173} {dock_state bottom} {dock_on_new_line false}}
#### Start - Readjusting docked view's offset / size
set dockAreaList { top left right bottom }
foreach dockArea $dockAreaList {
  set viewList [gui_ekki_get_window_ids -active_parent -dock_area $dockArea]
  foreach view $viewList {
      if {[lsearch -exact [gui_get_window_pref_keys -window $view] dock_width] != -1} {
        set dockWidth [gui_get_window_pref_value -window $view -key dock_width]
        set dockHeight [gui_get_window_pref_value -window $view -key dock_height]
        set offset [gui_get_window_pref_value -window $view -key dock_offset]
        if { [string equal "top" $dockArea] || [string equal "bottom" $dockArea]} {
          gui_set_window_attributes -window $view -dock_offset $offset -width $dockWidth
        } else {
          gui_set_window_attributes -window $view -dock_offset $offset -height $dockHeight
        }
      }
  }
}
#### End - Readjusting docked view's offset / size
gui_sync_global -id ${TopLevel.1} -option true

# MDI window settings
set Console.1 [gui_create_window -type {Console}  -parent ${TopLevel.1}]
gui_show_window -window ${Console.1} -show_state maximized
gui_update_layout -id ${Console.1} {{show_state maximized} {dock_state undocked} {dock_on_new_line false}}
set Source.1 [gui_create_window -type {Source}  -parent ${TopLevel.1}]
gui_show_window -window ${Source.1} -show_state maximized
gui_update_layout -id ${Source.1} {{show_state maximized} {dock_state undocked} {dock_on_new_line false}}

# End MDI window settings


# Create and position top-level window: TopLevel.2

if {![gui_exist_window -window TopLevel.2]} {
    set TopLevel.2 [ gui_create_window -type TopLevel \
       -icon $::env(DVE)/auxx/gui/images/toolbars/dvewin.xpm] 
} else { 
    set TopLevel.2 TopLevel.2
}
gui_show_window -window ${TopLevel.2} -show_state normal -rect {{4 25} {2095 1019}}

# ToolBar settings
gui_set_toolbar_attributes -toolbar {TimeOperations} -dock_state top
gui_set_toolbar_attributes -toolbar {TimeOperations} -offset 0
gui_show_toolbar -toolbar {TimeOperations}
gui_hide_toolbar -toolbar {&File}
gui_set_toolbar_attributes -toolbar {&Edit} -dock_state top
gui_set_toolbar_attributes -toolbar {&Edit} -offset 0
gui_show_toolbar -toolbar {&Edit}
gui_hide_toolbar -toolbar {CopyPaste}
gui_set_toolbar_attributes -toolbar {&Trace} -dock_state top
gui_set_toolbar_attributes -toolbar {&Trace} -offset 0
gui_show_toolbar -toolbar {&Trace}
gui_hide_toolbar -toolbar {TraceInstance}
gui_hide_toolbar -toolbar {BackTrace}
gui_hide_toolbar -toolbar {&Scope}
gui_set_toolbar_attributes -toolbar {&Window} -dock_state top
gui_set_toolbar_attributes -toolbar {&Window} -offset 0
gui_show_toolbar -toolbar {&Window}
gui_set_toolbar_attributes -toolbar {Signal} -dock_state top
gui_set_toolbar_attributes -toolbar {Signal} -offset 0
gui_show_toolbar -toolbar {Signal}
gui_set_toolbar_attributes -toolbar {Zoom} -dock_state top
gui_set_toolbar_attributes -toolbar {Zoom} -offset 0
gui_show_toolbar -toolbar {Zoom}
gui_set_toolbar_attributes -toolbar {Zoom And Pan History} -dock_state top
gui_set_toolbar_attributes -toolbar {Zoom And Pan History} -offset 0
gui_show_toolbar -toolbar {Zoom And Pan History}
gui_set_toolbar_attributes -toolbar {Grid} -dock_state top
gui_set_toolbar_attributes -toolbar {Grid} -offset 0
gui_show_toolbar -toolbar {Grid}
gui_hide_toolbar -toolbar {Simulator}
gui_hide_toolbar -toolbar {Interactive Rewind}
gui_set_toolbar_attributes -toolbar {Testbench} -dock_state top
gui_set_toolbar_attributes -toolbar {Testbench} -offset 0
gui_show_toolbar -toolbar {Testbench}

# End ToolBar settings

# Docked window settings
gui_sync_global -id ${TopLevel.2} -option true

# MDI window settings
set Wave.1 [gui_create_window -type {Wave}  -parent ${TopLevel.2}]
gui_show_window -window ${Wave.1} -show_state maximized
gui_update_layout -id ${Wave.1} {{show_state maximized} {dock_state undocked} {dock_on_new_line false} {child_wave_left 775} {child_wave_right 1311} {child_wave_colname 306} {child_wave_colvalue 465} {child_wave_col1 0} {child_wave_col2 1}}

# End MDI window settings

gui_set_env TOPLEVELS::TARGET_FRAME(Source) ${TopLevel.1}
gui_set_env TOPLEVELS::TARGET_FRAME(Schematic) ${TopLevel.1}
gui_set_env TOPLEVELS::TARGET_FRAME(PathSchematic) ${TopLevel.1}
gui_set_env TOPLEVELS::TARGET_FRAME(Wave) none
gui_set_env TOPLEVELS::TARGET_FRAME(List) none
gui_set_env TOPLEVELS::TARGET_FRAME(Memory) ${TopLevel.1}
gui_set_env TOPLEVELS::TARGET_FRAME(DriverLoad) none
gui_update_statusbar_target_frame ${TopLevel.1}
gui_update_statusbar_target_frame ${TopLevel.2}

#</WindowLayout>

#<Database>

# DVE Open design session: 

if { ![gui_is_db_opened -db {vcdplus.vpd}] } {
	gui_open_db -design V1 -file vcdplus.vpd -nosource
}
gui_set_precision 1ps
gui_set_time_units 1ps
#</Database>

# DVE Global setting session: 


# Global: Bus

# Global: Expressions

# Global: Signal Time Shift

# Global: Signal Compare

# Global: Signal Groups
gui_load_child_values {TB_IGR_TOP.top.epl_shim_0.ctrl0}
gui_load_child_values {TB_IGR_TOP.top.epl_shim_0.segs0}
gui_load_child_values {TB_IGR_TOP.top.igr_pb}
gui_load_child_values {TB_IGR_TOP.top.igr_pb.pb_mem_shell_0.gen_mem_shell_banks[0].u_mem_bank_shell_1024x644.u_master_1rw_shell}
gui_load_child_values {TB_IGR_TOP.top.igr_pb.pb_mem_shell_0}
gui_load_child_values {TB_IGR_TOP.top.igr_pb.pb_mem_shell_0.gen_mem_shell_banks[0].u_mem_bank_shell_1024x644}
gui_load_child_values {TB_IGR_TOP.top.pb_srams.igr_wrap_mem_igr_pb0_ram_shell_1024x644_0.behave_mem}
gui_load_child_values {TB_IGR_TOP.top.dpc_wrap}
gui_load_child_values {TB_IGR_TOP.top}
gui_load_child_values {TB_IGR_TOP.top.epl_shim_0}
gui_load_child_values {TB_IGR_TOP.top.pb_srams.igr_wrap_mem_igr_pb0_ram_shell_1024x644_0}


set _session_group_1 Group1
gui_sg_create "$_session_group_1"
set Group1 "$_session_group_1"

gui_sg_addsignal -group "$_session_group_1" { TB_IGR_TOP.top.cclk TB_IGR_TOP.top.rst TB_IGR_TOP.top.grp_a_rx_data_valid TB_IGR_TOP.top.grp_a_rx_metadata TB_IGR_TOP.top.grp_a_rx_time_stamp TB_IGR_TOP.top.grp_a_rx_data_w_ecc TB_IGR_TOP.top.dpc_wrap.o_dpc_pb0_p0 TB_IGR_TOP.top.dpc_wrap.grp_a_rx_data_valid TB_IGR_TOP.top.dpc_wrap.dpc_0.s1_cnt_ones TB_IGR_TOP.top.dpc_wrap.dpc_0.qs2_dpc_ts_p0 TB_IGR_TOP.top.dpc_wrap.dpc_0.s1_or_rx_data_v TB_IGR_TOP.top.dpc_wrap.dpc_0.s1_port_num TB_IGR_TOP.top.dpc_wrap.dpc_0.qs2_dpc_md_p0 TB_IGR_TOP.top.dpc_wrap.dpc_0.qs2_rx_data_p0 TB_IGR_TOP.top.dpc_wrap.dpc_0.o_dpc_pb_p0 TB_IGR_TOP.top.dpc_wrap.dpc_0.s1_or_rx_data_v TB_IGR_TOP.top.dpc_wrap.dpc_0.qs2_or_rx_data_v TB_IGR_TOP.top.dpc_wrap.dpc_0.qs2_port_v TB_IGR_TOP.top.dpc_wrap.dpc_0.cclk }

set _session_group_2 Group2
gui_sg_create "$_session_group_2"
set Group2 "$_session_group_2"

gui_sg_addsignal -group "$_session_group_2" { TB_IGR_TOP.top.dpc_wrap.o_dpc_pb0_p0 TB_IGR_TOP.top.igr_pb.pb_ctrl_p0.o_pb_shell_ctrl_wdata TB_IGR_TOP.top.igr_pb.pb_ctrl_p0.shell_mem_we_bnk TB_IGR_TOP.top.igr_pb.o_igr_igr_pb0_ram_0_to_mem TB_IGR_TOP.top.igr_pb.o_igr_igr_pb0_ram_1_to_mem TB_IGR_TOP.top.igr_pb.o_igr_igr_pb0_ram_2_to_mem TB_IGR_TOP.top.igr_pb.o_igr_igr_pb0_ram_3_to_mem TB_IGR_TOP.top.pb_srams.igr_wrap_mem_igr_pb0_ram_shell_1024x644_0.behave_mem.clk TB_IGR_TOP.top.pb_srams.igr_wrap_mem_igr_pb0_ram_shell_1024x644_0.behave_mem.address TB_IGR_TOP.top.pb_srams.igr_wrap_mem_igr_pb0_ram_shell_1024x644_0.behave_mem.rd_en TB_IGR_TOP.top.pb_srams.igr_wrap_mem_igr_pb0_ram_shell_1024x644_0.behave_mem.wr_en TB_IGR_TOP.top.pb_srams.igr_wrap_mem_igr_pb0_ram_shell_1024x644_0.behave_mem.data_in TB_IGR_TOP.top.pb_srams.igr_wrap_mem_igr_pb0_ram_shell_1024x644_0.behave_mem.data_out TB_IGR_TOP.top.pb_srams.igr_wrap_mem_igr_pb0_ram_shell_1024x644_0.wr_en TB_IGR_TOP.top.igr_pb.pb_mem_shell_0.i_pb_shell_ctrl_wdata {TB_IGR_TOP.top.igr_pb.pb_mem_shell_0.gen_mem_shell_banks[0].u_mem_bank_shell_1024x644.u_master_1rw_shell.wr_en_mux} {TB_IGR_TOP.top.igr_pb.pb_mem_shell_0.gen_mem_shell_banks[0].u_mem_bank_shell_1024x644.u_master_1rw_shell.inner_ls_delay} {TB_IGR_TOP.top.igr_pb.pb_mem_shell_0.gen_mem_shell_banks[0].u_mem_bank_shell_1024x644.u_master_1rw_shell.inner_ls_delay_comp} {TB_IGR_TOP.top.igr_pb.pb_mem_shell_0.gen_mem_shell_banks[0].u_mem_bank_shell_1024x644.u_master_1rw_shell.init_done} TB_IGR_TOP.top.igr_pb.pb_ctrl_p0.i_pb_shell_rdata TB_IGR_TOP.top.pb_srams.igr_wrap_mem_igr_pb0_ram_shell_1024x644_0.wrap_shell_from_mem TB_IGR_TOP.top.pb_srams.igr_wrap_mem_igr_pb0_ram_shell_1024x644_0.rd_valid TB_IGR_TOP.top.pb_srams.igr_wrap_mem_igr_pb0_ram_shell_1024x644_0.rd_en TB_IGR_TOP.top.igr_pb.pb_mem_shell_0.i_pb_shell_ctrl_wdata {TB_IGR_TOP.top.igr_pb.pb_mem_shell_0.gen_mem_shell_banks[0].u_mem_bank_shell_1024x644.rd_en} {TB_IGR_TOP.top.igr_pb.pb_mem_shell_0.gen_mem_shell_banks[0].u_mem_bank_shell_1024x644.u_master_1rw_shell.rd_en_mux} {TB_IGR_TOP.top.igr_pb.pb_mem_shell_0.gen_mem_shell_banks[0].u_mem_bank_shell_1024x644.u_master_1rw_shell.to_mem_bus} {TB_IGR_TOP.top.igr_pb.pb_mem_shell_0.gen_mem_shell_banks[0].u_mem_bank_shell_1024x644.u_master_1rw_shell.to_mem_bus.rd_en} TB_IGR_TOP.top.pb_srams.igr_wrap_mem_igr_pb0_ram_shell_1024x644_0.rd_en {TB_IGR_TOP.top.igr_pb.pb_mem_shell_0.gen_mem_shell_banks[0].u_mem_bank_shell_1024x644.u_master_1rw_shell.wrap_shell_to_mem} TB_IGR_TOP.top.pb_srams.igr_wrap_mem_igr_pb0_ram_shell_1024x644_0.wrap_shell_to_mem TB_IGR_TOP.top.pb_srams.igr_wrap_mem_igr_pb0_ram_shell_1024x644_0.wrap_shell_to_mem TB_IGR_TOP.top.igr_pb.pb_ctrl_p0.i_pb_shell_rdata TB_IGR_TOP.top.igr_pb.pb_ctrl_p0.o_pb_shim TB_IGR_TOP.top.epl_shim_0.o_shim_pb_data_p0 TB_IGR_TOP.top.epl_shim_0.i_pb_shim TB_IGR_TOP.top.epl_shim_0.o_shim_pb_data_p0 TB_IGR_TOP.top.epl_shim_0.qs1_data_p0 TB_IGR_TOP.top.epl_shim_0.qs1_tsmd_p0 TB_IGR_TOP.top.epl_shim_0.ctrl0.i_port_e TB_IGR_TOP.top.epl_shim_0.ctrl0.cnt_ones TB_IGR_TOP.top.epl_shim_0.ctrl0.qs2_rx_v TB_IGR_TOP.top.epl_shim_0.ctrl0.qs2_cnt_ones TB_IGR_TOP.top.epl_shim_0.ctrl0.qs2_eflit TB_IGR_TOP.top.epl_shim_0.ctrl0.qs2_sflit TB_IGR_TOP.top.epl_shim_0.ctrl0.qs2_sop_v TB_IGR_TOP.top.epl_shim_0.ctrl0.current_flit TB_IGR_TOP.top.epl_shim_0.ctrl0.o_seg0_md TB_IGR_TOP.top.epl_shim_0.ctrl0.o_seg0_ts TB_IGR_TOP.top.epl_shim_0.ctrl0.o_seg0_sop_e TB_IGR_TOP.top.epl_shim_0.ctrl0.o_seg0_sel TB_IGR_TOP.top.epl_shim_0.ctrl0.o_seg0_we TB_IGR_TOP.top.epl_shim_0.ctrl0.o_seg_e TB_IGR_TOP.top.epl_shim_0.segs0.i_rx_data TB_IGR_TOP.top.epl_shim_0.segs0.o_shim_pb_data TB_IGR_TOP.top.epl_shim_0.segs0.o_shim_pb_v TB_IGR_TOP.top.epl_shim_0.segs0.i_rx_data TB_IGR_TOP.top.epl_shim_0.segs0.i_seg0_md TB_IGR_TOP.top.epl_shim_0.segs0.i_seg_e TB_IGR_TOP.top.epl_shim_0.segs0.i_seg0_we TB_IGR_TOP.top.epl_shim_0.segs0.seg0.i_rx_data TB_IGR_TOP.top.epl_shim_0.segs0.seg0.i_rx_ts TB_IGR_TOP.top.epl_shim_0.segs0.seg0.i_seg_sop_e TB_IGR_TOP.top.epl_shim_0.segs0.seg0.i_seg_we TB_IGR_TOP.top.epl_shim_0.segs0.seg0.i_seg_e }
gui_sg_addsignal -group "$_session_group_2" { TB_IGR_TOP.top.epl_shim_0.segs0.seg0.o_seg_data TB_IGR_TOP.top.epl_shim_0.segs0.seg0.seg_sop TB_IGR_TOP.top.epl_shim_0.segs0.seg0.i_seg_sel TB_IGR_TOP.top.epl_shim_0.segs0.seg0.s4q_rx_data_seg TB_IGR_TOP.top.epl_shim_0.segs0.seg0.i_seg_we TB_IGR_TOP.top.epl_shim_0.segs0.seg0.rx_data_seg TB_IGR_TOP.top.epl_shim_0.segs0.seg0.s4q_rx_data_seg TB_IGR_TOP.top.epl_shim_0.segs0.seg0.s4q_rx_data_seg TB_IGR_TOP.top.epl_shim_0.segs0.seg0.rx_data_seg TB_IGR_TOP.top.epl_shim_0.segs0.seg0.i_seg_we TB_IGR_TOP.top.epl_shim_0.segs0.seg0.cclk TB_IGR_TOP.top.epl_shim_0.segs0.seg0.i_seg_sel }

# Global: Highlighting
gui_highlight_signals -color #00ff00 {{TB_IGR_TOP.top.igr_pb.pb_ctrl_p0.pb_shell_ctrl_wdata[0].rd_en} {TB_IGR_TOP.top.igr_pb.pb_ctrl_p0.pb_shell_ctrl_wdata[0]} {TB_IGR_TOP.top.igr_pb.pb_ctrl_p0.pb_shell_ctrl_wdata[3:0]} {TB_IGR_TOP.top.dpc_wrap.dpc_0.qs1_rx_port_num[1:0]}}

# Global: Stack
gui_change_stack_mode -mode list

# Post database loading setting...

# Restore C1 time
gui_set_time -C1_only 268



# Save global setting...

# Wave/List view global setting
gui_cov_show_value -switch false

# Close all empty TopLevel windows
foreach __top [gui_ekki_get_window_ids -type TopLevel] {
    if { [llength [gui_ekki_get_window_ids -parent $__top]] == 0} {
        gui_close_window -window $__top
    }
}
gui_set_loading_session_type noSession
# DVE View/pane content session: 


# Hier 'Hier.1'
gui_show_window -window ${Hier.1}
gui_list_set_filter -id ${Hier.1} -list { {Package 1} {All 0} {Process 1} {VirtPowSwitch 0} {UnnamedProcess 1} {UDP 0} {Function 1} {Block 1} {SrsnAndSpaCell 0} {OVA Unit 1} {LeafScCell 1} {LeafVlgCell 1} {Interface 1} {LeafVhdCell 1} {$unit 1} {NamedBlock 1} {Task 1} {VlgPackage 1} {ClassDef 1} {VirtIsoCell 0} }
gui_list_set_filter -id ${Hier.1} -text {*}
gui_hier_list_init -id ${Hier.1}
gui_change_design -id ${Hier.1} -design V1
catch {gui_list_expand -id ${Hier.1} TB_IGR_TOP}
catch {gui_list_expand -id ${Hier.1} TB_IGR_TOP.top}
catch {gui_list_expand -id ${Hier.1} TB_IGR_TOP.top.epl_shim_0}
catch {gui_list_expand -id ${Hier.1} TB_IGR_TOP.top.epl_shim_0.segs0}
catch {gui_list_select -id ${Hier.1} {TB_IGR_TOP.top.epl_shim_0.segs0.seg0}}
gui_view_scroll -id ${Hier.1} -vertical -set 0
gui_view_scroll -id ${Hier.1} -horizontal -set 0

# Data 'Data.1'
gui_list_set_filter -id ${Data.1} -list { {Buffer 0} {Input 1} {Others 0} {Linkage 0} {Output 1} {LowPower 0} {Parameter 0} {All 0} {Aggregate 0} {LibBaseMember 1} {Event 0} {Assertion 0} {Constant 0} {Interface 0} {BaseMembers 1} {Signal 0} {$unit 0} {Inout 0} {Variable 0} }
gui_list_set_filter -id ${Data.1} -text {*}
gui_list_show_data -id ${Data.1} {TB_IGR_TOP.top.epl_shim_0.segs0.seg0}
gui_view_scroll -id ${Data.1} -vertical -set 0
gui_view_scroll -id ${Data.1} -horizontal -set 0
gui_view_scroll -id ${Hier.1} -vertical -set 0
gui_view_scroll -id ${Hier.1} -horizontal -set 0

# DriverLoad 'DriverLoad.1'
gui_get_drivers -session -id ${DriverLoad.1} -signal TB_IGR_TOP.top.dpc_wrap.dpc_0.qs2_or_rx_data_v -time 3 -starttime 41
gui_get_drivers -session -id ${DriverLoad.1} -signal {TB_IGR_TOP.top.dpc_wrap.dpc_0.s1_port_num[3:0]} -time 1 -starttime 41
gui_get_drivers -session -id ${DriverLoad.1} -signal {TB_IGR_TOP.top.igr_pb.pb_mem_shell_0.gen_mem_shell_banks[0].u_mem_bank_shell_1024x644.rd_en} -time 247 -starttime 247

# Source 'Source.1'
gui_src_value_annotate -id ${Source.1} -switch false
gui_set_env TOGGLE::VALUEANNOTATE 0
gui_open_source -id ${Source.1}  -replace -active TB_IGR_TOP.top.epl_shim_0.segs0.seg0 /nfs/sc/disks/slx_1130/edwardro/mby/work_root/mby.edr.1/subBlock/mbyc/src/igr/epl_shim/rtl/mby_igr_epl_shim_seg.sv
gui_src_value_annotate -id ${Source.1} -switch true
gui_set_env TOGGLE::VALUEANNOTATE 1
gui_view_scroll -id ${Source.1} -vertical -set 1853
gui_src_set_reusable -id ${Source.1}

# View 'Wave.1'
gui_wv_sync -id ${Wave.1} -switch false
set groupExD [gui_get_pref_value -category Wave -key exclusiveSG]
gui_set_pref_value -category Wave -key exclusiveSG -value {false}
set origWaveHeight [gui_get_pref_value -category Wave -key waveRowHeight]
gui_list_set_height -id Wave -height 25
set origGroupCreationState [gui_list_create_group_when_add -wave]
gui_list_create_group_when_add -wave -disable
gui_marker_create -id ${Wave.1} C2 274
gui_marker_select -id ${Wave.1} {  C2 }
gui_marker_set_ref -id ${Wave.1}  C1
gui_wv_zoom_timerange -id ${Wave.1} 257 289
gui_list_add_group -id ${Wave.1} -after {New Group} {Group1}
gui_list_add_group -id ${Wave.1} -after {New Group} {Group2}
gui_list_expand -id ${Wave.1} TB_IGR_TOP.top.dpc_wrap.o_dpc_pb0_p0
gui_list_expand -id ${Wave.1} TB_IGR_TOP.top.dpc_wrap.dpc_0.o_dpc_pb_p0
gui_list_expand -id ${Wave.1} TB_IGR_TOP.top.igr_pb.pb_ctrl_p0.o_pb_shell_ctrl_wdata
gui_list_expand -id ${Wave.1} {TB_IGR_TOP.top.igr_pb.pb_ctrl_p0.o_pb_shell_ctrl_wdata[2]}
gui_list_expand -id ${Wave.1} {TB_IGR_TOP.top.igr_pb.pb_ctrl_p0.o_pb_shell_ctrl_wdata[1]}
gui_list_expand -id ${Wave.1} {TB_IGR_TOP.top.igr_pb.pb_ctrl_p0.o_pb_shell_ctrl_wdata[0]}
gui_list_expand -id ${Wave.1} TB_IGR_TOP.top.igr_pb.pb_mem_shell_0.i_pb_shell_ctrl_wdata
gui_list_expand -id ${Wave.1} {TB_IGR_TOP.top.igr_pb.pb_mem_shell_0.i_pb_shell_ctrl_wdata[0]}
gui_list_expand -id ${Wave.1} TB_IGR_TOP.top.igr_pb.pb_ctrl_p0.i_pb_shell_rdata
gui_list_expand -id ${Wave.1} {TB_IGR_TOP.top.igr_pb.pb_ctrl_p0.i_pb_shell_rdata[0]}
gui_list_expand -id ${Wave.1} {TB_IGR_TOP.top.igr_pb.pb_mem_shell_0.gen_mem_shell_banks[0].u_mem_bank_shell_1024x644.u_master_1rw_shell.to_mem_bus}
gui_list_expand -id ${Wave.1} TB_IGR_TOP.top.igr_pb.pb_ctrl_p0.o_pb_shim
gui_list_expand -id ${Wave.1} TB_IGR_TOP.top.epl_shim_0.i_pb_shim
gui_list_expand -id ${Wave.1} TB_IGR_TOP.top.epl_shim_0.segs0.i_rx_data
gui_list_expand -id ${Wave.1} TB_IGR_TOP.top.epl_shim_0.segs0.o_shim_pb_data
gui_list_expand -id ${Wave.1} TB_IGR_TOP.top.epl_shim_0.segs0.o_shim_pb_data.seg0
gui_list_expand -id ${Wave.1} TB_IGR_TOP.top.epl_shim_0.segs0.o_shim_pb_v
gui_list_select -id ${Wave.1} {TB_IGR_TOP.top.epl_shim_0.segs0.o_shim_pb_data.seg0 }
gui_seek_criteria -id ${Wave.1} {Any Edge}



gui_set_env TOGGLE::DEFAULT_WAVE_WINDOW ${Wave.1}
gui_set_pref_value -category Wave -key exclusiveSG -value $groupExD
gui_list_set_height -id Wave -height $origWaveHeight
if {$origGroupCreationState} {
	gui_list_create_group_when_add -wave -enable
}
if { $groupExD } {
 gui_msg_report -code DVWW028
}
gui_list_set_filter -id ${Wave.1} -list { {Buffer 1} {Input 1} {Others 1} {Linkage 1} {Output 1} {Parameter 1} {All 1} {Aggregate 1} {LibBaseMember 1} {Event 1} {Assertion 1} {Constant 1} {Interface 1} {BaseMembers 1} {Signal 1} {$unit 1} {Inout 1} {Variable 1} }
gui_list_set_filter -id ${Wave.1} -text {*}
gui_list_set_insertion_bar  -id ${Wave.1} -group Group2  -item TB_IGR_TOP.top.epl_shim_0.segs0.seg0.i_seg_sel -position below

gui_marker_move -id ${Wave.1} {C1} 268
gui_view_scroll -id ${Wave.1} -vertical -set 3310
gui_show_grid -id ${Wave.1} -enable false
# Restore toplevel window zorder
# The toplevel window could be closed if it has no view/pane
if {[gui_exist_window -window ${TopLevel.1}]} {
	gui_set_active_window -window ${TopLevel.1}
	gui_set_active_window -window ${Source.1}
}
if {[gui_exist_window -window ${TopLevel.2}]} {
	gui_set_active_window -window ${TopLevel.2}
	gui_set_active_window -window ${Wave.1}
}
#</Session>

