# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

# Begin_DVE_Session_Save_Info
# DVE full session
# Saved on Thu Jan 17 13:18:30 2019
# Toplevel windows open: 2
# 	TopLevel.1
# 	TopLevel.2
#   Source.1: top.pre_post_wrap.mby_igr_post_ppe.mem.post_ppe_rf_mems.post_ppe_wrap_mem_igr_post_body_ll_ram_shell_340x73_0
#   Wave.1: 321 signals
#   Group count = 16
#   Group pre_ppe top signal count = 30
#   Group epl0_port0_ctrl signal count = 21
#   Group epl0_port0_header signal count = 7
#   Group epl0_port0_wdata signal count = 6
#   Group epl0_wrarb signal count = 12
#   Group pre_arb signal count = 59
#   Group rx_ppe signal count = 3
#   Group post_ppe signal count = 21
#   Group post_ppe_header signal count = 18
#   Group post_ppe_merge signal count = 15
#   Group write_queue signal count = 17
#   Group free_id signal count = 29
#   Group free ptr signal count = 17
#   Group work count, header words signal count = 7
#   Group post body_list signal count = 46
#   Group tag signal count = 15
# End_DVE_Session_Save_Info

# DVE version: N-2017.12-SP2-3_Full64
# DVE build date: Sep 20 2018 21:09:19


#<Session mode="Full" path="/nfs/sc/disks/slx_1338/scottgre/mby/work_root/a0/3_mby/subBlock/mbyc/src/igr/sandbox/pre_post/session.vcdplus.vpd.tcl" type="Debug">

gui_set_loading_session_type Post
gui_continuetime_set

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
gui_show_window -window ${TopLevel.1} -show_state normal -rect {{73 50} {1241 1117}}

# ToolBar settings
gui_set_toolbar_attributes -toolbar {TimeOperations} -dock_state top
gui_set_toolbar_attributes -toolbar {TimeOperations} -offset 0
gui_show_toolbar -toolbar {TimeOperations}
gui_hide_toolbar -toolbar {&File}
gui_set_toolbar_attributes -toolbar {&Edit} -dock_state top
gui_set_toolbar_attributes -toolbar {&Edit} -offset 0
gui_show_toolbar -toolbar {&Edit}
gui_set_toolbar_attributes -toolbar {CopyPaste} -dock_state top
gui_set_toolbar_attributes -toolbar {CopyPaste} -offset 0
gui_show_toolbar -toolbar {CopyPaste}
gui_hide_toolbar -toolbar {&Trace}
gui_hide_toolbar -toolbar {TraceInstance}
gui_hide_toolbar -toolbar {BackTrace}
gui_hide_toolbar -toolbar {&Scope}
gui_hide_toolbar -toolbar {&Window}
gui_set_toolbar_attributes -toolbar {Signal} -dock_state top
gui_set_toolbar_attributes -toolbar {Signal} -offset 0
gui_show_toolbar -toolbar {Signal}
gui_set_toolbar_attributes -toolbar {Zoom} -dock_state top
gui_set_toolbar_attributes -toolbar {Zoom} -offset 0
gui_show_toolbar -toolbar {Zoom}
gui_set_toolbar_attributes -toolbar {Zoom And Pan History} -dock_state top
gui_set_toolbar_attributes -toolbar {Zoom And Pan History} -offset 0
gui_show_toolbar -toolbar {Zoom And Pan History}
gui_hide_toolbar -toolbar {Grid}
gui_hide_toolbar -toolbar {Simulator}
gui_hide_toolbar -toolbar {Interactive Rewind}
gui_hide_toolbar -toolbar {Testbench}

# End ToolBar settings

# Docked window settings
set HSPane.1 [gui_create_window -type HSPane -parent ${TopLevel.1} -dock_state left -dock_on_new_line true -dock_extent 543]
catch { set Hier.1 [gui_share_window -id ${HSPane.1} -type Hier] }
gui_hier_list_init -id ${Hier.1}
gui_set_window_pref_key -window ${HSPane.1} -key dock_width -value_type integer -value 543
gui_set_window_pref_key -window ${HSPane.1} -key dock_height -value_type integer -value 456
gui_set_window_pref_key -window ${HSPane.1} -key dock_offset -value_type integer -value 0
gui_update_layout -id ${HSPane.1} {{left 0} {top 0} {width 542} {height 455} {dock_state left} {dock_on_new_line true} {child_hier_colhier 517} {child_hier_coltype 0} {child_hier_colpd 0} {child_hier_col1 0} {child_hier_col2 1} {child_hier_col3 -1}}
set DLPane.1 [gui_create_window -type DLPane -parent ${TopLevel.1} -dock_state left -dock_on_new_line false -dock_extent 543]
catch { set Data.1 [gui_share_window -id ${DLPane.1} -type Data] }
gui_set_window_pref_key -window ${DLPane.1} -key dock_width -value_type integer -value 543
gui_set_window_pref_key -window ${DLPane.1} -key dock_height -value_type integer -value 361
gui_set_window_pref_key -window ${DLPane.1} -key dock_offset -value_type integer -value 0
gui_update_layout -id ${DLPane.1} {{left 0} {top 0} {width 542} {height 360} {dock_state left} {dock_on_new_line false} {child_data_colvariable 336} {child_data_colvalue 61} {child_data_coltype 135} {child_data_col1 0} {child_data_col2 1} {child_data_col3 2}}
set Console.1 [gui_create_window -type Console -parent ${TopLevel.1} -dock_state bottom -dock_on_new_line true -dock_extent 172]
gui_set_window_pref_key -window ${Console.1} -key dock_width -value_type integer -value -1
gui_set_window_pref_key -window ${Console.1} -key dock_height -value_type integer -value 172
gui_set_window_pref_key -window ${Console.1} -key dock_offset -value_type integer -value 0
gui_update_layout -id ${Console.1} {{left 0} {top 0} {width 247} {height 179} {dock_state bottom} {dock_on_new_line true}}
set DriverLoad.1 [gui_create_window -type DriverLoad -parent ${TopLevel.1} -dock_state bottom -dock_on_new_line false -dock_extent 180]
gui_set_window_pref_key -window ${DriverLoad.1} -key dock_width -value_type integer -value 150
gui_set_window_pref_key -window ${DriverLoad.1} -key dock_height -value_type integer -value 180
gui_set_window_pref_key -window ${DriverLoad.1} -key dock_offset -value_type integer -value 0
gui_update_layout -id ${DriverLoad.1} {{left 0} {top 0} {width 920} {height 179} {dock_state bottom} {dock_on_new_line false}}
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
gui_show_window -window ${TopLevel.2} -show_state normal -rect {{365 58} {1490 1063}}

# ToolBar settings
gui_set_toolbar_attributes -toolbar {TimeOperations} -dock_state top
gui_set_toolbar_attributes -toolbar {TimeOperations} -offset 0
gui_show_toolbar -toolbar {TimeOperations}
gui_hide_toolbar -toolbar {&File}
gui_set_toolbar_attributes -toolbar {&Edit} -dock_state top
gui_set_toolbar_attributes -toolbar {&Edit} -offset 0
gui_show_toolbar -toolbar {&Edit}
gui_set_toolbar_attributes -toolbar {CopyPaste} -dock_state top
gui_set_toolbar_attributes -toolbar {CopyPaste} -offset 0
gui_show_toolbar -toolbar {CopyPaste}
gui_hide_toolbar -toolbar {&Trace}
gui_hide_toolbar -toolbar {TraceInstance}
gui_hide_toolbar -toolbar {BackTrace}
gui_hide_toolbar -toolbar {&Scope}
gui_hide_toolbar -toolbar {&Window}
gui_set_toolbar_attributes -toolbar {Signal} -dock_state top
gui_set_toolbar_attributes -toolbar {Signal} -offset 0
gui_show_toolbar -toolbar {Signal}
gui_set_toolbar_attributes -toolbar {Zoom} -dock_state top
gui_set_toolbar_attributes -toolbar {Zoom} -offset 0
gui_show_toolbar -toolbar {Zoom}
gui_set_toolbar_attributes -toolbar {Zoom And Pan History} -dock_state top
gui_set_toolbar_attributes -toolbar {Zoom And Pan History} -offset 0
gui_show_toolbar -toolbar {Zoom And Pan History}
gui_hide_toolbar -toolbar {Grid}
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
gui_update_layout -id ${Wave.1} {{show_state maximized} {dock_state undocked} {dock_on_new_line false} {child_wave_left 521} {child_wave_right 599} {child_wave_colname 306} {child_wave_colvalue 211} {child_wave_col1 0} {child_wave_col2 1}}

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

gui_set_time_units 1ps
#</Database>

# DVE Global setting session: 


# Global: Bus

# Global: Expressions

# Global: Signal Time Shift

# Global: Signal Compare

# Global: Signal Groups
gui_load_child_values {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.header_fifo}
gui_load_child_values {top.pre_post_wrap.mby_igr_pre_ppe.id_list}
gui_load_child_values {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.wdata_fifo}
gui_load_child_values {top.pre_post_wrap.mby_igr_post_ppe}
gui_load_child_values {top.pre_post_wrap.mby_igr_post_ppe.body_list.body_ll_p0_7}
gui_load_child_values {top.pre_post_wrap.mby_igr_post_ppe.merge}
gui_load_child_values {top.pre_post_wrap.mby_igr_post_ppe.header}
gui_load_child_values {top.pre_post_wrap.mby_igr_post_ppe.tag_seq_0}
gui_load_child_values {top.pre_post_wrap.mby_igr_post_ppe.mem.post_ppe_rf_mems.post_ppe_wrap_mem_igr_post_body_ll_ram_shell_340x73_0.behave_mem}
gui_load_child_values {top.pre_post_wrap.mby_igr_pre_ppe}
gui_load_child_values {top.pre_post_wrap}
gui_load_child_values {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.header_fifo.hdr_fifo}
gui_load_child_values {top.pre_post_wrap.mby_igr_post_ppe.body_list}
gui_load_child_values {top.pre_post_wrap.mby_igr_pre_ppe.arb}
gui_load_child_values {top.pre_post_wrap.mby_igr_post_ppe.write_queue}
gui_load_child_values {top.pre_post_wrap.mby_igr_post_ppe.mem.post_ppe_rf_mems}
gui_load_child_values {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.wrarb}


set _session_group_1349 {pre_ppe top}
gui_sg_create "$_session_group_1349"
set {pre_ppe top} "$_session_group_1349"

gui_sg_addsignal -group "$_session_group_1349" { top.pre_post_wrap.mby_igr_pre_ppe.cclk top.pre_post_wrap.mby_igr_pre_ppe.rst top.pre_post_wrap.mby_igr_pre_ppe.i_shim_pb_v_p0 top.pre_post_wrap.mby_igr_pre_ppe.i_shim_pb_v_p1 top.pre_post_wrap.mby_igr_pre_ppe.i_shim_pb_v_p2 top.pre_post_wrap.mby_igr_pre_ppe.i_shim_pb_v_p3 top.pre_post_wrap.mby_igr_pre_ppe.i_shim_pb_data_p0 top.pre_post_wrap.mby_igr_pre_ppe.i_shim_pb_data_p1 top.pre_post_wrap.mby_igr_pre_ppe.i_shim_pb_data_p2 top.pre_post_wrap.mby_igr_pre_ppe.i_shim_pb_data_p3 top.pre_post_wrap.mby_igr_pre_ppe.i_shim_pb_md_p0 top.pre_post_wrap.mby_igr_pre_ppe.i_shim_pb_md_p1 top.pre_post_wrap.mby_igr_pre_ppe.i_shim_pb_md_p2 top.pre_post_wrap.mby_igr_pre_ppe.i_shim_pb_md_p3 top.pre_post_wrap.mby_igr_pre_ppe.igr_rx_ppe top.pre_post_wrap.mby_igr_pre_ppe.pdata_lpp0_fpp top.pre_post_wrap.mby_igr_pre_ppe.pdata_lpp1 top.pre_post_wrap.mby_igr_pre_ppe.sop_mdata_lpp0_fpp top.pre_post_wrap.mby_igr_pre_ppe.sop_mdata_lpp1 top.pre_post_wrap.mby_igr_pre_ppe.tag_info_epl top.pre_post_wrap.mby_igr_pre_ppe.tag_info_vp top.pre_post_wrap.mby_igr_pre_ppe.wr_data_0 top.pre_post_wrap.mby_igr_pre_ppe.wr_data_1 top.pre_post_wrap.mby_igr_pre_ppe.wr_data_2 top.pre_post_wrap.mby_igr_pre_ppe.i_free_ptr_valid top.pre_post_wrap.mby_igr_pre_ppe.o_free_ptr_req top.pre_post_wrap.mby_igr_pre_ppe.i_free_seg_ptr top.pre_post_wrap.mby_igr_pre_ppe.i_free_sema top.pre_post_wrap.mby_igr_pre_ppe.i_return_id_valid top.pre_post_wrap.mby_igr_pre_ppe.i_return_id }

set _session_group_1350 epl0_port0_ctrl
gui_sg_create "$_session_group_1350"
set epl0_port0_ctrl "$_session_group_1350"

gui_sg_addsignal -group "$_session_group_1350" { {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.cclk} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.rst} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.header_if} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.i_free_id} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.i_free_id_valid} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.i_free_ptr_valid} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.i_free_seg_ptr} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.i_free_sema} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.i_shim_pb_data} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.i_shim_pb_md} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.i_shim_pb_v} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.mux_shim_ts_md} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.mux_64B_data} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.hdr_sop_valid} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.hdr_eop_valid} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.tag_valid} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.wdata_valid} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.o_free_id_req} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.o_free_ptr_req} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.tag_info_if} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.wr_data_if} }

set _session_group_1351 epl0_port0_header
gui_sg_create "$_session_group_1351"
set epl0_port0_header "$_session_group_1351"

gui_sg_addsignal -group "$_session_group_1351" { {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.header_fifo.cclk} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.header_fifo.header_if} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.header_fifo.igr_rx_ppe} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.header_fifo.pdata_if} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.header_fifo.sop_mdata_if} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.header_fifo.hdr_fifo.DEPTH} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.header_fifo.hdr_fifo.WIDTH} }
gui_set_radix -radix {decimal} -signals {{V1:top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.header_fifo.hdr_fifo.DEPTH}}
gui_set_radix -radix {twosComplement} -signals {{V1:top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.header_fifo.hdr_fifo.DEPTH}}
gui_set_radix -radix {decimal} -signals {{V1:top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.header_fifo.hdr_fifo.WIDTH}}
gui_set_radix -radix {twosComplement} -signals {{V1:top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.header_fifo.hdr_fifo.WIDTH}}

set _session_group_1352 epl0_port0_wdata
gui_sg_create "$_session_group_1352"
set epl0_port0_wdata "$_session_group_1352"

gui_sg_addsignal -group "$_session_group_1352" { {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.wdata_fifo.cclk} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.wdata_fifo.rst} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.wdata_fifo.tag_info_in} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.wdata_fifo.tag_info_out} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.wdata_fifo.wr_data_in} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.wdata_fifo.wr_data_out} }

set _session_group_1353 epl0_wrarb
gui_sg_create "$_session_group_1353"
set epl0_wrarb "$_session_group_1353"

gui_sg_addsignal -group "$_session_group_1353" { {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.wrarb.cclk} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.wrarb.rst} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.wrarb.tag_info_if} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.wrarb.tag_info_p0} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.wrarb.tag_info_p1} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.wrarb.tag_info_p2} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.wrarb.tag_info_p3} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.wrarb.wr_data_if} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.wrarb.wr_data_p0} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.wrarb.wr_data_p1} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.wrarb.wr_data_p2} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.wrarb.wr_data_p3} }

set _session_group_1354 pre_arb
gui_sg_create "$_session_group_1354"
set pre_arb "$_session_group_1354"

gui_sg_addsignal -group "$_session_group_1354" { top.pre_post_wrap.mby_igr_pre_ppe.arb.cclk top.pre_post_wrap.mby_igr_pre_ppe.arb.rst top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_req top.pre_post_wrap.mby_igr_pre_ppe.arb.pdata_lpp0_fpp top.pre_post_wrap.mby_igr_pre_ppe.arb.pdata_lpp1 top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_lpp0_fpp top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_lpp1 }
gui_sg_addsignal -group "$_session_group_1354" { Divider } -divider
gui_sg_addsignal -group "$_session_group_1354" { top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_p0 top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_p1 top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_p2 top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_p3 top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_p4 top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_p5 top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_p6 top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_p7 top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_p8 top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_p9 top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_p10 top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_p11 top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_p12 top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_p13 top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_p14 top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_p15 top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_vp top.pre_post_wrap.mby_igr_pre_ppe.arb.port_pdata_p0 top.pre_post_wrap.mby_igr_pre_ppe.arb.port_pdata_p1 top.pre_post_wrap.mby_igr_pre_ppe.arb.port_pdata_p2 top.pre_post_wrap.mby_igr_pre_ppe.arb.port_pdata_p3 top.pre_post_wrap.mby_igr_pre_ppe.arb.port_pdata_p4 top.pre_post_wrap.mby_igr_pre_ppe.arb.port_pdata_p5 top.pre_post_wrap.mby_igr_pre_ppe.arb.port_pdata_p6 top.pre_post_wrap.mby_igr_pre_ppe.arb.port_pdata_p7 top.pre_post_wrap.mby_igr_pre_ppe.arb.port_pdata_p8 top.pre_post_wrap.mby_igr_pre_ppe.arb.port_pdata_p9 top.pre_post_wrap.mby_igr_pre_ppe.arb.port_pdata_p10 top.pre_post_wrap.mby_igr_pre_ppe.arb.port_pdata_p11 top.pre_post_wrap.mby_igr_pre_ppe.arb.port_pdata_p12 top.pre_post_wrap.mby_igr_pre_ppe.arb.port_pdata_p13 top.pre_post_wrap.mby_igr_pre_ppe.arb.port_pdata_p14 top.pre_post_wrap.mby_igr_pre_ppe.arb.port_pdata_p15 top.pre_post_wrap.mby_igr_pre_ppe.arb.port_pdata_vp top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_p0 top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_p1 top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_p2 top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_p3 top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_p4 top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_p5 top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_p6 top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_p7 top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_p8 top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_p9 top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_p10 top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_p11 top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_p12 top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_p13 top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_p14 top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_p15 top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_vp }

set _session_group_1355 rx_ppe
gui_sg_create "$_session_group_1355"
set rx_ppe "$_session_group_1355"

gui_sg_addsignal -group "$_session_group_1355" { top.rx_ppe.igr_rx_ppe_intf0_head top.rx_ppe.intf0_queue top.rx_ppe.rx_ppe_igr_intf0 }

set _session_group_1356 post_ppe
gui_sg_create "$_session_group_1356"
set post_ppe "$_session_group_1356"

gui_sg_addsignal -group "$_session_group_1356" { top.pre_post_wrap.mby_igr_post_ppe.cclk top.pre_post_wrap.mby_igr_post_ppe.egr_igr_wreq top.pre_post_wrap.mby_igr_post_ppe.mim_wreq_0 top.pre_post_wrap.mby_igr_post_ppe.mim_wreq_1 top.pre_post_wrap.mby_igr_post_ppe.mim_wreq_2 top.pre_post_wrap.mby_igr_post_ppe.mim_wreq_3 top.pre_post_wrap.mby_igr_post_ppe.mim_wreq_4 top.pre_post_wrap.mby_igr_post_ppe.mim_wreq_5 top.pre_post_wrap.mby_igr_post_ppe.pdata_lpp0_fpp top.pre_post_wrap.mby_igr_post_ppe.pdata_lpp1 top.pre_post_wrap.mby_igr_post_ppe.rst top.pre_post_wrap.mby_igr_post_ppe.rx_ppe_igr top.pre_post_wrap.mby_igr_post_ppe.sop_mdata_lpp0_fpp top.pre_post_wrap.mby_igr_post_ppe.sop_mdata_lpp1 top.pre_post_wrap.mby_igr_post_ppe.tag_info_epl top.pre_post_wrap.mby_igr_post_ppe.tag_info_vp top.pre_post_wrap.mby_igr_post_ppe.wr_data_0 top.pre_post_wrap.mby_igr_post_ppe.wr_data_1 top.pre_post_wrap.mby_igr_post_ppe.wr_data_2 top.pre_post_wrap.mby_igr_post_ppe.igr_tag_ring_lltag0 top.pre_post_wrap.mby_igr_post_ppe.igr_tag_ring_lltag1 }

set _session_group_1357 post_ppe_header
gui_sg_create "$_session_group_1357"
set post_ppe_header "$_session_group_1357"

gui_sg_addsignal -group "$_session_group_1357" { top.pre_post_wrap.mby_igr_post_ppe.header.cclk top.pre_post_wrap.mby_igr_post_ppe.header.rst top.pre_post_wrap.mby_igr_post_ppe.header.rx_ppe_igr top.pre_post_wrap.mby_igr_post_ppe.header.tag_info top.pre_post_wrap.mby_igr_post_ppe.header.sop_mdata_lpp0_fpp top.pre_post_wrap.mby_igr_post_ppe.header.sop_mdata_lpp1 top.pre_post_wrap.mby_igr_post_ppe.header.o_merge_md top.pre_post_wrap.mby_igr_post_ppe.header.o_body_head_ptr top.pre_post_wrap.mby_igr_post_ppe.header.o_merge_xmd top.pre_post_wrap.mby_igr_post_ppe.header.header_data {top.pre_post_wrap.mby_igr_post_ppe.header.next_header_data[31]} top.pre_post_wrap.mby_igr_post_ppe.header.next_header_data {top.pre_post_wrap.mby_igr_post_ppe.header.header_data[5]} {top.pre_post_wrap.mby_igr_post_ppe.header.header_data[4]} {top.pre_post_wrap.mby_igr_post_ppe.header.header_data[3]} {top.pre_post_wrap.mby_igr_post_ppe.header.header_data[2]} {top.pre_post_wrap.mby_igr_post_ppe.header.header_data[1]} {top.pre_post_wrap.mby_igr_post_ppe.header.header_data[0]} }

set _session_group_1358 post_ppe_merge
gui_sg_create "$_session_group_1358"
set post_ppe_merge "$_session_group_1358"

gui_sg_addsignal -group "$_session_group_1358" { top.pre_post_wrap.mby_igr_post_ppe.merge.cclk top.pre_post_wrap.mby_igr_post_ppe.merge.rst top.pre_post_wrap.mby_igr_post_ppe.merge.i_merge_valid top.pre_post_wrap.mby_igr_post_ppe.merge.i_merge_xmd top.pre_post_wrap.mby_igr_post_ppe.merge.i_merge_sema top.pre_post_wrap.mby_igr_post_ppe.merge.i_merge_seg_ptr top.pre_post_wrap.mby_igr_post_ppe.merge.i_merge_wd_sel top.pre_post_wrap.mby_igr_post_ppe.merge.i_merge_md top.pre_post_wrap.mby_igr_post_ppe.merge.md_0_data_ecc top.pre_post_wrap.mby_igr_post_ppe.merge.wr_partial_1 top.pre_post_wrap.mby_igr_post_ppe.merge.wr_partial_0 top.pre_post_wrap.mby_igr_post_ppe.merge.wr_md_1 top.pre_post_wrap.mby_igr_post_ppe.merge.wr_md_0 top.pre_post_wrap.mby_igr_post_ppe.merge.pdata_lpp1 top.pre_post_wrap.mby_igr_post_ppe.merge.pdata_lpp0_fpp }

set _session_group_1359 write_queue
gui_sg_create "$_session_group_1359"
set write_queue "$_session_group_1359"

gui_sg_addsignal -group "$_session_group_1359" { top.pre_post_wrap.mby_igr_post_ppe.write_queue.cclk top.pre_post_wrap.mby_igr_post_ppe.write_queue.rst top.pre_post_wrap.mby_igr_post_ppe.write_queue.wr_partial_1 top.pre_post_wrap.mby_igr_post_ppe.write_queue.wr_partial_0 top.pre_post_wrap.mby_igr_post_ppe.write_queue.wr_md_1 top.pre_post_wrap.mby_igr_post_ppe.write_queue.wr_md_0 top.pre_post_wrap.mby_igr_post_ppe.write_queue.wr_data_2 top.pre_post_wrap.mby_igr_post_ppe.write_queue.wr_data_1 top.pre_post_wrap.mby_igr_post_ppe.write_queue.wr_data_0 top.pre_post_wrap.mby_igr_post_ppe.write_queue.mim_wreq_5 top.pre_post_wrap.mby_igr_post_ppe.write_queue.mim_wreq_4 top.pre_post_wrap.mby_igr_post_ppe.write_queue.mim_wreq_3 top.pre_post_wrap.mby_igr_post_ppe.write_queue.mim_wreq_2 top.pre_post_wrap.mby_igr_post_ppe.write_queue.mim_wreq_1 top.pre_post_wrap.mby_igr_post_ppe.write_queue.mim_wreq_0 top.pre_post_wrap.mby_igr_post_ppe.write_queue.egr_igr_wreq top.pre_post_wrap.mby_igr_post_ppe.write_queue.next_mim_wr_data }

set _session_group_1360 free_id
gui_sg_create "$_session_group_1360"
set free_id "$_session_group_1360"

gui_sg_addsignal -group "$_session_group_1360" { top.pre_post_wrap.mby_igr_pre_ppe.id_list.cclk top.pre_post_wrap.mby_igr_pre_ppe.id_list.rst top.pre_post_wrap.mby_igr_pre_ppe.id_list.i_free_id_req top.pre_post_wrap.mby_igr_pre_ppe.id_list.o_free_id_valid top.pre_post_wrap.mby_igr_pre_ppe.id_list.o_free_id top.pre_post_wrap.mby_igr_pre_ppe.id_list.free_list top.pre_post_wrap.mby_igr_pre_ppe.id_list.full top.pre_post_wrap.mby_igr_pre_ppe.id_list.empty top.pre_post_wrap.mby_igr_pre_ppe.id_list.aempty top.pre_post_wrap.mby_igr_pre_ppe.id_list.read_ptr top.pre_post_wrap.mby_igr_pre_ppe.id_list.wen_QA top.pre_post_wrap.mby_igr_pre_ppe.id_list.ren_QA top.pre_post_wrap.mby_igr_pre_ppe.id_list.write_ptr top.pre_post_wrap.mby_igr_pre_ppe.id_list.empty_space top.pre_post_wrap.mby_igr_pre_ppe.id_list.FREE_LIST_WIDTH top.pre_post_wrap.mby_igr_pre_ppe.id_list.FREE_LIST_DEPTH top.pre_post_wrap.mby_igr_pre_ppe.id_list.FREE_LIST_ADDR_BITS top.pre_post_wrap.mby_igr_pre_ppe.id_list.MGP_PKT_ID_CNT }
gui_sg_addsignal -group "$_session_group_1360" { Divider } -divider
gui_sg_addsignal -group "$_session_group_1360" { {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.o_free_id_req} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.i_free_id_valid} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.i_free_id} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.next_free_id_req} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.curr_free_id} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.hdr_sop_valid} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.hdr_eop_valid} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.tag_valid} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.wdata_valid} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.curr_free_id_valid} }
gui_set_radix -radix {decimal} -signals {V1:top.pre_post_wrap.mby_igr_pre_ppe.id_list.FREE_LIST_WIDTH}
gui_set_radix -radix {twosComplement} -signals {V1:top.pre_post_wrap.mby_igr_pre_ppe.id_list.FREE_LIST_WIDTH}
gui_set_radix -radix {decimal} -signals {V1:top.pre_post_wrap.mby_igr_pre_ppe.id_list.FREE_LIST_DEPTH}
gui_set_radix -radix {twosComplement} -signals {V1:top.pre_post_wrap.mby_igr_pre_ppe.id_list.FREE_LIST_DEPTH}
gui_set_radix -radix {decimal} -signals {V1:top.pre_post_wrap.mby_igr_pre_ppe.id_list.FREE_LIST_ADDR_BITS}
gui_set_radix -radix {twosComplement} -signals {V1:top.pre_post_wrap.mby_igr_pre_ppe.id_list.FREE_LIST_ADDR_BITS}
gui_set_radix -radix {decimal} -signals {V1:top.pre_post_wrap.mby_igr_pre_ppe.id_list.MGP_PKT_ID_CNT}
gui_set_radix -radix {twosComplement} -signals {V1:top.pre_post_wrap.mby_igr_pre_ppe.id_list.MGP_PKT_ID_CNT}

set _session_group_1361 {free ptr}
gui_sg_create "$_session_group_1361"
set {free ptr} "$_session_group_1361"

gui_sg_addsignal -group "$_session_group_1361" { top.pre_post_wrap.rst top.pre_post_wrap.i_free_ptr_valid top.pre_post_wrap.i_free_seg_ptr top.pre_post_wrap.o_free_ptr_req {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.i_free_ptr_valid} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.o_free_ptr_req} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.i_free_seg_ptr} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.i_free_sema} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.next_free_ptr_req} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.curr_free_ptr} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.free_seg_ptr_list} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.free_sema_list} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.free_seg_valid} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.free_seg_requested} top.rx_ppe.rx_ppe_igr_intf1.md.md.uc_meta.unicast_meta.nh_egr_mirror_idx top.pre_post_wrap.i_free_ptr_valid top.pre_post_wrap.o_free_ptr_req }

set _session_group_1362 {work count, header words}
gui_sg_create "$_session_group_1362"
set {work count, header words} "$_session_group_1362"

gui_sg_addsignal -group "$_session_group_1362" { {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.cclk} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.i_shim_pb_v} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.word_count} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.word_count_plus_1} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.wdata_wd_sel} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.curr_word_in} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.curr_hdr_word} }

set _session_group_1363 {post body_list}
gui_sg_create "$_session_group_1363"
set {post body_list} "$_session_group_1363"

gui_sg_addsignal -group "$_session_group_1363" { top.pre_post_wrap.mby_igr_post_ppe.body_list.tag_info top.pre_post_wrap.mby_igr_post_ppe.body_list.igr_post_body_ll_if top.pre_post_wrap.mby_igr_post_ppe.mem.post_ppe_rf_mems.post_ppe_igr_post_body_ll_ram_0_from_mem top.pre_post_wrap.mby_igr_post_ppe.mem.post_ppe_rf_mems.post_ppe_igr_post_body_ll_ram_0_to_mem top.pre_post_wrap.mby_igr_post_ppe.mem.post_ppe_rf_mems.post_ppe_wrap_mem_igr_post_body_ll_ram_shell_340x73_0.wrap_shell_from_mem top.pre_post_wrap.mby_igr_post_ppe.body_list.cclk top.pre_post_wrap.mby_igr_post_ppe.body_list.body_ll_p0_7.read_ptr top.pre_post_wrap.mby_igr_post_ppe.body_list.body_ll_p0_7.write_ptr top.pre_post_wrap.mby_igr_post_ppe.body_list.body_ll_p0_7.empty_space top.pre_post_wrap.mby_igr_post_ppe.body_list.body_ll_p0_7.full top.pre_post_wrap.mby_igr_post_ppe.body_list.body_ll_p0_7.empty top.pre_post_wrap.mby_igr_post_ppe.body_list.body_ll_p0_7.aempty top.pre_post_wrap.mby_igr_post_ppe.body_list.body_ll_p0_7.wen_QA top.pre_post_wrap.mby_igr_post_ppe.body_list.body_ll_p0_7.ren_QA top.pre_post_wrap.mby_igr_post_ppe.body_list.body_ll_p0_7.wr_err top.pre_post_wrap.mby_igr_post_ppe.body_list.body_ll_p0_7.rd_err top.pre_post_wrap.mby_igr_post_ppe.body_list.body_ll_p0_7.read_req top.pre_post_wrap.mby_igr_post_ppe.body_list.body_ll_p0_7.i_read_start top.pre_post_wrap.mby_igr_post_ppe.body_list.body_ll_p0_7.i_read_next top.pre_post_wrap.mby_igr_post_ppe.body_list.read_data top.pre_post_wrap.mby_igr_post_ppe.body_list.write_data top.pre_post_wrap.mby_igr_post_ppe.body_list.i_body_ll_read_start top.pre_post_wrap.mby_igr_post_ppe.body_list.i_body_ll_read_start_port top.pre_post_wrap.mby_igr_post_ppe.body_list.i_body_ll_read_next top.pre_post_wrap.mby_igr_post_ppe.body_list.i_body_ll_read_next_port top.pre_post_wrap.mby_igr_post_ppe.body_list.o_body_ll_tag_valid top.pre_post_wrap.mby_igr_post_ppe.body_list.o_body_ll_tag_port top.pre_post_wrap.mby_igr_post_ppe.body_list.o_body_ll_tag top.pre_post_wrap.mby_igr_post_ppe.body_list.cclk top.pre_post_wrap.mby_igr_post_ppe.mem.post_ppe_rf_mems.post_ppe_wrap_mem_igr_post_body_ll_ram_shell_340x73_0.behave_mem.rd_add top.pre_post_wrap.mby_igr_post_ppe.mem.post_ppe_rf_mems.post_ppe_wrap_mem_igr_post_body_ll_ram_shell_340x73_0.behave_mem.wr_add top.pre_post_wrap.mby_igr_post_ppe.mem.post_ppe_rf_mems.post_ppe_wrap_mem_igr_post_body_ll_ram_shell_340x73_0.behave_mem.rd_en top.pre_post_wrap.mby_igr_post_ppe.mem.post_ppe_rf_mems.post_ppe_wrap_mem_igr_post_body_ll_ram_shell_340x73_0.behave_mem.wr_en top.pre_post_wrap.mby_igr_post_ppe.mem.post_ppe_rf_mems.post_ppe_wrap_mem_igr_post_body_ll_ram_shell_340x73_0.behave_mem.data_in top.pre_post_wrap.mby_igr_post_ppe.mem.post_ppe_rf_mems.post_ppe_wrap_mem_igr_post_body_ll_ram_shell_340x73_0.behave_mem.data_out top.pre_post_wrap.mby_igr_post_ppe.mem.post_ppe_rf_mems.post_ppe_igr_post_body_ll_ram_0_to_mem top.pre_post_wrap.mby_igr_post_ppe.mem.post_ppe_rf_mems.post_ppe_wrap_mem_igr_post_body_ll_ram_shell_340x73_0.wrap_shell_from_mem_int top.pre_post_wrap.mby_igr_post_ppe.mem.post_ppe_rf_mems.post_ppe_wrap_mem_igr_post_body_ll_ram_shell_340x73_0.wrap_shell_to_mem_int top.pre_post_wrap.mby_igr_post_ppe.mem.post_ppe_shells_wrapper.mby_mem_igr_post_body_ll_ram_shell_340x73_0.u_master_shell.wr_data top.pre_post_wrap.mby_igr_post_ppe.mem.post_ppe_shells_wrapper.mby_mem_igr_post_body_ll_ram_shell_340x73_0.u_master_shell.rd_data top.pre_post_wrap.mby_igr_post_ppe.mem.post_ppe_shells_wrapper.mby_mem_igr_post_body_ll_ram_shell_340x73_0.u_master_shell.wrap_shell_to_mem top.pre_post_wrap.mby_igr_post_ppe.mem.post_ppe_shells_wrapper.mby_mem_igr_post_body_ll_ram_shell_340x73_0.u_master_shell.ctl_shell_to_mem top.pre_post_wrap.mby_igr_post_ppe.mem.post_ppe_shells_wrapper.mby_mem_igr_post_body_ll_ram_shell_340x73_0.u_master_shell.wr_en_to_mem_int top.pre_post_wrap.mby_igr_post_ppe.mem.post_ppe_shells_wrapper.mby_mem_igr_post_body_ll_ram_shell_340x73_0.u_master_shell.wr_data_to_mem_int }
gui_sg_addsignal -group "$_session_group_1363" { top.pre_post_wrap.mby_igr_post_ppe.mem.post_ppe_shells_wrapper.mby_mem_igr_post_body_ll_ram_shell_340x73_0.u_master_shell.wr_en_to_mem top.pre_post_wrap.mby_igr_post_ppe.mem.post_ppe_shells_wrapper.mby_mem_igr_post_body_ll_ram_shell_340x73_0.u_master_shell.to_mem_bus }

set _session_group_1364 tag
gui_sg_create "$_session_group_1364"
set tag "$_session_group_1364"

gui_sg_addsignal -group "$_session_group_1364" { top.pre_post_wrap.mby_igr_post_ppe.tag_seq_0.i_tag_from_rx_ppe top.pre_post_wrap.mby_igr_post_ppe.tag_seq_0.curr_tag_seq_st top.pre_post_wrap.mby_igr_post_ppe.tag_seq_0.i_body_head_ptr top.pre_post_wrap.mby_igr_post_ppe.tag_seq_0.o_body_ll_read_start top.pre_post_wrap.mby_igr_post_ppe.tag_seq_0.i_body_ll_tag_valid top.pre_post_wrap.mby_igr_post_ppe.tag_seq_0.o_body_ll_read_start_ptr top.pre_post_wrap.mby_igr_post_ppe.tag_seq_0.o_body_ll_read_start_port top.pre_post_wrap.mby_igr_post_ppe.tag_seq_0.o_body_ll_read_next top.pre_post_wrap.mby_igr_post_ppe.tag_seq_0.o_body_ll_read_next_port top.pre_post_wrap.mby_igr_post_ppe.tag_seq_0.i_body_ll_tag_port top.pre_post_wrap.mby_igr_post_ppe.tag_seq_0.i_body_ll_tag top.pre_post_wrap.mby_igr_post_ppe.tag_seq_0.mem_sop_fifo_0_if top.pre_post_wrap.mby_igr_post_ppe.tag_seq_0.mem_sop_fifo_1_if top.pre_post_wrap.mby_igr_post_ppe.tag_seq_0.o_post_ppe_tag_at_rate top.pre_post_wrap.mby_igr_post_ppe.tag_seq_0.o_post_ppe_tag_set_aside }

# Global: Highlighting
gui_highlight_signals -color #a020f0 {{top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.o_free_ptr_req}}

# Global: Stack
gui_change_stack_mode -mode list

# Post database loading setting...

# Restore C1 time
gui_set_time -C1_only 7305



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
gui_list_set_filter -id ${Hier.1} -list { {Package 1} {All 0} {Process 1} {VirtPowSwitch 0} {UnnamedProcess 1} {UDP 0} {Function 1} {Block 1} {SrsnAndSpaCell 0} {OVA Unit 1} {LeafScCell 1} {LeafVlgCell 1} {Interface 0} {LeafVhdCell 1} {$unit 1} {NamedBlock 0} {Task 1} {VlgPackage 1} {ClassDef 1} {VirtIsoCell 0} }
gui_list_set_filter -id ${Hier.1} -text {*}
gui_hier_list_init -id ${Hier.1}
gui_change_design -id ${Hier.1} -design V1
catch {gui_list_expand -id ${Hier.1} top}
catch {gui_list_expand -id ${Hier.1} top.pre_post_wrap}
catch {gui_list_expand -id ${Hier.1} top.pre_post_wrap.mby_igr_post_ppe}
catch {gui_list_expand -id ${Hier.1} top.pre_post_wrap.mby_igr_post_ppe.mem}
catch {gui_list_expand -id ${Hier.1} top.pre_post_wrap.mby_igr_post_ppe.mem.post_ppe_rf_mems}
catch {gui_list_select -id ${Hier.1} {top.pre_post_wrap.mby_igr_post_ppe.mem.post_ppe_rf_mems.post_ppe_wrap_mem_igr_post_body_ll_ram_shell_340x73_0}}
gui_view_scroll -id ${Hier.1} -vertical -set 0
gui_view_scroll -id ${Hier.1} -horizontal -set 0

# Data 'Data.1'
gui_list_set_filter -id ${Data.1} -list { {Buffer 1} {Input 1} {Others 1} {Linkage 1} {Output 1} {LowPower 1} {Parameter 1} {All 1} {Aggregate 1} {LibBaseMember 1} {Event 1} {Assertion 1} {Constant 1} {Interface 1} {BaseMembers 1} {Signal 1} {$unit 1} {Inout 1} {Variable 1} }
gui_list_set_filter -id ${Data.1} -text {MEM*}
gui_list_show_data -id ${Data.1} {top.pre_post_wrap.mby_igr_post_ppe.mem.post_ppe_rf_mems.post_ppe_wrap_mem_igr_post_body_ll_ram_shell_340x73_0}
gui_view_scroll -id ${Data.1} -vertical -set 0
gui_view_scroll -id ${Data.1} -horizontal -set 0
gui_view_scroll -id ${Hier.1} -vertical -set 0
gui_view_scroll -id ${Hier.1} -horizontal -set 0

# Source 'Source.1'
gui_src_value_annotate -id ${Source.1} -switch false
gui_set_env TOGGLE::VALUEANNOTATE 0
gui_open_source -id ${Source.1}  -replace -active top.pre_post_wrap.mby_igr_post_ppe.mem.post_ppe_rf_mems.post_ppe_wrap_mem_igr_post_body_ll_ram_shell_340x73_0
gui_view_scroll -id ${Source.1} -vertical -set 720
gui_src_set_reusable -id ${Source.1}

# View 'Wave.1'
gui_wv_sync -id ${Wave.1} -switch false
set groupExD [gui_get_pref_value -category Wave -key exclusiveSG]
gui_set_pref_value -category Wave -key exclusiveSG -value {false}
set origWaveHeight [gui_get_pref_value -category Wave -key waveRowHeight]
gui_list_set_height -id Wave -height 25
set origGroupCreationState [gui_list_create_group_when_add -wave]
gui_list_create_group_when_add -wave -disable
gui_marker_create -id ${Wave.1} C2 9786
gui_marker_set_ref -id ${Wave.1}  C1
gui_wv_zoom_timerange -id ${Wave.1} 10899 12611
gui_list_add_group -id ${Wave.1} -after {New Group} {{pre_ppe top}}
gui_list_add_group -id ${Wave.1} -after {New Group} {epl0_port0_ctrl}
gui_list_add_group -id ${Wave.1} -after {New Group} {epl0_port0_header}
gui_list_add_group -id ${Wave.1} -after {New Group} {epl0_port0_wdata}
gui_list_add_group -id ${Wave.1} -after {New Group} {epl0_wrarb}
gui_list_add_group -id ${Wave.1} -after {New Group} {pre_arb}
gui_list_add_group -id ${Wave.1} -after {New Group} {rx_ppe}
gui_list_add_group -id ${Wave.1} -after {New Group} {post_ppe}
gui_list_add_group -id ${Wave.1} -after {New Group} {post_ppe_header}
gui_list_add_group -id ${Wave.1} -after {New Group} {post_ppe_merge}
gui_list_add_group -id ${Wave.1} -after {New Group} {write_queue}
gui_list_add_group -id ${Wave.1} -after {New Group} {free_id}
gui_list_add_group -id ${Wave.1} -after {New Group} {{free ptr}}
gui_list_add_group -id ${Wave.1} -after {New Group} {{work count, header words}}
gui_list_add_group -id ${Wave.1} -after {New Group} {{post body_list}}
gui_list_add_group -id ${Wave.1} -after {New Group} {tag}
gui_list_collapse -id ${Wave.1} {pre_ppe top}
gui_list_collapse -id ${Wave.1} epl0_port0_ctrl
gui_list_collapse -id ${Wave.1} epl0_port0_header
gui_list_collapse -id ${Wave.1} epl0_port0_wdata
gui_list_collapse -id ${Wave.1} epl0_wrarb
gui_list_collapse -id ${Wave.1} pre_arb
gui_list_collapse -id ${Wave.1} rx_ppe
gui_list_collapse -id ${Wave.1} post_ppe
gui_list_collapse -id ${Wave.1} post_ppe_header
gui_list_collapse -id ${Wave.1} post_ppe_merge
gui_list_collapse -id ${Wave.1} write_queue
gui_list_collapse -id ${Wave.1} free_id
gui_list_collapse -id ${Wave.1} {free ptr}
gui_list_collapse -id ${Wave.1} {work count, header words}
gui_list_collapse -id ${Wave.1} {post body_list}
gui_list_expand -id ${Wave.1} top.pre_post_wrap.mby_igr_post_ppe.tag_seq_0.o_post_ppe_tag_at_rate
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
gui_list_set_insertion_bar  -id ${Wave.1} -group {post body_list}  -position in

gui_marker_move -id ${Wave.1} {C1} 7305
gui_view_scroll -id ${Wave.1} -vertical -set 93
gui_show_grid -id ${Wave.1} -enable false

# DriverLoad 'DriverLoad.1'
gui_get_drivers -session -id ${DriverLoad.1} -signal {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.o_free_ptr_req} -time 7275 -starttime 7285
gui_get_drivers -session -id ${DriverLoad.1} -signal {top.pre_post_wrap.mby_igr_post_ppe.mem.post_ppe_rf_mems.post_ppe_wrap_mem_igr_post_body_ll_ram_shell_340x73_0.wrap_shell_from_mem[73:0]} -time 0 -starttime 8935
# Restore toplevel window zorder
# The toplevel window could be closed if it has no view/pane
if {[gui_exist_window -window ${TopLevel.2}]} {
	gui_set_active_window -window ${TopLevel.2}
	gui_set_active_window -window ${Wave.1}
}
if {[gui_exist_window -window ${TopLevel.1}]} {
	gui_set_active_window -window ${TopLevel.1}
	gui_set_active_window -window ${Source.1}
	gui_set_active_window -window ${HSPane.1}
}
#</Session>

