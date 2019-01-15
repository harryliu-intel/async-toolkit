# Begin_DVE_Session_Save_Info
# DVE full session
# Saved on Tue Jan 15 09:20:11 2019
# Toplevel windows open: 2
# 	TopLevel.1
# 	TopLevel.2
#   Source.1: top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl
#   Wave.1: 228 signals
#   Group count = 13
#   Group pre_ppe top signal count = 30
#   Group epl0_port0_ctrl signal count = 15
#   Group epl0_port0_header signal count = 7
#   Group epl0_port0_wdata signal count = 6
#   Group epl0_wrarb signal count = 12
#   Group pre_arb signal count = 59
#   Group rx_ppe signal count = 3
#   Group post_ppe signal count = 21
#   Group post_ppe_header signal count = 12
#   Group post_ppe_merge signal count = 14
#   Group write_queue signal count = 17
#   Group free_id signal count = 28
#   Group Group2 signal count = 5
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
gui_show_window -window ${TopLevel.1} -show_state normal -rect {{136 274} {1479 1150}}

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
set HSPane.1 [gui_create_window -type HSPane -parent ${TopLevel.1} -dock_state left -dock_on_new_line true -dock_extent 346]
catch { set Hier.1 [gui_share_window -id ${HSPane.1} -type Hier] }
gui_hier_list_init -id ${Hier.1}
gui_set_window_pref_key -window ${HSPane.1} -key dock_width -value_type integer -value 346
gui_set_window_pref_key -window ${HSPane.1} -key dock_height -value_type integer -value 338
gui_set_window_pref_key -window ${HSPane.1} -key dock_offset -value_type integer -value 0
gui_update_layout -id ${HSPane.1} {{left 0} {top 0} {width 345} {height 337} {dock_state left} {dock_on_new_line true} {child_hier_colhier 320} {child_hier_coltype 0} {child_hier_colpd 0} {child_hier_col1 0} {child_hier_col2 1} {child_hier_col3 -1}}
set DLPane.1 [gui_create_window -type DLPane -parent ${TopLevel.1} -dock_state left -dock_on_new_line false -dock_extent 346]
catch { set Data.1 [gui_share_window -id ${DLPane.1} -type Data] }
gui_set_window_pref_key -window ${DLPane.1} -key dock_width -value_type integer -value 346
gui_set_window_pref_key -window ${DLPane.1} -key dock_height -value_type integer -value 296
gui_set_window_pref_key -window ${DLPane.1} -key dock_offset -value_type integer -value 0
gui_update_layout -id ${DLPane.1} {{left 0} {top 0} {width 345} {height 295} {dock_state left} {dock_on_new_line false} {child_data_colvariable 201} {child_data_colvalue 35} {child_data_coltype 101} {child_data_col1 0} {child_data_col2 1} {child_data_col3 2}}
set Console.1 [gui_create_window -type Console -parent ${TopLevel.1} -dock_state bottom -dock_on_new_line true -dock_extent 172]
gui_set_window_pref_key -window ${Console.1} -key dock_width -value_type integer -value 1077
gui_set_window_pref_key -window ${Console.1} -key dock_height -value_type integer -value 172
gui_set_window_pref_key -window ${Console.1} -key dock_offset -value_type integer -value 0
gui_update_layout -id ${Console.1} {{left 0} {top 0} {width 1343} {height 171} {dock_state bottom} {dock_on_new_line true}}
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
gui_show_window -window ${TopLevel.2} -show_state normal -rect {{584 66} {1711 1072}}

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
gui_update_layout -id ${Wave.1} {{show_state maximized} {dock_state undocked} {dock_on_new_line false} {child_wave_left 473} {child_wave_right 649} {child_wave_colname 269} {child_wave_colvalue 199} {child_wave_col1 0} {child_wave_col2 1}}

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
gui_load_child_values {top.rx_ppe}
gui_load_child_values {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.wdata_fifo}
gui_load_child_values {top.pre_post_wrap.mby_igr_post_ppe}
gui_load_child_values {top.pre_post_wrap.mby_igr_post_ppe.merge}
gui_load_child_values {top.pre_post_wrap.mby_igr_post_ppe.header}
gui_load_child_values {top.pre_post_wrap.mby_igr_pre_ppe}
gui_load_child_values {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.header_fifo.hdr_fifo}
gui_load_child_values {top.pre_post_wrap.mby_igr_pre_ppe.arb}
gui_load_child_values {top.pre_post_wrap.mby_igr_post_ppe.write_queue}


set _session_group_442 {pre_ppe top}
gui_sg_create "$_session_group_442"
set {pre_ppe top} "$_session_group_442"

gui_sg_addsignal -group "$_session_group_442" { top.pre_post_wrap.mby_igr_pre_ppe.cclk top.pre_post_wrap.mby_igr_pre_ppe.rst top.pre_post_wrap.mby_igr_pre_ppe.i_shim_pb_v_p0 top.pre_post_wrap.mby_igr_pre_ppe.i_shim_pb_v_p1 top.pre_post_wrap.mby_igr_pre_ppe.i_shim_pb_v_p2 top.pre_post_wrap.mby_igr_pre_ppe.i_shim_pb_v_p3 top.pre_post_wrap.mby_igr_pre_ppe.i_shim_pb_data_p0 top.pre_post_wrap.mby_igr_pre_ppe.i_shim_pb_data_p1 top.pre_post_wrap.mby_igr_pre_ppe.i_shim_pb_data_p2 top.pre_post_wrap.mby_igr_pre_ppe.i_shim_pb_data_p3 top.pre_post_wrap.mby_igr_pre_ppe.i_shim_pb_md_p0 top.pre_post_wrap.mby_igr_pre_ppe.i_shim_pb_md_p1 top.pre_post_wrap.mby_igr_pre_ppe.i_shim_pb_md_p2 top.pre_post_wrap.mby_igr_pre_ppe.i_shim_pb_md_p3 top.pre_post_wrap.mby_igr_pre_ppe.igr_rx_ppe top.pre_post_wrap.mby_igr_pre_ppe.pdata_lpp0_fpp top.pre_post_wrap.mby_igr_pre_ppe.pdata_lpp1 top.pre_post_wrap.mby_igr_pre_ppe.sop_mdata_lpp0_fpp top.pre_post_wrap.mby_igr_pre_ppe.sop_mdata_lpp1 top.pre_post_wrap.mby_igr_pre_ppe.tag_info_epl top.pre_post_wrap.mby_igr_pre_ppe.tag_info_vp top.pre_post_wrap.mby_igr_pre_ppe.wr_data_0 top.pre_post_wrap.mby_igr_pre_ppe.wr_data_1 top.pre_post_wrap.mby_igr_pre_ppe.wr_data_2 top.pre_post_wrap.mby_igr_pre_ppe.i_free_ptr_valid top.pre_post_wrap.mby_igr_pre_ppe.o_free_ptr_req top.pre_post_wrap.mby_igr_pre_ppe.i_free_seg_ptr top.pre_post_wrap.mby_igr_pre_ppe.i_free_sema top.pre_post_wrap.mby_igr_pre_ppe.i_return_id_valid top.pre_post_wrap.mby_igr_pre_ppe.i_return_id }

set _session_group_443 epl0_port0_ctrl
gui_sg_create "$_session_group_443"
set epl0_port0_ctrl "$_session_group_443"

gui_sg_addsignal -group "$_session_group_443" { {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.cclk} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.header_if} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.i_free_id} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.i_free_id_valid} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.i_free_ptr_valid} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.i_free_seg_ptr} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.i_free_sema} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.i_shim_pb_data} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.i_shim_pb_md} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.i_shim_pb_v} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.o_free_id_req} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.o_free_ptr_req} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.rst} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.tag_info_if} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.wr_data_if} }

set _session_group_444 epl0_port0_header
gui_sg_create "$_session_group_444"
set epl0_port0_header "$_session_group_444"

gui_sg_addsignal -group "$_session_group_444" { {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.header_fifo.cclk} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.header_fifo.header_if} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.header_fifo.igr_rx_ppe} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.header_fifo.pdata_if} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.header_fifo.sop_mdata_if} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.header_fifo.hdr_fifo.DEPTH} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.header_fifo.hdr_fifo.WIDTH} }
gui_set_radix -radix {decimal} -signals {{V1:top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.header_fifo.hdr_fifo.DEPTH}}
gui_set_radix -radix {twosComplement} -signals {{V1:top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.header_fifo.hdr_fifo.DEPTH}}
gui_set_radix -radix {decimal} -signals {{V1:top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.header_fifo.hdr_fifo.WIDTH}}
gui_set_radix -radix {twosComplement} -signals {{V1:top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.header_fifo.hdr_fifo.WIDTH}}

set _session_group_445 epl0_port0_wdata
gui_sg_create "$_session_group_445"
set epl0_port0_wdata "$_session_group_445"

gui_sg_addsignal -group "$_session_group_445" { {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.wdata_fifo.cclk} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.wdata_fifo.rst} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.wdata_fifo.tag_info_in} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.wdata_fifo.tag_info_out} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.wdata_fifo.wr_data_in} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.wdata_fifo.wr_data_out} }

set _session_group_446 epl0_wrarb
gui_sg_create "$_session_group_446"
set epl0_wrarb "$_session_group_446"

gui_sg_addsignal -group "$_session_group_446" { {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.wrarb.cclk} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.wrarb.rst} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.wrarb.tag_info_if} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.wrarb.tag_info_p0} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.wrarb.tag_info_p1} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.wrarb.tag_info_p2} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.wrarb.tag_info_p3} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.wrarb.wr_data_if} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.wrarb.wr_data_p0} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.wrarb.wr_data_p1} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.wrarb.wr_data_p2} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.wrarb.wr_data_p3} }

set _session_group_447 pre_arb
gui_sg_create "$_session_group_447"
set pre_arb "$_session_group_447"

gui_sg_addsignal -group "$_session_group_447" { top.pre_post_wrap.mby_igr_pre_ppe.arb.cclk top.pre_post_wrap.mby_igr_pre_ppe.arb.rst top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_req top.pre_post_wrap.mby_igr_pre_ppe.arb.pdata_lpp0_fpp top.pre_post_wrap.mby_igr_pre_ppe.arb.pdata_lpp1 top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_lpp0_fpp top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_lpp1 }
gui_sg_addsignal -group "$_session_group_447" { Divider } -divider
gui_sg_addsignal -group "$_session_group_447" { top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_p0 top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_p1 top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_p2 top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_p3 top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_p4 top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_p5 top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_p6 top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_p7 top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_p8 top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_p9 top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_p10 top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_p11 top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_p12 top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_p13 top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_p14 top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_p15 top.pre_post_wrap.mby_igr_pre_ppe.arb.igr_vp top.pre_post_wrap.mby_igr_pre_ppe.arb.port_pdata_p0 top.pre_post_wrap.mby_igr_pre_ppe.arb.port_pdata_p1 top.pre_post_wrap.mby_igr_pre_ppe.arb.port_pdata_p2 top.pre_post_wrap.mby_igr_pre_ppe.arb.port_pdata_p3 top.pre_post_wrap.mby_igr_pre_ppe.arb.port_pdata_p4 top.pre_post_wrap.mby_igr_pre_ppe.arb.port_pdata_p5 top.pre_post_wrap.mby_igr_pre_ppe.arb.port_pdata_p6 top.pre_post_wrap.mby_igr_pre_ppe.arb.port_pdata_p7 top.pre_post_wrap.mby_igr_pre_ppe.arb.port_pdata_p8 top.pre_post_wrap.mby_igr_pre_ppe.arb.port_pdata_p9 top.pre_post_wrap.mby_igr_pre_ppe.arb.port_pdata_p10 top.pre_post_wrap.mby_igr_pre_ppe.arb.port_pdata_p11 top.pre_post_wrap.mby_igr_pre_ppe.arb.port_pdata_p12 top.pre_post_wrap.mby_igr_pre_ppe.arb.port_pdata_p13 top.pre_post_wrap.mby_igr_pre_ppe.arb.port_pdata_p14 top.pre_post_wrap.mby_igr_pre_ppe.arb.port_pdata_p15 top.pre_post_wrap.mby_igr_pre_ppe.arb.port_pdata_vp top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_p0 top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_p1 top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_p2 top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_p3 top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_p4 top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_p5 top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_p6 top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_p7 top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_p8 top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_p9 top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_p10 top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_p11 top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_p12 top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_p13 top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_p14 top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_p15 top.pre_post_wrap.mby_igr_pre_ppe.arb.sop_mdata_vp }

set _session_group_448 rx_ppe
gui_sg_create "$_session_group_448"
set rx_ppe "$_session_group_448"

gui_sg_addsignal -group "$_session_group_448" { top.rx_ppe.igr_rx_ppe_intf0_head top.rx_ppe.intf0_queue top.rx_ppe.rx_ppe_igr_intf0 }

set _session_group_449 post_ppe
gui_sg_create "$_session_group_449"
set post_ppe "$_session_group_449"

gui_sg_addsignal -group "$_session_group_449" { top.pre_post_wrap.mby_igr_post_ppe.cclk top.pre_post_wrap.mby_igr_post_ppe.egr_igr_wreq top.pre_post_wrap.mby_igr_post_ppe.mim_wreq_0 top.pre_post_wrap.mby_igr_post_ppe.mim_wreq_1 top.pre_post_wrap.mby_igr_post_ppe.mim_wreq_2 top.pre_post_wrap.mby_igr_post_ppe.mim_wreq_3 top.pre_post_wrap.mby_igr_post_ppe.mim_wreq_4 top.pre_post_wrap.mby_igr_post_ppe.mim_wreq_5 top.pre_post_wrap.mby_igr_post_ppe.pdata_lpp0_fpp top.pre_post_wrap.mby_igr_post_ppe.pdata_lpp1 top.pre_post_wrap.mby_igr_post_ppe.rst top.pre_post_wrap.mby_igr_post_ppe.rx_ppe_igr top.pre_post_wrap.mby_igr_post_ppe.sop_mdata_lpp0_fpp top.pre_post_wrap.mby_igr_post_ppe.sop_mdata_lpp1 top.pre_post_wrap.mby_igr_post_ppe.tag_info_epl top.pre_post_wrap.mby_igr_post_ppe.tag_info_vp top.pre_post_wrap.mby_igr_post_ppe.wr_data_0 top.pre_post_wrap.mby_igr_post_ppe.wr_data_1 top.pre_post_wrap.mby_igr_post_ppe.wr_data_2 top.pre_post_wrap.mby_igr_post_ppe.igr_tag_ring_lltag0 top.pre_post_wrap.mby_igr_post_ppe.igr_tag_ring_lltag1 }

set _session_group_450 post_ppe_header
gui_sg_create "$_session_group_450"
set post_ppe_header "$_session_group_450"

gui_sg_addsignal -group "$_session_group_450" { top.pre_post_wrap.mby_igr_post_ppe.header.cclk top.pre_post_wrap.mby_igr_post_ppe.header.rst top.pre_post_wrap.mby_igr_post_ppe.header.rx_ppe_igr top.pre_post_wrap.mby_igr_post_ppe.header.tag_info top.pre_post_wrap.mby_igr_post_ppe.header.sop_mdata_lpp0_fpp top.pre_post_wrap.mby_igr_post_ppe.header.sop_mdata_lpp1 top.pre_post_wrap.mby_igr_post_ppe.header.o_merge_md top.pre_post_wrap.mby_igr_post_ppe.header.o_body_head_ptr top.pre_post_wrap.mby_igr_post_ppe.header.o_merge_xmd top.pre_post_wrap.mby_igr_post_ppe.header.header_data {top.pre_post_wrap.mby_igr_post_ppe.header.next_header_data[31]} top.pre_post_wrap.mby_igr_post_ppe.header.next_header_data }

set _session_group_451 post_ppe_merge
gui_sg_create "$_session_group_451"
set post_ppe_merge "$_session_group_451"

gui_sg_addsignal -group "$_session_group_451" { top.pre_post_wrap.mby_igr_post_ppe.merge.cclk top.pre_post_wrap.mby_igr_post_ppe.merge.rst top.pre_post_wrap.mby_igr_post_ppe.merge.i_merge_valid top.pre_post_wrap.mby_igr_post_ppe.merge.i_merge_xmd top.pre_post_wrap.mby_igr_post_ppe.merge.i_merge_sema top.pre_post_wrap.mby_igr_post_ppe.merge.i_merge_seg_ptr top.pre_post_wrap.mby_igr_post_ppe.merge.i_merge_md top.pre_post_wrap.mby_igr_post_ppe.merge.md_0_data_ecc top.pre_post_wrap.mby_igr_post_ppe.merge.wr_partial_1 top.pre_post_wrap.mby_igr_post_ppe.merge.wr_partial_0 top.pre_post_wrap.mby_igr_post_ppe.merge.wr_md_1 top.pre_post_wrap.mby_igr_post_ppe.merge.wr_md_0 top.pre_post_wrap.mby_igr_post_ppe.merge.pdata_lpp1 top.pre_post_wrap.mby_igr_post_ppe.merge.pdata_lpp0_fpp }

set _session_group_452 write_queue
gui_sg_create "$_session_group_452"
set write_queue "$_session_group_452"

gui_sg_addsignal -group "$_session_group_452" { top.pre_post_wrap.mby_igr_post_ppe.write_queue.cclk top.pre_post_wrap.mby_igr_post_ppe.write_queue.rst top.pre_post_wrap.mby_igr_post_ppe.write_queue.wr_partial_1 top.pre_post_wrap.mby_igr_post_ppe.write_queue.wr_partial_0 top.pre_post_wrap.mby_igr_post_ppe.write_queue.wr_md_1 top.pre_post_wrap.mby_igr_post_ppe.write_queue.wr_md_0 top.pre_post_wrap.mby_igr_post_ppe.write_queue.wr_data_2 top.pre_post_wrap.mby_igr_post_ppe.write_queue.wr_data_1 top.pre_post_wrap.mby_igr_post_ppe.write_queue.wr_data_0 top.pre_post_wrap.mby_igr_post_ppe.write_queue.mim_wreq_5 top.pre_post_wrap.mby_igr_post_ppe.write_queue.mim_wreq_4 top.pre_post_wrap.mby_igr_post_ppe.write_queue.mim_wreq_3 top.pre_post_wrap.mby_igr_post_ppe.write_queue.mim_wreq_2 top.pre_post_wrap.mby_igr_post_ppe.write_queue.mim_wreq_1 top.pre_post_wrap.mby_igr_post_ppe.write_queue.mim_wreq_0 top.pre_post_wrap.mby_igr_post_ppe.write_queue.egr_igr_wreq top.pre_post_wrap.mby_igr_post_ppe.write_queue.next_mim_wr_data }

set _session_group_453 free_id
gui_sg_create "$_session_group_453"
set free_id "$_session_group_453"

gui_sg_addsignal -group "$_session_group_453" { top.pre_post_wrap.mby_igr_pre_ppe.id_list.cclk top.pre_post_wrap.mby_igr_pre_ppe.id_list.rst top.pre_post_wrap.mby_igr_pre_ppe.id_list.i_free_id_req top.pre_post_wrap.mby_igr_pre_ppe.id_list.o_free_id_valid top.pre_post_wrap.mby_igr_pre_ppe.id_list.o_free_id top.pre_post_wrap.mby_igr_pre_ppe.id_list.free_list top.pre_post_wrap.mby_igr_pre_ppe.id_list.full top.pre_post_wrap.mby_igr_pre_ppe.id_list.empty top.pre_post_wrap.mby_igr_pre_ppe.id_list.aempty top.pre_post_wrap.mby_igr_pre_ppe.id_list.read_ptr top.pre_post_wrap.mby_igr_pre_ppe.id_list.wen_QA top.pre_post_wrap.mby_igr_pre_ppe.id_list.ren_QA top.pre_post_wrap.mby_igr_pre_ppe.id_list.write_ptr top.pre_post_wrap.mby_igr_pre_ppe.id_list.empty_space top.pre_post_wrap.mby_igr_pre_ppe.id_list.FREE_LIST_WIDTH top.pre_post_wrap.mby_igr_pre_ppe.id_list.FREE_LIST_DEPTH top.pre_post_wrap.mby_igr_pre_ppe.id_list.FREE_LIST_ADDR_BITS top.pre_post_wrap.mby_igr_pre_ppe.id_list.MGP_PKT_ID_CNT {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.o_free_id_req} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.i_free_id_valid} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.i_free_id} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.next_free_id_req} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.curr_free_id} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.hdr_sop_valid} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.hdr_eop_valid} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.tag_valid} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.wdata_valid} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.curr_free_id_valid} }
gui_set_radix -radix {decimal} -signals {V1:top.pre_post_wrap.mby_igr_pre_ppe.id_list.FREE_LIST_WIDTH}
gui_set_radix -radix {twosComplement} -signals {V1:top.pre_post_wrap.mby_igr_pre_ppe.id_list.FREE_LIST_WIDTH}
gui_set_radix -radix {decimal} -signals {V1:top.pre_post_wrap.mby_igr_pre_ppe.id_list.FREE_LIST_DEPTH}
gui_set_radix -radix {twosComplement} -signals {V1:top.pre_post_wrap.mby_igr_pre_ppe.id_list.FREE_LIST_DEPTH}
gui_set_radix -radix {decimal} -signals {V1:top.pre_post_wrap.mby_igr_pre_ppe.id_list.FREE_LIST_ADDR_BITS}
gui_set_radix -radix {twosComplement} -signals {V1:top.pre_post_wrap.mby_igr_pre_ppe.id_list.FREE_LIST_ADDR_BITS}
gui_set_radix -radix {decimal} -signals {V1:top.pre_post_wrap.mby_igr_pre_ppe.id_list.MGP_PKT_ID_CNT}
gui_set_radix -radix {twosComplement} -signals {V1:top.pre_post_wrap.mby_igr_pre_ppe.id_list.MGP_PKT_ID_CNT}

set _session_group_454 Group2
gui_sg_create "$_session_group_454"
set Group2 "$_session_group_454"

gui_sg_addsignal -group "$_session_group_454" { {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.cclk} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.i_shim_pb_v} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.word_count} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.curr_word_in} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.curr_hdr_word} }

# Global: Highlighting

# Global: Stack
gui_change_stack_mode -mode list

# Post database loading setting...

# Restore C1 time
gui_set_time -C1_only 7175



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
gui_list_set_filter -id ${Hier.1} -list { {Package 1} {All 0} {Process 1} {VirtPowSwitch 0} {UnnamedProcess 1} {UDP 0} {Function 1} {Block 1} {SrsnAndSpaCell 0} {OVA Unit 1} {LeafScCell 1} {LeafVlgCell 1} {Interface 0} {LeafVhdCell 1} {$unit 1} {NamedBlock 1} {Task 1} {VlgPackage 1} {ClassDef 1} {VirtIsoCell 0} }
gui_list_set_filter -id ${Hier.1} -text {*}
gui_hier_list_init -id ${Hier.1}
gui_change_design -id ${Hier.1} -design V1
catch {gui_list_expand -id ${Hier.1} top}
catch {gui_list_expand -id ${Hier.1} top.pre_post_wrap}
catch {gui_list_expand -id ${Hier.1} top.pre_post_wrap.mby_igr_pre_ppe}
catch {gui_list_expand -id ${Hier.1} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst}}
catch {gui_list_select -id ${Hier.1} {{top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.wrarb}}}
gui_view_scroll -id ${Hier.1} -vertical -set 60
gui_view_scroll -id ${Hier.1} -horizontal -set 0

# Data 'Data.1'
gui_list_set_filter -id ${Data.1} -list { {Buffer 1} {Input 1} {Others 1} {Linkage 1} {Output 1} {LowPower 1} {Parameter 1} {All 1} {Aggregate 1} {LibBaseMember 1} {Event 1} {Assertion 1} {Constant 1} {Interface 1} {BaseMembers 1} {Signal 1} {$unit 1} {Inout 1} {Variable 1} }
gui_list_set_filter -id ${Data.1} -text {*}
gui_list_show_data -id ${Data.1} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.wrarb}
gui_list_expand -id ${Data.1} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.wrarb.wr_data_p0}
gui_list_expand -id ${Data.1} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.wrarb.wr_data_p0.data_ecc}
gui_list_expand -id ${Data.1} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.wrarb.wr_data_p0.data_ecc[0]}
gui_list_expand -id ${Data.1} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.wrarb.wr_data_p0.data_ecc}
gui_list_expand -id ${Data.1} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.wrarb.wr_data_p0.data_ecc[0]}
gui_list_expand -id ${Data.1} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.wrarb.wr_data_p0.data_ecc[0]}
gui_view_scroll -id ${Data.1} -vertical -set 137
gui_view_scroll -id ${Data.1} -horizontal -set 0
gui_view_scroll -id ${Hier.1} -vertical -set 60
gui_view_scroll -id ${Hier.1} -horizontal -set 0

# Source 'Source.1'
gui_src_value_annotate -id ${Source.1} -switch false
gui_set_env TOGGLE::VALUEANNOTATE 0
gui_open_source -id ${Source.1}  -replace -active {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl}
gui_view_scroll -id ${Source.1} -vertical -set 432
gui_src_set_reusable -id ${Source.1}

# View 'Wave.1'
gui_wv_sync -id ${Wave.1} -switch false
set groupExD [gui_get_pref_value -category Wave -key exclusiveSG]
gui_set_pref_value -category Wave -key exclusiveSG -value {false}
set origWaveHeight [gui_get_pref_value -category Wave -key waveRowHeight]
gui_list_set_height -id Wave -height 25
set origGroupCreationState [gui_list_create_group_when_add -wave]
gui_list_create_group_when_add -wave -disable
gui_marker_create -id ${Wave.1} C2 7165
gui_marker_select -id ${Wave.1} {  C2 }
gui_marker_set_ref -id ${Wave.1}  C1
gui_wv_zoom_timerange -id ${Wave.1} 1079 1313
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
gui_list_add_group -id ${Wave.1} -after {New Group} {Group2}
gui_list_collapse -id ${Wave.1} epl0_port0_ctrl
gui_list_collapse -id ${Wave.1} epl0_port0_header
gui_list_collapse -id ${Wave.1} epl0_port0_wdata
gui_list_collapse -id ${Wave.1} epl0_wrarb
gui_list_collapse -id ${Wave.1} pre_arb
gui_list_collapse -id ${Wave.1} rx_ppe
gui_list_collapse -id ${Wave.1} post_ppe_header
gui_list_collapse -id ${Wave.1} post_ppe_merge
gui_list_collapse -id ${Wave.1} write_queue
gui_list_collapse -id ${Wave.1} free_id
gui_list_expand -id ${Wave.1} top.pre_post_wrap.mby_igr_pre_ppe.i_shim_pb_v_p0
gui_list_expand -id ${Wave.1} {top.pre_post_wrap.mby_igr_pre_ppe.i_shim_pb_v_p0[1]}
gui_list_expand -id ${Wave.1} {top.pre_post_wrap.mby_igr_pre_ppe.i_shim_pb_v_p0[0]}
gui_list_expand -id ${Wave.1} top.pre_post_wrap.mby_igr_pre_ppe.i_shim_pb_md_p0
gui_list_expand -id ${Wave.1} {top.pre_post_wrap.mby_igr_pre_ppe.i_shim_pb_md_p0[0]}
gui_list_expand -id ${Wave.1} {top.pre_post_wrap.mby_igr_pre_ppe.i_shim_pb_md_p0[0].md0}
gui_list_expand -id ${Wave.1} {top.pre_post_wrap.mby_igr_pre_ppe.i_shim_pb_md_p0[0].md0.md}
gui_list_expand -id ${Wave.1} top.pre_post_wrap.mby_igr_pre_ppe.sop_mdata_lpp0_fpp
gui_list_expand -id ${Wave.1} top.pre_post_wrap.mby_igr_pre_ppe.sop_mdata_lpp0_fpp.md
gui_list_expand -id ${Wave.1} top.pre_post_wrap.mby_igr_pre_ppe.tag_info_epl
gui_list_expand -id ${Wave.1} {top.pre_post_wrap.mby_igr_pre_ppe.tag_info_epl[0]}
gui_list_expand -id ${Wave.1} {top.pre_post_wrap.mby_igr_pre_ppe.tag_info_epl[0].md}
gui_list_expand -id ${Wave.1} top.pre_post_wrap.mby_igr_post_ppe.rx_ppe_igr
gui_list_expand -id ${Wave.1} top.pre_post_wrap.mby_igr_post_ppe.rx_ppe_igr.intf0
gui_list_expand -id ${Wave.1} top.pre_post_wrap.mby_igr_post_ppe.sop_mdata_lpp0_fpp
gui_list_expand -id ${Wave.1} top.pre_post_wrap.mby_igr_post_ppe.sop_mdata_lpp0_fpp.md
gui_list_expand -id ${Wave.1} top.pre_post_wrap.mby_igr_post_ppe.tag_info_epl
gui_list_expand -id ${Wave.1} {top.pre_post_wrap.mby_igr_post_ppe.tag_info_epl[0]}
gui_list_expand -id ${Wave.1} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.i_shim_pb_v}
gui_list_expand -id ${Wave.1} {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.curr_hdr_word}
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
gui_list_set_insertion_bar  -id ${Wave.1} -group Group2  -item {top.pre_post_wrap.mby_igr_pre_ppe.epl[0].inst.port0.ctrl.curr_hdr_word[0]} -position below

gui_marker_move -id ${Wave.1} {C1} 7175
gui_view_scroll -id ${Wave.1} -vertical -set 3442
gui_show_grid -id ${Wave.1} -enable false
# Restore toplevel window zorder
# The toplevel window could be closed if it has no view/pane
if {[gui_exist_window -window ${TopLevel.1}]} {
	gui_set_active_window -window ${TopLevel.1}
	gui_set_active_window -window ${Source.1}
	gui_set_active_window -window ${DLPane.1}
}
if {[gui_exist_window -window ${TopLevel.2}]} {
	gui_set_active_window -window ${TopLevel.2}
	gui_set_active_window -window ${Wave.1}
}
#</Session>

