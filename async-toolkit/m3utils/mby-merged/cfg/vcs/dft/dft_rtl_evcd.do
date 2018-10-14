dump -file ebg.evcd -type EVCD
dump -fid EVCD0 -add top.pch -depth 1

##################################################################################
# Internal signals for vep2 to insert loops and labels as HVM hooks for preambles:

# route_to_tam_en label for TAM and HBP preamble:
#dump -fid EVCD0 -add top.pch.parpsf1.parpsf1_pwell_wrapper.psf1.i_psf20_top_psf1.i_psf20_pwrgt.route_to_tam_en -depth 1
#dump -fid EVCD0 -add top.pch.parpsf2.parpsf2_pwell_wrapper.psf2.i_psf20_top_psf2.i_psf20_pwrgt.route_to_tam_en -depth 1
#dump -fid EVCD0 -add top.pch.parpsf3.parpsf3_pwell_wrapper.psf3.i_psf20_top_psf3.i_psf20_pwrgt.route_to_tam_en -depth 1
#dump -fid EVCD0 -add top.pch.parpsf8.i_fabric8.i_psf20_top_psf8.i_psf20_pwrgt.route_to_tam_en -depth 1
#dump -fid EVCD0 -add top.pch.parpsf9.i_fabric9.i_psf20_top_psf9.i_psf20_pwrgt.route_to_tam_en -depth 1
#dump -fid EVCD0 -add top.pch.parpsf10.i_fabric10.i_psf20_top_psf10.i_psf20_pwrgt.route_to_tam_en -depth 1
#dump -fid EVCD0 -add top.pch.parsata.parsata_pwell_wrapper.psf4.i_psf20_top_psf4.i_psf20_pwrgt.route_to_tam_en -depth 1
#dump -fid EVCD0 -add top.pch.parssata.parssata_pwell_wrapper.psf7.i_psf20_top_psf7.i_psf20_pwrgt.route_to_tam_en -depth 1
#dump -fid EVCD0 -add top.pch.parcsmeb.parcsmeb_pwell_wrapper.csme.i_psf20_top_csme.i_psf20_pwrgt.route_to_tam_en -depth 1
#dump -fid EVCD0 -add top.pch.parieb.parieb_pwell_wrapper.ie.i_psf20_top_ie.i_psf20_pwrgt.route_to_tam_en -depth 1

# Label SRAM_ZERO_DONE when this signal is "1"
#dump -fid EVCD0 -add top.pch.pardfx.pardfx_pwell_wrapper.mst_dfx_core1.dfx_secured_misc.dtglueunit1.dttcrlogic1.dt_sram_zero_done_rd -depth 1

# Label SRAM_ZERO_EN when this signal is "1"
#dump -fid EVCD0 -add top.pch.pardfx.pardfx_pwell_wrapper.mst_dfx_core1.dfx_secured_misc.dtglueunit1.dttcrlogic1.dtsus_scr_sram_zero_en -depth 1

# Label PON_SENSE_BEGIN
#dump -fid EVCD0 -add top.pch.parfuse.parfuse_pwell_wrapper.fuse_top1.i_fuse_clock_power_cntrl.i_fuse_pgcb_cntrl.start_pwron_sense_pulse -depth 1

# Label PON_SENSE_DONE
#dump -fid EVCD0 -add top.pch.parfuse.parfuse_pwell_wrapper.fuse_top1.i_chassis_fuse_controller_top.i_fuse_array_cntrl.i_fuse_sense_fsm.hvm_pwron_sense_done -depth 1

# WMP20 PLL warm up loop insertion:
#dump -fid EVCD0 -add top.pch.parfia20.parfia20_pwell_wrapper.hip_wm20.com0.cmnlogic.cmn_pllctrl_1.o_dtpll_lc_pllen_h -depth 1
#dump -fid EVCD0 -add top.pch.parfia20.parfia20_pwell_wrapper.hip_wm20.com1.cmnlogic.cmn_pllctrl_1.o_dtpll_lc_pllen_h -depth 1

# WMP26 PLL warm up loop insertion:
#dump -fid EVCD0 -add top.pch.parfia.parfia_pwell_wrapper.hip_wm26.com0.cmnlogic.cmn_pllctrl_1.o_dtpll_lc_pllen_h -depth 1
#dump -fid EVCD0 -add top.pch.parfia.parfia_pwell_wrapper.hip_wm26.com1.cmnlogic.cmn_pllctrl_1.o_dtpll_lc_pllen_h -depth 1
#dump -fid EVCD0 -add top.pch.parfia.parfia_pwell_wrapper.hip_wm26.com2.cmnlogic.cmn_pllctrl_1.o_dtpll_lc_pllen_h -depth 1

# WMP20 PLL Lock Indicator:
#dump -fid EVCD0 -add top.pch.parfia20.parfia20_pwell_wrapper.hip_wm20.com0.hspllebb1.lptop0.lptop0.i101.icontrol.plldig0.plllockout -depth 1
#dump -fid EVCD0 -add top.pch.parfia20.parfia20_pwell_wrapper.hip_wm20.com1.hspllebb1.lptop0.lptop0.i101.icontrol.plldig0.plllockout -depth 1

#WMP26 PLL Lock Indicator:
#dump -fid EVCD0 -add top.pch.parfia.parfia_pwell_wrapper.hip_wm26.com0.hspllebb1.lptop0.lptop0.i101.icontrol.plldig0.plllockout -depth 1
#dump -fid EVCD0 -add top.pch.parfia.parfia_pwell_wrapper.hip_wm26.com1.hspllebb1.lptop0.lptop0.i101.icontrol.plldig0.plllockout -depth 1
#dump -fid EVCD0 -add top.pch.parfia.parfia_pwell_wrapper.hip_wm26.com2.hspllebb1.lptop0.lptop0.i101.icontrol.plldig0.plllockout -depth 1

# Global Rcomp warm up loop insertion:
#dump -fid EVCD0 -add top.pch.parfia20.parfia20_pwell_wrapper.hip_wm20.com1.cmnlogic.cmn_reflogic.cmn_grc.o_dtpll_ref_fcompen_h -depth 1
#dump -fid EVCD0 -add top.pch.parfia.parfia_pwell_wrapper.hip_wm26.com0.cmnlogic.cmn_reflogic.cmn_grc.o_dtpll_ref_fcompen_h -depth 1

# Global Rcomp Done Label Insertion:
#dump -fid EVCD0 -add top.pch.parfia20.parfia20_pwell_wrapper.hip_wm20.com1.cmnlogic.cmn_reflogic.cmn_grc.o_grc_done_h -depth 1
#dump -fid EVCD0 -add top.pch.parfia.parfia_pwell_wrapper.hip_wm26.com0.cmnlogic.cmn_reflogic.cmn_grc.o_grc_done_h -depth 1

# Lane Rcomp Done Label Insertion:
#dump -fid EVCD0 -add top.pch.parfia20.parfia20_pwell_wrapper.hip_wm20.com1.cmnlogic.cmn_reflogic.cmn_lane_rcomp.o_lrc_done_h -depth 1
#dump -fid EVCD0 -add top.pch.parfia.parfia_pwell_wrapper.hip_wm26.com1.cmnlogic.cmn_reflogic.cmn_lane_rcomp.o_lrc_done_h -depth 1

# ISCLK Bclk PLL Lock Wait Delay:
#dump -fid EVCD0 -add top.pch.lbgns_io_wrapper.io_pwell_wrapper.isclk1.i_c73p1isclk_clksrctop.bcktp.clkan.pllan.icontrol.o_pllen_h_buf -depth 1

# ISCLK Dpclk PLL Lock Wait Delay:
#dump -fid EVCD0 -add top.pch.lbgns_io_wrapper.io_pwell_wrapper.isclk1.i_c73p1isclk_clksrctop.dcktp.clkan.pllan.icontrol.o_pllen_h_buf -depth 1

# USB2 PLL Lock Wait Delay:
#dump -fid EVCD0 -add top.pch.parxhcib.parxhcib_pwell_wrapper.hip_usb2.usbhsip_gblwrapper1.c73usb2e0_usbhsip_pcsafegblwrapper.pllcombg.io_usbpllckbuf.ipllanalog.ipllana.pllen -depth 1

# ISCLK Bclk PLL Lock Label:
#dump -fid EVCD0 -add top.pch.lbgns_io_wrapper.io_pwell_wrapper.isclk1.i_c73p1isclk_clksrctop.dcktp.clkan.pllan.icontrol.o_plllock -depth 1

# ISCLK Dpclk PLL Lock Label:
#dump -fid EVCD0 -add top.pch.lbgns_io_wrapper.io_pwell_wrapper.isclk1.i_c73p1isclk_clksrctop.bcktp.clkan.pllan.icontrol.o_plllock -depth 1

# USB2 PLL Lock Label:
#dump -fid EVCD0 -add top.pch.parxhcib.parxhcib_pwell_wrapper.hip_usb2.usbhsip_gblwrapper1.c73usb2e0_usbhsip_pcsafegblwrapper.pllcombg.io_usbpllckbuf.ipllanalog.ipllcore.subcore.idigtop.i_digcore.i_dft.o_plllock -depth 1
#dump -fid EVCD0 -add top.pch.lbgns_io_wrapper.icfiotxen_b_gpp_a_1_lad_0_espi_io_0_lv -depth 1
#dump -fid EVCD0 -add top.pch.lbgns_io_wrapper.icfiotxen_b_gpp_a_2_lad_1_espi_io_1_lv -depth 1
#dump -fid EVCD0 -add top.pch.lbgns_io_wrapper.icfiotxen_b_gpp_a_3_lad_2_espi_io_2_lv -depth 1
#dump -fid EVCD0 -add top.pch.lbgns_io_wrapper.icfiotxen_b_gpp_a_4_lad_3_espi_io_3_lv -depth 1
#dump -fid EVCD0 -add top.pch.lbgns_io_wrapper.icfiotxen_b_spi0_io_2_lv -depth 1
#dump -fid EVCD0 -add top.pch.lbgns_io_wrapper.icfiotxen_b_spi0_io_3_lv -depth 1
#dump -fid EVCD0 -add top.pch.lbgns_io_wrapper.icfiotxen_b_spi0_miso_io_1_lv -depth 1 
#dump -fid EVCD0 -add top.pch.lbgns_io_wrapper.icfiotxen_b_spi0_mosi_io_0_lv -depth 1
#dump -fid EVCD0 -add {top.pch.parleg.parleg_pwell_wrapper.spi_pw1.io_espi_spi_data[3]} -depth 1
#dump -fid EVCD0 -add {top.pch.parleg.parleg_pwell_wrapper.spi_pw1.io_espi_spi_data[2]} -depth 1
#dump -fid EVCD0 -add {top.pch.parleg.parleg_pwell_wrapper.spi_pw1.io_espi_spi_data[1]} -depth 1
#dump -fid EVCD0 -add {top.pch.parleg.parleg_pwell_wrapper.spi_pw1.io_espi_spi_data[0]} -depth 1
