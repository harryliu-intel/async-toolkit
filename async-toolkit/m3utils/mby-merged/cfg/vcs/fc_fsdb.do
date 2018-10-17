## FSDB dump commands
fsdbDumpfile verilog.fsdb
## fsdbDumpfile("verilog.fsdb", 5000); 
## Delay the dump
## #50000 fsdbDumpon  fsdbDumpoff
fsdbDumpvars 9 fc_hdl_top
fsdbDumpvars 8 fc_hvl_top
#fsdbDumpvars 0 fc_hdl_top.dut.icm_top
#fsdbDumpvars 0 fc_hdl_top.dut.par_clk
#fsdbDumpvars 0 fc_hdl_top.dut.par_dfxmdu
#fsdbDumpvars 0 fc_hdl_top.dut.par_fuse
#fsdbDumpvars 0 fc_hdl_top.dut.par_gpio
#fsdbDumpvars 0 fc_hdl_top.dut.par_pcie0
#fsdbDumpvars 0 fc_hdl_top.dut.par_pcie1
#fsdbDumpvars 0 fc_hdl_top.dut.par_pcie2
#fsdbDumpvars 0 fc_hdl_top.dut.par_pcie3
#fsdbDumpvars 0 fc_hdl_top.dut.par_psfnorth
#fsdbDumpvars 0 fc_hdl_top.dut.par_psfsouth
#fsdbDumpvars 0 fc_hdl_top.dut.par_sbr
#fsdbDumpvars 0 fc_hdl_top.dut.paremp

#fsdbDumpMDA 0 fc_hdl_top.dut
fsdbDumpSVA 0 fc_hvl_top
fsdbDumpSVA 0 fc_hdl_top
