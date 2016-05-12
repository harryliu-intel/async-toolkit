`ifndef CAST2VERILOG_INPUT_TIMINGBUFFER
`define CAST2VERILOG_INPUT_TIMINGBUFFER \input_timing_buffer$ 
`endif
`ifndef CAST2VERILOG_OUTPUT_TIMINGBUFFER
`define CAST2VERILOG_OUTPUT_TIMINGBUFFER \output_timing_buffer$ 
`endif
`ifndef CAST2VERILOG_INPUT_TIMINGBUFFER_SLACK0
`define CAST2VERILOG_INPUT_TIMINGBUFFER_SLACK0 \input_timing_buffer$s0 
`endif
`ifndef CAST2VERILOG_OUTPUT_TIMINGBUFFER_SLACK0
`define CAST2VERILOG_OUTPUT_TIMINGBUFFER_SLACK0 \output_timing_buffer$s0 
`endif
`ifndef CAST2VERILOG_BD_INPUT_TIMINGBUFFER
`define CAST2VERILOG_BD_INPUT_TIMINGBUFFER \bd_input_timing_buffer$ 
`endif
`ifndef CAST2VERILOG_BD_OUTPUT_TIMINGBUFFER
`define CAST2VERILOG_BD_OUTPUT_TIMINGBUFFER \bd_output_timing_buffer$ 
`endif
`ifndef CAST2VERILOG_BD_INPUT_TIMINGBUFFER_SLACK0
`define CAST2VERILOG_BD_INPUT_TIMINGBUFFER_SLACK0 \bd_input_timing_buffer$s0 
`endif
`ifndef CAST2VERILOG_BD_OUTPUT_TIMINGBUFFER_SLACK0
`define CAST2VERILOG_BD_OUTPUT_TIMINGBUFFER_SLACK0 \bd_output_timing_buffer$s0 
`endif
`ifndef CAST2VERILOG_INPUT_DFT
`define CAST2VERILOG_INPUT_DFT csp_input_dft
`endif
`ifndef CAST2VERILOG_OUTPUT_DFT
`define CAST2VERILOG_OUTPUT_DFT csp_output_dft
`endif
`ifndef CAST2VERILOG_NEW_INPUT_DFT
`define CAST2VERILOG_NEW_INPUT_DFT csp_new_input_dft
`endif
`ifndef CAST2VERILOG_NEW_OUTPUT_DFT
`define CAST2VERILOG_NEW_OUTPUT_DFT csp_new_output_dft
`endif
`ifndef CAST2VERILOG_PASSTHRU_DFT
`define CAST2VERILOG_PASSTHRU_DFT csp_passthru_dft
`endif
`ifndef CAST2VERILOG_SRAM_SERIAL
`define CAST2VERILOG_SRAM_SERIAL csp_sram_serial
`endif
`ifndef CAST2VERILOG_PASSTHRU_SRAM_SERIAL
`define CAST2VERILOG_PASSTHRU_SRAM_SERIAL csp_passthru_sram_serial
`endif
`ifndef CAST2VERILOG_ARBITER
`define CAST2VERILOG_ARBITER csp_fair_arbiter
`endif
`ifndef CAST2VERILOG_CELEMENT
`define CAST2VERILOG_CELEMENT CELEMENT
`endif
`ifndef BLACKBOX
`define BLACKBOX GATE_Z_MIKE
`endif
`ifndef BLACKBOX_TRIREG
`define BLACKBOX_TRIREG GATE_Z_MIKE_TRIREG
`endif
`ifndef BLACKBOX_CLK
`define BLACKBOX_CLK GATE_Z_MIKE_CLK
`endif
`ifndef BLACKBOX_TRIREG_CLK
`define BLACKBOX_TRIREG_CLK GATE_Z_MIKE_CLK_TRIREG
`endif
`ifndef NAND2
`define NAND2 nand
`endif
`ifndef NAND3
`define NAND3 nand
`endif
`ifndef NAND4
`define NAND4 nand
`endif
`ifndef NAND5
`define NAND5 nand
`endif
`ifndef NAND6
`define NAND6 nand
`endif
`ifndef NOR2
`define NOR2 nor
`endif
`ifndef NOR3
`define NOR3 nor
`endif
`ifndef NOR4
`define NOR4 nor
`endif
`ifndef NOR5
`define NOR5 nor
`endif
`ifndef NOR6
`define NOR6 nor
`endif
`ifndef INV
`define INV not
`endif
`ifndef AND
`define AND and
`endif
`ifndef OR
`define OR or
`endif
