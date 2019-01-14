`ifndef EGR_TQU_MEM_PKG
`define EGR_TQU_MEM_PKG

package egr_tqu_mem_pkg;

localparam EGR_TQU_EPL_EOP_FIFO_DEPTH = 384;
localparam EGR_TQU_EPL_EOP_FIFO_WIDTH = 14;
localparam EGR_TQU_EPL_EOP_FIFO_INST  = 4;

localparam EGR_TQU_EPL_PKTID_FIFO_DEPTH = 384;
localparam EGR_TQU_EPL_PKTID_FIFO_WIDTH = 9;
localparam EGR_TQU_EPL_PKTID_FIFO_INST  = 4;

localparam EGR_TQU_EPL_SMP_TB_DEPTH = 512;
localparam EGR_TQU_EPL_SMP_TB_WIDTH = 512;
localparam EGR_TQU_EPL_SMP_TB_INST  = 32;

localparam EGR_TQU_EPL_SOP_FIFO_DEPTH = 384;
localparam EGR_TQU_EPL_SOP_FIFO_WIDTH = 24;
localparam EGR_TQU_EPL_SOP_FIFO_INST  = 4;

localparam EGR_TQU_SMP_ROB_PTR_BANK_DEPTH = 1024;
localparam EGR_TQU_SMP_ROB_PTR_BANK_WIDTH = 12;
localparam EGR_TQU_SMP_ROB_PTR_BANK_INST  = 64;

typedef logic [$clog2(EGR_TQU_EPL_EOP_FIFO_DEPTH)-1:0] egr_tqu_epl_eop_fifo_addr_t;
typedef logic         [EGR_TQU_EPL_EOP_FIFO_WIDTH-1:0] egr_tqu_epl_eop_fifo_data_t;

typedef logic [$clog2(EGR_TQU_EPL_PKTID_FIFO_DEPTH)-1:0] egr_tqu_epl_pktid_fifo_addr_t;
typedef logic         [EGR_TQU_EPL_PKTID_FIFO_WIDTH-1:0] egr_tqu_epl_pktid_fifo_data_t;

typedef logic [$clog2(EGR_TQU_EPL_SMP_TB_DEPTH)-1:0] egr_tqu_epl_smp_tb_addr_t;
typedef logic         [EGR_TQU_EPL_SMP_TB_WIDTH-1:0] egr_tqu_epl_smp_tb_data_t;

typedef logic [$clog2(EGR_TQU_EPL_SOP_FIFO_DEPTH)-1:0] egr_tqu_epl_sop_fifo_addr_t;
typedef logic         [EGR_TQU_EPL_SOP_FIFO_WIDTH-1:0] egr_tqu_epl_sop_fifo_data_t;

typedef logic [$clog2(EGR_TQU_SMP_ROB_PTR_BANK_DEPTH)-1:0] egr_tqu_smp_rob_ptr_bank_addr_t;
typedef logic         [EGR_TQU_SMP_ROB_PTR_BANK_WIDTH-1:0] egr_tqu_smp_rob_ptr_bank_data_t;


endpackage:egr_tqu_mem_pkg
`endif
