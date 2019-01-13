`ifndef EGR_MRI_MEM_PKG
`define EGR_MRI_MEM_PKG

package egr_mri_mem_pkg;

localparam EGR_MRI_RREQ_FIFO_DEPTH = 4;
localparam EGR_MRI_RREQ_FIFO_WIDTH = 42;
localparam EGR_MRI_RREQ_FIFO_INST  = 8;

localparam EGR_MRI_RRSP_FIFO_DEPTH = 4;
localparam EGR_MRI_RRSP_FIFO_WIDTH = 528;
localparam EGR_MRI_RRSP_FIFO_INST  = 8;

localparam EGR_MRI_RRSP_MIM_STALL_REGS_DEPTH = 3;
localparam EGR_MRI_RRSP_MIM_STALL_REGS_WIDTH = 528;
localparam EGR_MRI_RRSP_MIM_STALL_REGS_INST  = 6;

typedef logic [$clog2(EGR_MRI_RREQ_FIFO_DEPTH)-1:0] egr_mri_rreq_fifo_addr_t;
typedef logic         [EGR_MRI_RREQ_FIFO_WIDTH-1:0] egr_mri_rreq_fifo_data_t;

typedef logic [$clog2(EGR_MRI_RRSP_FIFO_DEPTH)-1:0] egr_mri_rrsp_fifo_addr_t;
typedef logic         [EGR_MRI_RRSP_FIFO_WIDTH-1:0] egr_mri_rrsp_fifo_data_t;

typedef logic [$clog2(EGR_MRI_RRSP_MIM_STALL_REGS_DEPTH)-1:0] egr_mri_rrsp_mim_stall_regs_addr_t;
typedef logic         [EGR_MRI_RRSP_MIM_STALL_REGS_WIDTH-1:0] egr_mri_rrsp_mim_stall_regs_data_t;


endpackage:egr_mri_mem_pkg
`endif
