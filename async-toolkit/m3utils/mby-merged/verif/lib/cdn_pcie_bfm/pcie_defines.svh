`ifndef __pcie_defines_svh__
`define __pcie_defines_svh__

// Minimum packet length on the PCIe side is 1 dword
`define MIN_PCIE_PKTLEN_IN_DWORDS 'h1
// Max packet length for STL is 10368 bytes = 2592 dwords = 'ha20
`define MAX_PKTLEN_IN_DWORDS 'ha20

typedef enum {QUEUE_0=0, QUEUE_1=1, QUEUE_2=2, QUEUE_3=3} dma_queue_type;

typedef enum {NO_RESET=0, POWER_ON_RESET=1, PCIE_COLD_RESET=2, PCIE_HOT_RESET=3,
              DMAQ_SOFT_RESET_BY_RISC=4, DMAQ_SOFT_RESET_BY_PCIE=5} pcie_reset_type;

`endif
