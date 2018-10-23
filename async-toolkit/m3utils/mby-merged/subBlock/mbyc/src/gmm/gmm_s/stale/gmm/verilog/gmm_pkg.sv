`ifndef MBY_GMM_PKG_SV
    `define MBY_GMM_PKG_SV
    
package gmm_pkg;
    
     /**
    *  Control interface datatypes for global memory manager
    */
    
    //******************** LOCAL PARAMETERS ***************************
//   Local parameters of ring pod pointer packet  
localparam int unsigned W_SEG_POINTER       = 20;   // Data-width local parameter of segment pointer   
localparam int unsigned W_SEG_SEM           = 4;    // Data-width local parameter of semaphore 
localparam int unsigned W_VAL_RING_PKT      = 1;    // Data-width local parameter of valid bit of ring packet
localparam int unsigned W_STALL_RING_PKT    = 1;    // Data-width local parameter of stall bit of ring packet
localparam int unsigned W_CD_RING_PKT       = 1;    // Data-width local parameter of dirty/clean bit of ring packet
localparam int unsigned W_DSTID_RING_PKT    = 3;    // Data-width local parameter of destination ID of ring packet (ASSUMPTION: half rings)
localparam int unsigned W_SPARE_RING_PKT    = 2;    // Data-width local parameter of spare bits  of ring packet
localparam int unsigned W_ECC_RING_PKT      = 7;    // Data-width local parameter of ECC bits of ring packet  
localparam int unsigned W_CMP_SEG_POINTER   = W_SEG_POINTER + W_SEG_SEM;
localparam int unsigned W_RING_PKT          = W_CMP_SEG_POINTER + W_VAL_RING_PKT + W_STALL_RING_PKT + W_CD_RING_PKT+ W_DSTID_RING_PKT + W_SPARE_RING_PKT;
localparam int unsigned W_CMP_RING_PKT      = W_RING_PKT+W_ECC_RING_PKT;
// Local parameters of CPU MEM for loading/recovering segment pointers
localparam int unsigned  W_BYTE             = 8;    // Data width of a byte
localparam int unsigned  W_BYTES_PER_WORD   = 64;   // Data width of a system word 
// Local parameter of CSR
localparam int unsigned  W_CSR_DATA         = 512;  // Data width of a CSR data port //FIXME: review future releases of the FS to modify this
// Local parameter of pseudo row operations
localparam int unsigned  W_P_ROW_OP         = 53;   // Data width of a pseudo row operations  //FIXME: review future releases of the FS to modify this
     
    //******************** TYPE DEFs ***************************
    // Typedef of the system
    typedef logic [W_BYTE-1:0][W_BYTES_PER_WORD-1:0]    sys_word_t;
    // Typedef of pod pointer packet  
    typedef logic [W_SEG_POINTER-1:0]                   seg_ptr_t;
    typedef logic [W_SEG_SEM-1:0]                       seg_sem_t;
    typedef logic [W_CMP_SEG_POINTER-1:0]               cmp_seg_ptr_pkd_t;
    typedef logic [W_VAL_RING_PKT-1:0]                  val_ring_pkt_t;
    typedef logic [W_CD_RING_PKT-1:0]                   cd_ring_pkt_t;
    typedef logic [W_STALL_RING_PKT-1:0]                stall_ring_pkt_t;
    typedef logic [W_DSTID_RING_PKT-1:0]                dstid_ring_pkt_t;
    typedef logic [W_SPARE_RING_PKT-1:0]                spare_ring_pkt_t;
    typedef logic [W_ECC_RING_PKT-1:0]                  ecc_ring_pkt_t;
    typedef logic [W_RING_PKT-1:0]                      ring_pkt_pkd_t;
    typedef logic [W_CMP_RING_PKT-1:0]                  cmp_ring_pkt_pkd_t;
    // Typedef of  CSR
    typedef logic [W_CSR_DATA-1:0]                      csr_gmm_data_t;
    // Typedef of pseudo row operations 
    typedef logic [W_P_ROW_OP-1:0]                      p_row_op_t;   // Pseudo-row operation type
     
  typedef struct packed {   // Packed structure of a composed segment pointer that consists of segment pointer and semaphore
    seg_ptr_t           ptr;
    seg_sem_t           sem;
  } seg_ptr_handle_str_t;
  
  typedef union packed {    // Union of composed segment pointer for accessing any bit or any field
    cmp_seg_ptr_pkd_t       seg_pkd;
    seg_ptr_handle_str_t    seg_str;
  } seg_ptr_handle_td; 

  typedef struct packed {   // Packed structure of a pod pointer ring packet 
    seg_ptr_handle_str_t    pod_ptr;
    dstid_ring_pkt_t        dstid_ring_pkt;
    val_ring_pkt_t          val_ring_pkt;
    cd_ring_pkt_t           cd_ring_pkt;
    stall_ring_pkt_t        stall_ring_pkt;
    spare_ring_pkt_t        spare_ring_pkt;
  } ring_pkt_str_t;
 
  typedef struct packed {   //Packet structure of pod pointer ring packet with ECC
    ring_pkt_str_t      ring_pkt;
    ecc_ring_pkt_t      ecc_ring_pkt;
  } cmp_ring_pkt_str_t;
  
  typedef union packed {
    cmp_ring_pkt_pkd_t  ring_pkt_pkd;
    cmp_ring_pkt_str_t  ring_pkt_str;
  } cmp_ring_pkt_td; 

endpackage // gmm_pkg

`endif //MBY_GMM_PKG_SV

   