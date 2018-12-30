interface  egr_tqu_mem_if ();

// EPL EOP FIFO
localparam EPL_EOP_FIFO_W_DATA =  14;     // Data width
localparam EPL_EOP_FIFO_W_DEEP = 384;     // Address width
localparam EPL_EOP_FIFO_W_INST =   4;     // Number of instances of this memory
    

logic [$clog2(EPL_EOP_FIFO_W_DEEP)-1:0]   epl_eop_fifo_rd_adr    [EPL_EOP_FIFO_W_INST-1:0] ; 
logic                                     epl_eop_fifo_rd_en     [EPL_EOP_FIFO_W_INST-1:0] ; 
logic [$clog2(EPL_EOP_FIFO_W_DEEP)-1:0]   epl_eop_fifo_wr_adr    [EPL_EOP_FIFO_W_INST-1:0] ; 
logic         [EPL_EOP_FIFO_W_DATA-1:0]   epl_eop_fifo_wr_data   [EPL_EOP_FIFO_W_INST-1:0] ; 
logic                                     epl_eop_fifo_wr_en     [EPL_EOP_FIFO_W_INST-1:0] ; 
logic         [EPL_EOP_FIFO_W_DATA-1:0]   epl_eop_fifo_rd_data   [EPL_EOP_FIFO_W_INST-1:0] ; 
logic                                     epl_eop_fifo_rd_valid  [EPL_EOP_FIFO_W_INST-1:0] ;


// EPL PKTID FIFO
localparam EPL_PKTID_FIFO_W_DATA =   9;     // Data width
localparam EPL_PKTID_FIFO_W_DEEP = 384;     // Address width
localparam EPL_PKTID_FIFO_W_INST =   4;     // Number of instances of this memory
    

logic [$clog2(EPL_PKTID_FIFO_W_DEEP)-1:0] epl_pktid_fifo_rd_adr    [EPL_PKTID_FIFO_W_INST-1:0] ; 
logic                                     epl_pktid_fifo_rd_en     [EPL_PKTID_FIFO_W_INST-1:0] ; 
logic [$clog2(EPL_PKTID_FIFO_W_DEEP)-1:0] epl_pktid_fifo_wr_adr    [EPL_PKTID_FIFO_W_INST-1:0] ; 
logic         [EPL_PKTID_FIFO_W_DATA-1:0] epl_pktid_fifo_wr_data   [EPL_PKTID_FIFO_W_INST-1:0] ; 
logic                                     epl_pktid_fifo_wr_en     [EPL_PKTID_FIFO_W_INST-1:0] ; 
logic         [EPL_PKTID_FIFO_W_DATA-1:0] epl_pktid_fifo_rd_data   [EPL_PKTID_FIFO_W_INST-1:0] ; 
logic                                     epl_pktid_fifo_rd_valid  [EPL_PKTID_FIFO_W_INST-1:0] ;

// EPL SOP FIFO
localparam EPL_SOP_FIFO_W_DATA =  24;     // Data width
localparam EPL_SOP_FIFO_W_DEEP = 384;     // Address width
localparam EPL_SOP_FIFO_W_INST =   4;     // Number of instances of this memory
    

logic [$clog2(EPL_SOP_FIFO_W_DEEP)-1:0]   epl_sop_fifo_rd_adr    [EPL_SOP_FIFO_W_INST-1:0] ; 
logic                                     epl_sop_fifo_rd_en     [EPL_SOP_FIFO_W_INST-1:0] ; 
logic [$clog2(EPL_SOP_FIFO_W_DEEP)-1:0]   epl_sop_fifo_wr_adr    [EPL_SOP_FIFO_W_INST-1:0] ; 
logic         [EPL_SOP_FIFO_W_DATA-1:0]   epl_sop_fifo_wr_data   [EPL_SOP_FIFO_W_INST-1:0] ; 
logic                                     epl_sop_fifo_wr_en     [EPL_SOP_FIFO_W_INST-1:0] ; 
logic         [EPL_SOP_FIFO_W_DATA-1:0]   epl_sop_fifo_rd_data   [EPL_SOP_FIFO_W_INST-1:0] ; 
logic                                     epl_sop_fifo_rd_valid  [EPL_SOP_FIFO_W_INST-1:0] ;

// SMP ROB PTR BANK
localparam SMP_ROB_PTR_BANK_FIFO_W_DATA =  12;     // Data width
localparam SMP_ROB_PTR_BANK_FIFO_W_DEEP = 114;     // Address width
localparam SMP_ROB_PTR_BANK_FIFO_W_INST = 432;     // Number of instances of this memory
    

logic [$clog2(SMP_ROB_PTR_BANK_FIFO_W_DEEP)-1:0]   smp_rob_ptr_bank_rd_adr    [SMP_ROB_PTR_BANK_FIFO_W_INST-1:0] ; 
logic                                              smp_rob_ptr_bank_rd_en     [SMP_ROB_PTR_BANK_FIFO_W_INST-1:0] ; 
logic [$clog2(SMP_ROB_PTR_BANK_FIFO_W_DEEP)-1:0]   smp_rob_ptr_bank_wr_adr    [SMP_ROB_PTR_BANK_FIFO_W_INST-1:0] ; 
logic         [SMP_ROB_PTR_BANK_FIFO_W_DATA-1:0]   smp_rob_ptr_bank_wr_data   [SMP_ROB_PTR_BANK_FIFO_W_INST-1:0] ; 
logic                                              smp_rob_ptr_bank_wr_en     [SMP_ROB_PTR_BANK_FIFO_W_INST-1:0] ; 
logic         [SMP_ROB_PTR_BANK_FIFO_W_DATA-1:0]   smp_rob_ptr_bank_rd_data   [SMP_ROB_PTR_BANK_FIFO_W_INST-1:0] ; 
logic                                              smp_rob_ptr_bank_rd_valid  [SMP_ROB_PTR_BANK_FIFO_W_INST-1:0] ;

// EPL SMP TB
localparam EPL_SMP_TB_W_DATA = 512;     // Data width
localparam EPL_SMP_TB_W_DEEP = 512;     // Address width
localparam EPL_SMP_TB_W_INST =  32;     // Number of instances of this memory
   
logic [$clog2(EPL_SMP_TB_W_DEEP)-1:0]   epl_smp_tb_adr       [EPL_SMP_TB_W_INST-1:0] ; 
logic                                   epl_smp_tb_rd_en     [EPL_SMP_TB_W_INST-1:0] ; 
logic [EPL_SMP_TB_W_DATA-1:0]           epl_smp_tb_wr_data   [EPL_SMP_TB_W_INST-1:0] ; 
logic                                   epl_smp_tb_wr_en     [EPL_SMP_TB_W_INST-1:0] ; 
logic [EPL_SMP_TB_W_DATA-1:0]           epl_smp_tb_rd_data   [EPL_SMP_TB_W_INST-1:0] ; 
logic                                   epl_smp_tb_rd_valid  [EPL_SMP_TB_W_INST-1:0] ;

modport mem (
             // EPL EOP FIFO
             input   epl_eop_fifo_rd_adr  ,   
             input   epl_eop_fifo_rd_en   ,
             input   epl_eop_fifo_wr_adr  ,   
             input   epl_eop_fifo_wr_data ,
             input   epl_eop_fifo_wr_en   ,
             output  epl_eop_fifo_rd_data ,
             output  epl_eop_fifo_rd_valid,
             // EPL PKTID FIFO
             input   epl_pktid_fifo_rd_adr  ,   
             input   epl_pktid_fifo_rd_en   ,
             input   epl_pktid_fifo_wr_adr  ,   
             input   epl_pktid_fifo_wr_data ,
             input   epl_pktid_fifo_wr_en   ,
             output  epl_pktid_fifo_rd_data ,
             output  epl_pktid_fifo_rd_valid,
             // EPL SOP FIFO
             input   epl_sop_fifo_rd_adr  ,   
             input   epl_sop_fifo_rd_en   ,
             input   epl_sop_fifo_wr_adr  ,   
             input   epl_sop_fifo_wr_data ,
             input   epl_sop_fifo_wr_en   ,
             output  epl_sop_fifo_rd_data ,
             output  epl_sop_fifo_rd_valid,
             // EPL ROB PTR BANK FIFO
             input   smp_rob_ptr_bank_rd_adr  ,   
             input   smp_rob_ptr_bank_rd_en   ,
             input   smp_rob_ptr_bank_wr_adr  ,   
             input   smp_rob_ptr_bank_wr_data ,
             input   smp_rob_ptr_bank_wr_en   ,
             output  smp_rob_ptr_bank_rd_data ,
             output  smp_rob_ptr_bank_rd_valid,
             // EPL SMP TB
             input   epl_smp_tb_adr      ,
             input   epl_smp_tb_rd_en    ,
             input   epl_smp_tb_wr_data  ,
             input   epl_smp_tb_wr_en    ,
             output  epl_smp_tb_rd_data  ,
             output  epl_smp_tb_rd_valid  

);

modport cln (
             // EPL EOP FIFO
             output  epl_eop_fifo_rd_adr  ,   
             output  epl_eop_fifo_rd_en   ,
             output  epl_eop_fifo_wr_adr  ,   
             output  epl_eop_fifo_wr_data ,
             output  epl_eop_fifo_wr_en   ,
             input   epl_eop_fifo_rd_data ,
             input   epl_eop_fifo_rd_valid,
             // EPL PKTID FIFO
             output  epl_pktid_fifo_rd_adr  ,   
             output  epl_pktid_fifo_rd_en   ,
             output  epl_pktid_fifo_wr_adr  ,   
             output  epl_pktid_fifo_wr_data ,
             output  epl_pktid_fifo_wr_en   ,
             input   epl_pktid_fifo_rd_data ,
             input   epl_pktid_fifo_rd_valid,
             // EPL SOP FIFO
             output  epl_sop_fifo_rd_adr  ,   
             output  epl_sop_fifo_rd_en   ,
             output  epl_sop_fifo_wr_adr  ,   
             output  epl_sop_fifo_wr_data ,
             output  epl_sop_fifo_wr_en   ,
             input   epl_sop_fifo_rd_data ,
             input   epl_sop_fifo_rd_valid,
             // EPL ROB PTR BNK FIFO
             output  smp_rob_ptr_bank_rd_adr  ,   
             output  smp_rob_ptr_bank_rd_en   ,
             output  smp_rob_ptr_bank_wr_adr  ,   
             output  smp_rob_ptr_bank_wr_data ,
             output  smp_rob_ptr_bank_wr_en   ,
             input   smp_rob_ptr_bank_rd_data ,
             input   smp_rob_ptr_bank_rd_valid,
             // EPL SMP TB
             output  epl_smp_tb_adr      ,
             output  epl_smp_tb_rd_en    ,
             output  epl_smp_tb_wr_data  ,
             output  epl_smp_tb_wr_en    ,
             input   epl_smp_tb_rd_data  ,
             input   epl_smp_tb_rd_valid  

);
    
    
endinterface:egr_tqu_mem_if

