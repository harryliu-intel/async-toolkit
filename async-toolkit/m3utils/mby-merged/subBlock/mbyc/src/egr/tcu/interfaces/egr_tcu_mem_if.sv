interface egr_tcu_mem_if #(
parameter W_DATA = 512,     // Data width
parameter W_DEEP =  72,     // Address width
parameter W_CHNK =  64,     // Number of chunks when you use data write resolution, useful, when you want to write sections of a large data width
parameter W_INST =  48      // Number of instances of this memory
    ) ();
    
//TODO: create another dimension (creating a memory)
//TODO:crate chunk write enable.

   logic [$clog2(W_DEEP)-1:0]   rd_adr    [W_INST-1:0] ; 
   logic                        rd_en     [W_INST-1:0] ; 
   logic [$clog2(W_DEEP)-1:0]   wr_adr    [W_INST-1:0] ; 
   logic [W_DATA-1:0]           wr_data   [W_INST-1:0] ; 
   logic                        wr_en     [W_INST-1:0] ; 
   logic [W_DATA-1:0]           rd_data   [W_INST-1:0] ; 
   logic                        rd_valid  [W_INST-1:0] ;
   logic [W_CHNK-1:0]           wr_bwe    [W_INST] ; 
    
modport mem (
input   rd_adr  ,   
input   rd_en   ,
input   wr_adr  ,   
input   wr_bwe  ,
input   wr_data ,
input   wr_en   ,
output  rd_data ,
output  rd_valid
);

modport cln (
output  rd_adr  ,   
output  rd_en   ,
output  wr_adr  ,   
output  wr_bwe  ,
output  wr_data ,
output  wr_en   ,
input   rd_data ,
input   rd_valid
    );
    
    
endinterface

