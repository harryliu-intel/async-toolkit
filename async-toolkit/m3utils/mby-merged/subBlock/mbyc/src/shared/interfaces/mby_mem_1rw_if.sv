interface mby_mem_1rw_if #(
parameter W_DATA = 176,     // Data width
parameter W_DEEP = 128,     // Address width
//parameter W_CHNK =   2,     // Number of chunks when you use data write resolution, useful, when you want to write sections of a large data width
parameter W_INST =  16      // Number of instances of this memory
) ();
//Port List
//TODO: create another dimension
//TODO:crate chunk write enable.

   logic [$clog2(W_DEEP)-1:0]   adr       [W_INST-1:0] ; 
   logic                        rd_en     [W_INST-1:0] ; 
   //logic [W_CHNK-1:0]         wr_bwe    [W_INST-1:0] ; 
   logic [W_DATA-1:0]           wr_data   [W_INST-1:0] ; 
   logic                        wr_en     [W_INST-1:0] ; 
   logic [W_DATA-1:0]           rd_data   [W_INST-1:0] ; 
   logic                        rd_valid  [W_INST-1:0] ;


modport mem (
input   adr      , 
input   rd_en    , 
//input   wr_bwe   , 
input   wr_data  , 
input   wr_en    , 
output  rd_data  , 
output  rd_valid 
);

modport cln (
output  adr     , 
output  rd_en   ,
//output  wr_bwe  ,
output  wr_data ,
output  wr_en   ,
input   rd_data ,
input   rd_valid
);

endinterface

