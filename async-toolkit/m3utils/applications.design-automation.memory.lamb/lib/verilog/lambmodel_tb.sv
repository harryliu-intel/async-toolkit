`resetall
`default_nettype none

`include "latchblk.sv"
`include "lambmodel.sv"

module lambmodel_tb #() ();

   localparam CYCLETIME = 1000; // 1000 ps cycle
   
   localparam DEPTH=128;
   localparam DWIDTH=144;
   localparam AWIDTH=$clog2(DEPTH);

   localparam verbose = 0;
//   localparam verbose = 1;
   localparam ITERS = 10000000;

   logic first;
   
   logic clk;
   logic wen;
   logic ren;
   logic [AWIDTH-1:0] radr;
   logic [AWIDTH-1:0] wadr;
   logic [DWIDTH-1:0] wdata;
   logic [DWIDTH-1:0] dout;
   
   logic              test__scan_en;
   logic              dft__core_si;
   logic              icg_force_on;
   logic              dft_read_bypass;
   logic              dft__mem_wr_disable;
   logic              dft__core_so;

   //////////////////////////////////////////////////////////////////////
   //
   // D.U.T.
   //
   lambmodel #(DEPTH, DWIDTH, AWIDTH) dut(.*);

   //////////////////////////////////////////////////////////////////////
   //
   // DFT disable
   //
   assign test__scan_en       = '0;
   assign dft_core_si         = '0;
   assign icg_force_on        = '0;
   assign dft_read_bypass     = '0;
   assign dft__mem_wr_disable = '0;

   
   //////////////////////////////////////////////////////////////////////
   //
   // model state
   //
   logic [DWIDTH-1:0] mem [DEPTH];     // mem model
   logic              meminit [DEPTH]; // init of addr

   initial begin : init_init
      for (int i=0; i<DEPTH; ++i)
        meminit[i] = '0;
   end

   //////////////////////////////////////////////////////////////////////
   //
   // clock generator
   //
   initial begin : gen_clk
      clk = '0;

      while (1) begin
         #(CYCLETIME/2.0);

         clk = ~clk;
      end
   end

   //////////////////////////////////////////////////////////////////////
   //
   // input generation
   //
   initial begin : gen_inputs
      first = '1;
      
      repeat (ITERS) begin
         @(negedge clk);

         ////////////////////////////////////////////////////////////
         //
         // check previous iteration
         //
         
         if (verbose)
           $display("dout[%3h] (%1h)? = %36h meminit=%1h mem[] = %36h", 
                    radr, 
                    ren, 
                    dout, 
                    meminit[radr], 
                    mem[radr]); // 144 bits

         if (!first && !ren)
           assert (dout === {DWIDTH{1'bx}});

         if (!first && ren) begin
            if (meminit[radr])
              assert(dout == mem[radr]);
            else
              assert(dout === {DWIDTH{1'bx}});                
         end

         // the following property follows from the below code, but we
         // assert it just to be clear that the behavior on conflict is NEW

         if (!first && ren && wen && (radr == wadr))
           assert (dout == wdata);
         
         ////////////////////////////////////////////////////////////
         //
         // update inputs for next step
         //
         
         wen   = $urandom();
         ren   = $urandom();
         radr  = $urandom();
         wadr  = $urandom();
         first = '0;
                 
         for(int b = 0; b < DWIDTH; b += 32)
           wdata[b +: 32] = $urandom();

         ////////////////////////////////////////////////////////////
         //
         // update model of hardware
         //
         if (verbose)
           $display("mem[%3h] <-(%1h)? %36h", wadr, wen, wdata);
         
         if (wen) begin
            mem    [wadr] = wdata;
            meminit[wadr] = '1;
         end
                    
      end

      repeat(10) @(posedge clk);

      $stop();
   end
   
endmodule // lamb_tb


`default_nettype wire
