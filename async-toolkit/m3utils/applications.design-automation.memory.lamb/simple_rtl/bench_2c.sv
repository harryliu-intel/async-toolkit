`include "cdp_lamb_1r1w2c_128d_144b.sv"

module bench_2c #() ();

   localparam CYCLETIME = 1000; // 1000 ps cycle
   
   localparam DEPTH=128;
   localparam DWIDTH=144;
   localparam AWIDTH=$clog2(DEPTH);

//   localparam verbose = 0;
   localparam verbose = 1;
   localparam ITERS = 1000000;

   logic first;
   
   logic wclk;
   logic rclk;
   logic wen;
   logic ren;
   logic [AWIDTH-1:0] radr;
   logic [AWIDTH-1:0] wadr;
   logic [DWIDTH-1:0] wdata;
   logic [DWIDTH-1:0] dout;
   
   logic              test__scan_en;
   logic [1:0]        dft__core_si;
   logic              icg_force_on;
   logic              dft_read_bypass;
   logic              dft__mem_wr_disable;
   logic [1:0]        dft__core_so;

   //////////////////////////////////////////////////////////////////////
   //
   // D.U.T.
   //
   cdp_lamb_1r1w2c_128d_144b #(DEPTH, DWIDTH, AWIDTH) dut(.*);

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
   logic              rw_conflict_q;
   
   initial begin : init_init
      for (int i=0; i<DEPTH; ++i)
        meminit[i] = '0;
   end

   //////////////////////////////////////////////////////////////////////
   //
   // read input generation
   //
   initial begin : read_block
      logic first;
      
      first = '1;
      
      repeat (ITERS) begin
         @(negedge rclk);

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
              assert(rw_conflict_q || (dout == mem[radr])) 
                else 
                  $display("Assert failed: dout = %h != mem[radr = %h] = %h; rw_conflict_q = %h", dout, radr, mem[radr], rw_conflict_q);
            else
              assert(dout === {DWIDTH{1'bx}});                
         end

         ////////////////////////////////////////////////////////////
         //
         // update inputs for next step
         //

         ren   = $urandom();
         radr  = $urandom();
         first = '0;
      end

      repeat(10) @(posedge rclk);
      
      $stop();
   end // block: read_block

   //////////////////////////////////////////////////////////////////////
   //
   // WRITE SIDE
   //
   initial begin : write_block

      while(1) begin
         @(negedge wclk);

         ////////////////////////////////////////////////////////////
         //
         // update inputs for next step
         //

         wen   = $urandom();
         wadr  = $urandom();
                 
         for(int b = 0; b < DWIDTH; b += 32)
           wdata[b +: 32] = $urandom();

         ////////////////////////////////////////////////////////////
         //
         // update model of hardware
         // careful w/ timing in 2-clock case
         //
         @(posedge wclk);

         if (verbose)
           $display("mem[%3h] <-(%1h)? %36h", wadr, wen, wdata);

         
         if (wen) begin
            mem    [wadr] = wdata;
            meminit[wadr] = '1;
         end
      end
   end // block: write_block
   
   //////////////////////////////////////////////////////////////////////
   //
   // clock generator(s)
   //
   logic clk; // master clock
    
   initial begin : gen_clk
      clk = '0;

      while (1) begin
         #(CYCLETIME/4.0);

         clk = ~clk;
      end
   end

   initial begin : gen_wclk
      wclk = '0;

      while (1) begin
         @(posedge clk);
         
         if ($urandom() & 1)
           wclk = ~wclk;
      end
   end

   initial begin : gen_rclk
      rclk = '0;

      while (1) begin
         @(posedge clk);
         
         if ($urandom() & 1)
           rclk = ~rclk;
      end
   end

  // conflict checker
  logic [AWIDTH-1:0]            active_radr, active_wadr;
  logic                         ractive, wactive;
  logic                         rw_conflict;

  always_ff @(posedge rclk) begin
     // update conflict state
     if (ren) begin
        ractive <= '1;
        active_radr <= radr;
     end
     else
       ractive <= '0;
  end
  always_ff @(posedge wclk) begin
    // update conflict state
    if (wen) begin 
       wactive <= '1;
       active_wadr <= wadr;
    end
    else
      wactive <= '0;
  end

  assign rw_conflict = (wactive && ractive && (active_radr == active_wadr));

  always_ff @(posedge rclk or posedge rw_conflict) begin
     if (ren || rw_conflict)
       rw_conflict_q <= rw_conflict;
     else
       rw_conflict_q <= '0;
  end
     

endmodule // lamb_tb
