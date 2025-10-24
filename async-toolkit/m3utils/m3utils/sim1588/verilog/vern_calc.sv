// calculator

typedef enum logic [2:0] {
  CALC_OP_ADD=3'h0, // twos-complement add; output sum
  CALC_OP_SUB=3'h1, // twos-complement subtract; output difference
  CALC_OP_MUL=3'h2, // uns mul; output prod_LO; prod_HI
  CALC_OP_DIV=3'h3, // uns div, dividend->X, divisor->Y; output quotient; rem
  CALC_OP_GCD=3'h4, // uns GCD; output GCD
  CALC_OP_LCM=3'h5, // uns LCM; output LCM_LO; LCM_HI
  CALC_OP_SQT=3'h6  // SQRT((Y<<BITS)+X) ; output square root; remainder   
} vern_calc_cmd_t;

module vern_calc
  #(parameter BITS=  32)
   (
    input  logic                        clk,
    input  logic                        rst_n,

    input  logic [BITS-1:0]             i_x, 
    input  logic [BITS-1:0]             i_y,
    input  vern_calc_cmd_t              i_op,
    input  logic                        i_start_v,
    output logic                        o_ready_e,

    output logic [BITS-1:0]             o_result,
    output logic                        o_done_v,
    input  logic                        i_output_e
    );

  localparam NUM_REGS=9; // 8 flop registers, one always zero, total 9 addr.
  localparam ID_BITS=$clog2(NUM_REGS);
  localparam POS_BITS=$clog2(BITS);

  typedef enum logic [2:0] {
    INPUT_REG_NOP     =3'h0, // No OPeration
    INPUT_REG_SHL     =3'h1, // shift left one bit (bit to shift in given)
    INPUT_REG_SHR     =3'h2, // shift right one bit
    INPUT_REG_SHW     =3'h3, // shift left one word less one bit
    INPUT_REG_CLR     =3'h4, // clear
    INPUT_REG_SQTSTEP =3'h5  // square root step
  } vern_calc_inp_cmd_t;
  
  vern_calc_cmd_t curop_d, curop_q;

  logic done_v_d;

  typedef enum logic [4:0] { // the states of the FSM
    PH_RDY          = 5'h0,  // waiting for input
    PH_START        = 5'h1,  // parse input
    PH_ADD1         = 5'h2,
    PH_SUB1         = 5'h3,
    PH_MULLOOP      = 5'h4,  // multiply loop
    PH_MULOUT       = 5'h5,                            
    PH_DIVLOOP      = 5'h6,  // divide (inner) loop
    PH_DIVOUT       = 5'h7,
    PH_OUP2B0       = 5'h8,
    PH_OUP2A0       = 5'h9,
    PH_OUPA         = 5'ha,  // output accumulator
    PH_OUPW         = 5'hb,  // wait for final output handshake
    PH_GCD1         = 5'hc,
    PH_GCDLOOP      = 5'hd,  // GCD (outer) loop
    PH_LCM1         = 5'he,  // LCM setup (before GCD)
    PH_LCM2         = 5'hf,
    PH_LCMCODA0     = 5'h10, // LCM cleanup (after GCD)
    PH_LCMCODA1     = 5'h11,
    PH_OUPY         = 5'h12, // output Y register                            
    PH_SWAP2        = 5'h13,                            
    PH_GCDSWAP1     = 5'h14,                            
    PH_GCDSWAP2     = 5'h15,                            
    PH_SQT1         = 5'h16,                            
    PH_SQT2         = 5'h17,                            
    PH_SQTLOOP      = 5'h18  // square root loop                           
  } phase_t;

  phase_t curph_d, curph_q;
  phase_t divret_d, divret_q; // return address for divide
               
  always_ff @(posedge clk or negedge rst_n) begin
    curop_q <= rst_n ? curop_d : CALC_OP_ADD;  // .DC.
    curph_q <= rst_n ? curph_d : PH_RDY;       // this matters
    o_done_v <= rst_n ? done_v_d : 0;
    divret_q <= rst_n ? divret_d : PH_DIVLOOP; // force loop on incorrect entry
  end

  assign o_ready_e = (curph_q == PH_RDY);

  /////////////////////   REGISTERS   //////////////////////////////////

    typedef enum logic [3:0] {
    REG_ZERO = 4'h0,
    REG_X    = 4'h1, //     input X  (counter)
    REG_Y_LO = 4'h2, //     input Y  (carriage)
    REG_Y_HI = 4'h3,
    REG_A_LO = 4'h4, //     accumulator
    REG_A_HI = 4'h5, 
    REG_Z    = 4'h6, // 6    output Z (output)
    REG_S0   = 4'h7, // 7-8  scratch  
    REG_S1   = 4'h8  
} reg_idx_t;

  //
  // there are three main working registers:
  //
  // the "accumulator" A, which holds working sums and differences
  //
  // the "carriage" Y, which is directly connected to the accumulator,
  // and holds augends and subtrahends, as well as the divisor
  //
  // the "counter" X, which mainly is used for counting iterations
  //
  // A and Y are double width.  X is single width.
  //
  // The basic design is modeled on a pinwheel calculator (e.g., Odhner)
  //
  // registers X and Y are loaded from the environment.
  //
  // a final register Z is used to produce output.
  //
  // a single width bus is provided to transfer data from any
  // register to any register (incl. the input and output registers)
  //
  // finally there are two scratch registers, S0 and S1, which
  // are needed for the LCM.
  //
  // the only funky datapath is for the square root.  The rest of
  // the operations can use entirely the same datapath.  For the
  // square root, there are muxes that can set and clear individual
  // bits of Y.  I'm not 100% sure this part of the design is correct,
  // basically there is a bit of trial and error in the formula for
  // which bits to set and clear, rather than clear and concise math.
  //
  // The design is obviously parameterized and should work for any
  // setting of the BITS parameter.
  //
  // The number of flops scales as k * BITS + m * log(BITS) + n
  // k is roughly 8, m is roughly 5, n is roughly 10 (I think, haven't
  // checked carefully).  So best guess:
  //
  // flops \approx 8 * BITS + 5 * log(BITS) + 10
  //
  // A 64-bit word size, then, has about 550 flops.
  //
  // Author: Mika Nystrom <mika.nystroem@intel.com>
  // January, 2018
  //
  
  logic [BITS-1:0]                      x   [NUM_REGS-1:0],
                                        x_d [NUM_REGS-1:1];

  logic [NUM_REGS-1:0]                  inhibit; // inhibit write

  generate
    // note that reg 0 is constant
    // this code is needlessly complicated and general given
    // that only the A register uses the feature at all
    for (genvar ii=REG_ZERO+1; ii < NUM_REGS; ++ii) begin:gen_x_reg
      always_ff @(posedge clk or negedge rst_n)
        if (~inhibit[ii]) x[ii] <= rst_n ? x_d[ii] : '0;
    end
  endgenerate

  //////////////////////////////////////////////////////////////////////
         
  logic [BITS-1:0]                      bus; // the data bus
  logic [ID_BITS-1:0]                   rreg;
  logic [ID_BITS-1:0]                   wreg; 
  
  assign bus = x[rreg];

  // the following are aliases for the registers to make the code
  // clearer
  logic [  BITS-1:0]                    zeroreg;
  logic [  BITS-1:0]                    xreg;
  logic [2*BITS-1:0]                    yreg;
  logic [2*BITS-1:0]                    areg;
  logic [  BITS-1:0]                    zreg;

  assign zeroreg =  x[REG_ZERO];
  assign xreg    =  x[REG_X];
  assign yreg    = {x[REG_Y_HI],x[REG_Y_LO]};
  assign areg    = {x[REG_A_HI],x[REG_A_LO]};
  assign zreg    =  x[REG_Z];

  assign o_result = zreg;

  //////////////////////////////////////////////////////////////////////
  // register update logic

`define BUSWRITE(idx) begin x_d[idx] = x[idx]; if (wreg==(idx)) x_d[idx] = bus; end // must be first

  assign x[REG_ZERO] = '0;

  // X register

  vern_calc_inp_cmd_t opx;
  logic x_shin;
  
  always_comb begin
    `BUSWRITE(REG_X);

    casez (opx)
      INPUT_REG_SHL:
        x_d[REG_X] = {xreg[BITS-2:0],x_shin};
      INPUT_REG_SHR:
        x_d[REG_X] = {1'b0,xreg[BITS-1:1]};
      INPUT_REG_CLR:
        x_d[REG_X] = '0;
      default: 
        ; // skip
    endcase

    if (curph_q == PH_RDY) x_d[REG_X] = i_x;

  end
  
  // Y register

  // square root counter (for Y)
  logic [POS_BITS+2-1:0] sqtctr_d, sqtctr_q;

  always_ff @(posedge clk or negedge rst_n) begin
    sqtctr_q <= rst_n ? sqtctr_d : 2*BITS;
  end
  
  vern_calc_inp_cmd_t opy;

`define yreg_d  {x_d[REG_Y_HI],x_d[REG_Y_LO]}

  logic ystepdir;
  logic [2*BITS-1:0] yreg_s; // square root reg
  
  generate
    // there MUST be a clearer way of writing this code
    // Im not 100% certain its right, either
    for (genvar yi=0; yi<2*BITS; ++yi) begin : shift_and_extend
      assign yreg_s[yi] = (yi == sqtctr_q+1) ? ystepdir : 
                          (((yi == sqtctr_q)) ? 0 : ((yi == sqtctr_q-1) ? 1 :yreg[yi]));
    end
  endgenerate

  always_comb begin
    `BUSWRITE(REG_Y_LO);
    `BUSWRITE(REG_Y_HI);

    casez (opy)
      INPUT_REG_SHL:
        `yreg_d = {yreg[2*BITS-2:0],1'b0};
      INPUT_REG_SHR:
        `yreg_d = {1'b0,yreg[2*BITS-1:1]};
      INPUT_REG_SHW:
        `yreg_d = {1'b0 , yreg[BITS-1:0] , zeroreg[BITS-2:0]};
      INPUT_REG_CLR:
        `yreg_d = '0;
      INPUT_REG_SQTSTEP: begin
        `yreg_d = {1'b0,yreg_s[2*BITS-1:1]};
      end
      default: 
        ; // skip
    endcase

    if (curph_q == PH_RDY) 
      `yreg_d = {zeroreg,i_y};

  end

  // ACCUMULATOR
  logic opa_do,   // update reg
        opa_sub,  // subtract
        opa_clr,  // clear
        opa_exec; // connect arithmetic logic
  
 `define areg_d  {x_d[REG_A_HI],x_d[REG_A_LO]}

  logic [2*BITS-1+1:0] sum;
  
  logic                carry_out;
  assign carry_out = sum [2*BITS-1+1];

  logic              minus;
  logic [2*BITS-1:0] augend;
  
  assign augend = minus ? ~yreg : yreg;

  assign sum    = augend + areg + minus;

  assign minus = (opa_sub);

  logic              zeroflag;
  assign zeroflag = (areg == 0);

  always_comb begin
    `BUSWRITE(REG_A_LO);
    `BUSWRITE(REG_A_HI);

    if (opa_exec) `areg_d = sum; 

    if (opa_clr) `areg_d = '0;
  end

  // the rest of the registers have no special functions
  always_comb begin
    `BUSWRITE(REG_Z);
    `BUSWRITE(REG_S0);
    `BUSWRITE(REG_S1);
  end

  //////////////////////////////////////////////////////////////////////    
  // control state machine

  // multiply, divide counter
  logic [POS_BITS-1:0] cnt_d, cnt_q, cnt_q_m1;

  assign cnt_q_m1 = cnt_q - 1;
  
  always_ff @(posedge clk or negedge rst_n) begin
    cnt_q <= rst_n ? cnt_d : BITS-1;
  end

  always_comb begin
    done_v_d = 0;
    curop_d = curop_q;
    curph_d = curph_q;

    // X input register controls
    opx = INPUT_REG_NOP;
    x_shin = 0;

    // Y input register controls
    opy = INPUT_REG_NOP;
    ystepdir = 0;

    // accumulator controls
    opa_do = 0;
    opa_sub = 0;
    opa_clr = 0;
    opa_exec = 0;

    // counter controls
    cnt_d = cnt_q;
    sqtctr_d = sqtctr_q;
    
    // bus controls
    wreg = REG_ZERO;
    rreg = REG_ZERO;
    inhibit = '0;

    // division return address register
    divret_d = divret_q;
    
    casez (curph_q)
      
      PH_RDY: begin
        // clear A
        opa_do = 1;
        opa_clr = 1;

        // clear Y hi
        wreg = REG_Y_HI;

        if (i_start_v) begin
          curop_d = i_op;
          curph_d = PH_START;
        end
      end

      PH_START: begin
        casez (curop_q)
          CALC_OP_ADD: begin
            // copy x reg into accumulator-lo
            rreg = REG_X;
            wreg = REG_A_LO;
            curph_d = PH_ADD1;
          end
          CALC_OP_SUB: begin
            // copy x reg into accumulator-lo
            rreg = REG_X;
            wreg = REG_A_LO;
            curph_d = PH_SUB1;
          end
          CALC_OP_MUL: begin
            curph_d = PH_MULLOOP;
          end
          CALC_OP_DIV: begin
            opy = INPUT_REG_SHW;
            rreg = REG_X;
            wreg = REG_A_LO;
            curph_d = PH_DIVLOOP;
            divret_d = PH_DIVOUT;
          end
          CALC_OP_GCD: begin
            rreg = REG_X;     // A <- X
            wreg = REG_A_LO;
            curph_d = PH_GCD1;
            divret_d = PH_GCDLOOP;
          end
          CALC_OP_LCM: begin
            rreg = REG_X;     // A <- X
            wreg = REG_A_LO;
            curph_d = PH_LCM1;
            divret_d = PH_GCDLOOP;
          end
          CALC_OP_SQT: begin
            rreg = REG_Y_LO;
            wreg = REG_A_HI;
            curph_d = PH_SQT1;
            divret_d = PH_DIVOUT;
          end
        endcase
      end

      PH_SQT1: begin
        opy = INPUT_REG_CLR;
        rreg = REG_X;
        wreg = REG_A_LO;
        curph_d = PH_SQT2;
      end

      PH_SQT2: begin
        opx = INPUT_REG_CLR;
        curph_d = PH_SQTLOOP;
        opy = INPUT_REG_SQTSTEP;
        ystepdir = 0; // fail at pos 2*BITS
        sqtctr_d = sqtctr_q - 2;
      end

      PH_SQTLOOP: begin
        // invariant: next test subtrahend in Y
        // bit position of active bit in sqtctr_q
        opa_sub = 1;
        opa_exec = 1;
        opa_do = carry_out;
        x_shin = carry_out;
        opx = INPUT_REG_SHL;
        if (sqtctr_q == 0) begin
          sqtctr_d = 2*BITS;
          curph_d = divret_q;
        end else begin
          opy = INPUT_REG_SQTSTEP;
          ystepdir = carry_out; // carry 0 is fail sub, 1 is success
          sqtctr_d = sqtctr_q - 2;
        end
      end
      
      // for LCM, we set up machine same as GCD, except we
      // save alpha and beta (init operands) in S0 and S1
      PH_LCM1: begin // S0 <- X
        rreg = REG_X;
        wreg = REG_S0;
        curph_d = PH_LCM2;
      end

      PH_LCM2: begin // S1 <- Y
        rreg = REG_Y_LO;
        wreg = REG_S1;
        curph_d = PH_GCD1;
      end
                  
      PH_GCD1: begin // compare A=X against Y
        opa_sub = 1;
        opa_exec = 1;
        if (carry_out) begin // no borrow, Y < X
          opy = INPUT_REG_SHW;
          curph_d = PH_DIVLOOP; // jump to divide
        end else begin
          rreg = REG_Y_LO;
          wreg = REG_X; // X is scratch
          curph_d = PH_GCDSWAP1; // jump to swap (then to divide)
        end
      end

      PH_GCDLOOP: begin
        // return after a GCD division
        // state is that we have the two numbers in Y and A and A<Y
        // if A is zero, return Y, else swap A and Y and continue
        if      (zeroflag & (curop_q == CALC_OP_GCD))
          curph_d = PH_OUPY;
        else if (zeroflag & (curop_q == CALC_OP_LCM))
          curph_d = PH_LCMCODA0;
        else begin
          rreg = REG_Y_LO;
          wreg = REG_X; // X is scratch
          curph_d = PH_GCDSWAP1;
        end
      end

      PH_GCDSWAP1: begin // Y <- A
        rreg = REG_A_LO;
        wreg = REG_Y_LO;
        curph_d = PH_GCDSWAP2;
      end

      PH_GCDSWAP2: begin // A <- X
        rreg = REG_X;
        wreg = REG_A_LO;
        opy = INPUT_REG_SHW;
        curph_d = PH_DIVLOOP; 
        // NB X is not cleared, but that's OK, old value is shifted out
      end
      
      PH_LCMCODA0: begin
        // state is that GCD is in Y, inputs alpha and beta are in S0 and S1
        rreg = REG_S0;
        wreg = REG_A_LO;
        opy = INPUT_REG_SHW;
        curph_d = PH_DIVLOOP;
        divret_d = PH_LCMCODA1;
      end

      PH_LCMCODA1: begin
        // alpha/GCD is in X, beta is in S1, upper bits of Y are clear
        // Y <- S1, postcondition X = alpha/GCD & Y = beta
        rreg = REG_S1;
        wreg = REG_Y_LO;
        opa_clr = 1;
        opa_do  = 1;
        curph_d = PH_MULLOOP;
        // product = alpha/GCD*beta = LCM(alpha,beta)
      end

      //////////////////////////////////////////////////////////////////////
      
      PH_DIVLOOP: begin
        // setup:
        // previous cycle must have shifted divisor left in Y using SHW op
        // return address is in divret
        opa_sub = 1;
        opa_exec = 1;       
        opa_do = carry_out; // borrow is inverse of carry out
        x_shin = carry_out;
        opx = INPUT_REG_SHL;
        
        if (cnt_q == '0) begin
          cnt_d = BITS-1;
          curph_d = divret_q;
        end else begin
          opy = INPUT_REG_SHR; // ensure Y contains divisor
          cnt_d = cnt_q_m1;
        end
      end

      PH_DIVOUT: begin
        // read out quotient
        rreg = REG_X;
        wreg = REG_Z;
        done_v_d = '1;
        curph_d = PH_OUP2B0;
      end

      //////////////////////////////////////////////////////////////////////
      
      PH_MULLOOP: begin
        // multiply step
        cnt_d = cnt_q_m1;
        
        opa_do = x[REG_X][0]; // do update accum if bit set in X
        opa_exec = 1;
        opy = INPUT_REG_SHL;
        opx = INPUT_REG_SHR;
        if (cnt_q == '0) begin
          cnt_d = BITS-1;
          curph_d = PH_MULOUT;
        end
        else
          cnt_d = cnt_q_m1;
      end

      PH_MULOUT: begin
        rreg = REG_A_LO;
        wreg = REG_Z;
        done_v_d = '1;
        curph_d = PH_OUP2A0;
      end

      //////////////////////////////////////////////////////////////////////
      
      PH_ADD1: begin
        opa_do  = 1;
        opa_exec = 1;
        curph_d = PH_OUPA;
      end

      PH_SUB1: begin
        opa_sub = 1; 
        opa_do  = 1;
        opa_exec = 1;
        curph_d = PH_OUPA;
      end

      //////////////////////////////////////////////////////////////////////

      // output routines
      
      PH_OUPA: begin // output accumulator
        rreg = REG_A_LO;
        wreg = REG_Z;
        done_v_d = '1;
        curph_d = PH_OUPW;
      end

      PH_OUPY: begin // output Y
        rreg = REG_Y_LO;
        wreg = REG_Z;
        done_v_d = '1;
        curph_d = PH_OUPW;
      end
        
      PH_OUPW: begin
        // output WAIT
        // output handshake, single result, loaded in Z, wait for handshake
        done_v_d = ~i_output_e;

        if (done_v_d != '1) 
          curph_d = PH_RDY;
      end

      PH_OUP2A0: begin
        // output handshake, accumulator LO, accumulator HI
        // previous state has loaded ACC_LO into Z
        done_v_d = '1;

        if (i_output_e) begin
          rreg = REG_A_HI;
          wreg = REG_Z;
          curph_d = PH_OUPW;
        end
      end

      PH_OUP2B0: begin
        // output handshake, Y reg, accumulator LO
        // previous state has loaded Y into Z
        done_v_d = '1;
        
        if (i_output_e) begin
          rreg = REG_A_LO;
          wreg = REG_Z;
          curph_d = PH_OUPW;
        end
      end
    endcase // casez (curph_q)
    
    inhibit[REG_A_LO] = ~opa_do;
    inhibit[REG_A_HI] = ~opa_do;
    inhibit[wreg]     = 0;
  end
endmodule // vern_calc
