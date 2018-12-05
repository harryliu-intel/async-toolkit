/*%W%   %G%*/

/**************************************************************
 * File Name    : egr_tc_rr_sm.sv
 * Author Name  : John Lo
 * Description  : A simple round-robin arbitor for traffic class.
 * Parent Module: 
 * Child  Module:
 * Interface Mod: many
 * Date Created : 2018-11-22
 * Notes        : cclk, hreset_n, sreset_n, egr_reset_n
 *
 ***************************************************************/

//`include  "mby_egr_epl_defines.svh"

module egr_tc_rr_sm (
  // tx_mod_if
  output logic [7:0]                    tc_gnt,
  input  logic [7:0]                    tc_req,
  // epl_rx_pfc_if
  input  logic [7:0]                    pfc_xoff,  
  // csr_if
  // global signals
  input  logic 	                        reset_n,
  input  logic 	                        clk   
  );

  typedef enum logic [3:0] {
	                     IDLE             = 4'h8
	                   ,GRANT0            = 4'h0
                           ,GRANT1            = 4'h1
                           ,GRANT2            = 4'h2
                           ,GRANT3            = 4'h3
                           ,GRANT4            = 4'h4
                           ,GRANT5            = 4'h5
                           ,GRANT6            = 4'h6
                           ,GRANT7            = 4'h7
                           } statetype;

  statetype                state, nx_state ;
  logic  [7:0]             QA_req          ; 

  assign   QA_req = tc_req  & (~pfc_xoff);

  // begin egr_tc_rr_sm
  always_comb begin : EGR_EPL_TC_SM_COMB
       nx_state          = IDLE;
    case (state)
      IDLE  : begin
        if      (QA_req[0])         
          nx_state          = GRANT0;
        else if (QA_req[1])          
          nx_state          = GRANT1;
        else if (QA_req[2])         
          nx_state          = GRANT2;
        else if (QA_req[3])          
          nx_state          = GRANT3;
        else if (QA_req[4])           
          nx_state          = GRANT4;
        else if (QA_req[5])          
          nx_state          = GRANT5;
        else if (QA_req[6])          
          nx_state          = GRANT6;
        else if (QA_req[7])          
          nx_state          = GRANT7;
        else 
          nx_state          = IDLE;
      end // IDLE 
      GRANT0: begin
        if      (QA_req[1])
          nx_state          = GRANT1;
        else if (QA_req[2])
          nx_state          = GRANT2;
        else if (QA_req[3])
          nx_state          = GRANT3;
        else if (QA_req[4])
          nx_state          = GRANT4;
        else if (QA_req[5])
          nx_state          = GRANT5;
        else if (QA_req[6])
          nx_state          = GRANT6;
        else if (QA_req[7])
          nx_state          = GRANT7;
        else if (QA_req[0])
          nx_state          = GRANT0;
        else 
          nx_state          = IDLE;
      end // GRANT0
      GRANT1: begin
        if      (QA_req[2])
          nx_state          = GRANT2;
        else if (QA_req[3])          
          nx_state          = GRANT3;
        else if (QA_req[4])          
          nx_state          = GRANT4;
        else if (QA_req[5])          
          nx_state          = GRANT5;
        else if (QA_req[6])          
          nx_state          = GRANT6;
        else if (QA_req[7])          
          nx_state          = GRANT7;
        else if (QA_req[0])          
          nx_state          = GRANT0;
        else if (QA_req[1])          
          nx_state          = GRANT1;
        else 
          nx_state          = IDLE;
      end // GRANT1
      GRANT2: begin                
        if      (QA_req[3])          
          nx_state          = GRANT3;
        else if (QA_req[4])          
          nx_state          = GRANT4;
        else if (QA_req[5])          
          nx_state          = GRANT5;
        else if (QA_req[6])          
          nx_state          = GRANT6;
        else if (QA_req[7])          
          nx_state          = GRANT7;
        else if (QA_req[0])          
          nx_state          = GRANT0;
        else if (QA_req[1])         
          nx_state          = GRANT1;
        else if (QA_req[2])         
          nx_state          = GRANT2;
        else 
          nx_state          = IDLE;
      end // GRANT2
      GRANT3: begin                
        if      (QA_req[4])          
          nx_state          = GRANT4;
        else if (QA_req[5])           
          nx_state          = GRANT5;
        else if (QA_req[6])          
          nx_state          = GRANT6;
        else if (QA_req[7])          
          nx_state          = GRANT7;
        else if (QA_req[0])         
          nx_state          = GRANT0;
        else if (QA_req[1])          
          nx_state          = GRANT1;
        else if (QA_req[2])         
          nx_state          = GRANT2;
        else if (QA_req[3])         
          nx_state          = GRANT3;
        else 
          nx_state          = IDLE;
      end // GRANT3
      GRANT4: begin                
        if      (QA_req[5])           
          nx_state          = GRANT5;
        else if (QA_req[6])          
          nx_state          = GRANT6;
        else if (QA_req[7])          
          nx_state          = GRANT7;
        else if (QA_req[0])         
          nx_state          = GRANT0;
        else if (QA_req[1])          
          nx_state          = GRANT1;
        else if (QA_req[2])         
          nx_state          = GRANT2;
        else if (QA_req[3])          
          nx_state          = GRANT3;
        else if (QA_req[4])          
          nx_state          = GRANT4;
        else 
          nx_state          = IDLE;
      end // GRANT4
      GRANT5: begin                
        if      (QA_req[6])          
          nx_state          = GRANT6;
        else if (QA_req[7])          
          nx_state          = GRANT7;
        else if (QA_req[0])         
          nx_state          = GRANT0;
        else if (QA_req[1])          
          nx_state          = GRANT1;
        else if (QA_req[2])         
          nx_state          = GRANT2;
        else if (QA_req[3])          
          nx_state          = GRANT3;
        else if (QA_req[4])           
          nx_state          = GRANT4;
        else if (QA_req[5])           
          nx_state          = GRANT5;
        else 
          nx_state          = IDLE;
      end // GRANT5
      GRANT6: begin                
        if      (QA_req[7])          
          nx_state          = GRANT7;
        else if (QA_req[0])         
          nx_state          = GRANT0;
        else if (QA_req[1])          
          nx_state          = GRANT1;
        else if (QA_req[2])         
          nx_state          = GRANT2;
        else if (QA_req[3])          
          nx_state          = GRANT3;
        else if (QA_req[4])           
          nx_state          = GRANT4;
        else if (QA_req[5])          
          nx_state          = GRANT5;
        else if (QA_req[6])          
          nx_state          = GRANT6;
        else 
          nx_state          = IDLE;
      end // GRANT6
      GRANT7: begin                
        if      (QA_req[0])         
          nx_state          = GRANT0;
        else if (QA_req[1])          
          nx_state          = GRANT1;
        else if (QA_req[2])         
          nx_state          = GRANT2;
        else if (QA_req[3])          
          nx_state          = GRANT3;
        else if (QA_req[4])           
          nx_state          = GRANT4;
        else if (QA_req[5])          
          nx_state          = GRANT5;
        else if (QA_req[6])          
          nx_state          = GRANT6;
        else if (QA_req[7])          
          nx_state          = GRANT7;
        else 
          nx_state          = IDLE;
      end // GRANT7
      default:    nx_state          = IDLE;
    endcase // end case (state) 
  end : EGR_EPL_TC_SM_COMB
 
  // seq part
  always_ff @ (posedge clk) begin 
    if (!reset_n) begin
      state          <= IDLE;
      tc_gnt         <= '0;
    end
    else begin
      state          <= nx_state;
      tc_gnt[0]      <= nx_state == GRANT0;
      tc_gnt[1]      <= nx_state == GRANT1;
      tc_gnt[2]      <= nx_state == GRANT2;
      tc_gnt[3]      <= nx_state == GRANT3;
      tc_gnt[4]      <= nx_state == GRANT4;
      tc_gnt[5]      <= nx_state == GRANT5;
      tc_gnt[6]      <= nx_state == GRANT6;
      tc_gnt[7]      <= nx_state == GRANT7;
    end
  end 
 
endmodule : egr_tc_rr_sm


