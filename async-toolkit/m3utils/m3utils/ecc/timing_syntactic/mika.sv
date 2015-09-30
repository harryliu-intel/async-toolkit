`include "ecc_defs.v"

module mika(input  logic                clk,
            input  logic [TBITS-1:0]    i_input_d,
            output logic [DBITS-1:0]    o_data_q,
            output logic                o_err_detect_q,
            output logic                o_err_multpl_q);
            
  logic [TBITS-1:0] i_input_q;
  logic [DBITS-1:0] o_data_d;
  logic             o_err_detect_d, o_err_multpl_d;

  always @(posedge clk) begin : flop_input
    i_input_q <= i_input_d;
  end

  read_ecc  u_read_ecc (i_input_q[TBITS-1:CBITS],
                        i_input_q[CBITS-1:0],
                        o_err_detect_d, 
                        o_err_multpl_d, 
                        o_data_d);

  always @(posedge clk) begin : flop_output
    o_data_q       <= o_data_d;
    o_err_detect_q <= o_err_detect_d;
    o_err_multpl_q <= o_err_multpl_d;
  end

endmodule