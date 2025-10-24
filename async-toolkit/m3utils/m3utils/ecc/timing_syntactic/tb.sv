module tb();

logic [TBITS-1:0]    i_input_d;

logic [DBITS-1:0]    o_data_q_mika;
logic                o_err_detect_q_mika;
logic                o_err_multpl_q_mika;

logic [DBITS-1:0]    o_data_q_dw;
logic                o_err_detect_q_dw;
logic                o_err_multpl_q_dw;

  logic clk;

  // Generate clock
  initial begin:gen_clk
    while(1) begin
      #10;
      clk = ~clk;
    end
  end

  mika u_mika_dut(clk, i_input_d, o_data_q_mika, o_err_detect_q_mika, o_err_multpl_q_mika);
  dw u_dw_dut(clk, i_input_d, o_data_q_dw, o_err_detect_q_dw, o_err_multpl_q_dw);

  initial begin
    clk = '0;
    for (int i=0; i<100; ++i)
      begin
        @(posedge clk);
        #1;
        i_input_d = ($urandom << 64) | ($urandom << 32) | $urandom; 
        $display("i_input_d %12h o_data_q_mika %12h o_data_q_dw %12h",
                 i_input_d, o_data_q_mika, o_data_q_dw);
      end
  end

endmodule