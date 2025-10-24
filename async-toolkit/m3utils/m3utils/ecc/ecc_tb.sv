module ecc_tb();

`include "ecc_defs.v"

  logic clk;

  // Generate clock
  initial begin:gen_clk
    while(1) begin
      #10;
      clk = ~clk;
    end
  end
  
  logic err_flag, done_flag;

  logic [DBITS-1 : 0] i_data_a, o_data_a;
  logic [CBITS-1 : 0]           o_chk_a ;

  logic [DBITS-1 : 0] i_data_b, o_data_b;
  logic [CBITS-1 : 0] i_chk_b           ;

  logic o_err_detect_b, o_err_multpl_b;

  write_ecc u_dut_write_ecc
    (
    .i_data                             ( i_data_a ),
    .o_data                             ( o_data_a ),
    .o_chk                              ( o_chk_a )
    );

  read_ecc u_dut_read_ecc
    (
    .i_data                             ( i_data_b ),
    .i_chk                              ( i_chk_b  ),
    .o_err_detect                       ( o_err_detect_b ),
    .o_err_multpl                       ( o_err_multpl_b ),
    .o_data                             ( o_data_b )
    // maybe carry syndrome here, do we need it?
    );

  logic [DBITS+CBITS-1:0] pre_error, post_error, err_pattern;

  assign pre_error[CBITS-1:0]           = o_chk_a;
  assign pre_error[DBITS+CBITS-1:CBITS] = o_data_a;

  assign i_chk_b                        = post_error[CBITS-1:0];
  assign i_data_b                       = post_error[DBITS+CBITS-1:CBITS];

  assign post_error = pre_error ^ err_pattern;

  initial begin
    //int c; c = CBITS;
    localparam N=100;
    
    err_flag = '0;
    done_flag = '0;
    clk = '0;

    err_pattern = '0;
  
    for (int c=-1; c< TBITS; ++c) 
    for (int b=-1; b< TBITS; ++b)
      begin
        for (int i=0; i<N; ++i)
          begin
            logic any_err;
            logic multpl_err;

            any_err     = (b != -1) || (c != -1);
            multpl_err  = ((b != -1) && (c != -1)) && (b!=c);

            @(posedge clk);
            err_flag = '0;
            #1;
    
            //i_data_a                    = i;
            i_data_a                    = $urandom << 32 | $urandom;
            err_pattern                 = '0;
            err_pattern                 |= (b==-1) ? '0 : ('b1 << b);
            err_pattern                 |= (c==-1) ? '0 : ('b1 << c);
    
            @(negedge clk);

            $display("i_data_a %16h o_data_b %16h err_pattern %16h o_data_a^i_data_b %16h", i_data_a, o_data_b, err_pattern, o_data_a ^ i_data_b);

            assert(o_data_b == i_data_a || multpl_err) 
              else begin
                $display("data mismatch %16h != %16h", o_data_b, i_data_a);
                err_flag = '1;
              end;

            assert(o_err_detect_b == any_err)
              else begin
                $display("missed an error!");
                err_flag = '1;
              end;           

            assert(o_err_multpl_b == multpl_err)
              else begin
                $display("o_err_multpl_b flag is wrong %d != %d",
                       o_err_multpl_b,multpl_err);
                err_flag = '1;
              end
          end
      end

    done_flag = '1;
    $display("done.");
    $finish();
  end

endmodule
