module csp_input_dft (input _RESET, output reg enable);
always @(negedge _RESET) enable = 0;
always @(posedge _RESET) enable = 1;
endmodule

module csp_output_dft (input _RESET, output reg [2:0] data);
always @(negedge _RESET) data = 3'b0;
always @(posedge _RESET) data[2] = 1;
endmodule

module csp_new_input_dft (input                   _RESET,
                          input      signed [1:0] D$data,
                          output reg              D$enable,
                          input             [2:0] C);
always @(negedge _RESET) D$enable = 0;
always @(posedge _RESET) D$enable = 1;
endmodule

module csp_new_output_dft (input                   _RESET,
                           output reg signed [1:0] D$data,
                           input                   D$enable,
                           output reg        [2:0] C);
always @(negedge _RESET) begin
  C      = 3'b0;
  D$data = -1;
end
always @(posedge _RESET) begin
  C[0] = 1;
  D$data = -1;
end
endmodule

module csp_passthru_dft (input               _RESET,
                         input  signed [1:0] \LS.D$data ,
                         output              \LS.D$enable ,
                         input         [2:0] \LS.C ,

                         output signed [1:0] \RS.D$data ,
                         input               \RS.D$enable ,
                         output        [2:0] \RS.C );
assign \RS.D$data    = \LS.D$data ;
assign \LS.D$enable  = \RS.D$enable ;
assign \RS.C         = \LS.C ;
endmodule

module csp_sram_serial (input               _RESET,
                        input  signed [1:0] \SL.REP$data ,
                        output              \SL.REP$enable ,
                        output signed [1:0] \SL.ERR$data ,
                        input               \SL.ERR$enable ,
                        input  signed [1:0] \SL.DFT.D$data ,
                        output              \SL.DFT.D$enable ,
                        input         [2:0] \SL.DFT.C ,

                        output signed [1:0] \SR.REP$data ,
                        input               \SR.REP$enable ,
                        input  signed [1:0] \SR.ERR$data ,
                        output              \SR.ERR$enable ,
                        output signed [1:0] \SR.DFT.D$data ,
                        input               \SR.DFT.D$enable ,
                        output        [2:0] \SR.DFT.C );

`CAST2VERILOG_NEW_INPUT_DFT in_dft   (._RESET   (_RESET),
                                      .D$data   (\SL.DFT.D$data ),
                                      .D$enable (\SL.DFT.D$enable ),
                                      .C        (\SL.DFT.C ));
`CAST2VERILOG_NEW_OUTPUT_DFT out_dft (._RESET   (_RESET),
                                      .D$data   (\SR.DFT.D$data ),
                                      .D$enable (\SR.DFT.D$enable ),
                                      .C        (\SR.DFT.C ));

assign \SR.REP$data    = \SL.REP$data ;
assign \SL.REP$enable  = \SR.REP$enable ;
assign \SL.ERR$data    = \SR.ERR$data ;
assign \SR.ERR$enable  = \SL.ERR$enable ;

endmodule

module csp_passthru_sram_serial
                       (input               _RESET,
                        input  signed [1:0] \SL.REP$data ,
                        output              \SL.REP$enable ,
                        output signed [1:0] \SL.ERR$data ,
                        input               \SL.ERR$enable ,
                        input  signed [1:0] \SL.DFT.D$data ,
                        output              \SL.DFT.D$enable ,
                        input         [2:0] \SL.DFT.C ,

                        output signed [1:0] \SR.REP$data ,
                        input               \SR.REP$enable ,
                        input  signed [1:0] \SR.ERR$data ,
                        output              \SR.ERR$enable ,
                        output signed [1:0] \SR.DFT.D$data ,
                        input               \SR.DFT.D$enable ,
                        output        [2:0] \SR.DFT.C );

`CAST2VERILOG_PASSTHRU_DFT dft   (._RESET        (_RESET),
                                  .\LS.D$data    (\SL.DFT.D$data ),
                                  .\LS.D$enable  (\SL.DFT.D$enable ),
                                  .\LS.C         (\SL.DFT.C ),
                                  .\RS.D$data    (\SR.DFT.D$data ),
                                  .\RS.D$enable  (\SR.DFT.D$enable ),
                                  .\RS.C         (\SR.DFT.C ));

assign \SR.REP$data    = \SL.REP$data ;
assign \SL.REP$enable  = \SR.REP$enable ;
assign \SL.ERR$data    = \SR.ERR$data ;
assign \SR.ERR$enable  = \SL.ERR$enable ;

endmodule
