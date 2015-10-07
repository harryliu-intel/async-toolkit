module Nodes2ChannelConv 
    #(parameter base = 4, numBits = 3)
    (input [0:base-1] nodes,
     output enable, 
     output reg signed [numBits-1:0] channel$data,
     input channel$enable,
     input _RESET );

// temporary variable
integer i;
integer numNonZerosFound; // used for debugging

// use always block to handle data 
always @(nodes) 
begin
if (nodes == 0)
   channel$data = -1;
else 
   begin
      // Add debugging code for now. May delete later.
      numNonZerosFound = 0;
      for (i = 0; i < base; i=i+1)
      begin
          if (nodes[i] != 0) begin
              channel$data = i;
              numNonZerosFound = numNonZerosFound + 1;
          end
      end
`ifndef NODECONVERTER_DISABLE_WARNINGS
      if ((numNonZerosFound != 1) && (_RESET == 1)) 
      begin
          assert (0) else $error("ERROR: 1-hot protocol violated in Node2ChannelConv (instance: %m nodes: %b)\n", nodes);
          //$stop();
      end
`endif
   end
end      

// enable can be done with combinational assignment statement
assign enable = channel$enable & _RESET;

endmodule

module Channel2NodesConv
    #(parameter base = 4, numBits = 3)
    (output reg [0:base-1] nodes,
     input enable, 
     input signed [numBits-1:0] channel$data,
     output channel$enable,
     input _RESET);

// use always block to handle data 
always @(channel$data) 
begin
if (channel$data == -1)
   nodes = 0; 
else 
   begin
`ifndef NODECONVERTER_DISABLE_WARNINGS
      if (nodes != 0) begin
        assert (0) else $error("ERROR: 1-hot protocol violated in Channel2NodeConv (instance: %m nodes: %b)\n", nodes);
        //$stop();
      end 
`endif
      nodes[channel$data] = 1; 
   end
end

// enable can be done with combinational assignment statement
assign channel$enable = enable;

endmodule

module Nodes2ChannelConv2
    #(parameter base = 4, numBits = 3)
    (input [0:base-1] nodes,
     output enable, 
     output reg signed [numBits-1:0] channel$data,
     input channel$enable);

// temporary variable
integer i;
integer numNonZerosFound; // used for debugging
`ifdef NODECONVERTER_USE_VPI_RESET
reg dutInReset;
`endif

`ifndef NODECONVERTER_DISABLE_WARNINGS
reg firstToken;
initial firstToken = 1'b1;
`endif

// use always block to handle data 
always @(nodes) 
begin
if (nodes == 0)
   channel$data = -1;
else 
   begin
      // Add debugging code for now. May delete later.
      numNonZerosFound = 0;
      for (i = 0; i < base; i=i+1)
      begin
          if (nodes[i] != 0) begin
              channel$data = i;
              numNonZerosFound = numNonZerosFound + 1;
          end
      end
`ifndef NODECONVERTER_DISABLE_WARNINGS
  `ifdef NODECONVERTER_USE_VPI_RESET
      if (channel$enable == 1) begin
         if (numNonZerosFound != 1) begin
            $dutIsReset(dutInReset);
            if (dutInReset != 1'b1) begin
               channel$data = 'x;
               assert (0) else $error("ERROR: 1-hot protocol violated in Nodes2ChannelConv2 (instance: %m nodes: %b)\n", nodes);
            end
         end
      end 
   `else
      if (channel$enable == 1) begin
         if (numNonZerosFound == 1) begin
             firstToken = 1'b0;
         end else begin
             channel$data = 'x;
             if (!firstToken) begin
                 assert (0) else $error("ERROR: 1-hot protocol violated in Nodes2ChannelConv2 (instance: %m nodes: %b)\n", nodes);
                 //$stop();
             end
         end
      end 
   `endif
`endif
   end
end      

// enable can be done with combinational assignment statement
assign enable = channel$enable;

endmodule

module Channel2NodesConv2
    #(parameter base = 4, numBits = 3)
    (output reg [0:base-1] nodes,
     input enable, 
     input signed [numBits-1:0] channel$data,
     output channel$enable);

// use always block to handle data 
always @(channel$data) 
begin
if (channel$data == -1)
   nodes = 0; 
else 
   begin
`ifndef NODECONVERTER_DISABLE_WARNINGS
      if (nodes != 0) begin
        assert (0) else $error("ERROR: 1-hot protocol violated in Channel2NodesConv2 (instance: %m nodes: %b)\n", nodes);
        //$stop();
      end 
`endif
      nodes[channel$data] = 1; 
   end
end

// enable can be done with combinational assignment statement
assign channel$enable = enable;

endmodule
