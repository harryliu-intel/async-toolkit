module BDNodes2ChannelConv2
    #(parameter numBits = 3)
    (input [0:numBits-1] nodes,
     input req,
     output ack,
     output [numBits-1:0] channel$data,
     output channel$req,
     input channel$ack);
assign channel$data = {<< {nodes}};
assign channel$req = req;
assign ack = channel$ack;
endmodule

module BDChannel2NodesConv2
    #(parameter numBits = 3)
    (output [0:numBits-1] nodes,
     output req,
     input ack, 
     input [numBits-1:0] channel$data,
     input channel$req,
     output channel$ack);
assign nodes = {<< {channel$data}};
assign req = channel$req;
assign channel$ack = ack;
endmodule
