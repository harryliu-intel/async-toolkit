// Module:                      pcie_model_with_reset
// SOMA file:                   pcie_model_with_reset.spc
// Initial contents file:       

`timescale 10fs/10fs
module pcie_model_with_reset(
    CLK_TX,
    CLK_RX,
    PERST_n,
    TX,
    TX_,
    RX,
    RX_
);
    parameter linkWidth = 1;
    parameter interface_soma = "pcie_model_with_reset.spc";
    parameter init_file = "";
    input PERST_n;
    output [(linkWidth-1):0] TX;
    reg [(linkWidth-1):0] den_TX;
    assign TX = den_TX;
    output [(linkWidth-1):0] TX_;
    reg [(linkWidth-1):0] den_TX_;
    assign TX_ = den_TX_;
    input [(linkWidth-1):0] RX;
    input [(linkWidth-1):0] RX_;
    inout CLK_TX;
    reg den_CLK_TX;
    assign CLK_TX = den_CLK_TX;
    inout CLK_RX;
    reg den_CLK_RX;
    assign CLK_RX = den_CLK_RX;
initial
    $pcie_access(PERST_n,TX,den_TX,TX_,den_TX_,RX,RX_,CLK_TX,den_CLK_TX,CLK_RX,den_CLK_RX);
endmodule

