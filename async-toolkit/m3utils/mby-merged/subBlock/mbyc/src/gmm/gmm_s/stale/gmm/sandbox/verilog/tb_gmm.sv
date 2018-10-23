
module tb_gmm();
    
import sla_pkg::*;

    logic clk = 0;                              // declare clock
    logic arst_n =1;                            // declare asynchronous reset low active


    // define clock
    initial                                     // "initial" procedures are executed at the beginning of simulation
        forever                                 // "forever" is just a type of loop that executes forever 
            #(10/2)                             //  #<n> means wait <n> clocks
                clk = ~clk;                     //  invert the clock

     initial 
         begin
             #10 arst_n =1;
             #10 arst_n =0;
             #10 arst_n =1;
             #300ps;
         end
         
gmm_if gmm_ift();

gmm_top uut(
    .cclk(clk),       // system clock
    .arst(arst_n),       // Asynchronous reset
    .gmm_intf(gmm_ift.gmm) 
    );

clients_of_gmm client_uut(
    .cclk(clk),                 // system clock
    .arst(arst_n),              // Asynchronous reset
    .csr_intf(gmm_ift.csr),     //TODO: comments
    .cpu_intf(gmm_ift.cpu_mem),
    .lring_intf(gmm_ift.l_ring),
    .rring_intf(gmm_ift.r_ring),
    .prow_intf(gmm_ift.pseudo_row)
    );
         
         
endmodule
