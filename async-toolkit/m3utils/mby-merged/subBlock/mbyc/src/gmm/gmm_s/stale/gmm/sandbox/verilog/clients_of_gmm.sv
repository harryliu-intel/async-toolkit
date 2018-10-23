module clients_of_gmm
import gmm_pkg::*;
(
    input   cclk,       // system clock
    input   arst,       // Asynchronous reset
    gmm_if.csr          csr_intf,   //TODO: comments
    gmm_if.cpu_mem      cpu_intf,
    gmm_if.l_ring       lring_intf,
    gmm_if.r_ring       rring_intf,
    gmm_if.pseudo_row   prow_intf
);
 

endmodule
