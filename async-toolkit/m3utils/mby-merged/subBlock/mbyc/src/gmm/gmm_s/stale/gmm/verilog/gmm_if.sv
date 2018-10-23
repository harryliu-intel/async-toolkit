interface gmm_if ();
  import gmm_pkg::*;

//Port List
    cmp_ring_pkt_str_t      gmm_rdata_in;
    cmp_ring_pkt_str_t      gmm_rdata_out;
    cmp_ring_pkt_str_t      gmm_ldata_in;
    cmp_ring_pkt_str_t      gmm_ldata_out;
    sys_word_t              gmm_mem_data_in;
    sys_word_t              gmm_mem_data_out;
    csr_gmm_data_t          gmm_csr_data_in;
    csr_gmm_data_t          gmm_csr_data_out;
    p_row_op_t              gmm_prow_rdrsp_op_out;      // Read response operation
    sys_word_t              gmm_prow_rdrsp_data_in;     // Read response data-path
    p_row_op_t              gmm_prow_wrrqt_op_out;      // write request operation
    sys_word_t              gmm_prow_wrrqt_data_out;    // Write request data-path
    p_row_op_t              gmm_prow_rdrqt_op_out;      // Read request operation
    

modport r_ring(
    input   gmm_rdata_in ,              // right ring data in 
    output  gmm_rdata_out               // right ring data out  
    );

modport l_ring(
    input   gmm_ldata_in,               // left ring data in 
    output  gmm_ldata_out               // left ring data out 
    );

//FIXME. Review future releases of FS
modport cpu_mem(                
    input   gmm_mem_data_in,            // cpu data input for loading pods
    output  gmm_mem_data_out            // cpu data output for reading pods 
    );

//FIXME. Review future releases of FS
modport csr(
    input   gmm_csr_data_in,            // csr data input
    output  gmm_csr_data_out            // csr data output 
    );

//FIXME. Review future releases of FS on wikipage
// There are no specified requirements  
modport pseudo_row(
    output  gmm_prow_rdrsp_op_out,      // Read response operation
    input   gmm_prow_rdrsp_data_in,     // Read response data-path
    output  gmm_prow_wrrqt_op_out,      // write request operation
    output  gmm_prow_wrrqt_data_out,    // Write request data-path
    output  gmm_prow_rdrqt_op_out       // Read request operation 
    ); 

modport gmm(
    output  gmm_rdata_in ,              // right ring data in 
    input   gmm_rdata_out,              // right ring data out
    output  gmm_ldata_in,               // left ring data in 
    input   gmm_ldata_out,              // left ring data out
    output  gmm_mem_data_in,            // cpu data input for loading pods
    input   gmm_mem_data_out,           // cpu data output for reading pods
    output  gmm_csr_data_in,            // csr data input
    input   gmm_csr_data_out,           // csr data output 
    input   gmm_prow_rdrsp_op_out,      // Read response operation
    output  gmm_prow_rdrsp_data_in,     // Read response data-path
    input   gmm_prow_wrrqt_op_out,      // write request operation
    input   gmm_prow_wrrqt_data_out,    // Write request data-path
    input   gmm_prow_rdrqt_op_out       // Read request operation 
    );

endinterface: gmm_if
