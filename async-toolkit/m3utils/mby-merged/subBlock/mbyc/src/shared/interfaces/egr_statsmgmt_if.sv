// -- Description  : This is the interface for interconnecting 
//                   Egress (EGR) with the Stats Management

interface egr_statsmgmt_if ();
  import mby_egr_pkg::*;
    
//    updatedstats_t updatedstats;
//    localstatsstall_t localstatsstall;    
//    localstats_t localstats;

    logic updatedstats;
    logic localstatsstall;    
    logic localstats;
    
modport egr(
    input updatedstats,
    input localstatsstall,
    output localstats
    );

modport statsmgmt(
    output updatedstats,
    output localstatsstall,
    input localstats
    );
    
    
    
endinterface : egr_statsmgmt_if