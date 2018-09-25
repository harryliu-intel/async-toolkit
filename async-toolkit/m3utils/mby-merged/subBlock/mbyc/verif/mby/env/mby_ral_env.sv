
/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:    mby_ral_env.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : MBY IP
   ----------------------------------------------------------------------
 
  MBY Soala RAL ENV

  This file include the MBY IP connetion of RAL
 
 By default we define 2 access types:
 1. primary - access to registers through IOSF primary
 2. sideband - access to registers through IOSF sideband
 
 primary is mapped to <mby_iosf_pri_ral_access> seq
 
 sideband is mapped to <mby_iosf_sb_ral_access> seq

*/
//Include mby regs
`include "mby_regs_ral_env.svh"


class mby_ral_env extends mby_regs_ral_env;

    `ovm_component_utils(mby_ral_env);

    // --------------------------
    function new( string n="mby_ral_env", ovm_component p = null, string hdl_path = "");
        super.new( n, p, hdl_path);
    endfunction : new


    // --------------------------
    //virtual function void build();
    //  super.build();
    //endfunction : build

    // --------------------------
    //function void connect();
    //endfunction // void

  /*
   Function: end_of_elaboration - Map access type to sequences.
   
   Map primary and sideband to IOSF seq.
   */
    function void end_of_elaboration();
        super.end_of_elaboration();
// START IOSF_NOT_PRESENT
//        set_frontdoor_seq_type("primary","read","mby_ral_iosf_pri_access");
//        set_frontdoor_seq_type("primary","write","mby_ral_iosf_pri_access");
//        set_frontdoor_seq_type("sideband","read","mby_ral_iosf_sb_access");
//        set_frontdoor_seq_type("sideband","write","mby_ral_iosf_sb_access");
// END IOSF_NOT_PRESENT
    endfunction

    // --------------------------
    //function void init_regs();
    //endfunction

  /*
   Function: get_addr_val
   
   Parameters:
   slu_ral_access_path_t access_path - which  interface  to use (current support "premary" or "sideband"
   sla_ral_reg r  - which  regiser
   
   this function get access type and register and calculte the physical address of the register
   */
  function slu_ral_addr_t get_addr_val(slu_ral_access_path_t access_path, sla_ral_reg r);
    if(access_path == "primary")
      begin
	// Use space to define how to claculate the address
        case(r.get_space())
	  // PCIE cfg
          "CFG" : return(r.get_space_addr("CFG") | (r.get_func_num() <<16) | (r.get_dev_num() <<19) | (r.get_bus_num() <<24));
	  // Memory space
          "MEM" : begin
	    if(r.base_addr_reg !== null)
	      return(r.get_base_addr_val() + r.get_space_addr("MEM"));
	    else
	      // Fixed address register don't have BAR
	      return(r.get_space_addr("MEM"));
	  end
	  // IO space
          "IO" :begin
	    if(r.base_addr_reg !== null) begin
	      // IO BAR bit 0 is 1
	      // We need to claer the LSB bit of the BAR
	      slu_ral_data_t tmp_addr;
	      tmp_addr = r.get_base_addr_val();
	      tmp_addr[0:0] = 1'b0; // init with zero the RTE
	      return (tmp_addr  + r.get_space_addr("IO"));
	    end  else
	      // Fixed address register don't have BAR
	      return(r.get_space_addr("IO"));
	  end
        endcase // case(r.get_space())
      end
    else if (access_path == "sideband") 
      begin
        return(r.get_space_addr("MSG"));
      end
    else
      begin
        ovm_report_fatal (get_name(), $psprintf("Unsupport access type %s",access_path));
      end

    endfunction : get_addr_val


endclass : mby_ral_env
