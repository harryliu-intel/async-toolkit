/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:    tlm_tb.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : TLM1 IP
   ----------------------------------------------------------------------
 
  
 TLM1 IP TB top moudle

 This top moudle will:
 1. Instance the RTL
 2. Insance and connect all extrenal interfaces.
 3. Instance the IP TI
 4. Hold un-reuse (IP specific code ) like, clocks and resets



*/

`timescale 1ps/1ps 



`include "tlm_defines.sv"
`include "dummy_dut.v"

module tlm_tb ();
// parameter   UPF_SIMULATIONS = 1;
//  parameter   UPF_SIMULATIONS = 0;
  
    import uvm_pkg::*;
    
//   import UPF::*;
  
    
    logic primary_clock;
    logic gated_primary_clock;
    logic secondary_clock;
    logic gated_secondary_clock;

  
    // ===============================================
    // =                FSDB issues                  =
    // ===============================================
  reg [1023:0] fsdb_file;	
  int fsdb_config_file;
  longint fsdb_on,fsdb_off;
  int     fsdb_size = 1800;
  string str;
  logic        vccRail;
   
  initial 
    if($value$plusargs("fsdb=%s",fsdb_file)) begin
       
      // Support to start sample after a delay
      if ($value$plusargs("FSDB_ON=%s",str)) begin
	fsdb_on = convert_time(str);
	$display("FSDB DUMP:  Waiting %s before starting fsdb dump", str);
	#fsdb_on;
      end
      else fsdb_on = 0;
      // Limit fsdb file size
      if ($value$plusargs("FSDB_SIZE=%d",fsdb_size)) begin
	if (fsdb_size<32) fsdb_size = 32;
      end

      $fsdbAutoSwitchDumpfile(fsdb_size,fsdb_file,0,{fsdb_file,"_vf.log"});
      
      // This is used for a config file that tells FSDB which modules to sample.
      if ($test$plusargs("fsdb_config"))
        $fsdbDumpvarsToFile ("fsdb.dump.config");
      // This is used to dump all signals in the design
      else
        $fsdbDumpvars(0,tlm_tb,"+all");
      
      if ($value$plusargs("FSDB_OFF=%s",str)) begin
	fsdb_off = convert_time(str) - fsdb_on;
	$display("SIMSTAT:  Stopping FSDB dump in %s", str);
	if (fsdb_off>0)
          #fsdb_off;
	$fsdbDumpoff();
	$display("FSDB DUMP :  Stopped FSDB dump");
	fsdb_on = -1; //fsdb is off
      end
    end // if ($value$plusargs("fsdb=%s",fsdb_file))
  
  
  function automatic longint convert_time(string in);
    longint out = in.atoi();
    case (in.substr(in.len()-2, in.len()-1))
      "ms": out *= 1000000000; // ONE BILLION ps steps in a ms unit
      "us": out *= 1000000;    // ONE MILLION ps steps in a us unit
      "ns": out *= 1000;       // ONE THOUSAND ps steps in a ns unit
    endcase //case suffix
    return out;
  endfunction //convert_time()

  // ===============================================
  //DUT RTL instance
`include "tlm_top_inst.v"
  // ===============================================

  
    // Parameters for interfaces
    `include "tlm_params.sv"

    // ===============================================
    // Interfaces instance
    // ===============================================

    // ===============================================
    // Clock block instance
    // ===============================================
  
    int idle_conter;
    initial 
    begin
      primary_clock = 1'b0;
      gated_primary_clock = 1'b0;
      secondary_clock = 1'b0;
      idle_conter = 0;
      
    end
  
 //INTEG : remove since real RTL should drive this :
  initial begin
   
    force `TLM1_TOP_PATH.prim_pok = 0;
    @(posedge primary_clock);
    release `TLM1_TOP_PATH.prim_pok;
    force `TLM1_TOP_PATH.prim_pok = 1;
  end
  
initial begin
    force `TLM1_TOP_PATH.sb_pok = 0;
    @(posedge tlm_if.secondary_reset) ;
   // @(posedge secondary_clock);
    release `TLM1_TOP_PATH.sb_pok;
    force `TLM1_TOP_PATH.sb_pok = 1;
    
  end
   
    always #5000 primary_clock = (tlm_if.enable_primary_clock ? ~primary_clock : 0);
    always #5000 secondary_clock = (tlm_if.enable_secondary_clock ?~secondary_clock : 0);

  
  always @(primary_clock) begin
    if (idle_conter > 8) 
      gated_primary_clock  <= 0;
    else
      gated_primary_clock <=  primary_clock;
  end
    //
    // ===============================================
    // Reset block instance
    // ===============================================
    
    // ===============================================
    // INSTANCES
    // ===============================================
    // TLM1 controller instance + signal connections
    // ===============================================
    
    // Connecting clocks to RTL
    // ===============================================
  
    assign tlm_primary_clock = gated_primary_clock;
    assign tlm_secondary_clock = gated_secondary_clock;
    
    // Primary IOSF connection to TLM1
    // ===============================================


   //INTEG : remove while real signals are connected :
  assign   `TLM1_TOP_PATH.tlm_psf_mtag  = 0;
  assign   `TLM1_TOP_PATH.tlm_psf_mep = 0;
  assign   `TLM1_TOP_PATH.tlm_psf_mat = 0;
  assign   `TLM1_TOP_PATH.tlm_psf_msai = 0;
  assign   `TLM1_TOP_PATH.tlm_psf_mdest_id = 0;
  assign   `TLM1_TOP_PATH.tlm_psf_msrc_id = 0 ; 
  assign   `TLM1_TOP_PATH.tlm_psf_req_dest_id = 0;
  //signals for 2012WW46r121114 primary IOSF compliance
  //INTEG : remove if real signals are connected 

    // Instance TLM1 ENV interface
    // This is done in the TB if the IP need to drive also signals.
    // ===============================================
  tlm_env_if tlm_if();
  assign tlm_power_good_reset = tlm_if.power_good_reset;
  assign tlm_secondary_reset = tlm_if.secondary_reset;
  assign tlm_primary_reset = tlm_if.primary_reset;
  assign tlm_if.primary_clock = primary_clock;
  assign tlm_if.secondary_clock = secondary_clock;
  assign tlm_if.tlm_int_wire = `TLM1_TOP.tlm_int;
  
  
  
    // ===============================================
    // Test Island instance
    // ===============================================
    tlm_ti_high #(
            ) 
            u_tlm_ti_high (
		     .tlm_if          (tlm_if)
                 );

  

  dummy_dut dummy_dut_();
  

///----------------------------------
//INTEG : PUT YOUR CODE HERE : replace wiht your power supplies
//INTEG : this is is only example , please replace with your power domains
/* -----\/----- EXCLUDED -----\/-----

initial begin
  vccRail = 1'bx;
  #100ps;
  vccRail  = 1'b1;
end
  
  initial
begin: startCorruption
    $display("%m: Initial all the supply rails @ %0t", $time);
    if (UPF_SIMULATIONS)
      begin
	if (vccRail === 1'bx) //INTEG : replace with vccIF
	  supply_off("vcccorevid_1p03");
      end
end:startCorruption

 // PRIMARY Supply Rail
always @(vccRail)
begin: connectRail
    $display("%m: SCC Rail Triggered @ %0t", $time);
    // Only enable the connections for UPF simulations.
    // This code will not work for non-UPF based simulations
    if (UPF_SIMULATIONS)
    begin
      if (vccRail == 1'b1) 
	begin
          supply_on("vcccorevid_1p03", 1.0); //INTEG : replace with your voltage
	end
      else if (vccRail == 1'b0) 
	begin
          supply_off("vcccorevid_1p03");
	end
      else if (vccRail === 1'bx) 
	begin
          supply_off("vcccorevid_1p03");
	end
    end
end: connectRail 	
     
 -----/\----- EXCLUDED -----/\----- */
  
//////////////////////////////////////////////
// Hierarchy-Based RTL File List Dumping ////
/////////////////////////////////////////////
   
`include  "std_ace_util.vic"

    initial begin
        dump_hier();
    end // initial begin
   
endmodule // tlm_tb
