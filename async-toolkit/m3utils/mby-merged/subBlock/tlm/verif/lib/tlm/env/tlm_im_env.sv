
/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:    tlm_im_env.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : TLM1 IP
   ----------------------------------------------------------------------
 
  TLM1 Soala IM imp file

  This file implemnet the TLM1 IP saola IM.
 
 This file hold the IP ISR registerations and IP specific stuff.
 
 SOC don't reuse this file by default


*/

/*
 Class: TLM1_im_env

 TLM1 Saola IM implementation for TLM1 IP


 */

class tlm_im_env extends slu_im_env;

    `uvm_component_utils(tlm_im_env)

    
   /** Constructor. */
    function new(string name = "tlm_im_env", uvm_component parent);
        super.new(name, parent);
    endfunction // new
  

    function void connect_phase(uvm_phase phase);
        
        super.connect_phase(phase);;
    endfunction // void

  /*
   
   Function: tlm_im_env build
   
   In this function the IP connect its interrupts seq to the IM
   */
   
  virtual function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    add_intr_info("TLM1_INT",//interupt name
                  1,//priority - high number == high priority
                  1,//enable=1, disable = 0
                  "env",//sequencer to run on
                  "tlm_isr_seq"//sequence to run upon this interrupt
 		);

  endfunction // void
  
endclass : tlm_im_env
