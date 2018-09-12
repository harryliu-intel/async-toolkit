
/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:    mby_im_env.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : MBY IP
   ----------------------------------------------------------------------
 
  MBY Soala IM imp file

  This file implemnet the MBY IP saola IM.
 
 This file hold the IP ISR registerations and IP specific stuff.
 
 SOC don't reuse this file by default


*/

/*
 Class: MBY_im_env

 MBY Saola IM implementation for MBY IP


 */

class mby_im_env extends slu_im_env;

    `uvm_component_utils(mby_im_env)

    
   /** Constructor. */
    function new(string name = "mby_im_env", uvm_component parent);
        super.new(name, parent);
    endfunction // new
  

    function void connect_phase(uvm_phase phase);
        
        super.connect_phase(phase);;
    endfunction // void

  /*
   
   Function: mby_im_env build
   
   In this function the IP connect its interrupts seq to the IM
   */
   
  virtual function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    add_intr_info("MBY_INT",//interupt name
                  1,//priority - high number == high priority
                  1,//enable=1, disable = 0
                  "env",//sequencer to run on
                  "mby_isr_seq"//sequence to run upon this interrupt
 		);

  endfunction // void
  
endclass : mby_im_env
