// vim: noai : ts=4 : sw=4
// ---------------------------------------------------------------------------
// Copyright(C) 2014 Intel Corporation, Confidential Information
// ---------------------------------------------------------------------------
//
// Created By:  Dhivya Sankar
// Created On:  08/28/2018
// Description: MBY Saola RAL User Object
//
// ---------------------------------------------------------------------------

class mby_ral_user_object extends uvm_object;

   int burst_size; // 1/2/4/8 bytes = total 64 bits
   
   int beats; //Number of transfers within a burst

   //--------------------------------------------------------------------------
    function new(input string n = "user_object");
        super.new(n);
    endfunction : new

endclass