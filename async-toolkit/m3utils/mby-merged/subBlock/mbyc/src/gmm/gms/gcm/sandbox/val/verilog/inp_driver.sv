///
///  INTEL CONFIDENTIAL
///
///  Copyright 2017 Intel Corporation All Rights Reserved.
///
///  The source code contained or described herein and all documents related
///  to the source code ("Material") are owned by Intel Corporation or its
///  suppliers or licensors. Title to the Material remains with Intel
///  Corporation or its suppliers and licensors. The Material contains trade
///  secrets and proprietary and confidential information of Intel or its
///  suppliers and licensors. The Material is protected by worldwide copyright
///  and trade secret laws and treaty provisions. No part of the Material may
///  be used, copied, reproduced, modified, published, uploaded, posted,
///  transmitted, distributed, or disclosed in any way without Intel's prior
///  express written permission.
///
///  No license under any patent, copyright, trade secret or other intellectual
///  property right is granted to or conferred upon you by disclosure or
///  delivery of the Materials, either expressly, by implication, inducement,
///  estoppel or otherwise. Any license under such intellectual property rights
///  must be express and approved by Intel in writing.
///
// ---------------------------------------------------------------------------------------------------------------------
// -- Author : Jim McCormick <hess.hodge@intel.com>
// -- Project Name : ??? 
// -- Description  : This is a template for testbench code that drives
// --                the inputs of a DUT (Design Under Test). 
// ---------------------------------------------------------------------------------------------------------------------
`ifndef INP_DRIVER
`define INP_DRIVER

`include "scoreboard.sv"
`include "configuration.sv"
`include "stimulus.sv"

class inp_driver;

    stimulus                            stim;           // object for generating stimulus
    configuration                       cfg;            // config object in stim creation 
    virtual gcm_dut_if                  dut_if;         // input driver drives inputs in this interface

    shared_pkg::mby_gcm_ring_t          req_fifo_eq[$];    // a queue to hold incoming enqueue requests
    mby_gmm_pkg::mby_unicast_deque_t    req_fifo_dq[$];    // a queue to hold incoming dequeue requests
    //integer                         req_fifo[$];    // a queue to hold incoming requests

    gcm_sim_pkg::mby_mgp_id_t       i_eq_mgp_port;      // input mgp port number
    gcm_sim_pkg::mby_trk_id_t       i_eq_trk_port;      // input track port number
    gcm_sim_pkg::mby_mgp_id_t       i_dq_mgp_port;      // input mgp port number
    gcm_sim_pkg::mby_trk_id_t       i_dq_trk_port;      // input track port number
    integer                         clk_cnt;        // counts clocks
    integer                         num_reqs;       // number of input requests
    bit                             drv_done;       // set when driver is done
    string                          name;           // input driver name used in $display statements

    shared_pkg::mby_gcm_ring_t          drvr_req_eq_to_dut; 
    shared_pkg::mby_gcm_ring_t          drvr_req_eq_to_dut_p1; 
    mby_gmm_pkg::mby_unicast_deque_t    drvr_req_dq_to_dut;
    mby_gmm_pkg::mby_unicast_deque_t    drvr_req_dq_to_dut_p1;

    function new(
        gcm_sim_pkg::mby_mgp_id_t       i_eq_mgp_port,      // input mgp port number
        gcm_sim_pkg::mby_trk_id_t       i_eq_trk_port,      // input track port number
        gcm_sim_pkg::mby_mgp_id_t       i_dq_mgp_port,      // input mgp port number
        gcm_sim_pkg::mby_trk_id_t       i_dq_trk_port,      // input track port number
        virtual gcm_dut_if              dut_if, 
        configuration                   cfg
    );

        this.dut_if     = dut_if;
        this.cfg        = cfg;
        this.i_eq_mgp_port = i_eq_mgp_port;
        this.i_eq_trk_port = i_eq_trk_port;
        this.i_dq_mgp_port = i_dq_mgp_port;
        this.i_dq_trk_port = i_dq_trk_port;
        
        name        = "inp_driver.sv";
        stim = new(
            .cfg        (cfg        ),
            .i_eq_mgp_port (i_eq_mgp_port ),
            .i_eq_trk_port (i_eq_trk_port ),
            .i_dq_mgp_port (i_dq_mgp_port ),
            .i_dq_trk_port (i_dq_trk_port )
        );

        clk_cnt = 0;
    endfunction

    // reset input driver
    task reset();
        drv_done            = 1'b0;
        req_fifo_eq         = {};           // initialize to empty queue
        req_fifo_dq         = {};           // initialize to empty queue
        drvr_req_eq_to_dut  = '0;
        drvr_req_dq_to_dut  = '0;
    endtask

    // connect signal defined in input driver to DUT interface
    task connect_to_DUT_inputs();
        forever begin
            @(posedge dut_if.cclk);
            clk_cnt++;      // keep track of number of clocks
           
            // get inputs from drive_reqs() task
            drvr_req_eq_to_dut_p1 <= drvr_req_eq_to_dut;
            drvr_req_dq_to_dut_p1 <= drvr_req_dq_to_dut;

            // drive DUT inputs 
            dut_if.i_tag_to_gcm         [i_eq_mgp_port][i_eq_trk_port] = drvr_req_eq_to_dut_p1;
            dut_if.i_mby_deque_from_egr [i_dq_mgp_port][i_dq_trk_port] = drvr_req_dq_to_dut_p1;
            dut_if.i_mgp_enabled        [i_eq_mgp_port] = 1'b1;
            
            //dut_if.i_tag_to_gcm         [1][i_eq_trk_port] = drvr_req_eq_to_dut_p1;
            //dut_if.i_mby_deque_from_egr [1][i_dq_trk_port] = drvr_req_dq_to_dut_p1;
            //dut_if.i_mgp_enabled        [1] = 1'b1;
    
        end

    endtask

    // generate a specified number of requests and load them into testbench request FIFOs
    task load_stimulus(integer num_reqs);
        this.num_reqs = num_reqs;

        //while (this.num_reqs > 0) begin
        while (this.num_reqs >= 0) begin
            if (this.num_reqs == 0) begin
                stim.randomize();               // create a random request 
                req_fifo_eq.push_back('0);   // put random request into FIFO
                req_fifo_dq.push_back('0);   // put random request into FIFO
                this.num_reqs--;
            end
            else begin                
                stim.randomize();               // create a random request 
                req_fifo_eq.push_back(stim.req_eq);   // put random request into FIFO
                req_fifo_dq.push_back(stim.req_dq);   // put random request into FIFO
                this.num_reqs--;
            end
        end

`ifdef INP_DRIVER_DEBUG
        foreach (req_fifo_eq[idx])
             $display("(time: %0d)  INP_DRIVER_DEBUG: GENERATED REQUEST (i_eq_mgp_port %0d): (i_eq_trk_port %0d): vld=%d, outp=%d, data=%x", $time, i_eq_mgp_port, i_eq_trk_port, req_fifo_eq[idx].vld, req_fifo_eq[idx].outp,  req_fifo_eq[idx].data);
        
        foreach (req_fifo_dq[idx])
             $display("(time: %0d)  INP_DRIVER_DEBUG: GENERATED REQUEST (i_dq_mgp_port %0d): (i_dq_trk_port %0d): vld=%d, outp=%d, data=%x", $time, i_dq_mgp_port, i_dq_trk_port, req_fifo_dq[idx].vld, req_fifo_dq[idx].outp,  req_fifo_dq[idx].data);
`endif  // INP_DRIVER_DEBUG

    endtask

    // Drive requests into DUT (template)
    task drive_reqs();

        while (something_to_do()) begin
            @(posedge dut_if.cclk);

            if (req_fifo_eq.size() != 0) begin
                drvr_req_eq_to_dut = req_fifo_eq.pop_front();
            end else
                drvr_req_eq_to_dut = '0; 
            
            if (req_fifo_dq.size() != 0) begin
                drvr_req_dq_to_dut = req_fifo_dq.pop_front();
            end else
                drvr_req_dq_to_dut = '0; 
        end

        //$display("(time: %0d) %s: ** Done Driving Requests to Inputs (iport %0d) ** ", $time, name, iport);
        $display("(time: %0d) %s: ** Done Driving Requests to Inputs (i_eq_mgp_port %0d) (i_eq_trk_port %0d) (i_dq_mgp_port %0d) (i_dq_trk_port %0d) ** ", $time, name, i_eq_mgp_port, i_eq_trk_port, i_dq_mgp_port, i_dq_trk_port);

        @(posedge dut_if.cclk);

        drv_done = 1'b1;
    endtask

    // figure out if input driver is done or not
    function bit something_to_do();
        something_to_do = '0;
        if ( (req_fifo_eq.size() != 0) ||  (req_fifo_dq.size() != 0) ) something_to_do = '1;
        //if (drvr_req_eq_to_dut.vld || drvr_req_eq_to_dut_p1.vld) something_to_do = 1;
        return something_to_do;
    endfunction

endclass

`endif // INP_DRIVER
