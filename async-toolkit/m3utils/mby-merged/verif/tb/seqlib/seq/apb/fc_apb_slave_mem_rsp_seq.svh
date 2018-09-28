// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description: FC APB slave memory response sequence
// -----------------------------------------------------------------------------

class fc_apb_slave_mem_rsp_seq extends svt_apb_slave_base_sequence;

  /** UVM object utility macro */
  `uvm_object_utils(fc_apb_slave_mem_rsp_seq)

  /** class constructor */
  function new(string name="fc_apb_slave_mem_rsp_seq");
    super.new(name);
  endfunction

  virtual task body();
    `uvm_info("body", "entered ...", UVM_LOW)

    forever begin
      p_sequencer.response_request_port.peek(req);
      if (req.cfg == null) begin
        req.cfg = cfg;
      end

      /** 
       * demonstration of response randomization with constraints.
       */
      `uvm_rand_send_with(req, { pslverr_enable == 1'b0; })
    end

    `uvm_info("body", "exiting...", UVM_LOW)
  endtask: body

endclass: fc_apb_slave_mem_rsp_seq

