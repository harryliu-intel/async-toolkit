// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description: FC AXI slave memory response sequence
// -----------------------------------------------------------------------------

class fc_axi_slave_mem_rsp_seq extends svt_axi_slave_base_sequence;

  svt_axi_slave_transaction req_resp;
  protected fc_tb_env           tb_env;

  /** UVM object utility macro */
  `uvm_object_utils(fc_axi_slave_mem_rsp_seq)

  /** class constructor */
  function new(string name="fc_axi_slave_mem_rsp_seq");
    super.new(name);
    `slu_assert($cast(tb_env, slu_utils::get_comp_by_name("tb_env")),
                ($sformatf("fc_tb_env $cast failed to %s", "tb_env")));
  endfunction

  virtual task body();
    integer status;
    svt_configuration get_cfg;

    `uvm_info("body", "entered ...", UVM_LOW)

    p_sequencer.get_cfg(get_cfg);
    //tb_env.axi_seqr.get_cfg(get_cfg);
    if (!$cast(cfg, get_cfg)) begin
      `uvm_fatal("body", "unable to $cast the configuration to a svt_axi_port_configuration class");
    end

    // consumes responses sent by driver
    sink_responses();

    forever begin
      /**
       * get the response request from the slave sequencer. the response request is
       * provided to the slave sequencer by the slave port monitor, through
       * TLM port.
       */
      p_sequencer.response_request_port.peek(req_resp);
      //tb_env.axi_seqr.response_request_port.peek(req_resp);

      /**
       * randomize the response and delays
       */
      status=req_resp.randomize with {
        bresp == svt_axi_slave_transaction::OKAY;
        foreach (rresp[index])  {
          rresp[index] == svt_axi_slave_transaction::OKAY;
          }
       };
       if(!status)
        `uvm_fatal("body","unable to randomize a response")

      /**
       * if write transaction, write data into slave built-in memory, else get
       * data from slave built-in memory
       */
      if(req_resp.get_transmitted_channel() == svt_axi_slave_transaction::WRITE) begin
        `protect      
        put_write_transaction_data_to_mem(req_resp);
        `endprotect
      end
      else if (req_resp.get_transmitted_channel() == svt_axi_slave_transaction::READ) begin
        `protect
        get_read_data_from_mem_to_transaction(req_resp);
        `endprotect
      end
    
      $cast(req,req_resp);

      /**
       * send to driver
       */
      `uvm_send(req)

    end

    `uvm_info("body", "exiting...", UVM_LOW)
  endtask: body

endclass: fc_axi_slave_mem_rsp_seq

