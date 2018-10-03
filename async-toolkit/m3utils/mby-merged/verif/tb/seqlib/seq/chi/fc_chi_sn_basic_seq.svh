// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  CHI slave node directed basic sequence. provide SN response to 
//                 the CHI SN agent present in the CHI system env.
//                 the sequence receives a response object of type svt_chi_sn_transaction 
//                 from SN sequencer. the sequence class then updates the response fields
//                 and provides it to the SN protocol layer driver within the SN agent.
// -------------------------------------------------------------------------------

class fc_chi_sn_basic_seq extends svt_chi_sn_transaction_base_sequence;
  /*  response request from the SN sequencer */
  svt_chi_sn_transaction req_resp;

  /* port configuration obtained from the sequencer */
  svt_chi_node_configuration cfg;

  /** UVM object utility macro */
  `uvm_object_utils(fc_chi_sn_basic_seq)
  
  /** class constructor */
  function new(string name="fc_chi_sn_basic_seq");
    super.new(name);
    
  endfunction

  virtual task body();
    svt_configuration get_cfg;
    
    `uvm_info("body", "entered ...", UVM_LOW)

    p_sequencer.get_cfg(get_cfg);
    if (!$cast(cfg, get_cfg)) begin
      `uvm_fatal("body", "unable to $cast the configuration to a svt_chi_port_configuration class");
    end

    /** 
     * this method is defined in the svt_chi_sn_transaction_base_sequence.
     * it obtains the virtual sequencer sn_virt_seqr of type svt_chi_sn_virtual_sequencer 
     * from the configuration database and sets up the shared resources obtained from it: the response_request_port.
     * 
     **/ 
    get_sn_virt_seqr();

    /** this method is defined in the svt_chi_sn_transaction_base_sequence.
     * used to sink the responses from the response queue.
     **/
    sink_responses();

    forever begin
      /**
       * get the response request from the SN sequencer. the response request is
       * provided to the SN sequencer by the SN protocol layer monitor, through
       * TLM port.
       */
      wait_for_response_request(req_resp);

      /**
       * set the SN response type.
       * for read_no_snp, the xact_rsp_msg_type is set to RSP_MSG_COMPDATA.
       * o this makes the SN to transmit comp_data message(s).
       * for write_no_snp, the xact_rsp_msg_type is set to RSP_MSG_COMPDBIDRESP.
       * o this makes the SN to transmit compDBIDResp message.
       */
      if (req_resp.xact_type == svt_chi_sn_transaction::READNOSNP) begin
        for (int i=0; i<8; i++)
          req_resp.data[64*i+:64] = 64'hdead_beef_feed_beef;

        // construct dat_rsvdc to accomodate for the number of tx DAT
        // flits associated to this transaction and then setup the values.
        req_resp.dat_rsvdc = new[req_resp.compute_num_dat_flits()];
        foreach (req_resp.dat_rsvdc[idx])
          req_resp.dat_rsvdc[idx] = (idx+1);
        
        req_resp.xact_rsp_msg_type = svt_chi_sn_transaction::RSP_MSG_COMPDATA;
      end
      else if ((req_resp.xact_type == svt_chi_sn_transaction::WRITENOSNPFULL) ||
               (req_resp.xact_type == svt_chi_sn_transaction::WRITENOSNPPTL)) begin
        req_resp.xact_rsp_msg_type =  svt_chi_sn_transaction::RSP_MSG_COMPDBIDRESP;
      end

      $cast(req,req_resp);

      /**
       * send to driver
       */
      `uvm_info("body", $sformatf("sending %0s response to transaction %0s", req_resp.xact_rsp_msg_type.name(), `SVT_CHI_PRINT_PREFIX(req_resp)), UVM_LOW);
      `uvm_send(req)

    end

    `uvm_info("body", "exiting...", UVM_LOW)
  endtask: body

endclass: fc_chi_sn_basic_seq

