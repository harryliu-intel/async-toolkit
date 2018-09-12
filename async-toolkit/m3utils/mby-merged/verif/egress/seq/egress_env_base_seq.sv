//-----------------------------------------------------------------------------
// Title         : Egress env base sequence
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : egress_env_base_seq.sv
// Author        : jose.j.godinez.carrillo  <jjgodine@ichips.intel.com>
// Created       : 21.08.2018
// Last modified : 21.08.2018
//-----------------------------------------------------------------------------
// Description :
// This file contain all of the MBY base sequences.
// Class: egress_env_base_seq
// Base sequence for all the sequences.
// This base sequence setup the MBY RAL reg file pointer in the sequence which
// will be used by all the config seq to access MBY registers
// TODO: re-enable wm & sm
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//------------------------------------------------------------------------------
// Modification history :
// 21.08.2018 : created
//-----------------------------------------------------------------------------

class egress_env_base_seq extends shdv_base_seq;

  `uvm_object_utils(egress_env_base_seq)
  `uvm_declare_p_sequencer(slu_sequencer)

  slu_status_t status;
  slu_ral_data_t rd_val, wr_val;
  egress_regs_file mby;
  sla_ral_env ral;

  /*
   Function: new
   Constructor, set up the MBY RAL pointer.
   */
  function new(input string name = "egress_env_base_seq",
               uvm_sequencer_base sequencer=null, uvm_sequence parent_seq=null);
    super.new(name /*, sequencer, parent_seq*/);
    `slu_assert($cast(ral, sla_ral_env::get_ptr()), ("Unable to get handle to RAL."))
    `slu_assert($cast(mby, ral.find_file("egress_regs")), ("Unable to get handle to mby"))
  endfunction

  virtual function void sm_config();
    //sm.ag.allocate_mem(ag_result, "MMIO_LOW", 32'h2_0000, "GBE_MEM_LOW",32'h1_FFFF);
  endfunction

  //---------------------------------------------------------------------------
  // task: csr_write
  //
  // This function performs a write to a particular CSR using the front door access.
  // It also write to the same CSR in the White model.
  //
  // Arguments:
  //    sla_ral_reg csr - Register handle.
  //    sla_ral_data_t data - Data to be written.
  //    sla_ral_access_path_t access_path - "BACKDOOR" or frontdoor agent name
  //    ovm_sequence_base parent_seq - Sequence handle that performs the CSR access.
  //    boolean_t wait_for_complete - Wait until write operation complete.
  //    boolean_t ignore_access_error - Print error message if any.
  //    uvm_void/ovm_object user_object - User-customized object passed in the write call.
  //    bit predict - predict enable.
  //    sla_ral_sai_t sai - Optional SAI vector.
  //    sla_ral_root_space_t rs - Optional root space number.
  //---------------------------------------------------------------------------
  virtual task csr_write(
                         input     sla_ral_reg csr,
                         input     sla_ral_data_t data,
                         input     sla_ral_access_path_t access_path = "primary",
                         input     ovm_sequence_base parent_seq = null,
                         input     boolean_t wait_for_complete = SLA_TRUE,
                         input     boolean_t ignore_access_error = SLA_FALSE,
`ifdef SLA_RAL_UVM_USER_OBJECT
                         input     uvm_void user_object=null,
`else
                         input     ovm_object user_object=null,
`endif
                         input bit predict = 1,
                         input     sla_ral_sai_t sai = -1,
                         input     sla_ral_root_space_t rs = 0
                         );
    sla_status_t status;
    csr.write(status,
              data,
              access_path,
              parent_seq,
              wait_for_complete,
              ignore_access_error,
              user_object,
              predict,
              sai,
              rs
              );
    if (status != SLA_OK) begin
      `uvm_error("STATUS_CHECK", $sformatf("Failed writing %s", csr.get_name()))
    end

    //if(wm_reg_write(csr.get_addr_val("primary"), data))
    //`uvm_error(get_name(), "ERROR writing to WM register")

  endtask

  //---------------------------------------------------------------------------
  // task: csr_read
  //
  // This function performs a read to a particular CSR from RTL or from WM based
  // on the access_type specified.
  //
  // Arguments:
  //    sla_ral_reg csr - Register handle.
  //    sla_ral_data_t data - Read data.
  //    sla_ral_access_path_t access_path - "BACKDOOR" or frontdoor agent name
  //    ovm_sequence_base parent_seq - Sequence handle that performs the CSR access.
  //    boolean_t wait_for_complete - Wait until read operation complete.
  //    boolean_t ignore_access_error - Print error message if any.
  //    uvm_void/ovm_object user_object - User-customized object passed in the read call.
  //    bit update - update enable.
  //    sla_ral_sai_t sai - Optional SAI vector.
  //    sla_ral_root_space_t rs - Optional root space number.
  //---------------------------------------------------------------------------
  virtual task csr_read(
                        input     sla_ral_reg csr,
                                  ref sla_ral_data_t data,
                        input     sla_ral_access_path_t access_path = "primary",
                        input     ovm_sequence_base parent_seq = null,
                        input     boolean_t wait_for_complete = SLA_TRUE,
                        input     boolean_t ignore_access_error = SLA_FALSE,
`ifdef SLA_RAL_UVM_USER_OBJECT
                        input     uvm_void user_object=null,
`else
                        input     ovm_object user_object=null,
`endif
                        input bit update = 1,
                        input     sla_ral_sai_t sai = -1 ,
                        input     sla_ral_root_space_t rs = 0
                        );
    sla_status_t status;

    if(access_path.tolower() == "white_model") begin
      //if(wm_reg_read(csr.get_addr_val("primary"), data))
      //`uvm_error(get_name(), "ERROR writing to WM register")
    end
    else begin
      csr.read(status,
               data,
               access_path,
               parent_seq,
               wait_for_complete,
               ignore_access_error,
               user_object,
               update,
               sai,
               rs
               );
      if (^data === 1'bx)
        `uvm_error(get_name(),$sformatf("csr %s has XX's in data %h",csr.get_name(),data))
      if (status != SLA_OK) begin
        `uvm_error("STATUS_CHECK", $sformatf("Failed writing %s", csr.get_name()))
      end
    end
  endtask

endclass // egress_env_base_seq

