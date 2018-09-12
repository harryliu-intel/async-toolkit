// vim: noai : ts=3 : sw=3 : expandtab : ft=systemverilog

//------------------------------------------------------------------------------
//
// INTEL CONFIDENTIAL
//
// Copyright 2018 Intel Corporation All Rights Reserved.
//
// The source code contained or described herein and all documents related to
// the source code ("Material") are owned by Intel Corporation or its suppliers
// or licensors. Title to the Material remains with Intel Corporation or its
// suppliers and licensors. The Material contains trade secrets and proprietary
// and confidential information of Intel or its suppliers and licensors.  The
// Material is protected by worldwide copyright and trade secret laws and
// treaty provisions. No part of the Material may be used, copied, reproduced,
// modified, published, uploaded, posted, transmitted, distributed, or
// disclosed in any way without Intel's prior express written permission.
//
// No license under any patent, copyright, trade secret or other intellectual
// property right is granted to or conferred upon you by disclosure or delivery
// of the Materials, either expressly, by implication, inducement, estoppel or
// otherwise. Any license under such intellectual property rights must be
// express and approved by Intel in writing.
//
//------------------------------------------------------------------------------
//   Author        : Akshay Kotian
//   Project       : Madison Bay
//------------------------------------------------------------------------------


`ifndef __MBY_BASE_SEQ_GUARD
`define __MBY_BASE_SEQ_GUARD


`ifndef __INSIDE_MBY_COMMON_PKG
`error "Attempt to include file outside of mby_common_pkg."
`endif

//   Class:  mby_base_seq
//
//   This is the Base MBY sequence which extends from shdv_base_seq to add
//   register read and write methods to access White model registers.
//
//   All CTE's base sequences should extend from this sequence to inherit WM
//   register access methods.
//
class mby_base_seq extends shdv_base_seq;
    `uvm_object_utils(mby_base_seq)
    `uvm_declare_p_sequencer(slu_sequencer)

    slu_status_t status;
    slu_ral_data_t rd_val, wr_val;
    //mby_regs_file mby;
    //sla_ral_env ral;

    //  Constructor: new
    //  New mby_base_seq Object.
    //  Arguments:
    //  name       - MBY base sequence object name.
    function new(input string name = "mby_base_seq");
        super.new(name);

    //     `slu_assert($cast(ral, sla_ral_env::get_ptr()), ("Unable to get handle to RAL."))
    //
    //     `slu_assert($cast(mby, ral.find_file("mby_regs")), ("Unable to get handle to mby"))
    endfunction
    //
    //virtual function void sm_config();
    //    sm.ag.allocate_mem(ag_result, "MMIO_LOW", 32'h2_0000, "GBE_MEM_LOW",32'h1_FFFF);
    //endfunction


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
            input  sla_ral_reg           csr,
            input  sla_ral_data_t        data,
            input  sla_ral_access_path_t access_path = "primary",
            input  ovm_sequence_base     parent_seq = null,
            input  boolean_t             wait_for_complete = SLA_TRUE,
            input  boolean_t             ignore_access_error = SLA_FALSE,
      `ifdef SLA_RAL_UVM_USER_OBJECT
            input  uvm_void              user_object=null,
      `else
            input  ovm_object            user_object=null,
      `endif
            input  bit                   predict = 1,
            input  sla_ral_sai_t         sai = -1,
            input  sla_ral_root_space_t  rs = 0
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

        if(wm_reg_write(csr.get_addr_val("primary"), data))
            `uvm_error(get_name(), "ERROR writing to WM register")

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
            input sla_ral_reg           csr,
            ref   sla_ral_data_t        data,
            input sla_ral_access_path_t access_path = "primary",
            input ovm_sequence_base     parent_seq = null,
            input  boolean_t            wait_for_complete = SLA_TRUE,
            input  boolean_t            ignore_access_error = SLA_FALSE,
      `ifdef SLA_RAL_UVM_USER_OBJECT
            input  uvm_void             user_object=null,
      `else
            input  ovm_object           user_object=null,
      `endif
            input  bit                  update = 1,
            input  sla_ral_sai_t        sai = -1 ,
            input  sla_ral_root_space_t rs = 0
        );
        sla_status_t status;

        if(access_path.tolower() == "white_model") begin
            if(wm_reg_read(csr.get_addr_val("primary"), data))
                `uvm_error(get_name(), "ERROR writing to WM register")
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

endclass

`endif //__MBY_BASE_SEQ_GUARD

