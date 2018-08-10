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

//   Class:    mby_mc_ral_env
//
//   This is the Mplex Saola RAL Environment file.
//   This creates and connects the components.

`ifndef __MBY_MC_RAL_ENV_GUARD
`define __MBY_MC_RAL_ENV_GUARD


`ifndef __INSIDE_MBY_MC_ENV_PKG
`error "Attempt to include file outside of mby_mc_env_pkg."
`endif

typedef class mby_mc_env;

class mby_mc_ral_env extends sla_ral_env;

    // Variable: cfg
    // Handle to the Top Configuration
    mby_mc_tb_top_cfg   cfg;

    `ovm_component_utils(mby_mc_env_pkg::mby_mc_ral_env)

    // ------------------------------------------------------------------------
    //  Constructor: new
    //
    //  Arguments:
    //  name     - Mplex TOP RAL environment object name.
    //  parent   - Component parent object.
    //  hdl_path - String - HDl Path name.
    // ------------------------------------------------------------------------
    function new(string name = "mby_mc_ral_env", ovm_component parent = null, string hdl_path = "");
        super.new(name, parent, hdl_path);
    endfunction: new

    // ------------------------------------------------------------------------
    //  Function: connect
    //  create the EC RAL environment object.
    // ------------------------------------------------------------------------
    virtual function void           connect();
        mby_mc_env env;
        super.connect();

        $cast(env, this.get_slu_tb_env_parent());
        cfg = env.get_tb_cfg();

        this.set_variant_check(1'b0);
        this.set_bit_blasting(1);
        
    endfunction: connect

    // ------------------------------------------------------------------------
    //  Function: end_of_elaboration
    //  Set the Ral sequence type.
    // ------------------------------------------------------------------------
    virtual function void end_of_elaboration();
        super.end_of_elaboration();
        if (_level == SLA_TOP) begin
        //set_ral_seq("crk_oc_seq_pkg::crk_ral_csr_chain_seq", "frontdoor");
        //set_ral_seq("crk_oc_seq_pkg::crk_ral_csr_chain_seq", "FRONTDOOR");
        end
    endfunction: end_of_elaboration

    // ------------------------------------------------------------------------
    //  Function: set_ral_seq
    //  Custom function for register frontdoor sequence as all frontdoor sequence
    //  supports both read and write
    //
    //  Arguments:
    //  seq_name    -  Ral sequence name
    //  access_type -  Ral Access Type
    // ------------------------------------------------------------------------
    virtual function void set_ral_seq(string seq_name, string access_type);
        slu_tb_env env;
        bit rc;

        rc = $cast(env, this.get_parent());
        `slu_assert(rc, ("%-s : Unable to add sequencer agent to top SLA_TB_ENV", get_type_name()))

        set_frontdoor_seq_type(access_type, "read",  seq_name);
        set_frontdoor_seq_type(access_type, "write", seq_name);
    endfunction: set_ral_seq

    // ------------------------------------------------------------------------
    //  Function: get_addr_val
    //
    //  Arguments:
    //  access_path -  Access Path definition
    //  r           -  This is the sla_ral_reg
    // ------------------------------------------------------------------------
    virtual function sla_ral_addr_t get_addr_val(sla_ral_access_path_t access_path, sla_ral_reg r);
        if(access_path == "primary") begin
            // Use space to define how to calculate the address
            case(r.get_space())
                "CFG" : return(r.get_space_addr("CFG") | (r.get_func_num() <<16) | (r.get_dev_num() <<19) | (r.get_bus_num() <<24));
                "MEM" : begin
                    if(r.base_addr_reg !== null) begin
                        return(r.get_base_addr_val() + r.get_space_addr("MEM"));
                    end
                    else begin                            // Fixed address register don't have BAR
                        return(r.get_space_addr("MEM"));
                    end
                end
                "IO" :begin
                    if(r.base_addr_reg !== null) begin    // IO BAR bit 0 is 1. We need to clear the LSB bit of the BAR
                        sla_ral_data_t tmp_addr;
                        tmp_addr = r.get_base_addr_val();
                        tmp_addr[0:0] = 1'b0;              // init with zero the RTE
                        return (tmp_addr  + r.get_space_addr("IO"));
                    end
                    else begin                            // Fixed address register don't have BAR
                        return(r.get_space_addr("IO"));
                    end
                end
            endcase // case(r.get_space())
        end
        else if (access_path == "sideband") begin
            return(r.get_space_addr("MSG"));
        end
        else begin
            ovm_report_fatal (get_name(), $psprintf("Unsupport access type %s",access_path));
        end
    endfunction: get_addr_val

endclass: mby_mc_ral_env

`endif // __MBY_MC_RAL_ENV_GUARD
