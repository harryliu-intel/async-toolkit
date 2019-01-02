//------------------------------------------------------------------------------
//
//  INTEL CONFIDENTIAL
//
//  Copyright 2006 - 2017 Intel Corporation All Rights Reserved.
//
//  The source code contained or described herein and all documents related
//  to the source code ("Material") are owned by Intel Corporation or its
//  suppliers or licensors. Title to the Material remains with Intel
//  Corporation or its suppliers and licensors. The Material contains trade
//  secrets and proprietary and confidential information of Intel or its
//  suppliers and licensors. The Material is protected by worldwide copyright
//  and trade secret laws and treaty provisions. No part of the Material may
//  be used, copied, reproduced, modified, published, uploaded, posted,
//  transmitted, distributed, or disclosed in any way without Intel's prior
//  express written permission.
//
//  No license under any patent, copyright, trade secret or other intellectual
//  property right is granted to or conferred upon you by disclosure or
//  delivery of the Materials, either expressly, by implication, inducement,
//  estoppel or otherwise. Any license under such intellectual property rights
//  must be express and approved by Intel in writing.
//
//------------------------------------------------------------------------------

// This files contains common custom SVA properties assertions and assumptions
//  that can be `included at the top of a module's *_mon.sva file.
// This library is meant to supplement Intel's SVA_LIB and may use some of
//  SVA_LIB's properties as building blocks.

`ifndef INTEL_SVA_OFF
`ifndef HLP_CHECKERS_EXT_MACROS_VS
`define HLP_CHECKERS_EXT_MACROS_VS

  //////////////////////////////////////////////////////////////////////////////
  // Defined Expressions

`define default_sva_clk posedge clk
`define default_sva_rst rst_n!==1'b1


  //////////////////////////////////////////////////////////////////////////////
  // Properties

  // reset checks
  //   check that a signal matches a certain value when reset was asserted
  property p_reset_check_value(sig, value, clk, rst);  
    @(clk) disable iff (0)
      $fell(rst) |-> ($past(sig)===value);
  endproperty


  // delayed (pipelined) data matching
  //   make sure the output data matches the input data after a set delay
  property p_delay(delay, data_in, data_out, clk, rst);
    @(clk) disable iff (rst)
      ##delay (data_out===$past(data_in,delay));
  endproperty
  //   same as p_delay, but triggered only when valid condition is asserted
  property p_delay_v(delay, v_in, data_in, data_out, clk, rst);
    @(clk) disable iff (rst)
      ##delay $past(v_in,delay) |-> (data_out===$past(data_in,delay));
  endproperty
  //   check for output clockgating when valid condition is not asserted
  property p_delay_cg(delay, v_in, data_out, clk, rst);
    @(clk) disable iff (rst)
      ##delay $past(!v_in,delay) |-> $stable(data_out);
  endproperty

  // no delay (combinational) data matching
  property p_no_delay(data_in, data_out, clk, rst);
    @(clk) disable iff (rst)
      (data_out===data_in);
  endproperty


  // valid+enable interface (ve_intf)
  //   make sure an asserted valid and data do not change if enable is low
  property p_ve_intf_stable(v, e, data, clk, rst);
    @(clk) disable iff (rst)  v && !e |=> v && $stable(data);
  endproperty


  // sop+eop interface (seop_intf) per port
  //  every sop will eventually be followed by an eop
  property p_seop_intf_completion(sop, eop, v, clk, rst);
    @(clk) disable iff (rst)
      (sop && v) |-> ##[0:$] (eop && v);
  endproperty
  //  if sop already, cannot have another sop before eop
  property p_seop_intf_unique_sop(sop, eop, v, clk, rst);
    @(clk) disable iff (rst)
      (sop && v && !eop) |=> !(sop && v)[*0:$] ##0 (eop && v && !sop);
  endproperty
  //  cannot have an eop without an sop
  property p_seop_intf_sop_first(sop, eop, v, clk, rst);
    @(clk) disable iff (rst)
      $past(rst) || (eop && v) |=> !(eop && v && !sop)[*0:$] ##0 (sop && v);
  endproperty



  //////////////////////////////////////////////////////////////////////////////
  // Assertions & Assumptions (sequential only)
  //  The defaults for the clock and reset conditions are defined above,
  //  so they do not need to be explicitly stated in most use cases.
  //  The error message is also optional
  //
  //  Usage examples:
  //    `HLP_ASRT_DELAY_V(asrt_data_passthru_v, 3, i_data_v, i_data, o_data);
  //    `HLP_ASRT_DELAY_V(asrt_data_passthru_v, 3, i_data_v, i_data, o_data,
  //                      "Data passthrough mismatch", negedge d_clk, d_rst);
  //

  // reset checks
`define HLP_ASRT_RESET_VALUE(name, sig, value, err_msg="Value mismatch at reset", clk=`default_sva_clk, rst=`default_sva_rst) \
  name: assert property(p_reset_check_value(sig, value, clk, rst)) else $error(err_msg);

  // delayed (pipelined) data matching
  //    only use with FPV; otherwise, disabled with empty macro
`ifdef HLP_FPV_RESTRICT
`define HLP_ASSM_DELAY(name, delay, data_in, data_out, err_msg="", clk=`default_sva_clk, rst=`default_sva_rst) \
  name: assume property(p_delay(delay, data_in, data_out, clk, rst));
`define HLP_ASSM_DELAY_V(name, delay, v_in, data_in, data_out, err_msg="", clk=`default_sva_clk, rst=`default_sva_rst) \
  name: assume property(p_delay_v(delay, v_in, data_in, data_out, clk, rst));
`define HLP_ASSM_DELAY_CG(name, delay, v_in, data_out, err_msg="", clk=`default_sva_clk, rst=`default_sva_rst) \
  name: assume property(p_delay_cg(delay, v_in, data_out, clk, rst));
`define HLP_ASSM_DELAY_V_CG(name, delay, v_in, data_in, data_out, err_msg="", clk=`default_sva_clk, rst=`default_sva_rst) \
  name``_delay_v: assume property(p_delay_v(delay, v_in, data_in, data_out, clk, rst)); \
  name``_delay_cg: assume property(p_delay_cg(delay, v_in, data_out, clk, rst));
`define HLP_ASSM_NO_DELAY(name, data_in, data_out, err_msg="", clk=`default_sva_clk, rst=`default_sva_rst) \
  name: assume property(p_no_delay(data_in, data_out, clk, rst));
`define HLP_ASRT_DELAY(name, delay, data_in, data_out, err_msg="Data pipeline mismatch", clk=`default_sva_clk, rst=`default_sva_rst) \
  name: assert property(p_delay(delay, data_in, data_out, clk, rst)) else $error(err_msg);
`define HLP_ASRT_DELAY_V(name, delay, v_in, data_in, data_out, err_msg="Data pipeline mismatch", clk=`default_sva_clk, rst=`default_sva_rst) \
  name: assert property(p_delay_v(delay, v_in, data_in, data_out, clk, rst)) else $error(err_msg);
`define HLP_ASRT_DELAY_CG(name, delay, v_in, data_out, err_msg="Data pipeline mismatch", clk=`default_sva_clk, rst=`default_sva_rst) \
  name: assert property(p_delay_cg(delay, v_in, data_out, clk, rst)) else $error(err_msg);
`define HLP_ASRT_DELAY_V_CG(name, delay, v_in, data_in, data_out, err_msg="Data pipeline mismatch", clk=`default_sva_clk, rst=`default_sva_rst) \
  name``_delay_v: assert property(p_delay_v(delay, v_in, data_in, data_out, clk, rst)) else $error(err_msg); \
  name``_delay_cg: assert property(p_delay_cg(delay, v_in, data_out, clk, rst)) else $error(err_msg);
`define HLP_ASRT_NO_DELAY(name, data_in, data_out, err_msg="Data combinational mismatch", clk=`default_sva_clk, rst=`default_sva_rst) \
  name: assert property(p_no_delay(data_in, data_out, clk, rst)) else $error(err_msg);
`else
`define HLP_ASSM_DELAY(name, delay, data_in, data_out, err_msg=, clk=, rst=)
`define HLP_ASSM_DELAY_V(name, delay, v, data_in, data_out, err_msg=, clk=, rst=)
`define HLP_ASSM_DELAY_CG(name, delay, v, data_out, err_msg=, clk=, rst=)
`define HLP_ASSM_DELAY_V_CG(name, delay, v, data_in, data_out, err_msg=, clk=, rst=)
`define HLP_ASSM_NO_DELAY(name, data_in, data_out, err_msg=, clk=, rst=)
`define HLP_ASRT_DELAY(name, delay, data_in, data_out, err_msg=, clk=, rst=)
`define HLP_ASRT_DELAY_V(name, delay, v, data_in, data_out, err_msg=, clk=, rst=)
`define HLP_ASRT_DELAY_CG(name, delay, v, data_out, err_msg=, clk=, rst=)
`define HLP_ASRT_DELAY_V_CG(name, delay, v, data_in, data_out, err_msg=, clk=, rst=)
`define HLP_ASRT_NO_DELAY(name, data_in, data_out, err_msg=, clk=, rst=)
`endif // HLP_FPV_RESTRICT

  // valid+enable interface (ve_intf)
`define HLP_ASRT_VE_INTF_STABLE(name, v, e, data, err_msg="valid+enable interface: valid and/or data not stable", clk=`default_sva_clk, rst=`default_sva_rst) \
  name: assert property(p_ve_intf_stable(v, e, data, clk, rst)) else $error(err_msg);
`define HLP_ASSM_VE_INTF_STABLE(name, v, e, data, err_msg="valid+enable interface: valid and/or data not stable", clk=`default_sva_clk, rst=`default_sva_rst) \
  name: assume property(p_ve_intf_stable(v, e, data, clk, rst)) else $error(err_msg);

  // sop+eop interface (seop_intf) - separate checking per port
`define HLP_ASRT_SEOP_INTF(name, N_PORTS, port, sop, eop, v, err_msg="start/end of packet interface violated", clk=`default_sva_clk, rst=`default_sva_rst) \
  generate \
    for (genvar i=0; i<N_PORTS; i+=1) begin : gen_asrt_seop_intf_per_port \
      name``_completion: assert property(p_seop_intf_completion(sop, eop, (v && (port==i)), clk, rst)) else $error(err_msg); \
      name``_unique_sop: assert property(p_seop_intf_unique_sop(sop, eop, (v && (port==i)), clk, rst)) else $error(err_msg); \
      name``_sop_first:  assert property(p_seop_intf_sop_first (sop, eop, (v && (port==i)), clk, rst)) else $error(err_msg); \
    end \
  endgenerate
`define HLP_ASSM_SEOP_INTF(name, N_PORTS, port, sop, eop, v, err_msg="start/end of packet interface violated", clk=`default_sva_clk, rst=`default_sva_rst) \
  generate \
    for (genvar i=0; i<N_PORTS; i+=1) begin : gen_assm_seop_intf_per_port \
      name``_completion: assume property(p_seop_intf_completion(sop, eop, (v && (port==i)), clk, rst)) else $error(err_msg); \
      name``_unique_sop: assume property(p_seop_intf_unique_sop(sop, eop, (v && (port==i)), clk, rst)) else $error(err_msg); \
      name``_sop_first:  assume property(p_seop_intf_sop_first (sop, eop, (v && (port==i)), clk, rst)) else $error(err_msg); \
    end \
  endgenerate


`endif // HLP_CHECKERS_EXT_MACROS_VS
`endif // INTEL_SVA_OFF
