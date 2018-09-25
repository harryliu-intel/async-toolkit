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
//=======================================================================
// COPYRIGHT (C)  2012 SYNOPSYS INC.
// This software and the associated documentation are confidential and
// proprietary to Synopsys, Inc. Your use or disclosure of this software
// is subject to the terms and conditions of a written license agreement
// between you, or your company, and Synopsys, Inc. In the event of
// publications, the following notice is applicable:
//
// ALL RIGHTS RESERVED
//
// The entire notice above must be reproduced on all authorized copies.
//
//------------------------------------------------------------------------------
//   Author        : Dhivya Sankar
//   Project       : Madison Bay
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//Class:    cust_svt_ahb_system_configuration
//
//
//Class cust_svt_ahb_system_configuration is basically used to encapsulate all
//the configuration information.  It extends system configuration and set the
//appropriate fields like number of master/slaves, create master/slave
//configurations etc..., which are required by System Env.
//------------------------------------------------------------------------------

class cust_svt_ahb_system_configuration extends svt_ahb_system_configuration;

   /** UVM Object Utility macro */
   `uvm_object_utils_begin (cust_svt_ahb_system_configuration)
      `uvm_field_int(num_masters,    UVM_DEFAULT)
      `uvm_field_int(num_slaves,    UVM_DEFAULT)
    `uvm_object_utils_end

   /** Class Constructor */
   function new (string name = "cust_svt_ahb_system_configuration");

      super.new(name);
      
   endfunction 
 
   

endclass