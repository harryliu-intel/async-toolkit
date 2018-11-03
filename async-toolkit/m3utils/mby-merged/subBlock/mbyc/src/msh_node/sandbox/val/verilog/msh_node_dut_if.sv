///
///  INTEL CONFIDENTIAL
///
///  Copyright 2018 Intel Corporation All Rights Reserved.
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
// -- Author : Jim McCormick <jim.mccormick@intel.com>
// -- Project Name : Madison Bay (MBY)
// -- Description  : The Mesh DUT interface 
// ---------------------------------------------------------------------------------------------------------------------

// This is a connectivity only interface and thus is just a named bundle of nets that can be passed around as a group 
// to save typing.  Anywhere an interface is defined, individual nets in the interface can be referenced as follows:    
//
//      <interface name>.<net name>
//

interface msh_node_dut_if (
   input mclk                                        // mclk is passed in a parameter and becomes part of the interface
);


// local paramters


// DUT inputs  (direction not specified in this interface)
logic               i_reset;                                // reset


// DUT outputs  (direction not specified in this interface)


endinterface // msh_node_dut_if
