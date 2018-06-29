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
// and confidential information of Intel or its suppliers and licensors. The
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
//   Project       : MBY
//   Description   : Package to import all the DPI functions to connect to the White model.
//------------------------------------------------------------------------------

package mby_wm_dpi_pkg;

    import "DPI-C" function wm_server_start(string server_path);
    import "DPI-C" function wm_server_stop();
    import "DPI-C" function wm_reg_write(int addr, longint val);
    import "DPI-C" function wm_reg_read(input int addr,  output longint val);
    import "DPI-C" function wm_svpkt_push(input int port,  inout byte data[], input int len);
    import "DPI-C" function wm_svpkt_get(output int port, output byte data[], output int len);
    //This function is used for debug purpose only-- To connect directly to Idea IDE and debug Scala code.
    import "DPI-C" function wm_connect(string server_path);

endpackage


// vim: noai : tw=80 : ts=3 : sw=3 : expandtab : ft=systemverilog

