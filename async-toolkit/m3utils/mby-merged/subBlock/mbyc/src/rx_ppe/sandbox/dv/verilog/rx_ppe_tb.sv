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
// -------------------------------------------------------------------
// -- Intel Proprietary
// -- Copyright (C) 2018 Intel Corporation
// -- All Rights Reserved
// -------------------------------------------------------------------
// -- Author : Jon Bagge <jon.bagge@intel.com>
// -- Project Name : Madison Bay
// -- Description : RX PPE top level verification file
// --
// -------------------------------------------------------------------
`timescale 1ps/1ps

module rx_ppe_tb
();

igr_rx_ppe_if       igr_rx_ppe_if();
rx_ppe_igr_if       rx_ppe_igr_if();
rx_ppe_ppe_stm0_if  rx_ppe_ppe_stm0_if();
rx_ppe_ppe_stm1_if  rx_ppe_ppe_stm1_if();
ahb_rx_ppe_if       ahb_rx_ppe_if();
glb_rx_ppe_if       glb_rx_ppe_if();

rx_ppe  rx_ppe (
    .cclk               (1'b0),
    .reset              (1'b0),
    .igr_rx_ppe_if      (igr_rx_ppe_if),
    .rx_ppe_igr_if      (rx_ppe_igr_if),
    .rx_ppe_ppe_stm0_if (rx_ppe_ppe_stm0_if),
    .rx_ppe_ppe_stm1_if (rx_ppe_ppe_stm1_if),
    .ahb_rx_ppe_if      (ahb_rx_ppe_if),
    .glb_rx_ppe_if      (glb_rx_ppe_if)
);

endmodule
