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
//------------------------------------------------------------------------------
// -- Author : Luis Alfonso Maeda-Nunez
// -- Project Name : Madison Bay (MBY) 
// -- Description  : TQU to TCU Interface
//                   For connecting the Transmit Queuing Unit to the Transmit
//                   Controller Unit
//------------------------------------------------------------------------------

interface egr_tcu_tqu_if ();

    logic dummy;
    //TODO data word t            [3:0] //
    //TODO data ecc??             [3:0] //TODO Discuss ecc bits
    //TODO data valid             [3:0] //
    //TODO pop queue t            [3:0] // One per queue: 36 TCU->TQU
    //TODO data ready t           [3:0] // One per queue: 36 TQU->TCU
    
    //TODO metadata t             [3:0] //
    //TODO metadata ecc?          [3:0] //TODO Discuss ecc bits
    //TODO metadata valid         [3:0] //
    //TODO local metadata pop t   [3:0] // One per queue: 36 TCU->TQU
    //TODO local metadata ready t [3:0] // One per queue: 36 TQU->TCU
    //TODO local metadata contents??
    //TODO mesh metadata pop t    [3:0] // One per queue: 36 TCU->TQU
    //TODO mesh  metadata ready t [3:0] // One per queue: 36 TQU->TCU
    //TODO mesh  metadata contents??

    //TODO Discuss protocol for syncing given pipeline between blocks

modport tqu(
    output dummy
    );

modport tcu(
    input dummy
    );

endinterface : egr_tcu_tqu_if

