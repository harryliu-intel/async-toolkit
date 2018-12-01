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
///  ---------------------------------------------------------------------------------------------------------------------
///  -- Author       : Isaac Perez-Andrade
///  -- Project Name : Madison Bay (MBY) 
///  -- Description  : Multicast Tag Manager (MTM) interface with
///                    Packet Fetch Scheduler (PFS)
///  ------------------------------------------------------------------------------

interface mtm_pfs_if();
    // signals
    logic dummy;
    
// MTM provides to PFS
modport mtm(
    // port list
    output dummy
    );

// PFS consumes from MTM
modport pfs(
    // port list
    input dummy
    );

endinterface : mtm_pfs_if
