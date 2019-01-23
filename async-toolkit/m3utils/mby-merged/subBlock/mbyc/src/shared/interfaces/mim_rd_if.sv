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
// -- Author : Scott Hussong 
// -- Project Name : Madison Bay (MBY) 
// -- Description  : MIM read interface
//

// =====================================================================================================================

interface mim_rd_if ();
    import mby_msh_pkg::*;

    mshpt_rreq_t     msh_rreq;

    logic            msh_rd_lat_sat;
    logic            msh_crdt_rtn_for_rreq;
    logic            msh_mcast_crdt_rtn_for_rreq;
    
    logic            msh_crdt_rtn_for_rrsp;
    logic            msh_mcast_crdt_rtn_for_rrsp;

    mshpt_rrsp_t     msh_rrsp;

    msh_data_t       msh_rd_data;
    


    modport request(

        output msh_rreq,

        input  msh_rd_lat_sat,
        input  msh_crdt_rtn_for_rreq,
        input  msh_mcast_crdt_rtn_for_rreq,
    
        output msh_crdt_rtn_for_rrsp,
        output msh_mcast_crdt_rtn_for_rrsp,

        input  msh_rrsp,

        input  msh_rd_data
    
        );  

    modport receive(

        input  msh_rreq,

        output msh_rd_lat_sat,
        output msh_crdt_rtn_for_rreq,
        output msh_mcast_crdt_rtn_for_rreq,
    
        input  msh_crdt_rtn_for_rrsp,
        input  msh_mcast_crdt_rtn_for_rrsp,

        output msh_rrsp,

        output msh_rd_data
    
    );

endinterface : mim_rd_if
