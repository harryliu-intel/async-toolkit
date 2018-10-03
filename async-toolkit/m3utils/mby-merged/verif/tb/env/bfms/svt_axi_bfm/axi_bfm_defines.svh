// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  SNPS AXI SVT BFM Defines, Typedefs, or PARAMETERS.
// -----------------------------------------------------------------------------


   `define SVT_AXI_USER_DEFINES_SVI

   `define SVT_AXI_MAX_ID_WIDTH 12

   `ifndef SVT_AXI_MAX_WRITE_RESP_REORDERING_DEPTH
    //  If no cfg in xact, default value must match
    // max value, since that is checked in is_valid
    // Hence setting MAX to default value.
    `ifdef SVT_AXI_SVC_NO_CFG_IN_XACT 
    `define SVT_AXI_MAX_WRITE_RESP_REORDERING_DEPTH 1
    `else
    `define SVT_AXI_MAX_WRITE_RESP_REORDERING_DEPTH 8
    `endif

    `endif
