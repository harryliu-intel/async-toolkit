// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Subodh Nanal 
// Created On   :  10/04/2018
// Description  :  EPC test island instantiation and connectivity
//                 Based on the fc model there will be variable number of EPC ti's
//                 fc_8 - 2 EPC TI's (2 MPP's)
//                 fc_64 - 8 EPC TI's (8 MPP's)
//------------------------------------------------------------------------------

`ifdef EPC_ENV_ENABLE

  `include "epc_conn_defines.svh"

  //Instantiate and connect the EPC TI
  //`igr_conn(`FC_ENV, `HDL_TOP``.igr, 0)
  `epc_conn(`FC_ENV, `soc, 1)
  `epc_conn(`FC_ENV, `soc, 2)
  `ifdef FC_64
      `epc_conn(`FC_ENV, `soc, 3)
      `epc_conn(`FC_ENV, `soc, 4)
      `epc_conn(`FC_ENV, `soc, 5)
      `epc_conn(`FC_ENV, `soc, 6)
      `epc_conn(`FC_ENV, `soc, 7)
      `epc_conn(`FC_ENV, `soc, 8)
  `endif

`endif

// <<< VIM SETTINGS
// vim: ts=4 et
// >>>

