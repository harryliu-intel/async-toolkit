// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Subodh Nanal 
// Created On   :  10/04/2018
// Description  :  IGR test island instantiation and connectivity
//                 Based on the fc model there will be variable number of ingress ti's
//                 fc_8 - 4 igr TI's (2 MPP's)
//                 fc_64 - 16 igr TI's (8 MPP's)
//------------------------------------------------------------------------------

`ifdef IGR_ENV_ENABLE

  `include "igr_conn_defines.svh"

  //Instantiate and connect the IGR TI
  //`igr_conn(`FC_ENV, `HDL_TOP``.igr, 0)
  `igr_conn(`FC_ENV, `HDL_TOP, 0)
  `igr_conn(`FC_ENV, `HDL_TOP, 1)
  `igr_conn(`FC_ENV, `HDL_TOP, 2)
  `igr_conn(`FC_ENV, `HDL_TOP, 3)
  `ifdef FC_64
      `igr_conn(`FC_ENV, `HDL_TOP, 4)
      `igr_conn(`FC_ENV, `HDL_TOP, 5)
      `igr_conn(`FC_ENV, `HDL_TOP, 6)
      `igr_conn(`FC_ENV, `HDL_TOP, 7)
      `igr_conn(`FC_ENV, `HDL_TOP, 8)
      `igr_conn(`FC_ENV, `HDL_TOP, 9)
      `igr_conn(`FC_ENV, `HDL_TOP, 10)
      `igr_conn(`FC_ENV, `HDL_TOP, 11)
      `igr_conn(`FC_ENV, `HDL_TOP, 12)
      `igr_conn(`FC_ENV, `HDL_TOP, 13)
      `igr_conn(`FC_ENV, `HDL_TOP, 14)
      `igr_conn(`FC_ENV, `HDL_TOP, 15)
  `endif

`endif

// <<< VIM SETTINGS
// vim: ts=4 et
// >>>

