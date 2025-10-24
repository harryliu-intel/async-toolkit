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
  //`define igr_conn(FC_ENV_PATH, DUT_PATH, MPP_IDX, MGP_IDX, IDX) \
  `igr_conn(`FC_ENV, `soc, 0, 0, 0)
  `igr_conn(`FC_ENV, `soc, 0, 1, 1)
  `igr_conn(`FC_ENV, `soc, 1, 0, 2)
  `igr_conn(`FC_ENV, `soc, 1, 1, 3)
  `ifdef FC_64
      `igr_conn(`FC_ENV, `soc, 2, 0, 4)
      `igr_conn(`FC_ENV, `soc, 2, 1, 5)
      `igr_conn(`FC_ENV, `soc, 3, 0, 6)
      `igr_conn(`FC_ENV, `soc, 3, 1, 7)
      `igr_conn(`FC_ENV, `soc, 4, 0, 8)
      `igr_conn(`FC_ENV, `soc, 4, 1, 9)
      `igr_conn(`FC_ENV, `soc, 5, 0, 10)
      `igr_conn(`FC_ENV, `soc, 5, 1, 11)
      `igr_conn(`FC_ENV, `soc, 6, 0, 12)
      `igr_conn(`FC_ENV, `soc, 6, 1, 13)
      `igr_conn(`FC_ENV, `soc, 7, 0, 14)
      `igr_conn(`FC_ENV, `soc, 7, 1, 15)
  `endif

`endif

// <<< VIM SETTINGS
// vim: ts=4 et
// >>>

