// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

///------------------------------------------------------------------------------
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
// -- Author : Lewis Stenberg
// -- Project Name : Madison Bay
// -- Description : object type declaration for Parser to mapper interface
// --               This started out in life as Jon Bagge's par_class_if.sv -- supplimented for use by the testbench
// TODO: Confirm that this conforms to the MAS when the MAS is available
// --
// -------------------------------------------------------------------

// TODO: extend from shdv  sequence object when available
class mby_rx_ppe_parser_obj extends mby_rx_ppe_sb_obj;

   imn_rpl_bkwd_t      rpl_bkwd;       //Management status from downstream blocks
   imn_rpl_frwd_t      rpl_frwd;       //Managment to downstream blocks

   parser_out_t        parser_out;     //Parser results
   logic   [1:0]       parser_out_v;   //Parser results valid

   igr_rx_ppe_md_t     if0_md;         //Interface 0 metadata
   igr_rx_ppe_md_t     if1_md;         //Interface 1 metadata

   igr_rx_ppe_tail_t   tail_info;      //Tail info
   logic               tail_info_v;    //Tail info valid

   `uvm_object_utils(mby_rx_ppe_parser_obj)

   function new( string name = "mby_rx_ppe_parser_obj");
      super.new(name);
   endfunction: new

   virtual function string my_type();
      return("mby_rx_ppe_parser_obj ");
   endfunction : my_type

endclass : mby_rx_ppe_parser_obj
