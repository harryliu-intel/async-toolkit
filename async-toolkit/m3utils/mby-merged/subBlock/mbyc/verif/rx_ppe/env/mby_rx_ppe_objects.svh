// vim: noai : ts=3 : sw=3 : expandtab : ft=systemverilog

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
// and confidential information of Intel or its suppliers and licensors.  The
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
//   Author        : Lewis Sternberg
//   Project       : Madison Bay
//------------------------------------------------------------------------------


//FIXME: There should be one object file per object.  This is a stand-in until the objects are scoped
//FIXME: What is the proper base object to inherit from?
//TODO:  The base clase should have a virtual function which returns the class type so that parameterized classes can determine their type in build_phase()
//       .. because it appears to be necessary to provide a default type for the parameterized scoreboard


class mby_rx_ppe_sb_obj extends uvm_object;
   `uvm_object_utils(mby_rx_ppe_sb_obj)
   function new( string name = "mby_rx_ppe_parser_obj");
      super.new(name);
   endfunction: new

   virtual function string my_type();
      `uvm_fatal(get_name(), "mby_rx_ppe_sb_obj must not be used directly")
   endfunction : my_type
endclass : mby_rx_ppe_sb_obj



class mby_rx_ppe_parser_obj extends mby_rx_ppe_sb_obj;
   `uvm_object_utils(mby_rx_ppe_parser_obj)
   function new( string name = "mby_rx_ppe_parser_obj");
      super.new(name);
   endfunction: new

   virtual function string my_type();
      return("mby_rx_ppe_parser_obj ");
   endfunction : my_type
endclass : mby_rx_ppe_parser_obj 



class mby_rx_ppe_mapper_obj extends mby_rx_ppe_sb_obj;
   `uvm_object_utils(mby_rx_ppe_mapper_obj)
   function new( string name = "mby_rx_ppe_mapper_obj");
      super.new(name);
   endfunction: new

   virtual function string my_type();
      return("mby_rx_ppe_mapper_obj ");
   endfunction : my_type
endclass : mby_rx_ppe_mapper_obj 



class mby_rx_ppe_classifier_obj extends mby_rx_ppe_sb_obj;
   `uvm_object_utils(mby_rx_ppe_classifier_obj)
   function new( string name = "mby_rx_ppe_classifier_obj");
      super.new(name);
   endfunction: new

   virtual function string my_type();
      return("mby_rx_ppe_classifier_obj ");
   endfunction : my_type
endclass : mby_rx_ppe_classifier_obj 




class mby_rx_ppe_hash_obj extends mby_rx_ppe_sb_obj;
   `uvm_object_utils(mby_rx_ppe_hash_obj)
   function new( string name = "mby_rx_ppe_hash_obj");
      super.new(name);
   endfunction: new

   virtual function string my_type();
      return("mby_rx_ppe_hash_obj ");
   endfunction : my_type
endclass : mby_rx_ppe_hash_obj 



class mby_rx_ppe_next_hop_obj extends mby_rx_ppe_sb_obj;
   `uvm_object_utils(mby_rx_ppe_next_hop_obj)
   function new( string name = "mby_rx_ppe_next_hop_obj");
      super.new(name);
   endfunction: new

   virtual function string my_type();
      return("mby_rx_ppe_next_hop_obj ");
   endfunction : my_type
endclass : mby_rx_ppe_next_hop_obj 



class mby_rx_ppe_mask_gen_obj extends mby_rx_ppe_sb_obj;
   `uvm_object_utils(mby_rx_ppe_mask_gen_obj)
   function new( string name = "mby_rx_ppe_mask_gen_obj");
      super.new(name);
   endfunction: new

   virtual function string my_type();
      return("mby_rx_ppe_mask_gen_obj ");
   endfunction : my_type
endclass : mby_rx_ppe_mask_gen_obj 



class mby_rx_ppe_triggers_obj extends mby_rx_ppe_sb_obj;
   `uvm_object_utils(mby_rx_ppe_triggers_obj)
   function new( string name = "mby_rx_ppe_triggers_obj");
      super.new(name);
   endfunction: new

   virtual function string my_type();
      return("mby_rx_ppe_triggers_obj ");
   endfunction : my_type
endclass : mby_rx_ppe_triggers_obj 



class mby_rx_ppe_congestion_mgt_obj extends mby_rx_ppe_sb_obj;
   `uvm_object_utils(mby_rx_ppe_congestion_mgt_obj)
   function new( string name = "mby_rx_ppe_congestion_mgt_obj");
      super.new(name);
   endfunction: new

   virtual function string my_type();
      return("mby_rx_ppe_congestion_mgt_obj ");
   endfunction : my_type
endclass : mby_rx_ppe_congestion_mgt_obj 



class mby_rx_ppe_rx_stats_obj extends mby_rx_ppe_sb_obj;
   `uvm_object_utils(mby_rx_ppe_rx_stats_obj)
   function new( string name = "mby_rx_ppe_rx_stats_obj");
      super.new(name);
   endfunction: new

   virtual function string my_type();
      return("mby_rx_ppe_rx_stats_obj ");
   endfunction : my_type
endclass : mby_rx_ppe_rx_stats_obj 

