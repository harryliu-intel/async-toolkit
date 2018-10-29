//-----------------------------------------------------------------------------
// Title         : Ingress Testbench Virtual Sequencer
// Project       : Madison Bay
//-----------------------------------------------------------------------------
// File          : mby_igr_tb_sequencer.sv
// Author        :
// Created       :
// Last modified :
//-----------------------------------------------------------------------------
// Description :
//-----------------------------------------------------------------------------
// Copyright (c) 2018 by Intel Corporation This model is the confidential and
// proprietary property of Intel Corporation and the possession or use of this
// file requires a written license from Intel Corporation.
//-----------------------------------------------------------------------------
// Modification history :
// xx.xx.2018 : created
//-----------------------------------------------------------------------------

`ifndef __MBY_IGR_ENV_PKG_GUARD
`error "Attempt to include file outside of mby_igr_env_pkg."
`endif 

`ifndef __MBY_IGR_TB_VSEQR_SVH__
`define __MBY_IGR_TB_VSEQR_SVH__

//-------------------------------------------------------------------------------
// Class: mby_igr_tb_sequencer
//-------------------------------------------------------------------------------
class mby_igr_tb_sequencer extends uvm_sequencer;
   `uvm_component_utils_begin(mby_igr_tb_sequencer)
   `uvm_component_utils_end

   protected static uvm_sequencer_base _sequencer_list[string];
   protected static string _agent_type_list[string];

   //---------------------------------------------------------------------------
   // Function: new
   //---------------------------------------------------------------------------
   function new(string name = "mby_igr_tb_sequencer", uvm_component parent = null);
      super.new(name, parent);
      // disable auto sequence launch in run_phase
      count = 0;
   endfunction: new


   //---------------------------------------------------------------------------
   // Function: add_sequencer
   // Updates the sequencer list with a pointer to an agent sub-sequencer.
   // Adds the uvm_sequencer_base object to the <_sequencer_list> list
   // and the agent_name to the _agent_name_list.
   // Returns status flag of type bit
   //---------------------------------------------------------------------------
   function bit add_sequencer(string agent_type, string agent_name, uvm_sequencer_base sequencer);

      if( sequencer == null ) begin
         `uvm_error({get_name(),".add_sequencer()"},"input sequencer is null");
         return(0);
      end

      if( find_sequencer( agent_name ) ) begin
         if($test$plusargs("ERROR_ON_DUPLICATE") || $test$plusargs("SLA_ERR_ON_DUPLICATE") ) begin
            `uvm_error({get_name(),".add_sequencer()"},{"existing sequencer of agent: ",agent_name," agent_type: ",agent_type," will be replaced."});
         end
         else begin
            `uvm_warning({get_name(),".add_sequencer()"},{"existing sequencer of agent: ",agent_name," agent_type: ",agent_type," will be replaced."});
         end
         delete_sequencer( agent_name );
      end

      _sequencer_list[agent_name]  = sequencer;
      _agent_type_list[agent_name] = agent_type;

      return(1);

   endfunction : add_sequencer


   //---------------------------------------------------------------------------
   // Function: find_sequencer
   //  Searches the <_sequencer list> and returns the handle to the specified 
   //  agent sub-sequencer.
   //  Returns an uvm_sequencer_base object if found, null if not 
   //---------------------------------------------------------------------------
   static function uvm_sequencer_base find_sequencer(string agent_name);
      if( _sequencer_list.exists( agent_name )) begin
         return( _sequencer_list[agent_name]);
      end else begin
         return( null );
      end
   endfunction : find_sequencer


   //---------------------------------------------------------------------------
   // Function: delete_sequencer
   //  Removes the uvm_sequencer_base object from the <_sequencer_list> if found.
   //---------------------------------------------------------------------------
   static function void delete_sequencer( string agent_name );
      if( _sequencer_list.exists( agent_name )) begin
         _sequencer_list.delete( agent_name );
      end
      if( _agent_type_list.exists( agent_name )) begin
         _agent_type_list.delete( agent_name );
      end
   endfunction : delete_sequencer


   //---------------------------------------------------------------------------
   // Function: pick_sequencer
   //  Arbitrarily returns a sequencer of an agent_type, if found.
   //  By default, any valid sub-sequencer is returned. 
   //---------------------------------------------------------------------------
   static function uvm_sequencer_base pick_sequencer( string agent_name_or_type = "ANY" );

      if( find_sequencer( agent_name_or_type ) ) begin
         return( find_sequencer( agent_name_or_type ));
      end
      else begin
         string aL[$];
         string agent_name;
         int    index;

         foreach ( _agent_type_list[agent_name] ) begin
            if( agent_name == agent_name_or_type || _agent_type_list[agent_name] == agent_name_or_type || (agent_name_or_type == "ANY" && _agent_type_list[agent_name] != "TB_SEQUENCER")) begin
               aL.push_back( agent_name );
            end
         end

         if( aL.size() == 0 ) begin
                return( null );
         end

         index  = $urandom_range( 0, (aL.size() - 1 ));
         return( find_sequencer( aL[index] ));
      end

   endfunction : pick_sequencer

endclass: mby_igr_tb_sequencer

`endif // __MBY_IGR_TB_VSEQR_SVH__
