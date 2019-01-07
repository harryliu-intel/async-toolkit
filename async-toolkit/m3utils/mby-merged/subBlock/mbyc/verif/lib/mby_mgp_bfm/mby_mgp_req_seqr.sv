
class mby_mgp_req_seqr extends uvm_sequencer#(mby_mgp_req_seq_item);

   `uvm_component_utils_begin(mby_mgp_req_seqr)
   `uvm_component_utils_end
   // Constructor: new
   //
   // Constructor, sets default values.

   function new(string        name   = "mby_mgp_req_seqr",
                uvm_component parent = null);
      super.new(name, parent);
   endfunction

endclass