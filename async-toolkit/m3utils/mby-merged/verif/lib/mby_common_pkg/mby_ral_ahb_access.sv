/*
 Class: mby_ral_ahb_access

 RAL implementation for AHB interafce
 
 This sequence extract data from the RAL register and transalte the 
 to AHB transaction
 
 This sequences extends slu_ral_sequence_base


 */

class mby_ral_ahb_access extends mby_ral_base_sequence;
   
   `uvm_object_utils(mby_ral_ahb_access) 
   
   function new(input string n = "mby_ral_ahb_access");
      super.new(n);
   endfunction : new

   virtual task initiate_txn(input int length_dw, sla_ral_addr_t addr, sla_ral_data_t data, mby_ral_user_object user_object, output sla_ral_data_t rsp_data);

      svt_ahb_bfm_pkg::ahb_master_directed_sequence ahb_req;
      svt_ahb_bfm_pkg::cust_svt_ahb_master_transaction::xact_type_enum xact_type;
      bit [63:0] req_data;
      ahb_req = svt_ahb_bfm_pkg::ahb_master_directed_sequence::type_id::create("ahb_req");
      

      req_data = data;

      case (target.get_space())
          
         "MEM" : begin
                    xact_type = (operation=="write") ?  svt_ahb_bfm_pkg::cust_svt_ahb_master_transaction::WRITE : svt_ahb_bfm_pkg::cust_svt_ahb_master_transaction::READ;
                 end

      endcase

      if(xact_type == svt_ahb_bfm_pkg::cust_svt_ahb_master_transaction::WRITE) begin

         `uvm_do_with(ahb_req, {
             ahb_req.write_tran.xact_type   == xact_type;
             ahb_req.write_tran.addr        == addr[31:0];
	     foreach (ahb_req.write_tran.data[i]) {
                ahb_req.write_tran.data[i]  == req_data[i];
	     }
	     ahb_req.write_tran.burst_type  == (user_object.beats == 1) ? svt_ahb_bfm_pkg::cust_svt_ahb_master_transaction::SINGLE : svt_ahb_bfm_pkg::cust_svt_ahb_master_transaction::INCR;		
	     ahb_req.write_tran.burst_size  == svt_ahb_bfm_pkg::cust_svt_ahb_master_transaction::BURST_SIZE_64BIT;
          })  
      end        
      else if(xact_type == svt_ahb_bfm_pkg::cust_svt_ahb_master_transaction::READ) begin    

         `uvm_do_with(ahb_req, {
             ahb_req.read_tran.xact_type   == xact_type;
             ahb_req.read_tran.addr        == addr[31:0];
	     ahb_req.read_tran.burst_type  == (user_object.beats == 1) ? svt_ahb_bfm_pkg::cust_svt_ahb_master_transaction::SINGLE : svt_ahb_bfm_pkg::cust_svt_ahb_master_transaction::INCR;
	     ahb_req.read_tran.burst_size  == svt_ahb_bfm_pkg::cust_svt_ahb_master_transaction::BURST_SIZE_64BIT;
         })            
      end 
      if ( operation == "read" && wait_for_complete == SLA_TRUE ) begin
	 foreach (req_data[idx]) begin
            req_data[idx] = ahb_req.rsp.data[idx];
         end
	 rsp_data = req_data[63:0];
      end
   endtask 
endclass