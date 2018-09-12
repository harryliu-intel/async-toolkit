/*
 Class: mby_ral_iosf_pri_access

 RAL implementation for IOSF Primary interafce
 
 This sequence extract data from the RAL register and transalte the 
 to IOSF Primary transaction
 
 This sequences extends slu_ral_sequence_base


 */
class mby_ral_iosf_pri_access extends slu_ral_sequence_base;
  
  //UVM UTIL
  `uvm_object_utils(mby_ral_iosf_pri_access) 
  `uvm_declare_p_sequencer(IosfAgtSeqr)

  IosfTxn iosfTxn;
    
  Iosf::iosf_cmd_t cmd;
  Iosf::address_t  address;
  Iosf::data_t  iosf_data[];
  slu_ral_data_t align_ral_data;
  
  
  bit cast_wait_for_complete;
  bit [3:0] byte_en;
  bit [3:0] last_byte_en;
  bit [1:0] align_bytes;
  int reg_size;
  rand int unsigned     reqTrID = 0 ;
  
   
  virtual task body();
    int dw_len;
    Iosf::data_t              cmplData[];   // Response data
     
    byte_en = 4'hf;
    last_byte_en = 4'h0;
    
    align_bytes = 0;
    
    align_ral_data = ral_data;
    
    
    //Get rgeister size
    reg_size = target.get_size()/8;
    
    
    // Get register address
    address = target.get_addr_val("primary");
     
    dw_len = (reg_size + address[1:0]) / 4 + ((reg_size + address[1:0]) %4 > 0);

    // IOSF data is SW align
    iosf_data = new [dw_len];
    
    //Set cmd
    case (operation)
       "read"  : begin
	  //READ
	  case (target.get_space())
	    //CFG
	    "CFG" : begin
	       // Check BUS for type1/0
	       if (target.get_bus_num() == 0)
		 cmd = Iosf::CfgRd0;
	       else
		 cmd = Iosf::CfgRd1;
	       align_address_data();
	    end
	    //IO
	    "IO" : begin
	       cmd = Iosf::IORd;
	       
	       //Align the address
	       align_address_data();
	    end
	    //MEM
	    "MEM" : begin
	       if (address[63:32] != 0)
		 cmd = Iosf::MRd64;
	       else
		 cmd = Iosf::MRd32;
	       // Align the address
	       align_address_data();
	    end
	    default : begin
              uvm_report_error(get_name(), $psprintf ("Unsupported target space. Tagert %s, Space %s",target.get_name(),target.get_space()));
	    end
	  endcase
       end // case: "read"
      
      //WRITE
      "write" : begin 
	case (target.get_space())
	  //CFG
	  "CFG" : begin
	    // Check BUS for type1/0
	    if (target.get_bus_num() == 0)
	      cmd = Iosf::CfgWr0;
	    else
	      cmd = Iosf::CfgWr1;
	    align_address_data();
	  end
	  //IO
	  "IO" : begin
	    cmd = Iosf::IOWr;
	    //Align the address
	    align_address_data();
	  end
	  //MEM
	  "MEM" : begin
	    if (address[63:32] != 0)
	      cmd = Iosf::MWr64;
	    else
	      cmd = Iosf::MWr32;
	    //Align the address
	    align_address_data();
	    // No need for completion is posted
	    wait_for_complete = sla_pkg::SLA_FALSE;
	  end // case: "MEM"
	  
	  default : begin
            uvm_report_error(get_name(), $psprintf ("Unsupported target space. Tagert %s, Space %s",target.get_name(),target.get_space()));
	  end
	  
	endcase
      end
    endcase // case (operation)

    // Pack data to DW
     for (int i=0; i < (reg_size/4 + (reg_size% 4 > 0 )); i ++)
       $cast (iosf_data[i],align_ral_data[i*32+:32]);
     
    `slu_msg( UVM_LOW, get_name(), (
                                    "source=%s, target=%s, operation=%s, data=%h, BE=%h, space=%s, wait_for_complete=%s",
                                    source, target.get_name(), operation,iosf_data[0],byte_en,target.get_space(), (wait_for_complete ? "TRUE" : "FALSE")
                                    ));
    
    $cast (cast_wait_for_complete,wait_for_complete);
 
        iosfTxn = new ("iosfTxn");
        iosfTxn.set_sequencer (get_sequencer ());
        iosfTxn.reqChId           = 0;
        iosfTxn.trafficClass      = 0;
        iosfTxn.cmd               = cmd;
        iosfTxn.reqType           = Iosf::getReqTypeFromCmd (iosfTxn.cmd);
        iosfTxn.address           = address; 
        iosfTxn.data           = new [iosf_data.size()];
        foreach (iosf_data[i])
            iosfTxn.data [i]   = iosf_data [i];
        iosfTxn.length            = iosf_data.size();
        iosfTxn.first_byte_en     = byte_en;
        iosfTxn.last_byte_en      = last_byte_en;
        iosfTxn.expectRsp = cast_wait_for_complete; 
        iosfTxn.set_transaction_id (reqTrID); 
   
        `uvm_send (iosfTxn)

        if (cast_wait_for_complete) 
        begin
            IosfTgtTxn                rxRspTgtTxn;  // Rsp Transaction
            uvm_pkg::uvm_sequence_item rsp;
            string msg;
            get_response (rsp, reqTrID);
            assert ($cast (rxRspTgtTxn, rsp));
            $sformat (msg, "RSP reqTrID = 0x%h , %s", reqTrID, rxRspTgtTxn.convert2string ());
            `uvm_info (get_type_name(), msg, UVM_INFO);
            if (rxRspTgtTxn.data.size () > 0) 
            begin
                cmplData = new [rxRspTgtTxn.data.size ()];
                foreach (rxRspTgtTxn.data [idx])
                    cmplData [idx] = rxRspTgtTxn.data [idx];
            end

        end // if (waitForCompletion)
   
      
      if (operation == "read") begin
	foreach(cmplData[i])
	  ral_data[32*i+:32] = cmplData[i];
	align_data();
	
	
	`slu_msg( UVM_LOW, get_name(), (
					"source=%s, target=%s, operation=%s, wait_for_complete=%s, data=%h  cmpl done",
					source, target.get_name(), operation, (wait_for_complete ? "TRUE" : "FALSE"),ral_data
					));
      end
    ral_status = SLA_OK;
  endtask // body
  
  // Helping function to align return data
  function align_data();
    ral_data = ral_data >> (8* align_bytes);
  endfunction // align_data
  
  //Align address,data and set byte enable
  function align_address_data();
    int align_int;
    align_ral_data =  align_ral_data << (8* address[1:0]);
    align_bytes = address[1:0];
    byte_en = 0;
    
    align_int = ( address[1:0] + reg_size < 4 ?  address[1:0] + reg_size : 4);
    
    for (int i = address[1:0] ;  i<align_int; i++) begin
      byte_en[i] = 1;
    end
    address[1:0] = 0 ;
    
    assert (address[1:0] + reg_size <= 8 ) else
      uvm_report_error (get_name(), $psprintf ("Received request to write reister with size more than 2 DW, Register %s , size %d ",target.get_name(),address[1:0] + reg_size ) );
    
    if ( address[1:0] + reg_size > 4)
      for (int i = 0 ;  i< ((address[1:0] + reg_size) % 4 == 0 ? 4 : (address[1:0] + reg_size) % 4) ; i++) begin
	last_byte_en[i] = 1;
      end
  endfunction // align_address_data
  
endclass // gmf_iosf_pri_access

