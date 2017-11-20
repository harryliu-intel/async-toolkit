
/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:    tlm_slu_iosf_seq.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : TLM1 IP
   ----------------------------------------------------------------------
 
  TLM1 Soala IOSF sequences for SM and RAL

  This file include IOSF sequences that are used in TLM1 SM and RAL
 
 
 1. <tlm_iosf_pri_basic_trans> - Basic IOSF seq that can be called using uvm_do_with. Use by SM and RAL Primary seq.
 2. <tlm_ral_iosf_sb_access> - RAL IOSF SB interafce  main sequence.
 2. <tlm_ral_iosf_pri_access> - RAL IOSF Primary main sequence.

*/

/*
 Class: tlm_iosf_pri_basic_trans

 Basic IOSF seq that can be called using uvm_do_with


 */


class tlm_iosf_pri_basic_trans extends slu_ral_sequence_base;
  
    //UVM UTIL
    `uvm_object_utils(tlm_iosf_pri_basic_trans) 
    `uvm_declare_p_sequencer(IosfAgtSeqr)
  
    //------------------------------------------
    // Data Members 
    //------------------------------------------
    local const string      CLASSNAME = "tlm_iosf_pri_basic_trans";
    rand Iosf::iosf_cmd_t  m_cmd;  
    rand Iosf::address_t   m_address;
    rand Iosf::data_t      m_data [];           // req: mdata
    rand bit [3:0]         m_first_byte_en;     // req: mfbe
    rand bit [3:0]         m_last_byte_en;      // req: mfbe
    rand bit waitForCompletion;
    rand int unsigned     reqTrID ;
    Iosf::data_t return_data [];
   
    //------------------------------------------
    // Constraints 
    //------------------------------------------
   
   
    function   new   (string name = "");
        super.new (name);
        ral_data = 0;
    endfunction // new
  
    task body  ();
        IosfTxn iosfTxn;
        IosfAgtSeqr       iosfAgtSeqr;

        iosfTxn = new ("iosfTxn");
        iosfTxn.set_sequencer (get_sequencer ());
        iosfTxn.reqChId           = 0;
        iosfTxn.trafficClass      = 0;
        iosfTxn.cmd               = m_cmd;
        iosfTxn.reqType           = Iosf::getReqTypeFromCmd (iosfTxn.cmd);
        iosfTxn.length            = m_data.size();
        iosfTxn.address           = m_address; 
        iosfTxn.data           = new [m_data.size()];
        foreach (m_data[i])
            iosfTxn.data [i]   = m_data [i];
        iosfTxn.first_byte_en     = m_first_byte_en;
        iosfTxn.last_byte_en      = m_last_byte_en;
        iosfTxn.expectRsp = waitForCompletion; //support for pvc 2012WW40r121005
        iosfTxn.set_transaction_id (reqTrID); 

        `uvm_send (iosfTxn)

        if (waitForCompletion) 
        begin
            IosfTgtTxn                rxRspTgtTxn;  // Rsp Transaction
            uvm_pkg::uvm_sequence_item rsp;
            Iosf::data_t              cmplData[];   // Response data
            string msg;
            get_response (rsp, reqTrID);
            assert ($cast (rxRspTgtTxn, rsp));
            $sformat (msg, "RSP reqTrID = 0x%h , %s", 
            reqTrID, rxRspTgtTxn.convert2string ());
            uvm_report_info (CLASSNAME, msg);
            if (rxRspTgtTxn.data.size () > 0) 
            begin
                cmplData = new [rxRspTgtTxn.data.size ()];
                foreach (rxRspTgtTxn.data [idx])
                    cmplData [idx] = rxRspTgtTxn.data [idx];
            end
            return_data = cmplData;

        end // if (waitForCompletion)
    endtask
endclass: tlm_iosf_pri_basic_trans


/*
 Class: tlm_ral_iosf_sb_access

 RAL implementation for IOSF SB interafce
 
 This sequence extract data from the RAL register and transalte the 
 to IOSF SB transaction
 
 This sequences extends slu_ral_sequence_base


 */

class tlm_ral_iosf_sb_access extends slu_ral_sequence_base;
   
  `uvm_object_utils(tlm_ral_iosf_sb_access) 
   
  `uvm_declare_p_sequencer(iosfsbm_cm::iosfsbc_sequencer);

  // IOSF SB to be used.
  iosfsbm_seq::iosf_sb_seq sb_seq;
  iosfsbm_cm::xaction_class_e m_xaction_class;
  bit[2:0] bar;
  bit [3:0] byte_en;
  int reg_size;
  bit [1:0] align_bytes;
  byte fid = 0;
  iosfsbm_cm::pid_t m_dest_pid;
  iosfsbm_cm::opcode_t m_opcode;
  iosfsbm_cm::flit_t m_data[];
  iosfsbm_cm::flit_t m_addr[];
  bit m_exp_rsp;
  
  function new(input string name = "",
               uvm_sequencer_base sequencer=null, uvm_sequence parent_seq=null);
     int dummy;
    super.new(name);

    
  endfunction

   
  task body();
    // IOSF SB sequencer.
    iosfsbm_cm::iosfsbc_sequencer seq;
    
    bit [63:0] my_addr;   
    int addr_align;
    int m_size;
    byte_en  = 4'b1111;
    align_bytes = 0;
    sb_seq = new ("MY_SEQ");
    $cast(seq,get_sequencer());
    // Get MSG/SB  adrress
    my_addr = target.get_space_addr("MSG");
    
    assert (my_addr != undef) else
      uvm_report_error (get_name(), {"Trying to access register which does not have MSG address define - ", target.get_name()});
     
    // GET transaction class 
    m_xaction_class = (operation == "write" ? iosfsbm_cm::POSTED :  iosfsbm_cm::NON_POSTED);
    // GEt SB port ID
    m_dest_pid = target.get_space_addr("msg_bus_port");
    
    case (operation)
      
      "read"  : begin 
	//READ
	case (target.get_space())
	  //CFG
	  "CFG" : begin
	    m_xaction_class = iosfsbm_cm::NON_POSTED;
	    m_opcode = iosfsbm_cm::OP_CFGRD;
	    fid = (target.get_dev_num() << 3 |  target.get_func_num());
	  end
	  //IO
	  "IO" : begin
	    m_xaction_class = iosfsbm_cm::NON_POSTED;
	    m_opcode = iosfsbm_cm::OP_IORD;
	  end
	  //MEM
	  "MEM" : begin
	    m_xaction_class = iosfsbm_cm::NON_POSTED;
	    m_opcode = iosfsbm_cm::OP_MRD;
	  end
	  "MSG" : begin
	    m_xaction_class = iosfsbm_cm::NON_POSTED;
	    m_opcode = iosfsbm_cm::OP_CRRD;
	  end   
	  default :begin
            uvm_report_error(get_name(), $psprintf ("Unsupported target space. Tagert %s, Space %s",target.get_name(),target.get_space()));
	  end
	    
	endcase // case (target.get_space())
      end // case: "read"
      
      //WRITE
      "write" : begin 
	case (target.get_space())
	  //CFG
	  "CFG" : begin
	    m_xaction_class = iosfsbm_cm::NON_POSTED;
	    m_opcode = iosfsbm_cm::OP_CFGWR;
	    fid = (target.get_dev_num() << 3 |  target.get_func_num());
	  end
	  //IO
	  "IO" : begin
	    m_xaction_class = iosfsbm_cm::NON_POSTED;
	    m_opcode = iosfsbm_cm::OP_IOWR;
	  end
	  //MEM
	  "MEM" : begin
	    m_xaction_class = iosfsbm_cm::POSTED;
	    m_opcode = iosfsbm_cm::OP_MWR;
	  end
	  "MSG" : begin
	    m_xaction_class = iosfsbm_cm::NON_POSTED;
	    m_opcode = iosfsbm_cm::OP_CRWR;
	  end   
	  default : begin
          uvm_report_error(get_name(), $psprintf ("Unsupported target space. Tagert %s, Space %s",target.get_name(),target.get_space()));

	  end // case: default
	endcase // case (target.get_space())
      end // case: "write"
       
     endcase // case (operation)

    
    
    
    m_size = target.get_size()/8;
    
    // Align data and address only if the tagert requre
    if (!target_support_unalign_address(m_dest_pid)) begin
      if (m_size < 4)
	begin
          byte_en = 4'b0000;
          addr_align = my_addr[1:0];
	  align_bytes = my_addr[1:0];
          for (int i = my_addr[1:0],int j=0 ;i<4; i++,j++) 
	    begin
	      if(j<m_size)
		byte_en[i] = 1'b1;
	    end
          my_addr[1:0] = 2'b00;
          while (addr_align > 0)
            begin
              ral_data = ral_data << 8;
              addr_align--;
            end
          `slu_msg( UVM_HIGH, get_name(), ("byte_en = %0x, ral_data = %0x, my_addr = %0x", byte_en, ral_data, my_addr));
	end // if (m_size < 4)
    end // if (!target_support_unalign_address(m_dest_pid))
    
    m_data = (operation == "write" ? {ral_data[7:0],ral_data[15:8],ral_data[23:16],ral_data[31:24]} : {});
    
    m_addr = (target.get_space() == "MEM" ? 
	      {my_addr[7:0],my_addr[15:8],my_addr[23:16],my_addr[31:24],my_addr[39:32],my_addr[47:40]} : 
	      {my_addr[7:0],my_addr[15:8]});


    
     
    m_exp_rsp = (operation == "write" ? 0 : 1);

    // Default BAR
    bar = 3'h0;;
;     
    `uvm_do_with(sb_seq, {xaction_class_i == m_xaction_class;
			  dest_pid_i == m_dest_pid;
			  opcode_i == m_opcode;
			  data_i.size() == m_data.size();
			  foreach (data_i[i])
			  data_i[i] == m_data[i];
			  addr_i.size() == m_addr.size();
			  foreach (addr_i[i])
			  addr_i[i] == m_addr[i];
			  fbe_i == byte_en;
			  sbe_i == 0;
			  bar_i == bar;
			  fid_i == fid;
			  xaction_delay_i == 0;
			  exp_rsp_i == m_exp_rsp;
			  compare_completion_i == 0;
			  })
      
      `slu_msg( UVM_LOW, get_name(), (
                                      "source=%s, target=%s, operation=%s, byte_en=%h, wait_for_complete=%s",
                                      source, target.get_name(), operation, byte_en, (wait_for_complete ? "TRUE" : "FALSE")
                                      ));	
    // Completion waiting
    if (operation == "read")
      //check completion status
      if (sb_seq.rx_compl_xaction.rsp == 0 && sb_seq.rx_compl_xaction.data.size() == 4)
	begin
	  //pack completion data
	  ral_data = {sb_seq.rx_compl_xaction.data[3],sb_seq.rx_compl_xaction.data[2],sb_seq.rx_compl_xaction.data[1],sb_seq.rx_compl_xaction.data[0]};
          align_data();
        end
      else begin
	uvm_report_error (get_name(), $psprintf ("Received bad completion on SB NP request, Register %s , completion status %x",target.get_name(), sb_seq.rx_compl_xaction.rsp) );
      end
    ral_status = SLA_OK;
    
  endtask // body
  
 
  // Helping function to align return data
  function align_data();
    ral_data = ral_data >> (8* align_bytes);
  endfunction // align_data
  
  // Helping function to not align address adn data to target that used unalign address
  function bit target_support_unalign_address ( iosfsbm_cm::pid_t dest_pid);
    
    //  return (dest_pid inside {`SC_PSF0_SB,`SC_PSF1_SB,`SC_PSF2_SB});
    return 0;
  endfunction // bit
  
      
  
endclass // tlm_ral_iosf_sb_access


/*
 Class: tlm_ral_iosf_pri_access

 RAL implementation for IOSF Primary interafce
 
 This sequence extract data from the RAL register and transalte the 
 to IOSF Primary transaction
 
 This sequences extends slu_ral_sequence_base


 */
class tlm_ral_iosf_pri_access extends slu_ral_sequence_base;
  
  //UVM UTIL
  `uvm_object_utils(tlm_ral_iosf_pri_access) 
  `uvm_declare_p_sequencer(IosfAgtSeqr)

    
    tlm_iosf_pri_basic_trans reg_seq;
  Iosf::iosf_cmd_t cmd;
  Iosf::address_t  address;
  Iosf::data_t  iosf_data[];
  slu_ral_data_t align_ral_data;
  
  
  bit cast_wait_for_complete;
  bit [3:0] byte_en;
  bit [3:0] last_byte_en;
  bit [1:0] align_bytes;
  int reg_size;
  
   
  virtual task body();
    int dw_len;
     
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
    
    
    `uvm_do_with(reg_seq, {m_cmd == cmd;
			   m_address == address;
			   m_data.size() == iosf_data.size(); 
			   foreach (iosf_data[i])
			   m_data[i] == iosf_data[i];
			   m_first_byte_en == byte_en;
			   m_last_byte_en == last_byte_en;
			   waitForCompletion == cast_wait_for_complete;
			   })
      
      if (operation == "read") begin
	foreach(reg_seq.return_data[i])
	  ral_data[32*i+:32] = reg_seq.return_data[i];
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
  
endclass // tlm_iosf_pri_access

