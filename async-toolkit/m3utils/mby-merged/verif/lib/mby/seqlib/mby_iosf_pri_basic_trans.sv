
/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:    mby_slu_iosf_seq.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : MBY IP
   ----------------------------------------------------------------------
 
  MBY Soala IOSF sequences for SM and RAL

  This file include IOSF sequences that are used in MBY SM and RAL
 
 
 1. <mby_iosf_pri_basic_trans> - Basic IOSF seq that can be called using uvm_do_with. Use by SM and RAL Primary seq.
 2. <mby_ral_iosf_sb_access> - RAL IOSF SB interafce  main sequence.
 2. <mby_ral_iosf_pri_access> - RAL IOSF Primary main sequence.

*/

/*
 Class: mby_iosf_pri_basic_trans

 Basic IOSF seq that can be called using uvm_do_with


 */


class mby_iosf_pri_basic_trans extends mby_extended_base_seq;
  
    //UVM UTIL
    `uvm_object_utils(mby_iosf_pri_basic_trans) 
    `uvm_declare_p_sequencer(IosfAgtSeqr)
  
    //------------------------------------------
    // Data Members 
    //------------------------------------------
    local const string      CLASSNAME = "mby_iosf_pri_basic_trans";
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
endclass: mby_iosf_pri_basic_trans

