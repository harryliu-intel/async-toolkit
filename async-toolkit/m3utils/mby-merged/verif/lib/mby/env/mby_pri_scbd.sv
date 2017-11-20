

/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:    mby_pri_scbd.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : MBY IP
   ----------------------------------------------------------------------
 
  MBY Primary Scoreboard

 This is a very basic infrastructure for scbd imp.
 
 This example is for IOSF primary but user should 
 
 This scbd do not support transaction timeout
 
 
 

*/



class mby_pri_scbd extends ovm_scoreboard;
  

    // ***************************************************************
    // MBY event pool
    // ***************************************************************
    ovm_event_pool    MBYevPool;

  
    // ***************************************************************
    // Queues for transactions
    // ***************************************************************
  
  ovm_analysis_export #(IosfMonTxn) expected_transaction_port;
  ovm_analysis_export #(IosfMonTxn) actaul_transaction_port;
  tlm_analysis_fifo #(IosfMonTxn) expected_transaction;
  tlm_analysis_fifo #(IosfMonTxn) actaul_transaction;
  
  IosfMonTxn  expected_Q[$];

  /* 
   Variable: scbd_in_order
   Flag that indicate if scbd is "in order"
   */
  bit scbd_in_order = 1;
   /* 
   Variable: enable_scbd
   Enable/Disable the scbd
   */
  protected bit enable_scbd = 1;
   /* 
   Variable: mby_pri_scbd_timeout
    Scoreboard timeout to be used in Stop phase.
    The us value to wait in stop phase to all expected transactions
   */
  int mby_pri_scbd_timeout = 100;

  `ovm_component_utils_begin(mby_pri_scbd)
    `ovm_field_int(scbd_in_order, OVM_ALL_ON)
    `ovm_field_int(enable_scbd, OVM_ALL_ON)
     `ovm_field_int(mby_pri_scbd_timeout, OVM_ALL_ON)    
  `ovm_component_utils_end
    /*
    Function: new
    
    constractor 
       
    */
  function new(string name="mby_pri_scbd", ovm_component parent=null);
    super.new(name,parent);
    // Enable OVM STOP flow
    enable_stop_interrupt = 1;

    expected_transaction_port = new ("expected_transaction_port",this);
    actaul_transaction_port = new ("actaul_transaction_port",this);
    expected_transaction = new ("expected_transaction",this);
    actaul_transaction = new ("actaul_transaction",this);
  endfunction // new


  function void connect();
    super.connect();
    expected_transaction_port.connect(expected_transaction.analysis_export);
    actaul_transaction_port.connect(actaul_transaction.analysis_export);
    
  endfunction


  /*
   Task: Run
   
   invoke scbd threads
   */
  task run();
    super.run();
    // wait unit scbd is enabled
    wait (enable_scbd == 1);
    fork
      collect_expected_tranx();
      collect_actual_tranx();
    join_none
  endtask // run

  /*
   Task: collect_expected_tranx
   
   This task collect expected transactions
   and put them in the expected Q
   */

  task collect_expected_tranx();
    IosfMonTxn expected_trans;
    // forever loop on the expected Q
    forever begin
      // wait unit scbd is enabled
      wait (enable_scbd == 1);
      // blocking until getting an expected transaction
      expected_transaction.get(expected_trans);
      // Adding to the expected transaction to expected Q
      // INTEG - here the user can add need to verfy that this transaction should enetr the scbd
      // and add its golden model
      if (expected_trans.eventType == Iosf::MCMD) begin
	`sla_msg(OVM_NONE, get_name(),("SCBD recieve expected trandaction %s, adding for Q",trans_to_string(expected_trans)));
	expected_Q.push_back(expected_trans);
      end
      
    end
  endtask

  /*
   Task: collect_actual_tranx
   
   This task collect actaul transaciton and send them to match & compare functions
   
   */
  task collect_actual_tranx();
    IosfMonTxn actual_trans;
    // forever loop on the actaul Q
    forever begin
      // wait unit scbd is enabled
      wait (enable_scbd == 1);
      // blocking until getting an actaul transaction
      actaul_transaction.get(actual_trans);
      //=============================================================================
      // INTEG - This delay is just becuase both fifo's are cinnectedd to the same port!
      // INTEG - this line must be removed once the SCBD become functional
      //=============================================================================
      #1;
      // Sending the actaul transaciton to match and compare functions
      if (actual_trans.eventType == Iosf::MCMD) begin
	`sla_msg(OVM_NONE, get_name(),("SCBD recieve actaul trandaction %s, passing it to process",trans_to_string(actual_trans)));
	process_actaul_tranX(actual_trans);
      end
      
    end
    
  endtask // collect_actual_tranx

  /*
   Function: process_actaul_tranX
   
   The function should find a match for this transaction and compare for data integrity
   */

  function process_actaul_tranX (IosfMonTxn trans);
    //Hold the matching index
    int match_index = 0;
    bit found_match = 0;
    
    // Loop to find match
    for (int i = 0; i < expected_Q.size(); i++) begin
      if (is_match(expected_Q[i],trans)) begin
	match_index = i;
	found_match = 1;
	break;
      end
    end
    // If found match
    if (found_match) begin
      // If scbd is "in order" check order here
      if (scbd_in_order) begin
	if (match_index != 0 ) begin
	  `sla_error (get_name(),("Out of order transaction - %s",trans_to_string(trans)));
	end
      end

      // Check data integrity
      compare_trans(expected_Q[match_index],trans);

      //delete transaciton from the Q
      expected_Q.delete(match_index);
	
    end else begin
      `sla_error (get_name(),("Unexpected transaction - %s",trans_to_string(trans)));
    end // else: !if(found_match)
  endfunction


  /*
   Function: is_match
   
   This function should check of the transaction match
   
   User should implement it.
   */
  function bit is_match (IosfMonTxn exp_trans, IosfMonTxn act_trans);
    bit match = 1;

    `sla_warning (get_name(), ("INTEG - Need to imp is_match function"));

    /*
     User code
     */
    
    return (match);
  endfunction // bit

  /*
   Function: Compare_trans 
   
   This function should compare the transaction and check the daya integrity
   
   */
  function  compare_trans (IosfMonTxn exp_trans, IosfMonTxn act_trans);


    `sla_warning (get_name(), ("INTEG - Need to imp compare_trans function"));
    /*
     User to implemeted
     */
    
  endfunction // Compare_trans
    

  /*
   Function: trans_to_string
   
   Afunction that do fresdnly print of the trans
   
   */

  function string trans_to_string (IosfMonTxn trans);
    string trans_string;
    `sla_warning (get_name(), ("INTEG - Need to imp trans_to_string function"));
    /*
     User code
     */

    return (trans_string);
  endfunction
  /*
    Task: stop
    
    This task hold the test form stop until it ends.
   
   SCBD Should use it until all expected transaction recieved
       
    */
  virtual task stop (string ph_name);
    int counter = 0;
    `sla_msg(OVM_NONE, get_name(),("%t Start STOP task",$time));
    while (expected_Q.size() > 0 && counter < (mby_pri_scbd_timeout/10)) begin
      `sla_msg(OVM_NONE, get_name(),("%t SCBD expected_Q is not empty wait 10us",$time));
      #10us;
      counter++;
    end
    if (expected_Q.size() > 0) begin
    `sla_error(get_name(),("SCBD expected Q is not empty after %d us",mby_pri_scbd_timeout));
    end
  endtask // stop


  /*
   Function: set_scbd_enable
   
   Set the scbd to enable/disable
   */
  function void set_scbd_enable(bit val = 1);
     enable_scbd = val;
  endfunction
  /*
   Function: get_scbd_enable
   
   Get the scbd to enable/disable flag
   
   
   */
  function bit get_scbd_enable();
     return enable_scbd;
  endfunction
endclass // mby_pri_scbd
