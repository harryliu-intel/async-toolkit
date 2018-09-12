class mby_ral_base_sequence extends slu_ral_sequence_base;

    sla_ral_addr_t reg_addr;
    int            reg_size_bytes;

    int            total_length_dw;
    int            dw_sent;

    bit[3:0]       byte_enables[$];


    sla_ral_data_t read_data;
    sla_ral_data_t write_data;

    sla_ral_addr_t txn_addr;
    sla_ral_data_t txn_data;
    sla_ral_data_t txn_rsp_data;
    int            txn_length_dw;

    mby_ral_user_object user_obj;
   

    //--------------------------------------------------------------------------
    function new (input string n = "mby_ral_base_sequence");
        super.new(n);
    endfunction : new

    virtual task body();
       bit [63:0] wr_data;

       int burst_q[$] = {1, 2, 4, 8}; // 1,2,4,8  bytes of data per burst
       int beats_q[$] = {8, 4, 2, 1}; // number of beats per burst. A burst size of 4 bytes would have 2 beats to transfer 64 bits of data per burst.  
       int incr;
       int data_size;
       
       int burst_idx = $urandom_range(0, burst_q.size()-1); //random pick of burst size
       data_size = burst_q[burst_idx]*8; // Data size : 4bytes*8 = 32 bits.  
       reg_addr = target.get_addr_val(source);

       write_data = ral_data;

       user_obj = new("user_obj");
       user_obj.burst_size = burst_q[burst_idx];
       user_obj.beats = beats_q[burst_idx];

       set_user_object(user_obj);

       for (int idx = 0; idx < data_size; idx++) // part select of write_data. Beat size
          wr_data[idx] = write_data[idx];
       
       
       for (int idx = 0; idx < beats_q[burst_idx]; idx++) begin //loop for number of beats per burst
          txn_length_dw = burst_q[burst_idx]; //txn len is data size. 
          txn_addr = reg_addr[31:0] + incr; //32 bit address for AHB/AXI (assuming address is aligned)
          txn_data = wr_data; //64 bit data broken down into beats of data transfer.
          initiate_txn(txn_length_dw,txn_addr,txn_data,user_obj,txn_rsp_data);
          read_data = txn_rsp_data; //Response after a read/write
          incr += 8; // have to update increments
          write_data = write_data >>  data_size; // align data after every transfer
       end 

    endtask


    //--------------------------------------------------------------------------
    // Task to initiate transaction
    //--------------------------------------------------------------------------
    virtual task initiate_txn(input int length_dw, sla_ral_addr_t addr, sla_ral_data_t data, mby_ral_user_object user_object, output sla_ral_data_t rsp_data);
        print_txn("transaction initiated", addr, data);
       
        `sla_fatal(get_name(), ("mby_ral_base_sequence::initiate_txn must be extended!!!"));
    endtask : initiate_txn

    //--------------------------------------------------------------------------
    // Print transaction details.
    //--------------------------------------------------------------------------
    virtual function void print_txn(input string msg_id, sla_ral_addr_t addr, sla_ral_data_t data);
        string msg = msg_id;
        $swrite(msg, {msg, ": source=%s"},   source);
        $swrite(msg, {msg, ", target=%s"},   target.get_name());
        $swrite(msg, {msg, ", space=%s"},    target.get_space());
        $swrite(msg, {msg, ", device=%0d"},   target.get_dev_num());
        $swrite(msg, {msg, ", function=%0d"}, target.get_func_num());
        $swrite(msg, {msg, ", addr=%0h"},     addr);
        $swrite(msg, {msg, ", data=%0h"},     data);
        $swrite(msg, {msg, ", seq_id=%0d"},   get_sequence_id());
        `sla_msg(UVM_LOW, "RAL-Frontdoor", (msg))
    endfunction : print_txn

endclass