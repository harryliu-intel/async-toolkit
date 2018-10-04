// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  CHI saola ral adapter sequence
// -----------------------------------------------------------------------------

`ifndef INC_chi_ral_adapter_seq
`define INC_chi_ral_adapter_seq

class chi_ral_adapter_seq extends sla_ral_sequence_base;

    `ovm_sequence_utils(chi_ral_adapter_seq, sla_sequencer) 


    sla_ral_data_t                   fc_data;
    sla_ral_data_t                   fc_addr;
    sla_ral_space_t                  space;
    sla_ral_env                      ral;

    fc_tb_env                          tb_env; 
    fc_cfg_obj                         cfg_obj;

    integer                          n_bits;   //number of bits in txn
    integer                          reg_size; //size of register

    string                           ip_name;

    //------------------------------------------
    function new(string name = "chi_ral_adapter_seq");
        super.new(name);
        get_tb_env(); 
    endfunction

    // -----------------------------------------------------------------------
    // gets a handle to fc_tb_env
    // -----------------------------------------------------------------------
    function void get_tb_env();
       `sla_assert($cast(tb_env, slu_utils::get_comp_by_name("tb_env")), ($sformatf("tb_env $cast failed to %s", "tb_env")));
       `sla_assert(tb_env, ($sformatf("could not fetch %s handle", "tb_env")));
    endfunction


    //------------------------------------------
    virtual task body();

        `ovm_info(get_type_name(), $psprintf("chi_ral_adaptor_seq start"), OVM_MEDIUM)

        //-------------------------------------------------------------------------
        //    RAL handle and declaration
        //-------------------------------------------------------------------------
        `slu_assert( $cast(ral, sla_ral_env::get_ptr()), ("unable to get handle to RAL"));
        ral_status = SLA_OK;

        //-------------------------------------------------------------------------
        //    step 1. get the RAL address and RAL data
        //-------------------------------------------------------------------------
        ovm_report_info(get_type_name(),"querying information from the RAL...");
        fc_data = ral_data;
        uvm_report_info(get_type_name(),$psprintf("ral : data = %h",fc_data));

        //-------------------------------------------------------------------------
        //    step2. find/retrieve the followoing
        //    . size of the register being exercised
        //    . address of the transaction
        //    . space of the register 
        //    . number of datawords for the register - for e.g. if the register is 32 bits or less, the DW_LEN = 1 and so on...
        //-------------------------------------------------------------------------
        //get register size from the RAL
        reg_size = target.get_size()/8;
        n_bits = reg_size;
        uvm_report_info(get_type_name(),$psprintf("ral : reg_size = %h",reg_size));

        // get register address from the RAL
        `uvm_info(get_type_name(), $psprintf("ral : access path = %s",source), OVM_MEDIUM)
        fc_addr = target.get_addr_val(source);
        uvm_report_info(get_type_name(), $psprintf("ral : address = %h", fc_addr));

        space = target.get_space();
        uvm_report_info(get_type_name(), $psprintf("ral : space = %s", space));

        send_chi_txn();
        
        if ( (ral_data == 'h0) === 1'bx)
        begin
            ral_status = SLA_FAIL; 
           `ovm_error(get_type_name(), $psprintf("RAL data %h contained X, so setting the ral_status to %s",ral_data, ral_status))
        end             

        if(ral_status != SLA_OK)
            `ovm_warning(get_type_name(), $psprintf("RAL STATUS = %s -- NOTE : RAL will not get updated with the read/write value",ral_status.name()))
        else
            `ovm_info(get_type_name(), $psprintf("RAL STATUS = %s",ral_status.name()), OVM_MEDIUM)

        uvm_report_info(get_type_name(), "--------------execution of ral adaptor sequence ends----------------");
 
    endtask : body

    // -----------------------------------------------------------------------
    task send_chi_txn ();

        fc_chi_txn_seq    chi_txn;
 bit [(`SVT_CHI_MAX_DATA_WIDTH-1):0] data;
        

        `ovm_info(get_type_name(), "start send_chi_txn ... ", OVM_MEDIUM)


         if (operation == "write") begin          
            // data = new[1]; //burst_length

             `xvm_do_on_with(chi_txn, tb_env.chi_req_node_seqr,{
                                chi_txn.chi_addr         == fc_addr;
                                chi_txn.chi_xact_type    == svt_chi_transaction::WRITENOSNPFULL;
                                chi_txn.chi_data == fc_data;
                });
        end
        else if (operation == "read") begin          

             `xvm_do_on_with(chi_txn, tb_env.chi_req_node_seqr,{
                                chi_txn.chi_addr         == fc_addr;
                                chi_txn.chi_xact_type    == svt_chi_transaction::READNOSNP;
                });
        end


        `ovm_info(get_type_name(), "finish sending_sbr_tx ... ", OVM_MEDIUM)
    endtask



endclass : chi_ral_adapter_seq
`endif
