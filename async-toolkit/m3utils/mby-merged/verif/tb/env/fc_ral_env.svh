// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  Fullchip RAL Environment component
// -----------------------------------------------------------------------------


typedef class fc_tb_env;

//TODO. to be deleted 
//`include "pmu_mmr_regs.svh"

class fc_ral_env extends sla_ral_env;

    `ovm_component_utils_begin(fc_ral_env)
    `ovm_component_utils_end

    //TODO to be deleted
    rand pmu_mmr_file pmu_mmr;

    protected string   rtlPath;
    protected string   clusterName;
    protected fc_cfg_obj fcCfgObj; 
    protected fc_tb_env  tb_env;

    // ------------------------------------------------------------
    function new( string n="", ovm_component p = null, string hdl_path = "");
        bit rc;
        super.new( n, p);

        rc = $cast(tb_env, slu_tb_env::get_top_tb_env());
            assert (rc) else `ovm_fatal(get_name(), "Unable to get TOP TB ENV")
        fcCfgObj = tb_env.get_cfg_obj_handle();
    endfunction

    // ------------------------------------------------------------
    virtual function void build();
         super.build();
         //TODO to be deleted
       pmu_mmr = pmu_mmr_file::type_id::create("pmu_mmr", this);
      add_file("pmu_mmr",pmu_mmr);

         `ovm_info(get_name(), "Exit FC RAL build()", UVM_LOW);

    endfunction

    // ------------------------------------------------------------
    function void connect();
        super.connect();

        //TODO to be deleted
        pmu_mmr.set_base(4'h0);
        pmu_mmr.set_base_per_space("MSG", 16'h0000);
        pmu_mmr.set_space_addr("msg_bus_port", 8'h45);
        
        if (_level == SLA_TOP) begin
            foreach (_files[file_item]) begin
                _files[file_item].set_ral_env(this);
            end
        end

         `ovm_info(get_name(), "Exit FC RAL build()", UVM_LOW);

    endfunction

    // ------------------------------------------------------------
    function void end_of_elaboration();
        `ifdef AXI_ENV_ENABLE
        set_frontdoor_seq_type("axi_slu","write","axi_ral_adapter_seq");
        set_frontdoor_seq_type("axi_slu","read","axi_ral_adapter_seq");
        `endif
        `ifdef APB_ENV_ENABLE
        set_frontdoor_seq_type("apb_slu","write","apb_ral_adapter_seq");
        set_frontdoor_seq_type("apb_slu","read","apb_ral_adapter_seq");
        `endif
        `ifdef CHI_ENV_ENABLE
        set_frontdoor_seq_type("chi_slu","write","chi_ral_adapter_seq");
        set_frontdoor_seq_type("chi_slu","read","chi_ral_adapter_seq");
        `endif

        

    endfunction: end_of_elaboration

    // ------------------------------------------------------------
    virtual function void start_of_simulation();
        super.start_of_simulation();
        print_all_ral_csrs("start");
    endfunction


    function match(string s1,s2);
        int l1,l2;
        l1 = s1.len();
        l2 = s2.len();
        match = 0 ;
        if( l2 > l1 )
          return 0;
        for(int i = 0;i < l1 - l2 + 1; i ++)
          if( s1.substr(i,i+l2 -1) == s2)
            return 1;
    endfunction // match

    function print_all_ral_csrs(string phase_name);
        string file_name_s;
        string fields, fld;
        int    fld_width;
        int    row_count;
        sla_ral_file files[$];
        sla_ral_field mfields[$];
        sla_ral_reg mr[$];
        int    fd, fd_all;
        int    port_num;
        slu_ral_data_t wr_vals[$];
        slu_ral_data_t rd_vals[$];
        slu_ral_space_t spaces[$];
        slu_ral_data_t cfg_value;
        string list_spaces = "";
        string prim_addr_s;
        slu_ral_addr_t addr_sb;
        slu_ral_data_t fld_val;
        string side_addr_s;
        bit    after_start = 0;
        int    changed = 0;
        string wr_sai = "NO";
        string rd_sai = "NO";
        string Reserved = "Reserved";
        string RSVD = "RSVD";
        string UNDEF = "-NA-";
        string port_num_s;
        int    new_file = 0;
        string msg_opc[$];
        string list_opc = "";

        if(this != null) begin
            this.get_reg_files(files); // For LBG FC
            // top_ral_env.get_reg_files(files); // For LBG FC
        end
        // this.ral.get_reg_files(files); // For MSU inside TbEnv
        // top_ral_env.get_reg_files(files); // For MSU
        this.get_reg_files(files); // For LBG FC

        if(phase_name != "start") begin
            after_start = 1;
            //fd_all = $fopen({"ral_csr_changed_after_", phase_name,
            //"_trk\.out"}, "w");
        end
            fd_all = $fopen({"all_ral_registers_after_", phase_name, "_trk\.out"}, "w");
        // end


        foreach(files[i]) begin
                    row_count = 0;
                    new_file = 1;
// $fdisplay(fd_all,"+--------------------+----------------------------------+----------------+------------+------+--------+------------+------------+----------------------------------------------+");
// $fdisplay(fd_all,"| File Name          | Register Name                    |
// Space     | Reg Addr   | PID  |MSG Addr| Reset Val  | Config Val | Csr
// Fields - exclude fields named Reserved - |");
// $fdisplay(fd_all,"+--------------------+----------------------------------+----------------+------------+------+--------+------------+------------+----------------------------------------------+");

            files[i].get_regs(mr);
            foreach(mr[j]) begin
                        changed = 0;
                        mr[j].get_legal_sai_values(RAL_WRITE, wr_vals); // use this! 
                                                                        // function bit is_sai_protected(slu_ral_rw_t op = RAL_NONE); after v20140417
                        if (wr_vals.size() > 0) wr_sai = "YES";
                        else wr_sai = "NO";
                        mr[j].get_legal_sai_values(RAL_WRITE, rd_vals);
                        if (rd_vals.size() > 0) rd_sai = "YES";
                        else rd_sai = "NO";
                        mr[j].get_fields(mfields);
                        fields =  "";
                        foreach(mfields[f]) begin // Remove all RESERVED fields.
                            fld = mfields[f].get_name();
                            if(!match(fld, Reserved) && !match(fld, RSVD) && !match(fld, "RESERVED" ) ) begin

                                if(after_start) begin
                                    fld_val = mfields[f].get_val(mr[j].get_actual()) ;
                                end else begin
                                    fld_val = mfields[f].get_val(mr[j].get_cfg_val()) ;
                                end

               // fld_val = mfields[f].get_val(mr[j].get_cfg_val()) ;
                                $sformat(fields, "%s[%0d:%0d].%s=%0h %s", mfields[f].get_name(), mfields[f].get_lsb()+mfields[f].get_size()-1,mfields[f].get_lsb(), mfields[f].get_attr(),fld_val,fields );
                            end
                        end
                        if(mr[j].get_space_addr("msg_bus_port") == undef) begin
                            port_num_s =  UNDEF;
                        end else begin
                                $sformat(port_num_s, "0x%0H", mr[j].get_space_addr("msg_bus_port") );
                        end
                        mr[j].get_all_spaces(spaces);
                        list_spaces = "";
                        foreach(spaces[s]) begin
                            $sformat(list_spaces, "%s %s", list_spaces, spaces[s]);
                        end

                        mr[j].get_all_msg_opcode(msg_opc);
                        list_opc = "";
                        foreach(msg_opc[o]) begin
                            $sformat(list_opc, "%s %s", list_opc, msg_opc[o]);
                        end
                
                        prim_addr_s = UNDEF;

                        if(mr[j].get_addr_val("primary") <= 'hFFFFFFFF ) begin
                            $sformat(prim_addr_s, "0x%08h", mr[j].get_addr_val("primary") );
                        end

                        side_addr_s = UNDEF;
                        addr_sb = mr[j].get_space_addr(mr[j].get_space());
                        if(addr_sb <= 'hFFFF) begin
                            $sformat(side_addr_s, "0x%04h",  addr_sb );
                        end

                        if(after_start) begin
                            if (  ^mr[j].get_actual() === 'x) begin
                                changed = 0;
                            end else if((mr[j].get_reset_val() != mr[j].get_actual()) ) begin
                                changed = 1;
                            end
                        end

                  if (changed) begin
                            $sformat(fields, "%s <Change in CSR Fields>", fields,);
                        end // if(!changed)

                            row_count++;
                            if( new_file || (row_count > 50) ) begin
 $fdisplay(fd_all,"+--------------------+----------------------------------+----------------+------------+------------+------+--------+------------+------------+------------------------------------------------------+");
 $fdisplay(fd_all,"| File Name          | Register Name                    | Space          |  OPC       | Reg Addr   | PID  |MSG Addr| Reset Val  | Config Val | Csr Fields - exclude fields named <Reserved, RSVD> - |");
 $fdisplay(fd_all,"+--------------------+----------------------------------+----------------+------------+------------+------+--------+------------+------------+------------------------------------------------------+");
                                row_count = 0;
                                new_file = 0;
                            end

                            if(after_start) begin
                                // if(changed) begin
                                    cfg_value = mr[j].get_actual();
                                // end else begin
                                //     cfg_value = mr[j].get_cfg_val();
                                // end
                            end else begin
                                cfg_value = mr[j].get_cfg_val();
                            end

                            // OLD FORMAT  $fdisplay(fd_all,"| %-18s | %-30s |
                            // Space(%s) PortIDs(%s) Prim_Offsets(%s)
                            // SB_Offset(%s) Default(0x%08h) Fields[%s]",
                            $fdisplay(fd_all,"| %-18s | %-32s | %-14s | %-10s | %-10s | %04s | %-6s | 0x%08H | 0x%08H |  %-50s  ",
                                      files[i].get_name(),  mr[j].get_name(),
                                      list_spaces,list_opc,  prim_addr_s,  port_num_s ,
                                      side_addr_s, mr[j].get_reset_val(), cfg_value  , fields );

                    end
                end
        $fclose(fd_all);
    endfunction // print_all_ral_csrs

endclass


