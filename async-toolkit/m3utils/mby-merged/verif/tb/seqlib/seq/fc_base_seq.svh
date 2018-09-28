// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  : 
// -----------------------------------------------------------------------------
typedef enum {WR_RD,WR,RD} RW_TRANS;
typedef class fc_base_seq;

virtual class fc_base_seq extends uvm_sequence;

    protected fc_tb_env           tb_env;
    protected fc_cfg_obj          cfg_obj;
    //fc_fuse_env fuse;

    protected virtual sig_if      fc_sig_if;
    //protected virtual fc_dut_if     dut_if;
    protected string              access_path;
    //protected slaral_env        ral_env;
    //protected mem_access         mem_access;
    //    fc_sm_env sm;
    //    protected fc_im_env tb_im;
    pmu_mmr_file  pmu_mmr;

    slu_status_t status;
    slu_ral_data_t rd_val;
    bit rc;
    uvm_pkg::uvm_object tmp_obj;

    // reference to RAL
    // note using opi specifc RAL to make sequence portable
    // pointer to the top level saola environment
    static protected sla_ral_env ral;
    string inst,port;

    //local param &def

    `ifdef USE_SPARE_SB_BFM
    iosfsbm_cm::iosfsbc_sequencer    iosfsbc_sqr;
    //ssn uvm_sequencer_base               sqr;
    `endif

    `ifdef PMU_ENV_ENABLE
     //protected pmu_ral_pkg::pmu_mmr_file        pmu_mmr;
    `endif 


    //`ifdef HSUART_ENV_ENABLE
    // protected hsu_pkg::dnv_hsuart_regs_fnx_file hsuart_regs_fn[3];
    //`endif 

    //`ifdef IOW_ENV_ENABLE
    // protected iow_saola_pack::iow_regs_fnx_file iow_regs_fn;
    //`endif 

    `ifdef GPIO_ENV_ENABLE
     protected gpio_env_pkg::gpio_mem_MST_file gpio_mem_reg;
    `endif

    // -----------------------------------------------------------------------
    function new(string name="fc_base_seq");
        super.new(name);
        get_tb_env();   // gets a handle to the fc_tb_env

        //`slu_assert($cast(fuse, slu_fuse_env::get_ptr()), ("unable to get handle to fc_fuse_env."));

        // get RAL reference
        `slu_assert( ($cast(ral, sla_ral_env::get_ptr()) && (ral != null)), ("unable to get handle to RAL"));
        //`slu_assert($cast(sm,slu_sm_env::get_ptr()), ("unable to get handle to SM"));
   
        //ssn `ifdef USE_SPARE_SB_BFM
        //ssn     sqr = slu_sequencer::pick_sequencer("spare_iosf_sb");
        //ssn `endif

        `ifdef PMU_ENV_ENABLE
        `slu_assert( ($cast(pmu_mmr, ral.find_file("pmu_mmr")) && (pmu_mmr != null)), ("unable to get handle to PMU MMR register file"));
        `endif

        `ifdef ICM_ENV_ENABLE
        `slu_msg (UVM_MEDIUM,get_full_name(),("RAL handler initialization for ICM"))
        `slu_assert( ($cast(icm_ipp_globals, ral.find_file("icm_ipp_globals")) && icm_ipp_globals != null), ("unable to get handle to ICM register file icm_ipp_globals "));
        `slu_assert( ($cast(icm_ipp_dp, ral.find_file("icm_ipp_dp")) && (icm_ipp_dp != null)), ("unable to get handle to ICM register file icm_ipp_dp "));
        `slu_assert( ($cast(icm_ipp_rp, ral.find_file("icm_ipp_rp")) && (icm_ipp_rp != null)), ("unable to get handle to ICM register file icm_ipp_rp "));
        `slu_assert( ($cast(icm_ipp_afi, ral.find_file("icm_ipp_afi")) && (icm_ipp_afi != null)), ("unable to get handle to ICM register file icm_ipp_afi "));
        `slu_assert( ($cast(icm_ipp_cpp, ral.find_file("icm_ipp_cpp")) && (icm_ipp_cpp != null)), ("unable to get handle to ICM register file icm_ipp_cpp "));

        `slu_assert( ($cast(icm_tile0_ipret_ap_pe, ral.find_file("i_proc_tile_slu_env.icm_ipret_ap_pe")) && (icm_tile0_ipret_ap_pe != null)), ("unable to get handle to ICM register file icm_tile0_ipret_ap_pe "));
        `slu_assert( ($cast(icm_tile0_ipret_ap_profiler, ral.find_file("i_proc_tile_slu_env.icm_ipret_ap_profiler")) && (icm_tile0_ipret_ap_profiler != null)), ("unable to get handle to ICM register file icm_tile0_ipret_ap_profiler "));
        `slu_assert( ($cast(icm_tile0_ipret_pp_pe, ral.find_file("i_proc_tile_slu_env.icm_ipret_pp_pe")) && (icm_tile0_ipret_pp_pe != null)), ("unable to get handle to ICM register file icm_tile0_ipret_pp_pe "));
        `slu_assert( ($cast(icm_tile0_ipret_pp_profiler, ral.find_file("i_proc_tile_slu_env.icm_ipret_pp_profiler")) && (icm_tile0_ipret_pp_profiler != null)), ("unable to get handle to ICM register file icm_tile0_ipret_pp_profiler "));

        `slu_assert( ($cast(icm_tile0_imidt_ap_pe, ral.find_file("i_proc_tile_slu_env.icm_imidt_ap_pe")) && (icm_tile0_imidt_ap_pe != null)), ("unable to get handle to ICM register file icm_tile0_imidt_ap_pe "));
        `slu_assert( ($cast(icm_tile0_imidt_ap_profiler, ral.find_file("i_proc_tile_slu_env.icm_imidt_ap_profiler")) && (icm_tile0_imidt_ap_profiler != null)), ("unable to get handle to ICM register file icm_tile0_imidt_ap_profiler "));

        `slu_assert( ($cast(icm_tile0_ipostt_pp_pe, ral.find_file("i_proc_tile_slu_env.icm_ipostt_pp_pe")) && (icm_tile0_ipostt_pp_pe != null)), ("unable to get handle to ICM register file icm_tile0_ipostt_pp_pe "));
        `slu_assert( ($cast(icm_tile0_ipostt_pp_profiler, ral.find_file("i_proc_tile_slu_env.icm_ipostt_pp_profiler")) && (icm_tile0_ipostt_pp_profiler != null)), ("unable to get handle to ICM register file icm_tile0__ipostt_pp_profiler "));
        `slu_assert( ($cast(icm_tile0_ipost_chk, ral.find_file("i_proc_tile_slu_env.icm_ipost_chk")) && (icm_tile0_ipost_chk != null)), ("unable to get handle to ICM register file icm_tile0_ipost_chk "));

        `slu_assert( ($cast(icm_sadb_ctrl, ral.find_file("icm_sadb_ctrl")) && (icm_sadb_ctrl != null)), ("unable to get handle to ICM register file icm_sadb_ctrl "));
        `slu_assert( ($cast(icm_sadb_wqm, ral.find_file("icm_sadb_wqm")) && (icm_sadb_wqm != null)), ("unable to get handle to ICM register file icm_sadb_wqm "));
        `slu_assert( ($cast(icm_pinned_rb, ral.find_file("icm_pinned_rb")) && (icm_pinned_rb != null)), ("unable to get handle to ICM register file icm_pinned_rb "));
        `slu_assert( ($cast(icm_tag_ram, ral.find_file("icm_tag_ram")) && (icm_tag_ram != null)), ("unable to get handle to ICM register file icm_tag_ram "));
        `slu_assert( ($cast(icm_sa_table, ral.find_file("icm_sa_table")) && (icm_sa_table != null)), ("unable to get handle to ICM register file icm_sa_table "));
        `slu_assert( ($cast(icm_wkt, ral.find_file("icm_wkt")) && (icm_wkt != null)), ("unable to get handle to ICM register file icm_wkt "));
        `slu_assert( ($cast(icm_cache_ctrl, ral.find_file("icm_cache_ctrl")) && (icm_cache_ctrl != null)), ("unable to get handle to ICM register file icm_cache_ctrl "));
        `endif

        `ifdef HSUART_ENV_ENABLE
        `slu_assert( ($cast(hsuart_regs_fn[0], ral.find_file("dnv_hsuart_regs_fn0")) && (hsuart_regs_fn[0] != null)), ("unable to get handle to HSUART fn0 register file"))
        `slu_assert( ($cast(hsuart_regs_fn[1], ral.find_file("dnv_hsuart_regs_fn1")) && (hsuart_regs_fn[1] != null)), ("unable to get handle to HSUART fn1 register file"))
        `slu_assert( ($cast(hsuart_regs_fn[2], ral.find_file("dnv_hsuart_regs_fn2")) && (hsuart_regs_fn[2] != null)), ("unable to get handle to HSUART fn2 register file"));
        `endif
        
        `ifdef IOW_ENV_ENABLE
        `slu_assert( ($cast(iow_regs_fn, ral.find_file("iow_regs_fnx")) && (iow_regs_fn != null)), ("unable to get handle to IOWidget register file"))
        `endif
   
        `ifdef GPIO_ENV_ENABLE
        `slu_assert( ($cast(gpio_mem_reg, ral.find_file("gpio_mem_MST")) && (gpio_mem_reg != null)), ("unable to get handle to GPIO mem register file"))
        `endif

    endfunction

    // -----------------------------------------------------------------------
    // gets a handle to fc_tb_env
    // -----------------------------------------------------------------------
    function void get_tb_env();
        `slu_assert($cast(tb_env, slu_utils::get_comp_by_name("tb_env")),
                    ($sformatf("fc_tb_env $cast failed to %s", "tb_env")));
        `slu_assert(tb_env, ($sformatf("could not fetch %s handle", "tb_env")));

        //`slu_assert($cast(tb_im,slu_im_env::get_ptr()) && tb_im != null, ("unable to get handle to saola IM"));
        
        fc_sig_if   = tb_env.get_fc_sig_if();
        //dut_if      = tb_env.get_dut_if();
        cfg_obj     = tb_env.get_cfg_obj_handle();
        //tb_im       = tb_env.fc_intr_env;
        //tb_im       = tb_env.get_fc_im_env_handle();
    endfunction

    // -----------------------------------------------------------------------
    // get a handle to the FC ral file
    // -----------------------------------------------------------------------
    //function void get_fc_ral_file();
    //    // get a handle to the slaral_env
    //    $cast(ral_env, slaral_env::get_ptr());
    //    `slu_assert( (ral_env != null), ("unable to get ral handle"));
    //endfunction

    // -----------------------------------------------------------------------

    //ssn: doesn/t seem to be required. causes run phase to end       without
    //executing other saola phases
    //virtual task pre_body(); // added to keep tests from quitting early.
    //    uvm_test_done.raise_objection(this);
    //    `uvm_info(get_type_name(), "raise objection ", UVM_MEDIUM)
    // endtask

    virtual task body();
        `uvm_info(get_name(), "inside sequence body of fc_base_seq", UVM_MEDIUM);

    endtask: body

    // -----------------------------------------------------------------------
    // search all maps and return the first tag that matches the specified
    // string.
    // -----------------------------------------------------------------------
    virtual function slu_sm_am_tag find_tag(string name);
        int tag_index[$];
        slu_sm_env sm = slu_sm_env::get_ptr();

        foreach (sm.am.maps[j]) begin
            tag_index = sm.am.maps[j].tags.find_index(item) with (item.tagname_str == name);
            if (tag_index.size() != 0) begin
                return sm.am.maps[j].tags[tag_index[0]];
            end      
        end      

        `uvm_error(get_name(), $sformatf("failed to find tag named %s", name));
        return null;

    endfunction

    // -----------------------------------------------------------------------
    function sla_pkg::addr_t get_low_addr(string range);
        slu_sm_am_tag tag;
        tag = find_tag(range);
        `slu_assert(tag, ("could not find tag %s", range));
        return tag.low_addr;
    endfunction

    // -----------------------------------------------------------------------
    function sla_pkg::addr_t get_high_addr(string range);
        slu_sm_am_tag tag;
        tag = find_tag(range);
        `slu_assert(tag, ("could not find tag %s", range));
        return tag.high_addr;
    endfunction

    // -----------------------------------------------------------------------
    function sla_pkg::addr_t get_low_result(string map_name);
        slu_sm_ag_result ag_result;
        slu_sm_env sm = slu_sm_env::get_ptr();

        if (sm.is_in_hash(map_name)) begin
            ag_result = sm.get_sm_hash(map_name); 
            return ag_result.addr;
        end       else begin
            `uvm_error(get_name(), $sformatf("failed to find tag named %s", map_name));
        end       

    endfunction
    // -----------------------------------------------------------------------
    function sla_pkg::addr_t get_high_result(string map_name);
        slu_sm_ag_result ag_result;
        sla_pkg::addr_t high_addr;
        slu_sm_env sm = slu_sm_env::get_ptr();

        if (sm.is_in_hash(map_name)) begin
            ag_result = sm.get_sm_hash(map_name); 
            high_addr =  ag_result.addr + ag_result.length - 1; 
            return high_addr;
        end       else begin
            `uvm_error(get_name(), $sformatf("failed to find tag named %s", map_name));
        end       
    endfunction

    // -----------------------------------------------------------------------

endclass: fc_base_seq

