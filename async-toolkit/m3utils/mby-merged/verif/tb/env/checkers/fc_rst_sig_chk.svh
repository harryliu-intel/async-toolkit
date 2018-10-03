// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  reset signal checker depends on chassis reset checker for 
//                 deriving the current reset state of the system
//
// -----------------------------------------------------------------------------

typedef class fc_tb_env;

// -----------------------------------------------------------------------------
class fc_rst_sig_chk extends uvm_component;
    typedef string stringQ[$];

    tb_utils_pkg::vintf_bundle _vintf_bundle;
    uvm_tlm_analysis_fifo #(chassis_reset_pkg::chassis_boot_reset_seq_item) fifo;  // analysis fifo that gets hooked up to the analysis_port of the monitor.
    chassis_defines::group_flow     flow = chassis_defines::BOOT;

    // thread process
    process P;
    process::state pstate;

    // interfaces
    protected virtual sig_if sigif;

    // misc
    protected bit host_initial_g3;
    protected bit csme_initial_g3;

    local string CLASSNAME = "fc_rst_sig_chk";
    protected UVM_FILE debug_file;
    protected string debug_file_name = "rst_sig_checker.out";

    protected fc_tb_env   tb_env;
    //protected jtag_fuse_env fuse;

    `uvm_component_utils_begin(fc_rst_sig_chk)
    `uvm_component_utils_end

    function new(string name, uvm_component parent = null);
        super.new(name, parent);
        fifo = new({name, "fifo"}, this);
        uvm_report_info(get_name(), $psprintf("new instance fc_rst_sig_chk name = %s", name));
        `slu_assert($cast(tb_env, slu_utils::get_comp_by_name( "tb_env" )), ("fc_tb_env $cast failed"));
    endfunction


    // -----------------------------------------------------------------------
    function void config_debug_reporting();
        set_report_default_file_hier(debug_file);
        set_report_severity_action_hier(UVM_INFO,    UVM_LOG);
        set_report_severity_action_hier(UVM_WARNING, UVM_LOG);
        set_report_severity_action_hier(UVM_ERROR,   UVM_LOG | UVM_DISPLAY | UVM_COUNT);
        set_report_severity_action_hier(UVM_FATAL,   UVM_LOG | UVM_DISPLAY | UVM_EXIT);
    endfunction // config_debug_reporting

    // ------------------------------------------------------------------------
    virtual function void build_phase(uvm_phase phase); 
        uvm_object tmp_obj;
        super.build_phase(phase);
        `slu_assert(uvm_config_object::get(this, "","fc_vintf_bundle", tmp_obj), ("no interface bundle is found in the cfg db"));
        `slu_assert($cast(_vintf_bundle, tmp_obj), ("type mismatch when casting _vintf_bundle"));

        debug_file = $fopen(debug_file_name, "w");
        config_debug_reporting();

    endfunction

    virtual function void connect_phase(uvm_phase phase);
       super.connect_phase(phase);
       tb_env.pmc_subenv.pmc_env.i_pmc_env.ch_env.ch_comp.monitor.ap.connect(fifo.analysis_export);
    endfunction

    // FIXME
    task automatic check_sig(input string name, ref logic sig, input FC::rst_sig_trans_type edge_type, int delay, int seq_enforced);
        bit expired;
        `uvm_info(get_name(), $psprintf("waiting for %s to go %b", sig, edge_type), UVM_LOW);
        fork 
        begin
            // rising edge check
            if (edge_type == FC::RST_SIG_RISE) begin
                if (sig === 0) begin
                    sigif.check_sig_val(name, sig, 1);
                end       else begin
                    if (seq_enforced) 
                        `uvm_error(get_name(), $psprintf("RESET_CHECKER: %0s already at %b by the time the check is performed",name, sig));
                end      
                `uvm_info(get_name(), $psprintf("RESET_CHECKER: %0s is %b now",name,sig), UVM_LOW);
            // falling edge check
            end       else if (edge_type == FC::RST_SIG_FALL) begin 
                if (sig === 1) begin
                    sigif.check_sig_val(name, sig, 0);
                end       else begin
                    if (seq_enforced) 
                        `uvm_error(get_name(), $psprintf("RESET_CHECKER: %0s already at %b by the time the check is performed",name, sig));
                end      
                `uvm_info(get_name(), $psprintf("RESET_CHECKER: %0s is %b now",name,sig), UVM_LOW);
            end       else begin
                `uvm_error(get_name(), $psprintf("RESET_CHECKER: invalid signal level check on %s",name));
            end      
        end      
        begin
            sigif.start_WD_timer(delay, expired);
        end      
        join_any

        disable fork;

        if (expired) `uvm_fatal(get_name(), $psprintf("timed out waiting for %0s de-assertion",name))

    endtask :check_sig

    // ------------------------------------------------------------------------
    // standard UVM run task
    // ------------------------------------------------------------------------
    virtual task run_phase (uvm_phase phase);
        super.run_phase(phase);
    
        sigif = tb_env.get_fc_sig_if();

        `uvm_info(get_name(), "entering task start_reset_checkers", UVM_LOW); 
        `uvm_info(get_name(), "end of fc_rst_sig_chk", UVM_LOW);
    endtask

endclass // fc_rst_sig_chk
