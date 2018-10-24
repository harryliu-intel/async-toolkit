// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :   Raghu P Gudla
// Created On   :   09/22/2018
// Description  :   Top Level Integration testbench
//              
// -----------------------------------------------------------------------------
class fc_tb_env extends slu_tb_env;

    // ENV pointer
    static fc_tb_env _tb_env;

    // Sub-system Environments
    //fc_sm_env sm;
    //

    // static protected sla_ral_env ral;
    //fc_im_env tb_im;

    // File Name manager (contains all the settings for the File Names)
    protected fc_file_names _fileNames;

    // Configuration Objects
    protected fc_cfg_obj fcCfgObj;

    // Interfaces
    protected virtual sig_if  fc_sig_if;
    protected virtual FcDutIf dut_if;

    // RAL
    fc_uvm_reg_map regModel;
  
    // ------------------------------------------------------------------------
    // Debug output files
    // ------------------------------------------------------------------------
    local UVM_FILE _tb_file;
    local UVM_FILE _uvm_hier_file;

    //-- Include All the Sub-system Integ Envs
    `include "fc_sub_env.svh"

    // ------------------------------------------------------------------------
    // Standard function new
    // ------------------------------------------------------------------------
    function new(string n, uvm_component p = null);
        fc_report_server repServ;
        uvm_report_server globserv;

        super.new(n, p);

        if (_tb_env == null)
            _tb_env = this;

        if (_level == SLA_TOP) begin
            ral_type  = "fc_ral_env";
            sm_type   = "fc_sm_env";
            im_type   = "fc_im_env";
        end      

    endfunction : new

    // -------------------------------------------------------------------------
    // Pure virtual function of Saola base class that must be implemented
    // -------------------------------------------------------------------------
    function virtual sig_if get_fc_sig_if();
        return fc_sig_if;
    endfunction

    function virtual FcDutIf getDutIf();
        return dut_if;
    endfunction

    function fc_cfg_obj get_cfg_obj_handle();
        return fcCfgObj;
    endfunction

    function bit getSkipPhaseHandle(string phase_name);
        return _skip_phase_name_q[phase_name];
    endfunction

    // ------------------------------------------------------------------------
    // Reconfigures UVM default table printer
    // - Requires _uvm_hier_file to be open
    // ------------------------------------------------------------------------
    virtual function void setupPrinter();
        uvm_default_table_printer.knobs.name_width     = 60;
        uvm_default_table_printer.knobs.type_width     = 60;
        uvm_default_table_printer.knobs.value_width    = 60;
        uvm_default_table_printer.knobs.begin_elements = 10;
        uvm_default_table_printer.knobs.end_elements   = 10;
        uvm_default_table_printer.knobs.reference      = 0;
        uvm_default_table_printer.knobs.show_root      = 1;
        //ssn uvm_default_table_printer.knobs.indent_str     = "| ";
        uvm_default_table_printer.knobs.mcd            = _uvm_hier_file;
        // For debug purpose only
        uvm_top.enable_print_topology = (m_rh.get_verbosity_level() >= UVM_DEBUG);
    endfunction

    static function fc_tb_env get_tb_env();
        return _tb_env;
    endfunction

    // ------------------------------------------------------------------------
    // Standard UVM build task
    // ------------------------------------------------------------------------
    virtual function void build_phase(uvm_phase phase); 

      string id_str = {get_type_name(),"buildFuseCtrlEnv"};
      `uvm_info(get_name(), "Enter FC build()", UVM_LOW);

      getCfgObj();           // Get a handle to the config object and forward as necessary
      initFileNames();       // Initialize and set the file names
      openFiles();           // Open the various output files we will use

      super.build_phase(phase);

      fetchVintf();       // Fetch the virtual interfaces

      // Build UVM RAL
      uvm_ral_build();

      //-- Build sub-sytem integ envs
      build_sub_system_envs();

      //------------------------------------------------------------
        //This should be last piece of code in the Build function.
        //------------------------------------------------------------
        if (_level == SLA_TOP) begin
            setupTypeOverrides();
            setupPrinter(); // Sets the uvm hier file
        end      

        // ------------------------------------------------------------------------
        // Build Reset Checker
        // This object should be built after FC setupPrinter so as to ensure
        // the reset tracker file is generated properly
        // ------------------------------------------------------------------------
        //buildResetChecker();

        `uvm_info(get_name(), "Exit FC build()", UVM_LOW);
    endfunction : build_phase

    // ------------------------------------------------------------------------
    // Initialize the file names
    // ------------------------------------------------------------------------
    function void initFileNames();
        _fileNames = fc_file_names::type_id::create(FC::FILE_CLASS);
        _fileNames.init_table(this);
    endfunction

    // ------------------------------------------------------------------------
    // Open the files
    // ------------------------------------------------------------------------
    function void openFiles();
        if (_level == SLA_TOP) begin
            _uvm_hier_file = $fopen(_fileNames.get_file_name(FC::UVM_FILE), "w");
        end      
        _tb_file          = $fopen(_fileNames.get_file_name(FC::TB_FILE),  "w");
    endfunction

    // ------------------------------------------------------------------------
    // Gets a handle to the config object - this is only necessary because
    // we must push the config object to ral/sm prior to calling super.build_phase(phase)
    // so the uvm_field_object macro won't auto-populate the variable in time
    // ------------------------------------------------------------------------

    function void getCfgObj();
        bit        rc;
        uvm_object temp;

        // Get handle to CfgObj
        rc = uvm_config_object::get(this, "",FC::CFG_OBJ, temp);
        `slu_assert(rc, ("No fcCfgObj found in the cfg db"));

        // Get handle to CfgObj
        rc = $cast(fcCfgObj, temp);
        `slu_assert(rc, ("Failed to cast fcCfgObj"));

        fcCfgObj.set_cluster(this);
    endfunction

    // ------------------------------------------------------------------------
    // Fetch interface bundle, and specific virtual interface out of bundle
    // ------------------------------------------------------------------------

    function void fetchVintf();
        uvm_config_db#(virtual sig_if)::get(this, "", "sig_if", fc_sig_if);
    endfunction

    // ------------------------------------------------------------------------
    // Standard UVM connect task
    // ------------------------------------------------------------------------
    virtual function void connect_phase(uvm_phase phase);

      `uvm_info(get_name(), "Enter FC connect()", UVM_LOW);

      super.connect_phase(phase);

      // Connect UVM RAL
      uvm_ral_connect();

      `uvm_info(get_name(), "Exit FC connect()", UVM_LOW);
    endfunction

    //--------------------------------------------------------------------------
    // UVM end_of_elaboration function.
    //--------------------------------------------------------------------------
    function void end_of_elaboration_phase(uvm_phase phase);
        super.end_of_elaboration_phase(phase);
        // The report file has to be set here as
        // creating components resets this. Memory map is
        // populated during elaboration phase and resets this
        uvm_top.set_report_default_file_hier(_tb_file);
        //ovm_top.print_config_setting(,ovm_top,1);
        //uvm_top.print_config_setting(,uvm_top,1);
    endfunction : end_of_elaboration_phase

    // -----------------------------------------------------------------------
    virtual function void start_of_simulation_phase(uvm_phase phase);
        super.start_of_simulation_phase(phase);
    endfunction

    // ------------------------------------------------------------------------
    // Connect RAL
    // ------------------------------------------------------------------------
    virtual function void connectRAL();

    endfunction

    // ------------------------------------------------------------------------
    // Standard UVM run task
    // ------------------------------------------------------------------------
    virtual task run_phase (uvm_phase phase);
        super.run_phase(phase);

    endtask

    // ------------------------------------------------------------------------
    // Setup uvm message redirection. It is recommended to re-direct UVM_INFO
    // and UVM_WARNING into logfile only and not stdout. This helps in reducing
    // the size of the test log file which is parsed by the postsim error handler
    // ------------------------------------------------------------------------
    virtual function void set_report_hier();
        if (_level == SLA_TOP) begin
            uvm_top.set_report_default_file_hier(_tb_file);
            uvm_top.set_report_severity_action_hier(UVM_INFO,    UVM_LOG);
            uvm_top.set_report_severity_action_hier(UVM_WARNING, UVM_LOG);
            uvm_top.set_report_severity_action_hier(UVM_ERROR,   UVM_DISPLAY | UVM_LOG);
            uvm_top.set_report_severity_action_hier(UVM_FATAL,   UVM_DISPLAY | UVM_LOG | UVM_EXIT);
        end      
    endfunction: set_report_hier

    // ------------------------------------------------------------------------
    // Standard UVM check task
    // ------------------------------------------------------------------------
    virtual function void check_phase(uvm_phase phase);
        super.check_phase(phase);
    endfunction

    // ------------------------------------------------------------------------
    protected virtual function void setupReporting(string name, UVM_FILE file, bit child = 1);
        uvm_component tmpobj;

        if ( child ) begin
            tmpobj = lookup(name);
        end       else begin
            tmpobj = uvm_top.find(name);
        end      
        `slu_assert(tmpobj, ("Could not find %s, child = %0d", name, child));

        `uvm_info(get_name(), $sformatf("Setting up reporting for %s", tmpobj.get_full_name()), UVM_LOW);

        tmpobj.set_report_default_file_hier(file);
        tmpobj.set_report_verbosity_level_hier(uvm_top.get_report_verbosity_level());
        tmpobj.set_report_severity_action_hier(UVM_INFO,    UVM_LOG);
        tmpobj.set_report_severity_action_hier(UVM_WARNING, UVM_LOG);
        tmpobj.set_report_severity_action_hier(UVM_ERROR,   UVM_DISPLAY | UVM_LOG);
        tmpobj.set_report_severity_action_hier(UVM_FATAL,   UVM_DISPLAY | UVM_LOG | UVM_EXIT);
    endfunction

    // ------------------------------------------------------------------------
    protected virtual function void setupLogs();
        set_report_default_file_hier(_tb_file);
        set_report_verbosity_level_hier(uvm_top.get_report_verbosity_level());

        setupReporting("ral", _tb_file);
        //setupReporting(FC::CCU_VC_INST_NAME, _bfm_file);
    endfunction

    // ------------------------------------------------------------------------
    // UVM factory overrides
    // ------------------------------------------------------------------------
    virtual function void setupTypeOverrides();
        // Set default type overrides here
    endfunction

    // -----------------------------------------------------------------------
    // UVM RAL Build method 
    // -----------------------------------------------------------------------
    virtual function void uvm_ral_build ();
       regModel = fc_uvm_reg_map::type_id::create("regModel");
       regModel.build();
       fcCfgObj.reg_model = regModel;
    endfunction : uvm_ral_build

    // -----------------------------------------------------------------------
    // UVM RAL Connect method 
    // -----------------------------------------------------------------------
    virtual function void uvm_ral_connect ();
      //`ifdef AXI_ENV_ENABLE
      //  if (_tb_env.axi_subenv.axi_bfm_cfg.master_cfg[0].is_active == UVM_ACTIVE) begin
      //    regModel.axi_reg_map.set_sequencer(axi_seqr, _tb_env.axi_subenv.axi_bfm.reg2axi_adapter);
      //    //regModel.apb_reg_map.set_sequencer(axiSeqr, _tb_env.axi_subenv.axi_bfm.reg2axi_adapter);
      //  end
      //`endif
    endfunction : uvm_ral_connect

    // -----------------------------------------------------------------------
    // Task to communiate all clock and reset activity to Saola system manager
    // -----------------------------------------------------------------------
    virtual task set_clk_rst();
        fork
            forever begin
                @(fc_sig_if.tb_clk);
                #0;

                if (fc_sig_if.tb_clk === 1'b1) begin
                    ->ral.ref_clk;
                    ->sys_clk_r;
                end      

                if (fc_sig_if.tb_clk === 1'b0) begin
                    ->sys_clk_f;
                end      
            end      
            forever begin
                @(fc_sig_if.tb_rst_b);
                #0;

                if (fc_sig_if.tb_rst_b === 1'b1) begin
                    ->sys_rst_r;
                end      

                if (fc_sig_if.tb_rst_b === 1'b0) begin
                    ->sys_rst_f;
                end      
            end      

        join_none;
    endtask

    // ------------------------------------------------------------------------
    `uvm_component_utils_begin(fc_tb_env)
    `uvm_component_utils_end

endclass
