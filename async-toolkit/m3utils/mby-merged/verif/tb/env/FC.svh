// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  FC Testbench Enums, Constants and static methods
// -----------------------------------------------------------------------------


class FC;

    // CCU VC constants
    localparam int unsigned REF_CLK_SRC          = 0;
    localparam int unsigned REF_CLK_SLICE        = 0;
    localparam int unsigned PRIM_CLK_SRC          = 1;
    localparam int unsigned PRIM_CLK_SLICE        = 1;

    static const string CCU_VC_INST_NAME         = "ccu_vc_dut";
    static const string CCU_VC_NAME              = "ccu_vc_dut";


    // config names
    static const string FC_TBENV_NAME            = "tb_env";
    static const string FC_CFG_OBJ               = "fc_cfg_obj";
    static const string FC_TEST_CFG_OBJ          = "fc_test_cfg_obj";
    static const string FC_CHIP_CFG_OBJ          = "fc_chip_cfg_obj";
    static const string FC_RST_CFG_OBJ           = "fc_rst_cfg_obj";
    static const string RTL_HIER_NAME            = "rtl_hierarchy";
    static const string CLUSTER_INST_NAME        = "cluster_inst_name";
    static const string MEMACCESS_NAME           = "fc_memaccess";
    static const string CFG_OBJ                  = "fc_cfg_obj";
    static const string FUSE_CFG_OBJ             = "fc_fuse_cfg_obj";
    static const string FC_IFWRAP_NAME           = "fc_sig_intf_wrapper";
    static const string TI_CFG_OBJ               = "ti_cfg";



    // interface mode
    typedef bit[255:0] ifmode_var_type;
    `define ONE_HOT(idx) (256'h1 << idx)
    typedef enum ifmode_var_type {
        // PIN GROUPS
        CONNECT_CLK_IF                  = `ONE_HOT(0),
        CONNECT_RST_IF                  = `ONE_HOT(1),
        CONNECT_MPHY_EXT_CLK_IF         = `ONE_HOT(3),
        CONNECT_JTAG0_IF                = `ONE_HOT(10),
        CONNECT_JTAG1_IF                = `ONE_HOT(11),
        CONNECT_JTAG2_IF                = `ONE_HOT(12),
        CONNECT_TAM_IF                  = `ONE_HOT(13),
        CONNECT_HVM_IF                  = `ONE_HOT(14),
        CONNECT_HVM_STRAP_IF            = `ONE_HOT(15),
        CONNECT_BURNIN_IF               = `ONE_HOT(16),
        CONNECT_SCAN_IF                 = `ONE_HOT(17),
        DISABLE_LOOPBACK_TX_TO_RX       = `ONE_HOT(18),
        DISABLE_LOOPBACK_KR_TX_TO_RX    = `ONE_HOT(19),

        //HVM HARD STRAPS

        // FLOW CHANGES
        RUN_FUNC_RESET_FLOW             = `ONE_HOT(60),
        RUN_QUICK_RESET_FLOW            = `ONE_HOT(61),
        RUN_BSCAN_RESET_FLOW            = `ONE_HOT(62),
        RUN_HVM_RESET_FLOW              = `ONE_HOT(63),
        RUN_SHORT_HVM_RESET_FLOW        = `ONE_HOT(64),
        RUN_MID_HVM_RESET_FLOW          = `ONE_HOT(65),
        RUN_FULL_RESET_TIMERS           = `ONE_HOT(66),
        RUN_JTAG_FUSE_OVERRIDE          = `ONE_HOT(67),
        RUN_SLOW_JTAG_CLOCK             = `ONE_HOT(68),
        RUN_HALF_SPEED_JTAG_CLOCK       = `ONE_HOT(69),
        RUN_UNLOCK_ONLY_HVM_RESET_FLOW  = `ONE_HOT(70),
        RUN_FUSE_SHORT_RESET_FLOW       = `ONE_HOT(71),
        RUN_CUSTOMER_RESET_FLOW         = `ONE_HOT(72),
        FREE_RUNNING_RTCX1              = `ONE_HOT(73),
        HALTED_RTCX1                    = `ONE_HOT(74),

        // TAM/HBP variations
        FORCE_SCAN_CHAINS               = `ONE_HOT(180),
        SCANMODE_ATSPEED                = `ONE_HOT(181),
        PGCB_OVERRIDE                   = `ONE_HOT(182),
        RUN_PGCB_REG_WRITE              = `ONE_HOT(183),
        DISABLE_PGCB_CHECKS             = `ONE_HOT(184),
        SKIP_DFXMAS_PGCB_WRITE          = `ONE_HOT(185),
        TAPCR_RESET_OVRD                = `ONE_HOT(186),

        DISABLE_ASSERTIONS              = `ONE_HOT(190),
        DISABLE_RU                      = `ONE_HOT(191),
        SWITCH_CMSE_IE_CLK              = `ONE_HOT(192),
        SKU_3                           = `ONE_HOT(193),
        SKU_4                           = `ONE_HOT(194),
        SKU_6                           = `ONE_HOT(195),
        //BCLK and DPCLK configuration for lock test
        BCLK_PLL_LOCK                   = `ONE_HOT(200),
        DPCLK_PLL_LOCK                  = `ONE_HOT(201),

        // PLL monitor code
        USB_MON                         = `ONE_HOT(210),
        WM20_GEN2_PLL_MON               = `ONE_HOT(211),
        WM20_GEN3_PLL_MON               = `ONE_HOT(212),
        WM26_GEN2_PLL_MON               = `ONE_HOT(213),
        WM26_GEN3_PLL_MON               = `ONE_HOT(214),
        WM26_SATA_PLL_MON               = `ONE_HOT(215),
        WM_PHY_MONITOR_SEQ              = `ONE_HOT(216),
        USB_BURNIN_MON                  = `ONE_HOT(217),

        // pattern optimazations
        SKIP_DUPLICATE_IRS              = `ONE_HOT(220),
        DOUBLE_FREQ_FUSE_OVERRIDE       = `ONE_HOT(221),
        SKIP_MPHY_RCOMP_DELAY           = `ONE_HOT(222),
        SHORTEN_MPHY_CRI_WR_DELAY       = `ONE_HOT(223),
        SKIP_CSRWR_SBMSGRSP             = `ONE_HOT(224),
        SKIP_MISR_READ_AT_START         = `ONE_HOT(225),
        ISOLATE_BRANCH_MODE             = `ONE_HOT(226),
        MULTIPLE_ISOLATE_MODE           = `ONE_HOT(227),
        SKIP_DEBUG_READS                = `ONE_HOT(228),
        MODE_NOT_SET                    = 0
    } interface_mode_type;

    /*
    typedef enum ifmode_var_type {
        //COMPOUND_MODES
        RUN_FULL_HVM_RESET_FLOW  = RUN_HVM_RESET_FLOW | RUN_FULL_RESET_TIMERS | FUSE_SENSE_NOT_BYPASSED | RUN_JTAG_FUSE_OVERRIDE | SKIP_FUSE_RAM_FORCES | RUN_SLOW_JTAG_CLOCK | UNGATE_ALL_CLOCKS | LOCK_USB_PLLS,
        CONNECT_ALL_CLKRST_IF    = CONNECT_CLK_IF | CONNECT_RST_IF,
        BASIC_HVM_MODE           = CONNECT_ALL_CLKRST_IF | CONNECT_JTAG0_IF | CONNECT_JTAG1_IF | CONNECT_HVM_IF | CONNECT_HVM_STRAP_IF | RUN_HVM_RESET_FLOW,
        BURNIN_MODE              = BASIC_HVM_MODE | CONNECT_BURNIN_IF | FORCE_SCAN_CHAINS | XTAL_FREQ_100MHZ | DISABLE_LOOPBACK_TX_TO_RX | DISABLE_ASSERTIONS | DISABLE_LOOPBACK_TX_TO_RX | RUN_FULL_HVM_RESET_FLOW,
        SCAN_MODE                = BASIC_HVM_MODE | CONNECT_SCAN_IF | FORCE_SCAN_CHAINS | LOCK_USB_PLLS | TAPCR_RESET_OVRD | DISABLE_ASSERTIONS,
        GLS_RESET_MODE           = BASIC_HVM_MODE | RUN_HVM_RESET_FLOW | RUN_FULL_RESET_TIMERS,

        //SUL (DLCS=MFG, UNLOCK_L, MMP), IUL (DLCS=PROD, UNLOCK_R, PHTM FUSE)
        SECURITY_UNLOCKED        = BASIC_HVM_MODE | INITIAL_HVM_RESET | SKIP_VOLTAGE_DETECT_FORCE | SKIP_SECURITY_FORCE | DISABLE_ASSERTIONS | RUN_FULL_HVM_RESET_FLOW,

        //BCLK and DPCLK test
        RUN_BCLK_LOCK_TEST       = BASIC_HVM_MODE | WAIT_FOR_PLTRSTB | BCLK_PLL_LOCK | DISABLE_RU,
        RUN_DPCLK_LOCK_TEST      = BASIC_HVM_MODE | WAIT_FOR_PLTRSTB | DPCLK_PLL_LOCK | DISABLE_RU,

        COMP_MODE_NOT_SET        = 0
    } compound_interface_mode_type;
    */

    typedef enum reg[15:0] {
        // {2'b00, decode_mode[1:0], psf[3:0], port_group[0:0], port[3:0], ch[2:0]}
                                            //                       PSF   PORT
        SS3VS0_SRCDEC_DSTID = 16'h2788,     // PSF7 port  1 -> 0010_0111_1_0001_000
        SS3VS1_SRCDEC_DSTID = 16'h2790,     // PSF7 port  2 -> 0010_0111_1_0010_000
        SS3VS2_SRCDEC_DSTID = 16'h2798,     // PSF7 port  3 -> 0010_0111_1_0011_000
        SS3VS3_SRCDEC_DSTID = 16'h27A0,     // PSF7 port  4 -> 0010_0111_1_0100_000
        SS3VS4_SRCDEC_DSTID = 16'h27A8,     // PSF7 port  5 -> 0010_0111_1_0101_000
        SS3VS5_SRCDEC_DSTID = 16'h27B0      // PSF7 port  6 -> 0010_0111_1_0110_000

    } source_decode_dest_id_type;

    // BDF numbers from "table 6-7. LBG PCI devices and functions"

    typedef enum reg[31:0] {
        CFIO_PAD_VAL_INPUT   = 32'b010,
        CFIO_PAD_VAL_OUTPUT  = 32'b100,
        CFIO_PAD_VAL_DISABLE = 32'b110
    } cfio_pad_val_type;

    // -------------------------------------------------------------------------
    // performance enums
    // -------------------------------------------------------------------------


    typedef enum int {
      NTI_SEC_JTAG,
      NTI_DFX_MON,
      NTI_HBP,
      NTI_CPU_OVRD,
      NTI_SPI_CLK_OVRD,
      NTI_ADSP_HVM_MODE,
      NTI_SCC_SMT_CLK_OVR,
      NTI_TAM1,
      NTI_SCAN,
      NTI_BIMON,
      NTI_HVM_CLK_OVRD,
      NTI_PCI_CLK_OVRD,
      NTI_PCI_DLY_CLK_OVRD
    } scanunit_nti_select;

    // rst_sig_chk
    typedef enum bit {
        RST_SIG_RISE = 'b1,
        RST_SIG_FALL = 'b0
    } rst_sig_trans_type;

    // JTAG
    static const string   JTAG_SQR_NAME0        = "jtag_controller_sequencer0";
    static const string   JTAG_SQR_NAME1        = "jtag_controller_sequencer1";
    static const string   JTAG_SQR_NAME2        = "jtag_controller_sequencer2";
    static const string   JTAG_SQR_NAME3        = "jtag_controller_sequencer3";
    static const string   JTAG_SQR_NAME4        = "jtag_controller_sequencer4";
    static const string   JTAG_SQR_NAME[4:0]    =  '{JTAG_SQR_NAME4, JTAG_SQR_NAME3, JTAG_SQR_NAME2, JTAG_SQR_NAME1, JTAG_SQR_NAME0};
    static const string   JTAGSQRTYPE           = "jtag_controller_sequencer";
    static const string   JTAGBFM_SQR_NAME      = "jtag_prim";
    static const string   FPK_JTAGBFM_SQR_NAME  = "jtag_fpk";

    // NTI
    static const string   NTI_SQR_NAME          = "nti2Sb_bfm_sequencer0";
    static const string   NTISQRTYPE            = "nti2Sb_bfm_sequencer";

    // TAM
    static const string   TAM_SQR_NAME          = "atebfm_seqr";

    // SAOLA SEQ
    static const string   SAOLA_SEQ_NAME        = "tb_env";
    static const string   SAOLASQRTYPE          = "SLA_SEQUENCER";

    // interfaces
    static const string   FCSIGIFNAME           = "fc_sig_if";
    static const string   FCDUTIFNAME           = "fc_dut_if";

    static virtual force_if apply_forces;

// <<< default parameters for IOSF-SB interface
    `ifndef DEF_IOSF_SB_IF_PARAMS
        `define DEF_IOSF_SB_IF_PARAMS \
                .AGENT_MASTERING_SB_IF(0), \
                .PAYLOAD_WIDTH(8), \
                .CLKACK_SYNC_DELAY(4)
    `endif
// >>>


    static const string VINTF_BUNDLE_NAME       = "fc_vintf_bundle";

    // IOSF sideband
    static const string   SB_VINTF_BUNDLE_NAME  = "sb_vintfbundle";

    // file access names (used by get/set_config_string
    static const string FILE_CLASS               = "fc_file_names";
    static const string BFM_FILE                 = "BFM_FILE";
    static const string UVM_FILE                 = "UVM_FILE";
    static const string TB_FILE                  = "TB_FILE";

    //
    // all methods are specified after this define. protecting the methods around
    // the define ensures that "module" code can still access all the constants and
    // parameters without having to import all the UVM/saola overhead
    //
`ifndef SKIP_FC_METHODS

    // -------------------------------------------------------------------------
`ifdef JTAG_ONLY
    static function virtual dfts_if get_dfts_if();
        static virtual dfts_if pins;
        slu_vif_container #(virtual dfts_if) wrapper;
`else
    static function virtual sig_if getsig_if();
        static virtual sig_if pins;
        slu_vif_container #(virtual sig_if) wrapper;
`endif
        if (pins == null) begin
            //bit rc;
            //uvm_object tmp_obj;
            //tb_utils_pkg::vintf_bundle vintf_bundle;

            //rc = uvm_top.get_config_object(FC::VINTF_BUNDLE_NAME, tmp_obj, 0);
            //`slu_assert(rc, ("no interface bundle is found in the cfg db"));

            //rc = ($cast(vintf_bundle, tmp_obj));
            //`slu_assert(rc, ("type mismatch when casting vintf_bundle"));

            //rc = $cast(wrapper, vintf_bundle.get_data(FC::FCSIGIFNAME));
            //`slu_assert(rc, ("interface wrapper is of incorrect type for %s", FC::FCSIGIFNAME));

            //uvm_config_db#(virtual sig_if)::get(this, "", "sig_if", pins);

            //pins = wrapper.get_v_if();
        end      
        return pins;
    endfunction

    // -------------------------------------------------------------------------
`ifdef JTAG_ONLY
    static function void set_ifmode(ifmode_var_type mode);
        interface_mode_type mode_iter;
        static virtual dfts_if pins;
        string names = "";
        ifmode_var_type diff_mode = 0;
        if (pins == null) begin
            pins = get_dfts_if();
            diff_mode = ~diff_mode;     // set all diff bits first time fuction is called
        end      
        else begin
            diff_mode = pins.ifmode_val ^ mode;  // set changed bits
        end      
        pins.ifmode_val = mode;
        mode_iter = mode_iter.first;
        forever begin
            if(mode_iter & diff_mode) begin
                if(mode_iter & mode) begin
                    if($test$plusargs({mode_iter.name,"=0"})) begin
                        `uvm_info("set_ifmode", $sformatf("clearing ifmode bit %s due to plusarg", mode_iter.name), UVM_NONE)
                        pins.ifmode_val = pins.ifmode_val & ~mode_iter;
                    end      
                end      
                else begin
                    if($test$plusargs(mode_iter.name) && !$test$plusargs({mode_iter.name,"=0"})) begin
                        `uvm_info("set_ifmode", $sformatf("setting ifmode bit %s due to plusarg", mode_iter.name), UVM_NONE)
                        pins.ifmode_val = pins.ifmode_val | mode_iter;
                    end      
                end      
            end      
            if ((mode_iter & pins.ifmode_val) != 0) begin
                names = (names == "") ? mode_iter.name : {names,",",mode_iter.name};
            end      
            if (mode_iter == mode_iter.last) break;
            mode_iter = mode_iter.next;
        end      
        `uvm_info("set_ifmode", $sformatf("current ifmode bits set: %s", names), UVM_LOW)
    endfunction

    static function ifmode_var_type get_ifmode();
        static virtual dfts_if pins;
        if (pins == null) begin
            pins = get_dfts_if();
        end      
        return pins.ifmode_val;
    endfunction

    static function void append_ifmode(ifmode_var_type mode);
        set_ifmode(get_ifmode() | mode);
    endfunction

    static function void clearbits_ifmode(ifmode_var_type mode);
        set_ifmode(get_ifmode() & ~mode);
    endfunction

    // -------------------------------------------------------------------------
    static function bit is_ifmode_set(ifmode_var_type mode);
        static virtual dfts_if pins;
        if (pins == null) begin
            pins = get_dfts_if();
        end      
        return ((pins.ifmode_val & mode) != 0);
    endfunction
`else
    // stub version to make functional model compile
    static function bit is_ifmode_set(ifmode_var_type mode);
        return 0;
    endfunction
`endif

    // -------------------------------------------------------------------------
    static function bit is_model_without_dut();
        `ifdef MODEL_WITHOUT_DUT
            return 1;
        `else
            return 0;
        `endif
    endfunction

    // -------------------------------------------------------------------------
    static function time parsesimtime(string str, string name);
        return parse_sim_time(.str(str), .name(name));
    endfunction

    static function time parse_sim_time(string str, string name);
        time parsed_sim_time = 0;
        time sim_time = 0;
        string time_unit = "";
        int tmp;
        time multiplier = 0;

        tmp = $sscanf(str, "%d%s*", sim_time, time_unit );
        if (sim_time > 0) begin
            // determine time unit, if supplied, then compute
            // the requested simulation time.
            case (time_unit)
                ""   : multiplier = 1;           // default is simulator resolution
                "ps" : multiplier = 1ps;
                "ns" : multiplier = 1ns;
                "us" : multiplier = 1us;
                "ms" : multiplier = 1ms;
                default : multiplier = 0;  // this is an error case
            endcase
        end      

        if (multiplier == 0) begin
            `uvm_fatal("parse_sim_time", $sformatf("unknown sim time unit %s specified in time unit %s for %s", time_unit, str, name))
        end       else begin
            parsed_sim_time = sim_time * multiplier;
            `uvm_info("parse_sim_time", $sformatf("%s=%0t", name, parsed_sim_time), UVM_LOW)
        end      
        return parsed_sim_time;
    endfunction


    // -------------------------------------------------------------------------
    static function time get_time_overrides(input time delay, string plusarg);
        string str;
        uvm_top.get_config_int(plusarg, delay);
        if ($value$plusargs({plusarg,"=%s"}, str)) begin
            delay = parse_sim_time(str, plusarg);
        end      
        return delay;
    endfunction

    // -------------------------------------------------------------------------
    static task automatic wait_for_delay(input time delay, string plusarg);
        delay = get_time_overrides(.delay(delay), .plusarg(plusarg));
        $display("%t FC::wait_for_delay -- %s delay is %t", $time, plusarg, delay);
        #(delay);
    endtask

    // -------------------------------------------------------------------------
    static task automatic align_time_to_increment(input time increment);
        time delay, quotient;
        quotient = $time / increment;
        delay = increment + (quotient * increment) - $time;
        $display("FC::align_time_to_increment -- delaying for %t to align to %t", delay, increment);
        #(delay);
    endtask

    // -------------------------------------------------------------------------
    static function time get_int_overrides(input int value, string plusarg, string parg_type = "=%d", bit debug_msg = 0);
        uvm_top.get_config_int(plusarg, value);
        $value$plusargs({plusarg,parg_type}, value);
        if(debug_msg) begin
            $display("FC::get_int_overrides %s = 'h%0x", plusarg, value);
        end      
        return value;
    endfunction

    // -------------------------------------------------------------------------
    static function longint systime(input string prefix = "", bit display = 1);
        static longint lasttime = 0;
        static longint zerotime = 0;
        longint difftime;
        longint totaltime;
        integer FP;
        longint now_in_seconds;
        reg [80:1] str;
        int ret;

`ifdef JTAG_ONLY
        str = sigaccess_wrapper::system_exec("date +%s");
        ret = $sscanf(str,"%d",systime);
`else
        ret = $systemf("date +%s > systime.txt");
        FP = $fopen("systime.txt","r");
        ret = $fgets(str,FP);
        ret = $sscanf(str,"%d",systime);
        $fclose(FP);
        ret = $systemf("rm systime.txt");
`endif

        if (display) begin
            difftime = systime - lasttime;
            totaltime = systime - zerotime;
            if (lasttime == 0) begin
                zerotime = systime;
                $display("(%0t) '%s' systime: %0d", $time, prefix, systime);
            end       else begin
                $display("(%0t) '%s' systime: %0d  lasttime: %0d  diff: %0d  totaltime: %0d", $time, prefix, systime, lasttime, difftime, totaltime);
            end      
        end      
        lasttime = systime;
    endfunction

    // ------------------------------------------------------------------------
    static function time freq_to_period(int freq);
        case (freq)
             24: return 41667ps;
             25: return 40000ps;
             62: return 16000ps;
            100: return 10000ps;
            125: return  8000ps;
            133: return  7500ps;
            166: return  6000ps;
            200: return  5000ps;
            250: return  4000ps;
            333: return  3000ps;
            400: return  2500ps;
            500: return  2000ps;
            533: return  1875ps;
            666: return  1500ps;
            800: return  1250ps;
           1000: return  1000ps;
           1250: return   800ps;
           1600: return   625ps;
           2500: return   400ps;
           2800: return   357ps;
           3700: return   268ps;
           5000: return   200ps;
           8000: return   125ps;
        endcase
        `uvm_error("freq_to_period", $sformatf("unknown freq=%0d", freq));
        return 10ns;
    endfunction // freq_to_period

    // -------------------------------------------------------------------------
    static function bit is_global_mode_set(string name);
        int value = 0;

`ifdef JTAG_ONLY
        // interface_mode code
        static virtual dfts_if pins;
        static interface_mode_type mode_num[string];
        if (pins == null) begin
            pins = get_dfts_if();
        end      
        if(mode_num.size() == 0) begin
            interface_mode_type mode;
            mode = mode.first;
            forever begin
                mode_num[mode.name] = mode;
                //$display("is_global_mode_set decode for %s = 'h%0x", mode.name, mode_num[mode.name]);
                if (mode == mode.last) break;
                mode = mode.next;
            end      
        end      
        if(mode_num.exists(name) && (mode_num[name] & pins.ifmode_val)) begin
            value = 1;
        end      
`endif
        uvm_top.get_config_int(name, value);
        if($test$plusargs(name)) begin
            value = 1;
            $value$plusargs({name,"=%b"}, value);
        end      
        return (value != 0) ? 1 : 0;
    endfunction
`endif
endclass
/*
 *  ----------------
 *    end of file
 *  ----------------
*/
// <<< VIM SETTINGS
// vim: ts=4 et
// >>>
