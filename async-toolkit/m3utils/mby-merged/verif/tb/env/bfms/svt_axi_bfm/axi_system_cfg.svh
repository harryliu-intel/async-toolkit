// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  AXI System Configuration Compoenent
// -----------------------------------------------------------------------------

class axi_system_cfg extends svt_axi_system_configuration;

    int data_width = 256;
    //Utility macro
    `uvm_object_utils (axi_system_cfg)

    function new (string str="axi_system_cfg");
        super.new(str);

        // Assign the necessary configuration parameters. This example uses single
        //master and single slave configuration.
        //Setup to have a single master by default.
        this.num_masters = 1;
        this.num_slaves  = 1;
        this.system_monitor_enable = 1;

        // Create port configurations
        this.create_sub_cfgs(num_masters,num_slaves);

        for (int idx = 1 ; idx <= num_masters ; idx ++ ) begin
            this.master_cfg[idx-1].axi_interface_type = svt_axi_port_configuration::AXI4;
            // Enable transaction level coverage
            this.master_cfg[idx-1].transaction_coverage_enable = 1;
            this.master_cfg[idx-1].is_active = 1;

            // Enable protocol file generation for Protocol Analyzer
            this.master_cfg[idx-1].enable_xml_gen = 1;

            this.master_cfg[idx-1].pa_format_type = svt_xml_writer::FSDB;

            this.master_cfg[idx-1].transaction_coverage_enable = 1;

            this.master_cfg[idx-1].data_width = data_width;
            this.master_cfg[idx-1].id_width = 8;

            this.master_cfg[idx-1].reordering_algorithm = svt_axi_port_configuration::RANDOM;
            this.master_cfg[idx-1].write_resp_reordering_depth = `SVT_AXI_MAX_WRITE_RESP_REORDERING_DEPTH;

            this.set_addr_range(0,64'h0,64'hffff_ffff_ffff_ffff); //rgudla

            this.master_cfg[idx-1].enable_tracing = 1;
            this.master_cfg[idx-1].enable_reporting = 1;
            this.master_cfg[idx-1].data_trace_enable = 1;
        end

        for (int idx = 1 ; idx <= num_slaves ; idx ++ ) begin
            this.slave_cfg[idx-1].axi_interface_type = svt_axi_port_configuration::AXI4;
            // Enable transaction level coverage
            this.slave_cfg[idx-1].transaction_coverage_enable = 1;
            this.slave_cfg[idx-1].is_active = 1;

            // Enable protocol file generation for Protocol Analyzer
            this.slave_cfg[idx-1].enable_xml_gen = 1;

            this.slave_cfg[idx-1].pa_format_type= svt_xml_writer::FSDB;

            this.slave_cfg[idx-1].transaction_coverage_enable = 1;

            this.slave_cfg[idx-1].data_width = data_width;
            this.slave_cfg[idx-1].id_width = 8;

            this.slave_cfg[idx-1].reordering_algorithm = svt_axi_port_configuration::RANDOM;
            this.slave_cfg[idx-1].write_resp_reordering_depth = `SVT_AXI_MAX_WRITE_RESP_REORDERING_DEPTH;
        //this.set_addr_range(0,64'h0,64'hffff_ffff_ffff_ffff);
            this.slave_cfg[idx-1].enable_tracing = 1;
            this.slave_cfg[idx-1].enable_reporting = 1;
            this.slave_cfg[idx-1].data_trace_enable = 1;
        end
    /*
     // Set performance constraints
     // All intervals, latencies are in terms of timescale unit
     this.master_cfg[0].perf_recording_interval = -1;
     this.master_cfg[0].perf_avg_max_read_xact_latency= 100000;
     `ifdef AXI_PERF_API_USAGE //using this macro will result in constraint failure errors. This is to demonstrate the usage of the APIs of class svt_axi_port_perf_status
     this.master_cfg[0].perf_max_read_xact_latency= 100;
     `else
     this.master_cfg[0].perf_max_read_xact_latency= 100000;
     `endif
     this.master_cfg[0].perf_max_write_throughput = 20;
     this.master_cfg[0].perf_min_write_throughput = 0.0001;
     // Unit is bytes/timescale
     this.master_cfg[0].perf_max_read_throughput = 20;
     this.master_cfg[0].perf_min_read_throughput = 0.0001;
     this.display_perf_summary_report = 1;
     this.master_cfg[0].perf_avg_max_write_xact_latency = 100000;
     this.master_cfg[0].perf_avg_min_read_xact_latency = 0.000001;
     this.master_cfg[0].perf_avg_min_write_xact_latency = 0.000001;
     this.master_cfg[0].perf_exclude_inactive_periods_for_throughput = 1;
     this.master_cfg[0].perf_inactivity_algorithm_type = svt_axi_port_configuration::EXCLUDE_BEGIN_END;
     `ifdef AXI_PERF_API_USAGE
     this.master_cfg[0].perf_max_write_xact_latency = 100;
     this.master_cfg[0].perf_min_read_xact_latency = 100000;
     this.master_cfg[0].perf_min_write_xact_latency = 100000;
     `else
     this.master_cfg[0].perf_max_write_xact_latency = 10000000;
     this.master_cfg[0].perf_min_read_xact_latency = 10;
     this.master_cfg[0].perf_min_write_xact_latency = 10;
     `endif
     */
    endfunction

endclass
