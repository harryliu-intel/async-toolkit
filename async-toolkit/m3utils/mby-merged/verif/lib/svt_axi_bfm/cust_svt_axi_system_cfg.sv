// vim: noai : ts=3 : sw=3 : expandtab : ft=systemverilog

//------------------------------------------------------------------------------
//
// INTEL CONFIDENTIAL
//
// Copyright 2018 Intel Corporation All Rights Reserved.
//
// The source code contained or described herein and all documents related to
// the source code ("Material") are owned by Intel Corporation or its suppliers
// or licensors. Title to the Material remains with Intel Corporation or its
// suppliers and licensors. The Material contains trade secrets and proprietary
// and confidential information of Intel or its suppliers and licensors.  The
// Material is protected by worldwide copyright and trade secret laws and
// treaty provisions. No part of the Material may be used, copied, reproduced,
// modified, published, uploaded, posted, transmitted, distributed, or
// disclosed in any way without Intel's prior express written permission.
//
// No license under any patent, copyright, trade secret or other intellectual
// property right is granted to or conferred upon you by disclosure or delivery
// of the Materials, either expressly, by implication, inducement, estoppel or
// otherwise. Any license under such intellectual property rights must be
// express and approved by Intel in writing.
//
//=======================================================================
// COPYRIGHT (C)  2012 SYNOPSYS INC.
// This software and the associated documentation are confidential and
// proprietary to Synopsys, Inc. Your use or disclosure of this software
// is subject to the terms and conditions of a written license agreement
// between you, or your company, and Synopsys, Inc. In the event of
// publications, the following notice is applicable:
//
// ALL RIGHTS RESERVED
//
// The entire notice above must be reproduced on all authorized copies.
//
//------------------------------------------------------------------------------
//   Author        : Akshay Kotian
//   Project       : Madison Bay
//------------------------------------------------------------------------------

//Class:    cust_svt_axi_system_configuration
//
//
//Class cust_svt_axi_system_configuration is basically used to encapsulate all
//the configuration information.  It extends system configuration and set the
//appropriate fields like number of master/slaves, create master/slave
//configurations etc..., which are required by System Env.
//

`ifndef __CUST_SVT_AXI_SYSTEM_CONFIGURATION_GUARD
`define __CUST_SVT_AXI_SYSTEM_CONFIGURATION_GUARD


`ifndef __INSIDE_SVT_AXI_BFM_PKG__
`error "File is meant to be used only through the svt_axi_bfm_pkg.  Do not include it individually."
`endif


class cust_svt_axi_system_configuration extends svt_axi_system_configuration;

    int data_width = 256;
    //Utility macro
    `uvm_object_utils (cust_svt_axi_system_configuration)

    function new (string str="cust_svt_axi_system_configuration");
        super.new(str);

        // Assign the necessary configuration parameters. This example uses single
        //master and single slave configuration.
        //Setup to have a single master by default.
        this.num_masters = 1;
        this.num_slaves  = 0;

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
`endif // __CUST_SVT_AXI_SYSTEM_CONFIGURATION_GUARD
