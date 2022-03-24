##
## GTR Generator Tools Release v1.0.0
##
##------------------------------------------------------------------------------
##
## INTEL CONFIDENTIAL
##
## Copyright 2021 - 2022 Intel Corporation All Rights Reserved.
##
## The source code contained or described herein and all documents related
## to the source code ("Material") are owned by Intel Corporation or its
## suppliers or licensors. Title to the Material remains with Intel
## Corporation or its suppliers and licensors. The Material contains trade
## secrets and proprietary and confidential information of Intel or its
## suppliers and licensors. The Material is protected by worldwide copyright
## and trade secret laws and treaty provisions. No part of the Material may
## be used, copied, reproduced, modified, published, uploaded, posted,
## transmitted, distributed, or disclosed in any way without Intel's prior
## express written permission.
##
## No license under any patent, copyright, trade secret or other intellectual
## property right is granted to or conferred upon you by disclosure or
## delivery of the Materials, either expressly, by implication, inducement,
## estoppel or otherwise. Any license under such intellectual property rights
## must be express and approved by Intel in writing.
##
##------------------------------------------------------------------------------

proc gtr_lamb_gen_behav_sv { args } {
    set proc_name [lindex [regsub "::" [info level 0]  "" ] 0 ]
    set date [date ]
    parse_proc_arguments -args $args arg
    if { [info exists arg(-verbose) ] } {
        set verbose 1
    } else {
        set verbose 0
    }
    set dir verilog
    file mkdir $dir
    set fname ${dir}/$arg(-block_name).sv
    echo "INFO: $proc_name, Generating SV view: $fname"

    if { [info exists arg(-filelistVar) ] } {
        upvar $arg(-filelistVar) fileList
        set thisEntry [dict create]
        dict set thisEntry path $fname
        dict set thisEntry nda_protection_level front_end 
        dict set thisEntry pg_included false 
        dict set thisEntry standard_name verilog 
        dict set thisEntry type verilog_default_filelist 
        lappend fileList $thisEntry
    } 
    set depth $arg(-data_depth)
    set width $arg(-data_width)
    
    set of [open $fname w ]
    
    puts $of "//------------------------------------------------------------------------------"
    puts $of "//"
    puts $of "// INTEL CONFIDENTIAL"
    puts $of "//"
    puts $of "// Copyright 2021 - 2021 Intel Corporation All Rights Reserved."
    puts $of "//"
    puts $of "// The source code contained or described herein and all documents related"
    puts $of "// to the source code (\"Material\") are owned by Intel Corporation or its"
    puts $of "// suppliers or licensors. Title to the Material remains with Intel"
    puts $of "// Corporation or its suppliers and licensors. The Material contains trade"
    puts $of "// secrets and proprietary and confidential information of Intel or its"
    puts $of "// suppliers and licensors. The Material is protected by worldwide copyright"
    puts $of "// and trade secret laws and treaty provisions. No part of the Material may"
    puts $of "// be used, copied, reproduced, modified, published, uploaded, posted,"
    puts $of "// transmitted, distributed, or disclosed in any way without Intel's prior"
    puts $of "// express written permission."
    puts $of "//"
    puts $of "// No license under any patent, copyright, trade secret or other intellectual"
    puts $of "// property right is granted to or conferred upon you by disclosure or"
    puts $of "// delivery of the Materials, either expressly, by implication, inducement,"
    puts $of "// estoppel or otherwise. Any license under such intellectual property rights"
    puts $of "// must be express and approved by Intel in writing."
    puts $of "//"
    puts $of "//------------------------------------------------------------------------------\n"
    puts $of "`resetall"
    puts $of "`default_nettype none\n"
    puts $of "`ifndef INTEL_DC\n"
    puts $of "module $arg(-block_name)"
    puts $of "    #("
    puts $of "        parameter DEPTH=$depth,"
    puts $of "        parameter DWIDTH=$width,"
    puts $of "        parameter AWIDTH=\$clog2(DEPTH),"
    puts $of "        parameter X_ON_NOT_REN=1"
    puts $of "    )"
    puts $of "("
    puts $of "    input  logic clk,"
    puts $of "    input  logic wen,"
    puts $of "    input  logic ren,"
    puts $of "    input  logic \[AWIDTH-1:0\] radr,"
    puts $of "    input  logic \[AWIDTH-1:0\] wadr,"
    puts $of "    input  logic \[DWIDTH-1:0\] wdata,"
    puts $of "    output logic \[DWIDTH-1:0\] dout,\n"
    puts $of "    input  logic test__scan_en,"
    puts $of "    input  logic [1:0] dft__core_si,"
    puts $of "    input  logic icg_force_on,"
    puts $of "    input  logic dft_read_bypass,"
    puts $of "    input  logic dft__mem_wr_disable,"
    puts $of "    output logic [1:0] dft__core_so"
    puts $of ");"
    puts $of "    logic \[DEPTH-1:0\]\[DWIDTH-1:0\] mem_array;"
    puts $of "    logic                         do_x;\n"
    puts $of "    // write"
    puts $of "    always_ff @(posedge clk) begin"
    puts $of "        if (wen)"
    puts $of "            mem_array\[wadr\] <= wdata;"
    puts $of "    end\n"
    puts $of "	  logic \[AWIDTH-1:0\] radr_int;"
    puts $of "    always_ff @(posedge clk) begin"
    puts $of "        if (ren) begin"
    puts $of "            radr_int <= radr;"
    puts $of "            do_x <= '0;"
    puts $of "    end"
    puts $of "    else"
    puts $of "        do_x <= X_ON_NOT_REN;"
    puts $of "    end"
    puts $of "    assign dout = do_x ? {DWIDTH{1'bx}} : mem_array\[radr_int\];\n"
    puts $of "endmodule\n"
    puts $of "`endif\n"
    puts $of "`default_nettype wire"
    close $of
}

define_proc_attributes gtr_lamb_gen_behav_sv \
    -info "Utility to generate Lamb Behavioural LAMB System Verilog Module" \
    -define_args {
      {-block_name "Specify memory block name" "String" string required}
      {-data_depth "Depth of the memory in words/entries(layout x direction)" "int" int required}
      {-data_width "Data bus width of the memory in bits(layout y direction)" "int" int required}
      {-dual_clocks "Specify if memory has dual async clocks" "" boolean optional}
      {-verbose "Verbose Reporting" "" boolean optional}
      {-filelistVar "Update filelist for manifest.xml" "" string optional}
}
