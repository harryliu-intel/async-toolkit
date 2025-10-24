// -----------------------------------------------------------------------------
// INTEL CONFIDENTIAL
// Copyright(c) 2018, Intel Corporation. All Rights Reserved
// -----------------------------------------------------------------------------
//
// Created By   :  Raghu P Gudla
// Created On   :  09/22/2018
// Description  :  Full-chip report server for dumping stack trace on errors 
// -----------------------------------------------------------------------------


class fc_report_server extends uvm_default_report_server;
    static int num_stack_dumps = 10000;
    static int num_errors_observed = 0;
    static int suppress_count_message = 0;
    int temp_val;

    // -----------------------------------------------------------------------
    virtual function string compose_message(uvm_severity severity, string name, string id, string message, string filename, int line);
        uvm_severity sv = uvm_severity'(severity);
        string msg_head      = $sformatf("%-11s @%11t: ", sv.name(), $time);
        string msg_name      = (name != "") ?  $sformatf("%-s ", name): "";
        string msg_file      = (filename != "" && !$test$plusargs("UVM_REPORT_DISABLE_FILE_LINE")) ? $sformatf("<%-s(%0d)> ", filename, line) : "";
        string msg_body      = $sformatf("[%-s] %-s", id, message);

`ifdef VCS
        if (severity == UVM_FATAL) begin
            $display("stack dump for %s %s", name, id);
            $stack;
        end      

        if (severity == UVM_ERROR) begin

            uvm_top.get_config_int("num_stack_dumps", temp_val);
            uvm_top.get_config_int("suppress_count_message", suppress_count_message);

            if ($value$plusargs("STACKDUMP_NUM=%0d", temp_val));

            num_stack_dumps = temp_val;
            if(!suppress_count_message) begin
                $display("stck dump value %d, %d, %d", num_stack_dumps, num_errors_observed, temp_val);    
            end      
            if (num_errors_observed < num_stack_dumps) begin
                $display("stack dump for %s %s:", name, id);
                $stack;
            end      
            num_errors_observed++;
        end      
`endif

        return {msg_head, msg_file, name, msg_body};
    endfunction

endclass

