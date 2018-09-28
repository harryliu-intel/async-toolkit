`ifndef SCOREBOARD_SV
`define SCOREBOARD_SV

`include "id_generator.sv"
`include "configuration.sv"
`include "request_record.sv"

class scoreboard;

    id_generator        id_gen;
    configuration       cfg;

    string              name;
    request_record      req_in_record;
    request_record      req_out_record;
    request_record      req_in_fifo[tmpl_pkg::NUM_INPUTS-1:0][$];
    request_record      req_out_fifo[tmpl_pkg::NUM_INPUTS-1:0][$];


    function new(configuration cfg, id_generator id_gen);

        this.cfg                = cfg;
        this.id_gen             = id_gen;

        name                    = "scoreboard.sv";

        foreach (req_in_fifo[iport])
            req_in_fifo[iport] = {};

    endfunction

    task reset();
    endtask

    function bit all_empty();
        all_empty = 1'b1; 
        foreach (req_in_fifo[i])
            if (req_in_fifo[i].size() != 0)
                all_empty = 1'b0;
        return all_empty; 
    endfunction

    function req_in_notification(tmpl_pkg::enc_inp_t iport, tmpl_pkg::req_in_t req_in);

        req_in_record               = new(); 
        req_in_record.id            = id_gen.get_id(); 
        req_in_record.req_in_time   = $time;
        req_in_record.req_in        = req_in;

        $display("(time: %0d) %s: INCOMING REQUEST: id = %0d, inp%0d => outp%0d, data = %x", $time, name, req_in_record.id, iport, req_in_record.req_in.outp, req_in_record.req_in.data);

        // add incoming request to corresponding scoreboard FIFO
        req_in_fifo[iport].push_back(req_in_record);

    endfunction

    function arb_bid_notification(tmpl_pkg::enc_outp_t oport, tmpl_pkg::inp_t bids, tmpl_pkg::inp_t gnt);
`ifdef ARB_DEBUG
        $display("(time: %0d) ARB_DEBUG:  oport = %0d, bids = 0x%0x, gnt = 0x%0x", $time, oport, bids, gnt);
`endif
        foreach (bids[i]) begin
            if (bids[i]) begin
                req_in_fifo[i][0].num_bids++;
            end
            if (gnt[i]) begin
                if (req_in_fifo[i].size() == 0) begin
                    $display("ERROR:  (time: %0d)  REQ_IN_FIFO UNEXPECTEDLY EMPTY  (iport = %0d)", $time, i);
                    $assertoff;
                    $finish;
                end
                if (req_in_fifo[i][0].req_in.outp != oport) begin
                    $display("ERROR:  (time: %0d)  ROUTING MISMATCH (oport = %0d):  request routed to different output than specified", $time, oport);
                    req_in_fifo[i][0].print("ERROR:  ");
                    $assertoff;
                    $finish;
                end
                req_in_fifo[i][0].arb_win_time = $time;
                req_out_fifo[oport].push_back(req_in_fifo[i].pop_front());
            end
            else if (bids[i])
                req_in_fifo[i][0].losers[i] = 1'b1; 
        end // foreach bids[i]
    endfunction

    function req_out_notification(tmpl_pkg::enc_outp_t oport, tmpl_pkg::req_out_t req_out);
       
        req_out_record              = req_out_fifo[oport].pop_front();
        req_out_record.req_out_time = $time; 
        req_out_record.req_out      = req_out; 

        $display("(time: %0d) %s: OUTGOING REQUEST: id = %0d, inp%0d => outp%0d, data = %x", $time, name, req_out_record.id, req_out_record.req_out.inp, oport, req_out_record.req_out.data);

        // check that incoming and outgoing records match
        if (req_out_record.req_in.data != req_out_record.req_out.data) begin
            $display("ERROR:  (time: %0d)  REQUEST MISMATCH (oport = %0d):  outgoing data doesn't match incoming data", $time, oport);
            req_out_record.print("ERROR:  ");
            $assertoff;
            $finish;
        end

        if (req_out_record.req_in.outp != oport) begin
            $display("ERROR:  (time: %0d)  REQUEST MISMATCH (oport = %0d):  request going out different port than specified", $time, oport);
            req_out_record.print("ERROR:  ");
            $assertoff;
            $finish;
        end

        // print request record for debugging purposes
`ifdef REQ_RECORD_DEBUG
            req_out_record.print("REQ_RECORD_DEBUG:  ");
`endif

    endfunction

endclass

`endif // SCOREBOARD_SV
