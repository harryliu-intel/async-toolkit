`ifndef SCOREBOARD_SV
`define SCOREBOARD_SV

`include "id_generator.sv"
//-hz:
//`include "configuration.sv"
//`include "request_record.sv"

class scoreboard;

//  id_generator        id_gen;
//-hz:
//  configuration       cfg;

    string              name;
//  request_record      req_in_record;
//  request_record      req_out_record;
//-hz:
//  request_record      req_in_fifo[tmpl_pkg::NUM_INPUTS-1:0][$];
//  request_record      req_out_fifo[tmpl_pkg::NUM_INPUTS-1:0][$];



logic[531:0]	wr_req_in_fifo[$];	// wdata
logic[531:0]	data_fromQ;


//-hz:
//  function new(configuration cfg, id_generator id_gen);
    function new( );

//      this.cfg                = cfg;
//      this.id_gen             = id_gen;

        name                    = "scoreboard.sv";

	wr_req_in_fifo = {};
	data_fromQ = '0;

    endfunction


    task reset();
    endtask

    function bit all_empty();
        all_empty = 1'b1; 
//      foreach (req_in_fifo[i])
//          if (req_in_fifo[i].size() != 0)
//              all_empty = 1'b0;
        return all_empty; 
    endfunction




// -hz: 12/7/2018: check mem_bank0 wr:
    function mem_bank0_wr_notification(logic[11:0] adr, logic[531:0] wdata);
//    $display("(time: %0d) %s: WRITE at MEM BANK 0: adr= 'h%0h, wdata= 'h%0h", $time, name, adr, wdata);
      if ((adr != 'h33) || (wdata != 'ha5a5)) begin
   // if ((adr != 'h30) || (wdata != 'ha5a5)) begin  // <- make an wrong expectation on purpose on adr ('h30)
   // if ((adr != 'h33) || (wdata != 'h55a5)) begin  // <- make an wrong expectation on purpose on data ('h55a5)
      	$display("ERROR: (time: %0d) %s: WRITE at MEM BANK 0 IS NOT CORRECT (adr='h%0h), (wdata='h%0h)", 
                         $time, name, adr, wdata);
      	$assertoff;
      	$finish;
      end
    endfunction


// -hz: 12/10/2018: check mem_bank0 rd req:
    function mem_bank0_rd_req_notification(logic[11:0] adr);
      if (adr != 'h33) begin
   // if (adr != 'h20) begin  // <- make an wrong expectation on purpose on adr ('h20)
      	$display("ERROR: (time: %0d) %s: READ REQ at MEM BANK 0 IS NOT CORRECT (adr='h%0h)", 
                         $time, name, adr);
      	$assertoff;
      	$finish;
      end
    endfunction


// -hz: 12/10/2018: check mem_bank0 rd data:
    function mem_bank0_rd_data_notification(logic[531:0] rdata);
      if (rdata != 'ha5a5) begin
   // if (rdata != 'h5555) begin  // <- make an wrong expectation on purpose on data ('h5555)
      	$display("ERROR: (time: %0d) %s: READ DATA at MEM BANK 0 IS NOT CORRECT (wdata='h%0h)", 
                         $time, name, rdata);
      	$assertoff;
      	$finish;
      end
    endfunction



// new 1: just for rd rsp
    function wb_p0_rd_rsp_out_notification(mby_msh_pkg::msh_row_rd_rsp_t rd_rsp_out);
//    $display("(time: %0d) %s: OUTGOING WB P0 rd RESPONSE:  id %0d", $time, name, rd_rsp_out.id);
      if (rd_rsp_out.id != 1) begin
      	$display("ERROR:  (time: %0d)  RESPONSE ID IS NOT CORRECT (id = %0d)", $time, rd_rsp_out.id);
      	$assertoff;
      	$finish;
      end
    endfunction

// new 2: just for rd data
    function wb_p0_rd_data_out_notification(mby_msh_pkg::msh_data_t rd_data_out);
//    $display("(time: %0d) %s: OUTGOING WB P0 rd DATA: %0d", $time, name, rd_data_out);
      if (rd_data_out != 'ha5a5) begin
   // if (rd_data_out != 'ha5) begin  // <- make an error on purpose
      	$display("ERROR detected: (time: %0d)", $time);
      	$display("ERROR: expected rd data ('ha5a5),  detected rd data ('h%0h)", rd_data_out);
      	$assertoff;
      	$finish;
      end
    endfunction


//-hz: 12/11/2018: push wr to fifo:

//  function mem_bank0_req_in_notification(logic[11:0] adr, logic[531:0] wdata);
//      wr_req_in_fifo.push_back(wdata);
//      $display("(time: %0d) %s: Push Wdata to Q : adr=%0h, wdata=%0h", $time, name, adr, wdata);
//  endfunction

    function eb_p0_wr_req_in_notification(logic[531:0] wdata);
        wr_req_in_fifo.push_back(wdata);
        $display("(time: %0d) %s: Push Wdata to Q : wdata=%0h", $time, name, wdata);
    endfunction



    // 12/11/2018: check rd data against wr fifo:

    function wb_p0_check_rd_data_notification(logic[531:0] rdata);
	data_fromQ = wr_req_in_fifo.pop_front();
        $display("(time: %0d) %s: Pop data from Q : data=%0h", $time, name, data_fromQ);

        if (rdata != data_fromQ) begin
      	    $display("ERROR:  (time: %0d)  RD DATA IS NOT CORRECT (rdata=%0h) (wdata=%0h)", $time, rdata, data_fromQ);
      	    $assertoff;
      	    $finish;
        end

        $display("(time: %0d) %s: --------------------------------------------", $time, name);
    endfunction


//-hz:
//  function arb_bid_notification(tmpl_pkg::enc_outp_t oport, tmpl_pkg::inp_t bids, tmpl_pkg::inp_t gnt);
/*
    function arb_bid_notification( );

`ifdef ARB_DEBUG
        $display("(time: %0d) ARB_DEBUG:  oport = %0d, bids = 0x%0x, gnt = 0x%0x", $time, oport, bids, gnt);
`endif
        foreach (bids[i]) begin
            if (bids[i]) begin
//              req_in_fifo[i][0].num_bids++;
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
*/

//-hz:
//  function req_out_notification(tmpl_pkg::enc_outp_t oport, tmpl_pkg::req_out_t req_out);
    function req_out_notification( );
       
//      req_out_record              = req_out_fifo[oport].pop_front();
//      req_out_record.req_out_time = $time; 
//      req_out_record.req_out      = req_out; 

//      $display("(time: %0d) %s: OUTGOING REQUEST: id = %0d, inp%0d => outp%0d, data = %x", $time, name, req_out_record.id, req_out_record.req_out.inp, oport, req_out_record.req_out.data);

        // check that incoming and outgoing records match
/*
        if (req_out_record.req_in.data != req_out_record.req_out.data) begin
            $display("ERROR:  (time: %0d)  REQUEST MISMATCH (oport = %0d):  outgoing data doesn't match incoming data", $time, oport);
            req_out_record.print("ERROR:  ");
            $assertoff;
            $finish;
        end
*/

/*
        if (req_out_record.req_in.outp != oport) begin
            $display("ERROR:  (time: %0d)  REQUEST MISMATCH (oport = %0d):  request going out different port than specified", $time, oport);
            req_out_record.print("ERROR:  ");
            $assertoff;
            $finish;
        end
*/

        // print request record for debugging purposes
`ifdef REQ_RECORD_DEBUG
//          req_out_record.print("REQ_RECORD_DEBUG:  ");
`endif

    endfunction

endclass

`endif // SCOREBOARD_SV
