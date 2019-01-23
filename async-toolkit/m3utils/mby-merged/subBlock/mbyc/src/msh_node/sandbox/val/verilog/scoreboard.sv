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



mby_msh_pkg::msh_data_t		wr_req_in_fifo[$];	// wdata
mby_msh_pkg::msh_data_t		data_fromQ;

mby_msh_pkg::msh_row_wr_req_t	exp_o_p0_eb_wreq_fifo[$];
mby_msh_pkg::msh_row_wr_req_t	exp_o_p0_wb_wreq_fifo[$];
mby_msh_pkg::msh_col_wr_req_t	exp_o_p0_nb_wreq_fifo[$];
mby_msh_pkg::msh_col_wr_req_t	exp_o_p0_sb_wreq_fifo[$];

mby_msh_pkg::msh_row_wr_req_t	exp_o_p1_eb_wreq_fifo[$];
mby_msh_pkg::msh_row_wr_req_t	exp_o_p1_wb_wreq_fifo[$];
mby_msh_pkg::msh_col_wr_req_t	exp_o_p1_nb_wreq_fifo[$];
mby_msh_pkg::msh_col_wr_req_t	exp_o_p1_sb_wreq_fifo[$];

mby_msh_pkg::msh_data_t    exp_o_p0_eb_wdata_fifo[$]; 
mby_msh_pkg::msh_data_t    exp_o_p0_wb_wdata_fifo[$]; 
mby_msh_pkg::msh_data_t    exp_o_p0_nb_wdata_fifo[$]; 
mby_msh_pkg::msh_data_t    exp_o_p0_sb_wdata_fifo[$];
mby_msh_pkg::msh_data_t    exp_o_p1_eb_wdata_fifo[$];
mby_msh_pkg::msh_data_t    exp_o_p1_wb_wdata_fifo[$];
mby_msh_pkg::msh_data_t    exp_o_p1_nb_wdata_fifo[$];
mby_msh_pkg::msh_data_t    exp_o_p1_sb_wdata_fifo[$]; // wdata


logic[13:0] 	exp_mem_wreq_fifo[$];    // addr
logic[531:0]    exp_mem_wdata_fifo[$];   // wdata

mby_msh_pkg::msh_row_wr_req_t    exp_o_p0_eb_wreq_Q;
mby_msh_pkg::msh_row_wr_req_t    exp_o_p0_wb_wreq_Q;
mby_msh_pkg::msh_col_wr_req_t    exp_o_p0_nb_wreq_Q;
mby_msh_pkg::msh_col_wr_req_t    exp_o_p0_sb_wreq_Q;

mby_msh_pkg::msh_row_wr_req_t    exp_o_p1_eb_wreq_Q;
mby_msh_pkg::msh_row_wr_req_t    exp_o_p1_wb_wreq_Q;
mby_msh_pkg::msh_col_wr_req_t    exp_o_p1_nb_wreq_Q;
mby_msh_pkg::msh_col_wr_req_t    exp_o_p1_sb_wreq_Q;


mby_msh_pkg::msh_data_t    exp_o_p0_eb_wdata_Q;
mby_msh_pkg::msh_data_t    exp_o_p0_wb_wdata_Q;
mby_msh_pkg::msh_data_t    exp_o_p0_nb_wdata_Q;
mby_msh_pkg::msh_data_t    exp_o_p0_sb_wdata_Q;
mby_msh_pkg::msh_data_t    exp_o_p1_eb_wdata_Q;
mby_msh_pkg::msh_data_t    exp_o_p1_wb_wdata_Q;
mby_msh_pkg::msh_data_t    exp_o_p1_nb_wdata_Q;
mby_msh_pkg::msh_data_t    exp_o_p1_sb_wdata_Q;

logic[13:0]    	exp_mem_wreq_Q;	// 14 bits from req. each bank has 12 bits
logic[531:0]    exp_mem_wdata_Q;


mby_msh_pkg::msh_row_rd_req_t   exp_o_p0_eb_rreq_fifo[$]; 
mby_msh_pkg::msh_row_rd_req_t   exp_o_p0_wb_rreq_fifo[$];
mby_msh_pkg::msh_col_rd_req_t   exp_o_p0_nb_rreq_fifo[$];
mby_msh_pkg::msh_col_rd_req_t   exp_o_p0_sb_rreq_fifo[$];
mby_msh_pkg::msh_row_rd_req_t   exp_o_p1_eb_rreq_fifo[$]; 
mby_msh_pkg::msh_row_rd_req_t   exp_o_p1_wb_rreq_fifo[$]; 
mby_msh_pkg::msh_col_rd_req_t   exp_o_p1_nb_rreq_fifo[$];
mby_msh_pkg::msh_col_rd_req_t   exp_o_p1_sb_rreq_fifo[$];

logic[13:0]    exp_mem_rreq_fifo[$];    // addr
logic[13:0]    exp_mem_rreq_Q;

mby_msh_pkg::msh_row_rd_req_t   exp_o_p0_eb_rreq_Q;
mby_msh_pkg::msh_row_rd_req_t   exp_o_p0_wb_rreq_Q;
mby_msh_pkg::msh_col_rd_req_t   exp_o_p0_nb_rreq_Q;
mby_msh_pkg::msh_col_rd_req_t   exp_o_p0_sb_rreq_Q;
mby_msh_pkg::msh_row_rd_req_t   exp_o_p1_eb_rreq_Q;
mby_msh_pkg::msh_row_rd_req_t   exp_o_p1_wb_rreq_Q;
mby_msh_pkg::msh_col_rd_req_t   exp_o_p1_nb_rreq_Q;
mby_msh_pkg::msh_col_rd_req_t   exp_o_p1_sb_rreq_Q;


mby_msh_pkg::msh_row_rd_rsp_t exp_o_p0_eb_rsp_fifo[$];
mby_msh_pkg::msh_row_rd_rsp_t exp_o_p0_wb_rsp_fifo[$];
mby_msh_pkg::msh_col_rd_rsp_t exp_o_p0_nb_rsp_fifo[$];
mby_msh_pkg::msh_col_rd_rsp_t exp_o_p0_sb_rsp_fifo[$];

mby_msh_pkg::msh_row_rd_rsp_t exp_o_p1_eb_rsp_fifo[$];
mby_msh_pkg::msh_row_rd_rsp_t exp_o_p1_wb_rsp_fifo[$];
mby_msh_pkg::msh_col_rd_rsp_t exp_o_p1_nb_rsp_fifo[$];
mby_msh_pkg::msh_col_rd_rsp_t exp_o_p1_sb_rsp_fifo[$];


mby_msh_pkg::msh_row_rd_rsp_t	exp_o_p0_eb_rsp_Q;
mby_msh_pkg::msh_row_rd_rsp_t	exp_o_p0_wb_rsp_Q;
mby_msh_pkg::msh_col_rd_rsp_t	exp_o_p0_nb_rsp_Q;
mby_msh_pkg::msh_col_rd_rsp_t	exp_o_p0_sb_rsp_Q;

mby_msh_pkg::msh_row_rd_rsp_t	exp_o_p1_eb_rsp_Q;
mby_msh_pkg::msh_row_rd_rsp_t	exp_o_p1_wb_rsp_Q;
mby_msh_pkg::msh_col_rd_rsp_t	exp_o_p1_nb_rsp_Q;
mby_msh_pkg::msh_col_rd_rsp_t	exp_o_p1_sb_rsp_Q;


mby_msh_pkg::msh_data_t    exp_o_p0_eb_rdata_fifo[$]; // rdata
mby_msh_pkg::msh_data_t    exp_o_p0_wb_rdata_fifo[$]; // rdata
mby_msh_pkg::msh_data_t    exp_o_p0_nb_rdata_fifo[$]; // rdata
mby_msh_pkg::msh_data_t    exp_o_p0_sb_rdata_fifo[$]; // rdata

mby_msh_pkg::msh_data_t    exp_o_p1_eb_rdata_fifo[$]; // rdata
mby_msh_pkg::msh_data_t    exp_o_p1_wb_rdata_fifo[$]; // rdata
mby_msh_pkg::msh_data_t    exp_o_p1_nb_rdata_fifo[$]; // rdata
mby_msh_pkg::msh_data_t    exp_o_p1_sb_rdata_fifo[$]; // rdata

mby_msh_pkg::msh_data_t    exp_o_p0_eb_rdata_Q;
mby_msh_pkg::msh_data_t    exp_o_p0_wb_rdata_Q;
mby_msh_pkg::msh_data_t    exp_o_p0_nb_rdata_Q;
mby_msh_pkg::msh_data_t    exp_o_p0_sb_rdata_Q;

mby_msh_pkg::msh_data_t    exp_o_p1_eb_rdata_Q;
mby_msh_pkg::msh_data_t    exp_o_p1_wb_rdata_Q;
mby_msh_pkg::msh_data_t    exp_o_p1_nb_rdata_Q;
mby_msh_pkg::msh_data_t    exp_o_p1_sb_rdata_Q;



//-hz:
//  function new(configuration cfg, id_generator id_gen);
    function new( );

//      this.cfg                = cfg;
//      this.id_gen             = id_gen;

        name                    = "scoreboard.sv";

	wr_req_in_fifo = {};
	data_fromQ = '0;

        exp_o_p0_eb_wreq_fifo = {};
        exp_o_p0_wb_wreq_fifo = {};
        exp_o_p0_nb_wreq_fifo = {};
        exp_o_p0_sb_wreq_fifo = {};
        exp_o_p1_eb_wreq_fifo = {};
        exp_o_p1_wb_wreq_fifo = {};
        exp_o_p1_nb_wreq_fifo = {};
        exp_o_p1_sb_wreq_fifo = {};

        exp_o_p0_eb_wdata_fifo = {};
        exp_o_p0_wb_wdata_fifo = {};
        exp_o_p0_nb_wdata_fifo = {};
        exp_o_p0_sb_wdata_fifo = {};
        exp_o_p1_eb_wdata_fifo = {};
        exp_o_p1_wb_wdata_fifo = {};
        exp_o_p1_nb_wdata_fifo = {};
        exp_o_p1_sb_wdata_fifo = {};

        exp_mem_wreq_fifo = {};
        exp_mem_wdata_fifo = {};

        exp_o_p0_eb_wreq_Q    = '0;
        exp_o_p0_wb_wreq_Q    = '0;
        exp_o_p0_nb_wreq_Q    = '0;
        exp_o_p0_sb_wreq_Q    = '0;
        exp_o_p1_eb_wreq_Q    = '0;
        exp_o_p1_wb_wreq_Q    = '0;
        exp_o_p1_nb_wreq_Q    = '0;
        exp_o_p1_sb_wreq_Q    = '0;

        exp_o_p0_eb_wdata_Q   = '0;
        exp_o_p0_wb_wdata_Q   = '0;
        exp_o_p0_nb_wdata_Q   = '0;
        exp_o_p0_sb_wdata_Q   = '0;
        exp_o_p1_eb_wdata_Q   = '0;
        exp_o_p1_wb_wdata_Q   = '0;
        exp_o_p1_nb_wdata_Q   = '0;
        exp_o_p1_sb_wdata_Q   = '0;

        exp_mem_wreq_Q      = '0;
        exp_mem_wdata_Q     = '0;

        exp_o_p0_eb_rreq_fifo = {};
        exp_o_p0_wb_rreq_fifo = {};
        exp_o_p0_nb_rreq_fifo = {};
        exp_o_p0_sb_rreq_fifo = {};
        exp_o_p1_eb_rreq_fifo = {};
        exp_o_p1_wb_rreq_fifo = {};
        exp_o_p1_nb_rreq_fifo = {};
        exp_o_p1_sb_rreq_fifo = {};
        exp_mem_rreq_fifo = {};

        exp_mem_rreq_Q      = '0;
        exp_o_p0_eb_rreq_Q    = '0;
        exp_o_p0_wb_rreq_Q    = '0;
        exp_o_p0_nb_rreq_Q    = '0;
        exp_o_p0_sb_rreq_Q    = '0;
        exp_o_p1_eb_rreq_Q    = '0;
        exp_o_p1_wb_rreq_Q    = '0;
        exp_o_p1_nb_rreq_Q    = '0;
        exp_o_p1_sb_rreq_Q    = '0;


        exp_o_p0_eb_rsp_fifo = {};
        exp_o_p0_wb_rsp_fifo = {};
        exp_o_p0_nb_rsp_fifo = {};
        exp_o_p0_sb_rsp_fifo = {};

        exp_o_p1_eb_rsp_fifo = {};
        exp_o_p1_wb_rsp_fifo = {};
        exp_o_p1_nb_rsp_fifo = {};
        exp_o_p1_sb_rsp_fifo = {};

	exp_o_p0_eb_rsp_Q	= '0;
	exp_o_p0_wb_rsp_Q	= '0;
	exp_o_p0_nb_rsp_Q	= '0;
	exp_o_p0_sb_rsp_Q	= '0;

	exp_o_p1_eb_rsp_Q	= '0;
	exp_o_p1_wb_rsp_Q	= '0;
	exp_o_p1_nb_rsp_Q	= '0;
	exp_o_p1_sb_rsp_Q	= '0;


        exp_o_p0_eb_rdata_fifo = {};
        exp_o_p0_wb_rdata_fifo = {};
        exp_o_p0_nb_rdata_fifo = {};
        exp_o_p0_sb_rdata_fifo = {};

        exp_o_p1_eb_rdata_fifo = {};
        exp_o_p1_wb_rdata_fifo = {};
        exp_o_p1_nb_rdata_fifo = {};
        exp_o_p1_sb_rdata_fifo = {};

	exp_o_p0_eb_rdata_Q 	= '0;
	exp_o_p0_wb_rdata_Q 	= '0;
	exp_o_p0_nb_rdata_Q 	= '0;
	exp_o_p0_sb_rdata_Q 	= '0;

	exp_o_p1_eb_rdata_Q 	= '0;
	exp_o_p1_wb_rdata_Q 	= '0;
	exp_o_p1_nb_rdata_Q 	= '0;
	exp_o_p1_sb_rdata_Q 	= '0;


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



//-hz: 12/11/2018: push wr to fifo:

//  function mem_bank0_req_in_notification(logic[11:0] adr, logic[531:0] wdata);
//      wr_req_in_fifo.push_back(wdata);
//      $display("(time: %0d) %s: Push Wdata to Q : adr=%0h, wdata=%0h", $time, name, adr, wdata);
//  endfunction
//
//  function eb_p0_wr_req_in_notification(logic[531:0] wdata);
//      wr_req_in_fifo.push_back(wdata);
//      $display("(time: %0d) %s: Push Wdata to Q : wdata=%0h", $time, name, wdata);
//  endfunction
//
//
//
//  // 12/11/2018: check rd data against wr fifo:
//
//  function wb_p0_check_rd_data_notification(logic[531:0] rdata);
//	data_fromQ = wr_req_in_fifo.pop_front();
//      $display("(time: %0d) %s: Pop data from Q : data=%0h", $time, name, data_fromQ);
//
//      if (rdata != data_fromQ) begin
//    	    $display("ERROR:  (time: %0d)  RD DATA IS NOT CORRECT (rdata=%0h) (wdata=%0h)", $time, rdata, data_fromQ);
//    	    $assertoff;
//    	    $finish;
//      end
//
//      $display("(time: %0d) %s: --------------------------------------------", $time, name);
//  endfunction



        // 1/8/2019: push wb, nb and sb wr_req to output of eb, wb, nb, sb and input of mem:

    function exp_o_p0_eb_wreq_notification(mby_msh_pkg::msh_row_wr_req_t exp_o_wreq);
        exp_o_p0_eb_wreq_fifo.push_back(exp_o_wreq);
    endfunction

    function exp_o_p0_eb_wdata_notification(logic[531:0] exp_o_wdata);
        exp_o_p0_eb_wdata_fifo.push_back(exp_o_wdata);
    endfunction


    function exp_o_p0_wb_wreq_notification(mby_msh_pkg::msh_row_wr_req_t exp_o_wreq);
        exp_o_p0_wb_wreq_fifo.push_back(exp_o_wreq);
    endfunction

    function exp_o_p0_wb_wdata_notification(logic[531:0] exp_o_wdata);
        exp_o_p0_wb_wdata_fifo.push_back(exp_o_wdata);
    endfunction


    function exp_o_p0_nb_wreq_notification(mby_msh_pkg::msh_col_wr_req_t exp_o_wreq);
        exp_o_p0_nb_wreq_fifo.push_back(exp_o_wreq);
    endfunction

    function exp_o_p0_nb_wdata_notification(logic[531:0] exp_o_wdata);
        exp_o_p0_nb_wdata_fifo.push_back(exp_o_wdata);
    endfunction


    function exp_o_p0_sb_wreq_notification(mby_msh_pkg::msh_col_wr_req_t exp_o_wreq);
        exp_o_p0_sb_wreq_fifo.push_back(exp_o_wreq);
    endfunction

    function exp_o_p0_sb_wdata_notification(logic[531:0] exp_o_wdata);
        exp_o_p0_sb_wdata_fifo.push_back(exp_o_wdata);
    endfunction


    function exp_o_p1_eb_wreq_notification(mby_msh_pkg::msh_row_wr_req_t exp_o_wreq);
        exp_o_p1_eb_wreq_fifo.push_back(exp_o_wreq);
    endfunction

    function exp_o_p1_eb_wdata_notification(logic[531:0] exp_o_wdata);
        exp_o_p1_eb_wdata_fifo.push_back(exp_o_wdata);
    endfunction


    function exp_o_p1_wb_wreq_notification(mby_msh_pkg::msh_row_wr_req_t exp_o_wreq);
        exp_o_p1_wb_wreq_fifo.push_back(exp_o_wreq);
    endfunction

    function exp_o_p1_wb_wdata_notification(logic[531:0] exp_o_wdata);
        exp_o_p1_wb_wdata_fifo.push_back(exp_o_wdata);
    endfunction



    function exp_o_p1_nb_wreq_notification(mby_msh_pkg::msh_col_wr_req_t exp_o_wreq);
        exp_o_p1_nb_wreq_fifo.push_back(exp_o_wreq);
    endfunction

    function exp_o_p1_nb_wdata_notification(logic[531:0] exp_o_wdata);
        exp_o_p1_nb_wdata_fifo.push_back(exp_o_wdata);
    endfunction


    function exp_o_p1_sb_wreq_notification(mby_msh_pkg::msh_col_wr_req_t exp_o_wreq);
        exp_o_p1_sb_wreq_fifo.push_back(exp_o_wreq);
    endfunction

    function exp_o_p1_sb_wdata_notification(logic[531:0] exp_o_wdata);
        exp_o_p1_sb_wdata_fifo.push_back(exp_o_wdata);
    endfunction


    function exp_mem_wreq_notification(mby_msh_pkg::mshnd_addr_t exp_waddr);
        exp_mem_wreq_fifo.push_back(exp_waddr);
    endfunction

    function exp_mem_wdata_notification(logic[531:0] exp_wdata);
        exp_mem_wdata_fifo.push_back(exp_wdata);
    endfunction


    // check p0 wr_req and wr_data:

    function check_p0_eb_wreq_notification(mby_msh_pkg::msh_row_wr_req_t det_o_wreq);
        exp_o_p0_eb_wreq_Q = exp_o_p0_eb_wreq_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p0_eb_wreq=(%0h)", $time, name, exp_o_p0_eb_wreq_Q);
        $display("(time: %0d) %s: det_o_p0_eb_wreq=(%0h)", $time, name, det_o_wreq);
        if (det_o_wreq !== exp_o_p0_eb_wreq_Q) begin

           $display("(time: %0d) %s: check eb output wreq:", $time, name);
           $display("(time: %0d) %s: exp_o_p0_eb_wreq.vld     =(%0h)", $time, name, exp_o_p0_eb_wreq_Q.vld);
           $display("(time: %0d) %s: exp_o_p0_eb_wreq.node_col=(%0h)", $time, name, exp_o_p0_eb_wreq_Q.node_col);
           $display("(time: %0d) %s: exp_o_p0_eb_wreq.node_row=(%0h)", $time, name, exp_o_p0_eb_wreq_Q.node_row);
           $display("(time: %0d) %s: exp_o_p0_eb_wreq.csr     =(%0h)", $time, name, exp_o_p0_eb_wreq_Q.csr);
           $display("(time: %0d) %s: exp_o_p0_eb_wreq.addr    =(%0h)", $time, name, exp_o_p0_eb_wreq_Q.addr);
           $display("(time: %0d) %s: exp_o_p0_eb_wreq.sema_val=(%0h)", $time, name, exp_o_p0_eb_wreq_Q.sema_val);
           $display("(time: %0d) %s: exp_o_p0_eb_wreq.age     =(%0h)", $time, name, exp_o_p0_eb_wreq_Q.age);
           $display("(time: %0d) %s: det_o_p0_eb_wreq.vld     =(%0h)", $time, name, det_o_wreq.vld);
           $display("(time: %0d) %s: det_o_p0_eb_wreq.node_col=(%0h)", $time, name, det_o_wreq.node_col);
           $display("(time: %0d) %s: det_o_p0_eb_wreq.node_row=(%0h)", $time, name, det_o_wreq.node_row);
           $display("(time: %0d) %s: det_o_p0_eb_wreq.csr     =(%0h)", $time, name, det_o_wreq.csr);
           $display("(time: %0d) %s: det_o_p0_eb_wreq.addr    =(%0h)", $time, name, det_o_wreq.addr);
           $display("(time: %0d) %s: det_o_p0_eb_wreq.sema_val=(%0h)", $time, name, det_o_wreq.sema_val);
           $display("(time: %0d) %s: det_o_p0_eb_wreq.age     =(%0h)", $time, name, det_o_wreq.age);

           $display("ERROR:  (time: %0d)  p0 eb wr req does NOT match expected output", $time);
           $assertoff;
           $finish;
        end
    endfunction

    function check_p0_eb_wdata_notification(logic[531:0] det_o_wdata);
        exp_o_p0_eb_wdata_Q = exp_o_p0_eb_wdata_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p0_eb_wdata=(%0h)", $time, name, exp_o_p0_eb_wdata_Q);
        $display("(time: %0d) %s: det_o_p0_eb_wdata=(%0h)", $time, name, det_o_wdata);
        if (det_o_wdata !== exp_o_p0_eb_wdata_Q) begin
           $display("(time: %0d) %s: exp_o_p0_eb_wdata=(%0h)", $time, name, exp_o_p0_eb_wdata_Q);
           $display("(time: %0d) %s: det_o_p0_eb_wdata=(%0h)", $time, name, det_o_wdata);

           $display("ERROR:  (time: %0d)  p0 eb wr DATA does NOT match expected output", $time);
           $assertoff;
           $finish;
        end
    endfunction



    function check_p0_wb_wreq_notification(mby_msh_pkg::msh_row_wr_req_t det_o_wreq);
        exp_o_p0_wb_wreq_Q = exp_o_p0_wb_wreq_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p0_wb_wreq=(%0h)", $time, name, exp_o_p0_wb_wreq_Q);
        $display("(time: %0d) %s: det_o_p0_wb_wreq=(%0h)", $time, name, det_o_wreq);
        if (det_o_wreq !== exp_o_p0_wb_wreq_Q) begin
           $display("(time: %0d) %s: exp_o_p0_wb_wreq.vld     =(%0h)", $time, name, exp_o_p0_wb_wreq_Q.vld);
           $display("(time: %0d) %s: exp_o_p0_wb_wreq.node_col=(%0h)", $time, name, exp_o_p0_wb_wreq_Q.node_col);
           $display("(time: %0d) %s: exp_o_p0_wb_wreq.node_row=(%0h)", $time, name, exp_o_p0_wb_wreq_Q.node_row);
           $display("(time: %0d) %s: exp_o_p0_wb_wreq.csr     =(%0h)", $time, name, exp_o_p0_wb_wreq_Q.csr);
           $display("(time: %0d) %s: exp_o_p0_wb_wreq.addr    =(%0h)", $time, name, exp_o_p0_wb_wreq_Q.addr);
           $display("(time: %0d) %s: exp_o_p0_wb_wreq.sema_val=(%0h)", $time, name, exp_o_p0_wb_wreq_Q.sema_val);
           $display("(time: %0d) %s: exp_o_p0_wb_wreq.age     =(%0h)", $time, name, exp_o_p0_wb_wreq_Q.age);
           $display("(time: %0d) %s: det_o_p0_wb_wreq.vld     =(%0h)", $time, name, det_o_wreq.vld);
           $display("(time: %0d) %s: det_o_p0_wb_wreq.node_col=(%0h)", $time, name, det_o_wreq.node_col);
           $display("(time: %0d) %s: det_o_p0_wb_wreq.node_row=(%0h)", $time, name, det_o_wreq.node_row);
           $display("(time: %0d) %s: det_o_p0_wb_wreq.csr     =(%0h)", $time, name, det_o_wreq.csr);
           $display("(time: %0d) %s: det_o_p0_wb_wreq.addr    =(%0h)", $time, name, det_o_wreq.addr);
           $display("(time: %0d) %s: det_o_p0_wb_wreq.sema_val=(%0h)", $time, name, det_o_wreq.sema_val);
           $display("(time: %0d) %s: det_o_p0_wb_wreq.age     =(%0h)", $time, name, det_o_wreq.age);

           $display("ERROR:  (time: %0d)  p0 wb wr req error", $time);
           $assertoff;
           $finish;
        end
    endfunction

    function check_p0_wb_wdata_notification(logic[531:0] det_o_wdata);
        exp_o_p0_wb_wdata_Q = exp_o_p0_wb_wdata_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p0_wb_wdata=(%0h)", $time, name, exp_o_p0_wb_wdata_Q);
        $display("(time: %0d) %s: det_o_p0_wb_wdata=(%0h)", $time, name, det_o_wdata);
        if (det_o_wdata !== exp_o_p0_wb_wdata_Q) begin
           $display("(time: %0d) %s: exp_o_p0_wb_wdata=(%0h)", $time, name, exp_o_p0_wb_wdata_Q);
           $display("(time: %0d) %s: det_o_p0_wb_wdata=(%0h)", $time, name, det_o_wdata);
           $display("ERROR:  (time: %0d)  p0 wb wr data error", $time);
           $assertoff;
           $finish;
        end
    endfunction



    function check_p0_nb_wreq_notification(mby_msh_pkg::msh_col_wr_req_t det_o_wreq);
        exp_o_p0_nb_wreq_Q = exp_o_p0_nb_wreq_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p0_nb_wreq=(%0h)", $time, name, exp_o_p0_nb_wreq_Q);
        $display("(time: %0d) %s: det_o_p0_nb_wreq=(%0h)", $time, name, det_o_wreq);

        if (det_o_wreq !== exp_o_p0_nb_wreq_Q) begin

           $display("(time: %0d) %s: check nb output wreq:", $time, name);
           $display("(time: %0d) %s: exp_o_p0_nb_wreq.vld     =(%0h)", $time, name, exp_o_p0_nb_wreq_Q.vld);
           $display("(time: %0d) %s: exp_o_p0_nb_wreq.node_row=(%0h)", $time, name, exp_o_p0_nb_wreq_Q.node_row);
           $display("(time: %0d) %s: exp_o_p0_nb_wreq.csr     =(%0h)", $time, name, exp_o_p0_nb_wreq_Q.csr);
           $display("(time: %0d) %s: exp_o_p0_nb_wreq.addr    =(%0h)", $time, name, exp_o_p0_nb_wreq_Q.addr);
           $display("(time: %0d) %s: exp_o_p0_nb_wreq.sema_val=(%0h)", $time, name, exp_o_p0_nb_wreq_Q.sema_val);
           $display("(time: %0d) %s: exp_o_p0_nb_wreq.age     =(%0h)", $time, name, exp_o_p0_nb_wreq_Q.age);
           $display("(time: %0d) %s: det_o_p0_nb_wreq.vld     =(%0h)", $time, name, det_o_wreq.vld);
           $display("(time: %0d) %s: det_o_p0_nb_wreq.node_row=(%0h)", $time, name, det_o_wreq.node_row);
           $display("(time: %0d) %s: det_o_p0_nb_wreq.csr     =(%0h)", $time, name, det_o_wreq.csr);
           $display("(time: %0d) %s: det_o_p0_nb_wreq.addr    =(%0h)", $time, name, det_o_wreq.addr);
           $display("(time: %0d) %s: det_o_p0_nb_wreq.sema_val=(%0h)", $time, name, det_o_wreq.sema_val);
           $display("(time: %0d) %s: det_o_p0_nb_wreq.age     =(%0h)", $time, name, det_o_wreq.age);

           $display("ERROR:  (time: %0d)  p0 nb wr req error", $time);
           $assertoff;
           $finish;
        end
    endfunction

    function check_p0_nb_wdata_notification(logic[531:0] det_o_wdata);
        exp_o_p0_nb_wdata_Q = exp_o_p0_nb_wdata_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p0_nb_wdata=(%0h)", $time, name, exp_o_p0_nb_wdata_Q);
        $display("(time: %0d) %s: det_o_p0_nb_wdata=(%0h)", $time, name, det_o_wdata);
        if (det_o_wdata !== exp_o_p0_nb_wdata_Q) begin
           $display("(time: %0d) %s: exp_o_p0_nb_wdata=(%0h)", $time, name, exp_o_p0_nb_wdata_Q);
           $display("(time: %0d) %s: det_o_p0_nb_wdata=(%0h)", $time, name, det_o_wdata);

           $display("ERROR:  (time: %0d)  p0 nb wr data error", $time);
           $assertoff;
           $finish;
        end
    endfunction



    function check_p0_sb_wreq_notification(mby_msh_pkg::msh_col_wr_req_t det_o_wreq);
        exp_o_p0_sb_wreq_Q = exp_o_p0_sb_wreq_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p0_sb_wreq=(%0h)", $time, name, exp_o_p0_sb_wreq_Q);
        $display("(time: %0d) %s: det_o_p0_sb_wreq=(%0h)", $time, name, det_o_wreq);

        if (det_o_wreq !== exp_o_p0_sb_wreq_Q) begin

           $display("(time: %0d) %s: check sb output wreq:", $time, name);
           $display("(time: %0d) %s: exp_o_p0_sb_wreq.vld     =(%0h)", $time, name, exp_o_p0_sb_wreq_Q.vld);
           $display("(time: %0d) %s: exp_o_p0_sb_wreq.node_row=(%0h)", $time, name, exp_o_p0_sb_wreq_Q.node_row);
           $display("(time: %0d) %s: exp_o_p0_sb_wreq.csr     =(%0h)", $time, name, exp_o_p0_sb_wreq_Q.csr);
           $display("(time: %0d) %s: exp_o_p0_sb_wreq.addr    =(%0h)", $time, name, exp_o_p0_sb_wreq_Q.addr);
           $display("(time: %0d) %s: exp_o_p0_sb_wreq.sema_val=(%0h)", $time, name, exp_o_p0_sb_wreq_Q.sema_val);
           $display("(time: %0d) %s: exp_o_p0_sb_wreq.age     =(%0h)", $time, name, exp_o_p0_sb_wreq_Q.age);
           $display("(time: %0d) %s: det_o_p0_sb_wreq.vld     =(%0h)", $time, name, det_o_wreq.vld);
           $display("(time: %0d) %s: det_o_p0_sb_wreq.node_row=(%0h)", $time, name, det_o_wreq.node_row);
           $display("(time: %0d) %s: det_o_p0_sb_wreq.csr     =(%0h)", $time, name, det_o_wreq.csr);
           $display("(time: %0d) %s: det_o_p0_sb_wreq.addr    =(%0h)", $time, name, det_o_wreq.addr);
           $display("(time: %0d) %s: det_o_p0_sb_wreq.sema_val=(%0h)", $time, name, det_o_wreq.sema_val);
           $display("(time: %0d) %s: det_o_p0_sb_wreq.age     =(%0h)", $time, name, det_o_wreq.age);

           $display("ERROR:  (time: %0d)  p0 sb wr req error", $time);
           $assertoff;
           $finish;
        end
    endfunction

    function check_p0_sb_wdata_notification(logic[531:0] det_o_wdata);
        exp_o_p0_sb_wdata_Q = exp_o_p0_sb_wdata_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p0_sb_wdata=(%0h)", $time, name, exp_o_p0_sb_wdata_Q);
        $display("(time: %0d) %s: det_o_p0_sb_wdata=(%0h)", $time, name, det_o_wdata);
        if (det_o_wdata !== exp_o_p0_sb_wdata_Q) begin
           $display("(time: %0d) %s: exp_o_p0_sb_wdata=(%0h)", $time, name, exp_o_p0_sb_wdata_Q);
           $display("(time: %0d) %s: det_o_p0_sb_wdata=(%0h)", $time, name, det_o_wdata);

           $display("ERROR:  (time: %0d)  p0 sb wr data error", $time);
           $assertoff;
           $finish;
        end
    endfunction


        // p1

    function check_p1_eb_wreq_notification(mby_msh_pkg::msh_row_wr_req_t det_o_wreq);
        exp_o_p1_eb_wreq_Q = exp_o_p1_eb_wreq_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p1_eb_wreq=(%0h)", $time, name, exp_o_p1_eb_wreq_Q);
        $display("(time: %0d) %s: det_o_p1_eb_wreq=(%0h)", $time, name, det_o_wreq);

        if (det_o_wreq !== exp_o_p1_eb_wreq_Q) begin

           $display("(time: %0d) %s: exp_o_p1_eb_wreq.vld     =(%0h)", $time, name, exp_o_p1_eb_wreq_Q.vld);
           $display("(time: %0d) %s: exp_o_p1_eb_wreq.node_col=(%0h)", $time, name, exp_o_p1_eb_wreq_Q.node_col);
           $display("(time: %0d) %s: exp_o_p1_eb_wreq.node_row=(%0h)", $time, name, exp_o_p1_eb_wreq_Q.node_row);
           $display("(time: %0d) %s: exp_o_p1_eb_wreq.csr     =(%0h)", $time, name, exp_o_p1_eb_wreq_Q.csr);
           $display("(time: %0d) %s: exp_o_p1_eb_wreq.addr    =(%0h)", $time, name, exp_o_p1_eb_wreq_Q.addr);
           $display("(time: %0d) %s: exp_o_p1_eb_wreq.sema_val=(%0h)", $time, name, exp_o_p1_eb_wreq_Q.sema_val);
           $display("(time: %0d) %s: exp_o_p1_eb_wreq.age     =(%0h)", $time, name, exp_o_p1_eb_wreq_Q.age);
           $display("(time: %0d) %s: det_o_p1_eb_wreq.vld     =(%0h)", $time, name, det_o_wreq.vld);
           $display("(time: %0d) %s: det_o_p1_eb_wreq.node_col=(%0h)", $time, name, det_o_wreq.node_col);
           $display("(time: %0d) %s: det_o_p1_eb_wreq.node_row=(%0h)", $time, name, det_o_wreq.node_row);
           $display("(time: %0d) %s: det_o_p1_eb_wreq.csr     =(%0h)", $time, name, det_o_wreq.csr);
           $display("(time: %0d) %s: det_o_p1_eb_wreq.addr    =(%0h)", $time, name, det_o_wreq.addr);
           $display("(time: %0d) %s: det_o_p1_eb_wreq.sema_val=(%0h)", $time, name, det_o_wreq.sema_val);
           $display("(time: %0d) %s: det_o_p1_eb_wreq.age     =(%0h)", $time, name, det_o_wreq.age);

           $display("ERROR:  (time: %0d)  p1 eb wr req error", $time);
           $assertoff;
           $finish;
        end
    endfunction

    function check_p1_eb_wdata_notification(logic[531:0] det_o_wdata);
        exp_o_p1_eb_wdata_Q = exp_o_p1_eb_wdata_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p1_eb_wdata=(%0h)", $time, name, exp_o_p1_eb_wdata_Q);
        $display("(time: %0d) %s: det_o_p1_eb_wdata=(%0h)", $time, name, det_o_wdata);

        if (det_o_wdata !== exp_o_p1_eb_wdata_Q) begin
           $display("(time: %0d) %s: exp_o_p1_eb_wdata=(%0h)", $time, name, exp_o_p1_eb_wdata_Q);
           $display("(time: %0d) %s: det_o_p1_eb_wdata=(%0h)", $time, name, det_o_wdata);

           $display("ERROR:  (time: %0d)  p1 eb wr data error", $time);
           $assertoff;
           $finish;
        end
    endfunction


    function check_p1_wb_wreq_notification(mby_msh_pkg::msh_row_wr_req_t det_o_wreq);
        exp_o_p1_wb_wreq_Q = exp_o_p1_wb_wreq_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p1_wb_wreq=(%0h)", $time, name, exp_o_p1_wb_wreq_Q);
        $display("(time: %0d) %s: det_o_p1_wb_wreq=(%0h)", $time, name, det_o_wreq);

        if (det_o_wreq !== exp_o_p1_wb_wreq_Q) begin

           $display("(time: %0d) %s: exp_o_p1_wb_wreq.vld     =(%0h)", $time, name, exp_o_p1_wb_wreq_Q.vld);
           $display("(time: %0d) %s: exp_o_p1_wb_wreq.node_col=(%0h)", $time, name, exp_o_p1_wb_wreq_Q.node_col);
           $display("(time: %0d) %s: exp_o_p1_wb_wreq.node_row=(%0h)", $time, name, exp_o_p1_wb_wreq_Q.node_row);
           $display("(time: %0d) %s: exp_o_p1_wb_wreq.csr     =(%0h)", $time, name, exp_o_p1_wb_wreq_Q.csr);
           $display("(time: %0d) %s: exp_o_p1_wb_wreq.addr    =(%0h)", $time, name, exp_o_p1_wb_wreq_Q.addr);
           $display("(time: %0d) %s: exp_o_p1_wb_wreq.sema_val=(%0h)", $time, name, exp_o_p1_wb_wreq_Q.sema_val);
           $display("(time: %0d) %s: exp_o_p1_wb_wreq.age     =(%0h)", $time, name, exp_o_p1_wb_wreq_Q.age);
           $display("(time: %0d) %s: det_o_p1_wb_wreq.vld     =(%0h)", $time, name, det_o_wreq.vld);
           $display("(time: %0d) %s: det_o_p1_wb_wreq.node_col=(%0h)", $time, name, det_o_wreq.node_col);
           $display("(time: %0d) %s: det_o_p1_wb_wreq.node_row=(%0h)", $time, name, det_o_wreq.node_row);
           $display("(time: %0d) %s: det_o_p1_wb_wreq.csr     =(%0h)", $time, name, det_o_wreq.csr);
           $display("(time: %0d) %s: det_o_p1_wb_wreq.addr    =(%0h)", $time, name, det_o_wreq.addr);
           $display("(time: %0d) %s: det_o_p1_wb_wreq.sema_val=(%0h)", $time, name, det_o_wreq.sema_val);
           $display("(time: %0d) %s: det_o_p1_wb_wreq.age     =(%0h)", $time, name, det_o_wreq.age);

           $display("ERROR:  (time: %0d)  p1 wb wr req error", $time);
           $assertoff;
           $finish;
        end
    endfunction

    function check_p1_wb_wdata_notification(logic[531:0] det_o_wdata);
        exp_o_p1_wb_wdata_Q = exp_o_p1_wb_wdata_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p1_wb_wdata=(%0h)", $time, name, exp_o_p1_wb_wdata_Q);
        $display("(time: %0d) %s: det_o_p1_wb_wdata=(%0h)", $time, name, det_o_wdata);
        if (det_o_wdata !== exp_o_p1_wb_wdata_Q) begin
           $display("(time: %0d) %s: exp_o_p1_wb_wdata=(%0h)", $time, name, exp_o_p1_wb_wdata_Q);
           $display("(time: %0d) %s: det_o_p1_wb_wdata=(%0h)", $time, name, det_o_wdata);

           $display("ERROR:  (time: %0d)  p1 wb wr data error", $time);
           $assertoff;
           $finish;
        end
    endfunction


    function check_p1_nb_wreq_notification(mby_msh_pkg::msh_col_wr_req_t det_o_wreq);
        exp_o_p1_nb_wreq_Q = exp_o_p1_nb_wreq_fifo.pop_front();

        $display("(time: %0d) %s: exp_o_p1_nb_wreq=(%0h)", $time, name, exp_o_p1_nb_wreq_Q);
        $display("(time: %0d) %s: det_o_p1_nb_wreq=(%0h)", $time, name, det_o_wreq);

        if (det_o_wreq !== exp_o_p1_nb_wreq_Q) begin

           $display("(time: %0d) %s: check nb output wreq:", $time, name);
           $display("(time: %0d) %s: exp_o_p1_nb_wreq.vld     =(%0h)", $time, name, exp_o_p1_nb_wreq_Q.vld);
           $display("(time: %0d) %s: exp_o_p1_nb_wreq.node_row=(%0h)", $time, name, exp_o_p1_nb_wreq_Q.node_row);
           $display("(time: %0d) %s: exp_o_p1_nb_wreq.csr     =(%0h)", $time, name, exp_o_p1_nb_wreq_Q.csr);
           $display("(time: %0d) %s: exp_o_p1_nb_wreq.addr    =(%0h)", $time, name, exp_o_p1_nb_wreq_Q.addr);
           $display("(time: %0d) %s: exp_o_p1_nb_wreq.sema_val=(%0h)", $time, name, exp_o_p1_nb_wreq_Q.sema_val);
           $display("(time: %0d) %s: exp_o_p1_nb_wreq.age     =(%0h)", $time, name, exp_o_p1_nb_wreq_Q.age);
           $display("(time: %0d) %s: det_o_p1_nb_wreq.vld     =(%0h)", $time, name, det_o_wreq.vld);
           $display("(time: %0d) %s: det_o_p1_nb_wreq.node_row=(%0h)", $time, name, det_o_wreq.node_row);
           $display("(time: %0d) %s: det_o_p1_nb_wreq.csr     =(%0h)", $time, name, det_o_wreq.csr);
           $display("(time: %0d) %s: det_o_p1_nb_wreq.addr    =(%0h)", $time, name, det_o_wreq.addr);
           $display("(time: %0d) %s: det_o_p1_nb_wreq.sema_val=(%0h)", $time, name, det_o_wreq.sema_val);
           $display("(time: %0d) %s: det_o_p1_nb_wreq.age     =(%0h)", $time, name, det_o_wreq.age);

           $display("ERROR:  (time: %0d)  p1 nb wr req error", $time);
           $assertoff;
           $finish;
        end
    endfunction

    function check_p1_nb_wdata_notification(logic[531:0] det_o_wdata);
        exp_o_p1_nb_wdata_Q = exp_o_p1_nb_wdata_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p1_nb_wdata=(%0h)", $time, name, exp_o_p1_nb_wdata_Q);
        $display("(time: %0d) %s: det_o_p1_nb_wdata=(%0h)", $time, name, det_o_wdata);

        if (det_o_wdata !== exp_o_p1_nb_wdata_Q) begin
           $display("(time: %0d) %s: exp_o_p1_nb_wdata=(%0h)", $time, name, exp_o_p1_nb_wdata_Q);
           $display("(time: %0d) %s: det_o_p1_nb_wdata=(%0h)", $time, name, det_o_wdata);

           $display("ERROR:  (time: %0d)  p1 nb wr data error", $time);
           $assertoff;
           $finish;
        end
    endfunction

    function check_p1_sb_wreq_notification(mby_msh_pkg::msh_col_wr_req_t det_o_wreq);
        exp_o_p1_sb_wreq_Q = exp_o_p1_sb_wreq_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p1_sb_wreq=(%0h)", $time, name, exp_o_p1_sb_wreq_Q);
        $display("(time: %0d) %s: det_o_p1_sb_wreq=(%0h)", $time, name, det_o_wreq);

        if (det_o_wreq !== exp_o_p1_sb_wreq_Q) begin

           $display("(time: %0d) %s: check sb output wreq:", $time, name);
           $display("(time: %0d) %s: exp_o_p1_sb_wreq.vld     =(%0h)", $time, name, exp_o_p1_sb_wreq_Q.vld);
           $display("(time: %0d) %s: exp_o_p1_sb_wreq.node_row=(%0h)", $time, name, exp_o_p1_sb_wreq_Q.node_row);
           $display("(time: %0d) %s: exp_o_p1_sb_wreq.csr     =(%0h)", $time, name, exp_o_p1_sb_wreq_Q.csr);
           $display("(time: %0d) %s: exp_o_p1_sb_wreq.addr    =(%0h)", $time, name, exp_o_p1_sb_wreq_Q.addr);
           $display("(time: %0d) %s: exp_o_p1_sb_wreq.sema_val=(%0h)", $time, name, exp_o_p1_sb_wreq_Q.sema_val);
           $display("(time: %0d) %s: exp_o_p1_sb_wreq.age     =(%0h)", $time, name, exp_o_p1_sb_wreq_Q.age);
           $display("(time: %0d) %s: det_o_p1_sb_wreq.vld     =(%0h)", $time, name, det_o_wreq.vld);
           $display("(time: %0d) %s: det_o_p1_sb_wreq.node_row=(%0h)", $time, name, det_o_wreq.node_row);
           $display("(time: %0d) %s: det_o_p1_sb_wreq.csr     =(%0h)", $time, name, det_o_wreq.csr);
           $display("(time: %0d) %s: det_o_p1_sb_wreq.addr    =(%0h)", $time, name, det_o_wreq.addr);
           $display("(time: %0d) %s: det_o_p1_sb_wreq.sema_val=(%0h)", $time, name, det_o_wreq.sema_val);
           $display("(time: %0d) %s: det_o_p1_sb_wreq.age     =(%0h)", $time, name, det_o_wreq.age);

           $display("ERROR:  (time: %0d)  p1 sb wr req error", $time);
           $assertoff;
           $finish;
        end
    endfunction

    function check_p1_sb_wdata_notification(logic[531:0] det_o_wdata);
        exp_o_p1_sb_wdata_Q = exp_o_p1_sb_wdata_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p1_sb_wdata=(%0h)", $time, name, exp_o_p1_sb_wdata_Q);
        $display("(time: %0d) %s: det_o_p1_sb_wdata=(%0h)", $time, name, det_o_wdata);

        if (det_o_wdata !== exp_o_p1_sb_wdata_Q) begin
           $display("(time: %0d) %s: exp_o_p1_sb_wdata=(%0h)", $time, name, exp_o_p1_sb_wdata_Q);
           $display("(time: %0d) %s: det_o_p1_sb_wdata=(%0h)", $time, name, det_o_wdata);

           $display("ERROR:  (time: %0d)  p1 sb wr data error", $time);
           $assertoff;
           $finish;
        end
    endfunction

        // check wr to mem

    function check_mem_wr_notification(logic[13:0] det_adr, logic[531:0] det_wdata);
        exp_mem_wreq_Q = exp_mem_wreq_fifo.pop_front();  // adr
        exp_mem_wdata_Q = exp_mem_wdata_fifo.pop_front();  // wdata

        $display("(time: %0d) %s: exp_mem_addr=(%0h), exp_mem_wdata=(%0h)", $time, name, exp_mem_wreq_Q, exp_mem_wdata_Q);
        $display("(time: %0d) %s: det_mem_addr=(%0h), det_mem_wdata=(%0h)", $time, name, det_adr, det_wdata);

     // if ((adr !== exp_mem_wreq_Q[11:0]) || (wdata != exp_mem_wdata_Q)) begin
     // if (det_adr !== exp_mem_wreq_Q[11:0]) begin
        if (det_adr !== exp_mem_wreq_Q) begin
           $display("(time: %0d) %s: exp_mem_addr=(%0h)", $time, name, exp_mem_wreq_Q);
           $display("(time: %0d) %s: det_mem_addr=(%0h)", $time, name, det_adr);
           $display("ERROR:  (time: %0d)  mem wr addr error", $time);
           $assertoff;
           $finish;
        end
        else if (det_wdata !== exp_mem_wdata_Q) begin
           $display("(time: %0d) %s: exp_mem_wdata=(%0h)", $time, name, exp_o_p0_sb_wdata_Q);
           $display("(time: %0d) %s: det_mem_wdata=(%0h)", $time, name, det_wdata);
           $display("ERROR:  (time: %0d)  mem wr data error", $time);
           $assertoff;
           $finish;
        end
    endfunction



        // push eb, wb, nb and sb rd_req to output of eb, wb, nb, sb and input of mem:

    function exp_o_p0_eb_rreq_notification(mby_msh_pkg::msh_row_rd_req_t exp_o_rreq);
        exp_o_p0_eb_rreq_fifo.push_back(exp_o_rreq);
    endfunction

    function exp_o_p0_wb_rreq_notification(mby_msh_pkg::msh_row_rd_req_t exp_o_rreq);
        exp_o_p0_wb_rreq_fifo.push_back(exp_o_rreq);
    endfunction

    function exp_o_p0_nb_rreq_notification(mby_msh_pkg::msh_col_rd_req_t exp_o_rreq);
        exp_o_p0_nb_rreq_fifo.push_back(exp_o_rreq);
    endfunction

    function exp_o_p0_sb_rreq_notification(mby_msh_pkg::msh_col_rd_req_t exp_o_rreq);
        exp_o_p0_sb_rreq_fifo.push_back(exp_o_rreq);
    endfunction


    function exp_o_p1_eb_rreq_notification(mby_msh_pkg::msh_row_rd_req_t exp_o_rreq);
        exp_o_p1_eb_rreq_fifo.push_back(exp_o_rreq);
    endfunction

    function exp_o_p1_wb_rreq_notification(mby_msh_pkg::msh_row_rd_req_t exp_o_rreq);
        exp_o_p1_wb_rreq_fifo.push_back(exp_o_rreq);
    endfunction

    function exp_o_p1_nb_rreq_notification(mby_msh_pkg::msh_col_rd_req_t exp_o_rreq);
        exp_o_p1_nb_rreq_fifo.push_back(exp_o_rreq);
    endfunction

    function exp_o_p1_sb_rreq_notification(mby_msh_pkg::msh_col_rd_req_t exp_o_rreq);
        exp_o_p1_sb_rreq_fifo.push_back(exp_o_rreq);
    endfunction


    function exp_mem_rreq_notification(mby_msh_pkg::mshnd_addr_t exp_raddr);
        exp_mem_rreq_fifo.push_back(exp_raddr);
    endfunction


    // check rd adr at mem if:

    function check_mem_rreq_notification(logic[11:0] det_adr);
        exp_mem_rreq_Q = exp_mem_rreq_fifo.pop_front();  // adr

        $display("(time: %0d) %s: exp_mem_rreq.adr=(%0h)", $time, name, exp_mem_rreq_Q[11:0]);
        $display("(time: %0d) %s: det_mem_rreq.adr=(%0h)", $time, name, det_adr);

        if (det_adr !== exp_mem_rreq_Q[11:0]) begin
           $display("ERROR:  (time: %0d)  mem rd addr error", $time);
           $assertoff;
           $finish;
        end
    endfunction


    function check_p0_eb_rreq_notification(mby_msh_pkg::msh_row_rd_req_t det_o_rreq);
        exp_o_p0_eb_rreq_Q = exp_o_p0_eb_rreq_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p0_eb_rreq=(%0h)", $time, name, exp_o_p0_eb_rreq_Q);
        $display("(time: %0d) %s: det_o_p0_eb_rreq=(%0h)", $time, name, det_o_rreq);

        if (det_o_rreq !== exp_o_p0_eb_rreq_Q) begin

           $display("(time: %0d) %s: -------------------------------", $time, name);
           $display("(time: %0d) %s: exp_o_p0_eb_rreq.vld     =(%0h)", $time, name, exp_o_p0_eb_rreq_Q.vld);
           $display("(time: %0d) %s: exp_o_p0_eb_rreq.id      =(%0h)", $time, name, exp_o_p0_eb_rreq_Q.id);
           $display("(time: %0d) %s: exp_o_p0_eb_rreq.node_col=(%0h)", $time, name, exp_o_p0_eb_rreq_Q.node_col);
           $display("(time: %0d) %s: exp_o_p0_eb_rreq.node_row=(%0h)", $time, name, exp_o_p0_eb_rreq_Q.node_row);
           $display("(time: %0d) %s: exp_o_p0_eb_rreq.csr     =(%0h)", $time, name, exp_o_p0_eb_rreq_Q.csr);
           $display("(time: %0d) %s: exp_o_p0_eb_rreq.addr    =(%0h)", $time, name, exp_o_p0_eb_rreq_Q.addr);
           $display("(time: %0d) %s: exp_o_p0_eb_rreq.sema_val=(%0h)", $time, name, exp_o_p0_eb_rreq_Q.sema_val);
           $display("(time: %0d) %s: exp_o_p0_eb_rreq.age     =(%0h)", $time, name, exp_o_p0_eb_rreq_Q.age);
           $display("(time: %0d) %s: -------------------------------", $time, name);
           $display("(time: %0d) %s: det_o_p0_eb_rreq.vld     =(%0h)", $time, name, det_o_rreq.vld);
           $display("(time: %0d) %s: det_o_p0_eb_rreq.id      =(%0h)", $time, name, det_o_rreq.id);
           $display("(time: %0d) %s: det_o_p0_eb_rreq.node_col=(%0h)", $time, name, det_o_rreq.node_col);
           $display("(time: %0d) %s: det_o_p0_eb_rreq.node_row=(%0h)", $time, name, det_o_rreq.node_row);
           $display("(time: %0d) %s: det_o_p0_eb_rreq.csr     =(%0h)", $time, name, det_o_rreq.csr);
           $display("(time: %0d) %s: det_o_p0_eb_rreq.addr    =(%0h)", $time, name, det_o_rreq.addr);
           $display("(time: %0d) %s: det_o_p0_eb_rreq.sema_val=(%0h)", $time, name, det_o_rreq.sema_val);
           $display("(time: %0d) %s: det_o_p0_eb_rreq.age     =(%0h)", $time, name, det_o_rreq.age);

           $display("ERROR:  (time: %0d)  p0 eb output rd req error", $time);
           $assertoff;
           $finish;
        end
    endfunction

    function check_p0_wb_rreq_notification(mby_msh_pkg::msh_row_rd_req_t det_o_rreq);
        exp_o_p0_wb_rreq_Q = exp_o_p0_wb_rreq_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p0_wb_rreq=(%0h)", $time, name, exp_o_p0_wb_rreq_Q);
        $display("(time: %0d) %s: det_o_p0_wb_rreq=(%0h)", $time, name, det_o_rreq);

        if (det_o_rreq !== exp_o_p0_wb_rreq_Q) begin
           $display("ERROR:  (time: %0d)  p0 wb output rd req error", $time);
           $assertoff;
           $finish;
        end
    endfunction

    function check_p0_nb_rreq_notification(mby_msh_pkg::msh_col_rd_req_t det_o_rreq);
        exp_o_p0_nb_rreq_Q = exp_o_p0_nb_rreq_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p0_nb_rreq=(%0h)", $time, name, exp_o_p0_nb_rreq_Q);
        $display("(time: %0d) %s: det_o_p0_nb_rreq=(%0h)", $time, name, det_o_rreq);

        if (det_o_rreq !== exp_o_p0_nb_rreq_Q) begin
           $display("ERROR:  (time: %0d)  p0 nb output rd req error", $time);
           $assertoff;
           $finish;
        end
    endfunction

    function check_p0_sb_rreq_notification(mby_msh_pkg::msh_col_rd_req_t det_o_rreq);
        exp_o_p0_sb_rreq_Q = exp_o_p0_sb_rreq_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p0_sb_rreq=(%0h)", $time, name, exp_o_p0_sb_rreq_Q);
        $display("(time: %0d) %s: det_o_p0_sb_rreq=(%0h)", $time, name, det_o_rreq);

        if (det_o_rreq !== exp_o_p0_sb_rreq_Q) begin
           $display("ERROR:  (time: %0d)  p0 sb output rd req error", $time);
           $assertoff;
           $finish;
        end
    endfunction


    function check_p1_eb_rreq_notification(mby_msh_pkg::msh_row_rd_req_t det_o_rreq);
        exp_o_p1_eb_rreq_Q = exp_o_p1_eb_rreq_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p1_eb_rreq=(%0h)", $time, name, exp_o_p1_eb_rreq_Q);
        $display("(time: %0d) %s: det_o_p1_eb_rreq=(%0h)", $time, name, det_o_rreq);

        if (det_o_rreq !== exp_o_p1_eb_rreq_Q) begin
           $display("ERROR:  (time: %0d)  p1 eb output rd req error", $time);
           $assertoff;
           $finish;
        end
    endfunction

    function check_p1_wb_rreq_notification(mby_msh_pkg::msh_row_rd_req_t det_o_rreq);
        exp_o_p1_wb_rreq_Q = exp_o_p1_wb_rreq_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p1_wb_rreq=(%0h)", $time, name, exp_o_p1_wb_rreq_Q);
        $display("(time: %0d) %s: det_o_p1_wb_rreq=(%0h)", $time, name, det_o_rreq);

        if (det_o_rreq !== exp_o_p1_wb_rreq_Q) begin
           $display("ERROR:  (time: %0d)  p1 wb output rd req error", $time);
           $assertoff;
           $finish;
        end
    endfunction

    function check_p1_nb_rreq_notification(mby_msh_pkg::msh_col_rd_req_t det_o_rreq);
        exp_o_p1_nb_rreq_Q = exp_o_p1_nb_rreq_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p1_nb_rreq=(%0h)", $time, name, exp_o_p1_nb_rreq_Q);
        $display("(time: %0d) %s: det_o_p1_nb_rreq=(%0h)", $time, name, det_o_rreq);

        if (det_o_rreq !== exp_o_p1_nb_rreq_Q) begin
           $display("(time: %0d) %s: --------------------------", $time, name);
           $display("(time: %0d) %s: exp_o_p1_nb_rreq.vld      =(%0d)", $time, name, exp_o_p1_nb_rreq_Q.vld);
           $display("(time: %0d) %s: exp_o_p1_nb_rreq.id       =(%0h)", $time, name, exp_o_p1_nb_rreq_Q.id);
           $display("(time: %0d) %s: exp_o_p1_nb_rreq.port_row =(%0h)", $time, name, exp_o_p1_nb_rreq_Q.port_row);
           $display("(time: %0d) %s: exp_o_p1_nb_rreq.port_side=(%0h)", $time, name, exp_o_p1_nb_rreq_Q.port_side);
           $display("(time: %0d) %s: exp_o_p1_nb_rreq.node_row =(%0h)", $time, name, exp_o_p1_nb_rreq_Q.node_row);
           $display("(time: %0d) %s: exp_o_p1_nb_rreq.csr      =(%0d)", $time, name, exp_o_p1_nb_rreq_Q.csr);
           $display("(time: %0d) %s: exp_o_p1_nb_rreq.addr     =(%0h)", $time, name, exp_o_p1_nb_rreq_Q.addr);
           $display("(time: %0d) %s: exp_o_p1_nb_rreq.sema_val =(%0d)", $time, name, exp_o_p1_nb_rreq_Q.sema_val);
           $display("(time: %0d) %s: exp_o_p1_nb_rreq.age      =(%0h)", $time, name, exp_o_p1_nb_rreq_Q.age);
           $display("(time: %0d) %s: --------------------------", $time, name);
           $display("(time: %0d) %s: det_o_p1_nb_rreq.vld      =(%0d)", $time, name, det_o_rreq.vld);
           $display("(time: %0d) %s: det_o_p1_nb_rreq.id       =(%0h)", $time, name, det_o_rreq.id);
           $display("(time: %0d) %s: det_o_p1_nb_rreq.port_row =(%0h)", $time, name, det_o_rreq.port_row);
           $display("(time: %0d) %s: det_o_p1_nb_rreq.port_side=(%0h)", $time, name, det_o_rreq.port_side);
           $display("(time: %0d) %s: det_o_p1_nb_rreq.node_row =(%0h)", $time, name, det_o_rreq.node_row);
           $display("(time: %0d) %s: det_o_p1_nb_rreq.csr      =(%0d)", $time, name, det_o_rreq.csr);
           $display("(time: %0d) %s: det_o_p1_nb_rreq.addr     =(%0h)", $time, name, det_o_rreq.addr);
           $display("(time: %0d) %s: det_o_p1_nb_rreq.sema_val =(%0d)", $time, name, det_o_rreq.sema_val);
           $display("(time: %0d) %s: det_o_p1_nb_rreq.age      =(%0h)", $time, name, det_o_rreq.age);

           $display("ERROR:  (time: %0d)  p1 nb output rd req error", $time);
           $assertoff;
           $finish;
        end
    endfunction

    function check_p1_sb_rreq_notification(mby_msh_pkg::msh_col_rd_req_t det_o_rreq);
        exp_o_p1_sb_rreq_Q = exp_o_p1_sb_rreq_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p1_sb_rreq=(%0h)", $time, name, exp_o_p1_sb_rreq_Q);
        $display("(time: %0d) %s: det_o_p1_sb_rreq=(%0h)", $time, name, det_o_rreq);

        if (det_o_rreq !== exp_o_p1_sb_rreq_Q) begin
           $display("ERROR:  (time: %0d)  p1 sb output rd req error", $time);
           $assertoff;
           $finish;
        end
    endfunction



// 1/13/2019 monitor rsp


    function exp_o_p0_wb_rsp_notification(mby_msh_pkg::msh_row_rd_rsp_t  exp_o_rsp);
        exp_o_p0_wb_rsp_fifo.push_back(exp_o_rsp);
    endfunction

    function exp_o_p0_eb_rsp_notification(mby_msh_pkg::msh_row_rd_rsp_t  exp_o_rsp);
        exp_o_p0_eb_rsp_fifo.push_back(exp_o_rsp);
    endfunction

    function exp_o_p0_nb_rsp_notification(mby_msh_pkg::msh_col_rd_rsp_t  exp_o_rsp);
        exp_o_p0_nb_rsp_fifo.push_back(exp_o_rsp);
    endfunction

    function exp_o_p0_sb_rsp_notification(mby_msh_pkg::msh_col_rd_rsp_t  exp_o_rsp);
        exp_o_p0_sb_rsp_fifo.push_back(exp_o_rsp);
    endfunction


    function exp_o_p1_wb_rsp_notification(mby_msh_pkg::msh_row_rd_rsp_t  exp_o_rsp);
        exp_o_p1_wb_rsp_fifo.push_back(exp_o_rsp);
    endfunction

    function exp_o_p1_eb_rsp_notification(mby_msh_pkg::msh_row_rd_rsp_t  exp_o_rsp);
        exp_o_p1_eb_rsp_fifo.push_back(exp_o_rsp);
    endfunction

    function exp_o_p1_nb_rsp_notification(mby_msh_pkg::msh_col_rd_rsp_t  exp_o_rsp);
        exp_o_p1_nb_rsp_fifo.push_back(exp_o_rsp);
    endfunction

    function exp_o_p1_sb_rsp_notification(mby_msh_pkg::msh_col_rd_rsp_t  exp_o_rsp);
        exp_o_p1_sb_rsp_fifo.push_back(exp_o_rsp);
    endfunction


	// push exp rd data:

    function exp_o_p0_eb_rdata_notification(mby_msh_pkg::msh_data_t exp_o_rdata);
        exp_o_p0_eb_rdata_fifo.push_back(exp_o_rdata);
    endfunction

    function exp_o_p0_wb_rdata_notification(mby_msh_pkg::msh_data_t exp_o_rdata);
     // $display("(time: %0d) %s: push exp_o_p0_wb_rdata=(%0h)", $time, name, exp_o_rdata);
        exp_o_p0_wb_rdata_fifo.push_back(exp_o_rdata);
    endfunction

    function exp_o_p0_nb_rdata_notification(mby_msh_pkg::msh_data_t exp_o_rdata);
        exp_o_p0_nb_rdata_fifo.push_back(exp_o_rdata);
    endfunction

    function exp_o_p0_sb_rdata_notification(mby_msh_pkg::msh_data_t exp_o_rdata);
        exp_o_p0_sb_rdata_fifo.push_back(exp_o_rdata);
    endfunction



    function exp_o_p1_eb_rdata_notification(mby_msh_pkg::msh_data_t exp_o_rdata);
        exp_o_p1_eb_rdata_fifo.push_back(exp_o_rdata);
    endfunction

    function exp_o_p1_wb_rdata_notification(mby_msh_pkg::msh_data_t exp_o_rdata);
        exp_o_p1_wb_rdata_fifo.push_back(exp_o_rdata);
    endfunction

    function exp_o_p1_nb_rdata_notification(mby_msh_pkg::msh_data_t exp_o_rdata);
        exp_o_p1_nb_rdata_fifo.push_back(exp_o_rdata);
    endfunction

    function exp_o_p1_sb_rdata_notification(mby_msh_pkg::msh_data_t exp_o_rdata);
        exp_o_p1_sb_rdata_fifo.push_back(exp_o_rdata);
    endfunction


	// check rsp


    function check_p0_eb_rsp_notification(mby_msh_pkg::msh_row_rd_rsp_t det_o_rsp);
        exp_o_p0_eb_rsp_Q = exp_o_p0_eb_rsp_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p0_eb_rsp=(%0h)", $time, name, exp_o_p0_eb_rsp_Q);
        $display("(time: %0d) %s: det_o_p0_eb_rsp=(%0h)", $time, name, det_o_rsp);

        if (det_o_rsp !== exp_o_p0_eb_rsp_Q) begin
           $display("ERROR:  (time: %0d)  p0 eb rsp output error", $time);
           $assertoff;
           $finish;
        end
    endfunction


    function check_p0_wb_rsp_notification(mby_msh_pkg::msh_row_rd_rsp_t det_o_rsp);
        exp_o_p0_wb_rsp_Q = exp_o_p0_wb_rsp_fifo.pop_front();

          $display("(time: %0d) %s: exp_o_p0_wb_rsp=(%0h)", $time, name, exp_o_p0_wb_rsp_Q);
          $display("(time: %0d) %s: det_o_p0_wb_rsp=(%0h)", $time, name, det_o_rsp);

        if (det_o_rsp !== exp_o_p0_wb_rsp_Q) begin

           $display("(time: %0d) %s: exp_o_p0_wb_rsp.vld=(%0h)", $time, name, exp_o_p0_wb_rsp_Q.vld);
           $display("(time: %0d) %s: exp_o_p0_wb_rsp.id=(%0h)",  $time, name, exp_o_p0_wb_rsp_Q.id);

           $display("(time: %0d) %s: det_o_rsp.vld=(%0h)", $time, name, det_o_rsp.vld);
           $display("(time: %0d) %s: det_o_rsp.id=(%0h)",  $time, name, det_o_rsp.id);

           $display("ERROR:  (time: %0d)  p0 wb rsp output error", $time);
           $assertoff;
           $finish;
        end
    endfunction


    function check_p0_nb_rsp_notification(mby_msh_pkg::msh_col_rd_rsp_t det_o_rsp);
        exp_o_p0_nb_rsp_Q = exp_o_p0_nb_rsp_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p0_nb_rsp=(%0h)", $time, name, exp_o_p0_nb_rsp_Q);
        $display("(time: %0d) %s: det_o_p0_nb_rsp=(%0h)", $time, name, det_o_rsp);

        if (det_o_rsp !== exp_o_p0_nb_rsp_Q) begin
           $display("ERROR:  (time: %0d)  p0 nb rsp output error", $time);
           $assertoff;
           $finish;
        end
    endfunction


    function check_p0_sb_rsp_notification(mby_msh_pkg::msh_col_rd_rsp_t det_o_rsp);
        exp_o_p0_sb_rsp_Q = exp_o_p0_sb_rsp_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p0_sb_rsp=(%0h)", $time, name, exp_o_p0_sb_rsp_Q);
        $display("(time: %0d) %s: det_o_p0_sb_rsp=(%0h)", $time, name, det_o_rsp);

        if (det_o_rsp !== exp_o_p0_sb_rsp_Q) begin
           $display("ERROR:  (time: %0d)  p0 sb rsp output error", $time);
           $assertoff;
           $finish;
        end
    endfunction

	// check p1 rsp

    function check_p1_eb_rsp_notification(mby_msh_pkg::msh_row_rd_rsp_t det_o_rsp);
        exp_o_p1_eb_rsp_Q = exp_o_p1_eb_rsp_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p1_eb_rsp=(%0h)", $time, name, exp_o_p1_eb_rsp_Q);
        $display("(time: %0d) %s: det_o_p1_eb_rsp=(%0h)", $time, name, det_o_rsp);

        if (det_o_rsp !== exp_o_p1_eb_rsp_Q) begin
           $display("ERROR:  (time: %0d)  p1 eb rsp output error", $time);
           $assertoff;
           $finish;
        end
    endfunction 
           
    function check_p1_wb_rsp_notification(mby_msh_pkg::msh_row_rd_rsp_t det_o_rsp);
        exp_o_p1_wb_rsp_Q = exp_o_p1_wb_rsp_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p1_wb_rsp=(%0h)", $time, name, exp_o_p1_wb_rsp_Q);
        $display("(time: %0d) %s: det_o_p1_wb_rsp=(%0h)", $time, name, det_o_rsp);

        if (det_o_rsp !== exp_o_p1_wb_rsp_Q) begin
           $display("ERROR:  (time: %0d)  p1 wb rsp output error", $time);
           $assertoff;
           $finish;
        end
    endfunction


    function check_p1_nb_rsp_notification(mby_msh_pkg::msh_col_rd_rsp_t det_o_rsp);
        exp_o_p1_nb_rsp_Q = exp_o_p1_nb_rsp_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p1_nb_rsp=(%0h)", $time, name, exp_o_p1_nb_rsp_Q);
        $display("(time: %0d) %s: det_o_p1_nb_rsp=(%0h)", $time, name, det_o_rsp);

        if (det_o_rsp !== exp_o_p1_nb_rsp_Q) begin
           $display("ERROR:  (time: %0d)  p1 nb rsp output error", $time);
           $assertoff;
           $finish;
        end
    endfunction


    function check_p1_sb_rsp_notification(mby_msh_pkg::msh_col_rd_rsp_t det_o_rsp);
        exp_o_p1_sb_rsp_Q = exp_o_p1_sb_rsp_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p1_sb_rsp=(%0h)", $time, name, exp_o_p1_sb_rsp_Q);
        $display("(time: %0d) %s: det_o_p1_sb_rsp=(%0h)", $time, name, det_o_rsp);

        if (det_o_rsp !== exp_o_p1_sb_rsp_Q) begin
           $display("ERROR:  (time: %0d)  p1 sb rsp output error", $time);
           $assertoff;
           $finish; 
        end
    endfunction



	// check rdata:


    function check_p0_eb_rdata_notification(mby_msh_pkg::msh_data_t det_o_rdata);
        exp_o_p0_eb_rdata_Q = exp_o_p0_eb_rdata_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p0_eb_rdata=(%0h)", $time, name, exp_o_p0_eb_rdata_Q);
        $display("(time: %0d) %s: det_o_p0_eb_rdata=(%0h)", $time, name, det_o_rdata);
        $display("(time: %0d) %s: --------", $time, name);

        if (det_o_rdata !== exp_o_p0_eb_rdata_Q) begin
           $display("ERROR:  (time: %0d)  p0 eb rdata output error", $time);
           $assertoff;
           $finish;
        end
    endfunction

    function check_p0_wb_rdata_notification(mby_msh_pkg::msh_data_t det_o_rdata);
        exp_o_p0_wb_rdata_Q = exp_o_p0_wb_rdata_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p0_wb_rdata=(%0h)", $time, name, exp_o_p0_wb_rdata_Q);
        $display("(time: %0d) %s: det_o_p0_wb_rdata=(%0h)", $time, name, det_o_rdata);
        $display("(time: %0d) %s: --------", $time, name);

//      $display("(time: %0d) %s: WARNING: before check", $time, name);
//
//      if (det_o_rdata == exp_o_p0_wb_rdata_Q) 
//         $display("(time: %0d) %s: WARNING: 1st CHECK (if == ): exp_o_rdata and det_o_rdata are     equal !!!", $time,name);
//	else
//         $display("(time: %0d) %s: WARNING: 1st CHECK (if == ): exp_o_rdata and det_o_rdata are NOT equal !!!", $time,name);
//
//      if (det_o_rdata === exp_o_p0_wb_rdata_Q) 
//         $display("(time: %0d) %s: WARNING: 2nd CHECK (if ===): exp_o_rdata and det_o_rdata are     equal !!!", $time,name);
//	else
//         $display("(time: %0d) %s: WARNING: 2nd CHECK (if ===): exp_o_rdata and det_o_rdata are NOT equal !!!", $time,name);
//
//      if (det_o_rdata !== exp_o_p0_wb_rdata_Q) 
//         $display("(time: %0d) %s: WARNING: 3rd CHECK (if != ): exp_o_rdata and det_o_rdata are NOT equal !!!", $time,name);
//	else
//         $display("(time: %0d) %s: WARNING: 3rd CHECK (if != ): exp_o_rdata and det_o_rdata are     equal !!!", $time,name);
//
//      if (det_o_rdata !=== exp_o_p0_wb_rdata_Q) 
//         $display("(time: %0d) %s: WARNING: 4th CHECK (if !==): exp_o_rdata and det_o_rdata are NOT equal !!!", $time,name);
//	else
//         $display("(time: %0d) %s: WARNING: 4th CHECK (if !==): exp_o_rdata and det_o_rdata are     equal !!!", $time,name);

// Note:
// in case one of variables is (xxxxxxxx...x), e.g.
//	exp_o_p0_wb_rdata=(xxxxxxxx...x)
//	det_o_p0_wb_rdata=(b4861143)
// "!=" won't truely tell they are different. So have to use "!==" in stead below


     // if (det_o_rdata != exp_o_p0_wb_rdata_Q) begin
        if (det_o_rdata !== exp_o_p0_wb_rdata_Q) begin
           $display("(time: %0d) %s: WARNING: exp_o_rdata and det_o_rdata are NOT equal !!!", $time,name);
           $display("ERROR:  (time: %0d)  p0 wb rdata output error", $time);
           $assertoff;
           $finish;
	end
//	else
//        $display("(time: %0d) %s: WARNING: 4th CHECK (if !==): exp_o_rdata and det_o_rdata are     equal !!!", $time,name);
//
//      $display("(time: %0d) %s: --------", $time, name);
    endfunction

    function check_p0_nb_rdata_notification(mby_msh_pkg::msh_data_t det_o_rdata);
        exp_o_p0_nb_rdata_Q = exp_o_p0_nb_rdata_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p0_nb_rdata=(%0h)", $time, name, exp_o_p0_nb_rdata_Q);
        $display("(time: %0d) %s: det_o_p0_nb_rdata=(%0h)", $time, name, det_o_rdata);
        $display("(time: %0d) %s: --------", $time, name);

        if (det_o_rdata !== exp_o_p0_nb_rdata_Q) begin
           $display("ERROR:  (time: %0d)  p0 nb rdata output error", $time);
           $assertoff;
           $finish;
        end
    endfunction

    function check_p0_sb_rdata_notification(mby_msh_pkg::msh_data_t det_o_rdata);
        exp_o_p0_sb_rdata_Q = exp_o_p0_sb_rdata_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p0_sb_rdata=(%0h)", $time, name, exp_o_p0_sb_rdata_Q);
        $display("(time: %0d) %s: det_o_p0_sb_rdata=(%0h)", $time, name, det_o_rdata);
        $display("(time: %0d) %s: --------", $time, name);

        if (det_o_rdata !== exp_o_p0_sb_rdata_Q) begin
           $display("ERROR:  (time: %0d)  p0 sb rdata output error", $time);
           $assertoff;
           $finish;
        end
    endfunction



    function check_p1_eb_rdata_notification(mby_msh_pkg::msh_data_t det_o_rdata);
        exp_o_p1_eb_rdata_Q = exp_o_p1_eb_rdata_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p1_eb_rdata=(%0h)", $time, name, exp_o_p1_eb_rdata_Q);
        $display("(time: %0d) %s: det_o_p1_eb_rdata=(%0h)", $time, name, det_o_rdata);
        $display("(time: %0d) %s: --------", $time, name);

        if (det_o_rdata !== exp_o_p1_eb_rdata_Q) begin
           $display("ERROR:  (time: %0d)  p1 eb rdata output error", $time);
           $assertoff;
           $finish;
        end
    endfunction

    function check_p1_wb_rdata_notification(mby_msh_pkg::msh_data_t det_o_rdata);
        exp_o_p1_wb_rdata_Q = exp_o_p1_wb_rdata_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p1_wb_rdata=(%0h)", $time, name, exp_o_p1_wb_rdata_Q);
        $display("(time: %0d) %s: det_o_p1_wb_rdata=(%0h)", $time, name, det_o_rdata);
        $display("(time: %0d) %s: --------", $time, name);

        if (det_o_rdata !== exp_o_p1_wb_rdata_Q) begin
           $display("ERROR:  (time: %0d)  p1 wb rdata output error", $time);
           $assertoff;
           $finish;
        end
    endfunction

    function check_p1_nb_rdata_notification(mby_msh_pkg::msh_data_t det_o_rdata);
        exp_o_p1_nb_rdata_Q = exp_o_p1_nb_rdata_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p1_nb_rdata=(%0h)", $time, name, exp_o_p1_nb_rdata_Q);
        $display("(time: %0d) %s: det_o_p1_nb_rdata=(%0h)", $time, name, det_o_rdata);
        $display("(time: %0d) %s: --------", $time, name);

        if (det_o_rdata !== exp_o_p1_nb_rdata_Q) begin
           $display("ERROR:  (time: %0d)  p1 nb rdata output error", $time);
           $assertoff;
           $finish;
        end
    endfunction

    function check_p1_sb_rdata_notification(mby_msh_pkg::msh_data_t det_o_rdata);
        exp_o_p1_sb_rdata_Q = exp_o_p1_sb_rdata_fifo.pop_front();
        $display("(time: %0d) %s: exp_o_p1_sb_rdata=(%0h)", $time, name, exp_o_p1_sb_rdata_Q);
        $display("(time: %0d) %s: det_o_p1_sb_rdata=(%0h)", $time, name, det_o_rdata);
        $display("(time: %0d) %s: --------", $time, name);

        if (det_o_rdata !== exp_o_p1_sb_rdata_Q) begin
           $display("ERROR:  (time: %0d)  p1 sb rdata output error", $time);
           $assertoff;
           $finish;
        end
    endfunction






//-hz:
//  function arb_bid_notification(tmpl_pkg::enc_outp_t oport, tmpl_pkg::inp_t bids, tmpl_pkg::inp_t gnt);


endclass

`endif // SCOREBOARD_SV
