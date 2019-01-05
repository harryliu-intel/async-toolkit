`ifndef REQUEST_RECORD_SV
`define REQUEST_RECORD_SV


class request_record;

    integer                 id;
    integer                 req_in_time;
    tmpl_pkg::req_in_t      req_in;
    integer                 arb_win_time;
    integer                 num_bids;
    tmpl_pkg::inp_t         losers;
    integer                 req_out_time;
    tmpl_pkg::req_out_t     req_out;

    string                  name;

    function new();
        name           = "request_record.sv";
        id             = -1;
        req_in_time    = -1;
        req_in         = '0;
        arb_win_time   = -1;
        num_bids       = 0;
        losers         = '0;
        req_out_time   = -1;
        req_out        = '0;
    endfunction

    task print(string prepend);
        $display("%s\tREQUEST RECORD (id = %0d):",  prepend, id);
        $display("%s\t\treq_in_time     = %0d",     prepend, req_in_time);
        $display("%s\t\treq_in.vld      = %0d",     prepend, req_in.vld);
        $display("%s\t\treq_in.outp     = %0d",     prepend, req_in.outp);
        $display("%s\t\treq_in.data     = %0d",     prepend, req_in.data);
        $display("%s\t\tarb_win_time    = %0d",     prepend, arb_win_time);
        $display("%s\t\tnum_bids        = %0d",     prepend, num_bids);
        $display("%s\t\tlosers          = 0x%0x",   prepend, losers);
        $display("%s\t\treq_out_time    = %0d",     prepend, req_out_time);
        $display("%s\t\treq_out.vld     = %0d",     prepend, req_out.vld);
        $display("%s\t\treq_out.inp     = %0d",     prepend, req_out.inp);
        $display("%s\t\treq_out.data    = %0d",     prepend, req_out.data);
    endtask

endclass

`endif // REQUEST_RECORD_SV
