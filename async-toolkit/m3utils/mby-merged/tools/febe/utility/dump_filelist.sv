module dump_filelist;                                                                                                                                   

  // To dump out rtllist for DUT and each partition
  initial begin
    if ($test$plusargs("dump_filelist")) begin
          $display("DUMPING FILELISTS");
    end
  end

`include "std_ace_util.vic"
initial begin
  dump_hier();
end

endmodule

