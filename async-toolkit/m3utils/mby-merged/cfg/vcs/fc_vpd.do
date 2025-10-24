#run 500us
dump -add { fc_hdl_top} -depth 9 -scope "."
dump -add { fc_hvl_top}  -depth 8 -scope "."
#TODO temporary placeholder: dump -add { fc_hdl_top.dut.ebg_io_wrapper}  -depth 0 -scope "."

# This code allows waves to be dumped every 600sec 
dump -flush
dump -autoflush off
dump -interval 600
#run 
