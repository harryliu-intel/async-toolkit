dump -add .
#dump -add top.pch -depth 1 -scope "."
#dump -add { top.pch   } -depth 0 -scope "."

#dump -add { top } -depth 2 -scope "."
#dump -add { top.pch.lbgns_io_wrapper }  -depth 0 -scope "."
#dump -add { top.pch.pardfx   } -depth 0 -scope "."
#dump -add { top.pch.parleg   } -depth 0 -scope "."
#dump -add { top.pch.paricc   } -depth 0 -scope "."
#dump -add { top.pch.parpsf2.parpsf2_pwell_wrapper } -depth 0 -scope "."
#dump -add { top.pch.parfuse  } -depth 0 -scope "."
#dump -add { top.pch.pardmi   } -depth 0 -scope "."
#dump -add { top.pch.parcsmeb } -depth 0 -scope "."

# This code allows waves to be dumped every 600sec
dump -flush
dump -autoflush off
dump -interval 600

