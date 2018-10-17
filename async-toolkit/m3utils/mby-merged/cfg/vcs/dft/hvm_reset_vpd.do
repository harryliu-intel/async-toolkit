# HVM Reset VPD file

# Dump all partition top levels
dump -add { top } -depth 3 -scope "."

# Dump PMC
#dump -add { top.pch.parleg    } -depth 0 -scope "."

# Dump central DFX logic
#dump -add { top.pch.pardfx    } -depth 0 -scope "."

# Dump CLTAP
#dump -add { top.pch.parxhcia.parxhcia_pwell_wrapper.cltap_wrapper1    } -depth 0 -scope "."

# Dump clocks
#dump -add { top.pch.paricc    } -depth 0 -scope "."

#dump -add { top.pch.lbgns_io_wrapper }  -depth 0 -scope "."

# WM COMs
#dump -add { top.pch.parfia20.parfia20_pwell_wrapper.hip_wm20.com0  } -depth 0 -scope "."
#dump -add { top.pch.parfia20.parfia20_pwell_wrapper.hip_wm20.com1  } -depth 0 -scope "."
#dump -add { top.pch.parfia.parfia_pwell_wrapper.hip_wm26.com0  } -depth 0 -scope "."
#dump -add { top.pch.parfia.parfia_pwell_wrapper.hip_wm26.com1  } -depth 0 -scope "."
#dump -add { top.pch.parfia.parfia_pwell_wrapper.hip_wm26.com2  } -depth 0 -scope "."

# Turn on deltaCycle
#dump -deltaCycle on

# This code allows waves to be dumped every 600sec
dump -flush
dump -autoflush off
dump -interval 600

