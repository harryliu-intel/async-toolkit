set fileID [dump -file design.vpd]

dump -add { mby_tb } -depth 0 -scope "."

call {$vcdplusmemon}

# This code allows waves to be dumped every 600sec
dump -autoflush off


