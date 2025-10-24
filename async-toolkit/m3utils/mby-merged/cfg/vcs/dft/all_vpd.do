dump -add .

# This is to dump multi-dimentional arrays
dump -add / -aggregates

# This code allows waves to be dumped every 600sec
dump -flush
dump -autoflush off
dump -interval 600

