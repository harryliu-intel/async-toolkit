
######################################################################################
# Perform checks that must be completed in order for GK script to allow turnin

# verify no latches
if { [llength [get_design_info -list latch]] != 0 } { error "Unexpected Latches Found!" }

# verifies no combo timing loop.  Does more than lintra because timing loops might occur due to lack of proper FPV constraints
prove 1

# get_design_info outputs a string that is checked for by the GK turnin script in order to give the OK for turnin
get_design_info

######################################################################################



