puts " Pre Analyze " 

set_message -warning VERI-9030

# Set GUI to AFL App
set_current_gui afl

# Initalize AFL App
check_afl -init

check_afl -configure -disable all

# Enable checks for design errors
#check_afl -configure -enable case_assertions
#check_afl -configure -enable default_case
#check_afl -configure -enable floating_bus
#check_afl -configure -enable contention_bus
check_afl -configure -enable out_of_bound_indexing
check_afl -configure -enable arithmetic_overflow
#check_afl -configure -enable x_assignment
check_afl -configure -enable fsm_deadlock
#check_afl -configure -enable fsm_livelock

# Enable checks for coverage
#check_afl -configure -enable stuckat_signals
#check_afl -configure -enable toggle_rise_signals
#check_afl -configure -enable toggle_fall_signals
#check_afl -configure -enable toggle_stable_signals
check_afl -configure -enable fsm_reachable_states
check_afl -configure -enable fsm_reachable_transitions
#check_afl -configure -enable fsm_reachable_multi_transitions
check_afl -configure -enable dead_code
