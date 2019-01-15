puts " Pre Analyze " 

set_message -warning VERI-9030

catch {set_message -warning ENL149}; # this message is relevant for 2017.12 only
set_message -warning VERI-9030
set_message -warning WCK006

