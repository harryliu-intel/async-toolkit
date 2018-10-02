#override port width mismatches etc. in this file
#uncomment this to enable collage to report all errors at once.
#DO NOT TURNIN with ContinueOnError true.
###set_activity_parameter ConfigureComponents ContinueOnError true

#examples
#set inst_name psf0
#collage_eval_in_component [::col::get_ip_hier_par $inst_name] {
#    collage_set_interface_port_attribute -instance $inst_name/group1_port1 MDEST_ID InterfaceLink pg1_fab_p1_mdest_id[7:0]
#    collage_set_interface_port_attribute -instance $inst_name/group1_port1 REQ_DEST_ID InterfaceLink pg1_fab_p1_req_dest_id[7:0]
#}
#
#set inst_name lcbx16_mm_0
#  collage_eval_in_component [::col::get_ip_hier_par $inst_name] {
#    for {set i 0} {$i < 16} {incr i} {
#      collage_set_interface_port_attribute -instance $inst_name/pcie_phy_pipe_$i PowerDown InterfaceLink "<open>"
#    }
#  }
#set inst_name ddrss
#  collage_eval_in_component [::col::get_ip_hier_par $inst_name] {
#      collage_set_interface_parameter_attribute -instance $inst_name/bms_pmi_0  ADDRESS_WIDTH Value 31
#      collage_set_interface_port_attribute -instance $inst_name/bms_pmi_0  REQUEST_ADDRESS InterfaceLink i_pmi0_request_address_0[30:0]
#  }

