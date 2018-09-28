#!/usr/intel/bin/tclsh8.5 -f


source  $::env(COLLAGE_ROOT)/utils/message_utils.tcl


#################################################################
# 
#################################################################
#
# this array store the message for collage.
# severity: [ FATAL  ERROR  WARN  INFO ]
#

#Different Error code buckets
### Design checks IP, HIER and RefDesign 0-100
### Clock checks SOC and IP 100-200
### CCUgen checks 200-300
### Clock stamping checks 300-400
### Clock connectivity checks 400-500
### Misc checks Spreadsheet checks,etc 900-1000

# Design checks IP, HIER and RefDesign 0-100
set ::collage_msg_handler::msg_array(CVAL-000) [list "ERROR" "IP does not exist in the SOC. " "" ]
set ::collage_msg_handler::msg_array(CVAL-001) [list "ERROR" "Multiple INOUT drivers found for driver. Skipping this connectivity. " ""]
set ::collage_msg_handler::msg_array(CVAL-002) [list "ERROR" "Multiple drivers found for receiver. Skipping this connectivity !. " ""]
set ::collage_msg_handler::msg_array(CVAL-003) [list "WARN" "IP is not an object of obv_hier_inst class, hence we cannot write sdc file for IP. " ""]
set ::collage_msg_handler::msg_array(CVAL-004) [list "WARN" "PIN found in ConnectedPinNames attribute is not a Pin. " ""]
set ::collage_msg_handler::msg_array(CVAL-005) [list "WARN" "PORT found in ConnectedPortNames attribute is not a Port. " ""]
set ::collage_msg_handler::msg_array(CVAL-006) [list "WARN" "Skipping interface of design. " ""]
set ::collage_msg_handler::msg_array(CVAL-007) [list "WARN" "DESIGN is not an object of obv_ip_inst class, hence we cannot write sdc file for DESIGN. " ""]
set ::collage_msg_handler::msg_array(CVAL-008) [list "WARN" "DESIGN is not an object of obv_ref_design class, hence we cannot write sdc file for DESIGN. " ""]
set ::collage_msg_handler::msg_array(CVAL-009) [list "WARN" "Obviate already initialized. " ""]
set ::collage_msg_handler::msg_array(CVAL-010) [list "WARN" "Total hiers without any interfaces. " ""]
set ::collage_msg_handler::msg_array(CVAL-011) [list "WARN" "Total hiers without any pins. " ""]
set ::collage_msg_handler::msg_array(CVAL-012) [list "WARN" "Total ips without any interfaces. " ""]
set ::collage_msg_handler::msg_array(CVAL-013) [list "WARN" "Total ips without any pins. " ""]
set ::collage_msg_handler::msg_array(CVAL-014) [list "WARN" "Total ips without any sbr clocks. " ""]
set ::collage_msg_handler::msg_array(CVAL-015) [list "ERROR" "Unknown pin direction  specified. Only supported values are {in out inout}. " ""]
set ::collage_msg_handler::msg_array(CVAL-016) [list "INFO" "Pin is a port. " ""]
set ::collage_msg_handler::msg_array(CVAL-017) [list "ERROR" "Clock pin does not exist on Design. " ""]
set ::collage_msg_handler::msg_array(CVAL-018) [list "ERROR" "Pin does not exist on Design" "IO delay will not be set on this pin"]
set ::collage_msg_handler::msg_array(CVAL-019) [list "WARN" "DESIGN is not an object of obv_hier_inst class, hence we cannot write sdc file for DESIGN. " ""]
set ::collage_msg_handler::msg_array(CVAL-020) [list "WARN" "REF DESIGN doesnot have any IP instances associated with it. " ""]
set ::collage_msg_handler::msg_array(CVAL-021) [list "ERROR" "REF DESIGN doesnot exist in SOC. " ""]
set ::collage_msg_handler::msg_array(CVAL-022) [list "INFO" "MASTER REF DEIGN has more than one design objects. " ""]
set ::collage_msg_handler::msg_array(CVAL-023) [list "ERROR" "PIN does not exist on REF DESIGN. " ""]
set ::collage_msg_handler::msg_array(CVAL-024) [list "ERROR" "There are no design objects with REF DESIGN name. " ""]
set ::collage_msg_handler::msg_array(CVAL-025) [list "INFO" "Validating clock tagging for REF DESIGN. " ""]
set ::collage_msg_handler::msg_array(CVAL-026) [list "INFO" "Validating clock tagging on CHILD REF DESIGN. " ""]
set ::collage_msg_handler::msg_array(CVAL-027) [list "ERROR" "There is no hier object correspoding to the hierarchy. " ""]
set ::collage_msg_handler::msg_array(CVAL-028) [list "ERROR" "Partition pin part of Par<-->IP pin map doesn't exist on hierarchy. " ""]
set ::collage_msg_handler::msg_array(CVAL-029) [list "ERROR" "Partition pin hierarchy doesn't match with hierarchal object. " ""]


# Clock checks SOC and IP 100-200
set ::collage_msg_handler::msg_array(CVAL-100) [list "ERROR" "Pin of IP CLOCK is not connected to any SOC clock. " ""]
set ::collage_msg_handler::msg_array(CVAL-101) [list "ERROR" "Pin belongs to more than 1 SOC clock. " ""]
set ::collage_msg_handler::msg_array(CVAL-102) [list "ERROR" "Pin of IP is not clock stamped where as there is an IP clock defined in the Clock spec on this pin. " ""]
set ::collage_msg_handler::msg_array(CVAL-103) [list "ERROR" "Pin of IP clock does not exist on IP. " ""]
set ::collage_msg_handler::msg_array(CVAL-104) [list "ERROR" "SOC clock trunk has sink pins, but no source pins. " "" ]
set ::collage_msg_handler::msg_array(CVAL-105) [list "ERROR" "SOC clock sink pins are not assigned to any trunks. " "" ]
set ::collage_msg_handler::msg_array(CVAL-106) [list "ERROR" "Req ack Interface specified on SOC CLK for IP is already assigned to IP and interface will not be added to IP. " ""]
set ::collage_msg_handler::msg_array(CVAL-107) [list "ERROR" "IP is already assigned to trunk , so this IP cannot be assigned to trunk. " "" ]
set ::collage_msg_handler::msg_array(CVAL-108) [list "ERROR" "SOC clock trunk has source pins but no sink pins. " "" ]
set ::collage_msg_handler::msg_array(CVAL-109) [list "ERROR" "Master clock source pin of clock doesn't exist on hierarchy . Hence we cannot create a generated clock. " ""]
set ::collage_msg_handler::msg_array(CVAL-110) [list "ERROR" "Max freq relationship doesn't match Min freq relationship. " ""] 
set ::collage_msg_handler::msg_array(CVAL-111) [list "ERROR" "To be filled. " ""]
set ::collage_msg_handler::msg_array(CVAL-112) [list "ERROR" "Port width of child and master mismatch on the IP. " ""]
set ::collage_msg_handler::msg_array(CVAL-113) [list "ERROR" "Child is a Port where as MASTERCLK_SOURCE_PIN is not a port. How do we map MASTERCLK_SOURCE_PIN  port to pin for IP. This clock constraint will be dropped. " ""]
set ::collage_msg_handler::msg_array(CVAL-114) [list "ERROR" "There are multiple root_ip's for the soc clock. " ""]  
set ::collage_msg_handler::msg_array(CVAL-115) [list "ERROR" "SOC clock doesnot exist on design. " ""]
set ::collage_msg_handler::msg_array(CVAL-116) [list "WARN" "Trunk empty. " ""]
set ::collage_msg_handler::msg_array(CVAL-117) [list "WARN" "No pin has been provided, please provide pins as a list of \"IP_name1/pin_name IP_name2/pin_name\". " "ex: czclk configure -intermediate_pins \"moddfx/clk1 parcdu/clk2\""]
set ::collage_msg_handler::msg_array(CVAL-118) [list "WARN" "Port has MASTERCLK_SOURCE_PIN which is not a port. " ""]
set ::collage_msg_handler::msg_array(CVAL-119) [list "WARN" "Variable clock port oesn't exist on IP hence no IP clock will be created for this PIN. " ""]
set ::collage_msg_handler::msg_array(CVAL-120) [list "WARN" "To be filled."  ""]
set ::collage_msg_handler::msg_array(CVAL-121) [list "INFO" "Pin of SOC clock doesn't have IP name prepended, please make sure this pin exists at top level. " ""]
set ::collage_msg_handler::msg_array(CVAL-122) [list "ERROR" "Pin of SOC clock does not exist on IP. " ""]
set ::collage_msg_handler::msg_array(CVAL-123) [list "FATAL" "Required argument is not set while creating clock object, hence clock object will not be created. " ""]
set ::collage_msg_handler::msg_array(CVAL-124) [list "FATAL" "Clock sink pins donot exist on IP for SOC clock. " ""]
set ::collage_msg_handler::msg_array(CVAL-125) [list "FATAL" "" ""] 
set ::collage_msg_handler::msg_array(CVAL-126) [list "ERROR" "Cannot trace the source ip for SOC clock hence cannot check if the REQ ACK interface provided is of type CLOCK_REQ_ACK. " ""] 
set ::collage_msg_handler::msg_array(CVAL-127) [list "FATAL" "Only frequency units \"k\", \"m\", \"g\", supported. " ""] 
set ::collage_msg_handler::msg_array(CVAL-128) [list "FATAL" "Malformed frequency specification for freq for clock. " ""]
set ::collage_msg_handler::msg_array(CVAL-129) [list "ERROR" "Sink Pin of SOC clock is already part of SOURCE pins. " ""]
set ::collage_msg_handler::msg_array(CVAL-130) [list "ERROR" "Source Pin of SOC clock is already part of Sink pins. " ""]
set ::collage_msg_handler::msg_array(CVAL-131) [list "ERROR" "Source Pin of SOC clock is an empty string. " ""]
set ::collage_msg_handler::msg_array(CVAL-132) [list "ERROR" "Source Pin of SOC clock is an empty string. " ""]
set ::collage_msg_handler::msg_array(CVAL-133) [list "ERROR" "IP CLOCK is not tagged. " ""]
set ::collage_msg_handler::msg_array(CVAL-134) [list "FATAL" "Multiple internal pins specified as source pins for the boundary pins.Please specifyone internal pin for a list of boundary output pins per line.  " ""]
set ::collage_msg_handler::msg_array(CVAL-135) [list "ERROR" "IP clock already exists in the database. Ignoring the redundant command. " ""]
set ::collage_msg_handler::msg_array(CVAL-136) [list "ERROR" "Sink pin doesnot belong to any IP's. " ""]
set ::collage_msg_handler::msg_array(CVAL-137) [list "ERROR" "Net connected to Sink pin is tied off. " ""]
set ::collage_msg_handler::msg_array(CVAL-138) [list "INFO" "Internal Pin of IP clock cannot be validated on IP. " ""]

# Clock stamping checks 200-300
set ::collage_msg_handler::msg_array(CVAL-200) [list "ERROR" "No -source_pin found for clock. Hence no generated clock will be created on hierarchy. " ""]
set ::collage_msg_handler::msg_array(CVAL-201) [list "ERROR" "None of the source pins of master clock fall in the hierarchy as the source pins of child clock. No generated clock will be created. " ""]
set ::collage_msg_handler::msg_array(CVAL-202) [list "INFO" "Stamping clock:. " ""]
set ::collage_msg_handler::msg_array(CVAL-203) [list "INFO" "Virtual Clocks will be ignored when rolling up clocks. " ""]
set ::collage_msg_handler::msg_array(CVAL-204) [list "INFO" "Ignoring pin as this is not on an IP. " ""]
set ::collage_msg_handler::msg_array(CVAL-205) [list "INFO" "Processing connected pin of clock  of source pin. " ""]
set ::collage_msg_handler::msg_array(CVAL-206) [list "INFO" "Cannot extract IP core kit data for MODULE as there are no instances defined for the following design. " ""]
set ::collage_msg_handler::msg_array(CVAL-207) [list "INFO" "Tracing clock at hierarchy with name using connectivity of pin. " ""]
set ::collage_msg_handler::msg_array(CVAL-208) [list "INFO" "Previous definition of clock is being overwritten. " ""]
set ::collage_msg_handler::msg_array(CVAL-209) [list "ERROR" "Parent pin not found for SOC clock at hierarchy. Hence no generated clock will be created. " ""]
set ::collage_msg_handler::msg_array(CVAL-210) [list "WARN" "SOC Clock has multiple source pins on the hierarchy, we will trace again to find the correct master clock pin."  ""]
set ::collage_msg_handler::msg_array(CVAL-211) [list "ERROR" "Auto generated sink file doesn't exist for the architectural clocks specified as input. "  ""]
set ::collage_msg_handler::msg_array(CVAL-212) [list "WARN" "There are no generated clocks found in the design. Uniquification is applicable to designs that have generated clocks. "  ""]
set ::collage_msg_handler::msg_array(CVAL-213) [list "INFO" "Generated SOC clock has more than one source pin and will be uniquified. "  ""]
set ::collage_msg_handler::msg_array(CVAL-214) [list "ERROR" "Generated SOC clock will not be uniquified because it has req-ack interfaces and we do not know how to distribute these req-ack interfaces when we uniquify the generated SOC clock. "  ""]
set ::collage_msg_handler::msg_array(CVAL-215) [list "ERROR" "Generated SOC clock has more than one sink pin and we do not know how to distribute these sinks pins when we uniquify SOC clock. "  ""]
set ::collage_msg_handler::msg_array(CVAL-216) [list "ERROR" "Generated SOC clock is not being uniquified due to previously encountered errors. "  ""]
set ::collage_msg_handler::msg_array(CVAL-217) [list "ERROR" "Multiple INOUT drivers found for driver.This could mess up clock stamping. " ""]
set ::collage_msg_handler::msg_array(CVAL-218) [list "ERROR" "Multiple drivers found for receiver.This could mess up clock stamping. " ""]
set ::collage_msg_handler::msg_array(CVAL-219) [list "ERROR" "No receivers found.This could mess up clock stamping. " ""]
set ::collage_msg_handler::msg_array(CVAL-220) [list "ERROR" "No drivers found.This could mess up clock stamping. " ""]
set ::collage_msg_handler::msg_array(CVAL-221) [list "ERROR" "Multiple INOUT drivers found for driver with no recievers. Updating the inout direction to in. This could mess up clock stamping. " ""]


# CCUgen checks 300-400
set ::collage_msg_handler::msg_array(CVAL-300) [list "ERROR" "Number of CCU and IP ifcs mismatch. " ""]
set ::collage_msg_handler::msg_array(CVAL-301) [list "FATAL" "No SOC clocks found to generate the CCX IP. Please read clock specs before running this. " ""]


# Clock connectivity checks 400-500


# Misc checks Spreadsheet checks,etc 900-1000
set ::collage_msg_handler::msg_array(CVAL-900) [list "ERROR" "Something is wrong with the input. Input length should be a multiple of 3. " ""]
set ::collage_msg_handler::msg_array(CVAL-901) [list "ERROR"  "Only {y, n} values allowed for Column. " ""]
set ::collage_msg_handler::msg_array(CVAL-902) [list "WARN"  "Multiple -master_clk_source_pins specified for IP clock. " ""]
set ::collage_msg_handler::msg_array(CVAL-903) [list "ERROR"  "Internal error: Multiple -post_reset_root_clocks not supported yet !. " ""]
set ::collage_msg_handler::msg_array(CVAL-904) [list "ERROR"  "File not found. " ""]
set ::collage_msg_handler::msg_array(CVAL-905) [list "ERROR"  "No data found in row . " ""]
set ::collage_msg_handler::msg_array(CVAL-906) [list "ERROR"  "Architecture clock has multiple \"SoC ClkGen Instance Name\" defined. " ""]
set ::collage_msg_handler::msg_array(CVAL-907) [list "ERROR"  "Architecture clock  previously defined value does not match with what was found in row. Only the first definition used. " ""]
set ::collage_msg_handler::msg_array(CVAL-908) [list "ERROR"   "Architecture clock name expected in row, no SOC clock will be created for this row entry. " ""]
set ::collage_msg_handler::msg_array(CVAL-909) [list "ERROR"   "SOC clock does not have source pin defined !. " ""]
set ::collage_msg_handler::msg_array(CVAL-910) [list "ERROR"   "Value for \"SoC Frequency Guardband\" , has to be a number. " ""]
set ::collage_msg_handler::msg_array(CVAL-911) [list "ERROR"   "\"IP Clock Max Frequency\" must be specified for every IP sink. " ""]
set ::collage_msg_handler::msg_array(CVAL-912) [list "INFO" "Sourcing clock core kit file. " ""]
set ::collage_msg_handler::msg_array(CVAL-913) [list "WARN" "File specified  already exists. Moving the file to file_name.OLD\n. " ""]
set ::collage_msg_handler::msg_array(CVAL-914) [list "INFO" "" ""]
set ::collage_msg_handler::msg_array(CVAL-915) [list "WARN" "" ""]
set ::collage_msg_handler::msg_array(CVAL-916) [list "ERROR" "" ""]
set ::collage_msg_handler::msg_array(CVAL-917) [list "FATAL" "" ""]
set ::collage_msg_handler::msg_array(CVAL-DEBUG) [list "INFO" "DEBUG. " ""]
set ::collage_msg_handler::msg_array(CVAL-918) [list "FATAL" "SOC clock specified in ip.arch_clks file doesnot exist on design. " ""]
set ::collage_msg_handler::msg_array(CVAL-919) [list "FATAL" "Unknown keyword specified in ip.arch_clks file for CLK.Allowed key words are SOURCE and SINK. " ""]
set ::collage_msg_handler::msg_array(CVAL-920) [list "FATAL" "Unknown keyword specified in ip.arch_clks file. Allowed key words are CLK and V. " ""]
set ::collage_msg_handler::msg_array(CVAL-921) [list "FATAL" "Soc clock spreadsheet doesn't exist. We can't proceed without this information, please provide the soc clock xls. " ""]
set ::collage_msg_handler::msg_array(CVAL-922) [list "FATAL" "Soc clock is a generated clock, the frequency factor is not an integer, hence Edges and Edge Shift are required values for this clock. " ""]
set ::collage_msg_handler::msg_array(CVAL-923) [list "ERROR" "Soc clock is not a generated clock, Edges and Edge Shift are not applicable for the root clock. " ""]

#Debug is being suppressed
collage_message_suppress CVAL-DEBUG
