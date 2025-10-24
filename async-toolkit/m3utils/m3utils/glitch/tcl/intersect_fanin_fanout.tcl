# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

######################################################################################
######################################################################################
## $Purpose: to write out logic cone between flop based afifo reg and rdata reg 
## $DateTime: 2022/01/11 
## $Author: cantony 
######################################################################################

    if [file exist fp.txt] {
        exec mv fp.txt fp.old.txt
    }
    reportFootPrint -outfile fp.txt
proc get_cell_func {cell_name {lib_name default}} {
    
    set fpt [open fp.txt r]
    set file_data [read $fpt]
    close $fpt
    set data [split $file_data "\n"]
    set flag 0
    foreach line $data {
        if [regexp "^Library" $line] {
            set libn [lindex [split $line {: }] 1]
        }
        if {$flag !=0} {
            if {$libn == ${lib_name}} {
                return [string trim $line]
            } else {
		#echo "$libn $lib_name"
		#echo $line
                set func [string trim $line]
		#	echo $func $line
                set flag 0
                continue
            }
        }
        if [regexp ^$cell_name $line] {
            set flag 1
		#echo "flag $line"
        }
    }
    return $func
}


proc intersect_fanin_fanout {x y} {
	if {[get_ports -quiet $x] != ""} {
		set X [all_fanout -from [get_ports $x]]
	} else {
		set X [all_fanout -from [get_pins $x]]
	} 
	if {[get_ports -quiet $y] != ""} {
		set Y [all_fanin -to [get_ports $y]]
	} else {
		set Y [all_fanin -to [get_pins $y]]
	} 
set result [remove_from_collection -intersect $X $Y]
return $result
} 
set fh [open By_Ryan_instance_${DESIGN(module_name)}_afifo_to_rdata_logic.txt w]
set fh2 [open By_Ryan_instance_${DESIGN(module_name)}_afifo_rdata_logic_term_info_with_flops.txt w]
#
set fh11 [open /nfs/sc/disks/bfn_pd_cb_02/cantony/cantony.cb.B0/pd/afifo_flops/${DESIGN(module_name)}_instances_only.txt r]
while { [ gets $fh11 line] >= 0 } {
	echo $line
	echo "AFIFO INFO: [llength [dbGet top.insts.instTerms.name *${line}*mem*reg*Q*] ]" 
		set search_pattern "*${line}*mem*reg*Q*"
	set var_len [dbGet top.insts.instTerms.name *${line}*mem*reg*Q*] 
 
	if {$var_len == "0x0" } { 
		echo "AFIFO_ERROR: search pattern $search_pattern doesn't exist" 
		continue	
	}  
foreach mem_reg_Qpin [dbGet top.insts.instTerms.name $search_pattern] {
puts $fh "AFIFO MEM REG NAME: $mem_reg_Qpin " 
puts $fh2 "AFIFO MEM REG NAME: $mem_reg_Qpin "
	set endpoints [get_object_name [all_fanout -from $mem_reg_Qpin -endpoints_only]] 
puts $fh "FROM $mem_reg_Qpin TO $endpoints " 
	set rdata_reg_Dpin [lsearch -all -inline $endpoints *\/D*]
	
	intersect_fanin_fanout $mem_reg_Qpin $rdata_reg_Dpin
	
	foreach pin [get_object_name [intersect_fanin_fanout $mem_reg_Qpin $rdata_reg_Dpin]] {
	 set cell_type [dbGet [dbGet top.insts.instTerms.name $pin -p].inst.cell.name]
	 set input_pin [dbGet [dbGet top.insts.instTerms.name $pin -p].isInput]
	 if {$input_pin == "0" &&  ![regexp "\/QN" $pin] &&  ![regexp "\/Q" $pin] &&  ![regexp "\/D" $pin]} {
	 	puts $fh "\t ( $cell_type ) "
		if { [regexp "^TS.N7.*VT.*HOCP" $cell_type ]} {
	 	set log_eq "SRAM" 
		} else {
	 	set log_eq [get_cell_func $cell_type] 
		}
	 	puts $fh "\t $log_eq " 
		}
	 if {$input_pin == "0" } {
	 puts $fh "\t$pin  ->"
	 } else {
	 puts $fh "\t  $pin"
	 }
	 }
	puts $fh " "
        puts $fh "NEXT AFIFO "

### for writing out net section

        foreach pin [get_object_name [get_cells -of_objects [get_object_name [intersect_fanin_fanout $mem_reg_Qpin $rdata_reg_Dpin]]]] {
         set cell_type [dbGet [dbGet top.insts.name $pin -p].cell.name]
         set cell_name [dbGet top.insts.name $pin ]
                puts $fh2 "\t Cell Name :  $cell_name  "
                        regsub -all "\{|\}" $cell_name "" cell_name
                puts $fh2 "\t Cell Type :  $cell_type  "

		if { [regexp "^TS.N7.*VT.*HOCP" $cell_type ]} {
	 	set log_eq "SRAM" 
		} else {
	 	set log_eq [get_cell_func $cell_type] 
		}
                puts $fh2 "\t Logic Function: $log_eq " 
                foreach term [dbGet [dbGet top.insts.name $cell_name -p].instTerms.name] {
                        regsub -all "\{|\}" $term "" term 
                        set net [dbGet [dbGet top.insts.instTerms.name $term -p].net.name]
                        puts $fh2 "\t Pin: $term  Net: $net"
                        }  
                        puts $fh2 " "
         }
        puts $fh2 " "
        puts $fh2 "NEXT AFIFO "

}	
}
                        puts $fh "END OF OUTPUT"
                        puts $fh2 "END OF OUTPUT"
close $fh
close $fh2
close $fh11 
