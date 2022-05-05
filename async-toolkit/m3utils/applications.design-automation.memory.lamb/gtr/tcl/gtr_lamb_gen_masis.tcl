##
## GTR Generator Tools Release v1.0.0
##
##------------------------------------------------------------------------------
##
## INTEL CONFIDENTIAL
##
## Copyright 2021 - 2022 Intel Corporation All Rights Reserved.
##
## The source code contained or described herein and all documents related
## to the source code ("Material") are owned by Intel Corporation or its
## suppliers or licensors. Title to the Material remains with Intel
## Corporation or its suppliers and licensors. The Material contains trade
## secrets and proprietary and confidential information of Intel or its
## suppliers and licensors. The Material is protected by worldwide copyright
## and trade secret laws and treaty provisions. No part of the Material may
## be used, copied, reproduced, modified, published, uploaded, posted,
## transmitted, distributed, or disclosed in any way without Intel's prior
## express written permission.
##
## No license under any patent, copyright, trade secret or other intellectual
## property right is granted to or conferred upon you by disclosure or
## delivery of the Materials, either expressly, by implication, inducement,
## estoppel or otherwise. Any license under such intellectual property rights
## must be express and approved by Intel in writing.
##
##------------------------------------------------------------------------------

# Generate MASIS Specification according to
# $PROJ_ARCHIVE/sn6xx000vpnnsmxnn000s/fe/a18/masis1.2.pdf

proc gtr_lamb_gen_masis { args } {    
   
   set proc_name [lindex [regsub "::" [info level 0]  "" ] 0 ]
   set date [date ]
   parse_proc_arguments -args $args arg
   if { [info exists arg(-verbose) ] } {
       set verbose 1
   } else {
       set verbose 0
   }
   
   set flowthrough $arg(-ftr_value)
   set dir mbist
   file mkdir $dir
   set block_name $arg(-block_name)
   set fname ${dir}/${block_name}.masis
   echo "INFO: $proc_name, Generating MASIS view: $fname, Flowthrough $flowthrough"
   
   if { [file exists $fname] } {
       puts "INFO: $proc_name, $fname already exists, deleting to create new content"
       file delete $fname
   }

   if { [info exists arg(-filelistVar) ] } {
      upvar $arg(-filelistVar) fileList
      set thisEntry [dict create]
      dict set thisEntry path $fname
      dict set thisEntry nda_protection_level front_end 
      dict set thisEntry standard_name mbist 
      dict set thisEntry type masis
      lappend fileList $thisEntry
   }
   
   set depth $arg(-data_depth)
   set width $arg(-data_width)
   set addr_width [expr int(ceil(log($depth)/log(2))) ]
   
   set of [open $fname w]
   
   puts $of "//------------------------------------------------------------------------------"
   puts $of "//"
   puts $of "// INTEL CONFIDENTIAL"
   puts $of "//"
   puts $of "// Copyright 2021 - 2021 Intel Corporation All Rights Reserved."
   puts $of "//"
   puts $of "// The source code contained or described herein and all documents related"
   puts $of "// to the source code (\"Material\") are owned by Intel Corporation or its"
   puts $of "// suppliers or licensors. Title to the Material remains with Intel"
   puts $of "// Corporation or its suppliers and licensors. The Material contains trade"
   puts $of "// secrets and proprietary and confidential information of Intel or its"
   puts $of "// suppliers and licensors. The Material is protected by worldwide copyright"
   puts $of "// and trade secret laws and treaty provisions. No part of the Material may"
   puts $of "// be used, copied, reproduced, modified, published, uploaded, posted,"
   puts $of "// transmitted, distributed, or disclosed in any way without Intel's prior"
   puts $of "// express written permission."
   puts $of "//"
   puts $of "// No license under any patent, copyright, trade secret or other intellectual"
   puts $of "// property right is granted to or conferred upon you by disclosure or"
   puts $of "// delivery of the Materials, either expressly, by implication, inducement,"
   puts $of "// estoppel or otherwise. Any license under such intellectual property rights"
   puts $of "// must be express and approved by Intel in writing."
   puts $of "//"
   puts $of "//------------------------------------------------------------------------------\n"

   puts $of "memory_name = \"$block_name\""
   puts $of "masis_version = \"masis_1.2\""

   puts $of "GeneralParameters \{"
	puts $of "  PortTypeNum \{"
	puts $of "    ReadPortNum = 1"
   puts $of "    WritePortNum = 1"
	puts $of "  \}"
   puts $of "  MemoryType = \"SRAM\""
	puts $of "  NumberOfBits = $width"
	puts $of "  NumberOfWords = $depth"
   puts $of "  AllowOutOfRange = False"
      
   if { $flowthrough } {
     puts $of "  ReadType = \"Asynchronous\""
   } else {
     puts $of "  ReadType = \"Synchronous\""
   }
	puts $of "\}"

   set ports { { clk Input Clock { {PortId "w r" }} } \
               { wen Input WriteEnable { { PortId w}} } \
               { ren Input ReadEnable { { PortId r}} } \
               { test__scan_en Input None { {TieLevel Wrapper } {SafeValue 1'b0 }} } \
               { dft__core_si Input None { {TieLevel Wrapper } {SafeValue 1'b0 }} } \
               { icg_force_on Input None { {TieLevel Wrapper } {SafeValue 1'b0 }} } \
               { dft_read_bypass Input None { {TieLevel Wrapper } {SafeValue 1'b0 }} } \
               { dft__mem_wr_disable Input None { {TieLevel Wrapper } {SafeValue 1'b0 }} } \
               { dft__core_so Output None } \
             }
   lappend ports [list wadr Input Address [list [list PortId w] [list Range \[[expr $addr_width - 1]:0\] ] ]]
   lappend ports [list radr Input Address [list [list PortId r] [list Range \[[expr $addr_width - 1]:0\] ] ]]
   lappend ports [list wdata Input Data   [list [list PortId w] [list Range \[[expr $width      - 1]:0\] ] ]]
   lappend ports [list dout Output Data   [list [list PortId r] [list Range \[[expr $width      - 1]:0\] ] ]]

   puts $of "Port \{"
   foreach p $ports {
     set port [lindex $p 0]
     set dir [lindex $p 1]
     set tag [lindex $p 2]
     set otherfields [lindex $p 3]

     puts $of " $port \{"
     puts $of "  Direction = $dir"
     puts $of "  PowerDomain = VDD"
     puts $of "  Ground = VSS"
     puts $of "  Tag = $tag"
     foreach o $otherfields {
       set field [lindex $o 0]
       set value [lindex $o 1]
       puts $of "  $field = \"$value\""        
     }
     puts $of " \}"
   }
   puts $of "\}"

   puts $of "PowerPorts \{"
   foreach p { { VDD MemorySupply } { VSS Ground } } {
     set port [lindex $p 0]
     set type [lindex $p 1] 
     puts $of " $port \{"
     puts $of "  Type = $type"
     puts $of " \}"
   }
   puts $of "\}"


   puts $of "AddressCounter \{"
   puts $of " RowAddressRange = \"\[0:[expr $depth-1]\]\""
   puts -nonewline $of " AddressScrambleMap = \""
   set sp ""
   for { set i [expr $addr_width - 1] } { $i >= 0 } { incr i -1} {
      puts -nonewline $of "${sp}RowAddress[${i}]"
      set sp " "
   }
   puts $of "\""
   puts $of "\}"

   puts $of "MemoryCoordinateOffset \{"
	puts $of " Top = 0.0"
	puts $of " Bottom = 0.0"
	puts $of " Left = 0.0"
	puts $of " Right = 0.0"
   puts $of "\}"
   close $of
}

define_proc_attributes gtr_lamb_gen_masis \
    -info "Utility to generate LAMB MASIS 1.2 definition" \
    -define_args {
      {-ftr_value "Specify flow-through or not" "int" int required}
      {-block_name "Specify memory block name" "String" string required}
      {-data_depth "Depth of the memory in words/entries(layout x direction)" "int" int required}
      {-data_width "Data bus width of the memory in bits(layout y direction)" "int" int required}
      {-dual_clocks "Specify if memory has dual async clocks" "" boolean optional}
      {-verbose "Verbose Reporting" "" boolean optional}
      {-filelistVar "Update filelist for manifest.xml" "" string optional}
}
