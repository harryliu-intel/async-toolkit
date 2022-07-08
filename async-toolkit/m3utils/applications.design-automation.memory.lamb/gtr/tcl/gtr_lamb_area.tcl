#
# Author : Mika Nystrom <mika.nystroem@intel.com>
# Author : Paul Donehue <paul.donehue@intel.com>
#
# This file consistently uses the following nomenclature:
#
# depth             - # of entries in a RAM array
# width             - # of bits in each entry of a RAM array
# X [NOT "width"]   - a physical dimension in the horizontal direction when
#                     viewing a physical design representation on
#                     a graphics display
# Y [NOT "height" ] - a physical dimension in the vertical direction when
#                     viewing a physical design representation on
#                     a graphics display
#
# N.B. depth of memory runs in X, width in Y !
#

proc gtr_lamb_area { args } {
    set proc_name [lindex [regsub "::" [info level 0]  "" ] 0 ]
    set date [date ]
    parse_proc_arguments -args $args arg
    if { [info exists arg(-v) ] } {
        set verbose 1
    } else {
        set verbose 0
    }

#    if { [info exists arg(-debug) ] } {
      echo "INFO: $proc_name, Starting, args is $args" 
#    }
    set depth $arg(-data_depth)
    set width $arg(-data_width)
    set tech_node $arg(-tech_node)

    if { $tech_node == "n3b" } {
        puts "WARNING: using \"n3b\" technology results in non-physical output dimensions.  Use with care!"
        set tech_node "n3";
    }

    set command [list $::env(GTR_HOME)/../python/pyarea/area.py -g -w ${width} -d ${depth} -T ${tech_node}]
    puts [list "COMMAND " $command ]
    set result [exec {*}$command]
    puts [ list "RESULT    " $result ]

    return $result
}

### Notes on proc handlest
# define_proc_attributes argHandler \
#    -info "Arguments processor" \
#    -define_args {
#        {-Oos "oos help"       AnOos   one_of_string {required value_help {values {a b}}}}
#        {-Int "int help"       AnInt   int     optional}
#        {-Float "float help"   AFloat  float   optional}
#        {-Bool "bool help"     ""      boolean optional}
#        {-String "string help" AString string  optional}
#        {-List "list help"     AList   list    optional}
#        {-IDup "int dup help"  AIDup   int     {optional merge_duplicates}}
#      } \
#    -define_arg_groups {
#       {exclusive {-Int -Float -Bool}}
# }
define_proc_attributes gtr_lamb_area \
    -info "Utility to generate LAMB Memory collaterals" \
    -define_args {
	{-tech_node "Specify tech node (default n3b)" "AnOos" one_of_string {required {values {"n3b" "n3e" "n5"}}}}
	{-data_depth "Depth of the memory in words/entries(layout x direction)" "int" int required}
	{-data_width "Data bus width of the memory in bits(layout y direction)" "int" int required}
	{-dual_clocks "Specify if memory has dual async clocks" "" boolean optional}
	{-verbose "Verbose Reporting" "" boolean optional}
	{-debug "Report additional logging for debug purposes" "" boolean optional}
    }
