
#
# Based on utility from Mika
# Author : Mika Nystrom <mika.nystroem@intel.com>
# Author : Paul Donehue <paul.donehue@intel.com>
#

proc gtr_lamb_power { args } {

    parse_proc_arguments -args $args arg
    if { [info exists arg(-verbose) ] } {
        set verbose 1
    } else {
        set verbose 0
    }
    set depth $arg(-data_depth)
    set width $arg(-data_width)
    # in uW /pJ per op
    switch $arg(-power_type) {
	"idle" {
	    ## lib units nW, Mika's plugin returns mW
	    set pow [expr ((1.4377040768018842e-18 * 1.0) + (2.5463572894612565e-17 * $depth) + (2.364608714114444e-17 * $width) + (4.665926896606388e-19 * $depth * $width))/1e-12]

	}
	"rd" {
	    set pow [expr ((9.542352013610499e-18 * 1.0) + (3.681341961988743e-17 * $depth) + (1.0423543116201884e-15 * $width) + (4.483009783994099e-18 * $depth * $width))/1e-12]
	}
	"wr" {
	    set pow [expr ((2.0000046202314615e-17 * 1.0) + (1.2449449740646995e-16 * $depth) + (1.85352980060391e-15 * $width) + (4.294009555315204e-17 * $depth * $width))/1e-12]
	}
	"rw" {
	    set pow [expr ((3.1028165776383104e-17 * 1.0) + (2.5231585522359304e-16 * $depth) + (2.9507202894979645e-15 * $width) + (4.3516303608623716e-17 * $depth * $width))/1e-12]
	}
	"leak" {
	    set pow_uw [expr ((0 * 1.0) + (0 * $depth) + (0 * $width) + (1.5814590719879427e-9 * $depth * $width))/1e-6]
	    set pow [expr $pow_uw * 1000 ]
	}
    }
    return $pow
}

define_proc_attributes gtr_lamb_power \
    -info "Utility to generate LAMB Memory collaterals" \
    -define_args {
	{-data_depth "Depth of the memory in words/entries(layout x direction)" "int" int required}
        {-data_width "Data bus width of the memory in bits(layout y direction)" "int" int required}
        {-power_type "Type of power to return (leak, idle, rd, wr, rw)" "oos" one_of_string {required value_help {values {leak idle rd wr rw}}}}
        {-dual_clocks "Specify if memory has dual async clocks" "" boolean optional}
        {-verbose "Verbose Reporting" "" boolean optional}
}
