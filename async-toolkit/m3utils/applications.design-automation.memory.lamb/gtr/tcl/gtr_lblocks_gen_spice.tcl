# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


#### gtr_gen_lamb_lblocks <ofile>
#### Generates a flat schematic netlist for an xN latch
proc gtr_lblocks_gen_spice { ofile } {
    set prefix BXDHDBSVT06
    set of [open $ofile w ]
    foreach nbits "4 5 8 9 16 17 32 33" {
	puts -nonewline $of ".subckt ${prefix}LB${nbits}_1 VDD VSS VBP VBN X "
	for { set i 0 } { $i < $nbits } { incr i } {
	    puts -nonewline $of "WWL${i} WWLN${i} RWL${i} RWLN${i} "
	}
	puts $of "DN"
	for { set i 0 } { $i < $nbits } { incr i } {
	    puts $of "**** Latch $i"
	    puts $of "**   Input Tristate"
	    puts $of "MPTXI${i} LFB${i} WWLN${i} DN VBP pch_svt_mac W=0.034u L=0.006u M=1 nfin=2 PPITCH=0 FBOUND=9"
	    puts $of "MNTXI${i} LFB${i} WWL${i} DN VBN nch_svt_mac W=0.034u L=0.006u M=1 nfin=2 PPITCH=0 FBOUND=9\n"
	    puts $of "** Feed Forward Inverter"
	    puts $of "MPFF${i} Q${i} LFB${i} VDD VBP pch_svt_mac W=0.034u L=0.006u M=1 nfin=2 PPITCH=0 FBOUND=9"
	    puts $of "MNFF${i} Q${i} LFB${i} VSS VBN nch_svt_mac W=0.034u L=0.006u M=1 nfin=2 PPITCH=0 FBOUND=9"
	    puts $of "** Feedback tristate inverters"
	    puts $of "MPFBS${i} TSP${i} Q${i} VDD VBP pch_svt_mac W=0.034u L=0.006u M=1 nfin=2 PPITCH=0 FBOUND=9"
	    puts $of "MPFBO${i} LFB${i} WWL${i} TSP${i} VBP pch_svt_mac W=0.034u L=0.006u M=1 nfin=2 PPITCH=0 FBOUND=9"
	    puts $of "MNFBO${i} LFB${i} WWLN${i} TSN${i} VBN nch_svt_mac W=0.034u L=0.006u M=1 nfin=2 PPITCH=0 FBOUND=9"
	    puts $of "MNFBS${i} TSN${i} Q${i} VSS VBN nch_svt_mac W=0.034u L=0.006u M=1 nfin=2 PPITCH=0 FBOUND=9"
	    puts $of "** Output inverter"
	    puts $of "MPTXO${i} Q${i} RWLN${i} X VBP pch_svt_mac W=0.034u L=0.006u M=1 nfin=2 PPITCH=0 FBOUND=9"
	    puts $of "MNTXO${i} Q${i} RWL${i} X VBN nch_svt_mac W=0.034u L=0.006u M=1 nfin=2 PPITCH=0 FBOUND=9\n"

	}
	puts $of ".ends\n"
	## TBD add PD support
    }
    close $of
}

proc gtr_lblocks_gen_spice_hier { ofile } {
    set prefix BXDHDBSVT06
    set of [open $ofile w ]
    foreach flip_type "R0 MX" {
	foreach nbits "4 5 8 9 16 17 32 33" {
	    puts -nonewline $of ".subckt ${prefix}_LB${nbits}_${flip_type}_1 "
	    for { set i 0 } { $i < $nbits } { incr i } {
		puts -nonewline $of "WWL${i} WWLN${i} RWL${i} RWLN${i} "
	    }
	    puts $of "DN PD"
	    set is_odd [expr $nbits % 2 ]
	    set msb [expr $nbits - 1 ]
	    set inst_index -1
	    for { set i 0 } { $i < $nbits } { incr i 2 } {
		set iplus [expr $i + 1 ]
		set msb [expr $nbits - 1 ]
		if { $is_odd && $i == $msb  } {
		    puts -nonewline $of "X${prefix}_LDPQM1_UCPD_1 VDD VSS VBP VBN X "
		    puts -nonewline $of "WWL${msb} WWLN${msb} RWL${msb} RWLN${msb} "
		    puts $of "DN PD ${prefix}_LDPQM2_UCPD_1"
		} else {
		    puts -nonewline $of "X${prefix}_LDPQM2_UCA_1_[incr inst_index ] VDD VSS VBP VBN X "
		    if { $flip_type == "R0" } {
			puts -nonewline $of "WWL${i} WWLN${i} RWL${i} RWLN${i} "
			puts -nonewline $of "WWL${iplus} WWLN${iplus} RWL${iplus} RWLN${iplus} "
		    } elseif { $flip_type == "MX" } {
			puts -nonewline $of "WWL${iplus} WWLN${iplus} RWL${iplus} RWLN${iplus} "
			puts -nonewline $of "WWL${i} WWLN${i} RWL${i} RWLN${i} "
		    }
		    puts $of "DN ${prefix}_LDPQM2_UCA_1"
		}
	    }
	    if { ! $is_odd } {
		puts $of "X${prefix}_LDPQM1_PD_1 VDD VSS VBP VBN X PD ${prefix}_LDPQM2_PD_1"
	    }
	    
	    puts $of ".ends\n"
	}
    }
    close $of
}
