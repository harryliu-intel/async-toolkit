# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

proc escape_name {name} {
    set oldhier [split $name /]
    set newhier {}
    foreach hier $oldhier {
        if {[string match {*[.\[\]*} $hier]} {
            set hier "\\$hier "
        }
        lappend newhier $hier
    }
    return [join $newhier /]
}

# workaround for bug 28948
proc disable_state1_tcheck {instfile} {
    if {[catch {open $instfile} ch]} {
        error "Can't open state1 latch instances file: $instfile"
    } else {
        while {[gets $ch line] >= 0} {
            tcheck [escape_name $line] SETUPHOLD -msg -disable
        }
        close $ch
    }
}

# avoid timing checks on din when reading
proc disable_cmo_din_tcheck {instfile} {
    if {[catch {open $instfile} ch]} {
        error "Can't open cmo instances file: $instfile"
    } else {
        while {[gets $ch line] >= 0} {
            if {[regexp {^mem\d+x(\d+)$} [lindex $line 0] -> bits]} {
                set base [escape_name [lindex $line 1]]
                for {set bit 0} {$bit < $bits} {incr bit} {
                    tcheck "$base/din\[$bit\]" SETUPHOLD -msg -disable
                }
            } else {
                error "Unknown CMO type: [lindex $line 0]"
            }
        }
        close $ch
    }
}

# disable timing checks during reset
proc reset_tcheck {duration} {
    tcheck {TESTBENCH/x} all -msg -disable -r
    run $duration
    tcheck {TESTBENCH/x} all -msg -enable -r
}

# run SDF related workarounds
proc run_workarounds {block sdf_prefix} {
    foreach inst [search -scope TESTBENCH -instances -module $block] {
        scope $inst
        disable_state1_tcheck ${sdf_prefix}.state1.latches
        disable_cmo_din_tcheck ${sdf_prefix}.cmo.instances
        scope TESTBENCH
    }
}
