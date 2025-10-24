# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

## Add below variable to enable creation and substitution of map_power_switch command when not present
## in files provided by IP teams
##dont_preserve_gated_supplies 1

namespace eval ::upf_config { }
set ::upf_config::PS_CELL d04pws10lq8b0 

proc ::get_pins { args } {
  return [join $args]
}

proc ::get_ports { args } {
  return [join $args]
}

# --- To handle [*] ports in retention
proc ::* { args } {
  return "\[*\]"
}

set_var synopsys_program_name dc_shell

## -- Allows selection of IP for stubbing
## -- Enabling all IP for now
set ::IP_ENABLE_ALL 1