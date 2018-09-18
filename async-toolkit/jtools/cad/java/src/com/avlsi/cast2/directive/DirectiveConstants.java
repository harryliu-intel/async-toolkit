/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast2.directive;

/**
 * An interface that contains directive constants.
 **/
public interface DirectiveConstants {
    /**
     * Define directive names here to enable external access.  To add a new
     * directive, add a type name here, and then add a line to the static
     * section in DirectiveTable.java to register the directive.
     **/
    String CYCLE_COUNT = "cycle_count";
    String RESET_CYCLE_COUNT = "reset_cycle_count";
    String CYCLE_NODE = "cycle_node";
    String CYCLE_TIME = "cycle_time";
    String NTPC_SPEC = "ntpc_spec";
    String ASPICE_DIGITAL_CYCLES = "aspice_digital_cycles";
    String ASPICE_ANALOG_CYCLES = "aspice_analog_cycles";
    String ASPICE_TIME_MAX = "aspice_time_max";
    String NTPC_SCALING = "ntpc_scaling";
    String RTE_COSIM_SPEC = "rte_cosim_spec";
    String RTE_ENV_COSIM_SPEC = "rte_env_cosim_spec";
    String RTE_IGNORE = "rte_ignore";
    String ASPICE_IGNORE = "aspice_ignore";
    String JAUTO_IGNORE = "jauto_ignore";
    String FRAGMENT = "fragment";
    String CELLNONOBSERVABLE = "cellnonobservable";
    String FLOORPLAN = "floorplan";
    String SYNCHRONOUS = "synchronous";
    String UNIMPL = "unimplementable";
    String NO_STAT = "nostaticizer";
    String SYMMETRIZE = "symmetrize";
    String SHARED = "shared";
    String DELAYBIAS = "delaybias";
    String EXTRA_DELAY = "extra_delay";
    String ASTA_EXTRA_DELAY = "asta_extra_delay";
    String DEFAULT_UP_DELAY = "default_up_delay";
    String DEFAULT_DN_DELAY = "default_dn_delay";
    String SIGNOFF = "sign_off";
    String SIGNOFF_CONSTANT = "signoff_constant";
    String UNUSED_PRS = "unused_prs";
    String CUTPATH = "cutpath";
    String FIXED_SIZE = "fixed_size";
    String MIN_WIRELENGTH = "min_wirelength";
    String MIN_WIRESPAN   = "min_wirespan";
    String WIRELENGTH = "wirelength";
    String WIRESPAN = "wirespan";
    String WIREWIDTH = "wirewidth";
    String WIRESPACE = "wirespace";
    String EXTERNAL_WIRELENGTH = "external_wirelength";
    String EXTERNAL_LOADCAP = "external_loadcap";
    String EXTERNAL_DRIVER = "external_driver";
    String INTERNAL_WIRELENGTH = "internal_wirelength";
    String INTERNAL_LOADCAP = "internal_loadcap";
    String INTERNAL_DRIVER = "internal_driver";
    String CDLSCALE = "cdlscale";
    String HEIGHT = "height";
    String WIDTH = "width";
    String SPLITTABLE = "splittable";
    String OWNER = "owner";
    String CHANNEL_BUNCHED = "bunched";
    String PINLAYER = "pinlayer";
    String BITPITCH = "bitpitch";
    String PINTYPE = "pintype";
    String PINTYPE_INPLACE = "inplace";
    String PINTYPE_PITCHED = "pitched";
    String LOADCAP = "loadcap";
    String PASSGATE = "passgate";
    String DENSITY_FACTOR = "density_factor";
    String INLINE_LAYOUT = "inline_layout";
    String RESISTANCE_SCALE = "resistance_scale";
    String STACK_GATE = "stack_gate";
    String STATICIZER_GATE = "staticizer_gate";
    String TAU = "tau";
    String MINDELAY = "mindelay";
    String STRENGTHBIAS = "strength_bias";
    String LAYOUT_ATTRIBUTES = "layout_attributes";
    String EXTRA_LAYOUT_ATTRIBUTES = "extra_layout_attributes";
    String LAYER_KEEPOUT_PATTERN = "layer_keepout_pattern";
    String LAYER_WIREPITCH = "layer_wirepitch";
    String LAYER_WIRESPACING = "layer_wirespacing";
    String LAYER_WIREWIDTH = "layer_wirewidth";
    String LAYER_POWERGRID_OFFSET = "layer_powergrid_offset";
    String LAYER_POWERGRID_SPACING = "layer_powergrid_spacing";
    String LAYER_POWERGRID_WIREWIDTH = "layer_powergrid_wirewidth";
    String DENSITY_SCALE_FACTOR = "density_scale_factor";
    String BASE_TRANSISTOR_LAYOUT_WIDTH = "base_transistor_layout_width";
    String BOUNDARY_SMALL_CELL_WARNING_CUTOFF = "boundary_small_cell_warning_cutoff";
    String WIDTH_MINIMUM = "width_minimum";
    String HEIGHT_MINIMUM = "height_minimum";
    String BOUNDARY_POSITION = "boundary_position";
    String WIDTH_INCREMENT = "width_increment";
    String HEIGHT_INCREMENT = "height_increment";
    String WIDTH_OVERHEAD = "width_overhead";
    String HEIGHT_OVERHEAD = "height_overhead";
    String NETLIST_PRIMITIVE = "netlist_primitive";
    String PRECHARGE_PRIMITIVE = "precharge_primitive";
    String CAP = "cap";
    String ANTENNA_DIODE = "antenna_diode";
    String ESTIMATED_DELAY = "estimated_delay";
    String MEASURED_DELAY = "measured_delay";
    String ESTIMATED_CAP = "estimated_cap";
    String MEASURED_CAP = "measured_cap";
    String DEFAULT_SLEW = "default_slew";
    String MEASURED_CHARGE = "measured_charge";
    String CSP_COSIM_WILL_FAIL = "csp_cosim_will_fail";
    String TIMED = "timed";
    String TIMED_JITTER = "timed_jitter";
    String PRS_NETLIST_MISMATCH_OK = "prs_netlist_mismatch_ok";
    String PROTEUS_TAG = "proteus_tag"; // for taging proteus imported cast
    String PROTEUS_NODE_ROLE = "proteus_node_role";
    String PROTEUS_CELL_TYPE = "proteus_cell_type";
    String PROTEUS_LIBERTY_FUNCTION = "proteus_liberty_function";

    String POWERGRID_GNDNET = "powergrid_gndnet";
    String POWERGRID_VDDNET = "powergrid_vddnet";

    String LVS_NODES = "lvs_labels";

    String ESTIMATED_DELAY_SIGNOFF = "estimated_delay_signoff";
    String NTPC_SCALING_SIGNOFF = "ntpc_scaling_signoff";
    String SLEW_SIGNOFF = "slew_signoff";
    String SKEW_SIGNOFF = "skew_signoff";
    String ALINT_SIGNOFF = "alint_signoff";
    String STATICIZER_RATIO_SIGNOFF = "staticizer_ratio_signoff";
    String BUMP_SIGNOFF = "bump_signoff";
    String THRESH_BUMP_SIGNOFF = "thresh_resp_signoff";
    String LEAKAGE_SIGNOFF = "leakage_signoff";
    String ALINT_IGNORE = "alint_ignore";
    String ACTIVITY_FACTOR = "activity_factor";
    String DC_WIRING_SPEC = "dc_wiring_spec";
    String AC_WIRING_SPEC = "ac_wiring_spec";
    String EFFECTIVE_RESISTANCE = "effective_resistance";
    String INTRINSIC_DELAY = "intrinsic_delay";
    String INTRINSIC_CAPACITANCE = "intrinsic_capacitance";
    String TRANSISTOR_TYPE = "transistor_type";
    String FAST_TRANSISTOR_TYPE = "fast_transistor_type";
    String SLOW_TRANSISTOR_TYPE = "slow_transistor_type";
    String TRANSISTOR_NAME = "transistor_name";
    String PLUS_NODE = "plus_node";
    String MINUS_NODE = "minus_node";
    String LEAKY = "leaky";
    String ROUTED = "routed";
    String AUTO_LAYOUT = "auto_layout";
    String COVERAGE_IGNORE = "coverage_ignore";
    String ALINT_MAX_BUMP_FANIN = "alint_max_bump_fanin";
    String ALINT_DEFAULT_SCENARIOS = "alint_default_scenarios";
    String ALINT_SCENARIO = "alint_scenario";
    String ALINT_DELAY_SCENARIO = "alint_delay_scenario";
    String ALINT_LEAK_SCENARIO = "alint_leak_scenario";
    String ALINT_BUMP_SCENARIO = "alint_bump_scenario";
    String EXCLCC = "exclcc";
    String NOCC_NODES = "nocc_nodes";
    String RESET_NET = "reset_net";
    String STATICIZER_RATIO = "staticizer_ratio";
    String STATICIZER_TYPE = "staticizer_type";
    String PHANTOM_PORT_SIGNOFF = "phantom_port_signoff";
    String PORT_GROUP = "port_group";
    String GROUP_FREQUENCY = "group_frequency";
    String GROUP_CLOCK = "group_clock";
    String DEFAULT_INTEGER_WIDTH = "default_integer_width";
    String SYNTHESIZABLE = "synthesizable";
    String FLOORPLAN_ARRAY = "floorplan_array";
    String NAME_MAPPING = "name_mapping";
    String STRENGTH_GROUP = "strength_group";
    String IDLE_STATE = "idle_state";
    String NUM_VALUES = "num_values";
    String POWER_NET = "power_net";
    String GROUND_NET = "ground_net";
    String INITIALIZE_ON_RESET = "initialize_on_reset";
    String ISOCHRONIC = "isochronic";

    /** CSP timing directives */
    String FORWARD_LATENCY = "forward_latency";
    String CONTROLLER_LATENCY = "controller_latency";
    String SLACK = "slack";
    String STAGES = "stages";
    String LATENCY = "latency";
    String LATENCY_PER_STAGE = "latency_per_stage";
    String LATENCY_PER_SLACK = "latency_per_slack";
    String SLACK_PER_STAGE = "slack_per_stage";
    String DEFAULT_CYCLE_TIME = "default_cycle_time";
    String FB = "fb";
    String FB_NEUTRAL = "fb_neutral";
    String FB_VALID = "fb_valid";
    String BF = "bf";
    String TIME_UNIT = "time_unit";
    String CYCLE_TIME_IN = "cycle_time_in";
    String CYCLE_TIME_OUT = "cycle_time_out";
    String STRICT_VARS = "strict_vars";
    String DISABLE_CHANDFT_HANDLER = "disable_chandft_handler";
    String WIRING = "wiring";
    String SUPPRESS_FAULTS = "suppress_faults";
    String DYNAMIC_SLACK = "dynamic_slack";
    String IMPLICIT_INIT = "implicit_init";
    String INTERNAL_SLACK = "internal_slack";
    String SYNCHRONIZE_CHANNELS = "synchronize_channels";
    String CSP_TIME = "csp_time";
    String CSP_TIME_INPUTS = "csp_time_inputs";
    String CSP_TIME_OUTPUTS = "csp_time_outputs";
    String CYCLE_TIME_INPUTS = "cycle_time_inputs";
    String CYCLE_TIME_OUTPUTS = "cycle_time_outputs";
    String TIME = "time";

    /** DFX related directives */
    String ASYNC_SCAN_INPUTS = "async_scan_inputs";
    String ASYNC_SCAN_OUTPUTS = "async_scan_outputs";
    String ASYNC_SCAN = "async_scan";
    String PROTEUS_SCAN_MODEL = "proteus_scan_model";
    String PROTEUS_SCAN_MACROS = "proteus_scan_macros";
    String PROTEUS_SCAN_DECLONE = "proteus_scan_declone";
    String TUNABLE_DELAY_MUL = "tunable_delay_mul";
    String TUNABLE_DELAY_ADD = "tunable_delay_add";
    String TUNABLE_DELAY_CONFIG = "tunable_delay_config";

    /** Slacker directives */
    String SLACKER_LEAF = "slacker_leaf";
    String SLACKER_PRIMITIVE = "slacker_primitive";
    String SLACKER_IS_SLACK = "slacker_is_slack";
    String SLACKER_TIME = "slacker_time";
    String SLACKER_TIME_INPUTS = "slacker_time_inputs";
    String SLACKER_TIME_OUTPUTS = "slacker_time_outputs";
    String SLACKER_FREE_SLACK = "slacker_free_slack";
    String SLACKER_ELASTICITY = "slacker_elasticity";
    String SLACKER_HANDSHAKES = "slacker_handshakes";
    String SLACKER_INITIAL_TOKENS = "slacker_initial_tokens";
    String SLACKER_ALIGNMENT = "slacker_alignment";
    String SLACKER_IGNORE = "slacker_ignore";
    String SLACKER_NTPC = "slacker_ntpc";
    String SLACKER_DONT_TOUCH = "slacker_dont_touch";
    String SLACKER_TRANSITIONS = "slacker_transitions";
    String SLACKER_COST = "slacker_cost";
    String SLACKER_USE_EXTRA_DELAY = "slacker_use_extra_delay";
    String SLACKER_CHANNEL = "slacker_channel";
    String CONDITION_TIME = "condition_time";
    String CONDITION_ORDER = "condition_order";
    String CONDITION_GROUP = "condition_group";

    /** ASTA directives */
    String ASTA_BLACKBOX = "asta_blackbox";
    String ASTA_GRAYBOX = "asta_graybox";
    String ASTA_IGNORE = "asta_ignore";
    String SLINT_IGNORE = "slint_ignore";
    
    /** VDCVerify directives */
    String VDC_LEAF = "vdc_leaf";
    String VDD_DEFAULT_DOMAIN = "vdd_default_domain";
    String VDD_DOMAIN = "vdd_domain";

    /** Verilog block directives */
    String BOUNDS = "bounds";

    /** Java block directives */
    String GROUP = "group";

    /** Define data types here to enable external access */
    String INT_TYPE = "int";    
    String BIGINT_TYPE = "bigint";    
    String FLOAT_TYPE = "float";
    String DOUBLE_TYPE = "double";
    String BOOLEAN_TYPE = "boolean";
    String STRING_TYPE = "string";
    String NODE_TYPE = "node";   
    String HALFOP_TYPE = "halfop";
    String UNCHECKED_NODE_TYPE = "unchecked_node";
    String UNCHECKED_HALFOP_TYPE = "unchecked_halfop";
    String DEEP_NODE_TYPE = "deep_node";
    String DEEP_HALFOP_TYPE = "deep_halfop";
    String RULE_TYPE = "rule";
    String DEEP_RULE_TYPE = "deep_rule";
    String CHANNEL_TYPE = "channel";
    String WIDE_CHANNEL_TYPE = "wide_channel";
    String POSSIBLY_WIDE_CHANNEL_TYPE = "possibly_wide_channel";
    String INSTANCE_TYPE = "instance";
    String ARRAYED_INSTANCE_TYPE = "arrayed_instance";
    String LAYER_TYPE = "layer";
    String COSIM_SPEC_TYPE = "cosim_spec";
    String ALINT_SCENARIO_TYPE = "alint_scenario";
    String PRS_EXPR_TYPE = "prs_expr";
    String DEEP_PRS_EXPR_TYPE = "deep_prs_expr";
    String CSP_LVALUE = "csp_lvalue";
}
