/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast2.directive.impl;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import com.avlsi.file.common.HierName;
import com.avlsi.csp.util.CspCallback;
import com.avlsi.layout.InstanceCallback;
import com.avlsi.layout.LayerCallback;
import com.avlsi.util.container.MultiMap;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.Triplet;
import com.avlsi.cast.impl.Value;
import com.avlsi.cast2.impl.PrsCallback;
import static com.avlsi.cast2.directive.DirectiveConstants.*;
import com.avlsi.cast2.directive.impl.DefaultCallback;
import com.avlsi.cast2.directive.impl.DirectiveCallback;
import com.avlsi.tools.cosim.spec.CoSimSpecCallback;

//imports specific for adding the different directives
import static com.avlsi.fast.BlockInterface.*;


/**
 * A class that acts as a central registry for valid Directives.
 **/
public class DirectiveTable {
    protected static Map callback = new HashMap();
    protected static Map directive = new HashMap();
    protected static MultiMap paramDirective = new MultiMap();

    static {
        DirectiveCallback cb = new DefaultCallback();
        registerCallback(null, INT_TYPE, cb);
        registerCallback(null, BIGINT_TYPE, cb);
        registerCallback(null, FLOAT_TYPE, cb);
        registerCallback(null, DOUBLE_TYPE, cb);
        registerCallback(null, BOOLEAN_TYPE, cb);
        registerCallback(null, STRING_TYPE, cb);
        registerCallback(null, NODE_TYPE, PrsCallback.getInstance());
        registerCallback(null, HALFOP_TYPE, PrsCallback.getInstance());
        registerCallback(null, UNCHECKED_NODE_TYPE, PrsCallback.getInstance());
        registerCallback(null, UNCHECKED_HALFOP_TYPE, PrsCallback.getInstance());
        registerCallback(null, DEEP_NODE_TYPE, PrsCallback.getInstance());
        registerCallback(null, DEEP_HALFOP_TYPE, PrsCallback.getInstance());
        registerCallback(null, RULE_TYPE, PrsCallback.getInstance());
        registerCallback(null, DEEP_RULE_TYPE, PrsCallback.getInstance());
        registerCallback(null, CHANNEL_TYPE, InstanceCallback.getInstance());
        registerCallback(null, WIDE_CHANNEL_TYPE, CspCallback.getInstance());
        registerCallback(null, POSSIBLY_WIDE_CHANNEL_TYPE, CspCallback.getInstance());
        registerCallback(null, INSTANCE_TYPE, InstanceCallback.getInstance());
        registerCallback(null, ARRAYED_INSTANCE_TYPE, InstanceCallback.getInstance());
        registerCallback(null, LAYER_TYPE, LayerCallback.getInstance());
        registerCallback(null, COSIM_SPEC_TYPE, CoSimSpecCallback.getInstance());
        registerCallback(null, ALINT_SCENARIO_TYPE, PrsCallback.getInstance());
        registerCallback(null, PRS_EXPR_TYPE, PrsCallback.getInstance());
        registerCallback(null, DEEP_PRS_EXPR_TYPE, PrsCallback.getInstance());

        /**
         * Register RTE directives and set default values
         */
        registerDirective(ENV, CYCLE_COUNT, INT_TYPE, null);
        registerDirective(ENV, RESET_CYCLE_COUNT, INT_TYPE, null);
        registerDirective(ENV, CYCLE_NODE, NODE_TYPE, null);
        registerDirective(ENV, CYCLE_TIME, INT_TYPE, null);
        registerDirective(CELL, RTE_COSIM_SPEC, COSIM_SPEC_TYPE, null);
        registerDirective(ENV, RTE_COSIM_SPEC, COSIM_SPEC_TYPE, null);
        registerDirective(CELL, RTE_ENV_COSIM_SPEC, COSIM_SPEC_TYPE, null);
        registerDirective(ENV, RTE_ENV_COSIM_SPEC, COSIM_SPEC_TYPE, null);
        registerDirective(ENV, RTE_IGNORE, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(ENV, ASPICE_IGNORE, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(ENV, NTPC_SPEC, NODE_TYPE, FLOAT_TYPE, null);
        registerDirective(CELL, ASPICE_DIGITAL_CYCLES, NODE_TYPE, INT_TYPE, null);
        registerDirective(ENV, ASPICE_DIGITAL_CYCLES, NODE_TYPE, INT_TYPE, null);
        registerDirective(ENV, ASPICE_ANALOG_CYCLES, NODE_TYPE, INT_TYPE, null);
        registerDirective(CELL, ASPICE_ANALOG_CYCLES, NODE_TYPE, INT_TYPE, null);
        registerDirective(ENV, ASPICE_TIME_MAX, FLOAT_TYPE, null);
        registerDirective(CELL, NTPC_SCALING, FLOAT_TYPE, new Float(1));
        registerDirective(ENV, TIMED, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(ENV, TIMED_JITTER, FLOAT_TYPE, null);
        registerDirective(CELL, TIMED, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(CELL, TIMED_JITTER, FLOAT_TYPE, null);
        registerDirective(ENV, CSP_COSIM_WILL_FAIL, BOOLEAN_TYPE, Boolean.FALSE);

        /**
         * Register top-level directives used by rte and jauto
         **/
        registerDirective(CELL, FRAGMENT, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(CELL, CELLNONOBSERVABLE, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(CELL, FLOORPLAN, BOOLEAN_TYPE, Boolean.TRUE);
        registerDirective(CELL, SYNCHRONOUS, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(CELL, UNIMPL, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(CELL, RTE_IGNORE, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(CELL, ASPICE_IGNORE, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(CELL, SIGNOFF, BOOLEAN_TYPE, Boolean.FALSE);

        /* Arguably, this should be inside the netlist block, but it would make
         * parsing more difficult.  In any case, this directive is only
         * effective when specified in a cell with a netlist block. */
        registerDirective(CELL, FIXED_SIZE, BOOLEAN_TYPE, Boolean.TRUE);
        registerDirective(CELL, CDLSCALE, FLOAT_TYPE, new Float(1.0));
        registerDirective(CELL, WIRING, BOOLEAN_TYPE, Boolean.FALSE);

        /** Jauto wiring directives **/
        registerDirective(PRS, MIN_WIRELENGTH, NODE_TYPE, FLOAT_TYPE, new Float(-1.0));
        registerDirective(PRS, MIN_WIRESPAN, NODE_TYPE, FLOAT_TYPE, new Float(-1.0));
        registerDirective(PRS, WIRELENGTH, NODE_TYPE, FLOAT_TYPE, new Float(-1.0));
        registerDirective(PRS, WIRESPAN, NODE_TYPE, FLOAT_TYPE, new Float(-1.0));
        registerDirective(PRS, WIREWIDTH, NODE_TYPE, FLOAT_TYPE, new Float(-1.0));
        registerDirective(PRS, WIRESPACE, NODE_TYPE, FLOAT_TYPE, new Float(-1.0));

        registerDirective(SUBCELL, MIN_WIRELENGTH, NODE_TYPE, FLOAT_TYPE, new Float(-1.0));
        registerDirective(SUBCELL, MIN_WIRESPAN, NODE_TYPE, FLOAT_TYPE, new Float(-1.0));
        registerDirective(SUBCELL, WIRELENGTH, NODE_TYPE, FLOAT_TYPE, new Float(-1.0));
        registerDirective(SUBCELL, WIRESPAN, NODE_TYPE, FLOAT_TYPE, new Float(-1.0));
        registerDirective(SUBCELL, WIREWIDTH, NODE_TYPE, FLOAT_TYPE, new Float(-1.0));
        registerDirective(SUBCELL, WIRESPACE, NODE_TYPE, FLOAT_TYPE, new Float(-1.0));

        registerDirective(CELL, MIN_WIRELENGTH, NODE_TYPE, FLOAT_TYPE, new Float(-1.0));
        registerDirective(CELL, MIN_WIRESPAN, NODE_TYPE, FLOAT_TYPE, new Float(-1.0));
        registerDirective(CELL, WIRELENGTH, NODE_TYPE, FLOAT_TYPE, new Float(-1.0));
        registerDirective(CELL, WIRESPAN, NODE_TYPE, FLOAT_TYPE, new Float(-1.0));
        registerDirective(CELL, WIRESPACE, NODE_TYPE, FLOAT_TYPE, new Float(-1.0) );
        registerDirective(CELL, WIREWIDTH, NODE_TYPE, FLOAT_TYPE, new Float(-1.0));

        registerDirective(CELL, MIN_WIRELENGTH, FLOAT_TYPE, new Float(-1.0));
        registerDirective(CELL, MIN_WIRESPAN, FLOAT_TYPE, new Float(-1.0));
        registerDirective(CELL, WIRELENGTH, FLOAT_TYPE, new Float(-1.0));
        registerDirective(CELL, WIRESPAN, FLOAT_TYPE, new Float(-1.0));
        registerDirective(CELL, WIREWIDTH, FLOAT_TYPE, new Float(-1.0));
        registerDirective(CELL, WIRESPACE, FLOAT_TYPE, new Float(-1.0));

        /** other sizing/simulation directives **/
        registerDirective(CELL, HEIGHT, FLOAT_TYPE, new Float(-1.0));
        registerDirective(CELL, WIDTH, FLOAT_TYPE, new Float(-1.0));
        registerDirective(CELL, SPLITTABLE, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(CELL, DELAYBIAS, FLOAT_TYPE, new Float(1.0));
        registerDirective(CELL, DEFAULT_UP_DELAY, FLOAT_TYPE, new Float(100));
        registerDirective(CELL, DEFAULT_DN_DELAY, FLOAT_TYPE, new Float(100));
        registerDirective(CELL, OWNER, STRING_TYPE, null);
        registerDirective(CELL, PASSGATE, STRING_TYPE, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(CELL, STACK_GATE, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(CELL, STATICIZER_GATE, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(CELL, TAU, FLOAT_TYPE, null);
        registerDirective(CELL, LAYOUT_ATTRIBUTES, STRING_TYPE, null);
        registerDirective(CELL, EXTRA_LAYOUT_ATTRIBUTES, STRING_TYPE, null);
        registerDirective(CELL, NETLIST_PRIMITIVE, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(CELL, PRECHARGE_PRIMITIVE, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(CELL, COVERAGE_IGNORE, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(CELL, STATICIZER_RATIO, FLOAT_TYPE, null);
        registerDirective(PRS, STATICIZER_RATIO, HALFOP_TYPE, FLOAT_TYPE, null);
        registerDirective(CELL, STATICIZER_TYPE, INT_TYPE, null);
        registerDirective(PRS, STATICIZER_TYPE, HALFOP_TYPE, INT_TYPE, new Integer(0));
        registerDirective(PRS, STRENGTH_GROUP, HALFOP_TYPE, INT_TYPE, null);

        /*
         * Register PRS synthesis directives; used by jauto "p2n", and PrsToNet
         * "mgn", and lvs, "nvp".
         */
        /* this next one should probably be a half operators. */
        registerDirective(PRS, NO_STAT, NODE_TYPE, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(PRS, DELAYBIAS, HALFOP_TYPE, FLOAT_TYPE, new Float(1.0));
        registerDirective(SUBCELL, DELAYBIAS, INSTANCE_TYPE, FLOAT_TYPE, new Float(1.0));
        registerDirective(PRS, EXTRA_DELAY, HALFOP_TYPE, FLOAT_TYPE, new Float(0.0));
        registerDirective(SUBCELL, EXTRA_DELAY, HALFOP_TYPE, FLOAT_TYPE, new Float(0.0));
        registerDirective(PRS, ASTA_EXTRA_DELAY, HALFOP_TYPE, FLOAT_TYPE, new Float(0.0));
        registerDirective(SUBCELL, ASTA_EXTRA_DELAY, HALFOP_TYPE, FLOAT_TYPE, new Float(0.0));
        registerDirective(PRS, SYMMETRIZE, HALFOP_TYPE, INT_TYPE, null);
        registerDirective(PRS, SHARED, HALFOP_TYPE, INT_TYPE, null);
        registerDirective(PRS, CUTPATH, NODE_TYPE, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(SUBCELL, CUTPATH, NODE_TYPE, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(PRS, LOADCAP, NODE_TYPE, FLOAT_TYPE, new Float(-1.0));
        registerDirective(SUBCELL, LOADCAP, NODE_TYPE, FLOAT_TYPE, new Float(-1.0));
        registerDirective(SUBCELL, RESISTANCE_SCALE, NODE_TYPE, FLOAT_TYPE, new Float(1.0));
        registerDirective(PRS, MINDELAY, HALFOP_TYPE, FLOAT_TYPE, null);
        registerDirective(PRS, STRENGTHBIAS, HALFOP_TYPE, FLOAT_TYPE, null);
        // power estimate in DSim, bug 2154
        registerDirective(PRS, CAP, NODE_TYPE, FLOAT_TYPE, new Float(0.0));
        registerDirective(SUBCELL, CAP, NODE_TYPE, FLOAT_TYPE, new Float(0.0));
        // delay estimate in DSim, bug 2156
        registerDirective(PRS, ESTIMATED_DELAY, HALFOP_TYPE, FLOAT_TYPE, new Float(0.0));
        registerDirective(SUBCELL, ESTIMATED_DELAY, HALFOP_TYPE, FLOAT_TYPE, new Float(0.0));
        registerDirective(PRS, MEASURED_DELAY, arrayify(UNCHECKED_HALFOP_TYPE), arrayify(arrayify(FLOAT_TYPE)), new Float(0.0));
        registerDirective(SUBCELL, MEASURED_DELAY, arrayify(UNCHECKED_HALFOP_TYPE), arrayify(arrayify(FLOAT_TYPE)), new Float(0.0));
        registerDirective(PRS, MEASURED_CHARGE, arrayify(UNCHECKED_HALFOP_TYPE), arrayify(arrayify(FLOAT_TYPE)), new Float(0.0));
        registerDirective(SUBCELL, MEASURED_CHARGE, arrayify(UNCHECKED_HALFOP_TYPE), arrayify(arrayify(FLOAT_TYPE)), new Float(0.0));
        registerDirective(PRS, ESTIMATED_CAP, UNCHECKED_NODE_TYPE, FLOAT_TYPE, new Float(0.0));
        registerDirective(SUBCELL, ESTIMATED_CAP, UNCHECKED_NODE_TYPE, FLOAT_TYPE, new Float(0.0));
        registerDirective(PRS, MEASURED_CAP, UNCHECKED_NODE_TYPE, FLOAT_TYPE, new Float(0.0));
        registerDirective(SUBCELL, MEASURED_CAP, UNCHECKED_NODE_TYPE, FLOAT_TYPE, new Float(0.0));
        registerDirective(CELL, DEFAULT_SLEW, FLOAT_TYPE, new Float(30.0));
        registerDirective(CELL, PRS_NETLIST_MISMATCH_OK, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(PRS, PRS_NETLIST_MISMATCH_OK, NODE_TYPE, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(CELL, JAUTO_IGNORE, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(SUBCELL, SIGNOFF_CONSTANT, DEEP_NODE_TYPE, INT_TYPE, new Integer(0));
        registerDirective(PRS, SIGNOFF_CONSTANT, NODE_TYPE, INT_TYPE, new Integer(0));
        registerDirective(SUBCELL, UNUSED_PRS, DEEP_RULE_TYPE, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(PRS, UNUSED_PRS, RULE_TYPE, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(CELL, EXTERNAL_WIRELENGTH, FLOAT_TYPE, null);
        registerDirective(CELL, EXTERNAL_WIRELENGTH, NODE_TYPE, FLOAT_TYPE, null);
        registerDirective(CELL, EXTERNAL_LOADCAP, FLOAT_TYPE, null);
        registerDirective(CELL, EXTERNAL_LOADCAP, NODE_TYPE, FLOAT_TYPE, null);
        registerDirective(CELL, EXTERNAL_DRIVER, STRING_TYPE, null);
        registerDirective(CELL, EXTERNAL_DRIVER, NODE_TYPE, STRING_TYPE, null);
        registerDirective(CELL, INTERNAL_WIRELENGTH, FLOAT_TYPE, null);
        registerDirective(CELL, INTERNAL_WIRELENGTH, NODE_TYPE, FLOAT_TYPE, null);
        registerDirective(CELL, INTERNAL_LOADCAP, FLOAT_TYPE, null);
        registerDirective(CELL, INTERNAL_LOADCAP, NODE_TYPE, FLOAT_TYPE, null);
        registerDirective(CELL, INTERNAL_DRIVER, STRING_TYPE, null);
        registerDirective(CELL, INTERNAL_DRIVER, NODE_TYPE, STRING_TYPE, null);
        registerDirective(CELL, NAME_MAPPING, STRING_TYPE, null);
        registerDirective(VERILOG, NAME_MAPPING, STRING_TYPE, null);
        registerDirective(PRS, IDLE_STATE, NODE_TYPE, INT_TYPE, null);
        registerDirective(CELL, IDLE_STATE, NODE_TYPE, INT_TYPE, null);
        registerDirective(CELL, NUM_VALUES, BIGINT_TYPE, null);
        registerDirective(CELL, PROTEUS_NODE_ROLE, NODE_TYPE, STRING_TYPE, null);
        registerDirective(CELL, PROTEUS_CELL_TYPE, STRING_TYPE, null);
        registerDirective(CELL, PROTEUS_LIBERTY_FUNCTION, NODE_TYPE, PRS_EXPR_TYPE, null);

        /**
         * Register Directives for Automated Layout.  Used by autopins, autobus
         **/

        registerDirective(CELL, PINTYPE, STRING_TYPE, PINTYPE_PITCHED );
        registerDirective(CELL, CHANNEL_BUNCHED, CHANNEL_TYPE, BOOLEAN_TYPE, Boolean.FALSE);
                        
        registerDirective(CELL, BITPITCH, FLOAT_TYPE, new Float(-1.0) );
        registerDirective(CELL, PINLAYER, LAYER_TYPE, null );
        registerDirective(CELL, POWERGRID_GNDNET, STRING_TYPE, null );
        registerDirective(CELL, POWERGRID_VDDNET, STRING_TYPE, null );

        registerDirective(PRS, ANTENNA_DIODE, NODE_TYPE, FLOAT_TYPE, new Float(-1.0) );
        registerDirective(SUBCELL, ANTENNA_DIODE, NODE_TYPE, FLOAT_TYPE, new Float(-1.0) );

        registerDirective(CELL, LAYER_WIREPITCH, LAYER_TYPE, FLOAT_TYPE, new Float(-1.0) );
        registerDirective(CELL, LAYER_WIRESPACING, LAYER_TYPE, FLOAT_TYPE, new Float(-1.0) );
        registerDirective(CELL, LAYER_WIREWIDTH, LAYER_TYPE, FLOAT_TYPE, new Float(-1.0) );

        registerDirective(CELL, LAYER_POWERGRID_OFFSET, LAYER_TYPE, INT_TYPE, new Integer(0) );
        registerDirective(CELL, LAYER_POWERGRID_SPACING, LAYER_TYPE, INT_TYPE, new Integer(-1) );
        registerDirective(CELL, LAYER_POWERGRID_WIREWIDTH, LAYER_TYPE, FLOAT_TYPE, new Float(-1.0) );
        registerDirective(CELL, LAYER_KEEPOUT_PATTERN, LAYER_TYPE, STRING_TYPE, "0" );
        registerDirective(CELL, AUTO_LAYOUT, INT_TYPE, new Integer(0));
        registerDirective(CELL, RESET_NET, NODE_TYPE, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(SUBCELL, RESET_NET, NODE_TYPE, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(SUBCELL, FLOORPLAN_ARRAY, ARRAYED_INSTANCE_TYPE, STRING_TYPE, null);
        registerDirective(CELL, POWER_NET, NODE_TYPE, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(CELL, GROUND_NET, NODE_TYPE, BOOLEAN_TYPE, Boolean.FALSE);

        /** updatenetlist directives **/
        registerDirective(CELL, BASE_TRANSISTOR_LAYOUT_WIDTH, FLOAT_TYPE, new Float(0.0));
        registerDirective(CELL, DENSITY_FACTOR, FLOAT_TYPE, new Float(1.0));
        registerDirective(CELL, DENSITY_SCALE_FACTOR, FLOAT_TYPE, new Float(1.0));
        registerDirective(SUBCELL, INLINE_LAYOUT, INSTANCE_TYPE, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(SUBCELL, INLINE_LAYOUT, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(CELL, ROUTED, BOOLEAN_TYPE, null);
        registerDirective(SUBCELL, ROUTED, INSTANCE_TYPE, BOOLEAN_TYPE, Boolean.TRUE);
        registerDirective(CELL, BOUNDARY_POSITION, arrayify(FLOAT_TYPE), null);
        registerDirective(CELL, WIDTH_MINIMUM, FLOAT_TYPE, new Float(0.0));
        registerDirective(CELL, HEIGHT_MINIMUM, FLOAT_TYPE, new Float(0.0));
        registerDirective(CELL, WIDTH_INCREMENT, FLOAT_TYPE, new Float(0.0));
        registerDirective(CELL, HEIGHT_INCREMENT, FLOAT_TYPE, new Float(0.0));
        registerDirective(CELL, WIDTH_OVERHEAD, FLOAT_TYPE, new Float(0.0));
        registerDirective(CELL, HEIGHT_OVERHEAD, FLOAT_TYPE, new Float(0.0));
        registerDirective(CELL, BOUNDARY_SMALL_CELL_WARNING_CUTOFF, FLOAT_TYPE, new Float(0.0));

        /** LVE Signoff Directives **/
        registerDirective(PRS, ESTIMATED_DELAY_SIGNOFF, HALFOP_TYPE, FLOAT_TYPE, new Float(0.0));
        registerDirective(SUBCELL, ESTIMATED_DELAY_SIGNOFF, HALFOP_TYPE, FLOAT_TYPE, new Float(0.0));
        registerDirective(PRS, SLEW_SIGNOFF, HALFOP_TYPE, FLOAT_TYPE, new Float(0.0));
        registerDirective(SUBCELL, SLEW_SIGNOFF, HALFOP_TYPE, FLOAT_TYPE, new Float(0.0));
        registerDirective(PRS, SKEW_SIGNOFF, HALFOP_TYPE, FLOAT_TYPE, new Float(0.0));
        registerDirective(SUBCELL, SKEW_SIGNOFF, HALFOP_TYPE, FLOAT_TYPE, new Float(0.0));
        registerDirective(CELL, NTPC_SCALING_SIGNOFF, FLOAT_TYPE, new Float(1));
        registerDirective(SUBCELL, ALINT_SIGNOFF, NODE_TYPE, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(PRS, ALINT_SIGNOFF, NODE_TYPE, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(PRS, STATICIZER_RATIO_SIGNOFF, HALFOP_TYPE, arrayify(FLOAT_TYPE), null);
        registerDirective(PRS, BUMP_SIGNOFF, HALFOP_TYPE, FLOAT_TYPE, new Float(0));
        registerDirective(SUBCELL, BUMP_SIGNOFF, HALFOP_TYPE, FLOAT_TYPE, new Float(0));
        registerDirective(PRS, THRESH_BUMP_SIGNOFF, HALFOP_TYPE, FLOAT_TYPE, new Float(0));
        registerDirective(SUBCELL, THRESH_BUMP_SIGNOFF, HALFOP_TYPE, FLOAT_TYPE, new Float(0));
        registerDirective(PRS, LEAKAGE_SIGNOFF, NODE_TYPE, FLOAT_TYPE, new Float(0));
        registerDirective(SUBCELL, LEAKAGE_SIGNOFF, NODE_TYPE, FLOAT_TYPE, new Float(0));
        registerDirective(PRS, ALINT_MAX_BUMP_FANIN, NODE_TYPE, INT_TYPE, new Integer(-1));
        registerDirective(SUBCELL, ALINT_MAX_BUMP_FANIN, NODE_TYPE, INT_TYPE, new Integer(-1));
        registerDirective(PRS, ALINT_DEFAULT_SCENARIOS, HALFOP_TYPE, BOOLEAN_TYPE, null);
        registerDirective(SUBCELL, ALINT_DEFAULT_SCENARIOS, HALFOP_TYPE, BOOLEAN_TYPE, null);
        registerDirective(PRS, ALINT_SCENARIO, HALFOP_TYPE, arrayify(ALINT_SCENARIO_TYPE), null);
        registerDirective(SUBCELL, ALINT_SCENARIO, HALFOP_TYPE, arrayify(ALINT_SCENARIO_TYPE), null);
        registerDirective(PRS, ALINT_DELAY_SCENARIO, HALFOP_TYPE, arrayify(ALINT_SCENARIO_TYPE), null);
        registerDirective(SUBCELL, ALINT_DELAY_SCENARIO, HALFOP_TYPE, arrayify(ALINT_SCENARIO_TYPE), null);
        registerDirective(PRS, ALINT_LEAK_SCENARIO, HALFOP_TYPE, arrayify(ALINT_SCENARIO_TYPE), null);
        registerDirective(SUBCELL, ALINT_LEAK_SCENARIO, HALFOP_TYPE, arrayify(ALINT_SCENARIO_TYPE), null);
        registerDirective(PRS, ALINT_BUMP_SCENARIO, HALFOP_TYPE, arrayify(ALINT_SCENARIO_TYPE), null);
        registerDirective(SUBCELL, ALINT_BUMP_SCENARIO, HALFOP_TYPE, arrayify(ALINT_SCENARIO_TYPE), null);
        registerDirective(CELL,ALINT_IGNORE, BOOLEAN_TYPE,Boolean.FALSE);

        registerDirective(PRS, ACTIVITY_FACTOR, NODE_TYPE, FLOAT_TYPE, new Float(-1.0));
        registerDirective(SUBCELL, ACTIVITY_FACTOR, NODE_TYPE, FLOAT_TYPE, new Float(-1.0));
        registerDirective(PRS, DC_WIRING_SPEC, NODE_TYPE, arrayify(FLOAT_TYPE), new Float(-1.0));
        registerDirective(SUBCELL, DC_WIRING_SPEC, NODE_TYPE, arrayify(FLOAT_TYPE), new Float(-1.0));
        registerDirective(PRS, AC_WIRING_SPEC, NODE_TYPE, arrayify(FLOAT_TYPE), new Float(-1.0));
        registerDirective(SUBCELL, AC_WIRING_SPEC, NODE_TYPE, arrayify(FLOAT_TYPE), new Float(-1.0));
        registerDirective(PRS, LEAKY, NODE_TYPE, BOOLEAN_TYPE, Boolean.FALSE);

        registerDirective(CELL, LVS_NODES, arrayify(STRING_TYPE), null);

        registerDirective(CELL, EFFECTIVE_RESISTANCE, HALFOP_TYPE, FLOAT_TYPE, null);
        registerDirective(CELL, INTRINSIC_DELAY, HALFOP_TYPE, FLOAT_TYPE, null);
        registerDirective(CELL, INTRINSIC_CAPACITANCE, HALFOP_TYPE, FLOAT_TYPE, null);
        registerDirective(PRS, TRANSISTOR_TYPE, HALFOP_TYPE, INT_TYPE, null);
        registerDirective(CELL, TRANSISTOR_TYPE, INT_TYPE, new Integer(1));
        registerDirective(CELL, FAST_TRANSISTOR_TYPE, INT_TYPE, new Integer(1));
        registerDirective(CELL, SLOW_TRANSISTOR_TYPE, INT_TYPE, new Integer(1));
        registerDirective(CELL, TRANSISTOR_NAME, INT_TYPE, STRING_TYPE, null);
        registerDirective(CELL, PLUS_NODE, NODE_TYPE, HierName.makeHierName("Vdd"));
        registerDirective(CELL, MINUS_NODE, NODE_TYPE, HierName.makeHierName("GND"));
        registerDirective(SUBCELL, EXCLCC, arrayify(UNCHECKED_NODE_TYPE), INT_TYPE, null);
        registerDirective(PRS, EXCLCC, arrayify(UNCHECKED_NODE_TYPE), INT_TYPE, null);
        registerDirective(SUBCELL, NOCC_NODES, arrayify(UNCHECKED_NODE_TYPE), null);
        registerDirective(PRS, NOCC_NODES, arrayify(UNCHECKED_NODE_TYPE), null);
        registerDirective(CELL, PHANTOM_PORT_SIGNOFF, NODE_TYPE, BOOLEAN_TYPE, null);
        registerDirective(CSP, DEFAULT_INTEGER_WIDTH, INT_TYPE, null);
        registerDirective(CSP, SYNTHESIZABLE, BOOLEAN_TYPE, Boolean.FALSE);

        /** CSP timing directives */
        registerDirective(CSP, SLACK, WIDE_CHANNEL_TYPE, INT_TYPE, null);
        registerDirective(CSP, DYNAMIC_SLACK, WIDE_CHANNEL_TYPE, INT_TYPE, null);
        registerDirective(CSP, STAGES, WIDE_CHANNEL_TYPE, INT_TYPE, null);
        registerDirective(CSP, LATENCY, WIDE_CHANNEL_TYPE, FLOAT_TYPE, null);
        registerDirective(CSP, LATENCY_PER_STAGE, FLOAT_TYPE, new Float(2.0));
        registerDirective(CSP, SLACK_PER_STAGE, FLOAT_TYPE, new Float(0.5));
        registerDirective(CSP, CYCLE_TIME, WIDE_CHANNEL_TYPE, FLOAT_TYPE, null);
        registerDirective(CSP, CYCLE_TIME_IN, WIDE_CHANNEL_TYPE, FLOAT_TYPE, null);
        registerDirective(CSP, CYCLE_TIME_OUT, WIDE_CHANNEL_TYPE, FLOAT_TYPE, null);
        registerDirective(CSP, DEFAULT_CYCLE_TIME, FLOAT_TYPE, new Float(18));
        registerDirective(CSP, FB, WIDE_CHANNEL_TYPE, FLOAT_TYPE, null);
        registerDirective(CSP, FB_NEUTRAL, WIDE_CHANNEL_TYPE, FLOAT_TYPE, null);
        registerDirective(CSP, FB_VALID, WIDE_CHANNEL_TYPE, FLOAT_TYPE, null);
        registerDirective(CSP, BF, WIDE_CHANNEL_TYPE, FLOAT_TYPE, null);
        registerDirective(CSP, TIME_UNIT, INT_TYPE, new Integer(100));
        registerDirective(CSP, INTERNAL_SLACK, WIDE_CHANNEL_TYPE, INT_TYPE, null);
        registerDirective(CSP, SYNCHRONIZE_CHANNELS, arrayify(WIDE_CHANNEL_TYPE), BOOLEAN_TYPE, Boolean.FALSE);

        // see bug 6085
        registerDirective(CSP, STRICT_VARS, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(CSP, DISABLE_CHANDFT_HANDLER, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(CSP, IMPLICIT_INIT, BOOLEAN_TYPE, Boolean.TRUE);

        /** Proteus directives */
        registerDirective(CELL, ASYNC_SCAN_INPUTS, BOOLEAN_TYPE, null);
        registerDirective(CELL, ASYNC_SCAN_OUTPUTS, BOOLEAN_TYPE, null);
        registerDirective(CELL, ASYNC_SCAN, POSSIBLY_WIDE_CHANNEL_TYPE, BOOLEAN_TYPE, null);
        registerDirective(CELL, PROTEUS_SCAN_MODEL, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(CELL, PROTEUS_SCAN_MACROS, BOOLEAN_TYPE, null);
        registerDirective(CELL, PROTEUS_SCAN_DECLONE, BOOLEAN_TYPE, Boolean.FALSE);

        /** Slacker directives */
        registerDirective(CELL, SLACKER_LEAF, BOOLEAN_TYPE, null);
        registerDirective(CELL, SLACKER_PRIMITIVE, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(CELL, SLACKER_NTPC, FLOAT_TYPE, null);
        registerDirective(CELL, SLACKER_IS_SLACK, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(CELL, SLACKER_TIME, POSSIBLY_WIDE_CHANNEL_TYPE, FLOAT_TYPE, new Float(0));
        registerDirective(CELL, SLACKER_FREE_SLACK, POSSIBLY_WIDE_CHANNEL_TYPE, FLOAT_TYPE, new Float(0));
        registerDirective(SUBCELL, SLACKER_FREE_SLACK, POSSIBLY_WIDE_CHANNEL_TYPE, FLOAT_TYPE, new Float(0));
        registerDirective(CELL, SLACKER_ELASTICITY, POSSIBLY_WIDE_CHANNEL_TYPE, FLOAT_TYPE, new Float(0));
        registerDirective(CELL, SLACKER_HANDSHAKES, POSSIBLY_WIDE_CHANNEL_TYPE, INT_TYPE, new Integer(0));
        registerDirective(CELL, SLACKER_INITIAL_TOKENS, POSSIBLY_WIDE_CHANNEL_TYPE, INT_TYPE, new Integer(0));
        registerDirective(CELL, SLACKER_ALIGNMENT, POSSIBLY_WIDE_CHANNEL_TYPE, INT_TYPE, new Integer(0));
        registerDirective(CELL, SLACKER_ALIGNMENT, INT_TYPE, null);
        registerDirective(CELL, SLACKER_IGNORE, POSSIBLY_WIDE_CHANNEL_TYPE, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(SUBCELL, SLACKER_IGNORE, POSSIBLY_WIDE_CHANNEL_TYPE, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(CELL, SLACKER_DONT_TOUCH, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(CELL, SLACKER_DONT_TOUCH, POSSIBLY_WIDE_CHANNEL_TYPE, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(SUBCELL, SLACKER_DONT_TOUCH, POSSIBLY_WIDE_CHANNEL_TYPE, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(CELL, SLACKER_TRANSITIONS, POSSIBLY_WIDE_CHANNEL_TYPE, FLOAT_TYPE, null);
        registerDirective(CELL, SLACKER_COST, FLOAT_TYPE, null);
        registerDirective(CELL, SLACKER_USE_EXTRA_DELAY, NODE_TYPE, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(CELL, SLACKER_CHANNEL, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(CELL, SUPPRESS_FAULTS, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(CELL, PORT_GROUP, POSSIBLY_WIDE_CHANNEL_TYPE, STRING_TYPE, null);
        registerDirective(CELL, GROUP_FREQUENCY, STRING_TYPE, FLOAT_TYPE, null);
        registerDirective(CELL, GROUP_CLOCK, STRING_TYPE, STRING_TYPE, null);
        registerDirective(VERILOG, BOUNDS, STRING_TYPE, arrayify(INT_TYPE), null);
        registerDirective(CELL, PROTEUS_TAG, STRING_TYPE, null);
        registerDirective(CELL, ASTA_BLACKBOX, BOOLEAN_TYPE, null);
        registerDirective(CELL, ASTA_GRAYBOX, BOOLEAN_TYPE, null);
        registerDirective(CELL, ASTA_IGNORE, arrayify(DEEP_HALFOP_TYPE), BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(PRS, ASTA_IGNORE, arrayify(DEEP_HALFOP_TYPE), BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(SUBCELL, ASTA_IGNORE, arrayify(DEEP_HALFOP_TYPE), BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(CELL, SLINT_IGNORE, arrayify(DEEP_HALFOP_TYPE), BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(PRS, SLINT_IGNORE, arrayify(DEEP_HALFOP_TYPE), BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(SUBCELL, SLINT_IGNORE, arrayify(DEEP_HALFOP_TYPE), BOOLEAN_TYPE, Boolean.FALSE);
        
        /** VDCVerify directives */
        registerDirective(CELL, VDC_LEAF, BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(CELL, VDD_DEFAULT_DOMAIN, NODE_TYPE, null);
        registerDirective(CELL, VDD_DOMAIN, NODE_TYPE, NODE_TYPE, null);

        /** Java block directives */
        registerDirective(JAVA, GROUP, WIDE_CHANNEL_TYPE, STRING_TYPE, null);
        registerDirective(JAVA, SLACK, WIDE_CHANNEL_TYPE, INT_TYPE, null);
        registerDirective(JAVA, DYNAMIC_SLACK, WIDE_CHANNEL_TYPE, INT_TYPE, null);
        registerDirective(JAVA, STAGES, WIDE_CHANNEL_TYPE, INT_TYPE, null);
        registerDirective(JAVA, LATENCY, WIDE_CHANNEL_TYPE, FLOAT_TYPE, null);
        registerDirective(JAVA, LATENCY_PER_STAGE, FLOAT_TYPE, new Float(2.0));
        registerDirective(JAVA, SLACK_PER_STAGE, FLOAT_TYPE, new Float(0.5));
        registerDirective(JAVA, CYCLE_TIME, WIDE_CHANNEL_TYPE, FLOAT_TYPE, null);
        registerDirective(JAVA, CYCLE_TIME_IN, WIDE_CHANNEL_TYPE, FLOAT_TYPE, null);
        registerDirective(JAVA, CYCLE_TIME_OUT, WIDE_CHANNEL_TYPE, FLOAT_TYPE, null);
        registerDirective(JAVA, DEFAULT_CYCLE_TIME, FLOAT_TYPE, new Float(18));
        registerDirective(JAVA, FB, WIDE_CHANNEL_TYPE, FLOAT_TYPE, null);
        registerDirective(JAVA, FB_NEUTRAL, WIDE_CHANNEL_TYPE, FLOAT_TYPE, null);
        registerDirective(JAVA, FB_VALID, WIDE_CHANNEL_TYPE, FLOAT_TYPE, null);
        registerDirective(JAVA, BF, WIDE_CHANNEL_TYPE, FLOAT_TYPE, null);
        registerDirective(JAVA, TIME_UNIT, INT_TYPE, new Integer(100));
    }

    /**
     * This class should not be instantiated.
     **/
    private DirectiveTable() { }

    /**
     * Register a callback object to parse parameters or values.
     * @param block Block in which the callback object is valid.  Use
     * <code>null</code> to use this callback object globally.
     * @param memberType Type that this callback object parses.
     * @param newCallback The callback object.
     **/
    public static
    DirectiveCallback registerCallback(String block, String memberType,
                                       DirectiveCallback newCallback) {
        final Pair key = new Pair(block, memberType);
        return (DirectiveCallback) callback.put(key, newCallback);
    }

    /**
     * Return the callback object associated with a given block and type.
     **/
    static
    DirectiveCallback lookupCallback(String block, String memberType) {
        Pair key = new Pair(block, memberType);
        DirectiveCallback cb = (DirectiveCallback) callback.get(key);
        if (cb == null) {
            key = new Pair(null, memberType);
            cb = (DirectiveCallback) callback.get(key);
        }
        return cb;
    }

    /**
     * Register a nonparameterized directive.
     * @param block Block in which the directive is valid.
     * @param key Name of the directive.
     * @param valueType Type of the value.
     * @param defval Default value.  Use <code>null</code> is no default value.
     **/
    public static
    void registerDirective(String block, String key,
                           String valueType, Object defval) {
        directive.put(new Pair(block, key), new Pair(valueType, defval));
    }

    /**
     * Register a parameterized directive.
     * @param block Block in which the directive is valid.
     * @param key Name of the directive.
     * @param memberType Type of the parameter.
     * @param valueType Type of the value.
     * @param defval Default value.  Use <code>null</code> is no default value.
     **/
    public static
    void registerDirective(String block, String key, String memberType,
                           String valueType, Object defval) {
        paramDirective.put(new Pair(block, key),
                           new Triplet(memberType, valueType, defval));
    }

    /**
     * Return a nonparameterized directive given the block and its name.
     **/
    public static
    Pair lookupDirective(String block, String key) {
        return (Pair) directive.get(new Pair(block, key));
    }

    /**
     * Return a parameterized directive given the block and its name.
     **/
    public static
    Triplet[] lookupParameterizedDirective(String block, String key) {
        Collection c = paramDirective.get(new Pair(block, key));
        if (c == null) return null;
        return (Triplet[]) c.toArray(new Triplet[0]);
    }

    public static String arrayify(final String type) {
        return type + "[]";
    }

    public static boolean isArray(final String type) {
        return type.endsWith("[]");
    }

    public static String deArray(final String type) {
        return type.substring(0, type.lastIndexOf("[]"));
    }
}
