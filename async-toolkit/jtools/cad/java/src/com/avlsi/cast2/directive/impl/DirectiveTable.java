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
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.directive.impl.DefaultCallback;
import com.avlsi.cast2.directive.impl.DirectiveCallback;
import com.avlsi.tools.cosim.spec.CoSimSpecCallback;

//imports specific for adding the different directives
import com.avlsi.fast.BlockInterface;


/**
 * A class that acts as a central registry for valid Directives.
 **/
public class DirectiveTable {
    protected static Map callback = new HashMap();
    protected static Map directive = new HashMap();
    protected static MultiMap paramDirective = new MultiMap();

    static {
        DirectiveCallback cb = new DefaultCallback();
        registerCallback(null, DirectiveConstants.INT_TYPE, cb);
        registerCallback(null, DirectiveConstants.BIGINT_TYPE, cb);
        registerCallback(null, DirectiveConstants.FLOAT_TYPE, cb);
        registerCallback(null, DirectiveConstants.DOUBLE_TYPE, cb);
        registerCallback(null, DirectiveConstants.BOOLEAN_TYPE, cb);
        registerCallback(null, DirectiveConstants.STRING_TYPE, cb);
        registerCallback(null, DirectiveConstants.NODE_TYPE, PrsCallback.getInstance());
        registerCallback(null, DirectiveConstants.HALFOP_TYPE, PrsCallback.getInstance());
        registerCallback(null, DirectiveConstants.UNCHECKED_NODE_TYPE, PrsCallback.getInstance());
        registerCallback(null, DirectiveConstants.UNCHECKED_HALFOP_TYPE, PrsCallback.getInstance());
        registerCallback(null, DirectiveConstants.DEEP_NODE_TYPE, PrsCallback.getInstance());
        registerCallback(null, DirectiveConstants.DEEP_HALFOP_TYPE, PrsCallback.getInstance());
        registerCallback(null, DirectiveConstants.RULE_TYPE, PrsCallback.getInstance());
        registerCallback(null, DirectiveConstants.DEEP_RULE_TYPE, PrsCallback.getInstance());
        registerCallback(null, DirectiveConstants.CHANNEL_TYPE, InstanceCallback.getInstance());
        registerCallback(null, DirectiveConstants.WIDE_CHANNEL_TYPE, CspCallback.getInstance());
        registerCallback(null, DirectiveConstants.POSSIBLY_WIDE_CHANNEL_TYPE, CspCallback.getInstance());
        registerCallback(null, DirectiveConstants.INSTANCE_TYPE, InstanceCallback.getInstance());
        registerCallback(null, DirectiveConstants.ARRAYED_INSTANCE_TYPE, InstanceCallback.getInstance());
        registerCallback(null, DirectiveConstants.LAYER_TYPE, LayerCallback.getInstance());
        registerCallback(null, DirectiveConstants.COSIM_SPEC_TYPE, CoSimSpecCallback.getInstance());
        registerCallback(null, DirectiveConstants.ALINT_SCENARIO_TYPE, PrsCallback.getInstance());

        /**
         * Register RTE directives and set default values
         */
        registerDirective(BlockInterface.ENV, DirectiveConstants.CYCLE_COUNT,
                          DirectiveConstants.INT_TYPE, null);
        registerDirective(BlockInterface.ENV,
                          DirectiveConstants.RESET_CYCLE_COUNT,
                          DirectiveConstants.INT_TYPE, null);

        registerDirective(BlockInterface.ENV, DirectiveConstants.CYCLE_NODE,
                          DirectiveConstants.NODE_TYPE, null);

        registerDirective(BlockInterface.ENV, DirectiveConstants.CYCLE_TIME,
                          DirectiveConstants.INT_TYPE, null);

        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.RTE_COSIM_SPEC,
                          DirectiveConstants.COSIM_SPEC_TYPE,
                          null);

        registerDirective(BlockInterface.ENV,
                          DirectiveConstants.RTE_COSIM_SPEC,
                          DirectiveConstants.COSIM_SPEC_TYPE,
                          null);

        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.RTE_ENV_COSIM_SPEC,
                          DirectiveConstants.COSIM_SPEC_TYPE,
                          null);

        registerDirective(BlockInterface.ENV,
                          DirectiveConstants.RTE_ENV_COSIM_SPEC,
                          DirectiveConstants.COSIM_SPEC_TYPE,
                          null);

        registerDirective(BlockInterface.ENV, DirectiveConstants.RTE_IGNORE,
                          DirectiveConstants.BOOLEAN_TYPE, Boolean.FALSE);

        registerDirective(BlockInterface.ENV, DirectiveConstants.ASPICE_IGNORE,
                          DirectiveConstants.BOOLEAN_TYPE, Boolean.FALSE);

        registerDirective(BlockInterface.ENV, DirectiveConstants.NTPC_SPEC,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          null);

        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.ASPICE_DIGITAL_CYCLES,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.INT_TYPE,
                          null);

        registerDirective(BlockInterface.ENV,
                          DirectiveConstants.ASPICE_DIGITAL_CYCLES,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.INT_TYPE,
                          null);

        registerDirective(BlockInterface.ENV,
                          DirectiveConstants.ASPICE_ANALOG_CYCLES,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.INT_TYPE,
                          null);

        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.ASPICE_ANALOG_CYCLES,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.INT_TYPE,
                          null);
        registerDirective(BlockInterface.ENV,
                          DirectiveConstants.ASPICE_TIME_MAX,
                          DirectiveConstants.FLOAT_TYPE,
                          null);

        registerDirective(BlockInterface.CELL, DirectiveConstants.NTPC_SCALING,
                          DirectiveConstants.FLOAT_TYPE, new Float(1));

        registerDirective(BlockInterface.ENV, DirectiveConstants.TIMED,
                          DirectiveConstants.BOOLEAN_TYPE, Boolean.FALSE);

        registerDirective(BlockInterface.ENV, DirectiveConstants.TIMED_JITTER,
                          DirectiveConstants.FLOAT_TYPE, null);

        registerDirective(BlockInterface.CELL, DirectiveConstants.TIMED,
                          DirectiveConstants.BOOLEAN_TYPE, Boolean.FALSE);
        
        registerDirective(BlockInterface.CELL, DirectiveConstants.TIMED_JITTER,
                          DirectiveConstants.FLOAT_TYPE, null);
        
        registerDirective(BlockInterface.ENV,
                          DirectiveConstants.CSP_COSIM_WILL_FAIL,
                          DirectiveConstants.BOOLEAN_TYPE, Boolean.FALSE);

        /**
         * Register top-level directives used by rte and jauto
         **/
        registerDirective(BlockInterface.CELL, DirectiveConstants.FRAGMENT,
                          DirectiveConstants.BOOLEAN_TYPE, Boolean.FALSE);

        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.CELLNONOBSERVABLE,
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);

        registerDirective(BlockInterface.CELL, DirectiveConstants.FLOORPLAN,
                          DirectiveConstants.BOOLEAN_TYPE, Boolean.TRUE);

        registerDirective(BlockInterface.CELL, DirectiveConstants.SYNCHRONOUS,
                          DirectiveConstants.BOOLEAN_TYPE, Boolean.FALSE);

        registerDirective(BlockInterface.CELL, DirectiveConstants.UNIMPL,
                          DirectiveConstants.BOOLEAN_TYPE, Boolean.FALSE);

        registerDirective(BlockInterface.CELL, DirectiveConstants.RTE_IGNORE,
                          DirectiveConstants.BOOLEAN_TYPE, Boolean.FALSE);

        registerDirective(BlockInterface.CELL, DirectiveConstants.ASPICE_IGNORE,
                          DirectiveConstants.BOOLEAN_TYPE, Boolean.FALSE);

        registerDirective(BlockInterface.CELL, DirectiveConstants.SIGNOFF,
                          DirectiveConstants.BOOLEAN_TYPE, Boolean.FALSE);

        /* Arguably, this should be inside the netlist block, but it would make
         * parsing more difficult.  In any case, this directive is only
         * effective when specified in a cell with a netlist block. */
        registerDirective(BlockInterface.CELL, DirectiveConstants.FIXED_SIZE,
                          DirectiveConstants.BOOLEAN_TYPE, Boolean.TRUE);
        registerDirective(BlockInterface.CELL, DirectiveConstants.CDLSCALE,
                          DirectiveConstants.FLOAT_TYPE, new Float(1.0));

        registerDirective(BlockInterface.CELL, DirectiveConstants.WIRING,
                          DirectiveConstants.BOOLEAN_TYPE, Boolean.FALSE);

        /** Jauto wiring directives **/
        registerDirective(BlockInterface.PRS, DirectiveConstants.MIN_WIRELENGTH,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(-1.0));
        registerDirective(BlockInterface.PRS, DirectiveConstants.MIN_WIRESPAN,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(-1.0));
        registerDirective(BlockInterface.PRS, DirectiveConstants.WIRELENGTH,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(-1.0));
        registerDirective(BlockInterface.PRS, DirectiveConstants.WIRESPAN,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(-1.0));
        registerDirective(BlockInterface.PRS, DirectiveConstants.WIREWIDTH,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(-1.0));
        registerDirective(BlockInterface.PRS, DirectiveConstants.WIRESPACE,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(-1.0));

        registerDirective(BlockInterface.SUBCELL, DirectiveConstants.MIN_WIRELENGTH,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(-1.0));
        registerDirective(BlockInterface.SUBCELL, DirectiveConstants.MIN_WIRESPAN,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(-1.0));
        registerDirective(BlockInterface.SUBCELL, DirectiveConstants.WIRELENGTH,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(-1.0));
        registerDirective(BlockInterface.SUBCELL, DirectiveConstants.WIRESPAN,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(-1.0));
        registerDirective(BlockInterface.SUBCELL, DirectiveConstants.WIREWIDTH,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(-1.0));
        registerDirective(BlockInterface.SUBCELL, DirectiveConstants.WIRESPACE,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(-1.0));

        registerDirective(BlockInterface.CELL, DirectiveConstants.MIN_WIRELENGTH,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(-1.0));
        registerDirective(BlockInterface.CELL, DirectiveConstants.MIN_WIRESPAN,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(-1.0));
        registerDirective(BlockInterface.CELL, DirectiveConstants.WIRELENGTH,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(-1.0));
        registerDirective(BlockInterface.CELL, DirectiveConstants.WIRESPAN,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(-1.0));
        registerDirective(BlockInterface.CELL, DirectiveConstants.WIRESPACE,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(-1.0) );
        registerDirective(BlockInterface.CELL, DirectiveConstants.WIREWIDTH,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(-1.0));

        registerDirective(BlockInterface.CELL, DirectiveConstants.MIN_WIRELENGTH,
                          DirectiveConstants.FLOAT_TYPE, new Float(-1.0));
        registerDirective(BlockInterface.CELL, DirectiveConstants.MIN_WIRESPAN,
                          DirectiveConstants.FLOAT_TYPE, new Float(-1.0));
        registerDirective(BlockInterface.CELL, DirectiveConstants.WIRELENGTH,
                          DirectiveConstants.FLOAT_TYPE, new Float(-1.0));
        registerDirective(BlockInterface.CELL, DirectiveConstants.WIRESPAN,
                          DirectiveConstants.FLOAT_TYPE, new Float(-1.0));
        registerDirective(BlockInterface.CELL, DirectiveConstants.WIREWIDTH,
                          DirectiveConstants.FLOAT_TYPE, new Float(-1.0));
        registerDirective(BlockInterface.CELL, DirectiveConstants.WIRESPACE,
                          DirectiveConstants.FLOAT_TYPE, new Float(-1.0));

        /** other sizing/simulation directives **/
        registerDirective(BlockInterface.CELL, DirectiveConstants.HEIGHT,
                          DirectiveConstants.FLOAT_TYPE, new Float(-1.0));
        registerDirective(BlockInterface.CELL, DirectiveConstants.WIDTH,
                          DirectiveConstants.FLOAT_TYPE, new Float(-1.0));
        registerDirective(BlockInterface.CELL, DirectiveConstants.SPLITTABLE,
                          DirectiveConstants.BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(BlockInterface.CELL, DirectiveConstants.DELAYBIAS,
                          DirectiveConstants.FLOAT_TYPE, new Float(1.0));
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.DEFAULT_UP_DELAY,
                          DirectiveConstants.FLOAT_TYPE, new Float(100));
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.DEFAULT_DN_DELAY,
                          DirectiveConstants.FLOAT_TYPE, new Float(100));
        registerDirective(BlockInterface.CELL, DirectiveConstants.OWNER,
                          DirectiveConstants.STRING_TYPE, null);
        registerDirective(BlockInterface.CELL, DirectiveConstants.PASSGATE,
                          DirectiveConstants.STRING_TYPE,
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);
        registerDirective(BlockInterface.CELL, DirectiveConstants.STACK_GATE,
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.STATICIZER_GATE,
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);
        registerDirective(BlockInterface.CELL, DirectiveConstants.TAU,
                          DirectiveConstants.FLOAT_TYPE,
                          null);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.LAYOUT_ATTRIBUTES,
                          DirectiveConstants.STRING_TYPE,
                          null);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.EXTRA_LAYOUT_ATTRIBUTES,
                          DirectiveConstants.STRING_TYPE,
                          null);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.NETLIST_PRIMITIVE,
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.PRECHARGE_PRIMITIVE,
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.COVERAGE_IGNORE,
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.STATICIZER_RATIO,
                          DirectiveConstants.FLOAT_TYPE,
                          null);
        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.STATICIZER_RATIO,
                          DirectiveConstants.HALFOP_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          null);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.STATICIZER_TYPE,
                          DirectiveConstants.INT_TYPE,
                          null);
        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.STATICIZER_TYPE,
                          DirectiveConstants.HALFOP_TYPE,
                          DirectiveConstants.INT_TYPE,
                          new Integer(0));
        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.STRENGTH_GROUP,
                          DirectiveConstants.HALFOP_TYPE,
                          DirectiveConstants.INT_TYPE,
                          null);

        /*
         * Register PRS synthesis directives; used by jauto "p2n", and PrsToNet
         * "mgn", and lvs, "nvp".
         */
        /* this next one should probably be a half operators. */
        registerDirective(BlockInterface.PRS, DirectiveConstants.NO_STAT,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);

        registerDirective(BlockInterface.PRS, DirectiveConstants.DELAYBIAS,
                          DirectiveConstants.HALFOP_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(1.0));

        registerDirective(BlockInterface.SUBCELL, DirectiveConstants.DELAYBIAS,
                          DirectiveConstants.INSTANCE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(1.0));

        registerDirective(BlockInterface.PRS, DirectiveConstants.EXTRA_DELAY,
                          DirectiveConstants.HALFOP_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0.0));

        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.EXTRA_DELAY,
                          DirectiveConstants.HALFOP_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0.0));

        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.ASTA_EXTRA_DELAY,
                          DirectiveConstants.HALFOP_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0.0));

        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.ASTA_EXTRA_DELAY,
                          DirectiveConstants.HALFOP_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0.0));

        registerDirective(BlockInterface.PRS, DirectiveConstants.SYMMETRIZE,
                          DirectiveConstants.HALFOP_TYPE,
                          DirectiveConstants.INT_TYPE,
                          null);

        registerDirective(BlockInterface.PRS, DirectiveConstants.SHARED,
                          DirectiveConstants.HALFOP_TYPE,
                          DirectiveConstants.INT_TYPE,
                          null);

        registerDirective(BlockInterface.PRS, DirectiveConstants.CUTPATH,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);

        registerDirective(BlockInterface.SUBCELL, DirectiveConstants.CUTPATH,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);

        registerDirective(BlockInterface.PRS, DirectiveConstants.LOADCAP,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(-1.0));

        registerDirective(BlockInterface.SUBCELL, DirectiveConstants.LOADCAP,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(-1.0));

        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.RESISTANCE_SCALE,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(1.0));
        registerDirective(BlockInterface.PRS, DirectiveConstants.MINDELAY,
                          DirectiveConstants.HALFOP_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          null);
        registerDirective(BlockInterface.PRS, DirectiveConstants.STRENGTHBIAS,
                          DirectiveConstants.HALFOP_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          null);
        registerDirective(BlockInterface.PRS, DirectiveConstants.CAP,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0.0));  // power estimate in DSim, bug 2154
        registerDirective(BlockInterface.SUBCELL, DirectiveConstants.CAP,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0.0));  // power estimate in DSim, bug 2154
        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.ESTIMATED_DELAY,
                          DirectiveConstants.HALFOP_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0.0));  // delay estimate in DSim, bug 2156
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.ESTIMATED_DELAY,
                          DirectiveConstants.HALFOP_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0.0));  // delay estimate in DSim, bug 2156
        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.MEASURED_DELAY,
                          arrayify(DirectiveConstants.UNCHECKED_HALFOP_TYPE),
                          arrayify(arrayify(DirectiveConstants.FLOAT_TYPE)),
                          new Float(0.0));
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.MEASURED_DELAY,
                          arrayify(DirectiveConstants.UNCHECKED_HALFOP_TYPE),
                          arrayify(arrayify(DirectiveConstants.FLOAT_TYPE)),
                          new Float(0.0));
        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.MEASURED_CHARGE,
                          arrayify(DirectiveConstants.UNCHECKED_HALFOP_TYPE),
                          arrayify(arrayify(DirectiveConstants.FLOAT_TYPE)),
                          new Float(0.0));
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.MEASURED_CHARGE,
                          arrayify(DirectiveConstants.UNCHECKED_HALFOP_TYPE),
                          arrayify(arrayify(DirectiveConstants.FLOAT_TYPE)),
                          new Float(0.0));
        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.ESTIMATED_CAP,
                          DirectiveConstants.UNCHECKED_NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0.0));
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.ESTIMATED_CAP,
                          DirectiveConstants.UNCHECKED_NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0.0));
        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.MEASURED_CAP,
                          DirectiveConstants.UNCHECKED_NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0.0));
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.MEASURED_CAP,
                          DirectiveConstants.UNCHECKED_NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0.0));
        registerDirective(BlockInterface.CELL, DirectiveConstants.DEFAULT_SLEW,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(30.0));
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.PRS_NETLIST_MISMATCH_OK,
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);
        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.PRS_NETLIST_MISMATCH_OK,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);
        registerDirective(BlockInterface.CELL, DirectiveConstants.JAUTO_IGNORE,
                          DirectiveConstants.BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.SIGNOFF_CONSTANT,
                          DirectiveConstants.DEEP_NODE_TYPE,
                          DirectiveConstants.INT_TYPE,
                          new Integer(0));
        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.SIGNOFF_CONSTANT,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.INT_TYPE,
                          new Integer(0));
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.UNUSED_PRS,
                          DirectiveConstants.DEEP_RULE_TYPE,
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);
        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.UNUSED_PRS,
                          DirectiveConstants.RULE_TYPE,
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.EXTERNAL_WIRELENGTH,
                          DirectiveConstants.FLOAT_TYPE,
                          null);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.EXTERNAL_WIRELENGTH,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          null);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.EXTERNAL_LOADCAP,
                          DirectiveConstants.FLOAT_TYPE,
                          null);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.EXTERNAL_LOADCAP,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          null);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.EXTERNAL_DRIVER,
                          DirectiveConstants.STRING_TYPE,
                          null);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.EXTERNAL_DRIVER,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.STRING_TYPE,
                          null);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.INTERNAL_WIRELENGTH,
                          DirectiveConstants.FLOAT_TYPE,
                          null);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.INTERNAL_WIRELENGTH,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          null);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.INTERNAL_LOADCAP,
                          DirectiveConstants.FLOAT_TYPE,
                          null);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.INTERNAL_LOADCAP,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          null);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.INTERNAL_DRIVER,
                          DirectiveConstants.STRING_TYPE,
                          null);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.INTERNAL_DRIVER,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.STRING_TYPE,
                          null);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.NAME_MAPPING,
                          DirectiveConstants.STRING_TYPE,
                          null);
        registerDirective(BlockInterface.VERILOG,
                          DirectiveConstants.NAME_MAPPING,
                          DirectiveConstants.STRING_TYPE,
                          null);
        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.IDLE_STATE,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.INT_TYPE,
                          null);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.IDLE_STATE,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.INT_TYPE,
                          null);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.NUM_VALUES,
                          DirectiveConstants.BIGINT_TYPE,
                          null);

        /**
         * Register Directives for Automated Layout.  Used by autopins, autobus
         **/

        registerDirective(BlockInterface.CELL, DirectiveConstants.PINTYPE,
                          DirectiveConstants.STRING_TYPE,
                          DirectiveConstants.PINTYPE_PITCHED );
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.CHANNEL_BUNCHED,
                          DirectiveConstants.CHANNEL_TYPE,
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);
                        
        registerDirective(BlockInterface.CELL, DirectiveConstants.BITPITCH,
                          DirectiveConstants.FLOAT_TYPE, new Float(-1.0) );
        registerDirective(BlockInterface.CELL, DirectiveConstants.PINLAYER,
                          DirectiveConstants.LAYER_TYPE, null );
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.POWERGRID_GNDNET,
                          DirectiveConstants.STRING_TYPE, null );
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.POWERGRID_VDDNET,
                          DirectiveConstants.STRING_TYPE, null );

        registerDirective(BlockInterface.PRS, DirectiveConstants.ANTENNA_DIODE,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(-1.0) );
        registerDirective(BlockInterface.SUBCELL, DirectiveConstants.ANTENNA_DIODE,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(-1.0) );

        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.LAYER_WIREPITCH,
                          DirectiveConstants.LAYER_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(-1.0) );
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.LAYER_WIRESPACING,
                          DirectiveConstants.LAYER_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(-1.0) );
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.LAYER_WIREWIDTH,
                          DirectiveConstants.LAYER_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(-1.0) );

        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.LAYER_POWERGRID_OFFSET,
                          DirectiveConstants.LAYER_TYPE,
                          DirectiveConstants.INT_TYPE,
                          new Integer(0) );
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.LAYER_POWERGRID_SPACING,
                          DirectiveConstants.LAYER_TYPE,
                          DirectiveConstants.INT_TYPE,
                          new Integer(-1) );
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.LAYER_POWERGRID_WIREWIDTH,
                          DirectiveConstants.LAYER_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(-1.0) );
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.LAYER_KEEPOUT_PATTERN,
                          DirectiveConstants.LAYER_TYPE,
                          DirectiveConstants.STRING_TYPE,
                          "0" );
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.AUTO_LAYOUT,
                          DirectiveConstants.INT_TYPE,
                          new Integer(0));
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.RESET_NET,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.RESET_NET,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.FLOORPLAN_ARRAY,
                          DirectiveConstants.ARRAYED_INSTANCE_TYPE,
                          DirectiveConstants.STRING_TYPE,
                          null);

        /** updatenetlist directives **/
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.BASE_TRANSISTOR_LAYOUT_WIDTH,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0.0));
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.DENSITY_FACTOR,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(1.0));
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.DENSITY_SCALE_FACTOR,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(1.0));
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.INLINE_LAYOUT,
                          DirectiveConstants.INSTANCE_TYPE,
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.INLINE_LAYOUT,
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.ROUTED,
                          DirectiveConstants.BOOLEAN_TYPE,
                          null);
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.ROUTED,
                          DirectiveConstants.INSTANCE_TYPE,
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.TRUE);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.BOUNDARY_POSITION,
                          arrayify(DirectiveConstants.FLOAT_TYPE),
                          null);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.WIDTH_MINIMUM,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0.0));
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.HEIGHT_MINIMUM,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0.0));
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.WIDTH_INCREMENT,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0.0));
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.HEIGHT_INCREMENT,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0.0));
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.WIDTH_OVERHEAD,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0.0));
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.HEIGHT_OVERHEAD,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0.0));
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.BOUNDARY_SMALL_CELL_WARNING_CUTOFF,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0.0));

        /** LVE Signoff Directives **/
        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.ESTIMATED_DELAY_SIGNOFF,
                          DirectiveConstants.HALFOP_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0.0));
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.ESTIMATED_DELAY_SIGNOFF,
                          DirectiveConstants.HALFOP_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0.0));
        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.SLEW_SIGNOFF,
                          DirectiveConstants.HALFOP_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0.0));
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.SLEW_SIGNOFF,
                          DirectiveConstants.HALFOP_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0.0));
        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.SKEW_SIGNOFF,
                          DirectiveConstants.HALFOP_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0.0));
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.SKEW_SIGNOFF,
                          DirectiveConstants.HALFOP_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0.0));
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.NTPC_SCALING_SIGNOFF,
                          DirectiveConstants.FLOAT_TYPE, new Float(1));
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.ALINT_SIGNOFF,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);
        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.ALINT_SIGNOFF,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);
        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.STATICIZER_RATIO_SIGNOFF,
                          DirectiveConstants.HALFOP_TYPE,
                          arrayify(DirectiveConstants.FLOAT_TYPE),
                          null);
        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.BUMP_SIGNOFF,
                          DirectiveConstants.HALFOP_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0));
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.BUMP_SIGNOFF,
                          DirectiveConstants.HALFOP_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0));
        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.THRESH_BUMP_SIGNOFF,
                          DirectiveConstants.HALFOP_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0));
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.THRESH_BUMP_SIGNOFF,
                          DirectiveConstants.HALFOP_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0));
        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.LEAKAGE_SIGNOFF,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0));
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.LEAKAGE_SIGNOFF,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0));
        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.ALINT_MAX_BUMP_FANIN,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.INT_TYPE,
                          new Integer(-1));
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.ALINT_MAX_BUMP_FANIN,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.INT_TYPE,
                          new Integer(-1));
        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.ALINT_DEFAULT_SCENARIOS,
                          DirectiveConstants.HALFOP_TYPE,
                          DirectiveConstants.BOOLEAN_TYPE,
                          null);
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.ALINT_DEFAULT_SCENARIOS,
                          DirectiveConstants.HALFOP_TYPE,
                          DirectiveConstants.BOOLEAN_TYPE,
                          null);
        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.ALINT_SCENARIO,
                          DirectiveConstants.HALFOP_TYPE,
                          arrayify(DirectiveConstants.ALINT_SCENARIO_TYPE),
                          null);
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.ALINT_SCENARIO,
                          DirectiveConstants.HALFOP_TYPE,
                          arrayify(DirectiveConstants.ALINT_SCENARIO_TYPE),
                          null);
        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.ALINT_DELAY_SCENARIO,
                          DirectiveConstants.HALFOP_TYPE,
                          arrayify(DirectiveConstants.ALINT_SCENARIO_TYPE),
                          null);
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.ALINT_DELAY_SCENARIO,
                          DirectiveConstants.HALFOP_TYPE,
                          arrayify(DirectiveConstants.ALINT_SCENARIO_TYPE),
                          null);
        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.ALINT_LEAK_SCENARIO,
                          DirectiveConstants.HALFOP_TYPE,
                          arrayify(DirectiveConstants.ALINT_SCENARIO_TYPE),
                          null);
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.ALINT_LEAK_SCENARIO,
                          DirectiveConstants.HALFOP_TYPE,
                          arrayify(DirectiveConstants.ALINT_SCENARIO_TYPE),
                          null);
        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.ALINT_BUMP_SCENARIO,
                          DirectiveConstants.HALFOP_TYPE,
                          arrayify(DirectiveConstants.ALINT_SCENARIO_TYPE),
                          null);
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.ALINT_BUMP_SCENARIO,
                          DirectiveConstants.HALFOP_TYPE,
                          arrayify(DirectiveConstants.ALINT_SCENARIO_TYPE),
                          null);
        registerDirective(BlockInterface.CELL,DirectiveConstants.ALINT_IGNORE,
                          DirectiveConstants.BOOLEAN_TYPE,Boolean.FALSE);

        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.ACTIVITY_FACTOR,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(-1.0));
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.ACTIVITY_FACTOR,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(-1.0));
        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.DC_WIRING_SPEC,
                          DirectiveConstants.NODE_TYPE,
                          arrayify(DirectiveConstants.FLOAT_TYPE),
                          new Float(-1.0));
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.DC_WIRING_SPEC,
                          DirectiveConstants.NODE_TYPE,
                          arrayify(DirectiveConstants.FLOAT_TYPE),
                          new Float(-1.0));
        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.AC_WIRING_SPEC,
                          DirectiveConstants.NODE_TYPE,
                          arrayify(DirectiveConstants.FLOAT_TYPE),
                          new Float(-1.0));
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.AC_WIRING_SPEC,
                          DirectiveConstants.NODE_TYPE,
                          arrayify(DirectiveConstants.FLOAT_TYPE),
                          new Float(-1.0));
        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.LEAKY,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.BOOLEAN_TYPE, Boolean.FALSE);

        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.LVS_NODES,
                          arrayify(DirectiveConstants.STRING_TYPE), null);

        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.EFFECTIVE_RESISTANCE,
                          DirectiveConstants.HALFOP_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          null);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.INTRINSIC_DELAY,
                          DirectiveConstants.HALFOP_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          null);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.INTRINSIC_CAPACITANCE,
                          DirectiveConstants.HALFOP_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          null);
        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.TRANSISTOR_TYPE,
                          DirectiveConstants.HALFOP_TYPE,
                          DirectiveConstants.INT_TYPE,
                          null);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.TRANSISTOR_TYPE,
                          DirectiveConstants.INT_TYPE,
                          new Integer(1));
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.FAST_TRANSISTOR_TYPE,
                          DirectiveConstants.INT_TYPE,
                          new Integer(1));
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.SLOW_TRANSISTOR_TYPE,
                          DirectiveConstants.INT_TYPE,
                          new Integer(1));
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.TRANSISTOR_NAME,
                          DirectiveConstants.INT_TYPE,
                          DirectiveConstants.STRING_TYPE,
                          null);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.PLUS_NODE,
                          DirectiveConstants.NODE_TYPE,
                          HierName.makeHierName("Vdd"));
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.MINUS_NODE,
                          DirectiveConstants.NODE_TYPE,
                          HierName.makeHierName("GND"));
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.EXCLCC,
                          arrayify(DirectiveConstants.UNCHECKED_NODE_TYPE),
                          DirectiveConstants.INT_TYPE,
                          null);
        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.EXCLCC,
                          arrayify(DirectiveConstants.UNCHECKED_NODE_TYPE),
                          DirectiveConstants.INT_TYPE,
                          null);
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.NOCC_NODES,
                          arrayify(DirectiveConstants.UNCHECKED_NODE_TYPE),
                          null);
        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.NOCC_NODES,
                          arrayify(DirectiveConstants.UNCHECKED_NODE_TYPE),
                          null);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.PHANTOM_PORT_SIGNOFF,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.BOOLEAN_TYPE,
                          null);
        registerDirective(BlockInterface.CSP,
                          DirectiveConstants.DEFAULT_INTEGER_WIDTH,
                          DirectiveConstants.INT_TYPE,
                          null);
        registerDirective(BlockInterface.CSP,
                          DirectiveConstants.SYNTHESIZABLE,
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);

        /** CSP timing directives */
        registerDirective(BlockInterface.CSP,
                          DirectiveConstants.SLACK,
                          DirectiveConstants.WIDE_CHANNEL_TYPE,
                          DirectiveConstants.INT_TYPE,
                          null);
        registerDirective(BlockInterface.CSP,
                          DirectiveConstants.DYNAMIC_SLACK,
                          DirectiveConstants.WIDE_CHANNEL_TYPE,
                          DirectiveConstants.INT_TYPE,
                          null);
        registerDirective(BlockInterface.CSP,
                          DirectiveConstants.STAGES,
                          DirectiveConstants.WIDE_CHANNEL_TYPE,
                          DirectiveConstants.INT_TYPE,
                          null);
        registerDirective(BlockInterface.CSP,
                          DirectiveConstants.LATENCY,
                          DirectiveConstants.WIDE_CHANNEL_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          null);
        registerDirective(BlockInterface.CSP,
                          DirectiveConstants.LATENCY_PER_STAGE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(2.0));
        registerDirective(BlockInterface.CSP,
                          DirectiveConstants.SLACK_PER_STAGE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0.5));
        registerDirective(BlockInterface.CSP,
                          DirectiveConstants.CYCLE_TIME,
                          DirectiveConstants.WIDE_CHANNEL_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          null);
        registerDirective(BlockInterface.CSP,
                          DirectiveConstants.CYCLE_TIME_IN,
                          DirectiveConstants.WIDE_CHANNEL_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          null);
        registerDirective(BlockInterface.CSP,
                          DirectiveConstants.CYCLE_TIME_OUT,
                          DirectiveConstants.WIDE_CHANNEL_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          null);
        registerDirective(BlockInterface.CSP,
                          DirectiveConstants.DEFAULT_CYCLE_TIME,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(18));
        registerDirective(BlockInterface.CSP,
                          DirectiveConstants.FB,
                          DirectiveConstants.WIDE_CHANNEL_TYPE,
                          DirectiveConstants.FLOAT_TYPE, null);
        registerDirective(BlockInterface.CSP,
                          DirectiveConstants.FB_NEUTRAL,
                          DirectiveConstants.WIDE_CHANNEL_TYPE,
                          DirectiveConstants.FLOAT_TYPE, null);
        registerDirective(BlockInterface.CSP,
                          DirectiveConstants.FB_VALID,
                          DirectiveConstants.WIDE_CHANNEL_TYPE,
                          DirectiveConstants.FLOAT_TYPE, null);
        registerDirective(BlockInterface.CSP,
                          DirectiveConstants.BF,
                          DirectiveConstants.WIDE_CHANNEL_TYPE,
                          DirectiveConstants.FLOAT_TYPE, null);
        registerDirective(BlockInterface.CSP,
                          DirectiveConstants.TIME_UNIT,
                          DirectiveConstants.INT_TYPE,
                          new Integer(100));

        // see bug 6085
        registerDirective(BlockInterface.CSP,
                          DirectiveConstants.STRICT_VARS,
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);

        registerDirective(BlockInterface.CSP,
                          DirectiveConstants.DISABLE_CHANDFT_HANDLER,
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);

        registerDirective(BlockInterface.CSP,
                          DirectiveConstants.IMPLICIT_INIT,
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.TRUE);

        /** Proteus directives */
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.ASYNC_SCAN_INPUTS,
                          DirectiveConstants.BOOLEAN_TYPE,
                          null);
        
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.ASYNC_SCAN_OUTPUTS,
                          DirectiveConstants.BOOLEAN_TYPE,
                          null);
        
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.ASYNC_SCAN,
                          DirectiveConstants.POSSIBLY_WIDE_CHANNEL_TYPE,
                          DirectiveConstants.BOOLEAN_TYPE,
                          null);

        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.PROTEUS_SCAN_MODEL,
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);

        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.PROTEUS_SCAN_MACROS,
                          DirectiveConstants.BOOLEAN_TYPE,
                          null);

        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.PROTEUS_SCAN_DECLONE,
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);

        /** Slacker directives */
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.SLACKER_LEAF,
                          DirectiveConstants.BOOLEAN_TYPE,
                          null);

        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.SLACKER_PRIMITIVE,
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);

        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.SLACKER_NTPC,
                          DirectiveConstants.FLOAT_TYPE,
                          null);

        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.SLACKER_IS_SLACK,
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);

        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.SLACKER_TIME,
                          DirectiveConstants.POSSIBLY_WIDE_CHANNEL_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0));

        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.SLACKER_FREE_SLACK,
                          DirectiveConstants.POSSIBLY_WIDE_CHANNEL_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0));
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.SLACKER_FREE_SLACK,
                          DirectiveConstants.POSSIBLY_WIDE_CHANNEL_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0));

        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.SLACKER_ELASTICITY,
                          DirectiveConstants.POSSIBLY_WIDE_CHANNEL_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0));

        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.SLACKER_HANDSHAKES,
                          DirectiveConstants.POSSIBLY_WIDE_CHANNEL_TYPE,
                          DirectiveConstants.INT_TYPE,
                          new Integer(0));

        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.SLACKER_INITIAL_TOKENS,
                          DirectiveConstants.POSSIBLY_WIDE_CHANNEL_TYPE,
                          DirectiveConstants.INT_TYPE,
                          new Integer(0));

        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.SLACKER_ALIGNMENT,
                          DirectiveConstants.POSSIBLY_WIDE_CHANNEL_TYPE,
                          DirectiveConstants.INT_TYPE,
                          new Integer(0));

        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.SLACKER_ALIGNMENT,
                          DirectiveConstants.INT_TYPE,
                          null);

        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.SLACKER_IGNORE,
                          DirectiveConstants.POSSIBLY_WIDE_CHANNEL_TYPE,
                          DirectiveConstants.BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.SLACKER_IGNORE,
                          DirectiveConstants.POSSIBLY_WIDE_CHANNEL_TYPE,
                          DirectiveConstants.BOOLEAN_TYPE, Boolean.FALSE);

        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.SLACKER_DONT_TOUCH,
                          DirectiveConstants.BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.SLACKER_DONT_TOUCH,
                          DirectiveConstants.POSSIBLY_WIDE_CHANNEL_TYPE,
                          DirectiveConstants.BOOLEAN_TYPE, Boolean.FALSE);
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.SLACKER_DONT_TOUCH,
                          DirectiveConstants.POSSIBLY_WIDE_CHANNEL_TYPE,
                          DirectiveConstants.BOOLEAN_TYPE, Boolean.FALSE);

        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.SLACKER_TRANSITIONS,
                          DirectiveConstants.POSSIBLY_WIDE_CHANNEL_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          null);

        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.SLACKER_COST,
                          DirectiveConstants.FLOAT_TYPE,
                          null);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.SLACKER_USE_EXTRA_DELAY,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);

        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.SUPPRESS_FAULTS,
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);

        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.PORT_GROUP,
                          DirectiveConstants.POSSIBLY_WIDE_CHANNEL_TYPE,
                          DirectiveConstants.STRING_TYPE,
                          null);

        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.GROUP_FREQUENCY,
                          DirectiveConstants.STRING_TYPE,
                          DirectiveConstants.FLOAT_TYPE, null);

        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.GROUP_CLOCK,
                          DirectiveConstants.STRING_TYPE,
                          DirectiveConstants.STRING_TYPE, null);

        registerDirective(BlockInterface.VERILOG,
                          DirectiveConstants.BOUNDS,
                          DirectiveConstants.STRING_TYPE,
                          arrayify(DirectiveConstants.INT_TYPE),
                          null);

        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.PROTEUS_TAG,
                          DirectiveConstants.STRING_TYPE, null);

        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.ASTA_BLACKBOX,
                          DirectiveConstants.BOOLEAN_TYPE,
                          null);

        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.ASTA_GRAYBOX,
                          DirectiveConstants.BOOLEAN_TYPE,
                          null);

        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.ASTA_IGNORE,
                          arrayify(DirectiveConstants.DEEP_HALFOP_TYPE),
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);
        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.ASTA_IGNORE,
                          arrayify(DirectiveConstants.DEEP_HALFOP_TYPE),
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.ASTA_IGNORE,
                          arrayify(DirectiveConstants.DEEP_HALFOP_TYPE),
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.SLINT_IGNORE,
                          arrayify(DirectiveConstants.DEEP_HALFOP_TYPE),
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);
        registerDirective(BlockInterface.PRS,
                          DirectiveConstants.SLINT_IGNORE,
                          arrayify(DirectiveConstants.DEEP_HALFOP_TYPE),
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);
        registerDirective(BlockInterface.SUBCELL,
                          DirectiveConstants.SLINT_IGNORE,
                          arrayify(DirectiveConstants.DEEP_HALFOP_TYPE),
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);
        
        /** VDCVerify directives */
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.VDC_LEAF,
                          DirectiveConstants.BOOLEAN_TYPE,
                          Boolean.FALSE);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.VDD_DEFAULT_DOMAIN,
                          DirectiveConstants.NODE_TYPE,
                          null);
        registerDirective(BlockInterface.CELL,
                          DirectiveConstants.VDD_DOMAIN,
                          DirectiveConstants.NODE_TYPE,
                          DirectiveConstants.NODE_TYPE,
                          null);

        /** Java block directives */
        registerDirective(BlockInterface.JAVA,
                          DirectiveConstants.GROUP,
                          DirectiveConstants.WIDE_CHANNEL_TYPE,
                          DirectiveConstants.STRING_TYPE,
                          null);
        registerDirective(BlockInterface.JAVA,
                          DirectiveConstants.SLACK,
                          DirectiveConstants.WIDE_CHANNEL_TYPE,
                          DirectiveConstants.INT_TYPE,
                          null);
        registerDirective(BlockInterface.JAVA,
                          DirectiveConstants.DYNAMIC_SLACK,
                          DirectiveConstants.WIDE_CHANNEL_TYPE,
                          DirectiveConstants.INT_TYPE,
                          null);
        registerDirective(BlockInterface.JAVA,
                          DirectiveConstants.STAGES,
                          DirectiveConstants.WIDE_CHANNEL_TYPE,
                          DirectiveConstants.INT_TYPE,
                          null);
        registerDirective(BlockInterface.JAVA,
                          DirectiveConstants.LATENCY,
                          DirectiveConstants.WIDE_CHANNEL_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          null);
        registerDirective(BlockInterface.JAVA,
                          DirectiveConstants.LATENCY_PER_STAGE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(2.0));
        registerDirective(BlockInterface.JAVA,
                          DirectiveConstants.SLACK_PER_STAGE,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(0.5));
        registerDirective(BlockInterface.JAVA,
                          DirectiveConstants.CYCLE_TIME,
                          DirectiveConstants.WIDE_CHANNEL_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          null);
        registerDirective(BlockInterface.JAVA,
                          DirectiveConstants.CYCLE_TIME_IN,
                          DirectiveConstants.WIDE_CHANNEL_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          null);
        registerDirective(BlockInterface.JAVA,
                          DirectiveConstants.CYCLE_TIME_OUT,
                          DirectiveConstants.WIDE_CHANNEL_TYPE,
                          DirectiveConstants.FLOAT_TYPE,
                          null);
        registerDirective(BlockInterface.JAVA,
                          DirectiveConstants.DEFAULT_CYCLE_TIME,
                          DirectiveConstants.FLOAT_TYPE,
                          new Float(18));
        registerDirective(BlockInterface.JAVA,
                          DirectiveConstants.FB,
                          DirectiveConstants.WIDE_CHANNEL_TYPE,
                          DirectiveConstants.FLOAT_TYPE, null);
        registerDirective(BlockInterface.JAVA,
                          DirectiveConstants.FB_NEUTRAL,
                          DirectiveConstants.WIDE_CHANNEL_TYPE,
                          DirectiveConstants.FLOAT_TYPE, null);
        registerDirective(BlockInterface.JAVA,
                          DirectiveConstants.FB_VALID,
                          DirectiveConstants.WIDE_CHANNEL_TYPE,
                          DirectiveConstants.FLOAT_TYPE, null);
        registerDirective(BlockInterface.JAVA,
                          DirectiveConstants.BF,
                          DirectiveConstants.WIDE_CHANNEL_TYPE,
                          DirectiveConstants.FLOAT_TYPE, null);
        registerDirective(BlockInterface.JAVA,
                          DirectiveConstants.TIME_UNIT,
                          DirectiveConstants.INT_TYPE,
                          new Integer(100));
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
