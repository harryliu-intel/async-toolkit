/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cosim;

import com.avlsi.fast.metaparameters.MetaParamDefinition;
import com.avlsi.tools.sigscan.DebugOpts;

/**
 * Class to wrap up all non-channel arguments to devices.  Fields can
 * be added to this class without breaking existing devices.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/

public class DeviceParameters {

    /** The instance name of the device. **/
    private final String name;

    /** Debugging options controlling logging. **/
    private final DebugOpts debugOpts;

    /** Array of metaparameters. **/
    private final MetaParamDefinition[] metaParameters;

    /**
     * Mode of arbitration. One of
     * <code>com.avlsi.tools.tsim.Arbiter.NON_LINKED</code>,
     * <code>com.avlsi.tools.tsim.Arbiter.LINKED_SLAVE</code>, or
     * <code>com.avlsi.tools.tsim.Arbiter.LINKED_MASTER</code>.
     * See {@see com.avlsi.tools.tsim.Arbiter}.
     **/
    private final int arbitrationMode;

    /**
     * An initial random seed.
     **/
    private final long seed;

    /**
     * A scale factor to apply to digital delays.
     **/
    private final float digitalTau;

    /**
     * Legacy class constructor.
     *
     * @param name  instance name of device
     * @param suppressOutput  whether output should be suppressed
     * @param metaParameters  array of metaparameters
     * @param arbitrationMode  Mode of arbitration. One of
     *     <code>com.avlsi.tools.tsim.Arbiter.NON_LINKED</code>,
     *     <code>com.avlsi.tools.tsim.Arbiter.LINKED_SLAVE</code>, or
     *     <code>com.avlsi.tools.tsim.Arbiter.LINKED_MASTER</code>.
     *     See {@see com.avlsi.tools.tsim.Arbiter}.
     **/
    public DeviceParameters(
            final String name,
            final boolean suppressOutput,
            final MetaParamDefinition[] metaParameters,
            final int arbitrationMode) {
        this(name, suppressOutput, metaParameters, arbitrationMode, 1);
    }

    /**
     * Legacy class constructor.
     *
     * @param name  instance name of device
     * @param suppressOutput  whether output should be suppressed
     * @param metaParameters  array of metaparameters
     * @param arbitrationMode  Mode of arbitration. One of
     *     <code>com.avlsi.tools.tsim.Arbiter.NON_LINKED</code>,
     *     <code>com.avlsi.tools.tsim.Arbiter.LINKED_SLAVE</code>, or
     *     <code>com.avlsi.tools.tsim.Arbiter.LINKED_MASTER</code>.
     *     See {@see com.avlsi.tools.tsim.Arbiter}.
     * @param seed Initial random seed to use.
     **/
    public DeviceParameters(
            final String name,
            final boolean suppressOutput,
            final MetaParamDefinition[] metaParameters,
            final int arbitrationMode,
            final long seed) {
        this(name, suppressOutput, metaParameters, arbitrationMode, seed, 1.0f);
    }

    /**
     * Legacy class constructor.
     *
     * @param name  instance name of device
     * @param suppressOutput  whether output should be suppressed
     * @param metaParameters  array of metaparameters
     * @param arbitrationMode  Mode of arbitration. One of
     *     <code>com.avlsi.tools.tsim.Arbiter.NON_LINKED</code>,
     *     <code>com.avlsi.tools.tsim.Arbiter.LINKED_SLAVE</code>, or
     *     <code>com.avlsi.tools.tsim.Arbiter.LINKED_MASTER</code>.
     *     See {@see com.avlsi.tools.tsim.Arbiter}.
     * @param seed Initial random seed to use.
     * @param digitalTau Scale factor to apply to digital delays.
     **/
    public DeviceParameters(
            final String name,
            final boolean suppressOutput,
            final MetaParamDefinition[] metaParameters,
            final int arbitrationMode,
            final long seed,
            final float digitalTau) {
        this(name, new DebugOpts(!suppressOutput), metaParameters,
                arbitrationMode, seed, digitalTau);
    }

    /**
     * Class constructor.
     *
     * @param name  instance name of device
     * @param suppressOutput  whether output should be suppressed
     * @param metaParameters  array of metaparameters
     * @param arbitrationMode  Mode of arbitration. One of
     *     <code>com.avlsi.tools.tsim.Arbiter.NON_LINKED</code>,
     *     <code>com.avlsi.tools.tsim.Arbiter.LINKED_SLAVE</code>, or
     *     <code>com.avlsi.tools.tsim.Arbiter.LINKED_MASTER</code>.
     *     See {@see com.avlsi.tools.tsim.Arbiter}.
     **/
    public DeviceParameters(
            final String name,
            final DebugOpts debugOpts,
            final MetaParamDefinition[] metaParameters,
            final int arbitrationMode) {
        this(name, debugOpts, metaParameters, arbitrationMode, 1);
    }
    /**
     * Class constructor.
     *
     * @param name  instance name of device
     * @param suppressOutput  whether output should be suppressed
     * @param metaParameters  array of metaparameters
     * @param arbitrationMode  Mode of arbitration. One of
     *     <code>com.avlsi.tools.tsim.Arbiter.NON_LINKED</code>,
     *     <code>com.avlsi.tools.tsim.Arbiter.LINKED_SLAVE</code>, or
     *     <code>com.avlsi.tools.tsim.Arbiter.LINKED_MASTER</code>.
     *     See {@see com.avlsi.tools.tsim.Arbiter}.
     * @param seed Initial random seed to use.
     **/
    public DeviceParameters(
            final String name,
            final DebugOpts debugOpts,
            final MetaParamDefinition[] metaParameters,
            final int arbitrationMode,
            final long seed) {
        this(name, debugOpts, metaParameters, arbitrationMode, seed, 1.0f);
    }

    /**
     * Class constructor.
     *
     * @param name  instance name of device
     * @param suppressOutput  whether output should be suppressed
     * @param metaParameters  array of metaparameters
     * @param arbitrationMode  Mode of arbitration. One of
     *     <code>com.avlsi.tools.tsim.Arbiter.NON_LINKED</code>,
     *     <code>com.avlsi.tools.tsim.Arbiter.LINKED_SLAVE</code>, or
     *     <code>com.avlsi.tools.tsim.Arbiter.LINKED_MASTER</code>.
     *     See {@see com.avlsi.tools.tsim.Arbiter}.
     * @param seed Initial random seed to use.
     * @param digitalTau Scale factor to apply to digital delays.
     **/
    public DeviceParameters(
            final String name,
            final DebugOpts debugOpts,
            final MetaParamDefinition[] metaParameters,
            final int arbitrationMode,
            final long seed,
            final float digitalTau) {
        this.name = name;
        this.debugOpts = debugOpts;
        this.metaParameters = metaParameters;
        this.arbitrationMode = arbitrationMode;
        this.seed = seed;
        this.digitalTau = digitalTau;
    }

    /**
     * Returns the instance name of the device.
     **/
    public String getName() {
        return name;
    }

    /**
     * Returns flag indicating if output should be suppressed.
     **/
    public boolean isOutputSuppressed() {
        return debugOpts.loggingScreen();
    }

    /**
     * Returns array of meta parameters.
     **/
    public MetaParamDefinition[] getMetaParameters() {
        return metaParameters;
    }

    /**
     * Returns the arbitration mode.  One of
     * <code>com.avlsi.tools.tsim.Arbiter.NON_LINKED</code>,
     * <code>com.avlsi.tools.tsim.Arbiter.LINKED_SLAVE</code>, or
     * <code>com.avlsi.tools.tsim.Arbiter.LINKED_MASTER</code>.
     * See {@see com.avlsi.tools.tsim.Arbiter}.
     **/
    public int getArbitrationMode() {
        return arbitrationMode;
    }

    /**
     * Return the random seed the device should use.
     **/
    public long getSeed() {
        return seed;
    }

    /**
     * Return the scale factor applied to digital delays.
     **/
    public float getDigitalTau() {
        return digitalTau;
    }
}
