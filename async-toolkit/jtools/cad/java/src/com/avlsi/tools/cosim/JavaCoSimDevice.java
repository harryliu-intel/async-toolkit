/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.tools.cosim;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ListIterator;
import java.util.Vector;
import java.util.Map;

import com.avlsi.fast.metaparameters.MetaParamDefinition;
import com.avlsi.tools.tsim.ChannelInput;
import com.avlsi.tools.tsim.ChannelOutput;
import com.avlsi.tools.tsim.Startable;

/**
 * Wrapper for an individual device under the new java syntax.
 * Belongs to a JavaCoSimInfo.  Immutable.  Does not actually contain
 * or refine an AbstractDevice, only describes one.
 **/
public class JavaCoSimDevice {
    private final String className;
    private final String instanceName;
    /**
     * In the order they'll be passed to the AbstractDevice
     * constructor (ie, the order they were in the cast).  Can't be
     * null, only empty.
     **/
    private final MetaParamDefinition[] metaParameters;

    /**
     * <code>Vector&lt;JavaClassParameter&gt;</code> holds the parameters
     * that were specified in the java block of this device.
     * JavaClassParameter's are Strings
     * and (possibly multi-dimensional) arrays of Strings. These have
     * been through meta-parameter processing; there are no variables
     * left. They might describe array elements or arrays; arrays have
     * been broken down into their original channels/nodes (which are
     * placed in a CoSimChannelArrayInterface). They're in the order
     * they'll be passed to the AbstractDevice constructor (i.e., the
     * order specified in the original CAST). Can't be null or empty.
     **/
    private final Vector channelAndNodeNames;

    /**
     * In the order they'll be passed to AbstractDevice.enqueue().
     * This should only contain floats/ints/bools/strings
     * (BigDecimals, BigIntegers, Booleans, and Strings). Can be null.
     *
     * <p>Not yet used for anything.
     **/
    private final Vector initializers;

    /**
     * See comments on the member variables for format notation on
     * each of these.
     **/
    public JavaCoSimDevice(final String className,
                           final String instanceName,
                           final MetaParamDefinition[] metaParameters,
                           final Vector channelAndNodeNames,
                           final Vector initializers) {
        this.className = className;
        this.instanceName = instanceName;
        this.metaParameters = metaParameters;
        this.channelAndNodeNames = channelAndNodeNames;
        this.initializers = initializers;
    }

    /**
     * Called by JavaCoSimInfo.addClass. Adds a channel name -> timing
     * info mapping to m, for each channel which has a custom slack
     * specified in the java block.
     **/
    public void getChannelTimingInfoMap(Map m) {
        for(int i=0; i<channelAndNodeNames.size(); i++) {
            JavaClassParameter p = 
                (JavaClassParameter)channelAndNodeNames.elementAt(i);
            p.getChannelTimingInfoMap(m);
        }
    }

    /**
     * Constructs the AbstractDevice described by this JavaCoSimDevice
     * and start()s it.
     *
     * TODO kwallmar: eventually all the relevant code should be moved
     * out of CellImpl and DSim to here, including the creation of the
     * input and output channels (maybe).
     **/
    protected void buildDevice(final String cellName,
                               final ChannelDictionary cdict,
                               final ClassLoader classLoader,
                               final int arbitrationMode)
        throws DeviceConstructionException {

        Class javaClass = null;
        try {
            javaClass = Class.forName(className, true, classLoader);
        } catch (ClassNotFoundException e) {
            throw new DeviceConstructionException("Couldn't get class",
                                                  e,
                                                  className,
                                                  cellName);
        }

        final String nonNullInstanceName;
        if (instanceName == null)
            nonNullInstanceName = cellName;
        else
            nonNullInstanceName = cellName + '.' + instanceName;

        final DeviceParameters params =
            new DeviceParameters(nonNullInstanceName,
                                 false, // suppressOutput
                                 metaParameters,
                                 arbitrationMode);

        // Construct Vectors of the types to pass to the constructor
        // and the actual values represented by them

        final Vector ctorArgTypes = new Vector();
        final Vector ctorArgs = new Vector();

        ctorArgTypes.add(DeviceParameters.class);
        ctorArgs.add(params);

        // parameters
        // TODO kwallmar: explicitly ignoring nodes
        for (final ListIterator i=channelAndNodeNames.listIterator();
             i.hasNext();
            ) {
            final JavaClassParameter param = (JavaClassParameter) i.next();
            if (!param.isArray()) { // Plain channel
                final String name = param.getChannelName();
                final ChannelInput chanIn = cdict.getInputChan(name);
                if (chanIn != null) {
                    ctorArgTypes.add(ChannelInput.class);
                    ctorArgs.add(chanIn);
                    continue;
                }
                final ChannelOutput chanOut = cdict.getOutputChan(name);
                if (chanOut != null) {
                    ctorArgTypes.add(ChannelOutput.class);
                    ctorArgs.add(chanOut);
                    continue;
                }
                
                throw new DeviceConstructionException
                    ("Couldn't find a channel for " + 
                     name, className, cellName);
            } else {            // Array
                final CoSimChannelNames array = param.getArrayChannelNames();
                final CoSimChannelArrayInterface channels =
                    array.convertToChannels(cdict);
                ctorArgTypes.add(channels.getClass());
                ctorArgs.add(channels);
            }
        }

        // Create the AbstractDevice
        final Class[] argTypesArray =
            (Class[]) ctorArgTypes.toArray(new Class[0]);
        final Object[] args = ctorArgs.toArray();

        Constructor ctor = null;
        try {
            ctor = javaClass.getConstructor(argTypesArray);
        } catch (NoSuchMethodException e) {
            throw new DeviceConstructionException("Couldn't get constructor for " + className + " taking args " + ctorArgTypes,
                                                  e,
                                                  className,
                                                  cellName);
        }

        Startable device = null;
        try {
            device = (Startable) ctor.newInstance(args);
        } catch (IllegalAccessException e) {
            throw new DeviceConstructionException("Couldn't instantiate the abstractDevice", e, className, cellName);
        } catch (InvocationTargetException e) { // Wrapper for constructor exceptions
            throw new DeviceConstructionException("Couldn't instantiate the AbstractDevice", e.getTargetException(), className, cellName);
        } catch (InstantiationException e) {
            throw new DeviceConstructionException("Couldn't instantiate the AbstractDevice", e, className, cellName);
        }

        device.start();
    }
    public String getClassName() {
        return className;
    }
}
