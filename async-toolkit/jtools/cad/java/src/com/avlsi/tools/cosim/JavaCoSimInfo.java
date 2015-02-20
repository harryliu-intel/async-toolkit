/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id: //depot/sw/cad/java/main/src/com/avlsi/tools/cosim/CoSimInfo.java#5 $
 * $DateTime: 2002/02/23 11:33:05 $
 * $Author: chrisb $
 */

package com.avlsi.tools.cosim;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.HashMap;

import com.avlsi.file.common.HierName;
import com.avlsi.util.debug.Debug;

/**
 * Extends CoSimInfo to contain information specific to the java block.
 **/
public class JavaCoSimInfo extends CoSimInfo {
    /** Only used for old-style syntax. **/
    private String className;

    /** Only used for new-style syntax.  Contains JavaCoSimDevices. **/
    private final Set classes;

    /** Whether this javablock is old-style or new-style. **/
    private boolean oldStyleSyntax;

    /** Whether there's any information in this data structure yet. **/
    private boolean empty;

    /** For now, a map from String to Integer, specifying the slack on
     * a channel if it was given in the cast. TODO: maybe create a
     * ChannelTimingInfo class to contain this and other types of
     * timing information, to replace Integer here. **/
    private HashMap customTimingInfo;

    public JavaCoSimInfo() {
        super();
        classes = new HashSet();
        customTimingInfo = new HashMap();
        oldStyleSyntax = true;
        empty = true;
    }

    public void addClass(JavaCoSimDevice classDescriptor) {
        oldStyleSyntax = false;
        empty = false;
        classes.add(classDescriptor);
        classDescriptor.getChannelTimingInfoMap(customTimingInfo);
    }

    public void setClassName(final String newName) {
        oldStyleSyntax = true;
        empty = false;
        className = newName;
    }

    public String getClassName() {
        Debug.assertTrue(oldStyleSyntax);
        return className;
    }

    /**
     * Adds info on slack/N/M to the internal ChannelParameterDict.
     * Overrides the slack value with a custom value if one was
     * specified in the java block.
     **/
    public void addChannelInfo(final String name, final String type,
                               final int slack,
                               final BigInteger N, final int M,
                               final boolean isArrayed) {
        int realSlack=slack;
        if(customTimingInfo.containsKey(name)) {
            realSlack = ((Integer)customTimingInfo.get(name)).intValue();
            // TODO: can remove this later. keep it for now since it
            // informs users of a recent behavior change
            System.err.println("Using "+realSlack+" instead of "+slack+" for slack on "+name);
        }
        super.addChannelInfo(name, type, realSlack, N, M, isArrayed);
    }

    /**
     * The old-style syntax sets the NodeChannels by hand; the
     * new-style lets them be constructed from the ports.
     **/
    protected boolean usePorts() { return !oldStyleSyntax; }

    public boolean isOldStyle() { return oldStyleSyntax; }

    public boolean isEmpty() { return empty; }

    public String toString() {
        if (oldStyleSyntax)
            return "Java class: " + className + " with input channels: "
                + inputChannels + " and output channels: " + outputChannels;
        else
            return "JavaCoSimInfo";
    }

    /**
     * Constructs appropriate AbstractDevices hooked up properly and
     * start()s them.
     *
     * TODO kwallmar: eventually this should be made a method of
     * CoSimInfo and absorb some code from CellImpl.
     **/
    public void buildClasses(final HierName cellHierName,
                             final ChannelDictionary cdict,
                             final ClassLoader classLoader,
                             final int arbitrationMode)
        throws DeviceConstructionException {

        final String cellName = cellHierName.getAsString('.');

        for (final Iterator i=classes.iterator(); i.hasNext(); ) {
            final JavaCoSimDevice classDescriptor = (JavaCoSimDevice) i.next();
            classDescriptor.buildDevice(cellName, cdict, classLoader,
                                        arbitrationMode);
        }
    }

    /**
     * Should only ever be used by CellImpl.setRefinementParent().
     * Java blocks refine by overriding.
     **/
    public void refineFrom(final JavaCoSimInfo parentInfo) {
        if (this.isEmpty() && ! parentInfo.isEmpty()) {
            super.refineFrom(parentInfo);
            
            this.className = parentInfo.className;
            this.classes.addAll(parentInfo.classes);
            this.oldStyleSyntax = parentInfo.oldStyleSyntax;
            this.empty = false;
        }
    }

    /**
     * Return the class names of Java classes that's part of this Java block.
     **/
    public Collection getClassNames() {
        if (oldStyleSyntax) {
            return Collections.singletonList(className);
        } else {
            final Collection result = new ArrayList();
            for (Iterator i = classes.iterator(); i.hasNext(); ) {
                final JavaCoSimDevice jcd = (JavaCoSimDevice) i.next();
                result.add(jcd.getClassName());
            }
            return result;
        }
    }
}
