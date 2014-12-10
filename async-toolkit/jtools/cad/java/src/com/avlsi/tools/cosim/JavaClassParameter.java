
/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.cosim;

import com.avlsi.tools.cosim.CoSimChannelNames;

import java.util.Map;

/**
 * Encapsulates the channel parameters which given for a Java class in
 * a Cast Java block.
 *
 * @author Frederik Eaton
 * @version $Revision$ $Date$
 **/
public class JavaClassParameter {
    boolean isArray;
    String scalarName;
    CoSimChannelNames arrayNames;
    int[] timingParams;
    
    public JavaClassParameter(String s) {
        scalarName = s;
        arrayNames = null;
        isArray = false;
        timingParams = null;
    }

    public JavaClassParameter(CoSimChannelNames a) {
        arrayNames = a;
        scalarName = null;
        isArray = true;
        timingParams = null;
    }

    public void setTimingParams(int[] t) {
        timingParams = t;
    }

    public boolean hasTimingParams() {
        return timingParams!=null;
    }

    public int[] getTimingParams() {
        return timingParams;
    }

    /**
     * Called by JavaCoSimDevice.getChannelTimingInfoMap. Adds a
     * channel name -> timing info mapping to m, if this channel has a
     * custom slack specified.
     **/
    public void getChannelTimingInfoMap(Map m) {
        if(hasTimingParams()) {
            Integer tp=new Integer(timingParams[0]);
            if(isArray) {
                String[] array=arrayNames.getAllElements();
                for(int i=0; i<array.length; i++) {
                    m.put(array[i], tp);
                }
            } else {
                m.put(scalarName, tp);
            }
        }
    }
    
    public boolean isArray() {
        return isArray;
    }

    public CoSimChannelNames getArrayChannelNames() {
        return arrayNames;
    }

    public String getChannelName() {
        return scalarName;
    }
}
