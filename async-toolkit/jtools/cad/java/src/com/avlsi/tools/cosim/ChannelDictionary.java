/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cosim;

import java.util.Hashtable;
import java.util.HashMap;
import java.util.Map;
import com.avlsi.tools.tsim.ChannelInput;
import com.avlsi.tools.tsim.ChannelOutput;
import com.avlsi.tools.tsim.WideNode;


public class ChannelDictionary {
    private Hashtable inChannelTable;
    private Hashtable outChannelTable;
    private Map nodeMap;

    public ChannelDictionary() {
        inChannelTable = new Hashtable();
        outChannelTable = new Hashtable();
        nodeMap = new HashMap();
    }

    public void addInputChan(String name, ChannelInput chan) {
        inChannelTable.put(name, chan);
    }

    /** Returns null if not found. **/
    public ChannelInput getInputChan(String name) {
        return (ChannelInput) inChannelTable.get(name);
    }

    public void addOutputChan(String name, ChannelOutput chan) {
        outChannelTable.put(name, chan);
    }

    /** Returns null if not found. **/
    public ChannelOutput getOutputChan(String name) {
        return (ChannelOutput) outChannelTable.get(name);
    }

    public void addWideNode(String name, WideNode node) {
        nodeMap.put(name, node);
    }

    public WideNode getWideNode(String name) {
        return (WideNode) nodeMap.get(name);
    }
}
