package com.avlsi.tools.slacker;

import java.util.*;
import java.io.*;
import java.util.List;
import java.util.ArrayList;

public class Subcell implements Comparable {

    /** Type of subcell */
    final Type type;
    
    /** Name of subcell */
    final String name;
    
    /** Input ports of subcell */
    final List inPorts = new ArrayList(); // of Channel

    /** Output ports of subcell */
    final List outPorts = new ArrayList(); // of Channel

    /** Constructor */
    Subcell(Type type, String name) {
        this.type = type;
        this.name = name;
    }

    /** Add an input Port */
    void addInPort(Channel chan) {
        inPorts.add(chan);
    }
    
    /** Add an output Port */
    void addOutPort(Channel chan) {
        outPorts.add(chan);
    }

    /** Compare by name */
    public int compareTo(Object b) {
        return name.compareTo(((Subcell) b).name);
    }
    
    /** Debugging printout */
    public String toString() {
        String s = "(subcell type=" + type.name + " name=" + name;
        for (int i=0; i<inPorts.size(); i++)  
            s += " -" + ((Channel) inPorts.get(i)).name;
        for (int i=0; i<outPorts.size(); i++) 
            s += " +" + ((Channel) outPorts.get(i)).name;
        return s +")";
    }
}
