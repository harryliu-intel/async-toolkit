/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.cdl.parser;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.avlsi.file.common.HierName;
import com.avlsi.file.common.DeviceTypes;
import com.avlsi.netlist.AbstractDevice;
import com.avlsi.netlist.AbstractDeviceIterator;
import com.avlsi.netlist.AbstractNetlist;
import com.avlsi.netlist.AbstractNetlistIterator;
import com.avlsi.netlist.AbstractNode;
import com.avlsi.netlist.AbstractNodeIterator;
import com.avlsi.netlist.Visitor;
import com.avlsi.netlist.impl.SimpleAbstractDeviceIterator;
import com.avlsi.netlist.impl.SimpleAbstractNetlistIterator;
import com.avlsi.netlist.impl.SimpleAbstractNodeIterator;
import com.avlsi.util.container.AliasedMap;

/**
 * A factory intended to be used when parsing a netlist block in CASTv2.  Only
 * resistor, transistor, inductor circuit elements are allowed.  Specifically,
 * subcircuit definitions, and subcircuit calls are NOT allowed.<p>
 * When viewed as an AbstractNetlist, it is a circuit that does not have any
 * input or output nodes, and has a default name of &lt;netlist block&gt;.  It
 * is the responsibility of the user (e.g., 
 * {@link com.avlsi.fast.NetlistAdapter NetlistAdapter})
 * to stitch in the netlist, so that it has the proper input and output nodes,
 * and the proper name.
 **/
public class NetlistBlockFactory extends CDLInterfaceSimplifier implements AbstractNetlist {
    /** The list of all circuit elements. */
    private List requests = new ArrayList();

    /**
     * The data stored in the AliasedMap is a 2-element array.  The first
     * element is a <code>Set</code> of all devices connected to the key.  The
     * second element contains the canonical name, which maybe different from
     * the canonical name returned by AliasedMap, and is expected to be set
     * after the CDL has been parsed.  When we merge, we union the
     * <code>Set</code> of all devices, and set the canonical name to null.
     **/
    private class MergeSet implements AliasedMap.MergeFunction {
        public Object merge(Object oldValue, Object newValue) {
            final Set s1 = (Set) ((Object[]) oldValue)[0];
            final Set s2 = (Set) ((Object[]) newValue)[0];
            final Set s3 = new HashSet();
            s3.addAll(s1); s3.addAll(s2);
            final Object[] o = new Object[2];
            o[0] = s3; o[1] = null;
            return o;
        }
    }

    /** All nodes in the netlist. */
    private AliasedMap nodes =
        new AliasedMap(new MergeSet(), HierName.getComparator());

    public AliasedMap getAliasedMap() {
        return nodes;
    }

    /* Return a HierName that is marked as generated, if it consists only of
     * digits */
    private HierName mark(HierName n) {
        if (n.isNumeric()) return n.appendString("#");
        else return n;
    }

    /**
     * Associate a node with a device, to allow enumeration of all devices
     * connected to a node.
     *
     * @param name Name of the node.
     * @param device AbstractDevice to associate the node with.
     **/
    private void newName(HierName name, AbstractDevice device) {
        final Object[] o = new Object[2];
        final Set s = new HashSet();
        s.add(device);
        o[0] = s; o[1] = null;
        try {
            nodes.addData(name, o);
        } catch (AliasedMap.MergeFailedException e) {
            System.err.println("MergeFailed: " + e);
        }
    }

    private class Node implements AbstractNode {
        private final HierName name;
        public Node(final HierName name) {
            this.name = name;
        }
        public HierName getCanonicalName() {
            final Object[] o = (Object[]) nodes.getValue(name);
            if (o[1] == null) {
                return (HierName) nodes.getCanonicalKey(name);
            } else {
                return (HierName) o[1];
            }
        }
        public Iterator getAliases() {
            return nodes.getAliases(name);
        }
        public AbstractDeviceIterator getDevices() {
            final Set s = (Set) ((Object[]) nodes.getValue(name))[0];
            return new SimpleAbstractDeviceIterator(s.iterator());
        }
    }

    private Node toNode(HierName name) {
        return new Node(name);
    }
    
    private final class Resistor implements AbstractDevice {
        private final HierName name, n1, n2;
        private final double val;
        public Resistor(HierName name, HierName n1, HierName n2, double val) {
            this.name = name;
            this.n1 = mark(n1);
            this.n2 = mark(n2);
            this.val = val;
            newName(this.n1, this); newName(this.n2, this);
        }

        public void accept(final Visitor visitor) {
            visitor.genericResistor(name, toNode(n1), toNode(n2), 1 / val,
                                    new HashMap());
        }
    }

    private final class Capacitor implements AbstractDevice {
        private final HierName name, npos, nneg;
        private final double val;
        public Capacitor(HierName name, HierName npos, HierName nneg,
                         double val) {
            this.name = name;
            this.npos = mark(npos);
            this.nneg = mark(nneg);
            this.val = val;
            newName(this.npos, this); newName(this.nneg, this);
        }

        public void accept(final Visitor visitor) {
            visitor.genericCapacitor(name, toNode(npos), toNode(nneg),
                                     val, new HashMap());
        }
    }

    private final class Transistor implements AbstractDevice { 
        private final HierName name, ns, nd, ng, nb;
        private final int type;
        private final double w, l;
        public Transistor(HierName name, int type, HierName ns,
                          HierName nd, HierName ng, HierName nb,
                          double w, double l) {
            this.name = name;
            this.type = type;
            this.ns = mark(ns);
            this.nd = mark(nd);
            this.ng = mark(ng);
            this.nb = mark(nb);
            this.w = w;
            this.l = l;
            newName(this.ns, this); newName(this.nd, this);
            newName(this.ng, this); newName(this.nb, this);
        }

        public void accept(final Visitor visitor) {
            visitor.genericTransistor(name, toNode(nd), toNode(ng),
                                      toNode(ns), toNode(nb), l, w,
                                      type == DeviceTypes.P_TYPE ? "p" : "n",
                                      new HashMap());
        }
    }

    private final class Inductor implements AbstractDevice {
        private final HierName name, npos, nneg;
        private final double val;
        public Inductor(HierName name, HierName npos, HierName nneg,
                        double val) {
            this.name = name;
            this.npos = mark(npos);
            this.nneg = mark(nneg);
            this.val = val;
            newName(this.npos, this); newName(this.nneg, this);
        }

        public void accept(final Visitor visitor) {
            visitor.genericInductor(name, toNode(npos), toNode(nneg),
                                    val, new HashMap());
        }
    }

    private final class Diode implements AbstractDevice {
        private final HierName name, npos, nneg;
        private final double l,w,p,a;
        private final int type;
        public Diode(HierName name, int type, HierName npos, HierName nneg,
                     double w, double l, double a, double p) {
            this.name = name;
            this.npos = mark(npos);
            this.nneg = mark(nneg);
            this.l = l;
            this.w = w;
            this.p = p;
            this.a = a;
            this.type = type;
            newName(this.npos, this); newName(this.nneg, this);
        }

        public void accept(final Visitor visitor) {
            visitor.genericDiode(name, toNode(npos), toNode(nneg),
                                 l,w,p,a, 
                                 type == DeviceTypes.N_TYPE ? "dw" : "dp",
                                 new HashMap());
        }
    }
    
    private final class Bipolar implements AbstractDevice {
        private final HierName name, nc, nb, ne;
        private final double a;
        private final int type;
        public Bipolar(HierName name, int type, HierName nc, HierName nb,
                       HierName ne, double a) {
            this.name = name;
            this.nc = mark(nc);
            this.nb = mark(nb);
            this.ne = mark(ne);
            this.a = a;
            this.type = type;
            newName(this.nc, this);
            newName(this.nb, this);
            newName(this.ne, this);
        }

        public void accept(final Visitor visitor) {
            throw new AssertionError("Bipolar devices not implemented");
        }
    }

    private final class Call implements AbstractDevice {
        private final HierName name;
        private final String subName;
        private final HierName[] args;
        private final Map parameters;
        public Call(HierName name, String subName, HierName[] args,
                    Map parameters) {
            this.name = name;
            this.subName = subName;
            this.args = args;
            this.parameters = parameters;
        }
        public void accept(final Visitor visitor) {
        }
    }

    public void makeResistor(HierName name, HierName n1, HierName n2,
                             double val) {
        if (val <= 0) {
            throw new RuntimeException("Resistor " + name +
                                       " has invalid resistance " + val + "!");
        }
        requests.add(new Resistor(name, n1, n2, val));
    }

    public void makeCapacitor(HierName name, HierName npos, HierName nneg,
                              double val) {
        requests.add(new Capacitor(name, npos, nneg, val));
    }

    public void makeTransistor(HierName name, int type, HierName ns,
                               HierName nd, HierName ng, HierName nb,
                               double w, double l) {
        requests.add(new Transistor(name, type, ns, nd, ng, nb, w, l));
    }

    public void makeInductor(HierName name, HierName npos, HierName nneg,
                             double val) {
        if (val <= 0) {
            throw new RuntimeException("Inductor " + name +
                                       " has invalid inductance " + val + "!");
        }
        requests.add(new Inductor(name, npos, nneg, val));
    }

    public void makeDiode(HierName name, int type, HierName npos, HierName nneg,
                          double w, double l, double a, double p) {
        requests.add(new Diode(name, type, npos, nneg, w, l, p, a) );
    }

    public void makeBipolar(HierName name, int type, HierName nc, HierName nb,
                            HierName ne, double a) { 
        requests.add(new Bipolar(name, type, nc, nb, ne, a) );
    }

    /* The following devices are not allowed in a netlist block */
    public void makeCall(HierName name, String subName, HierName[] args, Map parameters) {
    //    throw new RuntimeException("Cannot instantiate subcell " + subName + " as " + name + " from within a netlist block!");
    }

    public void beginSubcircuit(String subName, String[] in, String[] out) {
        throw new RuntimeException("Cannot define subcell " + subName + " from within a netlist block!");
    }

    public void endSubcircuit(String subName) {
        throw new RuntimeException("Cannot define subcell " + subName + " from within a netlist block!");
    }

    public AbstractNodeIterator getInputNodes() {
        return new SimpleAbstractNodeIterator(Collections.EMPTY_LIST.iterator());
    }

    public AbstractNodeIterator getOutputNodes() {
        return new SimpleAbstractNodeIterator(Collections.EMPTY_LIST.iterator());
    }

    public HierName getName() {
        return HierName.makeHierName("<netlist block>");
    }

    public AbstractNodeIterator getNodes() {
        final Iterator keys = nodes.getKeys();
        return new AbstractNodeIterator() {
            public boolean hasNext() {
                return keys.hasNext();
            }

            public AbstractNode next() {
                return toNode((HierName) keys.next());
            }
        };
    }

    public AbstractDeviceIterator getDevices() {
        return new SimpleAbstractDeviceIterator(requests.iterator());
    }

    public AbstractNetlistIterator getSubcircuits() {
        return new SimpleAbstractNetlistIterator(Collections.EMPTY_LIST.iterator());
    }
}
