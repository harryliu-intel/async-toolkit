/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.TreeMap;
import java.util.Set;
import java.util.SortedSet;
import java.util.HashSet;
import java.util.TreeSet;

import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;
import com.avlsi.fast.ConnectionInfo;
import com.avlsi.fast.CellType;
import com.avlsi.fast.CellTypeProcessor;
import com.avlsi.fast.HalfOperator;
import com.avlsi.file.common.DeviceTypes;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.file.cdl.parser.NetlistBlockFactory;
import com.avlsi.netlist.AbstractDevice;
import com.avlsi.netlist.AbstractDeviceIterator;
import com.avlsi.netlist.AbstractNetlist;
import com.avlsi.netlist.AbstractNetlistIterator;
import com.avlsi.netlist.AbstractNode;
import com.avlsi.netlist.AbstractNodeIterator;
import com.avlsi.netlist.Visitor;
import com.avlsi.netlist.impl.ConcatAbstractDeviceIterator;
import com.avlsi.netlist.impl.SimpleAbstractDeviceIterator;
import com.avlsi.netlist.impl.SimpleAbstractNetlistIterator;
import com.avlsi.netlist.impl.SimpleAbstractNodeIterator;
import com.avlsi.tools.lvs.NetGraph;
import com.avlsi.tools.cadencize.CadenceInfo;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.util.container.AliasedMap;
import com.avlsi.util.container.FilteringIterator;
import com.avlsi.util.container.Pair;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.functions.UnaryPredicate;

/**
   This class is an adapter to CellType that makes it look like a netlist.
 */

public class NetlistAdapter implements AbstractNetlist {
    private int stackId = 0;
    private class Stack implements AbstractDevice {
        private final NetGraph.NetNode start;
        private final NetGraph.NetNode end;
        private final List gates;
        private final int type;
        private final int transistorType;
        private final Map params;
        private int id;
        private String pn() {
            return (type == DeviceTypes.N_TYPE ? "N" : "P");
        }
        public Stack(final NetGraph.NetNode start,
                     final NetGraph.NetNode end,
                     final List gates,
                     final int type) {
            this.gates = new ArrayList(gates);
            this.type = type;
            this.params = new TreeMap();
            final NetGraph.NetEdge zero = (NetGraph.NetEdge) gates.get(0);
            params.put(pn() + "W" + gates.size(),
                       new Double(roundMinWidth(zero.width)));
            params.put(pn() + "L", new Double(zero.length));
            this.start = start;
            this.end = end;
            this.id = -1;
            this.transistorType = zero.getTransistorType();
        }
        public void accept(final Visitor visitor) {
            final Map map = new TreeMap();
            if (id < 0) id = stackId++;
            map.put("S", start);
            map.put("D", end);
            for (int i = 0; i < gates.size(); ++i) {
                map.put("G[" + i + "]", ((NetGraph.NetEdge) gates.get(i)).gate);
            }
            if (type == DeviceTypes.N_TYPE) {
                map.put("GND", graph.findNetNode(GND));
            } else {
                map.put("Vdd", graph.findNetNode(Vdd));
            }

            final AbstractNodeIterator nodes = new SimpleAbstractNodeIterator(map.entrySet().iterator()) {
                public AbstractNode next() {
                    final Map.Entry entry = (Map.Entry) iterator.next();
                    final NetGraph.NetNode to = (NetGraph.NetNode) entry.getValue();
                    return new Node(to, to != start &&
                                        to != end &&
                                        !to.isRail());
                }
            };

            final NetGraph gate = (NetGraph) stackMap.get(new Pair(new Integer(gates.size()), new Integer(type)));

            if ( gate == null ) {
            
                final String errorStr = 
                    "Cannot find appropriate gate for " + 
                    pn() + 
                    "-type chains of size " +
                    gates.size() +
                    ".\n For chain starting with \"" +
                    start.name.toString() +
                    "\" and ending with \"" +
                    end.name.toString() +
                    "\" in \"" +
                    cell.typeName +
                    "\".";

                throw new RuntimeException( errorStr );
                    
                
            }

            final String baseType = CellUtils.getBaseType(gate.gateType);
            final String realName = baseType.equals(gate.gateType) ?
                baseType : baseType + "(" + transistorType + ")";
            visitor.subcircuitCall(HierName.makeHierName("chain_" + id), new GateAdapter(gate, realName), nodes, params);
        }
        public String toString() {
            return "==> Start: " + start + "\n==> End: " + end + "\n==> Gates: " + gates;
        }
        public boolean equals(Object o) {
            return (o instanceof Stack) && gates.equals(((Stack) o).gates);
        }
        public int hashCode() {
            return gates.hashCode();
        }
    }

    private void stackize(final NetGraph.NetPath path, final Set stacks) {
        final int type = path.getType();
        final List edges = path.getEdges();
        NetGraph.NetNode start = path.getStartNode();
        final NetGraph.NetNode end = path.getEndNode();
        List stack = new ArrayList();
        NetGraph.NetNode last = start;
        for (Iterator i = edges.iterator(); i.hasNext(); ) {
            final NetGraph.NetEdge edge = (NetGraph.NetEdge) i.next();
            final boolean excluded = edge.library && !edge.floating;
            final NetGraph.NetNode next =
                edge.source == last ? edge.drain : edge.source;
            if (stack.isEmpty()) {
                if (!excluded) {
                    stack.add(edge);
                    start = last;
                }
            } else {
                final NetGraph.NetEdge zero = (NetGraph.NetEdge) stack.get(0);
                if (zero.length != edge.length || zero.width != edge.width ||
                    zero.getTransistorType() != edge.getTransistorType() ||
                    excluded) {
                    stacks.add(new Stack(start, last, stack, type));
                    stack.clear();
                    start = last;
                }
                if (!excluded) stack.add(edge);
            }
            if (!stack.isEmpty() && next.needsContact()) {
                stacks.add(new Stack(start, next, stack, type));
                stack.clear();
            }
            last = next;
        }
        if (!stack.isEmpty()) stacks.add(new Stack(start, end, stack, type));
    }

    private Collection getStacks(final NetGraph g) {
        final Set result = new LinkedHashSet();
        for (Iterator i = g.getNodes().iterator(); i.hasNext(); ) {
            final NetGraph.NetNode n = (NetGraph.NetNode) i.next();
            for (Iterator j = n.getPaths().iterator(); j.hasNext(); ) {
                final NetGraph.NetPath p = (NetGraph.NetPath) j.next();
                final NetGraph.NetNode start = p.getStartNode();
                final NetGraph.NetNode end = p.getEndNode();
                if (!start.isRail() && !end.isRail() && start.compareTo(end) > 0) continue;
                final int type = p.getType();
                if (type == DeviceTypes.P_TYPE || type == DeviceTypes.N_TYPE)
                    stackize(p, result);
            }
        }
        return result;
    }

    /** Obtain the port list in a consistent way using Cadencize. */
    public static SortedSet getParameterList(final CellInterface ci) {
        return getParameterList(ci, new Cadencize(true));
    }

    public static SortedSet getParameterList(final CellInterface ci,
                                             final Cadencize c) {
        /* Cadencize to find the port list. */
        final CadenceInfo cinfo = c.convert(ci);
        TreeSet port = new TreeSet();
        AliasedMap portNodes = cinfo.getPortNodes();
        for (Iterator i = portNodes.getCanonicalKeys(); i.hasNext(); ) {
            final HierName h = (HierName) i.next();
            if (((Boolean) portNodes.getValue(h)).booleanValue()) port.add(h);
        }
        return port;
    }

    /**
       This class is an adapter to turn a NetGraph representation of a gate
       into a Netlist.  The gate is read in from a CDL file, and is very
       simple.  In particular, this means no subcircuit calls.
     */
    public static class GateAdapter implements AbstractNetlist {
        private int transistorID = 0;
        private class Node implements AbstractNode {
            private final NetGraph.NetNode node;

            public Node(final NetGraph.NetNode node) {
                this.node = node;
            }

            public HierName getCanonicalName() {
                return node.name;
            }

            public Iterator getAliases() {
                /* There is no aliasing in CDL files except through parameter
                 * passing */
                return Collections.EMPTY_LIST.iterator();
            }

            public AbstractDeviceIterator getDevices() {
                return new SimpleAbstractDeviceIterator(node.edges.iterator()) {
                    public AbstractDevice next() {
                        final NetGraph.NetEdge edge =
                            (NetGraph.NetEdge) iterator.next();
                        return new Transistor(edge);
                    }
                };
            }
        }
        private class Transistor implements AbstractDevice {
            private final NetGraph.NetEdge edge;
            private final NetGraph.NetNode bulk;
            private String type;
            private final HierName name;

            public Transistor(final NetGraph.NetEdge edge) {
                this.edge = edge;
                if (edge.type == DeviceTypes.N_TYPE) {
                    bulk = graph.GND;
                    type = "N";
                } else {
                    bulk = graph.Vdd;
                    type = "P";
                }
                type = type + "[" + edge.getTransistorType() + "]";
                name = makeHierName(Integer.toString(transistorID++));
            }

            public void accept(final Visitor visitor) {
                visitor.genericTransistor(
                    name,
                    new Node(edge.drain), new Node(edge.gate),
                    new Node(edge.source), new Node(bulk),
                    edge.length, edge.width,
                    type, Collections.EMPTY_MAP
                );
            }
        }
        private final NetGraph graph;
        private final String name;
        public GateAdapter(NetGraph gateGraph) {
            this(gateGraph, gateGraph.gateType);
        }
        public GateAdapter(NetGraph gateGraph, String name) {
            this.graph = gateGraph;
            this.name = name;
        }
        /* There is no way to tell if a node is a input or an output from
         * NetGraph, so assume all nodes are output nodes. */
        public AbstractNodeIterator getInputNodes() {
            return new
                SimpleAbstractNodeIterator(Collections.EMPTY_LIST.iterator());
        }
        public AbstractNodeIterator getOutputNodes() {
            final UnaryPredicate isOutput = new UnaryPredicate() {
                public boolean evaluate(Object a) {
                    NetGraph.NetNode node = (NetGraph.NetNode) a;
                    return node.isOutput() && node.isNamed();
                }
            };
            final FilteringIterator filter = 
                new FilteringIterator(graph.getNodes().iterator(), isOutput);
            return new SimpleAbstractNodeIterator(filter) {
                public AbstractNode next() {
                    final NetGraph.NetNode node =
                        (NetGraph.NetNode) iterator.next();
                    return new Node(node);
                }
            };
        }
        public HierName getName() {
            return makeHierName(name);
        }
        public AbstractNodeIterator getNodes() {
            return new
                SimpleAbstractNodeIterator(graph.getNodes().iterator()) {
                    public AbstractNode next() {
                        final NetGraph.NetNode node =
                            (NetGraph.NetNode) iterator.next();
                        return new Node(node);
                    }
                };
        }
        public AbstractDeviceIterator getDevices() {
            return new
                SimpleAbstractDeviceIterator(graph.getEdges().iterator()) {
                    public AbstractDevice next() {
                        final NetGraph.NetEdge edge =
                            (NetGraph.NetEdge) iterator.next();
                        return new Transistor(edge);
                    }
                };
        }
        public AbstractNetlistIterator getSubcircuits() {
            return new
                SimpleAbstractNetlistIterator(Collections.EMPTY_LIST.iterator());
        }
    }

    private final CellType cell;
    private final NetGraph graph;
    private int transistorID = 0;
    private final Map portMap;
    private final boolean outputRC;
    private final Map stackMap;
    private final HierName GND, Vdd;

    /**
     * Float.isNaN(top) means old SPICE style; top &lt; 0 indicate non-top
     * level cell; otherwise top is the capacitance to attach to the port
     * nodes.
     **/
    private final float top;

    /**
     * Minimum width of a transistor for a stack or a gate.  This limit does
     * not apply to staticizers.
     **/
    private final float minWidth;
    private double roundMinWidth(final double val) {
        if (Float.isNaN(minWidth)) return val;
        else return Math.max(val, minWidth);
    }

    private static HierName makeSource(final HierName name) {
        return name.appendString(":source");
    }

    private static HierName makeSink(final HierName name) {
        return name;
    }

    private static final UnaryPredicate notLibraryEdge = new UnaryPredicate() {
        public boolean evaluate(Object a) {
            /* NetEdge.library is true if the edge is part of a matched gate.
             * We want to filter these out.  But we also want to keep the
             * "floating" transistors in. */
            final NetGraph.NetEdge edge = (NetGraph.NetEdge) a;
            return !edge.library || edge.floating;
        }
    };

    // XXX: Jcast does not handle definition within a definition
    private boolean subcircuits;

    private static HierName makeHierName(final String s) {
        HierName n = null;
        try {
            n = HierName.makeHierName(s, '.');
        } catch (InvalidHierNameException e) {
            Debug.assertTrue(false, "Cannot makeHierName");
        }
        return n;
    }

    private class Node implements AbstractNode {
        private final NetGraph.NetNode node;

        /** Is the node used as a gate node in a transistor? If so, it is
         * treated as a sink, otherwise, it is treated as a source. */
        private final boolean gate;

        public Node(final NetGraph.NetNode node, final boolean gate) {
            this.node = node;
            this.gate = gate;
        }

        public HierName getCanonicalName() {
            final CellNet net;
            if (outputRC && (net = cell.getNet(node.name)) != null) {
                final CellNet.Geometry geometry = net.getGeometry();
                if (geometry != null) return geometry.mapName(net, gate);
                else if (!Float.isNaN(top)) {
                    if (net.isInternalNet()) {
                        return gate ? node.name : makeSource(node.name);
                    } else if (top < 0) {
                        return gate ? makeSink(node.name) :
                                      makeSource(node.name);
                    } else {
                        return node.name;
                    }
                }
            }
            return node.name;
        }

        public Iterator getAliases() {
            return Collections.EMPTY_LIST.iterator();
        }

        public AbstractDeviceIterator getDevices() {
            final Iterator i =
                new FilteringIterator(node.edges.iterator(), notLibraryEdge);
            return new SimpleAbstractDeviceIterator(i) {
                public AbstractDevice next() {
                    NetGraph.NetEdge edge = (NetGraph.NetEdge) iterator.next();
                    return new Transistor(edge);
                }
            };
        }
    }

    private class PortNode implements AbstractNode {
        private final CellType cell;
        private final HierName name;

        public PortNode(final CellType cell, final HierName name) {
            this.cell = cell;
            this.name = name;
        }

        public HierName getCanonicalName() {
            return name;
        }

        public Iterator getAliases() {
            // XXX
            return null;
        }

        public AbstractDeviceIterator getDevices() {
            // XXX
            return null;
        }
    }

    private class Transistor implements AbstractDevice {
        private final NetGraph.NetEdge edge;
        private final NetGraph.NetNode bulk;
        private String type;
        private final HierName name;

        public Transistor(final NetGraph.NetEdge edge) {
            this.edge = edge;
            if (edge.type == DeviceTypes.N_TYPE) {
                bulk = graph.GND;
                type = "N";
            } else {
                bulk = graph.Vdd;
                type = "P";
            }
            type = type + "[" + edge.getTransistorType() + "]";
            name = makeHierName(Integer.toString(transistorID++));
        }

        public void accept(final Visitor visitor) {
            visitor.genericTransistor(
                name,
                new Node(edge.drain, false),
                new Node(edge.gate, true),
                new Node(edge.source, false),
                new Node(bulk, false),
                edge.length,
                edge.width,
                type,
                Collections.EMPTY_MAP
            );
        }
    }

    private class Capacitor implements AbstractDevice {
        private final HierName name;
        private final AbstractNode pos;
        private final AbstractNode neg;
        private final double cap;
        public Capacitor(final HierName name, final AbstractNode pos,
                         final AbstractNode neg, final double cap) {
            this.name = name;
            this.pos = pos;
            this.neg = neg;
            this.cap = cap;
        }
        public void accept(final Visitor visitor) {
            visitor.genericCapacitor(name, pos, neg, cap,
                                     Collections.EMPTY_MAP);
        }
    }

    private class Call implements AbstractDevice {
        private AbstractNodeIterator nodes;
        private HierName name;
        private ConnectionInfo ci;

        public Call(final ConnectionInfo ci) {
            this.ci = ci;
            this.name = ci.nameInParent;

            if (!portMap.containsKey(ci.child)) {
                new PortIterator(ci);
            }
            final Set formal = (Set) portMap.get(ci.child);
            final List list = new ArrayList();
            for (Iterator i = formal.iterator(); i.hasNext(); ) {
                final HierName port = (HierName) i.next();
                final HierName parentPort = ci.getParentName(port);
                final CellNet net = ci.parent.getNet(parentPort);
                final CellNet.Geometry geometry = net.getGeometry();
                if (Float.isNaN(top)) {
                    if (outputRC && geometry != null) {
                        final CellNet childNet = ci.child.getNet(port);
                        list.add(geometry.mapName(childNet, parentPort));
                    } else {
                        list.add(net.canonicalName);
                    }
                } else {
                    if (net.isInternalNet()) {
                        list.add(makeSource(net.canonicalName));
                        list.add(net.canonicalName);
                    } else if (top < 0) {
                        list.add(makeSource(net.canonicalName));
                        list.add(makeSink(net.canonicalName));
                    } else {
                        list.add(net.canonicalName);
                        list.add(net.canonicalName);
                    }
                }
            }

            this.nodes = new SimpleAbstractNodeIterator(list.iterator()) {
                public AbstractNode next() {
                    return new PortNode(ci.parent, (HierName) iterator.next());
                }
            };
        }

        public void accept(final Visitor visitor) {
            visitor.subcircuitCall(name, new NetlistAdapter(ci.child, false, outputRC, stackMap, GND, Vdd, Float.isNaN(top) ? Float.NaN : -1, minWidth), nodes, Collections.EMPTY_MAP);
        }
    }

    private static class UnusedGateOutputException extends Exception {
    }

    private HalfOperator[] findHalfOperator(NetGraph.NetNode node)
        throws UnusedGateOutputException {
        List halfops = cell.getListHalfOperators();
        HalfOperator[] ops = new HalfOperator[2];
        int count = 0;
        /* Expecting a pull-down and a pull-up for a node.  Exit early if both
         * found.  Currently, this is a linear search, as the number of half
         * operators in a leaf cell is expected to be small. */
        for (Iterator i = halfops.iterator(); i.hasNext() && count < 2;) {
            HalfOperator op = (HalfOperator) i.next();
            if (node.equals(op.outputNode)) {
                ops[count++] = op;
            }
        }
        if (count != 2) {
            throw new UnusedGateOutputException();
        }
        return ops;
    }

    /* gateDirection is a cache of whether a node in a gate NetGraph is ever
     * used as a gate node in a transistor. */
    private static final Map gateDirection = new HashMap();
    private class Gate implements AbstractDevice {
        private final NetGraph.GateInstance gate;
        private final HierName name;
        private final AbstractNodeIterator nodes;
        private final Map params;

        private boolean isGateNode(HierName node) {
            final NetGraph g = gate.getNetGraph();
            Set gateSet = (Set) gateDirection.get(g);
            if (gateSet == null) {
                gateSet = new HashSet();
                for (Iterator i = g.getEdges().iterator(); i.hasNext(); ) {
                    NetGraph.NetEdge edge = (NetGraph.NetEdge) i.next();
                    gateSet.add(edge.gate.name);
                }
                gateDirection.put(g, gateSet);
            }
            return gateSet.contains(node);
        }

        private void addWidth(final HalfOperator op, final Map params) {
            String np = null; 
            switch (op.driveDirection) {
              case 0: np = "NW"; break;
              case 1: np = "PW"; break;
              default: Debug.assertTrue(false, "Don't know how to handle driveDirection " + op.driveDirection);
            }
            /* Originally, this only outputs ports that are used, so any stack
             * depths only used by floating transistors are not included here.
             * But, there is some difficulty not emitting these unused ports
             * when emitting from a netlist block, which causes difficulty when
             * using aspice, since it expects parameters to match exactly.
             * This problem is resolved by adding the usued port to the gate
             * definitions (BUG 1273).  But, this fix causes aspice to complain
             * about the netlist generated directly from PRS.  So, just emit
             * all the ports, even ones unused, since that appears to be
             * harmless (BUG 1625). */
            /*
            int[] depths = op.getDepths();
            Debug.assertTrue(depths.length != 0, "Half operator has 0 depth!");
            for (int i = 0; i < depths.length; i++) {
                params.put(np + depths[i], new Double(op.getWidth(depths[i])));
            }
            */
            final Collection depths = op.depths;
            assert depths.size() > 0 : "Half operator has 0 depth!";
            for (Iterator i = depths.iterator(); i.hasNext(); ) {
                final int depth = ((Integer) i.next()).intValue();
                params.put(np + depth,
                           new Double(roundMinWidth(op.getWidth(depth))));
            }
        }

        private void nonStaticizerGate(final HalfOperator[] ops) {
            Debug.assertTrue(ops.length == 2, "Invalid ops.length = " + ops.length);
            Debug.assertTrue(ops[0].driveDirection != ops[1].driveDirection, "Found half operators that are the same direction!");
            addWidth(ops[0], params);
            addWidth(ops[1], params);
        }

        private void prechargeGate(final NetGraph.NetNode node) {
            for (Iterator i = node.getEdges(); i.hasNext(); ) {
                final NetGraph.NetEdge e = (NetGraph.NetEdge) i.next();
                if (e.precharge) {
                    final String t = e.type == DeviceTypes.N_TYPE ? "N" : "P";
                    params.put(t + "W1", new Double(e.width));
                    params.put(t + "L", new Double(e.length));
                }
            }
            assert false : "Precharge primitive attached on " + node +
                           " but no precharge transistor found";
        }

        private void staticizerGate(final NetGraph.NetNode node) {
            Debug.assertTrue(node.isStaticized(), "isStaticized returned false for " + node + ", but a STATICIZER gate is attached to it.");
            double nlength = -1, nwidth = -1;
            double plength = -1, pwidth = -1;
            Iterator i;
            i = node.getFeedbackEdges().iterator();
            while (i.hasNext()) {
                final NetGraph.NetEdge edge = (NetGraph.NetEdge) i.next();
                switch (edge.type) {
                case DeviceTypes.N_TYPE:
                    Debug.assertTrue(nlength == -1, "More than one staticizer for node " + node + "?");
                    nlength = edge.length;
                    nwidth  = edge.width;
                    break;
                case DeviceTypes.P_TYPE:
                    Debug.assertTrue(plength == -1, "More than one staticizer for node " + node + "?");
                    plength = edge.length;
                    pwidth  = edge.width;
                    break;
                default: Debug.assertTrue(false, "Don't know how to handle edge type " + edge.type);
                }
            }
            Debug.assertTrue(nlength > 0 && nwidth > 0 && plength > 0 && pwidth > 0);
            params.put("NW", new Double(nwidth));
            params.put("PW", new Double(pwidth));
            params.put("NL", new Double(nlength));
            params.put("PL", new Double(plength));
        }

        private void feedbackWidth(final NetGraph.NetNode node) {
            final SortedSet<Double> nws = new TreeSet<Double>();
            final SortedSet<Double> pws = new TreeSet<Double>();
            for (Iterator i = node.getFeedbackEdges().iterator();
                 i.hasNext(); ) {
                final NetGraph.NetEdge edge = (NetGraph.NetEdge) i.next();
                if (edge.isFeedbackOnly() && edge.library) {
                    if (edge.type == DeviceTypes.N_TYPE) {
                        nws.add(edge.width);
                    } else {
                        pws.add(edge.width);
                    }
                }
            }
            if (!nws.isEmpty()) {
                params.put("NWS", nws.last());
                assert nws.size() == 1 : "Too many NWS: " + nws + " at " +
                                         cell.typeName + "/" + node;
            }
            if (!pws.isEmpty()) {
                params.put("PWS", pws.last());
                assert pws.size() == 1 : "Too many PWS: " + pws + " at " +
                                         cell.typeName + "/" + node;
            }
        }

        public Gate(final NetGraph.GateInstance gate,
                    final NetGraph.NetNode n, final boolean isStaticizer)
        throws UnusedGateOutputException {
            this.gate = gate;
            final Map map = gate.getUnmodifiableMap();
            this.nodes = new SimpleAbstractNodeIterator(map.entrySet().iterator()) {
                public AbstractNode next() {
                    final Map.Entry entry = (Map.Entry) iterator.next();
                    final HierName from = (HierName) entry.getKey();
                    final HierName to = (HierName) entry.getValue();
                    final NetGraph.NetNode node = graph.findNetNode(to);
                    return isStaticizer ? new Node(node, false) : new Node(node, isGateNode(from));
                }
            };
            this.params = new TreeMap();
            if (n.isStaticizerInverter()) {
                this.name = n.name;
                // no width parameters for small inverters for now
            } else if (isStaticizer) {
                this.name = n.name.appendString("_stat");
                staticizerGate(n);
            } else if (gate.isPrechargePrimitive()) {
                this.name = n.name.appendString("_precharge");
                prechargeGate(n);
            } else {
                this.name = n.name;
                final HalfOperator ops[] = findHalfOperator(n);
                nonStaticizerGate(ops);
                feedbackWidth(n);
            }
        }
        public void accept(final Visitor visitor) {
            visitor.subcircuitCall(name,
                new GateAdapter(gate.getNetGraph(), gate.getType()),
                nodes, params);
        }
    }

    public NetlistAdapter(final CellType cell, final boolean outputRC) {
        this(cell, outputRC, null, null, null, Float.NaN, Float.NaN);
    }

    public NetlistAdapter(final CellType cell, final boolean outputRC,
                          final Map stackMap, final HierName GND,
                          final HierName Vdd, final float minWidth) {
        this(cell, true, outputRC, stackMap, GND, Vdd, Float.NaN, minWidth);
    }

    /**
     * Construct a new NetlistAdapter.
     * @param cell Cell to make an AbstractNetlist from.
     * @param outputRC Output resistors and capacitors suitable for SPICE
     * @param stackMap Map from a pair of depth and type
     * Pair&lt;Integer, Integer (DeviceTypes.N_TYPE | DeviceTypes.P_TYPE)&gt; to
     * NetGraph.
     **/
    public NetlistAdapter(final CellType cell, final boolean outputRC,
                          final Map stackMap, final HierName GND,
                          final HierName Vdd, final float top,
                          final float minWidth) {
        this(cell, true, outputRC, stackMap, GND, Vdd, top, minWidth);
    }

    private NetlistAdapter(final CellType cell, boolean subcircuits,
                           boolean outputRC, final Map stackMap,
                           final HierName GND, final HierName Vdd,
                           final float top, final float minWidth) {
        this.cell = cell;
        graph = cell.transistors;
        this.subcircuits = subcircuits;
        this.outputRC = outputRC;
        portMap = new HashMap();
        this.stackMap = stackMap;
        this.GND = GND;
        this.Vdd = Vdd;
        this.top = top;
        this.minWidth = minWidth;
    }

    public AbstractDeviceIterator getDevices() {
        /* We do not want to output transistors that are a part of the gate
         * library, since they are already output as subcircuit calls to
         * gates. */
        final Iterator edge =
            new FilteringIterator(graph.getEdges().iterator(), notLibraryEdge);
        final ArrayList calls = new ArrayList();
        if (!cell.isInternalEnv()) {
            for (Iterator iter = cell.getAllSubcellConnections().iterator();
                 iter.hasNext(); ) {
                ConnectionInfo ci = (ConnectionInfo) iter.next();
                if (!ci.child.isWiringCell) calls.add(ci);
            }
        }

        final ArrayList devices = new ArrayList();

        if (stackMap == null) {
            devices.add(new SimpleAbstractDeviceIterator(edge) {
                public AbstractDevice next() {
                    return new Transistor((NetGraph.NetEdge) iterator.next());
                }
            });
        } else {
            final Collection stacks = getStacks(graph);
            devices.add(new SimpleAbstractDeviceIterator(stacks.iterator()));
        }

        devices.add(new SimpleAbstractDeviceIterator(calls.iterator()) {
            public AbstractDevice next() {
                return new Call((ConnectionInfo) iterator.next());
            }
        });

        if (outputRC) {
            for (Iterator i = cell.getAllNets().iterator(); i.hasNext(); ) {
                final CellNet net = (CellNet) i.next();
                final CellNet.Geometry geometry = net.getGeometry();
                if (geometry==null) continue;
                if (net.isInternalNet() || // include small inverters created in netlist
                    net.getListSources().size() > 0 || net.getListSinks().size() > 0)
                    devices.add(geometry.getDeviceIterator());
            }
        }

        // top > 0 ==> !Float.isNaN(top)
        if (top > 0) {
            assert !Float.isNaN(top);
            final ArrayList portCaps = new ArrayList();
            for (AbstractNodeIterator i = getOutputNodes(); i.hasNext(); ) {
                final AbstractNode port = i.next();
                final HierName canon = port.getCanonicalName();
                if (canon.equals(GND) || canon.equals(Vdd)) continue;
                portCaps.add(new Capacitor(canon, new PortNode(cell, GND),
                                           port, top));
            }
            devices.add(new SimpleAbstractDeviceIterator(portCaps.iterator()));
        }

        final ArrayList gates = new ArrayList();
        for (Iterator iter = graph.getNodes().iterator(); iter.hasNext();) {
            final NetGraph.NetNode node = (NetGraph.NetNode) iter.next();
            final NetGraph.GateInstance gate = node.getGate();
            if (gate != null) {
                try {
                    gates.add(new Gate(gate, node, false));
                } catch (UnusedGateOutputException e) {
                    if (cell.isFixedSize()) {
                        System.err.println("Warning: in fixed sized cell " +
                            cell.typeName + ", " + node.name +
                            " is the output of a matched gate, but does" +
                            " not drive anything.");
                    } else {
                        System.err.println(
                            "Error: in cell " + cell.typeName + ", " +
                            node.name + " is the output of a matched gate, " +
                            "but does not drive anything.  Please remove the " +
                            "unnecessary production rules to reduce " +
                            "transistor count.");
                    }
                }
            }
            final NetGraph.GateInstance staticizer = node.getStaticizer();
            if (staticizer != null) {
                try {
                    gates.add(new Gate(staticizer, node, true));
                } catch (UnusedGateOutputException e) {
                    throw new AssertionError(
                        "Staticizer gate treated like non-staticizer gate: " +
                        cell.typeName + "/" + node.name);
                }
            }
        }
        devices.add(new SimpleAbstractDeviceIterator(gates.iterator()));

        return new ConcatAbstractDeviceIterator(devices);
    }

    // XXX: For now, assume all nodes are output nodes
    public AbstractNodeIterator getInputNodes() {
        return new SimpleAbstractNodeIterator(Collections.EMPTY_LIST.iterator());
    }

    private class PortIterator implements AbstractNodeIterator {
        private final ConnectionInfo ci;
        private final Iterator iterator;

        public PortIterator(final ConnectionInfo ci) {
            this.ci = ci;
            Set set = (Set) portMap.get(ci.child);
            if (set == null) {
                final Map map = new HashMap();
                for (ConnectionInfo.NameIterator it = ci.childIterator();
                     it.hasNext(); ) {
                    HierName port = (HierName) it.next();
                    CellNet net = ci.child.getNet(port);
                    HierName old = (HierName) map.get(net);
                    if (old != null && old.compareTo(port) <= 0)
                        continue;
                    map.put(net, port);
                }
                set = new TreeSet(map.values());
                portMap.put(ci.child, set);
            }
            final ArrayList ports = new ArrayList();
            for (Iterator i = set.iterator(); i.hasNext(); ) {
                final HierName hn = (HierName) i.next();
                final HierName cn = ci.child.getNet(hn).canonicalName;
                // top < 0 ==> !Float.isNaN(top)
                if (top < 0) {
                    assert !Float.isNaN(top);
                    ports.add(makeSource(cn));
                    ports.add(makeSink(cn));
                } else {
                    ports.add(cn);
                }
            }
            iterator = ports.iterator();
        }

        public boolean hasNext() {
            return iterator.hasNext();
        }

        public AbstractNode next() {
            HierName port = (HierName) iterator.next();
            return new PortNode(ci.child, port);
        }
    }

    public AbstractNodeIterator getOutputNodes() {
        if (cell.parentcellconnections.size() > 0) {
            final ConnectionInfo ci = (ConnectionInfo) cell.parentcellconnections.get(0);
            return new PortIterator(ci);
        } else {
            return new SimpleAbstractNodeIterator(Collections.EMPTY_LIST.iterator());
        }
    }

    public AbstractNodeIterator getNodes() {
        final Iterator i = graph.getNodes().iterator();
        return new SimpleAbstractNodeIterator(i);
    }

    public AbstractNetlistIterator getSubcircuits() {
        final ArrayList subckt = new ArrayList();
        if (subcircuits) {
            cell.walkOnce(new CellTypeProcessor() {
                public void processCellType(CellType c) {
                    if (cell != c && !c.isWiringCell) subckt.add(c);
                }
            });
        }

        final Iterator i = subckt.iterator();

        return new SimpleAbstractNetlistIterator(i) {
            public AbstractNetlist next() {
                return new NetlistAdapter((CellType) iterator.next(), false, outputRC, stackMap, GND, Vdd, Float.isNaN(top) ? Float.NaN : -1, minWidth);
            }
        };
    }

    public HierName getName() {
        return makeHierName(cell.typeName);
    }
}
