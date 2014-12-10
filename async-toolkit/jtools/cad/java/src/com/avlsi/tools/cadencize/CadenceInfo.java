/*
 * Copyright 2002, 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.cadencize;

import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import com.avlsi.cell.ExclusiveNodeSet;
import com.avlsi.cell.ExclusiveNodeSets;
import com.avlsi.cell.HierarchyInterface;
import com.avlsi.file.common.HierName;
import com.avlsi.util.container.AliasedMap;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.MappingIterator;
import com.avlsi.util.container.NaturalOrderComparator;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.UnmodifiableIterator;
import com.avlsi.util.exception.AssertionFailure;
import com.avlsi.util.functions.UnaryFunction;

/**
 * Contains information relevant to use the cell with the cadence flow.
 * <ul>
 *   <li> Port list of the cell, including which ports are connected
 *        together.
 *   <li> Subcell list of the cell, including connections between
 *        two subcells, and subcells and ports.
 * </ul>
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class CadenceInfo implements HierarchyInterface {
    public static final class BooleanMergeFunction
        implements AliasedMap/*<HierName,Boolean>*/.MergeFunction {
        public Object /*Boolean*/ merge (final Object /*Boolean*/ v1,
                                         final Object /*Boolean*/ v2) {
            if (v1 == null)
                return v2;
            else if (v2 == null)
                return v1;
            else {
                final boolean b1 = ((Boolean) v1).booleanValue();
                final boolean b2 = ((Boolean) v2).booleanValue();
                return b1 || b2 ? Boolean.TRUE : Boolean.FALSE;
            }
        }
    }

    /**
     * The type of the cell.
     **/
    private final String cellType;

    /**
     * The port list, an AliasedMap from HierNames to Boolean, signaling
     * whether or not the node should appear in the layout port list.
     * @see #getPortNodes()
     **/
    private final AliasedMap/*<HierName,Boolean>*/ portNodes;

    /**
     * The local nodes, an AliasedSet of HierNames.  @see #getLocalNodes()
     **/
    private final AliasedSet/*<HierName>*/ localNodes;

    /**
     * A map from instance name of subcell to cadence info.
     **/
    private final Map<HierName,CadenceInfo> subcellMap;

    /**
     * Exclusive node sets for local nodes of this cell, and those for
     * involving ports of subcells.
     **/
    private final ExclusiveNodeSets localExcls;

    /**
     * Exclusive node sets for ports of this cell.
     **/
    private final ExclusiveNodeSets portExcls;


    /**
     * Class constructor.
     **/
    public CadenceInfo(final String cellType, final boolean doExcls) {
        this(cellType, doExcls, new NaturalOrderComparator());
    }

    /**
     * Class constructor.
     **/
    public CadenceInfo(final String cellType, final boolean doExcls,
                       final Comparator canonicalnessComparator) {
        this.cellType = cellType;
        this.portNodes =
            new AliasedMap/*<HierName,Boolean>*/(
                    new BooleanMergeFunction(),
                    new NaturalOrderComparator());
        this.localNodes =
            new AliasedSet/*<HierName>*/(canonicalnessComparator,
                                         new NaturalOrderComparator());
        this.subcellMap = new TreeMap<HierName,CadenceInfo>();
        this.localExcls = doExcls ? new ExclusiveNodeSets() : null;
        this.portExcls = doExcls ? new ExclusiveNodeSets() : null;
    }


    /**
     * The type of the cell.
     **/
    public String getType() {
        return cellType;
    }


    //
    // port nodes
    //

    /**
     * Returns a namespace of all the HierNames for nodes in the port list, as
     * specified by cast.  Any connections between nodes captured here as
     * aliased names.  The values of the map are Boolean, true if the node
     * should appear in the layout port list.
     *
     * This connections in this namespace are a proper subset of those in
     * getLocalNodes().  In particular, for any port node N,
     * getLocalNodes().getCanonicalKey(N) is consistent with
     * getPortNodes().getCanonicalKey(N).  This implies that the canonical
     * names in the result of getPortNodes() may not necessarily be port nodes.
     **/
    public AliasedMap/*<HierName,Boolean>*/ getPortNodes() {
        return portNodes;
    }

    public void addPortNode(final HierName portName) {
        try {
            portNodes.addData(portName, Boolean.FALSE);
        } catch (AliasedMap.MergeFailedException e) {
            throw new AssertionFailure(e);
        }
    }

    public void connectPortNodes(final HierName portName1,
                                 final HierName portName2) {
        try {
            portNodes.makeEquivalent(portName1, portName2);
        } catch (AliasedMap.MergeFailedException e) {
            throw new AssertionFailure(e);
        }
    }


    //
    // local nodes
    //

    /**
     * Returns an AliasedSet consisting of all the names for all local
     * nodes, including any that are in the port list of any child cells.
     *
     * Any globals used in a subcell are also listed as aliases of the global.
     * i.e. if we have a subcell a that uses global b, we will include b! = a.b!
     **/ 
    public AliasedSet/*<HierName>*/ getLocalNodes() {
        return localNodes;
    }

    public void addLocalNode(final HierName localName) {
        localNodes.add(localName);
    }

    public void connectLocalNodes(final HierName localName1,
                                  final HierName localName2) {
        localNodes.makeEquivalent(localName1, localName2);
    }

    /**
     * Returns an iterator through pairs of HierName and CadenceInfo, where
     * the first of the pair is the instance name, and the second is
     * the CadenceInfo for the cell.
     **/
    public Iterator<Pair<HierName,CadenceInfo>> getSubcellPairIterator() {
        return new MappingIterator<Entry<HierName,CadenceInfo>,
                                   Pair<HierName,CadenceInfo>>
                (subcellMap.entrySet().iterator(),
                new UnaryFunction<Entry<HierName,CadenceInfo>,
                                  Pair<HierName,CadenceInfo>>() {
                    public Pair<HierName,CadenceInfo> execute
                        (final Entry<HierName,CadenceInfo> e) {
                        return new Pair<HierName,CadenceInfo>
                                       (e.getKey(), e.getValue());
                    }
                });
    }

    public CadenceInfo getSubcell(final HierName name) {
        return subcellMap.get(name);
    }

    public void addSubcellPair(final HierName name, final CadenceInfo ci) {
        subcellMap.put(name, ci);
    }

    public ExclusiveNodeSets getLocalExclusiveNodeSets() {
        return localExcls;
    }

    public ExclusiveNodeSets getPortExclusiveNodeSets() {
        return portExcls;
    }


    /**
     * Walk the cell, executing the specified actions.
     **/
    public void walk(CadenceActionInterface a) {
        // cell type
        // System.err.println("type: " + getType());
        a.doType(getType());
        
        // ports, don't print connections among ports
        for (final Iterator/*<HierName>*/ i =
                getPortNodes().getCanonicalKeys();
                i.hasNext(); ) {
            final HierName n = (HierName) i.next();

            final boolean used =
                ((Boolean) getPortNodes().getValue(n)).booleanValue();
            // System.err.println("port " + n + (used ? " used" : " not used"));
            if (!used)
                continue;

            final String s = n.getCadenceString();

            // XXX: this could be wrong.  CDL files cannot handle a different
            // name for the port net and local net, although cadence itself
            // can.  Consider this cast: define X()(node b) { node b = a; }
            // The port name we would pick is "b", but we would pick "a"
            // as the local name because it is before "b" alphabetically.
            // This cannot be represented in CDL files.  The most obvious fix
            // is to rename the ports to be the most canonical local node
            // that is connected to that port.  So in this case, we would 
            // have .subckt X a.
            final HierName netName =
                (HierName) getLocalNodes().getCanonicalKey(n);

            // System.err.println("port/net: " + n + " / " + netName);

            a.doPort(s);
            // port connections, net to port
            a.doPortConnection(netName.getCadenceString(), s);
        }

        // local nets
        for (final Iterator/*<HierName>*/ i =
                getLocalNodes().getCanonicalKeys();
                i.hasNext(); ) {
            final HierName n = (HierName) i.next();
            final String s = n.getCadenceString();

            a.doNet(s);

            // aliases of local nodes
            a.doAliases(s,
                    new UnmodifiableIterator/*<String>*/(
                        new MappingIterator/*<HierName,String>*/(
                            getLocalNodes().getAliases(n),
                            new UnaryFunction/*<HierName,String>*/() {
                                public Object /*String*/ execute
                                    (final Object /*HierName*/ o) {
                                    final HierName alias = (HierName) o;
                                    return alias.getCadenceString();
                                }
                            })));
        }

        // subcells
        for (final Iterator/*<Pair<HierName,CadenceInfo>>*/ i =
                getSubcellPairIterator(); i.hasNext(); ) {
            final Pair/*<HierName,CadenceInfo>*/ p = (Pair) i.next();
            final HierName name = (HierName) p.getFirst();
            final CadenceInfo subci = (CadenceInfo) p.getSecond();

            a.doSubcell(name.getCadenceString(), subci.getType());

            // subcell connections, net to subcell + port & subcell + net
            for (final Iterator/*<HierName>*/ iPort =
                    subci.getPortNodes().getCanonicalKeys();
                 iPort.hasNext(); ) {
                final HierName subcellPortName = (HierName) iPort.next();

                final boolean used =
                    ((Boolean) subci.getPortNodes()
                                    .getValue(subcellPortName))
                        .booleanValue();
                // System.err.println("subcell port " + name + '/' +
                        // subcellPortName + (used ? " used" : " not used"));
                if (!used)
                    continue;

                final HierName subcellNetName
                    = (HierName) subci.getLocalNodes()
                                      .getCanonicalKey(subcellPortName);

                final HierName netName =
                    (HierName) getLocalNodes().getCanonicalKey(
                       HierName.append(name, subcellPortName));

                a.doSubcellConnection(netName.getCadenceString(),
                        name.getCadenceString(),
                        subcellPortName.getCadenceString(),
                        subcellNetName.getCadenceString());
            }
        }

    }
}
