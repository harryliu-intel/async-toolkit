/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
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

import java.io.BufferedReader;
import java.io.InputStream ;
import java.io.File;
import java.io.FileReader;
import java.io.FileInputStream ;
import java.io.Writer;
import java.io.PrintWriter;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.FileOutputStream;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import com.avlsi.cast.CastFile;
import com.avlsi.cast.CastFileParser;

import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.util.DirectiveUtils;

import com.avlsi.cast.impl.ArrayValue;
import com.avlsi.cast.impl.NodeValue;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;
import com.avlsi.cell.ExclusiveNodeSet;
import com.avlsi.fast.BlockIterator;
import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.NetlistBlock;
import com.avlsi.fast.VerilogBlock;
import com.avlsi.file.cdl.parser.Template;
import com.avlsi.file.cdl.parser.CDLSimpleInterface;
import com.avlsi.file.common.HierName;
import com.avlsi.file.ext.ExtCell;
import com.avlsi.file.ext.parse.ExtParser;
import com.avlsi.io.FileSearchPath;
import com.avlsi.prs.ProductionRule;
import com.avlsi.util.container.AliasedMap;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.Namespace;
import com.avlsi.util.container.NaturalOrderComparator;
import com.avlsi.util.container.Pair;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.exception.AssertionFailure;
import com.avlsi.util.functions.UnaryAction;
import com.avlsi.util.text.StringUtil;
import com.avlsi.tools.cadencize.SubCellFile;
import com.avlsi.tools.cadencize.CadencizeDataInterfaceActionHandler;
import com.avlsi.tools.cadencize.CadencizeOutputToSkill;

import com.avlsi.tools.ext2cdl.Ext2Cdl;

import com.avlsi.util.container.StringContainerIterator;

import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
/**
 * Extracts information relevant for cadence from a cast CellInterface.
 * In the future, I expect that this functionality will be rolled into
 * CellInterface.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class Cadencize {
    public static final int VERILOG_NONE = 0;
    public static final int VERILOG_ALL = 1;
    public static final int VERILOG_PARTIAL = 2;

    public interface BlockCallback {
        /**
         * Returns true if the block <code>block</code> should be considered
         * when figuring out if a node in the port list is used.
         **/
        boolean mark(CellInterface cell, String block);

        /**
         * Returns true if the given subcell should be included in CadenceInfo.
         **/
        boolean subcell(CellInterface cell, HierName subCellName,
                        CellInterface subCell);

        /**
         * Returns true if the port should be marked as used.
         **/
        boolean used(CellInterface cell, HierName portCanon, boolean wiring,
                     boolean used);
    }

    public static class DefaultCallback implements BlockCallback {
        private final int considerVerilog;
        public DefaultCallback(final int considerVerilog) {
            this.considerVerilog = considerVerilog;
        }
        public boolean mark(CellInterface cell, String block) {
            final boolean impl = cell.containsCompletePrs() ||
                                 cell.containsCompleteSubcells() ||
                                 cell.containsNetlist();

            if (block == BlockInterface.PRS) {
                return cell.containsCompletePrs() &&
                       cell.hasRealProductionRule();
            } else if (block == BlockInterface.SUBCELL) {
                return cell.containsCompleteSubcells();
            } else if (block == BlockInterface.VERILOG) {
                return (considerVerilog == VERILOG_ALL ||
                        (considerVerilog == VERILOG_PARTIAL && !impl)) &&
                       cell.containsVerilog();
            } else if (block == BlockInterface.NETLIST) {
                return cell.containsNetlist();
            } else if (block == BlockInterface.CSP) {
                return !impl && cell.containsRunnableCsp();
            } else {
                return false;
            }
        }
        public boolean subcell(CellInterface cell, HierName subCellName,
                               CellInterface subCell) {
            return standardSubcellCallback(cell, subCellName, subCell);
        }
        public boolean used(CellInterface cell, HierName portCanon,
                            boolean wiring, boolean used) {
            return wiring || used;
        }
    }

    /**
     * If a netlist block exists, don't look at the subcells or prs block to
     * determine if a node in the port list is used or not.  Verilog block is
     * not examined.
     **/
    public static BlockCallback NETLIST_PRIORITY =
        new DefaultCallback(VERILOG_NONE) {
            public boolean mark(CellInterface cell, String block) {
                if (block == BlockInterface.NETLIST) {
                    return cell.containsNetlist();
                } else {
                    return !cell.containsNetlist() && super.mark(cell, block);
                }
            }
        };

    public static Comparator DEFAULT_CANONICALNESS_COMPARATOR =
        System.getProperty("cadencize.old_port_names") == null ?
            HierName.getPortComparator() : new NaturalOrderComparator();

    /**
     * Map from string of type name to CadenceInfo.
     * Cache of all types that have been converted.
     * Type might be "BUF_1of4".
     **/
    private final Map<String,CadenceInfo> convertedCellMap =
        new TreeMap<String,CadenceInfo>();

    private final boolean raiseExcls;
    private final BlockCallback callback;
    private final Comparator canonicalnessComparator;

    /**
     * Class constructor.
     *
     * @param raiseExcls whether to bring exclusion properties from 
     *   exported nets of subcells up.
     **/
    public Cadencize(final boolean raiseExcls) {
        this(raiseExcls, false);
    }

    public Cadencize(final boolean raiseExcls, final boolean considerVerilog) {
        this(raiseExcls, considerVerilog ? VERILOG_ALL : VERILOG_NONE);
    }

    public Cadencize(final boolean raiseExcls, final int considerVerilog) {
        this(raiseExcls, new DefaultCallback(considerVerilog));
    }

    public Cadencize(final boolean raiseExcls, final BlockCallback callback) {
        this(raiseExcls, callback, DEFAULT_CANONICALNESS_COMPARATOR);
    }

    public Cadencize(final boolean raiseExcls, final BlockCallback callback,
                     final Comparator canonicalnessComparator) {
        this.raiseExcls = raiseExcls;
        this.callback = callback;
        this.canonicalnessComparator = canonicalnessComparator;
    }

    public CadenceInfo getExistingCadenceInfo( final String fullyQualifiedCellName ) {
        final CadenceInfo ci = convertedCellMap.get( fullyQualifiedCellName );
        return ci;
    }

    public CadenceInfo removeCachedCadenceInfo( final String fullyQualifiedCellName ) {
        return convertedCellMap.remove( fullyQualifiedCellName );
    }

    private AliasedMap prefixAliasedSet(final HierName instance,
                                        final AliasedSet localNodes) {
        return prefixAliasedSet(instance, localNodes, false);
    }

    private AliasedMap prefixAliasedSet(final HierName instance,
                                        final AliasedSet localNodes,
                                        final boolean isPort) {
        try {
            return Namespace.prefixNamespace(
                    instance,
                    localNodes.toAliasedMap(
                        new AliasedMap.MergeFunction() {
                            public Object merge(Object o1, Object o2) {
                                return Boolean.FALSE;
                            }
                        },
                        Boolean.FALSE),
                    isPort);
        } catch (AliasedMap.MergeFailedException e) {
            throw new AssertionError(e);
        }
    }

    public static boolean standardSubcellCallback(
            CellInterface cell, HierName subCellName, CellInterface subCell) {
        return !CellUtils.isWiring(subCell) &&
               !cell.isInlinedSubcell(subCellName);
    }

    public CadenceInfo convert(CellInterface cell) {
        final String type = cell.getFullyQualifiedType();
        CadenceInfo ci = getExistingCadenceInfo( type );

        if (ci != null)
            return ci;

        // System.err.println("cell is " + type);

        ci = new CadenceInfo(type, raiseExcls, canonicalnessComparator);

        if (cell.isNode()) {
            convertedCellMap.put(type, ci);
            return ci;
        }

        // let the real handling begin
        
        // local and port subcells
        // for all port cells, add the prefixed namespace of 
        // their port nodes to the port nodes of this cell.
        // Ie if there is an e1of2 called "l" in the port list,
        // we will add "l.0", "l.1", "l.e", "l.d[0]", "l.d[1]" to
        // the port nodes of this cell.
        // Note that the cast definition of this is:
        // define e1of2(node "0", "1", e; node d[0..1])  {
        //    "0" = d[0]; "1" = d[1];
        // }
        for (final Iterator/*<Pair<HierName,CellInterface>*/ iSubCell =
             cell.getAllSubcellPairs(); 
             iSubCell.hasNext(); ) {
            final Pair/*<HierName,CellInterface>*/ p =
                (Pair) iSubCell.next();

            final HierName subCellName = (HierName) p.getFirst();
            final CellInterface subCell = (CellInterface) p.getSecond();
            final boolean isPortSubcell = cell.isPortSubcell(subCellName) ||
                                          cell.isChannel();

            // System.err.println("found subcell " + subCellName + " : " +
                    // subCell.getType() + " of cell " + type);

            if (subCell.isNode()) {
                final HierName node = HierName.makePortName(subCellName,
                                                            isPortSubcell);
                ci.addLocalNode(node);
                if (isPortSubcell)
                    ci.addPortNode(node);
                Debug.assertTrue(!subCell.hasRealProductionRule());
            } else {
                final CadenceInfo subci = convert(subCell);
                // if the cell is inlined, add the prefixed local nodes,
                // otherwise, add the prefixed port nodes
                final AliasedMap subcellPortNodes;
                if (cell.isInlinedSubcell(subCellName)) {
                    assert !isPortSubcell;
                    subcellPortNodes =
                        prefixAliasedSet(subCellName, subci.getLocalNodes());
                } else {
                    subcellPortNodes = 
                        Namespace.prefixNamespace(subCellName,
                                subci.getPortNodes(),
                                isPortSubcell);
                }

                // make all the ports non-exported
                for (final Iterator/*<HierName>*/ iSubPort =
                        subcellPortNodes.getCanonicalKeys();
                        iSubPort.hasNext(); ) {
                    final HierName k = (HierName) iSubPort.next();
                    subcellPortNodes.setValue(k, Boolean.FALSE);
                }

                // bring the exclusion properties up if requested
                if (raiseExcls) {
                    // get the exclusions for subcell, then check
                    // to see whether we export them, or 
                    for (Iterator/*<ExclusiveNodeSet>*/ iExcl =
                            subci.getPortExclusiveNodeSets().getIterator();
                            iExcl.hasNext(); ) {
                        final ExclusiveNodeSet excl =
                            (ExclusiveNodeSet) iExcl.next();
                        final ExclusiveNodeSet prefixedExcl =
                            excl.prefixNames(subCellName);

                        ci.getLocalExclusiveNodeSets()
                          .addExclusiveNodeSet(prefixedExcl);
                    }
                }

                // System.err.println("doing subcell " + subCellName + " : " +
                        // subCell.getType() + " of cell " + type);

                // System.err.println("Sub namespace: \n" +
                        // Namespace.toString(subci.getPortNodes()));
                // System.err.println("prefixed sub namespace: \n" +
                        // Namespace.toString(subcellPortNodes));

                if (callback.subcell(cell, subCellName, subCell)) {
                    Debug.assertTrue(!isPortSubcell);

                    ci.addSubcellPair(subCellName, subci);
                }

                // Add the nodes and connections of the subcell's
                // port namespace to the local namespace.
                Namespace.addNamespace(ci.getLocalNodes(),
                        new AliasedSet/*<HierName>*/(subcellPortNodes));

                // if it is a port subcell, also add to the port subcells
                if (isPortSubcell) {
                    try {
                        Namespace.addNamespace(ci.getPortNodes(),
                                subcellPortNodes);
                    } catch (AliasedMap.MergeFailedException e) {
                        throw new AssertionFailure(e);
                    }
                }

                // System.err.println(type + " local ns: \n" +
                        // Namespace.toString(ci.getLocalNodes()));
                // System.err.println(type + " port ns: \n" +
                        // Namespace.toString(ci.getPortNodes()));

            }
        }

        if (raiseExcls) {
            // add the locally defined exclusions:
            for (final Iterator/*<ExclusiveNodeSet>*/ iLocalExcl =
                    cell.getLocalExclusiveNodeSets().getIterator();
                    iLocalExcl.hasNext(); ) {
                final ExclusiveNodeSet excl =
                    (ExclusiveNodeSet) iLocalExcl.next();

                ci.getLocalExclusiveNodeSets().addExclusiveNodeSet(excl);
            }

            // filter the local exclusions using port nodes to obtain the
            // port exclusions
            for (final Iterator/*<ExclusiveNodeSet>*/ iLocalExcl =
                    ci.getLocalExclusiveNodeSets().getIterator();
                    iLocalExcl.hasNext(); ) {
                final ExclusiveNodeSet localExcl =
                    (ExclusiveNodeSet) iLocalExcl.next();

                final List/*<HierName>*/ nodes =
                    new ArrayList/*<HierName>*/(localExcl.getNumNodes());

                for (final Iterator/*<HierName>*/ iExclNode =
                        localExcl.getNodes();
                        iExclNode.hasNext(); ) {
                    final HierName n = (HierName) iExclNode.next();

                    if (ci.getPortNodes().getCanonicalKey(n) != null)
                        nodes.add(n);
                }

                if (nodes.size() > 1)
                    ci.getPortExclusiveNodeSets().addExclusiveNodeSet(
                            new ExclusiveNodeSet(localExcl.getHiLo(), nodes));
            }
        }


        // System.err.println(type + " cast namespace:\n" +
                // ((com.avlsi.cell.CellImpl) cell).getNamespaceString());

        // take care of cast connections
        absorbCastConnections(cell, ci);

        // 5/21/2001: I believe that this loop can be removed because
        // globals are now ports on all subcells, so they should already
        // be in the local nodes.  This loop was a second pass to add
        // in globals.

        // take care of globals & shit
        // perhaps this could be ditched
        for (final Iterator/*<HierName>*/ iCanonLocalNode =
             ci.getLocalNodes().getCanonicalKeys();
             iCanonLocalNode.hasNext(); ) {

            final HierName c1 = (HierName) iCanonLocalNode.next();

            for (final Iterator/*<HierName>*/ iConnLocalNode =
                 ci.getLocalNodes().getAliases(c1);
                 iConnLocalNode.hasNext(); ) {

                final HierName n1 = (HierName) iConnLocalNode.next();

                // This line is mysterious:  We need it, but I don't
                // understand why.  --jmr
                if (ci.getPortNodes().contains(n1))
                    ci.connectPortNodes(n1, c1);
            }
        }

        // figure out which ports are used by prs or subcells
        markUsedPorts(cell, ci);

        // System.err.println(type + " final local ns: \n" +
                // Namespace.toString(ci.getLocalNodes()));
        // System.err.println(type + " final port ns: \n" +
                // Namespace.toString(ci.getPortNodes()));

        convertedCellMap.put(type, ci);
        return ci;
    }

    /**
     * Absorb aliases specified in CAST.
     **/
    private void absorbCastConnections(final CellInterface cell,
                                       final CadenceInfo ci) {
        for (final Iterator/*<HierName>*/ iCanonCastNode =
             cell.getCanonicalNodes();
             iCanonCastNode.hasNext(); ) {

            final HierName canon = (HierName) iCanonCastNode.next();
            HierName fndLocal = null;
            HierName fndPort = null;

            for (final Iterator/*<HierName>*/ iConnCastNode =
                    cell.getConnectedNodes(canon);
                 iConnCastNode.hasNext(); ) {

                final HierName conn = (HierName) iConnCastNode.next();

                // Here is an example of where a node in the cast will
                // not be in the CadenceInfo:
                // define X()(node a) {
                //     node c;
                //     node b = a;
                //     prs { a => c-; }
                //     ... and b is never used later
                // }
                // The node b is merely a convenience.  Maybe there is
                // an example where b is used.  
                //
                // REVIEW: I don't know when this if statement will be false.
                // Maybe this was from when globals were really globals, they're
                // ports now.
                if (ci.getLocalNodes().contains(conn)) {
                    final boolean isPort = ci.getPortNodes().contains(conn) ||
                                           cell.isChannel();
                    final HierName portConn =
                        HierName.makePortName(conn, isPort);

                    if (fndLocal == null)
                        fndLocal = portConn;
                    else
                        ci.connectLocalNodes(fndLocal, portConn);

                    if (isPort) {
                        if (fndPort == null)
                            fndPort = portConn;
                        else
                            ci.connectPortNodes(fndPort, portConn);
                    }
                }
            }
        }
    }

    /**
     * Do almost everything <code>convert</code> does, except do not fully
     * examine aliases in subcells that are not channels or nodes.  For those
     * subcells, only examine aliases in their port subcells.  Except if the
     * subcell is specified in <code>allAliases</code>, then its aliases are
     * examined in their entirety.
     *
     * @param cell cell for which to return alias information
     * @param allAliases a map from subcell instance name
     * (<code>HierName</code>) to whether it should be examined fully
     * (<code>Boolean.TRUE</code> or <code>Boolean.FALSE</code>).
     * @return all aliases in <code>cell</code>, including any aliases defined
     * in channels, port subcells of subcells, and any cells specified in
     * <code>allAliases<code>
     **/
    public AliasedSet convertChannels(final CellInterface cell,
                                      final Map allAliases) {
        final CadenceInfo ci =
            new CadenceInfo(cell.getFullyQualifiedType(), false);

        for (final Iterator/*<Pair<HierName,CellInterface>*/ iSubCell =
             cell.getAllSubcellPairs(); iSubCell.hasNext(); ) {
            final Pair/*<HierName,CellInterface>*/ p = (Pair) iSubCell.next();

            final HierName subCellName = (HierName) p.getFirst();
            final CellInterface subCell = (CellInterface) p.getSecond();
            final boolean isPortSubcell = cell.isPortSubcell(subCellName);

            if (subCell.isNode()) {
                ci.addLocalNode(HierName.makePortName(subCellName,
                                                      isPortSubcell));
            } else {
                if (subCell.isChannel() ||
                    allAliases.get(subCellName) == Boolean.TRUE) {
                    final CadenceInfo subci = convert(subCell);
                    Namespace.addNamespace(ci.getLocalNodes(),
                            new AliasedSet/*<HierName>*/(
                                prefixAliasedSet(subCellName,
                                                 subci.getLocalNodes(),
                                                 isPortSubcell)));
                } else if (cell.isInlinedSubcell(subCellName)) {
                    assert !isPortSubcell;
                    final AliasedSet sublocals =
                        convertChannels(subCell, Collections.EMPTY_MAP);
                    Namespace.addNamespace(ci.getLocalNodes(),
                            new AliasedSet/*<HierName>*/(
                                prefixAliasedSet(subCellName, sublocals)));
                } else {
                    for (Iterator portSubCell = subCell.getPortSubcellPairs();
                         portSubCell.hasNext(); ) {
                        final Pair/*<HierName,CellInterface>*/ pp =
                            (Pair) portSubCell.next();

                        final HierName portCellName = (HierName) pp.getFirst();
                        final CellInterface portCell =
                            (CellInterface) pp.getSecond();
                        if (portCell.isNode()) {
                            ci.addLocalNode(HierName.append(subCellName,
                                                            portCellName));
                            continue;
                        }
                        final CadenceInfo subci = convert(portCell);

                        // Add the nodes and connections of the subcell's
                        // port namespace to the local namespace.
                        Namespace.addNamespace(ci.getLocalNodes(),
                            new AliasedSet/*<HierName>*/(
                                prefixAliasedSet(
                                    HierName.append(subCellName, portCellName),
                                    subci.getLocalNodes())));
                    }
                }
            }
        }

        absorbCastConnections(cell, ci);

        return ci.getLocalNodes();
    }

    /**
     * Determine which ports are used by a subcell or a production rule.
     * We only output ports that are used.
     **/
    private void markUsedPorts(final CellInterface cell, 
                               final CadenceInfo ci) {
        final Set/*<HierName>*/ usedLocalCanonSet =
            new HashSet/*<HierName>*/();

        // System.err.println("cell is " + cell.getType());

        // mark ports used by prs
        if (callback.mark(cell, BlockInterface.PRS)) {
            for (final Iterator/*<ProductionRule>*/ iPR =
                    cell.getProductionRuleSet().getProductionRules();
                    iPR.hasNext(); ) {
                final ProductionRule pr = (ProductionRule) iPR.next();

                pr.foreachHierName(new UnaryAction/*<HierName>*/() {
                    public void execute(final Object /*HierName*/ o) {
                        final HierName n = (HierName) o;
                        final HierName localCanon =
                            (HierName) ci.getLocalNodes().getCanonicalKey(n);
                        // Debug.assertTrue(localCanon != null);
                        // it's ok if we don't find a node.  This means that
                        // it was from an inlined subcell.
                        if (localCanon != null)
                            usedLocalCanonSet.add(localCanon);
                    }
                });
            }
        }

        // mark ports used by subcells
        if (callback.mark(cell, BlockInterface.SUBCELL)) {
            for (final Iterator/*<Pair<HierName,CadenceInfo>>*/ iSubCell =
                 ci.getSubcellPairIterator(); 
                 iSubCell.hasNext(); ) {
                final Pair/*<HierName,CadenceInfo>*/ p = (Pair) iSubCell.next();

                final HierName subCellName = (HierName) p.getFirst();
                final CadenceInfo subci = (CadenceInfo) p.getSecond();
                final AliasedMap/*<HierName,Boolean>*/ subPortNodes =
                    subci.getPortNodes();
                for (final Iterator/*<HierName>*/ iPort =
                     subPortNodes.getCanonicalKeys();
                     iPort.hasNext(); ) {
                    final HierName subcellPortName = (HierName) iPort.next();
                    final Boolean used =
                        (Boolean) subPortNodes.getValue(subcellPortName);
                    if (used.booleanValue()) {
                        final HierName localCanon =
                            (HierName) ci.getLocalNodes().getCanonicalKey(
                               HierName.append(subCellName, subcellPortName));

                        Debug.assertTrue(localCanon != null);
                        usedLocalCanonSet.add(localCanon);
                    }
                }
            }
        }

        // mark ports used by netlist body
        if (callback.mark(cell, BlockInterface.NETLIST)) {
            final BlockIterator bi =
                cell.getBlockInterface().iterator(BlockInterface.NETLIST);
            Debug.assertTrue(bi.hasNext(), "No netlist block found in " + cell.getFullyQualifiedType());
            final NetlistBlock block = (NetlistBlock) bi.next();
            final Template templ = block.getCDLTemplate();
            final AliasedMap ports = ci.getPortNodes();
            templ.execute(Collections.EMPTY_MAP, new CDLSimpleInterface() {
                private void addName(final HierName name) {
                    final Object key = ports.getCanonicalKey(name);
                    if (key != null) usedLocalCanonSet.add(key);
                }
                public void makeResistor(HierName name, HierName n1,
                                         HierName n2, double val) {
                    addName(n1); addName(n2);
                }
                public void makeCapacitor(HierName name, HierName npos,
                                          HierName nneg, double val) {
                    addName(npos); addName(nneg);
                }
                public void makeTransistor(HierName name, int type, HierName ns,
                                           HierName nd, HierName ng,
                                           HierName nb, double w, double l) {
                    addName(ns); addName(nd); addName(ng); addName(nb);
                }
                public void makeDiode(HierName name, int type, HierName npos,
                                      HierName nneg, double w, double l,
                                      double a, double p) {
                    addName(npos); addName(nneg);
                }
                public void makeInductor(HierName name, HierName npos,
                                         HierName nneg, double val) {
                    addName(npos); addName(nneg);
                }
                public void makeBipolar(HierName name, int type, HierName nc,
                                        HierName nb, HierName ne, double a) {
                    addName(nc); addName(nb); addName(ne);
                }
                public void makeCall(HierName name, String subName,
                                     HierName[] args, Map parameters) {
                    for (int i = 0; i < args.length; ++i) addName(args[i]);
                }
                public void beginSubcircuit(String subName, String[] in,
                                            String[] out) { }
                public void endSubcircuit(String subName) { }
            });
        }

        // mark ports used by verilog body or CSP body
        if (callback.mark(cell, BlockInterface.VERILOG) ||
            callback.mark(cell, BlockInterface.CSP)) {
            // It is unnecessary to eliminated unused ports in Verilog, because
            // not part of the flow that generates CDL for layout should care
            // about the verilog block, so mark all ports as used so that any
            // ports used in the verilog block would be included in the port
            // list
            // Assume all ports are used by the CSP block
            final AliasedMap ports = ci.getPortNodes();
            for (Iterator/*<HierName>*/ i = ports.getCanonicalKeys();
                    i.hasNext(); ) {
                usedLocalCanonSet.add(i.next());
            }
        }

        final boolean isWiring = 
            ((Boolean)DirectiveUtils.getTopLevelDirective(cell, DirectiveConstants.WIRING)).booleanValue();

        // for all ports, check to see if they were used, mark accordingly
        // If this is a wiring cell, we can't rule out that it's used
        for (final Iterator/*<HierName>*/ iPortCanon =
                ci.getPortNodes().getCanonicalKeys();
                iPortCanon.hasNext(); ) {
            final HierName portCanon = (HierName) iPortCanon.next();
            final HierName localCanon =
                (HierName) ci.getLocalNodes().getCanonicalKey(portCanon);
            final Boolean v =
                (Boolean) ci.getPortNodes().getValue(portCanon);
            // Debug.assertTrue(v.booleanValue() == FALSE);
            Debug.assertTrue(v == Boolean.FALSE);

            try {
                if (callback.used(cell, portCanon, isWiring,
                                  usedLocalCanonSet.contains(localCanon))) {
                    // System.err.println("port " + portCanon + " used");
                    ci.getPortNodes().addData(portCanon, Boolean.TRUE);
                } else {
                    // System.err.println("port " + portCanon + " not used");
                    ci.getPortNodes().addData(portCanon, Boolean.FALSE);
                }
            } catch (AliasedMap.MergeFailedException e) {
                throw new AssertionFailure(e);
            }
        }
    }

    private static void usage(final int exitStatus) {
        System.err.println("Usage: " +
                Cadencize.class.getName() +
		" [--ext-path=extractfilepath] [--cast-path=castpath]" +
                " ( --file=filename --rootcell=cellname ) |" +
                " ( unit cell subtype file.cast file.subcells output.il [ output.cdl [file.ext] ]) ");
        System.exit(exitStatus);
    }

    private static InputStream OpenInputFileStream( final String FileName ) 
        throws java.io.FileNotFoundException {
        
        File myFile = new File( FileName ) ;
        return new FileInputStream( myFile ) ;
    }

    private static OutputStream OpenOutputFileStream( final String FileName )
        throws java.io.FileNotFoundException {
        
        File myFile = new File( FileName );

        return new FileOutputStream( myFile );
    }

    private static PrintWriter MakePrintWriter( OutputStream s )
        throws java.io.UnsupportedEncodingException  {
        OutputStreamWriter MyWriter = new OutputStreamWriter( s, "UTF-8" ) ;
        return new PrintWriter( MyWriter, false );
    }

    private void processCell(
            final CastFileParser cfp,
            final String unitName,
            final String cellName,
            final String cellSubType,
            final String castFileName,
            final String subcellsFileName,
            final String outputSkillFileName,
            final String outputCDLFileName,
            final String extractFileName,
	    final FileSearchPath extractFileSearchPath ) throws Exception {
        OutputStream SkillOutStream = OpenOutputFileStream( outputSkillFileName ) ;
        
        PrintWriter SkillWriter = MakePrintWriter( SkillOutStream );

        SubCellFile MySubCells = new SubCellFile( OpenInputFileStream( subcellsFileName ) );
        

        // parse the file
        final CastFile cf = cfp.parse(castFileName);

        final CellInterface cell = cf.getCell(cellName);

        CadencizeDataInterfaceActionHandler MyActionHandler = 
            new CadencizeDataInterfaceActionHandler( unitName, 
                                                     cellSubType,
                                                     MySubCells.GetInstanceTypeMap() ) ;
        
        convert(cell).walk( MyActionHandler );
        
        CadenceDataInterface CadInfo = MyActionHandler.getCadenceData();
        
        CadencizeOutputToSkill.WriteSkill( SkillWriter, CadInfo );

        SkillOutStream.close();

        if ( outputCDLFileName != null ) {
            OutputStream CDLOutStream = OpenOutputFileStream( outputCDLFileName );
            PrintWriter CDLWriter = MakePrintWriter( CDLOutStream );

            ExtCell ext = null ;

            if ( extractFileName != null ) {
                ext = ExtParser.parse( cellName, extractFileName , extractFileSearchPath, false );
            }
            MessageFormat FETFormat = new MessageFormat( "M{0,number,############} {1} {2} {3} {4} {5} {6} {7}" );

            Ext2Cdl.WriteCDL( ext, CDLWriter, FETFormat, CadInfo );

            CDLOutStream.close();
        }
    }

    private void processLine(final CastFileParser cfp,
                             final String[] words,
			     final FileSearchPath extractFileSearchPath ) throws Exception {
        if (!(6 <= words.length && words.length <= 8))
            usage(1);

        final String unitName = words[0];
        final String cellName = words[1];
        final String cellSubType = words[2];
        final String castFileName = words[3];
        
        final String subcellsFileName = words[4];
        final String outputSkillFileName = words[5];
        final String outputCDLFileName = words.length > 6 ? words[6] : null;
        final String extractFileName = words.length > 7 ? words[7] : null;

        processCell(cfp, unitName, cellName, cellSubType, castFileName,
                subcellsFileName, outputSkillFileName,
                outputCDLFileName, extractFileName, extractFileSearchPath );
    }

    private void processFile(final CastFileParser cfp,
                             final String fileName,
                             final String rootCell,
			     final FileSearchPath extractFileSearchPath )
        throws Exception {
        final BufferedReader reader =
            new BufferedReader(new FileReader(fileName));

        int lineNum = 0;
        String line;
        while ((line = reader.readLine()) != null) {
            ++lineNum;
//            final String[] words = StringUtil.wordBreak(line);
            final String[] words = StringUtil.split(line, ' ');

            if (lineNum == 1 && !words[1].equals(rootCell)) {
                System.err.println(rootCell +
                        " specified as --rootCell, but not first in file");
                System.exit(2);
            }

            processLine(cfp, words, extractFileSearchPath );
        }
    }

    public static void main(String[] args) throws Exception {
        if (!(args.length == 4 || (6 <= args.length && args.length <= 8)))
            usage(1);


        CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
	CommandLineArgs argsWithConfigs =
	    new CommandLineArgsWithConfigFiles( parsedArgs ); 

	CommandLineArgs cachedArgs = 
	    new CachingCommandLineArgs( argsWithConfigs );

	CommandLineArgs theArgs = cachedArgs;

	final FileSearchPath castFileSearchPath = new FileSearchPath( theArgs.getArgValue( "cast-path", "." ),
								      System.getProperty( "user.home" ) );

        final CastFileParser cfp =
            new CastFileParser(castFileSearchPath);
        final Cadencize cad = new Cadencize(false);

	final FileSearchPath extractFileSearchPath = new FileSearchPath( theArgs.getArgValue( "ext-path", "." ),
									 System.getProperty( "user.home" ) );

	String fileName = theArgs.getArgValue( "file", null );
	String rootCell = theArgs.getArgValue( "rootcell", null );

	if ( ( fileName == null ) ^ ( rootCell == null ) ) {
	    usage(1);
	}
	else {
	    if ( fileName != null ) {
		cad.processFile(cfp, fileName, rootCell, extractFileSearchPath );
	    }
	    else {
		ArrayList argList = new ArrayList();

		StringContainerIterator strIter = theArgs.nonParsedArgumentsIterator();
		
		while ( strIter.hasNext() ) {
		    argList.add( strIter.next() );
		}

		cad.processLine(cfp, (String[]) argList.toArray(), extractFileSearchPath );
	    }
	}
    }
}
