/*
 * Copyright 2001-2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.lvs;

import java.io.File;
import java.io.InputStream;
import java.io.Reader;
import java.io.FileInputStream;
import java.io.InputStreamReader;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.NoSuchElementException;

import com.avlsi.cast.CastFileParser;

import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cell.CellImpl;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;
import com.avlsi.cell.SubcellCreationException;
import com.avlsi.file.aspice.AspiceFile;
import com.avlsi.file.aspice.Transistor;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.file.cdl.parser.AspiceCellAdapter;
import com.avlsi.file.cdl.parser.CDLFactoryInterface;
import com.avlsi.file.cdl.parser.CDLSimpleInterface;
import com.avlsi.file.cdl.parser.Inline;
import com.avlsi.file.cdl.parser.Template;
import com.avlsi.file.cdl.parser.ReadCDLIntoFactory;
import com.avlsi.file.cdl.parser.RemoveLVSNodesCDLFactory;
import com.avlsi.file.cdl.util.rename.CDLRenameFactory;
import com.avlsi.file.cdl.util.rename.CadenceReverseNameInterface;
import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.TrivialCDLNameInterfaceFactory;
import com.avlsi.file.cdl.util.rename.CDLRenameException;
import com.avlsi.file.ext.ExtCell;
import com.avlsi.file.ext.parse.ExtParser;
import com.avlsi.io.FileSearchPath;
import com.avlsi.prs.ProductionRule;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cadencize.CadenceInfo;
import com.avlsi.tools.ext2aspice.Ext2Aspice;
import com.avlsi.tools.jauto.PartialExtract;
import com.avlsi.util.container.AliasedMap;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.Namespace;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.CollectionUtils;
import com.avlsi.util.container.ObjectUtils;
import com.avlsi.util.functions.UnaryAction;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.exception.AssertionFailure;
import com.avlsi.util.text.StringUtil;


import com.avlsi.util.container.StringContainerIterator;

import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.cmdlineargs.defimpl.PedanticCommandLineArgs;


import com.avlsi.layout.LVSNodes;
import com.avlsi.layout.LVSNodesForExtract;


/**
 * Hierarchical layout versus schematic comparison.  Compares CAST
 * specification to Aspice device information obtained via .ext or .cdl
 * files.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public final class LVS {
    /** Print lots of debugging information.  debugPrintln() uses this
     * flag. **/
    private static final boolean VERBOSE = false;

    

    /** If true, allow subcells to connect to nets that do not have a label
     * in the layout, only an anonymous name when extracted.  This is needed
     * to handle abutting subcells, where there is no metal to label in the
     * parent cell. **/
    private static final boolean anonSubcellConnAllowed = true;

    /** If true, enable handling for unconnected Vdd and GND nets of
     * subcells.  Ie: <pre>
     * .subckt FOO Vdd! GND! 0 1 2 3 
     * ...
     * .ends FOO
     *
     * Xfoo Vdd! GND! Vdd! Vdd! GND! GND! FOO
     * </pre> **/
    private static final boolean unconnectedGlobalsAllowed = true;

    /** Map from name of pairs of cast and layout cells that have been
     * done to array of LVSProblems therein **/
    private final Map doneMap;

    /** Map from layout type (String) to Map from port name (HierName) to the
     * global it is connected to (HierName).  This is for Vdd! and GND!. **/
    private final Map globalMap;

    /** Map from layout type (String) to Set of globals (HierNames) that
     * at least one port must be connected to. **/
    private final Map neededGlobalsMap;

    /** The AspiceFile of layout that is being checked. **/
    private final AspiceFile aspiceCell;

    /** The cast cell definition against which the layout is being
     * checked.  **/
    private final CellInterface cell;

    /**
     * Cadencizer, keep this around to persist the map of cadencized cells.
     **/
    private final Cadencize cadencizer;

    /**
     * Class constructor.
     **/
    private LVS(final Map doneMap, final Map globalMap,
            final Map neededGlobalsMap,
            final Cadencize cadencizer,
            final CellInterface cell, final AspiceFile aspiceCell) {
        this.doneMap = doneMap;
        this.globalMap = globalMap;
        this.neededGlobalsMap = neededGlobalsMap;
        this.cadencizer = cadencizer;
        this.cell = cell;
        this.aspiceCell = aspiceCell;

        Debug.assertTrue(doneMap != null);
        Debug.assertTrue(unconnectedGlobalsAllowed == (globalMap != null));
        Debug.assertTrue(unconnectedGlobalsAllowed ==
                     (neededGlobalsMap != null));
        Debug.assertTrue(cadencizer != null);
        Debug.assertTrue(aspiceCell != null);
        Debug.assertTrue(cell != null);
    }

    /**
     * Class constructor.
     **/
    public LVS(final CellInterface cell, final AspiceFile aspiceCell) {
        this( cell, aspiceCell, new Cadencize(true) );
    }

    /**
     * Class constructor.
     **/
    public LVS( final CellInterface cell,
                final AspiceFile aspiceCell,
                final Cadencize cadencizer ) {
        this( new HashMap(),
              unconnectedGlobalsAllowed ? new HashMap() : null,
              unconnectedGlobalsAllowed ? new HashMap() : null,
              cadencizer,
              cell,
              aspiceCell);
    }

    /**
     * Run the lvs pass, returning a pair of a map from pairs of CAST cell
     * names and specification cell names to an array of
     * <code>LVSProblem</code>s, and a <code>Boolean</code>.
     * <code>Boolean.TRUE</code> if this cell is bypassed due to
     * prs_netlist_mismatch_ok, and <code>Boolean.FALSE</code> otherwise.
     **/
    public Pair/*<Map<Pair<String,String>,LVSProblem[]>, Boolean>*/
        lvs(final boolean checkSubcells, final boolean checkWires,
            final boolean checkUnwired, final boolean checkPRS,
            double minStaticizerRatio, double maxStaticizerRatio,
            final boolean inlineLayout) {
        final List problemList = new ArrayList();
      
        //
        // types
        //

        if (VERBOSE) {
            debugPrintln("Cast type: " + cell.getFullyQualifiedType());
            debugPrintln("Aspice type: " + aspiceCell.getName());
        }

        // note that we compare the name using the equivtype,
        // but still use the original cell.
        final String castType = cell.getFullyQualifiedType();
        final String aspiceBaseType = aspiceCell.getName();

        // Checking only the name is a bit dangerous.  We should also
        // check that we actually have the same cell.
        final Pair keyPair =
            new Pair(castType, aspiceCell.getName());
        if (doneMap.get(keyPair) != null)
            return new Pair(doneMap, Boolean.FALSE);

        // Check the types. Stop here if they don't agree, because we
        // will just get a bunch of meaningless errors if we
        // checked a BUF_1of2 vs. a BB_1of4 or something.
        if ( ! castType.equals(aspiceBaseType) ) {
            problemList.add(new LVSProblem( "Types don't match: " +
                                            castType + " vs. " +
                                            aspiceBaseType ) );
            final LVSProblem[] problems =
                (LVSProblem[]) problemList.toArray(new LVSProblem[0]);
            doneMap.put(keyPair, problems);
            return new Pair(doneMap, Boolean.FALSE);
        }

        // Do not LVS cells that are known to not match
        if (((Boolean) DirectiveUtils.getTopLevelDirective(cell, DirectiveConstants.PRS_NETLIST_MISMATCH_OK)).booleanValue()) {
            problemList.add(new LVSProblem( "Type " + castType + " has prs_netlist_mismatch_ok = true, LVS checking bypassed.", LVSProblem.LVS_WARNING));
            final LVSProblem[] problems =
                (LVSProblem[]) problemList.toArray(new LVSProblem[0]);
            doneMap.put(keyPair, problems);
            return new Pair(doneMap, Boolean.TRUE);
        }

        //
        // subcells, make sure the subcell names and types are the same
        //
        final Set aspiceSubcells = getAspiceSubcells();
        final Set castSubcells = getCastSubcells();
        final boolean hierarchyOK =
            checkHierarchy(aspiceSubcells, castSubcells, problemList);
        // Stop the check if the hierarchies don't agree.
        // We will run into NullPointerExceptions in checkNamespace()
        // if the subcell names don't match.
        if (!hierarchyOK) {
            final LVSProblem[] problems =
                (LVSProblem[]) problemList.toArray(new LVSProblem[0]);
            doneMap.put(keyPair, problems);
            return new Pair(doneMap, Boolean.FALSE);
        }

        //
        // check namespace, then PRS
        //

        if (checkWires) {
          final AliasedSet commonNamespace =
              checkNamespace(problemList,checkUnwired);
          if (checkPRS) {
            new LvsOperators(cell,
                             aspiceCell,
                             commonNamespace,
                             cadencizer.convert(cell)
                                       .getLocalExclusiveNodeSets()
                                       .canonicalizeNames(commonNamespace),
                             problemList,minStaticizerRatio,maxStaticizerRatio,
                             HierName.makeHierName("Vdd"),
                             HierName.makeHierName("GND")).check();
          }
        }

        //
        // recursively check subcells
        //

        if (checkSubcells) {
            // perhaps we should structure it as
            //   1: first do complete hierarchy checking, but no prs
            //   2: then recheck (needlessly) hierarchy, along with prs
            // this would allow us to report hierarchy errors quickly
            // actually, it seems better to do this in main()
            for (final Iterator iSubcellName = castSubcells.iterator();
                    iSubcellName.hasNext(); ) {
                final String useName = (String) iSubcellName.next();

                if (aspiceSubcells.contains(useName)) {
                    final HierName subCellName;
                    try {
                        subCellName = HierName.makeHierName(useName, '.');
                    } catch (InvalidHierNameException e) {
                        throw new AssertionFailure(e);
                    }
                    final CellInterface subCell =
                        cell.getSubcell(subCellName);
                    final AspiceFile subAspice =
                        aspiceCell.getSubcellDefForName(useName);
                    final LVS subLVS = new LVS(doneMap, globalMap,
                            neededGlobalsMap,
                            cadencizer,
                            inlineLayout ? subCell.inlineLayoutSubcells() :
                                           subCell,
                            subAspice);

                    subLVS.lvs(true, checkWires, checkUnwired, checkPRS, minStaticizerRatio, maxStaticizerRatio, inlineLayout);
                }
            }
        }

        // Do you want a way to find out which nodes were in the 
        // port list in both cast and layout?  This can be done.

        Object o;
        final LVSProblem[] problems =
            (LVSProblem[]) problemList.toArray(new LVSProblem[0]);
        o = doneMap.put(keyPair, problems);
        Debug.assertTrue(o == null);
        return new Pair(doneMap, Boolean.FALSE);
    }

    /**
     * Returns a <code>Set</code> of subcells that are in the
     * cast that should also appear in the layout.  Cells
     * without production rules should not appear in the
     * hierarchy (ie channels and wiring cells are ignored)
     **/
    private Set getCastSubcells() {
        final Set castSubcells = new HashSet();

        if (VERBOSE)
            debugPrintln("Cast subcells:");
        // there are other ways of accessing the subcells.  Perhaps
        // a more convenient way could be written depending on what
        // you want to do.
        for (final Iterator iSubPair = cell.getSubcellPairs();
                iSubPair.hasNext(); ) {
            final Pair p = (Pair) iSubPair.next();
            final HierName useName = (HierName) p.getFirst();
            final CellInterface subcell = (CellInterface) p.getSecond();

            if (VERBOSE)
                debugPrintln(useName.getAspiceString() + ": " +
                        subcell.getFullyQualifiedType());

            if (!CellUtils.isWiring(subcell))
                castSubcells.add(useName.getAspiceString());
        }

        return castSubcells;
    }

    /**
     * Returns a <code>Set</code> of subcells that are in the layout
     * that should also appear in the cast. Cells without devices
     * should not appear in the layout hierarchy (ie wiring cells are
     * ignored). NOTE, this now checks cells with explict resistors
     * and capacitors.  These used to be ignored when CDL was
     * extracted, but now that it is a pure spec without parasitics
     * this should be checked.  -- AML
     **/
    private Set getAspiceSubcells() {
        final Set aspiceSubcells = new HashSet();

        if (VERBOSE)
            debugPrintln("Aspice subcells:");
        // this isn't the same as for cast due to historical accident
        // it can be changed
        for (final Iterator iSubcellName = aspiceCell.getSubcellNames();
                iSubcellName.hasNext(); ) {
            final String useName = (String) iSubcellName.next();
            final String type = aspiceCell.getSubcellTypeForName(useName);

            if (VERBOSE)
                debugPrintln(useName + ": " + type);

            if (aspiceCell.getSubcellDefForName(useName).
                hasDevices(true,true,true,true))
                aspiceSubcells.add(useName);
        }

        return aspiceSubcells;
    }

    /**
     * Check that the subcells in the layout agree with those in the
     * cast.  Returns true if they agree, false if there is a problem.
     **/
    private boolean checkHierarchy(final Set aspiceSubcells,
            final Set castSubcells,
            final List problemList) {
        // compare the subcells.  The names and types must match.
        // here, we check the names.  Types will be checked by
        // the recursive lvs call below

        boolean hierarchyOK = true;

        // make sure all cast subcells are in the aspice
        final Set castExtras = new HashSet(castSubcells);
        castExtras.removeAll(aspiceSubcells);
        for (final Iterator iCastExtra = castExtras.iterator();
                iCastExtra.hasNext(); ) {
            final String castExtra = (String) iCastExtra.next();
            problemList.add(new LVSProblem("subcell " + castExtra +
                        " in cast, but not in layout."));
            hierarchyOK = false;
        }

        // make sure all aspice subcells are in the cast
        final Set aspiceExtras = new HashSet(aspiceSubcells);
        // aspiceSubcells and castSubcells are both sets of names.
        aspiceExtras.removeAll(castSubcells);

        for (final Iterator iAspiceExtra = aspiceExtras.iterator();
                iAspiceExtra.hasNext(); ) {
            final String aspiceExtra = (String) iAspiceExtra.next();
            problemList.add(new LVSProblem("subcell " + aspiceExtra +
                        " in layout, but not in cast."));
            hierarchyOK = false;
        }

        return hierarchyOK;
    }

    /**
     * Check that the namespaces are compatible.  This means several things:
     * <ol>
     *     <li> There are no shorts in the layout.</li>
     *     <li> There are no unconnected nodes in the layout.</li>
     *     <li> Every name that appears as part of a production rule
     *          is found in the layout.  </li>
     *     <li> Every port node of the cast subcells appears in the layout,
     *          and is properly connected to this cell.
     * </ol>
     * Returns a common namespace consisting of all names from both
     * cast and layout that will be used as part of the production
     * rule checking.
     **/
    private AliasedSet checkNamespace(final List problemList,
                                      boolean checkUnwired) {
        final CadenceInfo ci = cadencizer.convert(cell);

        if (VERBOSE) {
            debugPrintln("Cast namespace:");
            debugPrintln(((CellImpl) cell).getNamespaceString());

            debugPrintln("Cadencized cast local namespace:");
            debugPrintln(ci.getLocalNodes().toString());

            debugPrintln("Cadencized cast port namespace:");
            debugPrintln(ci.getPortNodes().toString());

            debugPrintln("Aspice namespace:");
            debugPrintln(aspiceCell.getNamespaceString());
        }

        // A note about why it is ok to use cadencized local nodes:
        // One might wonder why we use Cadencize.getLocalNodes() in
        // a program that must be proven correct, as
        // Cadencize.getLocalNodes() is a non-trivial algorithm.
        // The answer:  we use the same procedure in jflat, whose
        // output is then fed to dsim.  As long as the digital
        // simulations work, we are happy.  Thus, dsim is used to
        // debug the Cadencize code.
        // --jmr

        // start a new namespace, it will contain all names in
        // prs, and those in the layout file.
        // (Excl names should be the same as those in the prs.)
        // start it off from the layout namespace.
        final AliasedSet commonNS =
            new AliasedSet(HierName.getComparator());
        // the set of names on production rules
        final Set prNodeSet = new HashSet();

        // add the names in the prs, so we can ensure that they appear
        // in the layout.
        for (final Iterator iPR =
                cell.getProductionRuleSet().getProductionRules(); 
                iPR.hasNext(); ) {
            final ProductionRule pr = (ProductionRule) iPR.next();

            pr.foreachHierName(new UnaryAction() {
                    public void execute(final Object o) {
                        commonNS.add((HierName) o);
                        prNodeSet.add((HierName) o);
                    }
                });
        }

        // Make a copy of the aspiceCell's 
        // This is a waste of memory, but coding time is more important now
        // We make a copy because we wish to modify the namespace if
        // we have anonymous ports that were connected to a global by 
        // a parent.
        final AliasedSet layoutNS =
            makeLayoutNamespace(aspiceCell, problemList);

        if (VERBOSE) {
            debugPrintln("layout namespace:");
            debugPrintln(layoutNS.toString());

            debugPrintln("prs namespace (unconnected):");
            debugPrintln(commonNS.toString());
        }

        // check subcell port connections
        // connects port.cell and port/cell in layoutNS if port/cell
        // was connected to an anonymous node.
        checkSubcellPortConnections(ci, checkUnwired, layoutNS, problemList);

        // check for shorts, add layout connections to commonNS
        checkShorts(ci, layoutNS, commonNS, problemList);
        
        if (VERBOSE) {
            debugPrintln("prs + aspice namespace (aspice connections):");
            debugPrintln(commonNS.toString());
        }

        // check for unconnects, add cast connections to commonNS
        checkUnconnects(ci, layoutNS, prNodeSet, commonNS, problemList);

        Namespace.addNamespace(commonNS, ci.getLocalNodes());

        if (VERBOSE) {
            debugPrintln("common = cast + aspice namespace:");
            debugPrintln(commonNS.toString());
        }

        return commonNS;
    }

    /**
     * Finds shorts in layout, adding layout connections to
     * <code>commonNS</code>, and recording problems in
     * <code>problemList</code>.  Called by {@link #checkNamespace}.
     **/
    private void checkShorts(
            final CadenceInfo ci,
            final AliasedSet layoutNS,
            final AliasedSet commonNS,
            final List problemList) {
        // Ensure that the names in the layout set that are also in the
        // cast must have the same canonical name in the cast.  If they
        // have different canonical names in the cast, we have found
        // a short in the layout.
        // We do not ensure that names in the layout are in the cast.
        for (final Iterator iAspiceCanon = layoutNS.getCanonicalKeys();
                iAspiceCanon.hasNext(); ) {
            final HierName aspiceCanon = (HierName) iAspiceCanon.next();
            checkShortsInner(ci, aspiceCanon, commonNS, layoutNS, problemList);
        }
    }

    private void checkShortsInner(CadenceInfo ci, HierName aspiceCanon,
            AliasedSet commonNS, AliasedSet layoutNS, List problemList) {
        // The canonical name in the cast for the group of nodes.
        // All nodes found in this layout group must have this
        // same cast canonical name.
        HierName castCanonGroup = null;

        for (final Iterator iAspiceConn =
                layoutNS.getAliases(aspiceCanon);
                iAspiceConn.hasNext(); ) {
            final HierName aspiceConn = (HierName) iAspiceConn.next();

            // add the connections from the layout
            commonNS.makeEquivalent(aspiceCanon, aspiceConn);

            // the canonical name in the cast for this node
            final HierName castCanon = (HierName)
                ci.getLocalNodes().getCanonicalKey(aspiceConn);

            castCanonGroup = checkShortsCanon(castCanonGroup, castCanon,
                    aspiceCanon, aspiceConn, problemList);


            // if the node is a subcell node, make sure it is for a 
            // valid port.
            checkShortsSubcellName(aspiceConn, aspiceCanon, commonNS, problemList);
        }
    }

    private HierName checkShortsCanon(HierName castCanonGroup,
            HierName castCanon, HierName aspiceCanon,
            HierName aspiceConn, List problemList) {
        if (castCanon != null) {
            if (castCanonGroup == null)
                castCanonGroup = castCanon;
            else if (castCanonGroup != castCanon) {
                final LVSProblem p =
                    new LVSProblem(aspiceCanon.getAsString('.') +
                            " and " + aspiceConn.getAsString('.') +
                            " connected in layout, but not in cast: " +
                            aspiceCanon.getAsString('.') + " -> " +
                            castCanonGroup.getAsString('.') +
                            " but " + aspiceConn.getAsString('.') +
                            " -> " + castCanon.getAsString('.') +
                            ".");
                problemList.add(p);
            }
        }
        return castCanonGroup;
    }

    private void checkShortsSubcellName(HierName aspiceConn,
            HierName aspiceCanon, AliasedSet commonNS, List problemList) {
        final String path = aspiceConn.getAsString('.');
        final int slashIdx = path.indexOf('/');
        if (slashIdx == -1) return;
        final HierName subcellName;
        final HierName portName;
        final HierName dottedName;
        try {
            subcellName = HierName.makeHierName(
                    path.substring(0, slashIdx), '.');
            portName = HierName.makeHierName(
                    path.substring(slashIdx + 1), '.');
            dottedName = HierName.makeHierName(
                    StringUtil.replaceChars(path, '/', '.'),
                    '.');
        } catch (InvalidHierNameException e) {
            throw new AssertionFailure(e);
        }

        // Fix for Bug 246: connect a/b with a.b in the commonNS
        commonNS.makeEquivalent(dottedName, aspiceConn);

        // get the cell with that name
        final CellInterface subcell = cell.getSubcell(subcellName);

        // get the port cells
        final AliasedMap portNodes =
            cadencizer.convert(subcell).getPortNodes();

        // and check that it is a valid port
        if (portNodes.getCanonicalKey(portName) == null) {
            // if it is not a valid port, and it is an
            // anonymous node, it may be part of a split
            // Vdd! or GND! net.

            // find the canonical layout name for the
            // port.  If it's a global, record that the
            // anonymous node was connected to the global,
            // and verify that the same global appears
            // in the subcell.  If it's not a global,
            // it is a bad port error
            if (unconnectedGlobalsAllowed &&
                    aspiceCanon.isGlobal() &&
                    (portName.isGenerated() ||
                     portName.isGlobal())) {

                // the global must also be a port of the cell
                if (portNodes.getCanonicalKey(aspiceCanon) == null) {
                    if (portName.isGlobal()) {
                        // if the port is a global, then
                        // global!ext will be allowed as
                        // long as global! is a port.
                        final String s =
                            aspiceCanon.getSuffixString();
                        final int bangIdx = s.lastIndexOf('!');
                        if (bangIdx != s.length() - 1) {
                            final HierName n =
                                HierName.makeHierName(
                                        s.substring(0,
                                            bangIdx + 1));

                            if (portNodes.getCanonicalKey(n)
                                    == null) {
                                problemList.add(
                                        new LVSProblem(
                                            aspiceConn.getAsString('.') +
                                            " connected to " +
                                            aspiceCanon
                                            .getAsString('.') +
                                            " but " +
                                            n.getAsString('.') +
                                            " is not a port of " +
                                            subcell.getFullyQualifiedType()));
                            }
                        }
                    } else {
                        problemList.add(new LVSProblem(
                                    aspiceConn.getAsString('.') +
                                    " connected to " +
                                    aspiceCanon
                                    .getAsString('.') +
                                    " but it is not a port of " +
                                    subcell.getFullyQualifiedType()));
                    }
                }

                final String subcellLayoutType =
                    aspiceCell.getSubcellTypeForName(
                            subcellName.getAsString('.'));

                Map m = (Map) globalMap.get(subcellLayoutType);
                final boolean firstTime = m == null;
                if (m == null) {
                    m = new HashMap();
                    globalMap.put(subcellLayoutType, m);
                }

                final HierName n = (HierName) m.get(portName);
                if (n == null) {
                    if (firstTime) {
                        System.err.println("in " +
                                subcellLayoutType +
                                " connecting port " + portName +
                                " and " + aspiceCanon);
                        m.put(portName, aspiceCanon);
                    } else {
                        m.put(portName, aspiceCanon);
                        // problemList.add(new LVSProblem(""));
                    }
                } else if (!n.equals(aspiceCanon)) {
                    problemList.add(new LVSProblem(
                                aspiceConn.getAsString('.') +
                                " connected to both " +
                                n.getAsString('.') + " and " +
                                aspiceConn.getAsString('.')));
                }
            } else {
                problemList.add(new LVSProblem("Non-port node " +
                            subcellName.getAsString('.') + '.' +
                            portName.getAsString('.') +
                            " in layout, connected to " +
                            aspiceCanon.getAsString('.') + "."));
            }
        }
    }

    /**
     * Finds missing connections in layout, adding cast connections to
     * <code>commonNS</code>, and recording problems in
     * <code>problemList</code>.  Called by {@link #checkNamespace}.
     **/
    private void checkUnconnects(
            final CadenceInfo ci,
            final AliasedSet layoutNS,
            final Set prNodeSet,
            final AliasedSet commonNS,
            final List problemList) {
        // Ensure that all sets of names in the cadencized local nodes
        // that are either the guard or target of a production rule
        // have a representative in the aspice.  Further ensure that
        // the names in the set that are also in the layout must have
        // the same canonical name in the layout.
        for (final Iterator iCastCanon =
                ci.getLocalNodes().getCanonicalKeys();
                iCastCanon.hasNext(); ) {
            final HierName castCanon = (HierName) iCastCanon.next();
            // the canonical name in the cast for the group of nodes
            HierName aspiceCanonGroup = null;
            // the canonical name in the prs
            HierName prCanonGroup = null;
            // if it is in the layout
            boolean inPRS = false;

            for (final Iterator iCastConn =
                    ci.getLocalNodes().getAliases(castCanon);
                    iCastConn.hasNext(); ) {
                final HierName castConn = (HierName) iCastConn.next();

                // check to see if it is in on a production rule
                if (!inPRS && prNodeSet.contains(castConn))
                    inPRS = true;

                // Connect names that are in the prs
                final HierName prCanon =
                    (HierName) commonNS.getCanonicalKey(castConn);
                if (prCanon != null) {
                    if (prCanonGroup == null)
                        prCanonGroup = prCanon;
                    else
                        commonNS.makeEquivalent(prCanonGroup, prCanon);
                }

                // the canonical name in the layout for this node
                final HierName aspiceCanon =
                    (HierName) layoutNS.getCanonicalKey(castConn);

                if (aspiceCanon != null) {
                    if (aspiceCanonGroup == null)
                        aspiceCanonGroup = aspiceCanon;
                    else if (aspiceCanonGroup != aspiceCanon) {
                        final LVSProblem p =
                            new LVSProblem(castCanon.getAsString('.') +
                                    " and " + castConn.getAsString('.') +
                                    " connected in cast, but not in layout: " +
                                    castCanon.getAsString('.') + " -> " +
                                    aspiceCanonGroup.getAsString('.') +
                                    " but " + castConn.getAsString('.') +
                                    " -> " + aspiceCanon.getAsString('.') +
                                    ".");
                        problemList.add(p);
                    }
                }
            }

            // The node wasn't found in the layout
            if (aspiceCanonGroup == null) {
                // we should have found a representative in the layout
                // if it occurs in the production rules
                if (inPRS) {
                    final LVSProblem p =
                        new LVSProblem(castCanon.getAsString('.') +
                                " not found in layout.");
                    problemList.add(p);
                }
            }
        }
    }

    /**
     * Finds bad subcell connections in layout, and recording problems in
     * <code>problemList</code>.  Called by {@link #checkNamespace}.
     **/
    private void checkSubcellPortConnections(
            final CadenceInfo ci,
            final boolean checkUnwired,
            final AliasedSet layoutNS,
            final List problemList) {
        // for each cadencized cast subcell, check that the ports are
        // connected to something.  We depend on checkShorts and
        // checkUnconnects to make sure it is the right connection.
        for (final Iterator iSubcellPair = cell.getSubcellPairs();
                iSubcellPair.hasNext(); ) {
            final Pair p = (Pair) iSubcellPair.next();
            final HierName subcellName = (HierName) p.getFirst();
            final CellInterface subcell = (CellInterface) p.getSecond();

            if (!subcell.hasRealProductionRule())
                continue;

            final CadenceInfo subci = cadencizer.convert(subcell);
            final AliasedMap ports = subci.getPortNodes();

            // find possible phantom ports
            final Map phantomPorts =
                DirectiveUtils.canonizeKey(
                        new AliasedSet(ports),
                        DirectiveUtils.getTopLevelDirective(subcell,
                            DirectiveConstants.PHANTOM_PORT_SIGNOFF,
                            DirectiveConstants.NODE_TYPE));

            // for each port, make sure that it is connected in the layout,
            /*
            System.err.println("ports for subcell " + 
                    subcellName + ": " + subcell.getFullyQualifiedType() + 
                    " child of " + cell.getFullyQualifiedType());
            System.err.println(ports);
            */
            for (final Iterator iPortCanon = ports.getCanonicalKeys();
                    iPortCanon.hasNext(); ) {
                final HierName portCanon = (HierName) iPortCanon.next();
                final boolean used =
                    ((Boolean) ports.getValue(portCanon)).booleanValue();
                // skip ports that are not connected to subcells or
                // not used on a production rule.  Ie ports that are just
                // connected to other ports.
                if (!used)
                    continue;
                HierName layoutCanonGroup = null;

                for (final Iterator iPortConn = ports.getAliases(portCanon);
                        iPortConn.hasNext(); ) {
                    final HierName portConn = (HierName) iPortConn.next();
                    /*
                    System.err.println("checking alias " + portConn +
                            " of " + portCanon + " for subcell " + 
                            subcellName + ": " +
                            subcell.getFullyQualifiedType() + 
                            " child of " + cell.getFullyQualifiedType());
                    */
                    final HierName layoutName;
                    try {
                        layoutName = HierName.makeHierName(
                                subcellName.getAsString('.') + '/' +
                                portConn.getAsString('.'), '.');
                    } catch (InvalidHierNameException e) {
                        throw (AssertionFailure)
                            new AssertionFailure("Invalid HierName! How?")
                                .initCause(e);
                    }
                    final HierName layoutCanon =
                        (HierName) layoutNS.getCanonicalKey(layoutName);

                    if (layoutCanon != null) {
                        if (layoutCanonGroup == null) {
                            layoutCanonGroup = layoutCanon;
                            // searchName is layoutName, but with '.'
                            // instead of '/'.
                            final HierName searchName =
                                HierName.append(subcellName, portConn);
                            final HierName castCanon = 
                                (HierName) ci.getLocalNodes()
                                             .getCanonicalKey(searchName);
                            Debug.assertTrue(castCanon != null);
                            boolean allAnon = true;
                            boolean found = false;

                            // check the connection
                            // one of the layout matches for a/b.c
                            // must be something
                            // that was connected to a.b.c in the cast
                            for (final Iterator iLayoutConn =
                                    layoutNS.getAliases(layoutCanon);
                                    iLayoutConn.hasNext(); ) {
                                final HierName layoutConn =
                                    (HierName) iLayoutConn.next();

                                if (!layoutConn.isGenerated() &&
                                    layoutConn.getNumComponents() == 1 &&
                                    layoutConn.getAsString('.').indexOf('/')
                                        == -1) {
                                    allAnon = false;
                                }

                                if (castCanon ==
                                        (HierName) ci.getLocalNodes()
                                            .getCanonicalKey(layoutConn)) {
                                    found = true;
                                }
                            }

                            if (!found) {
                                final LVSProblem pr = new LVSProblem(
                                            "Subcell node " +
                                            searchName.getAsString('.') +
                                            " improperly connected.");
                                if (anonSubcellConnAllowed) {
                                    if (allAnon) {
                                        // PR 234: detect bad subcell
                                        // connections
                                        final HierName portName =
                                            HierName.append(subcellName,
                                                    portCanon);

                                        // if cell.port exists in layout
                                        // it is an error
                                        if (layoutNS.getCanonicalKey(
                                                portName) != null) {
                                            problemList.add(new LVSProblem(
                                                portName.getAsString('.') +
                                                " is in layout, but it is" +
                                                " not connected to " +
                                                layoutName
                                                    .getAsString('.')));
                                        } else {
                                            // otherwise, we record the fact
                                            // that cell/port is connected
                                            // to cell.port.  This cannot
                                            // introduce a short, because 
                                            // they are supposed to be 
                                            // connected.  It may introduce
                                            // an unconnect if there is
                                            // another label that is
                                            // connected in the cast
                                            // that is not connected to
                                            // it in the layout.  It may,
                                            // however, alert us to the
                                            // existance of a short.
                                            layoutNS.makeEquivalent(portName,
                                                    layoutName);
                                        }
                                    } else {
                                        problemList.add(pr);
                                    }
                                } else {
                                    problemList.add(pr);
                                }
                            }
                        } else {
                            // found two -- not good
                            problemList.add(
                                    new LVSProblem("Found more than 1 " +
                                        "connection to same port of cell " +
                                        subcellName.getAsString('.') + ": " +
                                        layoutCanonGroup.getAsString('.') +
                                        " and " +
                                        layoutCanon.getAsString('.')));
                        }
                    }
                }

                if (checkUnwired) {
                    final boolean isPhantom =
                        ObjectUtils.equals(phantomPorts.get(portCanon),
                                           Boolean.TRUE);

                    if ((layoutCanonGroup == null)) {
                        // port not found -- not good
                        // it's ok to not find a port if the node is a global
                        // and we have found a port node that has that
                        // same global passed in.
                        if (unconnectedGlobalsAllowed && portCanon.isGlobal()) {
                            final String subcellLayoutType =
                                aspiceCell.getSubcellTypeForName(
                                        subcellName.getAsString('.'));
                            Set s =
                                (Set) neededGlobalsMap.get(subcellLayoutType);
                            if (s == null) {
                                s = new HashSet();
                                neededGlobalsMap.put(subcellLayoutType, s);
                            }
                            s.add(portCanon);
                        } else {
                            problemList.add(new LVSProblem(
                                        "No connection found " +
                                        "for " + subcellName.getAsString('.') +
                                        '/' + portCanon.getAsString('.') +
                                        ".   Subcell connection missing." +
                                        (isPhantom ?
                                            "  (phantom_port_signoff=true)"
                                          : ""),
                                        isPhantom ? LVSProblem.LVS_WARNING :
                                                    LVSProblem.LVS_ERROR));
                        }
                    } else if (isPhantom) {
                        problemList.add(new LVSProblem(
                                    "Connection found for phantom port " +
                                    subcellName.getAsString('.') +
                                    '/' + portCanon.getAsString('.') +
                                    ".   Subcell connection extrenuous."));
                    }
                }
            }
        }
    }

    /**
     * Copies the namespace from the aspice cell to a fresh AliasedSet.
     * Connects globals in the current cell of the form global!ext to
     * global!, and connects any anonymous nodes
     **/
    private AliasedSet makeLayoutNamespace(
            final AspiceFile aspiceCell,
            final List problemList) {
        final AliasedSet ns = new AliasedSet(HierName.getComparator());

        // If we had anonymous ports for unconnected Vdd! or GND!,
        // connect them to the global signal.
        if (unconnectedGlobalsAllowed) {
            final Set foundGlobals = new HashSet();
            // connect up the anonymous ports to the globals
            final Map m = (Map) globalMap.get(aspiceCell.getName());
            if (m != null) {
                for (final Iterator iEntry = m.entrySet().iterator();
                        iEntry.hasNext(); ) {
                    final Entry e = (Entry) iEntry.next();
                    final HierName n1 = (HierName) e.getKey();
                    final HierName n2 = (HierName) e.getValue();

                    foundGlobals.add(n2);
                    ns.makeEquivalent(n1, n2);
                }
            }

            // make sure the needed globals are there
            final Set neededGlobals =
                (Set) neededGlobalsMap.get(aspiceCell.getName());
            if (neededGlobals != null) {
                for (final Iterator iNeeded = neededGlobals.iterator();
                        iNeeded.hasNext(); ) {
                    final HierName needed = (HierName) iNeeded.next();
                    if (!foundGlobals.contains(needed)) {
                        problemList.add(new LVSProblem(
                                    "No connection found " +
                                    "for port global " +
                                    needed.getAsString('.') +
                                    ".   Subcell connection missing."));
                    }
                }
            }
        }

        // transfer the namespace
        for (final Iterator iCanonNode = aspiceCell.getCanonicalNames();
             iCanonNode.hasNext(); ) {

            final HierName canonNode = (HierName) iCanonNode.next();

            ns.add(canonNode);

            for (final Iterator iConnNode =
                    aspiceCell.getEquivalentNames(canonNode);
                 iConnNode.hasNext(); ) {

                final HierName connNode = (HierName) iConnNode.next();

                ns.makeEquivalent(canonNode, connNode);

                // Connect global!foo to global!
                if (unconnectedGlobalsAllowed &&
                    connNode.isGlobal() &&
                    connNode.getNumComponents() == 1) {
                    final String s = connNode.getSuffixString();

                    // if not a subcell port
                    if (s.indexOf('/') == -1) {
                        final int bangIdx = s.indexOf('!');

                        if (bangIdx != s.length() - 1) {
                            final String t = s.substring(0, bangIdx + 1);

                            ns.makeEquivalent(connNode,
                                    HierName.makeHierName(t));
                        }
                    }
                }
            }
        }

        return ns;
    }

    private static void debugPrintln(final String s) {
        if (VERBOSE) {
            System.err.println(s);
        }
    }

    private static void usage( String m ) {
        System.err.println("Usage: jlvs\n" +
                           "  cast_cell \n" +
                           "    --specification-type=ext ext_file ext_cell\n" +
                           "  | --specification-type=cdl cdl_file cdl_cell\n" +
                           "  [--translate-cdl=none | cadence (default)]\n" +
                           "  [--gates=<cell:...>]\n" +
                           "  [--no-subcells] [--no-wires]\n" +
                           "  [--no-unwired] [--no-prs]\n" + 
                           "  [--min-staticizer-ratio=value]\n" + 
                           "  [--max-staticizer-ratio=value]\n" +
                           "  [--cast-path=castpath]\n" +
                           "  [--ext-path=extractpath]\n" + 
                           "  [--ignore-inline-layout]\n" +
			   "  [--cast-version=1 | --cast-version=2]\n" );
        if ( m != null && m.length() > 0)
            System.err.print( m );
        System.exit(1);
    }

    /**
     * A class that passes on calls to the underlying CDLNameInterface but
     * intercepts calls to renameDevice, and simply returns the old device name.
     * See bug 3590.  This is only here to avoid touching CadenceNameInterface
     * and CadenceReverseNameInterface before tapeout.
     **/
    private static class NoRenameDevice implements CDLNameInterface {
        final CDLNameInterface ni;
        public NoRenameDevice(final CDLNameInterface ni) {
            this.ni = ni;
        }

        public String renameCell(final String oldCellName)
            throws CDLRenameException {
            return ni.renameCell(oldCellName);
        }

        public String renameNode(final String oldNodeName)
            throws CDLRenameException {
            return ni.renameNode(oldNodeName);
        }

        public String renameDevice(final String oldDeviceName)
            throws CDLRenameException {
            return oldDeviceName;
        }

        public String renameSubCellInstance(final String oldInstanceName)
            throws CDLRenameException {
            return ni.renameSubCellInstance(oldInstanceName);
        }

        public String renameTransistorModel(final String oldTransistorModel)
            throws CDLRenameException {
            return ni.renameTransistorModel(oldTransistorModel);
        }
    }

    public static void main(String[] args) throws Exception {
        

	final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
	final CommandLineArgs argsWithConfigs =
	    new CommandLineArgsWithConfigFiles( parsedArgs ); 

	final CommandLineArgs cachedArgs = 
	    new CachingCommandLineArgs( argsWithConfigs );

	final PedanticCommandLineArgs pedanticArgs = 
	    new PedanticCommandLineArgs( cachedArgs );

	final CommandLineArgs theArgs = pedanticArgs;
        if (theArgs.argExists("version")) {
            System.out.println(
                com.avlsi.util.debug.VersionInfo.getVersionString(LVS.class));
        }

	final boolean checkSubcells = !( theArgs.argExists( "no-subcells" ) );
        final boolean checkWires    = !( theArgs.argExists( "no-wires" ) );
        final boolean checkUnwired  = !( theArgs.argExists( "no-unwired" ) );
        final boolean checkPRS      = !( theArgs.argExists( "no-prs" ) );
        final String castVersion    = theArgs.getArgValue("cast-version",null);
	
	final FileSearchPath castPath = new FileSearchPath( theArgs.getArgValue( "cast-path", "." ) );
	
        final double minStaticizerRatio = 
	    Double.parseDouble( theArgs.getArgValue( "min-staticizer-ratio", "2" ) );

	final double maxStaticizerRatio =
	    Double.parseDouble( theArgs.getArgValue( "max-staticizer-ratio", "20" ) );

        final boolean inlineLayout = !theArgs.argExists("ignore-inline-layout");
        
	// check number of arguments
        if (args.length < 4)
          usage(null);

        final String specificationType = theArgs.getArgValue( "specification-type", null );
        final String gateList = theArgs.getArgValue("gates", null);

	final StringContainerIterator strIter = theArgs.nonParsedArgumentsIterator();
	
        pedanticArgs.argTag( "ext-path" );
        pedanticArgs.argTag( "translate-cdl" );
        if ( ! pedanticArgs.pedanticOK( false, true ) ) {
            usage( pedanticArgs.pedanticString() );
        }

	try {
	    
	    final String castCellName = strIter.next();
	   
	    // get the cast body
	    final CastFileParser cfp = new CastFileParser(castPath,castVersion,VERBOSE);
            
            if ( castCellName.lastIndexOf('.') == -1 ) {
                usage( "You must specify a fully qualified cell name.\n");
            }

            final PartialExtract.CellPlusMinus fqcnSpec =
                new PartialExtract.CellPlusMinus(castCellName,
                                                 PartialExtract.Info.INCLUDE);
            if (!fqcnSpec.isEmpty()) {
                System.out.println("Warnings in comparing " + castCellName);
                System.out.println("Nothing done because LVS of partial extracted cell is not supported.");
                System.out.println("LVS NA.");
                System.exit(0);
            }
            
            final CellInterface castCell =
                cfp.getFullyQualifiedCell( castCellName );
	    
	    
	    // get the aspice cell
            AspiceFile aspiceCell;

            final Cadencize cadencizer = new Cadencize( true );
            
	    if ( specificationType != null ) {
		if ( specificationType.equals( "ext" ) ) {
		    final String extFileName = strIter.next();
		    final String extCellName = strIter.next();

		    final FileSearchPath extractPath = new FileSearchPath( theArgs.getArgValue( "ext-path", "." ) );

		    final ExtCell ext = ExtParser.parse(extCellName, extFileName, extractPath, true);
		    
		    aspiceCell = Ext2Aspice.ext2aspice( ext );
		}
		else if ( specificationType.equals( "cdl" ) ) {
		    final String cdlFileName = strIter.next();
		    final String cdlCellName = strIter.next();
                    final String rename =
                        theArgs.getArgValue("translate-cdl", "cadence");

                    // Parse gate names and insert into a set for quick access
                    final Set gateSet = new HashSet();
                    if (gateList != null) {
                        final StringTokenizer tok =
                            new StringTokenizer(gateList, ":");
                        while (tok.hasMoreTokens()) {
                            final String gateName = tok.nextToken();
                            gateSet.add(gateName);
                        }
                    }

                    final Map cellMap = new LinkedHashMap();
                    final CDLFactoryInterface templateFactory = 
                        new Template(cellMap);


                    final CDLFactoryInterface lvsNodesFactory;

                    final LVSNodes lvsNodes = 
                        new LVSNodesForExtract( cfp, cadencizer );
                    lvsNodesFactory = 
                        new RemoveLVSNodesCDLFactory( lvsNodes, templateFactory );
                   
                    final CDLFactoryInterface renameFactory;

                    // Setup translation of CDL names if necessary
                    if (rename.equals("cadence")) {
                        final CDLNameInterface reverseCadenceNameInterface = 
                            new NoRenameDevice(
                                new CadenceReverseNameInterface());
                        
                        final TrivialCDLNameInterfaceFactory niFactory =
                            new TrivialCDLNameInterfaceFactory( reverseCadenceNameInterface );

                        renameFactory = 
                            new CDLRenameFactory( lvsNodesFactory, 
                                                  niFactory );
                    } else if ( rename.equals("none") ) {
                        renameFactory = lvsNodesFactory;
                    }
                    else {
                        renameFactory = lvsNodesFactory;
                        System.err.println("Unknown CDL translation scheme " +
                                           rename + " ignored!");
                    } 

                    final File cdlFile = new File( cdlFileName );
                    final InputStream cdlStream = new FileInputStream( cdlFile );
                    final Reader cdlReader = new InputStreamReader( cdlStream, "UTF-8" );

                    

                    // Parse all cells in the CDL file into templates
                    ReadCDLIntoFactory.readCDL( cdlReader,
                                                renameFactory );

                    final Inline.Retriever retr =
                        new Inline.RetrieveFromCast(cfp) {
                            public Template getTemplate(String subName) {
                                CellInterface cell = loadCell(subName);
                                if (((Boolean) DirectiveUtils.getTopLevelDirective(cell, DirectiveConstants.NETLIST_PRIMITIVE)).booleanValue()) {
                                    return makeTemplate(cell);
                                } else {
                                    return null;
                                }
                            }
                        };

                    final Inline cdlInliner = new Inline(true, retr);

                    // If a gate definition is found, set it up for inlining
                    for (Iterator i = gateSet.iterator(); i.hasNext(); ) {
                        final String gateName = (String) i.next();
                        final Template templ = (Template) cellMap.get(gateName);
                        if (templ != null) {
                            cdlInliner.addTarget(gateName, templ);
                        }
                    }

                    final AspiceCellAdapter aspiceAdapter =
                        new AspiceCellAdapter();

                    // Determine what to execute templates to
                    CDLSimpleInterface cdlParser;
                    if (cdlInliner == null) {
                        cdlParser = aspiceAdapter;
                    } else {
                        cdlInliner.setProxy(aspiceAdapter);
                        cdlParser = cdlInliner;
                    }

                    // Execute all templates
                    for (Iterator i = cellMap.entrySet().iterator();
                         i.hasNext(); ) {
                        final Map.Entry entry = (Map.Entry) i.next();
                        final String subckt = (String) entry.getKey();
                        if (!gateSet.contains(subckt)) {
                            final Template templ = (Template) entry.getValue();
                            templ.execute(Collections.EMPTY_MAP, cdlParser,
                                          subckt);
                        }
                    }

		    aspiceCell = aspiceAdapter.getCell( cdlCellName );
		    
		    if (aspiceCell == null) {
			System.err.println("Cell " + cdlCellName +
					   " not found in file " + cdlFileName);
			System.exit(3);
		    }
		}
		else {
                    // to fool the compiler
                    aspiceCell = null;
		    usage("Invalid --specification-type: " + specificationType +
                          "\n");
		}
	    }
	    else {
                // to fool the compiler
                aspiceCell = null;
		usage("Required argument --specification-type missing\n");
	    }

	    final Pair lvsResult =
		new LVS( inlineLayout ? castCell.inlineLayoutSubcells() :
                         castCell,
                         aspiceCell,
                         cadencizer )
                .lvs( checkSubcells,
                      checkWires,
                      checkUnwired,
                      checkPRS,
                      minStaticizerRatio,
                      maxStaticizerRatio, 
                      inlineLayout );
	    final Map doneMap = (Map) lvsResult.getFirst();
            final boolean skipped =
                ((Boolean) lvsResult.getSecond()).booleanValue();
	    boolean passed = true;
	    for (Iterator i = doneMap.entrySet().iterator(); i.hasNext(); ) {
		final Entry e = (Entry) i.next();
		final Pair p = (Pair) e.getKey();
		final LVSProblem[] errs = (LVSProblem[]) e.getValue();
                int errors = 0;
                int warnings = 0;
		
                for (int ierr = 0; ierr < errs.length; ++ierr) {
                    if (errs[ierr].getType() == LVSProblem.LVS_ERROR) ++errors;
                    else ++warnings;
                }

                final String castType = (String) p.getFirst();
                final String aspiceType = (String) p.getSecond();
		if (errors > 0) {
		    passed = false;
		    
		    System.out.println("Errors in comparing " + castType +
				       " vs. " + aspiceType);
		    for (int ierr = 0; ierr < errs.length; ++ierr)
                        if (errs[ierr].getType() == LVSProblem.LVS_ERROR)
                            System.out.println(errs[ierr].getMessage());
		}
                if (warnings > 0) {
		    System.out.println("Warnings in comparing " + castType +
				       " vs. " + aspiceType);
		    for (int ierr = 0; ierr < errs.length; ++ierr)
                        if (errs[ierr].getType() == LVSProblem.LVS_WARNING)
                            System.out.println(errs[ierr].getMessage());
                }
	    }
	    
	    // final report
	    System.out.print("LVS " + 
			     (checkSubcells?"":"noSubcells ") +
			     (checkWires?"":"noWires ") +
			     (checkUnwired?"":"noUnwired ") + 
			     (checkPRS?"":"noPrs "));
            if (passed) {
                if (skipped) System.out.println("signoff.");
                else System.out.println("passed.");
            } else {
		System.out.println("failed.");
		System.exit(2);
	    }
	    
	}
	catch( NoSuchElementException e ) {
            e.printStackTrace();
	    usage(null);
	}
    }
}
