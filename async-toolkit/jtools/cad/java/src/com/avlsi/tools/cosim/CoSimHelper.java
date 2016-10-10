/*
 * Copyright 2002, 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cosim;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;
import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.BlockIterator;
import com.avlsi.fast.VerilogBlock;
import com.avlsi.file.common.HierName;
import com.avlsi.tools.cosim.CoSimParameters;
import com.avlsi.tools.cosim.spec.*;
import com.avlsi.tools.dsim.NoBehaviorFoundException;
import com.avlsi.util.container.CollectionUtils;
import com.avlsi.util.container.MultiMap;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.Triplet;
import com.avlsi.util.functions.UnaryPredicate;

public class CoSimHelper {
    /**
     * This class should not be instantiated.
     **/
    private CoSimHelper() { }

    private static String append(final String parent, final String subcell) {
        return parent == null ? subcell : parent + '.' + subcell;
    }

    /**
     * Modifies <code>coSimParams</code> to simulate
     * <code>instanceName</code> in the methods specified by
     * <code>coSimSpecList</code>.
     *
     * <p>If <code>verilogCellList</code> is not null, triplets
     * indicating the instance name, cell type, and verilog mode
     * of any cells to be simulated in verilog will be appended to
     * this list.  A mode of "prs" indicates that the verilog is to
     * be automatically generated from the prs if no verilog block
     * named "prs" exists.
     *
     * @param instanceName  The instance name.
     * @param cell  The cell type of which <code>instanceName</code> is an
     *     instance.
     * @param coSimSpecList  The list of methods to be cosimulated.
     * @param coSimParams  The cosimulation parameters to be modified.
     * @param verilogCellList
     *        A list of triplets of (instance name, cell type, verilog mode)
     *        of cells that are to be simulated in verilog.  May be null.
     **/
    public static void setCoSimParams(
            final String instanceName,
            final CellInterface cell,
            final CoSimSpecList coSimSpecList,
            final CoSimParameters coSimParams,
            final List/*<Triplet<String,String,String>>*/ verilogCellList,
            final boolean verbose)
        throws DuplicateInstanceSpecException,
               ExtraInstanceSpecException,
               HierarchyDepthException,
               NoBehaviorFoundException,
               NoSuchInstanceException {
        setCoSimParams(null, instanceName, cell, coSimSpecList, coSimParams,
                       verilogCellList, verbose);
    }

    private static void setCoSimParams(
            final String realInstance,
            final String instanceName,
            final CellInterface cell,
            final CoSimSpecList coSimSpecList,
            final CoSimParameters coSimParams,
            final List/*<Triplet<String,String,String>>*/ verilogCellList,
            final boolean verbose)
        throws DuplicateInstanceSpecException,
               ExtraInstanceSpecException,
               HierarchyDepthException,
               NoBehaviorFoundException,
               NoSuchInstanceException {
        for (final Iterator i = coSimSpecList.iterator(); i.hasNext(); ) {
            final CoSimSpec coSimSpec = (CoSimSpec) i.next();

            setCoSimParams(realInstance, instanceName, cell, coSimSpec,
                           coSimParams, verilogCellList, verbose);
        }
    }

    /**
     * Modifies <code>coSimParams</code> to simulate
     * <code>instanceName</code> in the method specified by
     * <code>coSimSpec</code>.  This is called once for each
     * member in a <code>CoSimSpecList</code>.  The result
     * of multiple calls is a <code>CoSimParameters</code>
     * reflecting the desired cosimulation.
     *
     * <p>If <code>verilogCellList</code> is not null, triplets
     * indicating the instance name, cell type, and verilog mode
     * of any cells to be simulated in verilog will be appended to
     * this list.  A mode of "prs" indicates that the verilog is to
     * be automatically generated from the prs if no verilog block
     * named "prs" exists.
     *
     * @param instanceName  The instance name.
     * @param cell  The cell type of which <code>instanceName</code> is an
     *     instance.
     * @param coSimSpec  A cosimulation method.
     * @param coSimParams  The cosimulation parameters to be modified.
     * @param verilogCellList
     *        A list of triplets of (instance name, cell type, verilog mode)
     *        of cells that are to be simulated in verilog.  May be null.
     **/
    public static void setCoSimParams(
            final String instanceName,
            final CellInterface cell,
            final CoSimSpec coSimSpec,
            final CoSimParameters coSimParams,
            final List/*<Triplet<String,String,String>>*/ verilogCellList,
            final boolean verbose)
        throws DuplicateInstanceSpecException,
               ExtraInstanceSpecException,
               HierarchyDepthException,
               NoBehaviorFoundException,
               NoSuchInstanceException {
        setCoSimParams(null, instanceName, cell, coSimSpec, coSimParams,
                       verilogCellList, verbose);
    }

    private static void setCoSimParams(
            final String realInstance,
            final String instanceName,
            final CellInterface cell,
            final CoSimSpec coSimSpec,
            final CoSimParameters coSimParams,
            final List/*<Triplet<String,String,String>>*/ verilogCellList,
            final boolean verbose)
        throws DuplicateInstanceSpecException,
               ExtraInstanceSpecException,
               HierarchyDepthException,
               NoBehaviorFoundException,
               NoSuchInstanceException {
        final LevelSpecInterface levelSpec = coSimSpec.getLevelSpec();
        final InstSpecList instSpecList = coSimSpec.getInstSpecList();

        if (levelSpec instanceof ModeListLevelSpec) {
            final ModeListLevelSpec modeListLevelSpec =
                (ModeListLevelSpec) levelSpec;

            // Try the various modes
            setCoSimParams(realInstance, instanceName, cell,
                    updateModeList(modeListLevelSpec.getModeList()),
                    instSpecList,
                    coSimParams,
                    verilogCellList, verbose);
        } else {
            final CoSimLevelSpec coSimLevelSpec =
                (CoSimLevelSpec) levelSpec;
            final CoSimSpecList coSimSpecList =
                coSimLevelSpec.getCoSimSpecList();
            final int level = coSimLevelSpec.getLevel();

            boolean hasSubcells = false;
            if (level > 0) {
                ensureRelevance(instanceName, cell, instSpecList);
                // recurse to subcells, dropping irrelevant instspecs,
                // keeping relevant ones, and substituting the one for
                // this cell, if any
                if (cell.containsCompleteSubcells()) {
                    for (final Iterator i = cell.getSubcellPairs();
                            i.hasNext(); ) {
                        final Pair p = (Pair) i.next();
                        final HierName subcellName =
                            (HierName) p.getFirst();
                        final CellInterface subcell =
                            (CellInterface) p.getSecond();
                        final String subcellNameString =
                            subcellName.getAsString('.');

                        // XXX: inlined cells still a problem
                        // XXX: what about wiring cells?
                        if (!subcell.isChannel() && !subcell.isNode()) {
                            if (!hasSubcells) {
                                coSimParams.setBehavior(instanceName,
                                        CoSimParameters.DIGITAL);
                                if (verbose)
                                    System.out.println("Set behavior of " +
                                            instanceName + " to digital");
                                hasSubcells = true;
                            }
                            final String newRealInstance =
                                append(realInstance, subcellNameString);
                            setCoSimParams(
                                    newRealInstance,
                                    instanceName + '.' + subcellNameString,
                                    subcell,
                                    deepen(level,
                                        coSimSpecList,
                                        subcell,
                                        newRealInstance,
                                        instSpecList),
                                    coSimParams,
                                    verilogCellList, verbose);
                        } // if
                    } // for
                }
            }
            // no longer throws HierarchyDepthException, see bug 14406
            if (!hasSubcells) {
                setCoSimParams(realInstance, instanceName, cell,
                        addInstSpecToCoSimSpecList(
                            coSimSpecList,
                            instSpecList),
                        coSimParams,
                        verilogCellList, verbose);
            }
        } // else
    } // setCoSimParams

    private static boolean isNarrow(final ModeList modeList) {
        Mode.SubcellsMode subcell = null;
        for (final Iterator i = modeList.iterator(); i.hasNext(); ) {
            final Mode m = (Mode) i.next();
            if (m == Mode.PRS) return true;
            else if (m == Mode.CSP) return false;
            else if (m instanceof Mode.SubcellsMode)
                subcell = (Mode.SubcellsMode) m;
        }
        return subcell.isNarrow();
    }

    private static ModeList updateModeList(final ModeList modeList) {
        return new ModeList(
            modeList.stream()
                    .map(m -> m instanceof Mode.SubcellsMode &&
                              isNarrow(modeList) ? ((Mode.SubcellsMode) m).makeNarrow()
                                                 : m)
                    .toArray(Mode[]::new));
    }

    /**
     * Modifies <code>coSimParams</code> to simulate
     * <code>instanceName</code> in the method specified by
     * <code>modeList</code>.  
     *
     * <p>If <code>verilogCellList</code> is not null, triplets
     * indicating the instance name, cell type, and verilog mode
     * of any cells to be simulated in verilog will be appended to
     * this list.  A mode of "prs" indicates that the verilog is to
     * be automatically generated from the prs if no verilog block
     * named "prs" exists.
     *
     * @param instanceName  The instance name.
     * @param cell  The cell type of which <code>instanceName</code> is an
     *     instance.
     * @param modeList  A list of modes to be attempted.
     * @param instSpecList  Instance by instance exceptions to the mode
     *     list.
     * @param coSimParams  The cosimulation parameters to be modified.
     * @param verilogCellList
     *        A list of triplets of (instance name, cell type, verilog mode)
     *        of cells that are to be simulated in verilog.  May be null.
     **/
    private static void setCoSimParams(
            final String realInstance,
            final String instanceName,
            final CellInterface cell,
            final ModeList modeList,
            final InstSpecList instSpecList,
            final CoSimParameters coSimParams,
            final List/*<Triplet<String,String,String>>*/ verilogCellList,
            final boolean verbose)
        throws DuplicateInstanceSpecException,
               ExtraInstanceSpecException,
               HierarchyDepthException,
               NoBehaviorFoundException,
               NoSuchInstanceException {
        boolean hasMode = false;
        for (final Iterator i = modeList.iterator(); i.hasNext(); ) {
            final Mode mode = (Mode) i.next();
            hasMode = true;
            if (mode == Mode.SPICE) {
                // spice level cosimulation not yet supported, treat as if
                // cells do not support it
            } else if (mode == Mode.PRS) {
                if (cell.containsCompletePrs()) {
                    coSimParams.setBehavior(instanceName,
                            CoSimParameters.DIGITAL);
                    if (verbose)
                        System.out.println("Set behavior of " +
                                instanceName + " to digital");
                    return;
                }
            } else if (mode instanceof Mode.SubcellsMode) {
                if (cell.containsCompleteSubcells()) {
                    ensureRelevance(instanceName, cell, instSpecList);

                    final Mode.SubcellsMode subMode = (Mode.SubcellsMode) mode;
                    coSimParams.setBehavior(instanceName,
                            CoSimParameters.DIGITAL,
                            subMode.isNarrow(),
                            subMode.getDescendPredicate());

                    if (verbose)
                        System.out.println("Set behavior of " +
                                instanceName + " to digital");

                    // stop when a routed hierarchy is found, but always
                    // recurse into the top-level subcells, even if it is
                    // routed
                    if (realInstance != null && !subMode.descendInto(cell))
                        return;

                    // recurse to subcells, dropping irrelevant instspecs,
                    // keeping relevant ones, and substituting the one for
                    // this cell, if any
                    for (final Iterator iSubcellPair = cell.getSubcellPairs();
                            iSubcellPair.hasNext(); ) {
                        final Pair p = (Pair) iSubcellPair.next();
                        final HierName subcellName =
                            (HierName) p.getFirst();
                        final CellInterface subcell =
                            (CellInterface) p.getSecond();
                        final String subcellNameString =
                            subcellName.getAsString('.');

                        // XXX: inlined cells still a problem
                        // XXX: what about wiring cells?
                        if (!subcell.isChannel() && !subcell.isNode()) {
                            final String newRealInstance =
                                append(realInstance, subcellNameString);
                            final CoSimSpecList newCoSimSpecList =
                                keepRelevant(instSpecList, null, subcell,
                                             newRealInstance);

                            if (newCoSimSpecList == null) {
                                setCoSimParams(
                                        newRealInstance,
                                        instanceName + '.' + subcellNameString,
                                        subcell,
                                        updateModeList(modeList),
                                        instSpecList,
                                        coSimParams,
                                        verilogCellList, verbose);
                            } else {
                                setCoSimParams(
                                        newRealInstance,
                                        instanceName + '.' + subcellNameString,
                                        subcell,
                                        addInstSpecToCoSimSpecList(
                                            newCoSimSpecList,
                                            instSpecList),
                                        coSimParams,
                                        verilogCellList, verbose);
                            } // else
                        } // if
                    } // for

                    return;
                }
            } else if (mode == Mode.CSP) {
                if (cell.containsRunnableCsp()) {
                    coSimParams.setBehavior(instanceName,
                            CoSimParameters.CSP);
                    if (verbose)
                        System.out.println("Set behavior of " +
                                instanceName + " to csp");
                    return;
                }
            } else if (mode == Mode.JAVA) {
                if (cell.containsJava()) {
                    coSimParams.setBehavior(instanceName,
                            CoSimParameters.JAVA);
                    if (verbose)
                        System.out.println("Set behavior of " +
                                instanceName + " to java");
                    return;
                }
            } else if (mode instanceof Mode.VerilogMode) {
                final Mode.VerilogMode verilogMode = (Mode.VerilogMode) mode;
                final BlockIterator bi =
                    cell.getBlockInterface()
                        .iterator(BlockInterface.VERILOG);

                if (bi.hasNext()) {
                    // get the verilog block, making sure there is exactly 1
                    final VerilogBlock verilogBlock =
                        (VerilogBlock) bi.next();
                    assert !bi.hasNext();

                    for (Iterator iName = verilogBlock.getNamesIterator();
                         iName.hasNext(); ) {
                        final String level = (String) iName.next();

                        if (level.equals(verilogMode.getLevel())) {
                            coSimParams.setBehavior(instanceName,
                                    CoSimParameters.VERILOG, level);
                            if (verbose)
                                System.out.println("Set behavior of " +
                                        instanceName + " to " + verilogMode);
                            if (verilogCellList != null)
                                verilogCellList.add
                                    (new Triplet(instanceName,
                                                 cell.getFullyQualifiedType(),
                                                 level));
                            return;
                        }
                    }
                }

                // if we haven't found a matching named block and the 
                // name is null, then we will automatically generate
                // the verilog using prs2verilog
                if (verilogMode.getLevel() == null) {
                    coSimParams.setBehavior(instanceName,
                            CoSimParameters.VERILOG, null);
                    if (verbose)
                        System.out.println("Set behavior of " +
                                instanceName + " to " + verilogMode);
                    if (verilogCellList != null)
                        verilogCellList.add
                            (new Triplet(instanceName,
                                         cell.getFullyQualifiedType(),
                                         null));
                    return;
                }
            }
        }

        if (!hasMode) {
            coSimParams.setBehavior(instanceName, CoSimParameters.NULL);
            if (verbose)
                System.out.println("Set behavior of " + instanceName +
                                   " to null");
            return;
        }

        // Error: this cell doesn't support any of the specified modes.
        throw new NoBehaviorFoundException("Specified behaviors " +
                modeList + instSpecList + " not " + "found in " +
                instanceName + "/" + cell.getFullyQualifiedType());
    }

    /**
     * Returns the cosim spec associated with an exception based on type, or
     * <code>null</code> if no match is found.
     **/
    private static CoSimSpecList typeMatches(final CellInterface cell,
                                             final InstSpecList instSpecList) {
        final CoSimSpecList[] result = new CoSimSpecList[] { null };
        final UnaryPredicate check = new UnaryPredicate() {
            public boolean evaluate(final Object o) {
                final CellInterface c = (CellInterface) o;
                for (Iterator i = instSpecList.iterator(); i.hasNext(); ) {
                    final InstSpec instSpec = (InstSpec) i.next();
                    final String inst = instSpec.getInstanceName();
                    final String type = c.getFullyQualifiedType();
                    if (inst.equals(type) ||
                        inst.equals(CellUtils.getBaseType(type))) {
                        result[0] = instSpec.getCoSimSpecList();
                        break;
                    }
                }
                return result[0] != null;
            }
        };
        final boolean found =
            CellUtils.matchRefinement(cell, check) ||
            CellUtils.matchRefinement(cell, new UnaryPredicate() {
                    public boolean evaluate(final Object o) {
                        final CellInterface c = (CellInterface) o;
                        return CellUtils.matchInheritance(c, check);
                    }
                });

        return result[0];
    }

    /**
     * Returns the cosim spec associated with an exception based on special
     * predicates, or <code>null</code> if no match is found.  Currently,
     * only the @asta_blackbox predicate is supported.
     **/
    private static CoSimSpecList specialMatches(final CellInterface cell,
                                                final InstSpecList instSpecList)
    {
        for (Iterator i = instSpecList.iterator(); i.hasNext(); ) {
            final InstSpec instSpec = (InstSpec) i.next();
            final String inst = instSpec.getInstanceName();
            if (inst.startsWith("@")) {
                if (inst.equals("@asta_blackbox")) {
                    if (CellUtils.isAstaBlackbox(cell)) {
                        return instSpec.getCoSimSpecList();
                    }
                } else {
                    // TODO: throw exception on unknown predicates
                }
            }
        }

        return null;
    }

    /**
     * Returns a new relevant <code>CoSimSpecList</code>.  The new
     * CoSimSpecList contains the entry, if any, from InstSpecList that exactly
     * matches <code>subcellName</code>, or <code>oldCoSimSpecList</code> if
     * none matches.
     **/
    private static CoSimSpecList keepRelevant(
            final InstSpecList oldInstSpecList,
            final CoSimSpecList oldCoSimSpecList,
            final CellInterface subcell,
            final String instance) {

        CoSimSpecList coSimSpecList = null;

        for (final Iterator i = oldInstSpecList.iterator(); i.hasNext(); ) {
            final InstSpec instSpec = (InstSpec) i.next();
            final String instanceName = instSpec.getInstanceName();

            // XXX: this will not catch bogus cells mentioned in the
            // InstSpecList
            if (instanceName.equals(instance)) {
                coSimSpecList = instSpec.getCoSimSpecList();
                break;
            }
        }

        if (coSimSpecList == null)
            coSimSpecList = typeMatches(subcell, oldInstSpecList);

        if (coSimSpecList == null)
            coSimSpecList = specialMatches(subcell, oldInstSpecList);

        //if (coSimSpecList != null && verbose)
        //    System.out.println("substituting " + coSimSpecList +
        //            " for " + instanceName);

        return coSimSpecList == null ? oldCoSimSpecList : coSimSpecList;
    }

    /**
     * Ensures that the exceptions mentioned in <code>instSpecList</code>
     * are not mentioned more than once.
     **/
    private static void ensureRelevance(
            final String instanceName,
            final CellInterface cell,
            final InstSpecList instSpecList)
        throws DuplicateInstanceSpecException,
               NoSuchInstanceException {
        final Set instNames = new HashSet();

        for (final Iterator i = instSpecList.iterator(); i.hasNext(); ) {
            final InstSpec instSpec = (InstSpec) i.next();

            final String instName = instSpec.getInstanceName();

            // instance exception mentioned twice
            if (!instNames.add(instName))
                throw new DuplicateInstanceSpecException(instanceName,
                        instName);
        }
    }

    /**
     * Returns a new CoSimSpec for the specified subcell.  The returned
     * CoSimSpec has a CoSimLevelSpec with level one less than the level
     * passed in and uses the <code>defaultCoSimSpecList</code> unless
     * one of the exceptions from the instSpecList exactly matches
     * this subcell.  All exceptions in instSpecList are passed on in the new
     * InstSpecList.
     **/
    private static CoSimSpec deepen(
            final int level,
            final CoSimSpecList defaultCoSimSpecList,
            final CellInterface subcell,
            final String instance,
            final InstSpecList instSpecList) {

        final CoSimSpecList newCoSimSpecList =
            keepRelevant(instSpecList, defaultCoSimSpecList, subcell, instance);

        return new CoSimSpec(
                new CoSimLevelSpec(level - 1, newCoSimSpecList),
                instSpecList);
    }

    /**
     * Returns a new CoSimSpecList that is the same as
     * <code>coSimSpecList</code>, but with <code>instSpecList</code>
     * appended to the <code>InstSpecList</code>s of the
     * component <code>CoSimSpec</code>s.
     **/
    private static CoSimSpecList addInstSpecToCoSimSpecList(
        final CoSimSpecList coSimSpecList,
        final InstSpecList instSpecList) {

        final List newCoSimSpecs = new ArrayList();

        for (final Iterator i = coSimSpecList.iterator(); i.hasNext(); ) {
            final CoSimSpec coSimSpec = (CoSimSpec) i.next();
            final List newInstSpecs = new ArrayList();

            // XXX: should we append or prepend???
            CollectionUtils.addAll(newInstSpecs,
                    coSimSpec.getInstSpecList().iterator());
            CollectionUtils.addAll(newInstSpecs,
                    instSpecList.iterator());

            newCoSimSpecs.add(new CoSimSpec(coSimSpec.getLevelSpec(),
                        new InstSpecList((InstSpec[])
                            newInstSpecs.toArray(new InstSpec[0]))));
        }

        return new CoSimSpecList((CoSimSpec[])
                newCoSimSpecs.toArray(new CoSimSpec[0]));
    }

    /**
     * Fill <code>result</code> with a mapping from a cell type to all of its
     * behaviors.
     **/
    public static void getBehaviorByType(final CellInterface cell,
                                         final CoSimParameters params,
                                         final HierName prefix,
                                         final MultiMap result) {
        if (cell.isNode() || cell.isChannel()) return;
        final String sinst = prefix.getAsString('.');
        final int beh = params.lookupBehavior(sinst);
        final String type = cell.getFullyQualifiedType();
        boolean recurse = false;
        if ((beh & CoSimParameters.CSP) != 0) result.put(type, Mode.CSP);
        if ((beh & CoSimParameters.JAVA) != 0) result.put(type, Mode.JAVA);
        if ((beh & CoSimParameters.DIGITAL) != 0) {
            result.put(type, cell.containsSubcells() ? Mode.SUBCELLS
                                                     : Mode.PRS);
            recurse = true;
        }
        if ((beh & CoSimParameters.VERILOG) != 0)
            result.put(type,
                       new Mode.VerilogMode(params.lookupVerilogLevel(sinst)));
        
        if (recurse) {
            for (Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
                final Pair/*<HierName,CellInterface>*/ p = (Pair) i.next();
                final HierName instance = (HierName) p.getFirst();
                final CellInterface subcell = (CellInterface) p.getSecond();
                final HierName full = HierName.append(prefix, instance);
                getBehaviorByType(subcell, params, full, result);
            }
        }
    }
}
