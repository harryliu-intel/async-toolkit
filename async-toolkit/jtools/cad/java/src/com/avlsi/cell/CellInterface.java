/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.cell;

import java.util.Iterator;
import java.util.stream.Stream;

import com.avlsi.csp.ast.CSPProgram;
import com.avlsi.csp.csp2java.runtime.CspSnoopingInterface;
import com.avlsi.csp.util.CSPCellInfo;
import com.avlsi.fast.EnvBlock;
import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.metaparameters.MetaParamDefinition;
import com.avlsi.fast.ports.PortDefinition;
import com.avlsi.file.common.HierName;
import com.avlsi.prs.ProductionRuleSet;
import com.avlsi.tools.cosim.CoSimInfo;
import com.avlsi.tools.cosim.ChannelDictionary;
import com.avlsi.tools.cosim.CSPCoSimInfo;
import com.avlsi.tools.cosim.DeviceConstructionException;
import com.avlsi.tools.cosim.JavaCoSimInfo;

/**
 * Data structure for instantiated cells.  Contains all the information a
 * CAST file does, in a form that is easy to use.  (The information isn't
 * all there yet.  HSE and CSP haven't been formalized, so they are not 
 * present.)  The following information is present:
 * <ul>
 *   <li> The cell type
 *   <li> Names of subcells, and their CellInterface instance.
 *   <li> The list of canonical nodes for the cell.
 *   <li> The list of nodes that are connected to each canonical node.
 *   <li> The list of connections between canonical nodes and nodes
 *       in subcells.
 *   <li> The production rule set (PRS).
 * </ul>
 * <p>
 * The CellInterface also can be flattened.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public interface CellInterface extends HierarchyInterface {
    /**
     * The type of the cell ie "FULLADDER".  Does not return the
     * fully-qualified name ever.  If the cell has metaparameters, the
     * metaparameters are included in the type (i.e., "SLACK_1of4(7)")
     *
     **/
    String getType();


    /**
     * Return the path of the file which defines this cell (never
     * null)
     **/
    String getFilename();

    /**
     * Returns the fully qualified type name of the cell, ie
     * "lib.math.add.FULLADDER".
     **/
    String getFullyQualifiedType();

    /**
     * Returns true if this cell was defined with a defchan rather than a
     * define.
     **/
    boolean isChannel();

    /**
     * Returns true if this cell type is a node.
     **/
    boolean isNode();

    /**
     * Returns true if this cell type is an alias.
     **/
    boolean isAlias();

    //
    //
    // Subcells
    //
    //

    /**
     * Returns an Iterator of HierNames of the subcell names.
     **/
    Iterator/*<HierName>*/ getSubcellNames();

    /**
     * Get the CellInterface for a given subcell name.
     **/
    CellInterface getSubcell(HierName name);

    /**
     * Returns an Iterator of Pairs of HierName and CellInterface,
     * including inlined subcells.
     **/
    Iterator/*<Pair<HierName,CellInterface>>*/ getAllSubcellPairs();

    /**
     * Returns an Iterator of Pairs of HierName and CellInterface,
     * excluding inlined subcells.
     **/
    Iterator/*<Pair<HierName,CellInterface>>*/ getSubcellPairs();

    /**
     * Returns an Iterator of Pairs of HierName and CellInterface
     * that are port subcells, excluding inlined subcells.
     **/
    Iterator/*<Pair<HierName,CellInterface>>*/ getPortSubcellPairs();

    /**
     * Returns an Iterator of Pairs of HierName and CellInterface
     * that are local subcells, excluding inlined subcells.
     **/
    Iterator/*<Pair<HierName,CellInterface>>*/ getLocalSubcellPairs();

    boolean isPortSubcell(final HierName n);

    boolean isInlinedSubcell(final HierName n);

    /** 
     * Subcell 'n' is instantiated by some other subcell that 
     * is inlined.
     **/
    boolean parentIsInlinedSubcell(final HierName n);

    /** @see CellImpl.autoInline **/
    void setAutoInline(boolean autoInline);

    /** @see CellImpl.autoInline **/
    boolean isAutoInline();

    /** Returns an Iterator of {@link
     * com.avlsi.fast.metaparameters.MetaParamDefinition}s, representing the
     * channels declared in the metaparameter list.
     *
     * @return iterator of metaparameter definitions, not null
     **/
    Iterator/*<MetaParamDefinition>*/ getMetaParamDefinitions();

    /**
     * Returns an Iterator of {@link com.avlsi.fast.ports.PortDefinition}s,
     * representing the channels declared in the port list.
     *
     * @return iterator of port channel definitions, not null
     **/
    Iterator<PortDefinition> getPortDefinitions();

    /**
     * Returns true if the named port is an implied port; false otherwise.
     **/
    boolean isImpliedPort(String name);

    //
    //
    // Nodes / connections
    //
    //

    /**
     * Returns an Iterator of HierNames of the canonical names in the cell.
     * The canonical names are those names that are not connected to any
     * other canonical names, and are lexicographically less than
     * the nodes that they are connected to.
     **/
    Iterator/*<HierName>*/ getCanonicalNodes();

    /**
     * Returns an iterator through all nodes connected to the given node.
     **/
    Iterator/*<HierName>*/ getConnectedNodes(HierName nodeName);

    /**
     * The canonical name of the given node.
     **/
    HierName/*<HierName>*/ getCanonicalName(HierName nodeName);

    //
    //
    // PRS
    //
    //

    /**
     * Return the synthesizable production rule set (PRS) specified by
     * the cell.
     **/
    ProductionRuleSet getProductionRuleSet();

    /**
     * Return the production rule set specified in the prs-env block
     * (old cast) or assert block (new cast).  These production rules
     * are used in simulation but not to produce transistors.
     **/
    ProductionRuleSet getAssertedProductionRuleSet();

    //
    //
    // Java block
    //
    //

    /**
     * Return the cosimulation inforamation from the Java block
     **/
    JavaCoSimInfo getJavaCoSimInfo();

    /**
     * Fills in the cosimulation information from the port list.  May be
     * called more than once with no adverse effect.
     **/
    void setCoSimInfoFromPorts(CoSimInfo cosimInfo);

    /**
     * Build the CSP class for this cell
     **/
    void buildCSPClass(HierName cellName, ChannelDictionary dict,
            int arbitrationMode, boolean emitCoverageProbes)
	throws ClassNotFoundException, InstantiationException;

    /**
     * Build the CSP class for this cell
     **/
    void buildCSPClass(HierName cellName, ChannelDictionary dict,
            int arbitrationMode, boolean emitCoverageProbes, long seed)
	throws ClassNotFoundException, InstantiationException;

    /**
     * Build the CSP class for this cell
     **/
    void buildCSPClass(HierName cellName, ChannelDictionary dict,
            int arbitrationMode, boolean emitCoverageProbes, long seed,
            float digitalTau)
	throws ClassNotFoundException, InstantiationException;

    /**
     * Build the CSP class for this cell
     **/
    void buildCSPClass(HierName cellName, ChannelDictionary dict,
            int arbitrationMode, boolean emitCoverageProbes,
            CspSnoopingInterface cspSnooper)
	throws ClassNotFoundException, InstantiationException;

    /**
     * Build the java class for this cell
     **/
    void buildJavaClass(HierName cellName,
                        ChannelDictionary dict,
                        ClassLoader aLoader,
                        int arbitrationMode)
	throws ClassNotFoundException, InstantiationException,
               DeviceConstructionException;

    //
    //
    // CSP stuff
    //
    //

    /** Return the cosimulation info from the csp block. **/
    CSPCoSimInfo getCSPCoSimInfo();

    /**
     * Returns as much non-csp info about the cell as the csp
     * processing is going to need (ports/metas information).
     **/
    CSPCellInfo getCSPInfo();

    /**
     * Returns the CSP body, or null if none.
     **/
    CSPProgram getCSPProgram();

    //
    //
    // Spec stuff: exclhi/excllo
    //
    //

    /**
     * Returns an Iterator of ExclusiveNodeSet telling which nodes 
     * can never be high (or low) at the same time.
     *
     * Does not contain information from any ports, subcells, etc.
     * Use the cadencizer to get that information.
     **/
    ExclusiveNodeSets getLocalExclusiveNodeSets();

    //
    //
    // Flatten
    //
    //

    /**
     * Returns a flattened version of the cell.  The flattened version 
     * has no subcells.
     *
     * @param maxPrsDepth  the maximum depth from which production rules
     *   should be imported.  Zero means only the current cell.
     *   Negative means none at all.
     **/
    CellInterface flatten(int maxPrsDepth);

    /**
     * Returns a flattened version of the cell.  The flattened version 
     * has no subcells.  Equivalent to <code>flatten(Integer.MAX_VALUE)<code>.
     **/
    CellInterface flatten();

    //
    //
    // Env block
    //
    //

    /**
     * Returns the environment block, from which environments in
     * instantiatable form may be requested by name.
     **/
    EnvBlock getEnvironments();

    //
    //
    // Misc
    //
    //

    /**
     * Returns true if this cell and c have the same port list and
     * share an ancestor with that port list.
     **/
    boolean sameRefinementAncestor(CellInterface c);

    /**
     * Returns true if this is cell or refined from cell or refined from a
     * CellInterface which is refined from cell or ....
     **/
    boolean eventuallyRefinesFrom(CellInterface cell);

    /**
     * Returns true if all of this cell's port subcells refine from
     * this defchan cell.  Port defchan subcells are traversed until either
     * nodes or a subcell that refines from parentChannel is encounted.
     * When parentChannel is standard.e1of, this method indicates
     * whether this cell communicates with its environment in a fully 
     * four-phase asynchronous manner (and is presumably DI).
     **/
    boolean allPortSubcellsRefineFrom(final CellInterface parentChannel);

    /**
     * Return the cosimulation info that matches the specified
     * behavior.  Gives precedence to java block over csp block.
     **/
    CoSimInfo getCoSimInfo(int cosimBehavior);

    /**
     * Returns true if this cell or any of its subcells have non-
     * env production rules.
     **/
    boolean hasRealProductionRule();

    /**
     * Returns true if this cell or any of its subcells contains a netlist
     * block.
     **/
    boolean hasNetlistBody();

    /**
     * Returns true if this cell or any of its subcells contains a Verilog 
     * block.
     **/
    boolean hasVerilog();

    /**
     * Returns true if this cell or any of its subcells contains runnable CSP.
     **/
    boolean hasRunnableCsp();

    /**
     * Return an BlockInterface view of this cell.
     **/
    BlockInterface getBlockInterface();

    /**
     * Returns the module the cell was defined in, which uniquely
     * specifies the file the cell definition can be found in.
     **/
    String getModuleName();

    /**
     * Return the direct refinement parent for a give cell, we can follow
     * use this to trace a cell all the way to CELL
     */
    CellInterface getDirectRefinementParent();

    /**
     * Returns true if the cell has a complete (non-fragment) prs block.
     **/
    boolean containsCompletePrs();

    /**
     * Returns true if the cell has a complete (non-fragment) subcells block.
     **/
    boolean containsCompleteSubcells();

    /**
     * Returns true if the cell has a java block.
     **/
    boolean containsJava();

    /**
     * Returns true if the cell has a csp block.
     **/
    boolean containsCsp();

    /**
     * Returns true if the cell has sequential CSP statements (as opposed to
     * only containing function and structure declarations).
     **/
    boolean containsRunnableCsp();

    /**
     * Returns true if the cell has a subcells block.
     **/
    boolean containsSubcells();

    /**
     * Returns true if the cell has a netlist block.
     **/
    boolean containsNetlist();

    /**
     * Returns true if the cell has a Verilog block.
     **/
    boolean containsVerilog();

    /**
     * Returns the cell for the named environment <code>envName</code>.
     * @throws NoSuchEnvironmentException If no environment with
     * that name could be found.
     **/
    CellInterface getEnvironment(String envName)
        throws NoSuchEnvironmentException;

    /**
     * Returns a new <code>CellInterface</code>, but with any subcells
     * inlined that have the <code>inline_layout</code> directive set.
     * The cell only has the information needed to lvs, it is not a complete
     * copy.
     **/
    CellInterface inlineLayoutSubcells();

    /**
     * Returns a stream of attribute cells that this cell inherited.  The
     * order of the cells returned by the stream are the same as reading the
     * attribute cells from left to right in CAST.  In other words, the first
     * <code>CellInterface</code> returned is the most overriding.
     **/
    Stream<CellInterface> getInheritedCells();

    /**
     * Returns the name that the implied port <code>self</code> must connect to
     * in a cell that instantiates this cell.
     **/
    String getParentImpliedPort(String self);

    /**
     * Returns the name that the environment extra port <code>self</code> must
     * connect to in the DUT.
     **/
    String getEnvExtraPortMapping(String self);

    /**
     * Returns a <code>CellInterface</code> that is the same as this cell, but
     * inlining any subcells necessary according to the <code>routed</code>
     * directive.  The cell only has the information needed to lvs, it is not a
     * complete copy.
     *
     * @param complete <code>false<code> if flattening according to routed
     * directive should be processed only one level; <code>true<code> if all
     * routed directive should be processed
     **/
    CellInterface routedSubcells(boolean complete);
}
