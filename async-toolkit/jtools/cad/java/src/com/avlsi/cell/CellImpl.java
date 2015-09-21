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

import java.lang.reflect.Constructor;

import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.StringWriter;

import java.math.BigInteger;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.StringTokenizer;
import java.util.stream.Stream;

import com.avlsi.cast.CastSemanticException;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.impl.CastParsingOption;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.csp.ast.CSPProgram;
import com.avlsi.csp.csp2java.CSP2Class;
import com.avlsi.csp.csp2java.runtime.CspSnoopingInterface;
import com.avlsi.csp.util.CSPCellInfo;
import com.avlsi.fast.AssertBlock;
import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.BlockIterator;
import com.avlsi.fast.CellBlock;
import com.avlsi.fast.CspBlock;
import com.avlsi.fast.DirectiveBlock;
import com.avlsi.fast.EnvBlock;
import com.avlsi.fast.JavaBlock;
import com.avlsi.fast.NetlistBlock;
import com.avlsi.fast.PrsBlock;
import com.avlsi.fast.PrsDirective;
import com.avlsi.fast.SubcellBlock;
import com.avlsi.fast.metaparameters.MetaParamDefinition;
import com.avlsi.fast.ports.ArrayType;
import com.avlsi.fast.ports.ChannelType;
import com.avlsi.fast.ports.NodeType;
import com.avlsi.fast.ports.PortDefinition;
import com.avlsi.fast.ports.PortTypeInterface;
import com.avlsi.fast.ports.StructureType;
import com.avlsi.file.common.HierName;
import com.avlsi.file.cdl.parser.CDLDeviceNameFilter;
import com.avlsi.file.cdl.parser.CDLNodeNameFilter;
import com.avlsi.file.cdl.parser.Template;
import com.avlsi.io.NullWriter;
import com.avlsi.prs.ProductionRule;
import com.avlsi.prs.ProductionRuleSet;
import com.avlsi.tools.cosim.ChannelDictionary;
import com.avlsi.tools.cosim.CoSimInfo;
import com.avlsi.tools.cosim.CoSimParameters;
import com.avlsi.tools.cosim.CSPCoSimInfo;
import com.avlsi.tools.cosim.DeviceConstructionException;
import com.avlsi.tools.cosim.DeviceParameters;
import com.avlsi.tools.cosim.JavaCoSimInfo;
import com.avlsi.tools.tsim.ChannelOutput;
import com.avlsi.tools.tsim.ChannelInput;
import com.avlsi.tools.tsim.Startable;
import com.avlsi.tools.tsim.WideNode;
import com.avlsi.util.bool.BooleanExpressionInterface;
import com.avlsi.util.bool.BooleanUtils;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.FilteringIterator;
import com.avlsi.util.container.MappingIterator;
import com.avlsi.util.container.MultiMap;
import com.avlsi.util.container.Namespace;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.Triplet;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.exception.AssertionFailure;
import com.avlsi.util.functions.UnaryFunction;
import com.avlsi.util.functions.UnaryPredicate;
import com.avlsi.util.text.StringUtil;


/**
 * Implementation of {@link CellInterface}, implements the 
 * interface, as well as adding modification functionality.
 **/
public class CellImpl implements CellInterface {

    /**
     * The type of node cells.
     **/
    public static final String NODE_TYPE = "$NODE";

    /**
     * Constant for <code>definitionKind</code>, indicating a cell
     * defined with <code>define</code>.
     **/
    public static final int CELL = 0;

    /**
     * Constant for <code>definitionKind</code>, indicating a cell
     * defined not in cast, but on the fly be java code.
     * Indicates a synthetic cell which isn't part of the
     * whole implied-port-refinement scheme but which might have
     * subcells which aren't synthetic.  InstanceValue will use this
     * when it assigns defaults to implied ports: if this is true, the
     * InstanceValue will create nodes for any implied ports which
     * aren't there.
     **/
    public static final int SYNTHETIC_CELL = 1;

    /**
     * Constant for <code>definitionKind</code>, indicating a cell
     * defined with <code>define ... attributes</code>.
     * Attributes cells cannot refine other cells, and are
     * the only cells that can be used in <code>&lt;+</code> inheritance.
     **/
    public static final int ATTRIBUTES_CELL = 2;

    /**
     * Constant for <code>definitionKind</code>, indicating a cell
     * defined with <code>defchan</code>.
     **/
    public static final int CHANNEL = 3;

    /**
     * Constant for <code>definitionKind</code>, indicating a cell
     * defined with <code>defalias</code>.
     **/
    public static final int ALIAS_CELL = 4;

    /**
     * The type of the cell.
     **/
    private final String type;

    /**
     * The filename of the file.
     **/
    private final String filename;

    /**
     * One of {@link CELL}, {@link SYNTHETIC_CELL}, {@link ATTRIBUTES_CELL},
     * or {@link CHANNEL}, indicating how the cell was declared.
     **/
    private final int definitionKind;

    /**
     * The type of the cell furthest up the refinement hierarchy which
     * is a direct ancestor of this cell and has the same port list.
     * This may refer to this cell.
     **/
    private String refinementAncestor;

    /**
     * The CellImpl this cell was refined from, if any.  This makes it
     * possible to tell if one cell specializes another one (see
     * eventuallyRefinesFrom())
     **/
    private CellImpl directRefinementParent;

    /**
     * The module the cell was defined in.  This also uniquely
     * specifies the file the cell definition can be found in.
     **/
    private final String moduleName;

    /**
     * Map from HierName (subcell usage name) to CellInterface
     * (the cell instance).
     **/
    private final Map/*<HierName,CellInterface>*/ subcellMap;

    /**
     * Map from HierName (subcell usage name) to a Triplet
     * (CellInterface, CellInterface, boolean).  The first
     * CellInterface is the cell instance of the refinement parent.
     * The second is the cell instance of the refinement child and
     * will be moved with the name into subcellMap when the refinement
     * parent is set.  The third is whether the subcell should be
     * inlined after being refined.
     *
     * <p> The movement into subcellMap and the accompanying
     * typechecks are "validation" of the subtype.  As subtypes are
     * validated by the refinement process, they are removed from this
     * Map, which should be empty at the end of refinement.
     *
     * <p> Elements in an array of subcells may be subtyped as a unit, or
     * subtyped individually.
     **/
    private final Map/*<HierName,
                       Triplet<CellInterface,
                               CellInterface,
                               Boolean>>*/ subtypeMap;
    private final Set/*<HierName>*/ subtypeArray;

    /**
     * Set of HierNames of cells that are port cells.
     **/
    private final Set/*<HierName>*/ portSubcellSet;

    /**
     * Set of HierNames of cells that have been inlined or flattened.
     **/
    private final Set/*<HierName>*/ inlinedSubcellSet;

    private final Set/*<HierName>*/ adoptedSet;

    /** List of {@link com.avlsi.fast.metaparameters.MetaParamDefinition}s. **/
    private final List/*<MetaParamDefinition>*/ metaParamDefinitions;

    /**
     * Map from names of ports (Strings) to their {@link
     * com.avlsi.fast.ports.PortDefinition}s.
     **/
    private final Map<String,PortDefinition> portDefinitions;

    /**
     * A mapping from the names of implied ports in this cell to the names to
     * be connected to in the parent cell.
     **/
    private final Map<String,String> impliedPortMapping;

    /**
     * A mapping from the names of environment extra ports in this cell to the
     * names to be connected to in the parent cell.
     **/
    private final Map<String,String> envExtraPortMapping;

    /**
     * Node aliases.
     * Possibly includes channel aliases.
     **/
    // Made non-final for cell-refinement
    private AliasedSet/*<HierName>*/ aliases;

    /**
     * Whether the cast cell contained a prs{...} construct, even an
     * empty one.  This is also set true as part of refinement if a
     * refinement ancestor had a prs block.
     **/
    private boolean hasPrsBlock = false;

    /**
     * True if the prs block is complete.  Undefined if 
     * <code>!hasPrsBlock</code>.
     **/
    private boolean prsBlockIsComplete = false;

    /**
     * Whether the cast cell contained a subcells{...} construct, even
     * an empty one.  This is also set true as part of refinement if a
     * refinement ancestor had a subcells block.
     **/
    private boolean hasSubcellsBlock = false;

    /**
     * True if the subcells block is complete.  Undefined if 
     * <code>!hasSubcellsBlock</code>.
     **/
    private Boolean subcellsBlockIsComplete = null;

    /**
     * Whether the cast cell contained a subtypes{...} construct, even
     * an empty one.  Is meaningless after completing the process of
     * refinement.
     **/
    private boolean hasSubtypesBlock = false;

    /**
     * The cosimulation info from the Java block
     **/
    private final JavaCoSimInfo javaCosimInfo;

    /**
     * A structure aggregating all blocks in a cell
     **/
    private final CellBlock blocks;

    /**
     * Whether this cell should be inlined wherever it is instantiated
     **/
    public boolean autoInline = false;

    /**
     * A list of cells that this cell inherited from via &lt;+.
     **/
    private final List<CellInterface> inheritedCells;

    private final Map/*<Position>*/ instancePositions;

    /**
     * Whether a warning about refinement has already been emitted for this
     * type.  This is only used when we turned off inline processing.
     **/
    private boolean refinementWarned = false;

    /** Constructor used by old cast parser **/
    public CellImpl(final String type) {
        this(type, null, CELL);
    }

    /**
     * Constructor used by new cast parser to generate all "real"
     * (read from cast) cells
     **/
    private CellImpl(final String type, final String moduleName) {
        this(type, moduleName, CELL);
    }

    public CellImpl(final String type,
                    final String moduleName,
                    final int definitionKind) {
        this(type, moduleName, definitionKind, null);
    }

    public CellImpl(final String type,
                    final String moduleName,
                    final int definitionKind,
                    final String filename) {
        this.type = type;
        this.filename = filename;
        // To start out with; modified if this cell is refined
        this.refinementAncestor =
            moduleName == null ? type : moduleName + '.' + type;
        this.directRefinementParent = null; // To start out with; modified if this cell is refined
        this.moduleName = moduleName;
        Debug.assertTrue(definitionKind == CELL ||
                     definitionKind == SYNTHETIC_CELL ||
                     definitionKind == ATTRIBUTES_CELL ||
                     definitionKind == CHANNEL ||
                     definitionKind == ALIAS_CELL);
        this.definitionKind = definitionKind;
        this.subcellMap = new LinkedHashMap/*<HierName,CellInterface>*/();
        this.subtypeMap =
            new HashMap/*<HierName,
                         Triplet<CellInterface,CellInterface,Boolean>>*/();
        this.subtypeArray = new HashSet/*<HierName>*/();
        this.portSubcellSet = new HashSet/*<HierName>*/();
        this.inlinedSubcellSet = new HashSet/*<HierName>*/();
        this.adoptedSet = new HashSet/*<HierName>*/();
        this.metaParamDefinitions = new ArrayList/*<MetaParamDefinition>*/();
        this.portDefinitions = new LinkedHashMap<String,PortDefinition>();
        this.impliedPortMapping = new LinkedHashMap<String,String>();
        this.envExtraPortMapping = new LinkedHashMap<String,String>();
        this.aliases =
            new AliasedSet/*<HierName>*/(HierName.getComparator());
        this.javaCosimInfo = new JavaCoSimInfo();
        blocks = new CellBlock();
        blocks.iterator(BlockInterface.ASSERT).add(new AssertBlock());
        blocks.iterator(BlockInterface.ENV).add(new EnvBlock(this));
        blocks.iterator(BlockInterface.PRS).add(new PrsBlock());
        blocks.iterator(BlockInterface.NETLIST).add(new NetlistBlock(this));
        blocks.iterator(BlockInterface.SUBCELL).add(new SubcellBlock());
        blocks.iterator(BlockInterface.CSP).add(new CspBlock());
        blocks.iterator(BlockInterface.JAVA).add(new JavaBlock());
        this.inheritedCells = new ArrayList<>();
        this.instancePositions = new LinkedHashMap/*<Position>*/();
    }

    public String getType() {
        return type;
    }

    public String getFilename() {
        return filename;
    }

    public String getModuleName() {
        return moduleName;
    }

    public String getFullyQualifiedType() {
        if (moduleName == null)
            return type;
        else
            return moduleName + '.' + type;
    }

    public boolean isSyntheticP() {
        return definitionKind == SYNTHETIC_CELL;
    }

    public boolean isChannel() {
        return definitionKind == CHANNEL;
    }

    public boolean isAttributesCell() {
        return definitionKind == ATTRIBUTES_CELL;
    }

    public boolean isAlias() {
        return definitionKind == ALIAS_CELL;
    }

    public boolean isNode() {
        return type.equals(NODE_TYPE);
    }

    //
    //
    // Subtypes (refining subcells)
    //
    //

    /**
     * Note that the cast cell contained a subtypes{...} construct.
     **/
    public void setHasSubtypesBlock() { hasSubtypesBlock = true; }

    /**
     * Add information on how cellName is refined to the internal
     * table.  Should not be called after setRefinementParent().
     * @param inlineP Whether the subcell should be inlined after it's
     *                refined.  Subcells can't be inlined before refinement.
     * @param looseCheck Whether to strictly require that the refinement child
     *                   descend linearly from the refinement parent.
     **/
    public void addSubtype(final HierName cellName,
                           final CellInterface refinementParent,
                           final CellInterface refinementChild,
                           final boolean inlineP,
                           final boolean looseCheck)
        throws RefinementException {

        if (looseCheck) {
            if (!((CellImpl) refinementParent).oldSameRefinementAncestor(
                        refinementChild)) {
                throw new RefinementException
                    ("subtype for " + cellName +
                     " has to have the same refinement ancestor and "+
                     "port list as the cells they're subtyping", 
                     refinementParent.getFullyQualifiedType(),
                     refinementChild.getFullyQualifiedType());
            }
        } else {
            if (!refinementParent.sameRefinementAncestor(refinementChild)) {
                throw new RefinementException
                    ("subtype for " + cellName +
                     " must be a direct descendent of the refinement ancestor" +
                     " (see bug 16459)",
                     refinementParent.getFullyQualifiedType(),
                     refinementChild.getFullyQualifiedType());
            }
        }

        final HierName baseName = cellName.getArrayBase();

        if (subtypeMap.containsKey(cellName)) {
            // { TYPE1 :> TYPE2 inst; TYPE1 :> TYPE2 inst; }
            throw new RefinementException("subtyped " + cellName + " twice",
                                          refinementParent.getFullyQualifiedType(),
                                          refinementChild.getFullyQualifiedType());
        } else if (subtypeMap.containsKey(baseName)) {
            // { TYPE1 :> TYPE2 inst; TYPE1 :> TYPE2 inst[0]; }
            throw new RefinementException("whole array " +
                                          cellName.getArrayBase() +
                                          " already subtyped",
                                          refinementParent.getFullyQualifiedType(),
                                          refinementChild.getFullyQualifiedType());
        } else if (subtypeArray.contains(cellName)) {
            // { TYPE1 :> TYPE2 inst[0]; TYPE1 :> TYPE2 inst; }
            throw new RefinementException("an element of the array " +
                                          cellName + " already subtyped",
                                          refinementParent.getFullyQualifiedType(),
                                          refinementChild.getFullyQualifiedType());
        } else {
            if (!baseName.equals(cellName)) subtypeArray.add(baseName);
            subtypeMap.put(cellName,
                           new Triplet(refinementParent,
                                       refinementChild,
                                       new Boolean(inlineP)));
        }
    }

    //
    //
    // Subcells
    //
    //

    /**
     * Note that the cast cell contained a fragment subcells{...} construct.
     **/
    public void setHasFragmentSubcellsBlock() {
        hasSubcellsBlock = true;
        subcellsBlockIsComplete = Boolean.FALSE;
    }

    /**
     * Note that the cast cell contained a non-fragment subcells{...}
     * construct.
     **/
    public void setHasCompleteSubcellsBlock() {
        hasSubcellsBlock = true;
        subcellsBlockIsComplete = Boolean.TRUE;
    }

    /**
     * Does the cell contain a subcells block, either explicitly or
     * through refinement?
     **/
    public boolean containsSubcells() {
        return hasSubcellsBlock || hasSubtypesBlock;
    }

    public boolean containsCompleteSubcells() {
        return hasSubcellsBlock && subcellsBlockIsComplete.booleanValue();
    }

    public boolean containsFragmentSubcells() {
        return hasSubcellsBlock && !subcellsBlockIsComplete.booleanValue();
    }

    /**
     * Returns an unmodifiable Iterator of HierNames of the subcell names.
     **/
    public Iterator getSubcellNames() {
        return Collections.unmodifiableMap(subcellMap).keySet().iterator();
    }

    /**
     * Get the CellInterface for a given subcell name, or null if there is
     * no subcell by that name.
     **/
    public CellInterface getSubcell(HierName name) {
        final CellInterface cell = (CellInterface) subcellMap.get(name);

        /*
        boolean found = false;
        for (final Iterator i = getSubcellPairs(); i.hasNext(); ) {
            final Pair p = (Pair) i.next();
            final HierName n = (HierName) p.getFirst();
            final CellInterface c = (CellInterface) p.getSecond();

            if (n.equals(name)) {
                found = true;
                break;
            }
        }

        Debug.assertTrue(found == (cell != null));
        */

        return cell;
    }

    /**
     * Returns an Iterator of Pairs of HierName and CellInterface for
     * non-inlined subcells.
     **/
    public Iterator getSubcellPairs() {
        return new FilteringIterator(getAllSubcellPairs(),
            new UnaryPredicate() {
                public boolean evaluate(final Object o) {
                    return !isInlinedSubcell((HierName) ((Pair) o).getFirst());
                }});
    }

    /**
     * Returns an Iterator of Pairs of HierName and CellInterface for
     * all subcells, even inlined ones.
     **/
    public Iterator getAllSubcellPairs() {
        return new MappingIterator(
            Collections.unmodifiableMap(subcellMap).entrySet().iterator(),
            new UnaryFunction() {
                public Object execute(final Object o) {
                    final Entry entry = (Entry) o;
                    return new Pair(entry.getKey(), entry.getValue());
                }
            });
    }

    /**
     * Adds the given cell as a subcell with the specified usage name.
     **/
    public void addSubcellPair(final HierName use,
                               final CellInterface subcell,
                               final boolean portP) {
        addSubcellPair(use, subcell, portP, -1, -1);
    }

    /**
     * Adds the given cell as a subcell with the specified usage name, and
     * parse location.
     **/
    public void addSubcellPair(final HierName use,
                               final CellInterface subcell,
                               final boolean portP,
                               final int line, final int column) {
        subcellMap.put(use, subcell);

        if (portP)
            portSubcellSet.add(use);

        if (line >= 0 && column >= 0 &&
            !subcell.isNode() && !subcell.isChannel())
            instancePositions.put(use, new Position(line, column));
    }

    private static class Position implements Comparable {
        private static class Point implements Comparable {
            public int line, column;
            public Point(int line, int column) {
                this.line = line;
                this.column = column;
            }
            public int compareTo(final Object o) {
                final Point p = (Point) o;
                return line == p.line ? column - p.column : line - p.line;
            }
            public String toString() {
                return line + ":" + column;
            }
        }

        public List location;

        public Position(int line, int column) {
            this.location = new ArrayList(1);
            location.add(new Point(line, column));
        }

        public Position(Position prefix, Position suffix) {
            this.location = new ArrayList(prefix.location.size() +
                                          suffix.location.size());
            location.addAll(prefix.location);
            location.addAll(suffix.location);
        }

        public int compareTo(final Object o) {
            final Position p = (Position) o;
            int i, j;
            for (i = 0, j = 0; i < location.size() && j < p.location.size();
                 ++i, ++j) {
                final Point pi = (Point) location.get(i);
                final Point pj = (Point) p.location.get(j);
                int x = pi.compareTo(pj);
                if (x != 0) return x;
            }
            return location.size() - p.location.size();
        }

        public String toString() {
            return location.toString();
        }
    }

    /**
     * Get an iterator over columns of instances that is an initial guess for
     * floorplanning.
     **/
    public Iterator getFloorplanInstances() {
        final MultiMap mm =
            new MultiMap(new TreeMap(), MultiMap.ARRAY_LIST_FACTORY);
        for (Iterator i = instancePositions.entrySet().iterator();
             i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            final HierName instance = (HierName) entry.getKey();
            if (!isInlinedSubcell(instance)) mm.put(entry.getValue(), instance);
        }
        final Iterator it = mm.keySet().iterator();
        return new Iterator() {
            public boolean hasNext() {
                return it.hasNext();
            }
            public Object next() {
                return mm.get(it.next());
            }
            public void remove() {
                throw new UnsupportedOperationException();
            }
        };
    }

    private void addPosition(final HierName newName,
                             final HierName instName,
                             final CellInterface cell,
                             final HierName subName) {
        if (cell instanceof CellImpl) {
            final Position prefix = (Position) instancePositions.get(instName);
            final Position suffix =
                (Position) ((CellImpl) cell).instancePositions.get(subName);
            if (suffix != null && prefix != null)
                instancePositions.put(newName, new Position(prefix, suffix));
        }
    }

    /**
     * Returns the non-inlined port subcell pairs.
     **/
    public Iterator getPortSubcellPairs() {
        return new FilteringIterator(getSubcellPairs(),
            new UnaryPredicate() {
                public boolean evaluate(final Object o) {
                    return isPortSubcell((HierName) ((Pair) o).getFirst());
                }});
    }

    /**
     * Returns the non-inlined local subcell pairs.
     **/
    public Iterator getLocalSubcellPairs() {
        return new FilteringIterator(getSubcellPairs(),
            new UnaryPredicate() {
                public boolean evaluate(final Object o) {
                    return !isPortSubcell((HierName) ((Pair) o).getFirst());
                }});
    }

    public Iterator getMetaParamDefinitions() {
        return Collections.unmodifiableList(metaParamDefinitions).iterator();
    }

    public Iterator<PortDefinition> getPortDefinitions() {
        return Collections.unmodifiableCollection(portDefinitions.values()).iterator();
    }

    public PortDefinition getPortDefinition(final String name) {
        return portDefinitions.get(name);
    }

    /**
     * Appends a port definition to the list.
     *
     * @param portDefinition  port definition
     **/
    public void addPortDefinition(final PortDefinition portDefinition) {
        portDefinitions.put(portDefinition.getName(), portDefinition);
    }

    public boolean isImpliedPort(final String name) {
        return impliedPortMapping.containsKey(name);
    }

    /**
     * Appends a metaparameter definition to the list.
     *
     * @param metaParamDefinition  metaparameter definition
     **/
    public void addMetaParamDefinition(
            final MetaParamDefinition metaParamDefinition) {
        metaParamDefinitions.add(metaParamDefinition);
    }

    public boolean isPortSubcell(final HierName n) {
        return portSubcellSet.contains(n);
    }

    public boolean isInlinedSubcell(final HierName n) {
        return inlinedSubcellSet.contains(n);
    }

    public boolean parentIsInlinedSubcell(final HierName n) {
        if (n.getNumComponents()<2) return false;
        for (Iterator scit=inlinedSubcellSet.iterator(); scit.hasNext();)
            if (n.isChildOf((HierName)scit.next())) return true;
        return false;
    }

    /**
     * Imports all production rules and connections from the 
     * specified subcell into this cell.
     *
     * @throws NoSuchSubcellException  if there is no subcell with the
     *   specified name
     * @throws SubcellCreationException if the subcell contains prs so
     *   can't be inlined.
     **/
    public void inlineSubcell(final HierName use)
        throws NoSuchSubcellException, SubcellCreationException {
        /* this actually doesn't work correctly.  
         * prs can refer to both foo.0 and foo.d[0] */
        inlineSubcell(use, false, true);
    }

    /**
     * Imports all production rules and connections from the 
     * flattened version of the specified subcell into this cell.
     *
     * @throws NoSuchSubcellException  if there is no subcell with the
     *   specified name
     * @throws SubcellCreationException if subcell contains netlist body so
     *   can't be flattened
     **/
    public void flattenSubcell(final HierName use)
        throws NoSuchSubcellException, SubcellCreationException {
        inlineSubcell(use, true, true);
    }

    /**
     * Imports all production rules and connections from 
     * the specified subcell into this cell.
     * Subcell will be flattened first if <code>flattenP</code>
     * is <code>true</code>.
     *
     * @throws NoSuchSubcellException  if there is no subcell with the
     *   specified name
     * @throws SubcellCreationException if the subcell contains prs so
     *   can't be inlined.
     **/
    private void inlineSubcell(final HierName use,
                               final boolean flattenP,
                               final boolean checkPrs)
        throws NoSuchSubcellException, SubcellCreationException {

        final CellInterface subcell = (CellInterface) subcellMap.get(use);

        if (subcell == null)
            throw new NoSuchSubcellException(use);

        if (flattenP) {
            if (subcell.hasNetlistBody()) {
                throw new SubcellCreationException("Subcell has netlist body, so cannot be flattened", use, getFullyQualifiedType());
            } else {
                adoptCell(use, subcell.flatten());
            }
        } else {
            // Check to see if production rules are being inlined.
            // This won't interfere with parsing of old cast, because
            // the old cast parser never sets hasPrsBlock to true.
            if (checkPrs && ((CellImpl) subcell).hasPrsBlock)
                throw new SubcellCreationException("Subcell has production rules, so can only be flattened, not inlined", use, getFullyQualifiedType());

            adoptCell(use, subcell);
        }
    }

    
    public void setAutoInline(boolean autoInline) {
        this.autoInline=autoInline;
    }

    public boolean isAutoInline() {
        return autoInline;
    }

    /**
     * Helper function for adoptCell(); returns a new ProductionRule
     * with nodes canonicalized as specified.
     **/
    private ProductionRule canonicalizeProductionRuleForAdoptCell(
            final HierName subcellName,
            final ProductionRule pr) {
        return new ProductionRule(
                        prefixAndCanonicalizeBooleanExpressionHierNames(
                            pr.getGuard(), subcellName, this),
                        canonicalizeName(
                            HierName.prefixName(subcellName,
                                pr.getTarget()),
                            this),
                        canonicalizeName(
                            HierName.prefixName(subcellName,
                                pr.getPowerSupply()),
                            this),
                        pr.getDirection(),
                        pr.isIsochronic(), pr.isUnstable(), pr.isMetastable(),
                        pr.isTimed(), pr.getAfter(), pr.isAbsolute());
    }

    /**
     * Import directives from a CellInterface for a specified block.
     **/
    private void adoptDirective(final CellInterface model, final String block) {
        final BlockIterator modelPrs = getDirectiveIterator(model, block);

        if (modelPrs.hasNext()) {
            final BlockIterator prsBlock = getDirectiveIterator(this, block);
            final PrsDirective newDir =
                new PrsDirective(block, (DirectiveBlock) modelPrs.next());
            if (prsBlock.hasNext()) {
                prsBlock.next();
                prsBlock.set(newDir);
            } else {
                prsBlock.add(newDir);
            }
        }
    }

    /**
     * Import directives from a CellInterface.  Currently, only PRS and subcell
     * directives.
     **/
    private void adoptDirective(final CellInterface model) {
        adoptDirective(model, BlockInterface.CELL);
        adoptDirective(model, BlockInterface.PRS);
        adoptDirective(model, BlockInterface.SUBCELL);
    }

    /**
     * Imports all production rules, connections, exclusion properties, 
     * and subcells from the specified cell into this cell.
     **/
    private void adoptCell(final HierName subcellName,
                           final CellInterface cell) {
        //    b. for each canonical node:
        //       i. add node and all connected nodes with
        //          usage name prefixed to name
        //    c. (1) add that cell's production rules to this cell,
        //       (2) add that cell's netlist block to this cell,
        //       with the usage name prefixed to each node,
        //       *taking care to use the new canonical node name
        //    d. add that cell's exclhi / excllo to the this cell
        //       *taking care to use the new canonical node name
        //    e. add that cell's subcells, and update the inlined set as
        //       necessary.
        //    f. inline directives from cell

        // b.
        for (final Iterator iCanonicalNode = cell.getCanonicalNodes();
                iCanonicalNode.hasNext(); ) {
            final HierName canonicalNode
                = (HierName) iCanonicalNode.next();
            final HierName subcellCanonicalNode
                = HierName.prefixName(subcellName, canonicalNode);

            // i.
            for (final Iterator iConnectedNode
                    = cell.getConnectedNodes(canonicalNode);
                    iConnectedNode.hasNext(); ) {
                 
                final HierName connectedNode
                    = (HierName) iConnectedNode.next();
                final HierName subcellConnectedNode
                    = HierName.prefixName(subcellName, connectedNode);

                addConnection(subcellCanonicalNode, subcellConnectedNode);
            }
        }

        /**/
        // c.
        // (1) add production rules
        for (final Iterator generalPRS
                = cell.getProductionRuleSet().getProductionRules();
                generalPRS.hasNext(); ) {
            final ProductionRule pr = (ProductionRule) generalPRS.next();

            // * be sure to use canonical name
            
            getProductionRuleSet().addProductionRule(
                canonicalizeProductionRuleForAdoptCell(subcellName, pr));
        }

        for (final Iterator assertedPRS
                = cell.getAssertedProductionRuleSet().getProductionRules();
                assertedPRS.hasNext(); ) {
            final ProductionRule pr = (ProductionRule) assertedPRS.next();

            // * be sure to use canonical name
            
            getAssertedProductionRuleSet().addProductionRule(
                canonicalizeProductionRuleForAdoptCell(subcellName, pr));
        }
        // (2) add netlist block
        final NetlistBlock cellNetlist =
            (NetlistBlock) cell.getBlockInterface()
                               .iterator(BlockInterface.NETLIST).next();
        final Template cellTemplate = cellNetlist.getCDLTemplate();
        if (cellTemplate != null) {
            final NetlistBlock myNetlist =
                (NetlistBlock) blocks.iterator(BlockInterface.NETLIST).next();
            Template myTemplate = myNetlist.getCDLTemplate();
            if (myTemplate == null) {
                myTemplate = new Template(Collections.EMPTY_MAP);
                myNetlist.setCDLTemplate(myTemplate, Collections.EMPTY_MAP);
            }
            cellTemplate.execute(
                new CDLDeviceNameFilter(
                    new CDLNodeNameFilter(myTemplate) {
                        protected HierName processNodeName(HierName node) {
                            return CellImpl.canonicalizeName(
                                HierName.prefixName(subcellName, node),
                                CellImpl.this);
                        }
                    }) {
                    protected HierName processDeviceName(HierName name) {
                        return HierName.prefixName(subcellName, name);
                    }
                }
            );
        }

        // d.
        importExcl(subcellName, cell);
        /**/

        // e.
        for (final Iterator iSubcellPair = cell.getAllSubcellPairs(); 
                iSubcellPair.hasNext(); ) {
            final Pair p = (Pair) iSubcellPair.next();
            final HierName asubcellName = (HierName) p.getFirst();
            final CellInterface asubcell = (CellInterface) p.getSecond();
            final HierName newSubcellName =
                HierName.append(subcellName, asubcellName);

            addSubcellPair(newSubcellName, asubcell, false);
            addPosition(newSubcellName, subcellName, cell, asubcellName);
            if (cell.isInlinedSubcell(asubcellName)) {
                inlinedSubcellSet.add(newSubcellName);
            }
            Debug.assertTrue(getSubcell(newSubcellName) == asubcell);
        }
        addSubcellPair(subcellName, cell, false);
        inlinedSubcellSet.add(subcellName);

        // f.
        createInlineDirectiveBlock(cell, subcellName, BlockInterface.CELL);
        createInlineDirectiveBlock(cell, subcellName, BlockInterface.PRS);
        createInlineDirectiveBlock(cell, subcellName, BlockInterface.SUBCELL);

        if (!cell.isNode()) {
            adoptedSet.add(subcellName);
            for (final Iterator i = ((CellImpl) cell).adoptedSet.iterator();
                 i.hasNext(); ) {
                adoptedSet.add(HierName.append(subcellName, (HierName) i.next()));
            }
        }
    }

    private static BlockIterator getDirectiveIterator(final CellInterface cell,
                                                      final String type) {
        BlockInterface block = cell.getBlockInterface();
        if (!type.equals(BlockInterface.CELL)) {
            block = block.iterator(type).next();
        }
        return block.iterator(BlockInterface.DIRECTIVE);
    }

    private void createInlineDirectiveBlock(final CellInterface cell,
                                            final HierName subcellName,
                                            final String type) {
        final BlockIterator origIt = getDirectiveIterator(cell, type);
        if (origIt.hasNext()) {
            final BlockIterator prsIt = getDirectiveIterator(this, type);
            final PrsDirective prsDir;
            if (prsIt.hasNext()) {
                final BlockInterface bi = prsIt.next();
                prsDir = new PrsDirective(type, (DirectiveBlock) bi);
                prsIt.set(prsDir);
            } else {
                prsDir = new PrsDirective(type);
                prsIt.add(prsDir);
            }
            prsDir.addInstance(subcellName, (DirectiveBlock) origIt.next());
        }
    }

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
    public Iterator getCanonicalNodes() {
        return aliases.getCanonicalKeys();
    }

    /**
     * Returns an iterator through all nodes connected to the given node.
     **/
    public Iterator getConnectedNodes(HierName nodeName) {
        return aliases.getAliases(nodeName);
    }

    /**
     * The canonical name of the given node.
     **/
    public HierName getCanonicalName(HierName nodeName) {
        return (HierName) aliases.getCanonicalKey(nodeName);
    }

    /**
     * Adds a local connection.
     **/
    public void addConnection(final HierName n1, final HierName n2) {
        aliases.makeEquivalent(n1, n2);
    }

    /**
     * Adds the specified node name to the list of known nodes.
     **/
    public void addNode(final HierName nodeName) {
        aliases.add(nodeName);
    }

    /**
     * For debugging only.
     **/
    public String getNamespaceString() {
        return aliases.toString();
    }

    //
    //
    // PRS
    //
    //

    /**
     * Note that the cast cell contained a fragment prs{...} construct.
     **/
    public void setHasFragmentPrsBlock() {
        this.hasPrsBlock = true;
        this.prsBlockIsComplete = false;
    }

    /**
     * Note that the cast cell contained a non-fragment prs{...} construct.
     **/
    public void setHasCompletePrsBlock() {
        this.hasPrsBlock = true;
        this.prsBlockIsComplete = true;
    }

    public boolean containsCompletePrs() {
        return hasPrsBlock && prsBlockIsComplete;
    }

    public boolean containsFragmentPrs() {
        return hasPrsBlock && !prsBlockIsComplete;
    }

    /**
     * Return the production rule set (PRS) specified by the cell.
     * This returns only the general-purpose production rules, those
     * which are eventually going to be turned into silicon.  The
     * production rules which implement asserts (and thus are only for
     * simulation) are accessed via getAssertedProductionRuleSet()
     **/
    public ProductionRuleSet getProductionRuleSet() {
        final PrsBlock prs =
            (PrsBlock) blocks.iterator(BlockInterface.PRS).next();
        return prs.getProductionRuleSet();
    }

    /** true if production rule set is not empty **/
    private boolean hasProductionRules() {
        return getProductionRuleSet().
            getProductionRules().hasNext();
    }

    /**
     * Return the production rule set inferred from the production
     * rule guards stated in the assert block.  These should never see
     * silicon.
     **/
    public ProductionRuleSet getAssertedProductionRuleSet() {
        final AssertBlock assertBlock =
            (AssertBlock) blocks.iterator(BlockInterface.ASSERT).next();
        return assertBlock.getProductionRuleSet();
    }

    //
    //
    // Java block
    //
    //

    /**
     * Return the cosimulation info from the Java block
     **/
    public JavaCoSimInfo getJavaCoSimInfo() {
        return javaCosimInfo;
    }

    /**
     * Fills in the cosimulation information of the specified
     * CoSimInfo from the port list.  May be called more than once
     * with no adverse effect.  Can be called on java or on csp.
     **/
    public void setCoSimInfoFromPorts(final CoSimInfo outCosimInfo) {
        // clear old channels in case we have been called before
        outCosimInfo.clearPortChannels();

        // add channels
        for (final Iterator i = getPortDefinitions(); i.hasNext(); ) {
            final PortDefinition portDef = (PortDefinition) i.next();

            // add all channels specified by this port
            addPortChannelsToCosimInfo(portDef.getName(), portDef.getType(),
                    portDef.getDirection(), outCosimInfo);
        }
    }

    /**
     * Adds all channels specified by the port to the CoSimInfo
     **/
    private void addPortChannelsToCosimInfo
        (String name, final PortTypeInterface type,
         final int direction, final CoSimInfo outCosimInfo) {
        if (type instanceof NodeType) {
            final NodeType nodeType = (NodeType) type;
            if (nodeType.isArrayed()) {
                for (int i = 0; i < nodeType.getWidth(); ++i)
                    addNodeToCoSimInfo(
                            StringUtil.replaceSubstring(name + '[' + i + ']',
                                                        "][", ","),
                            direction, outCosimInfo);
            } else {
                addNodeToCoSimInfo(StringUtil.replaceSubstring(name, "][", ","),
                                   direction, outCosimInfo);
            }
            final String newname =
                StringUtil.replaceSubstring(name, "][", ",");
            outCosimInfo.addWideNode(newname);
            outCosimInfo.addNodeInfo(newname, nodeType.getWidth(),
                                     nodeType.isArrayed(), direction);
        } else if (type instanceof ChannelType) {
            final ChannelType channelType = (ChannelType) type;

            // e1ofN*M
            final BigInteger N = channelType.getNumValues();
            final int M = channelType.getWidth();
            // TODO: give way to control slack
            final int slack = 1;

            // Convert <code>][</code> to <code>,</code>
            final String newname =
                StringUtil.replaceSubstring(name, "][", ",");

            // add channel params; use the real name, so that timing directives
            // can be looked up; for arrayed, width 1 channels, "[0]" will be
            // appended inside CoSimInfo.addChannelInfo to make the name look
            // identical to chanName below.
            addNewChannelToCosimInfo(newname, channelType.getTypeName(), slack,
                                     N, M, channelType.isArrayed(),
                                     outCosimInfo);

            // Fix for Bug 2373.  Add index for "wide" channels of width 1.
            // Do this before converting ][ -> , to get better channel
            // names.
            final String chanName = M == 1 && channelType.isArrayed() ?
                StringUtil.replaceSubstring(name + "[0]", "][", ",") : newname;

            // add channel name
            if (direction == PortDefinition.IN)
                outCosimInfo.addInputChannel(chanName);
            else if (direction == PortDefinition.OUT)
                outCosimInfo.addOutputChannel(chanName);
            else {              // INOUT or NONE
                System.out.println
                    ("WARNING: don't know direction of channel " + 
                     newname + " so not hooking it up for cosimulation");
            }
        } else if (type instanceof ArrayType) {
            final ArrayType arrayType = (ArrayType) type;

            for (int i = arrayType.getMinIndex();
                    i <= arrayType.getMaxIndex(); ++i) {
                addPortChannelsToCosimInfo(name + "[" + i + "]", 
                        arrayType.getArrayedType(), direction, outCosimInfo);
            }
        } else if (type instanceof StructureType) {
            final StructureType structType = (StructureType) type;
            if (CellUtils.isDftChannel(this, structType.getTag())) {
                final String newname =
                    StringUtil.replaceSubstring(name, "][", ",");
                if (direction == PortDefinition.IN)
                    outCosimInfo.addInputDftChannel(newname);
                else if (direction == PortDefinition.OUT)
                    outCosimInfo.addOutputDftChannel(newname);
                else {
                    System.out.println
                        ("WARNING: don't know direction of DFT channel " + 
                         name + " so not hooking it up for cosimulation");
                }
                return;
            }

            for (Iterator i = structType.iterator(); i.hasNext(); ) {
                PortDefinition p = (PortDefinition) i.next();
                int subDirection = p.getDirection();
                String subName = p.getName();
                PortTypeInterface subType = p.getType();

                if ((subDirection == PortDefinition.FORWARD 
                  && direction == PortDefinition.IN)
                 || (subDirection == PortDefinition.REVERSE
                  && direction == PortDefinition.OUT))
                    addPortChannelsToCosimInfo(name + "." + subName,
                            subType, PortDefinition.IN, outCosimInfo);
                else if ((subDirection == PortDefinition.FORWARD
                       && direction == PortDefinition.OUT)
                      || (subDirection == PortDefinition.REVERSE
                       && direction == PortDefinition.IN))
                    addPortChannelsToCosimInfo(name + "." + subName, 
                            subType, PortDefinition.OUT, outCosimInfo);
                else {          // Channel is bidirectional or doesn't have
                                // direction specified
                    System.out.println("WARNING: don't know direction of channel " + subName + " in complex channel " + name + " so not hooking it up for cosimulation");
                }
            }
        } else {
            throw new AssertionFailure("Unrecognized type: " + type);
        }
    }

    private void addNodeToCoSimInfo(final /*@ non_null @*/ String nodeName,
                                    final int direction,
                                    final /*@ non_null @*/ CoSimInfo
                                        outCosimInfo) {
        if (direction == PortDefinition.IN)
            outCosimInfo.addInputNode(nodeName);
        else if (direction == PortDefinition.OUT)
            outCosimInfo.addOutputNode(nodeName);
        else {              // INOUT or NONE
            System.out.println
                ("WARNING: don't know direction of node " + 
                 nodeName + " so not hooking it up for cosimulation");
        }
    }

    public void buildJavaClass(final HierName cellName,
                               final ChannelDictionary cdict,
                               final ClassLoader aLoader,
                               final int arbitrationMode)
        throws ClassNotFoundException, InstantiationException,
                       DeviceConstructionException {

        System.out.println("Getting Java class for: " + cellName);
        if (javaCosimInfo.isOldStyle()) {
            if (javaCosimInfo.getClassName() == null) {
                throw new ClassNotFoundException("no java class set");
            }
            final Class javaClass =
                Class.forName(javaCosimInfo.getClassName(), true, aLoader);
            buildGenericClass(cellName, cdict, javaClass, javaCosimInfo,
                    arbitrationMode);
        }
        else {
            javaCosimInfo.buildClasses(cellName, cdict, aLoader,
                                       arbitrationMode);
        }
    }

    public void buildCSPClass(HierName cellName, ChannelDictionary cdict,
            final int arbitrationMode, boolean emitCoverageProbes) 
                throws ClassNotFoundException, InstantiationException {
        buildCSPClass(cellName, cdict, arbitrationMode, emitCoverageProbes, 1);
    }

    public void buildCSPClass(HierName cellName, ChannelDictionary cdict,
            final int arbitrationMode, boolean emitCoverageProbes,
            final long seed) 
                throws ClassNotFoundException, InstantiationException {
        buildCSPClass(cellName, cdict, arbitrationMode, emitCoverageProbes,
                      seed, 1.0f);
    }

    public void buildCSPClass(HierName cellName, ChannelDictionary cdict,
            final int arbitrationMode, boolean emitCoverageProbes,
            final long seed, final float digitalTau) 
                throws ClassNotFoundException, InstantiationException {
        buildCSPClass(cellName, cdict, arbitrationMode, emitCoverageProbes,
                      null, seed, digitalTau);
    }

    public void buildCSPClass(HierName cellName, ChannelDictionary cdict,
            final int arbitrationMode, boolean emitCoverageProbes,
            CspSnoopingInterface cspSnooper) 
                throws ClassNotFoundException, InstantiationException {
        buildCSPClass(cellName, cdict, arbitrationMode, emitCoverageProbes,
                      cspSnooper, 1);
    }

    public void buildCSPClass(HierName cellName, ChannelDictionary cdict,
            final int arbitrationMode, boolean emitCoverageProbes,
            CspSnoopingInterface cspSnooper, long seed)
                throws ClassNotFoundException, InstantiationException {
        buildCSPClass(cellName, cdict, arbitrationMode, emitCoverageProbes,
                      cspSnooper, seed, 1.0f);
    }

    public void buildCSPClass(HierName cellName, ChannelDictionary cdict,
            final int arbitrationMode, boolean emitCoverageProbes,
            CspSnoopingInterface cspSnooper, long seed, float digitalTau)
                throws ClassNotFoundException, InstantiationException {
        try {
            if (getCspBlock().getCSPClass(emitCoverageProbes) == null) {
                final PrintWriter myNullPrintWriter = 
                    new PrintWriter( NullWriter.getInstance() );
                final PrintWriter myStdOutputWriter = 
                    new PrintWriter( new OutputStreamWriter( System.out ) );
                getCspBlock().setCSPClass(
                    new CSP2Class(myNullPrintWriter, myStdOutputWriter, 
                         myNullPrintWriter, emitCoverageProbes,
                                             cspSnooper != null)
                    .compile( this ),
                    emitCoverageProbes);
            }
        } catch (Exception e) {
            throw (InstantiationException)
                new InstantiationException (e.toString()).initCause(e);
        }
        buildGenericClass(cellName, cdict,
                          getCspBlock().getCSPClass(emitCoverageProbes),
                          getCSPCoSimInfo(), arbitrationMode, cspSnooper,
                          seed, digitalTau);
    }

    private void buildGenericClass(HierName cellName, ChannelDictionary cdict,
            Class cellClass, CoSimInfo cosimInfo, final int arbitrationMode) 
            throws ClassNotFoundException, InstantiationException {
        buildGenericClass(cellName, cdict, cellClass, cosimInfo,
                arbitrationMode, null);
    }

    private void buildGenericClass(HierName cellName, ChannelDictionary cdict,
            Class cellClass, CoSimInfo cosimInfo, final int arbitrationMode,
            final CspSnoopingInterface cspSnooper) 
            throws ClassNotFoundException, InstantiationException {
        buildGenericClass(cellName, cdict, cellClass, cosimInfo,
                          arbitrationMode, cspSnooper, 1);
    }

    private void buildGenericClass(HierName cellName, ChannelDictionary cdict,
            Class cellClass, CoSimInfo cosimInfo, final int arbitrationMode,
            final CspSnoopingInterface cspSnooper, final long seed) 
            throws ClassNotFoundException, InstantiationException {
        buildGenericClass(cellName, cdict, cellClass, cosimInfo,
                          arbitrationMode, cspSnooper, 1, 1.0f);
    }

    // TODO kwallmar: use DeviceConstructionException as an
    // appropriate generic wrapper exception
    private void buildGenericClass(HierName cellName, ChannelDictionary cdict,
            Class cellClass, CoSimInfo cosimInfo, final int arbitrationMode,
            final CspSnoopingInterface cspSnooper, final long seed,
            final float digitalTau) 
            throws ClassNotFoundException, InstantiationException {
        String cellClassName = cellClass.getName();
        /* Build array of input channels */
        final ChannelInput[] cin = cosimInfo.getInputChannelArray(cdict);

        /* Build array of output channels */
        ChannelOutput[] cout = cosimInfo.getOutputChannelArray(cdict);

        /* Build array of nodes */
        final WideNode[] nodes = cosimInfo.getWideNodeArray(cdict);

        final DeviceParameters params =
            new DeviceParameters(cellName.getAsString('.'),
                    false,
                    (MetaParamDefinition[])
                        metaParamDefinitions.toArray(
                             new MetaParamDefinition[0]),
                    arbitrationMode,
                    seed,
                    digitalTau);

        final Class[] argumentClasses;
        final Object[] arguments;
        if (cspSnooper != null) {
            argumentClasses = new Class[] { DeviceParameters.class,
                                            cin.getClass(), cout.getClass(),
                                            nodes.getClass(),
                                            CspSnoopingInterface.class };
            arguments = new Object[] { params, cin, cout, nodes, cspSnooper };
        } else {
            argumentClasses = new Class[] { DeviceParameters.class,
                                            cin.getClass(), cout.getClass(),
                                            nodes.getClass() };
            arguments = new Object[] { params, cin, cout, nodes };
        }

        final Constructor cnstr;
        try {
            cnstr = cellClass.getConstructor(argumentClasses);
        } catch (NoSuchMethodException e) {
            throw (InstantiationException)
                new InstantiationException("Instantiation of java class " +
                        cellClassName + " failed, constructor not found")
                    .initCause(e);
        }

        try {
            ((Startable) cnstr.newInstance(arguments)).start();
        } catch (Exception e) {
            e.printStackTrace(System.out);
            throw (InstantiationException)
                new InstantiationException ("Instantiation of java class " 
                        + cellClassName + " failed: " + e.toString())
                    .initCause(e);
        }
    }

    public void setJavaClass(String jclass) {
        javaCosimInfo.setClassName(jclass);
        System.err.println("Set java class name to " +
                           javaCosimInfo.getClassName());
    }

    /** Affects the java block information. **/
    public void setInputChannel(String incn) {
        final String newChanName = addNewChannelToCosimInfo(incn, javaCosimInfo);
        // Interim hack to ignore 1ofN's and _1ofN's
        if (newChanName != null)
            javaCosimInfo.addInputChannel(newChanName);
    }

    /** Affects the java block information. **/
    public void setOutputChannel(String outcn) {
        final String newChanName = addNewChannelToCosimInfo(outcn, javaCosimInfo);
        // Interim hack to ignore 1ofN's and _1ofN's
        if (newChanName != null)
            javaCosimInfo.addOutputChannel(newChanName);
    }

    /** Affects the java block information. **/
    public void setInternalChannel(String intcn) {
        final String newChanName = addNewChannelToCosimInfo(intcn, javaCosimInfo);
        // Interim hack to ignore 1ofN's and _1ofN's
        if (newChanName != null)
            javaCosimInfo.addInternalChannel(newChanName);
    }

    private String getE1ofType(final int N) {
        return "standard.channel.e1of(" + N + ")";
    }

    /** Adds the channel and parameters specified by <code>spec</code>
     *  to <code>outCosimInfo.chanParams</code> and returns the name
     *  of the new channel.
     **/
    private String addNewChannelToCosimInfo(String spec, final CoSimInfo outCosimInfo) {
        ChannelTokenizer strtok = new ChannelTokenizer(spec);
        final String name = strtok.nextToken();
        if (!strtok.hasMoreTokens()) {
            // No spec, just name.  Assume e1of2  slack 0  
            addNewChannelToCosimInfo(name, getE1ofType(2), 0, 2, 1, false, outCosimInfo);
            return name;
        }
        if (!strtok.nextToken().equals("(")) {
            System.out.println("Bad channel spec");
            throw new BadChannelSpec(name);
        }
        String buf = strtok.nextToken();
        // Interim hack to ignore 1ofN's and _1ofN's
        if (buf.startsWith("standard.channel.1of") ||
            buf.startsWith("standard.channel._1of"))
            return null;
        final int N = extractN(buf);
        final boolean arrayed;
        buf = strtok.nextToken();
        int M;
        if (buf.equals("*")) {
            M = Integer.parseInt(strtok.nextToken());
            buf = strtok.nextToken();
            arrayed = true;
        } else {
            M = 1;
            arrayed = false;
        }
        int slack = 0;
        if (!buf.equals(")")) {
            if (!buf.equals("slack"))
                throw new BadChannelSpec(name);
            buf = strtok.nextToken();
            slack = Integer.parseInt(buf);
            if (!strtok.nextToken().equals(")"))
                throw new BadChannelSpec(name);
        }
        if (strtok.hasMoreTokens()) {
            throw new BadChannelSpec(name);
        }

        addNewChannelToCosimInfo(name, getE1ofType(N), slack, N, M, arrayed, outCosimInfo);
        return name;
    }

    /**
     * Returns N from a string of the form standard.channel.e1ofN.
     **/
    private int extractN(final String channelType) {
        final int result = CellUtils.extractN(channelType);
        if (result == -1) throw new BadChannelSpec(channelType);
        else return result;
    }

    private void addNewChannelToCosimInfo
        (final String name, final String type, final int slack,
         final int N, final int M, final boolean isArrayed,
         final CoSimInfo cosimInfo) {
        addNewChannelToCosimInfo(name, type, slack, BigInteger.valueOf(N), M,
                                 isArrayed, cosimInfo);
    }

    private void addNewChannelToCosimInfo
        (final String name, final String type, final int slack,
         final BigInteger N, final int M, final boolean isArrayed,
         final CoSimInfo cosimInfo) {
        // System.err.println("Adding channel " + name + " with parameters " +
        //         "e1of" + N + " * " + M + "  slack " + slack);
        cosimInfo.addChannelInfo(name, type, slack, N, M, isArrayed);
    }

    class BadChannelSpec extends RuntimeException {
        public BadChannelSpec(String foo) { super(foo); }
    }

    class ChannelTokenizer {
        private final StringTokenizer instrtok;
        private String upcomingToken;
        public ChannelTokenizer(String str) {
            instrtok = new StringTokenizer(str, " ()*", true);
            upcomingToken = getNextToken();
        } 

        private String getNextToken() {
            String rv = " ";
            while (instrtok.hasMoreTokens() && 
                        (rv=instrtok.nextToken()).equals(" "))
                { }
            if (rv.equals(" "))
                return null;
            else 
                return rv;
        }

        public boolean hasMoreTokens() {
            return (upcomingToken != null);
        }

        public String nextToken() {
            final String rv = upcomingToken;
            upcomingToken = getNextToken();
            return rv;
        }
    }

    //
    //
    // CSP stuff
    //
    //

    private CspBlock getCspBlock() {
        return (CspBlock) blocks.iterator(BlockInterface.CSP).next();
    }

    /** Return the cosimulation info from the csp block. **/
    public CSPCoSimInfo getCSPCoSimInfo() {
        return getCspBlock().getCSPCoSimInfo();
    }

    /**
     * Returns the CSP body, or null if none.
     **/
    public CSPProgram getCSPProgram() {
        //System.out.println("Getting csp body " + ((csp == null) ? "null" : "not null") + " in object " + this);
        return getCspBlock().getCSPProgram();
    }

    /**
     * Sets the CSP body.
     **/
    public void setCSPProgram(final CSPProgram csp) {
        //System.out.println("Setting csp body to " + ((csp == null)? "null" : "not null") + " in object " + this);
        getCspBlock().setCSPProgram(csp);
    }

    /**
     * Returns as much non-csp info about the cell as the csp
     * processing is going to need (ports/metas information).
     **/
    public CSPCellInfo getCSPInfo() {
        final Collection<PortDefinition> ports;
        if (getCspBlock().getCSPPorts().isEmpty()) {
            ports = portDefinitions.values();
        } else {
            // Construct the list of port definitions for the csp block
            ports = new ArrayList/*<PortDefinition>*/();
            for (final Iterator i = getCspBlock().getCSPPorts().iterator();
                 i.hasNext(); ) {
                final String name = (String) i.next();
                final PortDefinition port = portDefinitions.get(name);
                Debug.assertTrue(port != null, "couldn't find port " + port + " anymore");
                ports.add(portDefinitions.get(name));
            }
        }
        // Should this be cached?
        return new CSPCellInfo(getFullyQualifiedType(), metaParamDefinitions, ports);
    }

    /**
     * Takes the name of a port for the csp block.  If this function
     * is never called, this cell passes all its ports to the
     * csp-&gt;java compiler.
     **/
    public void addCSPPort(String portName)
        throws BadCSPPortException {
        if (portDefinitions.get(portName) == null)
            throw new BadCSPPortException(getFullyQualifiedType(), portName);

        getCspBlock().getCSPPorts().add(portName);
    }

    //
    //
    // Spec stuff: exclhi/excllo
    //
    //

    public ExclusiveNodeSets getLocalExclusiveNodeSets() {
        final AssertBlock assertBlock =
            (AssertBlock) blocks.iterator(BlockInterface.ASSERT).next();
        return assertBlock.getExclusiveNodeSets();
    }

    /**
     * @deprecated Use getExclusiveNodeSets().addExclusiveSet(ens)
     **/
//      public void addExclusiveNodeSet(final ExclusiveNodeSet ens) {
//          getExclusiveNodeSets().addExclusiveNodeSet(ens);
//      }

    //
    //
    // Flatten
    //
    //

    /**
     * Returns a flattened version of the cell.  The flattened version 
     * has no subcells.
     * <p>
     * Flattening can cause globals to appear that were not previously
     * there (duh).
     **/
    public CellInterface flatten() {
        return flatten(Integer.MAX_VALUE);
    }

    /**
     * Returns a flattened version of the cell.  The flattened version 
     * has no subcells.
     * <p>
     * Flattening can cause globals to appear that were not previously
     * there (duh).
     *
     * @param maxPrsDepth  the maximum depth from which production rules
     *   should be imported.  Zero means only the current cell.
     *   Negative means none at all.
     **/
    public CellInterface flatten(final int maxPrsDepth) {
        return flatten(maxPrsDepth, new TreeMap());
    }

    private ProductionRule canonicalizeProductionRuleForFlatten(
            final CellImpl flatCell,
            final ProductionRule pr) {
        return new ProductionRule(
                            canonicalizeBooleanExpressionHierNames(
                                pr.getGuard(), flatCell),
                            canonicalizeName(pr.getTarget(), flatCell),
                            canonicalizeName(pr.getPowerSupply(), flatCell),
                            pr.getDirection(),
                            pr.isIsochronic(), pr.isUnstable(),
                            pr.isMetastable(), pr.isTimed(),
                            pr.getAfter(), pr.isAbsolute());
    }

    // Suffix used to create new "flat" version of the cell.
    private static String FLAT_SUFFIX = "$FLAT";

    /**
     * Use the cache, Luke!
     *
     * @param flattenedCellCache  cache of cells that have been
     *   flattened
     **/
    private CellInterface flatten(final int maxPrsDepth,
                                  final Map/*<String,CellInterface>*/
                                      flattenedCellCache) {
        // A node or a flat cell cannot be flattened further
        if (isNode() || getType().endsWith(FLAT_SUFFIX)) return this;

        // use the cached version if we have one
        final CellInterface cachedCell
            = (CellInterface) flattenedCellCache.get(getFullyQualifiedType() + FLAT_SUFFIX);
        if (cachedCell != null)
            return cachedCell;

        final CellImpl flatCell =
            new CellImpl(getType() + FLAT_SUFFIX, moduleName);

        // 1. for each canonical node in this cell:
        //    a. add all connected nodes
        // 1.5 inherit directives
        // 2. for each subcell:
        //    a. flatten it
        //    b. for each canonical node:
        //       i. add node and all connected nodes with
        //          usage name prefixed to name
        //    c. add that cell's production rules to the flat cell,
        //       with the usage name prefixed to each node,
        //       *taking care to use the new canonical node name
        //    d. add that cell's exclhi / excllo to the flattened cell
        //       *taking care to use the new canonical node name
        // 3. Add all production rules from this cell,
        //    *taking care to use the canonical name for each node
        // 4. import the exclhi / excllo statements from this cell
        //    *taking care to use the canonical name for each node

        // 1.
        for (final Iterator iCanonicalNode = getCanonicalNodes();
                iCanonicalNode.hasNext(); ) {
            final HierName canonicalNode = (HierName) iCanonicalNode.next();
            
            // a.
            for (final Iterator iConnectedNode
                    = getConnectedNodes(canonicalNode);
                    iConnectedNode.hasNext(); ) {
                final HierName connectedNode
                    = (HierName) iConnectedNode.next();

                flatCell.addConnection(canonicalNode, connectedNode);
            }
        }

        // 1.5
        flatCell.adoptDirective(this);

        // 2.
        for (final Iterator iSubcellPair = getAllSubcellPairs();
                iSubcellPair.hasNext(); ) {
            final Pair pair = (Pair) iSubcellPair.next();

            final HierName subcellName = (HierName) pair.getFirst();
            if (!adoptedSet.contains(subcellName)) {
                // a.
                final CellInterface subcell
                    = ((CellImpl) pair.getSecond())
                        .flatten(maxPrsDepth - 1, flattenedCellCache);

                // b. c. d.
                flatCell.adoptCell(subcellName, subcell);
            }
        }

        /**/
        // 3.
        if (maxPrsDepth >= 0) {
            for (final Iterator generalPRS
                    = getProductionRuleSet().getProductionRules();
                    generalPRS.hasNext(); ) {
                final ProductionRule pr = (ProductionRule) generalPRS.next();

                // * be sure to use canonical name

                flatCell.getProductionRuleSet().addProductionRule(
                    canonicalizeProductionRuleForFlatten(flatCell, pr));
            }

            for (final Iterator assertedPRS
                    = getAssertedProductionRuleSet().getProductionRules();
                    assertedPRS.hasNext(); ) {
                final ProductionRule pr = (ProductionRule) assertedPRS.next();

                // * be sure to use canonical name

                flatCell.getAssertedProductionRuleSet().addProductionRule(
                    canonicalizeProductionRuleForFlatten(flatCell, pr));
            }

        }

        // 4.
        flatCell.importExcl(null, this);
        /**/

        // save the flattened cell in the cache
        /** @bug jmr this is cachine regardless of maxPrsDepth!!!
         * the wrong thing may be cached.  We probably want to 
         * get rid of maxPrsDepth, considering we only pass 1 and
         * Integer.MAX_VALUE **/
        flattenedCellCache.put(getFullyQualifiedType() + FLAT_SUFFIX, flatCell);

        return flatCell;
    }

    /**
     * Import the exclhi / excllo declarations of the given cell
     * into <code>this<code>.  If <code>subcellName</code> is
     * non-<code>null</code>, then prefix the names of the nodes
     * in the excllo / exclhi sets with it.
     **/
    private void importExcl(final HierName subcellName,
                            final CellInterface cell) {
        for (final Iterator iENS = cell.getLocalExclusiveNodeSets().getIterator();
                iENS.hasNext(); ) {
            final ExclusiveNodeSet ens = (ExclusiveNodeSet) iENS.next();
            final ArrayList l = new ArrayList();

            // canonicalize all the nodes in the exclusion set
            for (final Iterator iExclNode = ens.getNodes();
                    iExclNode.hasNext(); ) {
                final HierName hn = (HierName) iExclNode.next();
                final HierName hn2;
                if (subcellName == null)
                    hn2 = hn;
                else
                    hn2 = HierName.prefixName(subcellName, hn);
                l.add(canonicalizeName(hn2, this));
            }

            getLocalExclusiveNodeSets().addExclusiveNodeSet(
                    new ExclusiveNodeSet(ens.getHiLo(), l));
        }
    }

    /**
     * Recurses through the BooleanExpressionInterface, returning
     * a new BooleanExpressionInterface with all HierNames prefixed by
     * the specified <code>subcellName</code>, unless the name is a global
     * name, in which case it will be passed through unchanged.
     **/
    private static BooleanExpressionInterface
        canonicalizeBooleanExpressionHierNames(
                final BooleanExpressionInterface be,
                final CellInterface cell) {
        return BooleanUtils.mapBooleanExpressionHierNames(be,
                new UnaryFunction() {
                    public Object execute(final Object o) {
                        return canonicalizeName((HierName) o, cell);
                    }
                });

    }

    /**
     * Recurses through the BooleanExpressionInterface, returning
     * a new BooleanExpressionInterface with all HierNames prefixed by
     * the specified <code>subcellName</code>, unless the name is a global
     * name, in which case it will be passed through unchanged.
     **/
    private static BooleanExpressionInterface
        prefixAndCanonicalizeBooleanExpressionHierNames(
                final BooleanExpressionInterface be,
                final HierName subcellName,
                final CellInterface cell) {
        return BooleanUtils.mapBooleanExpressionHierNames(be,
                new UnaryFunction() {
                    public Object execute(final Object o) {
                        return canonicalizeName(
                            HierName.prefixName(subcellName,
                                (HierName) o), cell);
                    }
                });

    }

    /**
     * Returns the canonical name of <code>name</code> in <code>cell</code>,
     * or, if the name isn't known to cell, return <code>name</code>.
     **/
    private static HierName canonicalizeName(final HierName name,
                                             final CellInterface cell) {
        final HierName canonName = cell.getCanonicalName(name);
        if (canonName == null)
            return name;
        else
            return canonName;
    }

    // smartFlatten
        // flatten could be done more intelligently, not allocating
        // much new data, but providing a structure with more
        // intelligent iterators

    //
    //
    // Env block
    //
    //

    /**
     * Used for setting as well as getting the named environments of this cell.
     **/
    public EnvBlock getEnvironments() {
        return (EnvBlock) blocks.iterator(BlockInterface.ENV).next();
    }


    //
    //
    // Misc
    //
    //

    /**
     * Returns true if this cell or any of its subcells have non-
     * env production rules.
     **/
    public boolean hasRealProductionRule() {
        // check the prs of this cell, returning true if a non-env
        // pr appears
        for (final Iterator iPr
                = getProductionRuleSet().getProductionRules();
                iPr.hasNext(); ) {
            final ProductionRule pr = (ProductionRule) iPr.next();
            return true;
        }

        // if any of its subcells have a real production rule,
        // then this one does, so return true.
        // TODO: this could be improved by iterating over the
        // types of subcells, rather than the names, and
        // further improved by caching the results.
        for (final Iterator iSubcellPair = getSubcellPairs();
                iSubcellPair.hasNext(); ) {
            final Pair pair = (Pair) iSubcellPair.next();
            final CellInterface subcell = ((CellImpl) pair.getSecond());

            if (subcell.hasRealProductionRule())
                return true;
        }

        // otherwise, return false, no real production rules
        return false;
    }

    public boolean hasNetlistBody() {
        // Should refactor with hasRealProductionRule above
        if (containsNetlist()) return true;
        for (final Iterator iSubcellPair = getSubcellPairs();
                iSubcellPair.hasNext(); ) {
            final Pair pair = (Pair) iSubcellPair.next();
            final CellInterface subcell = ((CellImpl) pair.getSecond());

            if (subcell.hasNetlistBody())
                return true;
        }

        return false;
    }

    public boolean hasVerilog() {
        // Should refactor with hasRealProductionRule above
        if (containsVerilog()) return true;
        for (final Iterator iSubcellPair = getSubcellPairs();
                iSubcellPair.hasNext(); ) {
            final Pair pair = (Pair) iSubcellPair.next();
            final CellInterface subcell = ((CellImpl) pair.getSecond());

            if (subcell.hasVerilog())
                return true;
        }

        return false;
    }

    public boolean hasRunnableCsp() {
        // Should refactor with hasRealProductionRule above
        if (containsRunnableCsp()) return true;
        for (final Iterator iSubcellPair = getSubcellPairs();
                iSubcellPair.hasNext(); ) {
            final Pair pair = (Pair) iSubcellPair.next();
            final CellInterface subcell = ((CellImpl) pair.getSecond());

            if (subcell.hasRunnableCsp())
                return true;
        }

        return false;
    }

    public BlockInterface getBlockInterface() {
        return blocks;
    }

    // Refinement stuff

    /**
     * Helper function for setRefinementParent().  Handles the
     * inheritance of subcells from the parent when there are subtypes
     * to deal with.
     **/
    private void refineSubcellsUsingSubtypes(CellImpl parent)
        throws RefinementException {
        // The subtype statements which referred to arrays can't be
        // removed until all of the array elements have passed by.
        // Temporary storage to remember to remove them at the end.
        Set/*<HierName>*/ arrayedSubtypeSet = new HashSet();

        // Need to check each cell to see if it's been subtyped.
        for (Iterator parentSubcells = parent.subcellMap.keySet().iterator();
             parentSubcells.hasNext();
            ) {
            final HierName cellName = (HierName) parentSubcells.next();
            final HierName baseCellName;
            final Triplet subtypeInfo;
            if (subtypeMap.containsKey(cellName)) {
                baseCellName = null;
                subtypeInfo = (Triplet) subtypeMap.get(cellName);
            } else {
                baseCellName = cellName.getArrayBase();
                subtypeInfo = (Triplet) subtypeMap.get(baseCellName);
            }
            if (subtypeInfo != null) {
                // Can't subtype flattened or inlined cells
                if (inlinedSubcellSet.contains(cellName)) {
                    throw new RefinementException("can't subtype flattened/inlined cell " + cellName + " in this revision of cast parsing",
                            getFullyQualifiedType(),
                            parent.getFullyQualifiedType());
                }
                
                // The specified parent type and actual parent
                // type of the subtyped subcell should match.
                String specifiedParentType =
                    ((CellImpl)subtypeInfo.getFirst())
                        .getFullyQualifiedType();
                String actualParentType =
                    ((CellImpl)parent.subcellMap.get(cellName))
                        .getFullyQualifiedType();
                
                if (! actualParentType.equals(specifiedParentType)) {
                    throw new RefinementException("can't subtype " + cellName + " of type " + actualParentType + " as a cell of type " + specifiedParentType,
                            getFullyQualifiedType(),
                            parent.getFullyQualifiedType());
                }
                
                subcellMap.put(cellName, subtypeInfo.getSecond());
                // If the newly refined subcell should be inlined.
                if (((Boolean) subtypeInfo.getThird()).booleanValue()) {
                    try {
                        inlineSubcell(cellName);
                    } catch (NoSuchSubcellException e) {
                        throw new RefinementException("Error inlining refined subcell: " + e.getMessage(), getFullyQualifiedType(),
                                parent.getFullyQualifiedType(), e);
                    } catch (SubcellCreationException e) {
                        throw new RefinementException("Error inlining refined subcell: " + e.getMessage(), getFullyQualifiedType(),
                                parent.getFullyQualifiedType(), e);
                    }
                }
                if (baseCellName == null) {
                    subtypeMap.remove(cellName);
                } else {
                    arrayedSubtypeSet.add(baseCellName);
                }
            }
            else {
                // Implicitly subtype port subcells; port compatiblity checked
                // in CastTwoTree.g
                if (!isPortSubcell(cellName))
                    subcellMap.put(cellName, parent.subcellMap.get(cellName));
            }
        }

        // Remove the subtype statements that referenced arrays.
        Iterator iter = arrayedSubtypeSet.iterator();
        while (iter.hasNext()) {
            subtypeMap.remove(iter.next());
        }
    }

    /**
     * Copies all node subcells from the parent cell to the child cell.
     **/
    public void inheritNodes(final CellImpl parent) {
        final Map/*<HierName,CellInterface>*/ childMap = subcellMap;
        final Map/*<HierName,CellInterface>*/ parentMap = parent.subcellMap;
        final Iterator parentEntries = parentMap.entrySet().iterator();
        while (parentEntries.hasNext()) {
            final Entry parentEntry = (Entry) parentEntries.next();
            final HierName n = (HierName) parentEntry.getKey();
            final CellImpl c = (CellImpl) parentEntry.getValue();
            if (c.isNode() && !childMap.containsKey(n))
                childMap.put(n, c);
        }
    }

    /**
     * This turns this cell into a refinement of the parent cell.  It
     * doesn't handle comparing the new ports to the old ports; that
     * needs to be handled at the UserDefinedValue or baseType level,
     * when the ports are still in order.
     *
     * <p>It changes this cell but not the parent cell.  They do share
     * some data storage now.
     *
     * <p>Refinement behavior is not defined on any cell which
     * produces compile warnings about deprecated features.
     **/

    public void setRefinementParent (CellImpl parent, boolean portsMatched,
                                     CastParsingOption opt)
            throws RefinementException {
        doInheritanceOrRefinement(parent, true, portsMatched, opt);
    }

    /**
     * Handles work of &lt;: refinement or &lt;+ inheritance.  Called by
     * <code>{@link setRefinementParent()}</code> and
     * <code>{@link setInheritance()}</code>.  Copies blocks (prs, subcells,
     * ...), directives, nodes, etc. from <code>parent</code> to
     * <code>this</code>.  
     *
     * @param parent Cell to inherit attributes from.
     * @param isRefinement If true, <code>directRefinementParent</code>
     *     will be set to the fully qualified type of parent.
     * @param portsMatched If true, and <code>isRefinement</code> is true,
     *     then set <code>refinementAncestor</code> to the
     *     <code>parent</code>'s ancestor.  Has no effect if
     *     <code>isRefinement</code> is false.
     **/
    private void doInheritanceOrRefinement(CellImpl parent,
                                           boolean isRefinement,
                                           boolean portsMatched,
                                           CastParsingOption opt)
        throws RefinementException {

        // To keep track of inherited behavior without confusing it
        // with specified behavior.
        boolean futureHasPrsBlock = hasPrsBlock;
        boolean futurePrsBlockIsComplete = prsBlockIsComplete;
        boolean futureHasSubcellsBlock = hasSubcellsBlock;

        // Keep track of what cells have already been adopted
        adoptedSet.addAll(parent.adoptedSet);

        // this needs to be done before blocks.refineFrom is called,
        // so we can tell whether or not our cell defines its own
        // production rules
        if (parent.hasPrsBlock && hasPrsBlock && parent.prsBlockIsComplete
            && hasProductionRules()) {
            // if parent has a complete prs block, we are not
            // allowed to have one.
            throw new RefinementException
                ("can't use prs with non-empty rule set to refine a cell with"+
                 "non-fragment prs",
                 getFullyQualifiedType(),
                 parent.getFullyQualifiedType());
        } 

        if (subcellsBlockIsComplete == null)
            subcellsBlockIsComplete = parent.subcellsBlockIsComplete;

        // Inherit the auto-inline property of the parent cell
        setAutoInline(isAutoInline() || parent.isAutoInline());

        blocks.refineFrom(parent.blocks);

        instancePositions.putAll(parent.instancePositions);

        // Merge parent and child aliases.
        Namespace.addNamespace(this.aliases,parent.aliases);

        /** @see bug 1478 **/ 
        if (containsNetlist() && (hasSubcellsBlock || hasSubtypesBlock)) {
            throw new RefinementException
                ("Cells with netlist blocks may not define a "+
                 "subcell or subtypes block, or inherit from a "+
                 "cell which defines one",
                 getFullyQualifiedType(),
                 parent.getFullyQualifiedType());
        }

        // Assert blocks handled by blocks

        // CSP block handled by blocks

        if (isRefinement)
            this.directRefinementParent = parent;

        // Exclusive node sets are handled by assert block

        if (hasPrsBlock || hasSubcellsBlock) {
            // prs blocks are additive, so copy parental inlined
            // subcells
            inlinedSubcellSet.addAll(parent.inlinedSubcellSet);
        } else if (!hasSubcellsBlock) {
            // if no prs and no subcells, copy from parent
            inlinedSubcellSet.clear();
            inlinedSubcellSet.addAll(parent.inlinedSubcellSet);
        }
        // subcells blocks replace, so if we have a subcells block,
        // leave inlined subcells as ours

        // java block overrides
        javaCosimInfo.refineFrom(parent.javaCosimInfo);

        // module name can't be inherited.

        // environments are handled by blocks

        // Either the ports match up exactly, or the parent's port
        // list is empty.  In either case, port definitions and port
        // subcells don't need to change.

        if (isRefinement && portsMatched) {
            refinementAncestor = parent.refinementAncestor;
        }

        // prs blocks merge
        if (parent.hasPrsBlock) {
            if (hasSubcellsBlock || hasSubtypesBlock) {
                throw new RefinementException
                    ("can't use subcells to refine a cell with prs", 
                     getFullyQualifiedType(), 
                     parent.getFullyQualifiedType());
            } 

            // actual merging handled by blocks

            // subcellMap has internal nodes
            fillInMap(subcellMap, parent.subcellMap);
            futureHasPrsBlock = true;


            // else parent is fragment, child is fragment or complete,
            // so child remains as is

            // no prs block, so copy completeness from parent
            if (!hasPrsBlock) {
                futurePrsBlockIsComplete = parent.prsBlockIsComplete;
            }
        }

        // subtypes will change the cell interfaces bound to
        // particular names, as well as mucking with aliases.
        else if (parent.hasSubcellsBlock) {
            if (hasPrsBlock) {
                throw new RefinementException
                    ("can't use prs to refine a cell with subcells", 
                     getFullyQualifiedType(), 
                     parent.getFullyQualifiedType());
            } else if (parent.containsCompleteSubcells() && hasSubcellsBlock) {
                throw new RefinementException
                    ("can't use subcells to refine a cell with complete " +
                     "subcells", 
                     getFullyQualifiedType(), 
                     parent.getFullyQualifiedType());
            } else if (parent.containsFragmentSubcells() && hasSubtypesBlock) {
                throw new RefinementException
                    ("can't use subtypes to refine a cell with fragment " +
                     "subcells", 
                     getFullyQualifiedType(), 
                     parent.getFullyQualifiedType());
            } else {
                futureHasSubcellsBlock = true;
                if (! hasSubtypesBlock) {
                    fillInMap(subcellMap, parent.subcellMap);
                }
                else { // Has subtypes
                    refineSubcellsUsingSubtypes(parent);
                }
            }
        }

        else { // Neither prs nor subcells in the parent, but it might
               // have aliases that need to be imported.
            fillInMap(subcellMap, parent.subcellMap);
        }

        // If inline processing has been turned off, any instances that come
        // from inlining would remain in subtypeMap.  Attempt to remove those
        // references from subtypeMap to prevent a fatal error.
        if (isRefinement && !subtypeMap.isEmpty() && opt != null) {
            final Set<HierName> pending = new HashSet<HierName>();
            for (Iterator i = subtypeMap.keySet().iterator(); i.hasNext(); ) {
                final HierName inst = (HierName) i.next();
                HierName name = inst;
                while (name != null && !subcellMap.containsKey(name)) {
                    name = name.getParent();
                }
                if (name != null) {
                    final CellInterface subcell =
                        (CellInterface) subcellMap.get(name);
                    if (!opt.processInline(subcell)) {
                        pending.add(inst);
                        if (!refinementWarned) {
                            refinementWarned = true;
                            System.out.println(
                                "WARNING: " + getFullyQualifiedType() +
                                " subtypes inlined subcells, but inline" +
                                " processing is turned off.");
                        }
                    }
                }
            }
            for (HierName inst : pending) subtypeMap.remove(inst);
        }

        // All subtypes should refer to actual subcells.
        if (isRefinement && !subtypeMap.isEmpty()) {
            throw new RefinementException
                ("not all subtypes refer to actual subcells.  "+
                 "Subtypes left over: " + 
                 getSubtypesString(subtypeMap),
                 getFullyQualifiedType(), parent.getFullyQualifiedType());
        }
        
        // type can't be inherited 

        hasSubcellsBlock = futureHasSubcellsBlock;
        hasPrsBlock = futureHasPrsBlock;
        prsBlockIsComplete = futurePrsBlockIsComplete;
    }

    /**
     * Called by the tree parser to setup inheritance via &lt;+.
     * @param parent Cell to inherit attributes from.
     **/
    public void setInheritance(final CellImpl parent)
            throws RefinementException {
        inheritedCells.add(parent);
        doInheritanceOrRefinement(parent, false, false, null);
    }

    private static String getSubtypesString(
            final Map/*<HierName,
                        Triplet<CellInterface,CellInterface,Boolean>>*/
                subtypeMap) {
        final StringBuffer sb = new StringBuffer();

        sb.append('[');
        boolean first = true;
        for (final Iterator i = subtypeMap.entrySet().iterator();
             i.hasNext(); ) {
            final Entry e = (Entry) i.next();
            final HierName subcellName = (HierName) e.getKey();
            final Triplet t = (Triplet) e.getValue();
            final CellInterface parent = (CellInterface) t.getFirst();
            final CellInterface child = (CellInterface) t.getSecond();

            if (!first)
                sb.append(", ");
            first = false;

            sb.append(parent.getFullyQualifiedType());
            sb.append(" :> ");
            sb.append(child.getFullyQualifiedType());
            sb.append(" ");
            sb.append(subcellName.getAsString('.'));
        }
        sb.append(']');

        return sb.toString();
    }

    public boolean oldSameRefinementAncestor(CellInterface c) {
        return ((CellImpl) c).refinementAncestor.equals(refinementAncestor);
    }

    public boolean sameRefinementAncestor(CellInterface c) {
        final String ancestor = ((CellImpl) c).refinementAncestor;
        while (c != null) {
            if (c == this) {
                return true;
            } else if (ancestor.equals(c.getFullyQualifiedType())) {
                return false;
            } else {
                c = c.getDirectRefinementParent();
            }
        }
        return false;
    }

    /**
     * Returns true if this is cell or refined from cell or refined from a
     * CellInterface which is refined from cell or ....
     **/
    public boolean eventuallyRefinesFrom(CellInterface cell) {
        CellImpl otherCell = (CellImpl) cell;
        CellImpl temp = this;
        while (temp != null) {
            if (temp.equals(otherCell)) return true;
            temp = temp.directRefinementParent;
        }
        return false;
    }

    /**
     * accessor for directRefinementParent
     */
    public CellInterface getDirectRefinementParent(){
        return (CellInterface)this.directRefinementParent;
    }

    /**
     * Returns true if all of this cell's port subcells refine from
     * this defchan cell.  Port defchan subcells are traversed until either
     * nodes or a subcell that refines from chanCell is encounted.
     * When chanCell is standard.e1of, this method indicates
     * whether this cell communicates with its environment in a fully 
     * four-phase asynchronous manner (and is presumably DI).
     **/
    public boolean allPortSubcellsRefineFrom(final CellInterface parentChannel) {
        if (portSubcellSet.isEmpty()) return false;
        for (Iterator pi=portSubcellSet.iterator(); pi.hasNext();) {
            HierName portName = (HierName) pi.next();
            if (impliedPortMapping.containsKey(portName.toString())) continue;
            CellInterface portType = (CellInterface)subcellMap.get(portName);
            if (!portType.eventuallyRefinesFrom(parentChannel) &&
                !portType.allPortSubcellsRefineFrom(parentChannel))
                return false;
        }
        return true;
    }

    /**
     * Return the cosimulation info that matches the specified
     * behavior.  Gives precedence to java block over csp block.
     **/
    public CoSimInfo getCoSimInfo(int cosimBehavior) {
        if ((cosimBehavior & CoSimParameters.JAVA) != 0)
            return javaCosimInfo;
        else if ((cosimBehavior & CoSimParameters.CSP) != 0)
            return getCSPCoSimInfo();
        Debug.assertTrue(false, "bad cosimBehavior passed in to getCoSimInfo: " + cosimBehavior);
        return null;            // mollify compiler
    }    

    /**
     * Debugging function.  Returns a string with all sorts of information
     * about the internal data structures.  Hasn't been updated to reflect
     * all the information currently stored in CellImpl.
     **/
    public String toString() {
        final StringWriter sw = new StringWriter();
        final PrintWriter pw = new PrintWriter(sw, true);

        pw.println("Dump of cell of type: "+getFullyQualifiedType());
        pw.println("dump:"+getFullyQualifiedType()+":aliases:");
        pw.println(aliases);
        pw.println("dump:"+getFullyQualifiedType()+"assertblock:");
        pw.println(blocks.iterator(BlockInterface.ASSERT).next());
        pw.println("dump:"+getFullyQualifiedType()+":csp:");
        pw.println(getCSPProgram());
        pw.println("dump:"+getFullyQualifiedType()+":csp cosim info:");
        pw.println(getCSPCoSimInfo());
        pw.println("dump:"+getFullyQualifiedType()+":cspClass:");
        pw.println(getCspBlock().getCSPClass(false));
        pw.println("dump:"+getFullyQualifiedType()+":cspClass:");
        pw.println(getCspBlock().getCSPClass(true));
        pw.println("dump:"+getFullyQualifiedType()+":directives:");
        BlockIterator iter = blocks.iterator(BlockInterface.DIRECTIVE);
        if (iter.hasNext())
            pw.println(iter.next());
        else
            pw.println("none");
        pw.println("dump:"+getFullyQualifiedType()+":java cosim info:");
        pw.println(javaCosimInfo);
        pw.println("dump:"+getFullyQualifiedType()+":inlinedSubcellSet:");
        pw.println(inlinedSubcellSet);
        pw.print  ("dump:"+getFullyQualifiedType()+":module name:");
        pw.println(moduleName);
        pw.println("dump:"+getFullyQualifiedType()+":namedEnvironments:");
        pw.println(getEnvironments());
        pw.println("dump:"+getFullyQualifiedType()+":portSubcellSet:");
        pw.println(portSubcellSet);
        pw.println("dump:"+getFullyQualifiedType()+":prsBlock:");
        pw.println(blocks.iterator(BlockInterface.PRS).next());
        pw.println("dump:"+getFullyQualifiedType()+":subcellMap:");
        pw.println(subcellMap);
        pw.println("dump:"+getFullyQualifiedType()+":adoptedSet:");
        pw.println(adoptedSet);

        return sw.toString();
    }

    /**
     * Helper function for setRefinementParent(): copies the
     * information from the parent map that isn't overridden by the
     * child into the child, but leaves the rest alone.  putAll
     * overwrites everything.  Useful when the child has refined
     * ports, for instance.
     **/
    private static void fillInMap(Map/*<HierName,CellInterface>*/ child,
                                  final/*<HierName,CellInterface>*/ Map
                                      parent) {
        final Iterator parentKeys = parent.keySet().iterator();
        while (parentKeys.hasNext()) {
            Object parentKey = parentKeys.next(); // casting isn't worth it
            if (! child.containsKey(parentKey))
                child.put(parentKey, parent.get(parentKey));
        }
    }

    public boolean containsJava() {
        return !javaCosimInfo.isEmpty();
    }

    public boolean containsCsp() {
        return getCSPProgram() != null;
    }

    public boolean containsRunnableCsp() {
        return getCspBlock().hasSequentialStatement();
    }

    public boolean containsNetlist() {
        final NetlistBlock netlistBlock =
            (NetlistBlock) blocks.iterator(BlockInterface.NETLIST).next();
        return !netlistBlock.isEmpty();
    }

    public boolean containsVerilog() {
        return blocks.iterator(BlockInterface.VERILOG).hasNext();
    }

    public CellInterface getEnvironment(final String envName)
        throws NoSuchEnvironmentException {
        final EnvBlock eb = getEnvironments();
        assert eb != null : "Cell " + getFullyQualifiedType() +
                            " has no environment block";
        try {
            final CellInterface result = eb.getNamedEnvironment(envName);
            if (result == null)
                throw new NoSuchEnvironmentException(getFullyQualifiedType(),
                                                     envName);
            else
                return result;
        } catch (CastSemanticException e) {
            throw new NoSuchEnvironmentException(getFullyQualifiedType(),
                                                 envName, e);
        }
    }

    // specified CellInterface
    public CellInterface inlineLayoutSubcells() {
        final CellImpl cell =
            new CellImpl(type, moduleName, definitionKind, filename);
        cell.subcellMap.putAll(subcellMap);
        cell.portSubcellSet.addAll(portSubcellSet);
        cell.inlinedSubcellSet.addAll(inlinedSubcellSet);
        final BlockIterator bi = blocks.iterator(BlockInterface.DIRECTIVE);
        if (bi.hasNext()) {
            cell.blocks.iterator(BlockInterface.DIRECTIVE).add(
                new DirectiveBlock((DirectiveBlock) bi.next()));
        }
        Namespace.addNamespace(cell.aliases, aliases);

        final PrsBlock prs =
            (PrsBlock) blocks.iterator(BlockInterface.PRS).next();
        final PrsBlock newPrs =
            (PrsBlock) cell.blocks.iterator(BlockInterface.PRS).next();

        newPrs.refineFrom(prs);

        // for all non-channel subcells, inline them if inline_subcell
        // is specified for that subcell or if inline_subcell is 
        // specified for all cells and the inlining of that cell has
        // not been overridden.

        final Map m = DirectiveUtils.getSubcellDirective(this,
                DirectiveConstants.INLINE_LAYOUT,
                DirectiveConstants.INSTANCE_TYPE);
        final boolean inlineAll
            = ((Boolean) DirectiveUtils.getSubcellDirective(this,
                        DirectiveConstants.INLINE_LAYOUT)).booleanValue();

        for (final Iterator i = getSubcellPairs(); i.hasNext(); ) {
            final Pair p = (Pair) i.next();
            final HierName subcellName = (HierName) p.getFirst();
            final CellInterface subcell = (CellInterface) p.getSecond();
            final Boolean inlineSubcell = (Boolean) m.get(subcellName);

            if (!subcell.isChannel()) {
                if ((inlineSubcell != null &&
                     inlineSubcell.booleanValue()) ||
                    (inlineAll && inlineSubcell == null)) {
                    try {
                        cell.inlineSubcell(subcellName, false, false);
                    } catch (NoSuchSubcellException x) {
                        throw new AssertionError(x);
                    } catch (SubcellCreationException x) {
                        throw new AssertionError(x);
                    }
                }
            }
        }

        return cell;
    }

    public Stream<CellInterface> getInheritedCells() {
        return inheritedCells.stream();
    }

    /**
     * Add the mapping from the given implied port to its connection in the
     * parent cell.
     *
     * @param self name of the implied port in this cell
     * @param parent name of the object in the parent to connect to
     **/
    public void addImpliedPortMapping(final String self,
                                      final String parent) {
        impliedPortMapping.put(self, parent);
    }

    public String getParentImpliedPort(final String self) {
        return impliedPortMapping.get(self);
    }

    public void addEnvExtraPortMapping(final String self,
                                       final String dut) {
        envExtraPortMapping.put(self, dut);
    }

    public String getEnvExtraPortMapping(final String self) {
        return envExtraPortMapping.get(self);
    }

    public CellInterface routedSubcells(final boolean complete) {
        return routedSubcells(complete, false, new HashMap());
    }

    private CellInterface routedSubcells(final boolean complete,
                                         final boolean addSuffix,
                                         final Map cache) {
        if (!getSubcellPairs().hasNext()) return this;

        final CellImpl cached = (CellImpl) cache.get(getFullyQualifiedType());
        if (cached != null) return cached;

        final CellImpl cell =
            new CellImpl(type + (addSuffix ? "$ROUTED" : ""), moduleName,
                         definitionKind, filename);
        cell.refinementAncestor = refinementAncestor;
        cell.directRefinementParent = directRefinementParent;
        cell.subcellMap.putAll(subcellMap);
        cell.portSubcellSet.addAll(portSubcellSet);
        cell.inlinedSubcellSet.addAll(inlinedSubcellSet);
        cell.adoptedSet.addAll(adoptedSet);
        cell.metaParamDefinitions.addAll(metaParamDefinitions);
        cell.portDefinitions.putAll(portDefinitions);
        cell.impliedPortMapping.putAll(impliedPortMapping);
        cell.envExtraPortMapping.putAll(envExtraPortMapping);
        cell.hasPrsBlock = hasPrsBlock;
        cell.prsBlockIsComplete = prsBlockIsComplete;
        cell.hasSubcellsBlock = hasSubcellsBlock;
        cell.subcellsBlockIsComplete = subcellsBlockIsComplete;
        cell.hasSubtypesBlock = hasSubtypesBlock;
        cell.inheritedCells.addAll(inheritedCells);
        cell.instancePositions.putAll(instancePositions);
        final BlockIterator bi = blocks.iterator(BlockInterface.DIRECTIVE);
        if (bi.hasNext()) {
            cell.blocks.iterator(BlockInterface.DIRECTIVE).add(
                new DirectiveBlock((DirectiveBlock) bi.next()));
        }
        Namespace.addNamespace(cell.aliases, aliases);

        final String[] refineable = new String[] { BlockInterface.ASSERT,
                                                   BlockInterface.ENV,
                                                   BlockInterface.PRS,
                                                   BlockInterface.NETLIST,
                                                   BlockInterface.CSP };
        for (int i = 0; i < refineable.length; ++i) {
            final BlockInterface oldBlock =
                blocks.iterator(refineable[i]).next();
            final BlockInterface newBlock =
                cell.blocks.iterator(refineable[i]).next();
            newBlock.refineFrom(oldBlock);
        }

        cell.adoptDirective(this);

        // for all non-channel subcells, inline them if inline_subcell
        // is specified for that subcell or if inline_subcell is 
        // specified for all cells and the inlining of that cell has
        // not been overridden.

        final Map m = DirectiveUtils.getSubcellDirective(this,
                DirectiveConstants.ROUTED,
                DirectiveConstants.INSTANCE_TYPE);

        for (final Iterator i = getSubcellPairs(); i.hasNext(); ) {
            final Pair p = (Pair) i.next();
            final HierName subcellName = (HierName) p.getFirst();
            final CellInterface subcell = (CellInterface) p.getSecond();
            final Boolean inlineSubcell = (Boolean) m.get(subcellName);
            final boolean unrouted =
                !CellUtils.isRouted(subcell) ||
                (inlineSubcell != null && !inlineSubcell.booleanValue());

            if (!subcell.isChannel() && !subcell.isNode()) {
                if (unrouted || complete) {
                    final CellInterface routedSubcell;
                    if (subcell instanceof CellImpl) {
                        routedSubcell =
                            ((CellImpl) subcell).routedSubcells(complete,
                                                                unrouted,
                                                                cache);
                    } else {
                        routedSubcell = subcell.routedSubcells(complete);
                    }
                    cell.subcellMap.put(subcellName, routedSubcell);
                    if (unrouted) {
                        try {
                            cell.inlineSubcell(subcellName, false, false);
                        } catch (NoSuchSubcellException x) {
                            throw new AssertionError(x);
                        } catch (SubcellCreationException x) {
                            throw new AssertionError(x);
                        }
                    }
                }
            }
        }

        cache.put(getFullyQualifiedType(), cell);
        return cell;
    }

    public CellInterface sizingEnvSubcells(CellUtils.SizingEnvCreator creator) {
        return sizingEnvSubcells(creator, true, false, new HashMap());
    }

    private CellInterface sizingEnvSubcells(
            final CellUtils.SizingEnvCreator creator,
            final boolean topLevel,
            final boolean addSuffix,
            final Map cache) {
        if (CellUtils.isWiring(this)) return this;

        final CellInterface cached =
            (CellInterface) cache.get(getFullyQualifiedType());
        if (cached != null) return cached;

        final Map<HierName, CellInterface> modifiedSubcells =
            new HashMap<HierName, CellInterface>();

        // Suppose we have:
        // define A()() { subcells { inline B b; } }
        // define B()() { subcells { C c; } }
        // define C()() { csp { ... } }
        //
        // If we don't convert all cells, including inlined instances, the
        // result we return, A', will reference B b, and C' b.c, but B still
        // references C, and both C and C' have the same cell name.  This
        // causes a problem with Cadencize, because it looks at all subcells,
        // including inlined ones, and also stores the result in a cache, keyed
        // by the name of the cell.  If Cadencize processes C before C', then
        // the aliases in C' won't be visible.
        for (final Iterator i = getAllSubcellPairs(); i.hasNext(); ) {
            final Pair p = (Pair) i.next();
            final HierName subcellName = (HierName) p.getFirst();
            final CellInterface subcell = (CellInterface) p.getSecond();

            if (!subcell.isChannel() && !subcell.isNode()) {
                final CellInterface sizeable =
                    ((CellImpl) subcell).sizingEnvSubcells(creator, false,
                                                           addSuffix, cache);
                if (sizeable != subcell)
                    modifiedSubcells.put(subcellName, sizeable);
            }
        }

        final CellInterface result;
        final boolean internalEnv = CellUtils.isInternalEnv(this);
        if (modifiedSubcells.isEmpty() && !internalEnv) {
            result = this;
        } else {
            final CellImpl cell =
                new CellImpl(type + (addSuffix ? "$SIZING" : ""), moduleName,
                             definitionKind, filename);
            cell.refinementAncestor = refinementAncestor;
            cell.directRefinementParent = directRefinementParent;
            cell.subcellMap.putAll(subcellMap);
            cell.subcellMap.putAll(modifiedSubcells);
            cell.portSubcellSet.addAll(portSubcellSet);
            cell.inlinedSubcellSet.addAll(inlinedSubcellSet);
            cell.adoptedSet.addAll(adoptedSet);
            cell.metaParamDefinitions.addAll(metaParamDefinitions);
            cell.portDefinitions.putAll(portDefinitions);
            cell.impliedPortMapping.putAll(impliedPortMapping);
            cell.envExtraPortMapping.putAll(envExtraPortMapping);
            cell.hasPrsBlock = hasPrsBlock;
            cell.prsBlockIsComplete = prsBlockIsComplete;
            cell.hasSubcellsBlock = hasSubcellsBlock;
            cell.subcellsBlockIsComplete = subcellsBlockIsComplete;
            cell.hasSubtypesBlock = hasSubtypesBlock;
            cell.inheritedCells.addAll(inheritedCells);
            cell.instancePositions.putAll(instancePositions);
            final BlockIterator bi = blocks.iterator(BlockInterface.DIRECTIVE);
            if (bi.hasNext()) {
                cell.blocks.iterator(BlockInterface.DIRECTIVE).add(
                    new DirectiveBlock((DirectiveBlock) bi.next()));
            }
            Namespace.addNamespace(cell.aliases, aliases);

            final String[] refineable = new String[] { BlockInterface.ASSERT,
                                                       BlockInterface.ENV,
                                                       BlockInterface.PRS,
                                                       BlockInterface.NETLIST,
                                                       BlockInterface.CSP };
            for (int i = 0; i < refineable.length; ++i) {
                final BlockInterface oldBlock =
                    blocks.iterator(refineable[i]).next();
                final BlockInterface newBlock =
                    cell.blocks.iterator(refineable[i]).next();
                newBlock.refineFrom(oldBlock);
            }

            cell.adoptDirective(this);
            if (internalEnv) {
                final AliasedSet/*<HierName>*/ sizingAliases =
                    new AliasedSet/*<HierName>*/(HierName.getComparator());
                final Map<HierName,CellInterface> sizingSubcells =
                    new HashMap<HierName,CellInterface>();
                creator.getInternalEnv(cell, sizingAliases, sizingSubcells);
                if (!sizingSubcells.isEmpty()) {
                    cell.setHasCompleteSubcellsBlock();
                    cell.subcellMap.putAll(sizingSubcells);
                    Namespace.addNamespace(cell.aliases, sizingAliases);
                }
            }
            result = cell;
        }

        cache.put(getFullyQualifiedType(), result);
        return result;
    }
}
