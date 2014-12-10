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

package com.avlsi.fast;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.NoSuchElementException;
import java.io.*;

import com.avlsi.io.FileSearchPath;

import com.avlsi.cast.CastFileParser;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.directive.impl.DirectiveSource;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cell.CellDelay;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;
import com.avlsi.cell.ExclusiveNodeSets;
import com.avlsi.fast.NetlistBlock;
import com.avlsi.fast.DirectiveBlock;
import com.avlsi.file.cdl.parser.CDLSimpleInterface;
import com.avlsi.file.cdl.parser.Template;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.netlist.AbstractNetlist;
import com.avlsi.netlist.AbstractNode;
import com.avlsi.netlist.AbstractNodeIterator;
import com.avlsi.prs.ProductionRuleSet;
import com.avlsi.prs.ProductionRule;
import com.avlsi.prs.UnimplementableProductionRuleException;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cadencize.CadenceInfo;
import com.avlsi.tools.lvs.NetGraph;
import com.avlsi.util.container.AliasedMap;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.MultiMap;
import com.avlsi.util.container.ObjectUtils;
import com.avlsi.util.container.Pair;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.functions.UnaryPredicate;
import com.avlsi.geometry.BoundingBox;

import com.avlsi.tools.jauto.*;
import com.avlsi.tools.jauto.AutoThreshold;
/**
 * Class to represent a node in the cast cell refinement hierarchy.
 *
 * Need not be a "cell", just a holder for information common to many
 * cells, such as technology constants, or leaf cell parameters.
 *
 * May also be a subtype of cell, or a subtype of a subtype.
 *
 * This is also a node in the instantiation hierarchy, which is a DAG.
 *
 * @author Aaron Denney
 * @version $Date$
 **/

public final class CellType {
    /**
     * Encapsulates a port.  A port is direction, and a CellNet.
     **/
    public static final class PortInfo {
        private final CellNet m_Net;

        public static int INPUT = 0;
        public static int OUTPUT = 1;
        public static int INPUT_OUTPUT = 2;
        public static int UNKNOWN = 3;

        private final int m_PortType;

        public BoundingBox bBox;

        public PortInfo( final CellNet net, int portType ) {
            m_Net = net;
            m_PortType = portType;
            Debug.assertTrue( ( portType == INPUT )        ||
                          ( portType == OUTPUT )       ||
                          ( portType == INPUT_OUTPUT ) ||
                          ( portType == UNKNOWN ) );
        }

        public int getType() {
            return m_PortType;
        }

        public CellNet getNet() {
            return m_Net;
        }

    }

    public interface PortInfoIterator {
        boolean hasNext();
        /** @throws NoSuchElementException **/
        PortInfo next();
    }

    /** flags for creating a new type from an existing type. **/
    private static final int REFINEMENT=0, SIBLING=1;

    /** the design we are part of. **/
    public final CastDesign design;

    /** Temporary.  Will be removed with new parser. **/
    public final CellInterface cast_cell;

    // Refinement hierarchy.

    /** the CellType we refine. **/
    public final CellType prototype;

    /** the CellType that are refinements of us. **/
    public final List/*<CellType>*/ refinements;

    /**
     * how many refinements have been created.
     * Also index for next anonymous derived cell.
     **/
    int subtypeCount = 0;

    /**
     * name of this cell type. Anonymous derived cells have the form
     * &lt;prototype's name&gt;:&lt;count&gt;
     **/
    public final String typeName;

    /** manually created, or automatic. **/
    public final boolean manual;

    /** which subtype is this? **/
    public final int subtypenumber;

    // Actual data!
    
    /** The available blocks. **/
    protected final Map/*<String,BlockInterface>*/ _blocks =
        new HashMap/*<String,BlockInterface>*/();
    public final Map/*<String,BlockInterface>*/ blocks =
        Collections.unmodifiableMap(_blocks);

    // The blocks with explicit typing information.
    
    // Instantiation DAG.

    /**
     * connections to subcells.  Instantiation DAG.  "Name" -&gt;
     * "subcellconnection"
     **/
    final Map<HierName,ConnectionInfo> subcellconnections;

    /**
     * names of subcellports -&gt; connectioninfo.  Follow to traverse nets.
     **/
    final Map/*<HierName,ConnectionInfo>*/ subcellports;

    /**
     * connections to parent instances.  Backpointers for Instantiation DAG.
     **/
    final List/*<ConnectionInfo>*/ parentcellconnections;

    // Node and name information.

    /** production rules for this cell type. **/

    public final ProductionRuleSet prs;

    /** which names are aliased together? **/
    public final AliasedSet/*<HierName>*/ namespace;

    /** names of nodes ... map hier names -&gt; nodes structure. **/
    public final Map/*<HierName,CellNet>*/ nets;

    /** Maps CellNet to PortInfo.*/
    private final Map/*<CellNet,PortInfo>*/ ports;

    /** all nets. **/
    public final Set/*<CellNet>*/ allNets;
    
    // Subtype-specific information.
    
    public        NetGraph transistors;
    
    // list of all the halfoperators in this cell
    public ArrayList<HalfOperator> listHalfOperators;

    // set of sizing paths belong to this cell
    public Set/*<SizingPath>*/      sizingPaths;

    // set of concatenated paths belong to this cell
    public Set/*<CatPath>*/         catPaths;

    // set of reduced (simplified) concatenated paths belong to this cell
    public Set/*<CatPath>*/         reducedCatPaths;

    // list of all the halfoperators in the concatenated paths of this cell
    public Set/*<HalfOperator>*/    setSizingHalfOperators;

    // strength_group directive
    private final AliasedSet strengthGroup;

    /** is this cell "fragment"? **/
    boolean                         isFragment = false;

    /** size of cell. **/
    public float xSize = 0, ySize = 0;

    /** cell level cached here. **/
    private int level = -1;

    /**
     * Are we a "wiring cell"?  That is, no transistors, and non in subcells.
     * Used strictly to collect wires and names for wires.  e1ofN, TWIST, BGZ,
     * etc.
     **/
    public final boolean isWiringCell;

    /**
     * ExclusiveNodeSets, from CellInterface, via Cadencize.
     **/
    public final ExclusiveNodeSets exclusives;

    /** Names of Vdd, GND, and _RESET */
    private final HierName Vdd, GND, _RESET;

    /** Cadencize to use */
    private final Cadencize cad;

    /** Information relayed to delays.  Include delaybias, cell delaybias,
     * extra_delay.
     **/
    private final CellDelay delay;
    
    /**
     * Is this cell a synthesized internal environment
     **/
    private final boolean isInternalEnv;

    private static Map/*<CellNet,PortInfo>*/ makePortMap(
            final Set/*<CellNet>*/ netSet) {
        //Iterate over all the nets and put ones that are ports into
        //this map.
        final Map/*<CellNet,PortInfo>*/ ports =
            new HashMap/*<CellNet,PortInfo>*/();
        
        for (final Iterator netIter = netSet.iterator();  netIter.hasNext(); ) {
            final CellNet currNet = ( CellNet ) netIter.next();

            int portType;

            switch( currNet.portDirection ) {
            case CellNet.INPUT:
                portType = PortInfo.INPUT;
                break;

            case CellNet.OUTPUT:
                portType = PortInfo.OUTPUT;
                break;

            case CellNet.INPUTOUTPUT:
                portType = PortInfo.INPUT_OUTPUT;
                break;

            case CellNet.UNKNOWN:
                portType = PortInfo.UNKNOWN;
                break;

            default:
                throw new AssertionError("Unknown port direction " +
                                         currNet.portDirection);
            }

            ports.put( currNet, new PortInfo( currNet, portType ) );
        }

        return ports;

    }

    /**
     * Constructor based on Cast Parser.
     *
     * package visibility, as it should only be invoked via CastDesign.
     **/
    CellType(CastDesign d, Cadencize c, CellInterface cell, HierName Vdd, HierName GND, HierName _RESET, boolean isInternalEnv, boolean useAstaExtraDelay) {
        this.Vdd = Vdd;
        this.GND = GND;
        this._RESET = _RESET;
        this.isInternalEnv = isInternalEnv;
        // Cast V1 has no refinements.
        this.prototype = null;
        this.design = d;
        this.prs = cell.getProductionRuleSet();
        this.typeName = cell.getFullyQualifiedType();
        this.isWiringCell = CellUtils.isWiring(cell);
        this.cad = c;
        CadenceInfo ci = c.convert(cell);
        this.namespace = ci.getLocalNodes();
        this.nets = nodeFactory(ci, cell);
        if (cell.containsNetlist()) {
            final Template templ = ((NetlistBlock) cell.getBlockInterface().iterator(BlockInterface.NETLIST).next()).getCanonicalTemplate(c);
            nodeNetlistFactory(this.nets, templ);
        }
        // Useful, because its the only list of all the nets without duplicates.
        this.allNets = new TreeSet/*<CellNet>*/(CellNet.getComparator());
        this.allNets.addAll(this.nets.values());
        // Work to get rid of this.
        this.cast_cell = cell;
        this.refinements = new ArrayList();
        this.subtypenumber = this.subtypeCount = 0;
        this.transistors = null;
        this.exclusives = new ExclusiveNodeSets();
        this.exclusives.merge(ci.getPortExclusiveNodeSets());
        this.exclusives.merge(ci.getLocalExclusiveNodeSets());
        
        this.ports = makePortMap( allNets );
        
        // FIXME: these are bogus.
        
        this.subcellports = new HashMap/*<HierName,ConnectionInfo>*/();
        this.subcellconnections = new TreeMap<HierName,ConnectionInfo>();
        this.parentcellconnections = new ArrayList/*<ConnectionInfo>*/();
        this.manual = false;

        this.listHalfOperators = new ArrayList<HalfOperator>();
        this.setSizingHalfOperators = new HashSet/*<HalfOperator>*/();
        this.sizingPaths = new LinkedHashSet/*<SizingPath>*/();
        this.catPaths = new LinkedHashSet/*<CatPath>*/();
        this.reducedCatPaths = new LinkedHashSet/*<CatPath>*/();
        this.delay =
            new CellDelay(cast_cell,
                          cad.convert(cast_cell).getLocalNodes(),
                          cast_cell.getProductionRuleSet().getProductionRules(),
                          Float.NaN, useAstaExtraDelay);
        this.strengthGroup = initializeStrengthGroup(cast_cell);

        addCutPaths();
    }

    /**
     * Constructor duplicating existing type.  Private as it should only be
     * invoked from functions here.
     **/
    private CellType(CellType o, int how, Set overrides) {
        this.Vdd = o.Vdd;
        this.GND = o.GND;
        this._RESET = o._RESET;
        this.isInternalEnv = o.isInternalEnv;
        this.parentcellconnections = new ArrayList/*<ConnectionInfo>*/();
        this.refinements = new ArrayList();
        
        this.subcellports = new HashMap/*<HierName,ConnectionInfo>*/();
        this.isWiringCell = o.isWiringCell;
        this.cad = o.cad;
        this.exclusives = o.exclusives;
        switch (how) {
          case REFINEMENT:
            this.prototype = o;
            // FIXME: need to be equivalent refinements.
            this.subcellconnections = o.subcellconnections;
            this.nets = o.nets;
            this.namespace = o.namespace;
            break;
          case SIBLING:
            this.prototype = o.prototype;
            // FIXME: need to make subcells point to me.
            this.subcellconnections = o.subcellconnections;
            // FIXME: need to make subcells point to me.
            this.nets = o.nets;
            this.namespace = o.namespace;
            break;
          default:
            throw new AssertionError("invalid flag in duplicating constructor");
        }
        this.prs = o.prs;
        this.subtypenumber = this.prototype.subtypeCount++;
        this.typeName = this.prototype.typeName + ":" + this.subtypenumber;
        this.manual = true;
        this.design = o.design;
        this.cast_cell = null;
        this.transistors = null;
        this.allNets = new HashSet/*<CellNet>*/( nets.values() );

        this.ports = makePortMap( allNets );

        this.listHalfOperators = o.listHalfOperators;
        this.setSizingHalfOperators = o.setSizingHalfOperators;
        this.sizingPaths = o.sizingPaths;
        this.catPaths = o.catPaths;
        this.reducedCatPaths = o.reducedCatPaths;
        this.delay = o.delay;
        this.strengthGroup = o.strengthGroup;

        addCutPaths();
    }

    public PortInfoIterator getPorts() {
        return new PortInfoIterator() {
                
                private final Iterator portIter = ports.values().iterator();
                
                public boolean hasNext() {
                    return portIter.hasNext();
                }

                public PortInfo next() {
                    return ( PortInfo ) portIter.next();
                }
            };
    }

    private void initializeStrengthGroupHelper(
            final MultiMap<Integer,Pair<CellNet,Integer>> grouped,
            final Map<HierName,Integer> directives,
            final Integer pull) {
        for (Map.Entry<HierName,Integer> entry : directives.entrySet()) {
            final Integer group = entry.getValue();
            if (group >= 0) {
                grouped.put(group,
                            new Pair<CellNet,Integer>(getNet(entry.getKey()),
                                                      pull));
            }
        }
    }

    // Convert strength_group directives into an aliased set.  Equivalent
    // members in the aliased set are in the same group.  Each member is a pair
    // of CellNet and drive direction.
    private AliasedSet initializeStrengthGroup(final CellInterface ci) {
        final Map dir = DirectiveUtils.getPrsDirective(ci,
                DirectiveConstants.STRENGTH_GROUP,
                DirectiveConstants.HALFOP_TYPE);
        final Map ups = (Map<HierName,Integer>)
            DirectiveUtils.canonizeKey(namespace, DirectiveUtils.getUps(dir));
        final Map dns = (Map<HierName,Integer>)
            DirectiveUtils.canonizeKey(namespace, DirectiveUtils.getDowns(dir));

        final MultiMap<Integer,Pair<CellNet,Integer>> grouped =
            new MultiMap<Integer,Pair<CellNet,Integer>>();
        initializeStrengthGroupHelper(grouped, ups,
                                      HalfOperator.DriveDirection.PULL_UP);
        initializeStrengthGroupHelper(grouped, dns,
                                      HalfOperator.DriveDirection.PULL_DOWN);

        final AliasedSet result = new AliasedSet(
            new Comparator<Pair<CellNet,Integer>>() {
                public int compare(final Pair<CellNet,Integer> p1,
                                   final Pair<CellNet,Integer> p2) {
                    int result = ObjectUtils.compare(p1.getSecond(),
                                                     p2.getSecond());
                    if (result == 0) {
                        result = CellNet.getComparator().compare(p1.getFirst(),
                                                                 p2.getFirst());
                    }

                    return result;
                }
            });
        for (Integer key : grouped.keySet()) {
            final Collection<Pair<CellNet,Integer>> values = grouped.get(key);
            Pair<CellNet,Integer> first = null;
            for (Pair<CellNet,Integer> value : values) {
                if (first == null) {
                    first = value;
                    result.add(first);
                } else {
                    result.makeEquivalent(first, value);
                }
            }
        }

        return result;
    }

    /**
     * Returns true if a half-operator driving net1 in the direction pull1 is
     * in the same strength group as a half-operator driving net2 in the
     * direction pull2.
     **/
    boolean hasSameStrengthGroup(final CellNet net1, final int pull1,
                                 final CellNet net2, final int pull2) {
        return strengthGroup.areEquivalent(
                new Pair<CellNet,Integer>(net1, pull1),
                new Pair<CellNet,Integer>(net2, pull2));
    }

    public PortInfo getPortInfoForCellNet(CellNet n) {
        return (PortInfo) ports.get(n);
    }

    public PortInfo getPortInfoForPinNamed(HierName h) {
        CellNet net = getNet(h);
        return getPortInfoForCellNet(net);
    }

    public PortInfo getPortInfoForPinNamed(String s) {
        CellNet net = getNet(s);
        return getPortInfoForCellNet(net);
    }

    /**
     * create all associated CellNets and stick them in parent.
     **/
    private Map/*<HierName,CellNet>*/ nodeFactory(CadenceInfo ci, CellInterface cell) {
        HashMap/*<HierName,CellNet>*/ nodes = new HashMap/*<HierName,CellNet>*/();
        // What's described below should no long be necessary.  But for
        // historical purposes:
        //
        // This only gets the ones declared in the Cast, not globals.
        // We don't use cadencize because it considers aliases of ports
        // to be ports.  Appropriate for its original purpose, but not
        // for what we want.
        //
        // To get the other ones we 
        // *UGLY HACK WARNING*
        // see if the name is global and only has one level, if so, it
        // must be in the portlist.
        Set/*<HierName>*/ cellPorts = collectPortNames(cell);
        AliasedSet/*<HierName>*/ l = ci.getLocalNodes();
        for (Iterator i = l.getCanonicalKeys(); i.hasNext();) {
            Set/*<HierName>*/ locals = new TreeSet/*<HierName>*/();
            Set/*<HierName>*/ ports = new TreeSet/*<HierName>*/();
            final HierName canon = (HierName) i.next();
            for (Iterator j = l.getAliases(canon); j.hasNext();) {
                HierName name = (HierName)j.next();
                if (cellPorts.contains(name)) {
                    ports.add(name);
                } else {
                    locals.add(name);
                }
            }
            addCellNets(nodes, locals, ports, canon);
        }
        return nodes;
    }

    /**
     * Create all CellNets internal to a netlist block
     **/
    private void nodeNetlistFactory(final Map/*<HierName,CellNet>*/ nodes, final Template cell) {
        cell.execute(Collections.EMPTY_MAP, new CDLSimpleInterface() {
            private void addName(final HierName name) {
                if (!name.isGenerated() &&
                    !name.isNumeric() &&
                    !nodes.containsKey(name)) {
                    final Set/*<HierName>*/ locals =
                        new TreeSet/*<HierName>*/();
                    locals.add(name);
                    final HierName canon =
                        (HierName) namespace.getCanonicalKey(name);
                    addCellNets(nodes, locals, Collections.EMPTY_SET,
                                canon == null ? name : canon);
                }
            }
            public void makeResistor(HierName name, HierName n1, HierName n2,
                                     double val) {
                addName(n1); addName(n2);
            }
            public void makeCapacitor(HierName name, HierName npos,
                                      HierName nneg, double val) {
                addName(npos); addName(nneg);
            }
            public void makeTransistor(HierName name, int type, HierName ns,
                                       HierName nd, HierName ng, HierName nb,
                                       double w, double l) {
                addName(ns); addName(nd); addName(ng); addName(nb);
            }
            public void makeDiode(HierName name, int type, HierName npos,
                                  HierName nneg, double w, double l, double a,
                                  double p) {
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
            public void makeCall(HierName name, String subName, HierName[] args,
                                 Map parameters) {
                for (int i = 0; i < args.length; ++i) addName(args[i]);
            }
            public void beginSubcircuit(String subName, String[] in,
                                        String[] out) { }
            public void endSubcircuit(String subName) { }
        });
    }

    private Set/*<HierName>*/ collectPortNames(CellInterface c) {
        Set/*<HierName>*/ rv = new TreeSet/*<HierName>*/();
        collectPortNamesRecurse(null, c, rv);
        return rv;
    }

    private void collectPortNamesRecurse(HierName prefix,
                                         CellInterface port,
                                         Set/*<HierName>*/ ports) {
        if (port.isNode()) {
            HierName local = prefix;
            ports.add(local);
        } else {
            for (Iterator i = port.getPortSubcellPairs(); i.hasNext();) {
                Pair p = (Pair) i.next();
                HierName newh = (HierName) p.getFirst();
                CellInterface c = (CellInterface) p.getSecond();
                HierName local = HierName.append(prefix, newh);
                collectPortNamesRecurse(local, c, ports);
            }
        }
    }

    private void addCellNets(Map/*<HierName,CellNet>*/ nodes,
                             Set/*<HierName>*/ l,
                             Set/*<HierName>*/ p,
                             HierName canon) {
        CellNet n = new CellNet(CellType.this, l, p,
                                new TreeSet/*<HierName>*/(), canon,
                                Vdd, GND, _RESET);
        for (Iterator j = l.iterator(); j.hasNext();) {
            HierName h = (HierName) j.next();
            nodes.put(h, n);
        }
        for (Iterator j = p.iterator(); j.hasNext();) {
            HierName h = (HierName) j.next();
            nodes.put(h, n);
        }
    }

    /**
     * Gets (or creates) the default subtype for this cell.
     *
     * This will have sizing information if there are transistors to
     * be sized.
     **/
    CellType getDefaultSubType() {
        if (refinements.isEmpty()) {
            createDefaultSubType();
        }
        return (CellType) refinements.get(0);
    }

    /**
     * creates a new automatic subtype.
     *
     * Calling context: after new completed.
     **/
    private void createDefaultSubType() {
        CellType st = new CellType(this, REFINEMENT, null);
        st.ensureSizingBlock();
        refinements.add(st);
    }

    // Block retrieval.
    public BlockInterface getBlock(String name) {
        return (BlockInterface) _blocks.get(name);
    }

    // Technology block.  

    /**
     * Technology constants.
     **/
    public float getUnitCapacitance() {
        return 0;
    }

    /**
     * Technology constants.
     **/
    public float getUnitResistance() {
        return 0;
    }

    // containment hierarchy manipulation.
    /**
     * create a new subtype that is a sibling to this one, with all
     * values starting the same.
     *
     * @return a new cell that is a sibling of this one.
     **/
    public CellType cloneAsSibling() {
        return new CellType(this, SIBLING, null);
    }

    /**
     * removes this cell from instance hierarchy, replacing it with the
     * other.
     **/
    public void replaceWith(CellType other) {
        // for all instances, paste other in instead.
        // (instances, parents, but not prototypes, or what we instance.)
    }

    /**
     * ensure that we have sizing information.
     **/
    private void ensureSizingBlock() {
    }

    /**
     * Get the net corresponding to a name s.
     **/
    public CellNet getNet(String s) {
        try {
            return (CellNet) nets.get(HierName.makeHierName(s, '.'));
        } catch (InvalidHierNameException e) {
            throw new RuntimeException("makeHierName failed.  Cannot happen!");
        }
    }

    public CellNet getNet(HierName local) {
        return (CellNet) nets.get(local);
    }

    /**
     * Translate a name to a string.
     *
     * @param local The name local to this cell.
     * @return The name represented as a string.
     **/
    public String getNameAsString(HierName h) {
        return h.getAspiceString();
    }

    /**
     * Walks the instance space, but visits each celltype at most once.
     **/
    public void walkOnce(CellTypeProcessor p) {
        HashSet/*<CellType>*/ s = new HashSet/*<CellType>*/();
        walkOnce(p, s);
    }

    public void walkOnce(CellTypeProcessor p, Set/*<CellType>*/ s) {
        if (s.contains(this)) return;

        if(DebugOption.printLevel <= 1){
            System.out.println("Note: walkOnce processing: " + this.typeName);
        }

        s.add(this);
        p.processCellType(this);
        for (Iterator i = subcellconnections.values().iterator(); i.hasNext(); ) {
            CellType c = ((ConnectionInfo) i.next()).child;
            c.walkOnce(p, s);
        }
    }

    /**
     * Walks the instance space, possibly visiting a given celltype multiple times.
     **/
    public interface InstanceProcessor {
        void processInstance(final CellType type, final HierName instance,
                             final Collection<ConnectionInfo> path);
    }

    private void walkMany(InstanceProcessor p, HierName instance,
                          LinkedList<ConnectionInfo> path) {
        p.processInstance(this, instance, path);
        for (Iterator i = subcellconnections.values().iterator();
             i.hasNext(); ) {
            final ConnectionInfo ci = (ConnectionInfo) i.next();
            path.addLast(ci);
            ci.child.walkMany(p, HierName.append(instance, ci.nameInParent),
                              path);
            path.removeLast();
        }
    }

    public void walkMany(InstanceProcessor p) {
        walkMany(p, null, new LinkedList<ConnectionInfo>());
    }

    /**
     * @return the number of logically different places this celltype is instanced.
     **/
    public int getLogicalInstanceCount() {
        return parentcellconnections.size();
    }

    /**
     * @return the number of physically different places this celltype is ever instanced.
     **/
    public int getPhysicalInstanceCount() {
        int count = parentcellconnections.size();
        if (count == 0) {
            return 1; // assume top level cell.
        }
        count = 0;
        for (Iterator i = parentcellconnections.iterator(); i.hasNext(); ) {
            ConnectionInfo c = (ConnectionInfo) i.next();
            count += c.parent.getPhysicalInstanceCount();
        }
        return count;
    }

    /**
     * Returns the list of pointers to all the half-operators in this cell
     **/
    public ArrayList<HalfOperator> getListHalfOperators() {
        return listHalfOperators;
    }

    /**
     * Returns the list of pointers to all the half-operators in the concatenated paths of this cell
     **/
    public Set/*<HalfOperator>*/ getSetHalfOperatorsInCatPaths()
    {
        return setSizingHalfOperators;
    }

    /**
     * For all the variables in the cell, set their "FIXED" properties
     **/
    public void fixVariables() {
        fixOrUnfixVariables(true);
    }

    /**
     * For all the variables in the cell, unset their "FIXED" properties
     **/
    public void unfixVariables() {
        fixOrUnfixVariables(false);
    }

    private void fixOrUnfixVariables(boolean fix) {
        // FIXME: possible cell/transistor mix in the future, don't use getLevel
        if(getLevel() == 0){

            for (Iterator ita = sizingPaths.iterator(); ita.hasNext(); ) {
                SizingPath spa = (SizingPath)ita.next();

                if(!spa.isFragment()){
                    for (Iterator itb = spa.getPath().iterator(); itb.hasNext(); ) {
                        HalfOperator hoa = (HalfOperator)itb.next();
                        if (fix)
                            hoa.fixVariable();
                        else
                            hoa.unfixVariable();
                    }
                }
            }
        }
        else{

            for (Iterator ita = reducedCatPaths.iterator(); ita.hasNext(); ) {
                CatPath cpa = (CatPath)ita.next();

                if(!cpa.isFragment()){
                    for (Iterator itb = cpa.getCatPath().iterator(); itb.hasNext(); ) {
                        SizingPath spa = (SizingPath)itb.next();

                        for (Iterator itc = spa.getPath().iterator(); itc.hasNext(); ) {
                            HalfOperator hoa = (HalfOperator)itc.next();
                            if (fix)
                                hoa.fixVariable();
                            else
                                hoa.unfixVariable();
                        }
                    }
                }
            }
        }
    }

    /**
     * Check if the sizes of all transisors in the cell are converged
     * Absolute differences
     **/
    public boolean sizeConvergedAbs(double threshold) {
        for (HalfOperator hoa : listHalfOperators) {
            double delta = Math.abs(hoa.getCurrentSize() - hoa.getPreviousSize());
            if (delta > threshold) {
                if(DebugOption.printLevel <= 1){
                    System.out.println(toString());
                    System.out.println("Current size: " + hoa.getCurrentSize());
                    System.out.println("Previous size: " + hoa.getPreviousSize());
                }
                return false;
            }
        }


        return true;
    }

    /**
     * Check if the sizes of all transisors in the cell are converged
     * Relative differences
     **/
    public boolean sizeConvergedRel(double threshold) {
        for (HalfOperator hoa : listHalfOperators) {
            double delta = Math.abs(hoa.getCurrentSize() / hoa.getPreviousSize() - 1.0);
            if (delta > threshold) {
                if(DebugOption.printLevel <= 3){
                    System.out.println("NOTE: Cell: " 
                        + typeName 
                        + " did not converge on: " 
                        + hoa.outputNet.canonicalName.getCadenceString());
                    System.out.println("Current size: " + hoa.getCurrentSize());
                    System.out.println("Previous size: " + hoa.getPreviousSize());
                }
                return false;
            }
        }


        return true;
    }

    /**
     * Do we have a subcell named name?  If so return it, else return null;
     *
     * @param name the name of the subcell.
     * @return The ConnectionInfo corresponding to this instance of the cell.
     **/
    public ConnectionInfo getSubcellNamed(HierName h) {
        return (ConnectionInfo) subcellconnections.get(h);
    }

    /**
     * What are the subcells connected to the net named name?
     *
     * @param name the name of the net
     * @return a set of ConnectionInfo that connect to this net.
     **/
    public Set<ConnectionInfo> getSubcellsConnectedTo(HierName h) {
        CellNet n = (CellNet) nets.get(h);
        if (n == null) return null;
        Set<ConnectionInfo> rv = new HashSet<ConnectionInfo>();
        for (Iterator i = n.subcellconnectionNames.iterator(); i.hasNext(); ) {
            HierName j = (HierName) i.next();
            ConnectionInfo ci = (ConnectionInfo) subcellports.get(j);
            if (ci != null) rv.add(ci);
        }
        return rv;
    }

    /**
     * What (if any) is the net in the subcell connected
     * to this particular alias.
     *
     * @param name the name in the parent.
     * @return the CellNet in the child, null if none.
     **/
    public CellNet getSubcellNetConnectedTo(HierName h) {
        ConnectionInfo ci = (ConnectionInfo) subcellports.get(h);
        if (ci == null) return null;
        HierName child = ci.getChildName(h);
        if (child == null) return null;
        return (CellNet) ci.child.getNet(child);
    }

    public ConnectionInfo getSubcellConnectedTo(HierName h) {
        return (ConnectionInfo) subcellports.get(h);
    }

    /**
     * Collection of all CellNets.
     **/
    public Set/*<CellNet>*/ getAllNets() {
        return allNets;
    }

    /**
     * Collection of all SubcellConnections (ConnectionInfo)
     **/
    public Collection<ConnectionInfo> getAllSubcellConnections() {
        return subcellconnections.values();
    }


    public Set/*<ConnectionInfo>*/ getAllInstances()
    {
        return new HashSet/*<ConnectionInfo>*/(getAllSubcellConnections());
    }

    /**
     * all subcells.  Usually not useful, as we generally want to know how they are connected.
     **/
    public Set/*<CellType>*/ getAllSubcells() {
        Collection/*<ConnectionInfo>*/ c = getAllSubcellConnections();
        Set/*<CellType>*/ s = new HashSet/*<CellType>*/(c.size());
        for (Iterator i = c.iterator(); i.hasNext(); ) {
            ConnectionInfo ci = (ConnectionInfo) i.next();
            s.add(ci.child);
        }
        return s;
    }

    /**
     * retrieve the "level" of this cell.
     *
     * The cell's level is one higher than the highest level of the subcells it contains.
     * A leaf cell has level 0.
     * 
     * @return The level of the cell.
     **/
    public int getLevel() {
        if (level < 0) {
            calculateLevel();
        }
        return level;
    }

    /**
     * Calculate the level of the cell.
     **/
    private void calculateLevel() {
        int max = -1;
        for (Iterator i = getAllSubcells().iterator(); i.hasNext(); ) {
            CellType c = (CellType) i.next();
            if (c.getLevel() > max) max = c.getLevel();
        }
        level = max + 1;
    }

    /**
     * returns the set of (NetGraph.NetEdge) driven by (i.e. gated by) net n.
     *
     * FIXME: does not actually work yet.
     **/
    public Set/*<NetGraph.NetEdge>*/ getTransistorsDrivenBy(CellNet n) {
        throw new AssertionError();
        // Set/*<NetGraph.NetEdge>*/ s = new HashSet/*<NetGraph.NetEdge>*/();
        // // HierName h = n.getName();
        // HierName h = null;
        // h = (HierName) namespace.getCanonicalKey(h);
        // for (Iterator i = transistors.getEdges().iterator(); i.hasNext(); ) {
        //     NetGraph.NetEdge prospective = (NetGraph.NetEdge) i.next();
        //     if (prospective.gate.name.equals(n)) {
        //         s.add(prospective);
        //     }
        // }
        // return s;
    }

    public String toString() {
        int numNonFragmentCatPaths = 0;

        for (Iterator ita = catPaths.iterator(); ita.hasNext(); ) {
            CatPath cpa = (CatPath)ita.next();
            if(!cpa.isFragment()){
                ++numNonFragmentCatPaths;
            }
        }
        
        return "Cell name: " + typeName + "\n" +
            "Subtype index: " + subtypenumber + "\n" +
            "Hier level: " + getLevel() + "\n" +
            "Number of Half-operators: " + listHalfOperators.size() + "\n" +
            "Number of sizing paths: " + sizingPaths.size() + "\n" +
            "Number of concatenated paths: " + catPaths.size() + "\n" +
            "Number of non-fragment concatenated paths: " + numNonFragmentCatPaths + "\n" +
            "Number of reduced concatenated paths: " + reducedCatPaths.size() + "\n";

//        return typeName + ":" + subtypenumber;
    }


    public void dumpInfo(BufferedWriter bw1, String prefix)
    {
        try{
            bw1.write(prefix + "CELL " + typeName + " {\n");
            bw1.write(prefix + "\tsubtype_index = " + subtypenumber + ";\n");
            bw1.write(prefix + "\thier_level = " + getLevel() + ";\n");
            bw1.write(prefix + "\tn_half_operators = " + listHalfOperators.size() + ";\n");

            // FIXME: possible cell/transistor mix in the future, don't use getLevel
            if(getLevel() == 0){
                bw1.write(prefix + "\tn_sizing_paths = " + sizingPaths.size() + ";\n");
            }
            else{
                bw1.write(prefix + "\tn_cat_paths = " + catPaths.size() + ";\n");
                bw1.write(prefix + "\tn_non_fragment_cat_paths = " + catPaths.size() + ";\n");
                bw1.write(prefix + "\tn_reduced_cat_paths = " + reducedCatPaths.size() + ";\n");
            }

            bw1.write(prefix + "\n");

            for (Iterator ita = allNets.iterator(); ita.hasNext(); ) {
                CellNet cna = (CellNet)ita.next();
                cna.dumpInfo(bw1, prefix + "\t");
            }

            bw1.write(prefix + "\n");

            // FIXME: possible cell/transistor mix in the future, don't use getLevel
            if(getLevel() == 0){
                for (Iterator ita = sizingPaths.iterator(); ita.hasNext(); ) {
                    SizingPath spa = (SizingPath)ita.next();
                    spa.dumpInfo(bw1, prefix + "\t");
                }

                bw1.write(prefix + "\n");
            }

            else{
                // ita = catPaths.iterator();
                for (Iterator ita = reducedCatPaths.iterator(); ita.hasNext(); ) {
                    CatPath cpa = (CatPath)ita.next();
                    cpa.dumpInfo(bw1, prefix + "\t");
                }

                bw1.write(prefix + "\n");
            }

            bw1.write(prefix + "}\n");
        }
        catch(IOException e){
            System.out.println("IO exception thrown: " + e);
        }
    }




    public void print()
    {
        System.out.println("Cell name: " + typeName);
        System.out.println("Subtype index: " + subtypenumber);
        System.out.println("Hier level: " + getLevel());
        // for (Iterator i = allNets.iterator(); i.hasNext(); ) {
        //     CellNet n = (CellNet) i.next();
        //     n.print();
        // }
        System.out.println();
    
    }

    /**
     * Creates a new instance graph with each cell being a refinement of the
     * corresponding cell in the original graph.
     *
     * At most one refinement per cell.
     *
     * @param flags What things the new instance hierarchy should have.
     * @return the subtype of the top-level cell.
     **/
    public CellType instanceMinimalSubTypes(int flags,
                                            String staticizer,
                                            String weakInverter, 
                                            String smallInverter,
                                            String[] gates,
                                            CastFileParser cfp,
                                            UnaryPredicate<CellType> netlistFromPrs) {
        return instanceMinimalSubTypes(flags, staticizer, weakInverter,
                                       smallInverter, gates, cfp,
                                       netlistFromPrs, null);
    }

    public CellType instanceMinimalSubTypes(
            int flags,
            String staticizer,
            String weakInverter, 
            String smallInverter,
            String[] gates,
            CastFileParser cfp,
            UnaryPredicate<CellType> netlistFromPrs,
            Map<AutoThreshold.Option,Boolean> autoThresholdOptions) {

        minimalInstancer m =
            new minimalInstancer(staticizer, weakInverter, smallInverter,
                                 gates, Vdd, GND, cfp, cad, netlistFromPrs,
                                 autoThresholdOptions);
        walkOnce(m, design.allCellTypes);

        // generate half-operators for all the cells
        design.generateHalfOperators(design.allCellTypes); 

        // Annotate all port cellnets with I/O directions
        CastDesign.addPortDirection(design.allCellTypes);

        // Generate source/sink lists for cellnets
        design.generateSourceSinkLists(design.allCellTypes);

        // Mark non-observable port nets
        CastDesign.markNonObservablePorts(design.allCellTypes);


        return m.newt;
    }

    public CellType instanceMinimalSubTypes(int flags,
                                            String staticizer,
                                            String weakInverter, 
                                            String smallInverter,
                                            String[] gates,
                                            CastFileParser cfp) {
        return instanceMinimalSubTypes(flags, staticizer, weakInverter, smallInverter,
                gates, cfp, new UnaryPredicate.Constant<CellType>(false));
    }

    private static class minimalInstancer implements CellTypeProcessor {
        CellType newt;
        final HierName Vdd, GND;
        NetGraph staticizer;
        NetGraph weakInverter;
        NetGraph smallInverter;
        NetGraph[] gates;
        final CastFileParser cfp;
        final Cadencize cad;
        final UnaryPredicate<CellType> netlistFromPrs;
        final Map<AutoThreshold.Option,Boolean> autoThresholdOptions;
   
        /**
         *  Constructor 
         *
         */
        public minimalInstancer(
                String staticizer, String weakInverter, 
                String smallInverter,
                String[] gates, HierName Vdd, HierName GND,
                CastFileParser cfp, Cadencize cad ,
                UnaryPredicate<CellType> netlistFromPrs,
                Map<AutoThreshold.Option,Boolean> autoThresholdOptions) {
            if (staticizer==null) this.staticizer=null;
            else try {
                this.staticizer = CastDesign.getGateNetGraph(cfp, staticizer, Vdd, GND, cad);
            } catch (Exception e) {
                this.staticizer = null;
                System.err.println("WARNING: Cannot load staticizer " + staticizer);
            }
            if (weakInverter==null) this.weakInverter=null;
            else try {
                this.weakInverter = CastDesign.getGateNetGraph(cfp, weakInverter, Vdd, GND, cad);
            } catch (Exception e) {
                this.weakInverter = null;
                System.err.println("WARNING: Cannot load weak-inverter " + weakInverter);
            }
            if (smallInverter==null) this.smallInverter=null;
            else try {
                this.smallInverter = CastDesign.getGateNetGraph(cfp, smallInverter, Vdd, GND, cad);
            } catch (Exception e) {
                this.smallInverter = null;
                System.err.println("WARNING: Cannot load small inverter " + smallInverter);
            }
            List gs = new ArrayList();
            for (int i = 0; i < gates.length; i++) {
                try {
                    NetGraph g = CastDesign.getGateNetGraph(cfp, gates[i], Vdd, GND, cad);
                    gs.add(g);
                } catch (Exception e) {
                    System.err.println("WARNING: Cannot load gate " + gates[i]);
                }
            }
            this.gates = new NetGraph[gs.size()];
            for (int i=0; i<gs.size(); i++) this.gates[i] = (NetGraph) gs.get(i);
            this.Vdd = Vdd;
            this.GND = GND;
            this.cfp = cfp;
            this.cad = cad;
            newt = null ;
            this.netlistFromPrs = netlistFromPrs;
            this.autoThresholdOptions = autoThresholdOptions;
        }

        /**
         * Get transistor types.
         *
         * @param tt Default transistor type
         * @param ftt Fast transistor type
         * @param stt Slow transistor type
         * @param ttup Existing transistor_type directive for pull-ups
         * @param ttdn Existing transistor_type directive for pull-downs
         * @param is idle_state directives
         * @param transistors Transistor netgraph
         * @param halfops Half-operators in the cell.
         * @return additional transistor_type directives that should be
         * annotated
         **/
        private Map<Pair<HierName,Boolean>,Integer> getTransistorTypes(
                final String typeName,                                                       
                final int tt,
                final int ftt,
                final int stt,
                final Map<HierName,Integer> ttup,
                final Map<HierName,Integer> ttdn,
                final Map<HierName,DirectiveUtils.IdleState> is,
                final NetGraph transistors,
                final List<HalfOperator> halfops,
                final Map<AutoThreshold.Option,Boolean> options) {
            final Map<Pair<HierName,Boolean>,Integer> result =
                AutoThreshold.getTransistorTypesMap(typeName,is,
                        transistors,stt,tt,ftt,ttup,ttdn,halfops,options);
            System.out.println("Result Map:"
                              +result);

            return result;
        }

        // Compute additional transistor_type directives
        private void updateTransistorTypes(final CellType c) {
            // default transistor type
            final Integer tt = (Integer)
                DirectiveUtils.getTopLevelDirective(c.cast_cell,
                        DirectiveConstants.TRANSISTOR_TYPE);

            // fast transistor type
            final Integer ftt = (Integer)
                DirectiveUtils.getTopLevelDirective(c.cast_cell,
                        DirectiveConstants.FAST_TRANSISTOR_TYPE);

            // slow transistor type
            final Integer stt = (Integer)
                DirectiveUtils.getTopLevelDirective(c.cast_cell,
                        DirectiveConstants.SLOW_TRANSISTOR_TYPE);

            // nothing to do if all transistor types are identical
            if (tt.intValue() == ftt.intValue() &&
                tt.intValue() == stt.intValue()) return;

            // existing transistor_type directives
            final Map ttype = DirectiveUtils.getPrsDirective(c.cast_cell,
                    DirectiveConstants.TRANSISTOR_TYPE,
                    DirectiveConstants.HALFOP_TYPE);
            final Map ttup = DirectiveUtils.canonizeKey(c.namespace,
                    DirectiveUtils.getUps(ttype));
            final Map ttdn = DirectiveUtils.canonizeKey(c.namespace,
                    DirectiveUtils.getDowns(ttype));

            // idle_state directives
            final Map<HierName,DirectiveUtils.IdleState> is =
                DirectiveUtils.getIdleState(c.cast_cell, c.namespace);

            // create half-operators to determine transistor sharing
            c.design.generateHalfOperator(c);

            // from idle_state directives, assign slow or fast transistor type
            // to each half-operator, existing transistor_type directives from
            // the user overrides
            final Map<Pair<HierName,Boolean>,Integer> results =
                getTransistorTypes(c.typeName,tt, ftt, stt, (Map<HierName,Integer>) ttup,
                                   (Map<HierName,Integer>) ttdn, is,
                                   c.transistors, c.getListHalfOperators(),
                                   autoThresholdOptions);

            c.getListHalfOperators().clear();

            // merge in transistor_type directive
            final DirectiveSource src =
                new DirectiveSource(BlockInterface.PRS);
            for (Map.Entry<Pair<HierName,Boolean>,Integer> r :
                    results.entrySet()) {
                src.definition(DirectiveConstants.TRANSISTOR_TYPE,
                               DirectiveConstants.HALFOP_TYPE,
                               r.getKey(), r.getValue());
            }

            c.cast_cell.getBlockInterface()
                       .iterator(BlockInterface.PRS).next()
                       .iterator(BlockInterface.DIRECTIVE)
                       .merge(new DirectiveBlock(src.getDirectiveInterface()));
        }

        public void processCellType(CellType c) {
            final boolean unimpl = ((Boolean) DirectiveUtils.getTopLevelDirective(c.cast_cell, DirectiveConstants.UNIMPL)).booleanValue();
            if (unimpl) {
                System.out.println("Note: Attempting to size " + c.typeName + ", which has directive unimplementable = true");
            }

            Map nostats = DirectiveUtils.getPrsDirective(
                    c.cast_cell, DirectiveConstants.NO_STAT,
                    DirectiveConstants.NODE_TYPE);
            Set nostatnodes = DirectiveUtils.canonize(c.namespace,
                    DirectiveUtils.getExplicitTrues(nostats));
            if (newt == null) newt = c;
            c.transistors = new NetGraph(c.namespace,
                                         c.exclusives, new ArrayList(),
                                         Vdd, GND,
                                         c.design.getTechnologyData().getMinimumTransistorWidth(),
                                         c.design.getTechnologyData().getDefaultGateLength(),
                                         nostatnodes);

            if (c.isJautoIgnore()) return;

            final boolean wantPrs = netlistFromPrs.evaluate(c);
            final CellInterface parent =
                c.cast_cell.getDirectRefinementParent();
            final boolean regenNetlist = wantPrs && !parent.containsNetlist();
            if (wantPrs && !regenNetlist) {
                System.err.println(
                        "WARNING: " + parent.getFullyQualifiedType() +
                        " contains netlist block; not regenerating " +
                        c.typeName + " from prs");
            }

            // Production rule insertion, gate matching and logic minimization
            try {
                if (autoThresholdOptions != null &&
                    regenNetlist &&
                    c.cast_cell.containsNetlist() &&
                    c.cast_cell.containsCompletePrs()) {
                    c.transistors.addCellInterface(c.cast_cell, gates, cfp,
                                                   cad);
                    c.transistors.prepareForLvs();
                    updateTransistorTypes(c);
                    c.transistors =
                        new NetGraph(c.namespace,
                                     c.exclusives, new ArrayList(),
                                     Vdd, GND,
                                     c.design.getTechnologyData().getMinimumTransistorWidth(),
                                     c.design.getTechnologyData().getDefaultGateLength(),
                                     nostatnodes);
                }

                if ((staticizer != null || weakInverter != null || smallInverter != null) &&
                    !regenNetlist) { 
                    c.transistors.addCellInterface(c.cast_cell, gates, cfp, cad);
                } else {
                    // In this case, we want to ignore staticizers that are in the
                    // netlist block, so we don't look at the netlist block. 
                    // This is used by Prs2Verilog --minimize-tri-reg option. 
                    c.transistors.addCellInterfacePrs(c.cast_cell, gates, cfp, cad);
                }

                if (!c.isFixedSize() && (regenNetlist ||
                                         !c.cast_cell.containsNetlist())) {
                    final double l =
                        c.design.getTechnologyData().getDefaultGateLength();
                    // set default gate length after netgraph generation
                    for (Iterator i = c.transistors.getEdges().iterator();
                         i.hasNext(); ) {
                        final NetGraph.NetEdge e = (NetGraph.NetEdge) i.next();
                        e.length = l;
                    }
                }
            } catch (UnimplementableProductionRuleException e) {
                System.err.println(e);
                System.exit(1);
            }
            c.transistors.prepareForLvs();

            // Assume transistors in a netlist block has already been properly
            // staticized and needs no further processing
            if (c.cast_cell.containsNetlist() &&
                !netlistFromPrs.evaluate(c)) return;

            // Insert staticizers
            if (staticizer != null || weakInverter != null || smallInverter != null) { 
                c.transistors.addStaticizers(gates, staticizer, weakInverter,
                                             smallInverter, true,
                                             c.cast_cell, cfp, cad);
            }

            // needs to be called before isStaticizerInverter() works.
            c.transistors.prepareForLvs();
        }
    }

    /**
     * Creates a new instance graph with each cell being a refinement of the
     * corresponding cell in the original graph.
     *
     * FIXME: does not work.
     * 
     * One refinement in each "context": name in parent.
     * 
     * @param flags What things the new instance hierarchy should have.
     * @return the subtype of the top-level cell.
     **/
    public CellType instanceStandardSubTypes(int flags) {
        return null;
        // this.transistors = new NetGraph(null, null, new ArrayList(), null, null, this);
    }


    
    public Set/*<SizingPath>*/ getSizingPaths()
    {
        return sizingPaths;
    }



    public Set/*<CatPath>*/ getCatPaths()
    {
        return catPaths;
    }


    public Set/*<CatPath>*/ getReducedCatPaths()
    {
        return reducedCatPaths;
    }


    public String checkPathCoverage()
    {
        HashSet/*<HalfOperator>*/ sizingPathHalfOps =
            new HashSet/*<HalfOperator>*/();
        for (Iterator ita = sizingPaths.iterator(); ita.hasNext(); ){
            SizingPath spa = (SizingPath)ita.next();
            sizingPathHalfOps.addAll(spa.getPath());
        }

        HashSet<HalfOperator> cellTypeHalfOps =
            new HashSet<HalfOperator>(listHalfOperators);

        if (sizingPathHalfOps.equals(cellTypeHalfOps)) {
            return "PASS";
        }
        else{
	    if(DebugOption.printLevel <= 3){
		System.out.println("---------------------------------------------------");
		System.out.println("HalfOperator number in SizingPath: " +
				   sizingPathHalfOps.size());
		System.out.println("HalfOperator number in CellType: " +
				   cellTypeHalfOps.size());
		
		printHalfOperatorSetDifference(sizingPathHalfOps, "SizingPath",
					       cellTypeHalfOps,   "CellType");
		printHalfOperatorSetDifference(cellTypeHalfOps,   "CellType",
					       sizingPathHalfOps, "SizingPath");
		
		System.out.println("---------------------------------------------------");
		
		System.out.println(transistors.cdlString());
            

	    }
	    return "FAIL";
        }
    }

    private void printHalfOperatorSetDifference(Set/*<HalfOperator>*/ set1,
                                                String setName1,
                                                Set/*<HalfOperator>*/ set2,
                                                String setName2) {
        HashSet/*<HalfOperator>*/ difference =
            new HashSet/*<HalfOperator>*/(set1);
        difference.removeAll(set2);
        System.out.println("HalfOperators in " + setName1 +
                           ", not in " + setName2 + ":");
        for (Iterator ita = difference.iterator();
             ita.hasNext(); ) {
            HalfOperator hoa = (HalfOperator) ita.next();
            System.out.println(
                "output net: " 
                + hoa.outputNet.canonicalName
                               .getCadenceString()
                + ", direction: "
                + hoa.driveDirection
                );

            for (Iterator itb = hoa.transistors.iterator();
                 itb.hasNext(); ) {
                NetGraph.NetEdge nea = (NetGraph.NetEdge) itb.next();

                System.out.println(
                    "gate: " 
                    + nea.gate.getName().getCadenceString()
                    + ", drain: "
                    + nea.drain.getName().getCadenceString()
                    + ", source: "
                    + nea.source.getName().getCadenceString()
                    );
            }
        }
    }


    public String checkHalfOperatorCoverage()
    {
        HashSet/*<NetGraph.NetEdge>*/ netgraphTransistors =
            new HashSet/*<NetGraph.NetEdge>*/();
        for (Iterator ita = transistors.getEdges().iterator(); ita.hasNext(); ) {
            NetGraph.NetEdge nea = (NetGraph.NetEdge)ita.next();

            if(nea.gate.isStaticizerInverter()){
                continue;
            }

            if(nea.source.isStaticizerInverter() || nea.drain.isStaticizerInverter()){
                continue;
            }

            netgraphTransistors.add(nea);
        }


        HashSet/*<NetGraph.NetEdge>*/ halfOpTransistors =
            new HashSet/*<NetGraph.NetEdge>*/();
        for (HalfOperator hoa : listHalfOperators) {
            halfOpTransistors.addAll(hoa.transistors);
        }

        if (netgraphTransistors.equals(halfOpTransistors)) {
            return "PASS";
        }
        else{
            System.out.println("---------------------------------------------------");
            System.out.println("Transistor number in NetGraph: " +
                               netgraphTransistors.size());
            System.out.println("Transistor number in HalfOperator: " +
                               halfOpTransistors.size());

            printTransistorSetDifference(netgraphTransistors, "NetGraph",
                                         halfOpTransistors,   "HalfOperator");
            printTransistorSetDifference(halfOpTransistors,   "HalfOperator",
                                         netgraphTransistors, "NetGraph");

            System.out.println("---------------------------------------------------");
            

            return "FAIL";
        }

    }

    private void printTransistorSetDifference(Set/*<NetGraph.NetEdge>*/ set1,
                                              String setName1,
                                              Set/*<NetGraph.NetEdge>*/ set2,
                                              String setName2) {
        HashSet/*<NetGraph.NetEdge>*/ difference =
            new HashSet/*<NetGraph.NetEdge>*/(set1);
        difference.removeAll(set2);
        System.out.println("Transistors in " + setName1 +
                           ", not in " + setName2 + ":");
        for (Iterator ita = difference.iterator(); ita.hasNext(); ) {
            NetGraph.NetEdge nea = (NetGraph.NetEdge)ita.next();
            System.out.println(
                "gate: " 
                + nea.gate.getName().getCadenceString()
                + ", drain: "
                + nea.drain.getName().getCadenceString()
                + ", source: "
                + nea.source.getName().getCadenceString()
                );
        }
    }


    public boolean hasPaths()
    {
        // FIXME: possible cell/transistor mix in the future, don't use getLevel
        if(getLevel() == 0){
            if(DebugOption.printLevel <= 3){
                System.out.println("Number of sizing paths: " + sizingPaths.size());
            }

            for (Iterator ita = sizingPaths.iterator(); ita.hasNext(); ) {
                SizingPath spa = (SizingPath)ita.next();
                if(!spa.isFragment()){
                    return true;
                }
            }
        }
        else{
            if(DebugOption.printLevel <= 3){
                System.out.println("Number of sizing paths: " + sizingPaths.size());
                System.out.println("Number of concatenated paths: " + catPaths.size());
            }

            for (Iterator ita = catPaths.iterator(); ita.hasNext(); ) {
                CatPath cpa = (CatPath)ita.next();
                if(!cpa.isFragment()){
                    return true;
                }
            }
        }

        return false;
    }


    public int getNumPaths()
    {
        int numPaths = 0;

        // FIXME: possible cell/transistor mix in the future, don't use getLevel
        if(getLevel() == 0){
            if(DebugOption.printLevel <= 3){
                System.out.println("Number of sizing paths: " + sizingPaths.size());
            }

            for (Iterator ita = sizingPaths.iterator(); ita.hasNext(); ) {
                SizingPath spa = (SizingPath)ita.next();
                if(!spa.isFragment()){
                    ++numPaths;
                }
            }
        }
        else{
            if(DebugOption.printLevel <= 3){
                System.out.println("Number of sizing paths: " + sizingPaths.size());
                System.out.println("Number of concatenated paths: " + catPaths.size());
            }

            for (Iterator ita = catPaths.iterator(); ita.hasNext(); ) {
                CatPath cpa = (CatPath)ita.next();
                if(!cpa.isFragment()){
                    ++numPaths;
                }
            }
        }

        return numPaths;
    }



    public void generateSizingHalfOperators()
    {
        // FIXME: possible cell/transistor mix in the future, don't use getLevel
        if (getLevel() == 0) { // Leaf cell
            for (Iterator ita = sizingPaths.iterator(); ita.hasNext(); ) {
                SizingPath spa = (SizingPath)ita.next();

                if(!spa.isFragment()){
                    setSizingHalfOperators.addAll(spa.getPath());
                }
            }
        }
        else{ // Non-leaf cell
            for (Iterator ita = reducedCatPaths.iterator(); ita.hasNext(); ) {
                CatPath cpa = (CatPath)ita.next();

                if(!cpa.isFragment()){
                    for (Iterator itb = cpa.getCatPath().iterator(); itb.hasNext(); ) {
                        SizingPath spa = (SizingPath)itb.next();

                        setSizingHalfOperators.addAll(spa.getPath());
                    }
                }
            }
        }
        
    }




    public boolean isSharingSizingHalfOperatorsWith(CellType ct1)
    {
        HashSet/*<HalfOperator>*/ seta =
            new HashSet/*<HalfOperator>*/(setSizingHalfOperators);

        seta.retainAll(ct1.setSizingHalfOperators);

        return !seta.isEmpty();
    }

    /** Is this the top-level Jauto sizing cell? **/
    public boolean isTopLevel()
    {
        return (typeName.equals(""));
    }

    public boolean isFloorplanned()
    {
        if (isTopLevel()) return false;
        return ((Boolean) DirectiveUtils.getTopLevelDirective(cast_cell, DirectiveConstants.FLOORPLAN)).booleanValue();
    }


    public boolean isFragment()
    {
        return isFragment;
    }


    public boolean setIsFragment(boolean b1)
    {
        isFragment = b1;

        return isFragment;
    }


    public boolean hasNonObservablePorts()
    {
        for (Iterator ita = getAllNets().iterator(); ita.hasNext(); ) {
            CellNet cna = (CellNet)ita.next();

            if((cna.isPortNet()) && (!cna.isObservable())){
                return true;
            }
        }

        return false;
    }


    /** Returns true if a cell with a netlist block has the fixed size
     *  directive set to true.  */
    public final boolean isFixedSize()
    {
        return CellUtils.isFixedSize(cast_cell);
    }

    public final double getAssignedWireWidth() {
        final Float width = (Float) DirectiveUtils.getTopLevelDirective(cast_cell, DirectiveConstants.WIREWIDTH);
        return width.doubleValue();
    }

    public final double getAssignedWireSpace() {
        final Float width = (Float) DirectiveUtils.getTopLevelDirective(cast_cell, DirectiveConstants.WIRESPACE);
        return width.doubleValue();
    }

    public final double getAssignedWireLength() {
        final Float length = (Float) DirectiveUtils.getTopLevelDirective(cast_cell, DirectiveConstants.WIRELENGTH);
        return length.doubleValue();
    }

    public final double getAssignedWireSpan() {
        final Float span = (Float) DirectiveUtils.getTopLevelDirective(cast_cell, DirectiveConstants.WIRESPAN);
        return span.doubleValue();
    }

    public final double getAssignedMinWireLength() {
        final Float minlen = (Float) DirectiveUtils.getTopLevelDirective(cast_cell, DirectiveConstants.MIN_WIRELENGTH);
        return minlen.doubleValue();
    }

    public final double getAssignedMinWireSpan() {
        final Float minspan = (Float) DirectiveUtils.getTopLevelDirective(cast_cell, DirectiveConstants.MIN_WIRESPAN);
        return minspan.doubleValue();
    }

    private void addCutPaths(final Map m) {
        final Set cutpaths = DirectiveUtils.getExplicitTrues(m);
        for (Iterator i = cutpaths.iterator(); i.hasNext(); ) {
            final HierName h = (HierName) i.next();
            final CellNet n = getNet(h);
            n.setCutPath(true);
        }
    }

    private void addCutPaths() {
        /* Strictly speaking, only one of these should ever apply to a cell,
        since the subcell block is mutually exclusive with the prs block, but
        it's harmless to try both. */
        final Map prsCutpath = DirectiveUtils.getPrsDirective(cast_cell, DirectiveConstants.CUTPATH, DirectiveConstants.NODE_TYPE);
        final Map subcellCutpath = DirectiveUtils.getSubcellDirective(cast_cell, DirectiveConstants.CUTPATH, DirectiveConstants.NODE_TYPE);
        addCutPaths(prsCutpath);
        addCutPaths(subcellCutpath);
        final Set/*<HierName>*/ nocutpaths = new HashSet/*<HierName>*/();
        nocutpaths.addAll(DirectiveUtils.canonize(namespace, DirectiveUtils.getExplicitFalses(prsCutpath)));
        nocutpaths.addAll(DirectiveUtils.canonize(namespace, DirectiveUtils.getExplicitFalses(subcellCutpath)));
        /* If a subcell has been inlined, preserve implied cutpath on its port
         * nodes, if the subcell is not non-observable, and if a port is not
         * explicitly declared as cutpath=false.  A possible fix for BUG 1289.
         */
        for (Iterator i = cast_cell.getAllSubcellPairs(); i.hasNext(); ) {
            final Pair p = (Pair) i.next();
            final HierName instance = (HierName) p.getFirst();
            final CellInterface ci = (CellInterface) p.getSecond();
            final boolean nonobs = ((Boolean) DirectiveUtils.getTopLevelDirective(ci, DirectiveConstants.CELLNONOBSERVABLE)).booleanValue();
            if (cast_cell.isInlinedSubcell(instance) && !nonobs) {
                final CadenceInfo cinfo = cad.convert(ci);
                final AliasedMap map = cinfo.getPortNodes();
                for (Iterator j = map.getCanonicalKeys(); j.hasNext(); ) {
                    final HierName port = (HierName) j.next();
                    final HierName full = (HierName) namespace.getCanonicalKey(HierName.append(instance, port));
                    Debug.assertTrue(full != null, "Cannot find canonical name for " + instance + "." + port + " in " + typeName);
                    if (nocutpaths.contains(full)) continue;
                    final CellNet n = getNet(full);
                    Debug.assertTrue(n != null, "Cannot find inlined net " + port + " in inlined cell " + ci.getFullyQualifiedType() + " in " + typeName);
                    n.setCutPath(true);
                }
            }
        }
    }

    public CellDelay getDelay() {
        return delay;
    }

    public float getTau() {
        return -1;
    }
    
    public boolean isJautoIgnore() {
        return ((Boolean) DirectiveUtils.getTopLevelDirective(cast_cell, DirectiveConstants.JAUTO_IGNORE)).booleanValue();
    }

    public boolean isInternalEnv() {
        return isInternalEnv;
    }

    private static final class NameComparator implements Comparator {
        private static NameComparator singleton = null;
        private NameComparator() { }
        public static NameComparator getInstance() {
            if (singleton == null) singleton = new NameComparator();
            return singleton;
        }
        public int compare(Object o1, Object o2) {
            return ObjectUtils.compare(((CellType) o1).typeName,
                                       ((CellType) o2).typeName);
        }
        public boolean equals(Object o) {
            return o == this;
        }
    }

    public static Comparator getNameComparator() {
        return NameComparator.getInstance();
    }
}
