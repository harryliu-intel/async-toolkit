/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.fast;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.TreeSet;

import java.io.*;

import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cell.CellUtils;
import com.avlsi.file.common.HierName;
import com.avlsi.netlist.AbstractDeviceIterator;
import com.avlsi.netlist.impl.SimpleAbstractDeviceIterator;
import com.avlsi.tools.jauto.GlobalNet;
import com.avlsi.tools.jauto.NetSource;
import com.avlsi.tools.jauto.NetSink;
import com.avlsi.tools.jauto.NetType;
import com.avlsi.tools.lvs.NetGraph;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.ObjectUtils;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.functions.Functional;
import com.avlsi.util.functions.BinaryFunction;

/**
 * Class for representing a net in a cell. Used by CellType. In
 * contrast to GlobalNet, which represents a net spanning several
 * levels of hierarchy, a CellNet only includes nodes which are
 * defined within a single cell.
 *
 * A "net" is a set of nodes which are either the same or are
 * connected by wires; something like a vertex in a circuit graph.
 * Nodes in a CellNet must be all defined in the same cell, and so
 * must be related to each other by an alias expression "a=b".
 *
 * @author Aaron Denney
 * @version $Date$
 **/

public class CellNet {

    static final HierName Reset = HierName.makeHierName("Reset");
    static final HierName _Reset = HierName.makeHierName("_Reset");

    public interface Geometry {
        /** Map a name that is part of a subcircuit call. */
        HierName mapName(final CellNet port, final HierName alias);

        /** Map a name that is internal to a leaf cell. */
        HierName mapName(final CellNet net, final boolean gate);
        Pair[] getRC(final HierName alias1, final HierName alias2);
        AbstractDeviceIterator getDeviceIterator();
    }

    private static Comparator nameComparator = null;
    public static Comparator getComparator() {
        if (nameComparator == null) {
            nameComparator = new Comparator() {
                public int compare(Object o1, Object o2) {
                    final CellNet cn1 = (CellNet) o1;
                    final CellNet cn2 = (CellNet) o2;
                    int x = ObjectUtils.compare(cn1.container.typeName,
                                                cn2.container.typeName);
                    if (x != 0) return x;
                    return ObjectUtils.compare(cn1.canonicalName,
                                               cn2.canonicalName);
                }
                public boolean equals(Object o) {
                    return this == o;
                }
            };
        }
        return nameComparator;
    }

    public final CellType container;
    public final Set/*<HierName>*/ internalNames;
    public final Set/*<HierName>*/ portNames;
    public final Set/*<HierName>*/ subcellconnectionNames;
    private Set/*<HierName>*/ wiringConnectionNames;
    public final HierName canonicalName;
    private Geometry geometry;

    public static final int INPUT = 0;
    public static final int OUTPUT = 1;
    public static final int INPUTOUTPUT = 2;
    public static final int UNKNOWN = 3;

    // I/O direction for port net
    // 0 = INPUT
    // 1 = OUTPUT
    // 2 = INPUT/OUTPUT
    // 3 = UNKNOWN
    public int             portDirection = UNKNOWN;
    public boolean         visited = false;
    
    public final List/*<NetSource>*/ listSources =
        new ArrayList/*<NetSource>*/();
    public final List/*<NetSink>*/ listSinks =
        new ArrayList/*<NetSink>*/();

    public List<GlobalNet> globalNets = new ArrayList<GlobalNet>();

    boolean                 isObservable = true;
    boolean                 cutpath = false;

    private double upSize = -1, downSize = -1;
    private final HierName Vdd, GND, _RESET;

    /**
     * Constructor from no-layout information.
     **/
    CellNet(CellType container,
            Set/*<HierName>*/ internalNames,
            Set/*<HierName>*/ portNames,
            Set/*<HierName>*/ subcellconnectionNames,
            HierName canonicalName,
            HierName Vdd, HierName GND, HierName _RESET) {
        this.container = container;
        this.internalNames = internalNames;
        this.portNames = portNames;
        this.subcellconnectionNames = subcellconnectionNames;
        this.wiringConnectionNames = Collections.EMPTY_SET;
        this.geometry = null;
        this.Vdd = Vdd;
        this.GND = GND;
        this._RESET = _RESET;
        this.canonicalName = canonicalName;
    }

    /**
     * Constructor from layout-information, inheriting from other net.
     **/
    CellNet(CellNet prototype, CellType container) {
        Debug.assertTrue(container.prototype == prototype.container);
        this.container = container;
        this.internalNames = prototype.internalNames;
        this.portNames = prototype.portNames;
        this.subcellconnectionNames = prototype.subcellconnectionNames;
        this.wiringConnectionNames = prototype.wiringConnectionNames;
        this.canonicalName = prototype.canonicalName;
        this.geometry = null; //FIXME
        this.Vdd = prototype.Vdd;
        this.GND = prototype.GND;
        this._RESET = prototype._RESET;
    }

    public Geometry getGeometry() {
        return geometry;
    }

    public void setGeometry(Geometry geometry) {
        this.geometry = geometry;
    }

    /**
     * hack to approximate lexicographic ordering.  
     *
     * The size of the various arrays corresponds to the index of the highest value of that type.
     *
     * Hmm.  Totally wrong.  need to get smallest element of each set.
     **/
    public int compareTo(CellNet o) {
        if (o == this) return 0;
        assert o.container == container
             : "Can't compare nets in different cells!";
        if (o.internalNames.size() < internalNames.size()) return -1;
        else if (o.internalNames.size() > internalNames.size()) return 1;
        else if (o.portNames.size() < portNames.size()) return -1;
        else if (o.portNames.size() > portNames.size()) return 1;
        else if (o.subcellconnectionNames.size() < subcellconnectionNames.size()) return -1;
        else if (o.subcellconnectionNames.size() > subcellconnectionNames.size()) return 1;
        else return 0;
    }

    public int compareTo(Object o) {
        return compareTo((CellNet) o);
    }

    public boolean isPortNet() {
        return this.portNames.size() > 0;
    }

    public boolean isReset()
    {
        return (portNames.contains(_RESET) || portNames.contains(Reset) || portNames.contains(_Reset));
    }

    public boolean isVdd()
    {
        return portNames.contains(Vdd);
    }

    public boolean isGND()
    {
        return portNames.contains(GND);
    }

    public boolean connectedToSubcells() {
        return this.subcellconnectionNames.size() > 0;
    }

    public boolean isInternalNet() {
        return !isPortNet();
    }

    private LinkedHashMap/*<ConnectionInfo,Set<CellNet>>*/
    getConnectionInfoMap() {
        LinkedHashMap/*<ConnectionInfo,Set<CellNet>>*/ map =
            new LinkedHashMap/*<ConnectionInfo,Set<CellNet>>*/();
        for (Iterator i = subcellconnectionNames.iterator(); i.hasNext(); ) {
            HierName j = (HierName) i.next();
            ConnectionInfo ci = (ConnectionInfo) container.subcellports.get(j);
            HierName child = ci.getChildName(j);
            CellNet sub = ci.child.getNet(child);
            Set/*<CellNet>*/ s = (Set) map.get(ci);
            if (s == null) {
                s = new LinkedHashSet/*<CellNet>*/();
                map.put(ci,s);
            }
            s.add(sub);
        }
        return map;
    }

    /**
     * Nets in subcells this connects to.
     * List&gt;CellNet&lt;.
     *
     * For cells that are connected to multiple times, we want one net for each cell instance.
     **/
    public List/*<CellNet>*/ getSubcellNets() {
        LinkedHashMap/*<ConnectionInfo,Set<CellNet>>*/ map =
            getConnectionInfoMap();
        List/*<CellNet>*/ rv =
            new ArrayList/*<CellNet>*/(map.size());
        for (Iterator i = map.values().iterator(); i.hasNext(); ) {
            rv.addAll((Set)i.next());
        }
        return rv;
    }


    /**
     * Return at most one cellnet for each net we can be connected to via any
     * instance of a subcell.
     **/
    public Set<CellNet> getSetSubcellNets(boolean includeWiringSubcells) {
        Set<CellNet> rv = new LinkedHashSet<CellNet>();
        for (Iterator i = subcellconnectionNames.iterator(); i.hasNext(); ) {

            HierName j = (HierName) i.next();
            CellNet sub = container.getSubcellNetConnectedTo(j);
            rv.add(sub);
        }
        if (includeWiringSubcells) {
            for (Iterator i = wiringConnectionNames.iterator(); i.hasNext(); ) {

                HierName j = (HierName) i.next();
                CellNet sub = container.getSubcellNetConnectedTo(j);
                rv.add(sub);
            }
        }
        return rv;
    }

    public Set<CellNet> getSetSubcellNets() {
        return getSetSubcellNets(false);
    }

    /**
     * return a list of list of cellnets we are connected to in subcells,
     * organized by instance.
     **/
    public List/*<List<CellNet>>*/ getListListSubcellNets() {
        LinkedHashMap/*ConnectionInfo,Set<CellNet>>*/ map =
            getConnectionInfoMap();
        List/*<List<CellNet>>*/ rv =
            new ArrayList/*<List<CellNet>>*/(map.size());
        for (Iterator i = map.values().iterator(); i.hasNext(); ) {
            Set s = (Set) i.next();
            rv.add(new ArrayList(s));
        }
        return rv;
    }

    // REVIEW: Do these fields need to be copied, or can we just
    // keep a reference to the ConnectionInfo to save memory?
    // How much memory do we use for InstanceInfos?
    public static final class InstanceInfo {
        final ConnectionInfo      inst;
        final Set/*<CellNet>*/    setSubcellNets;
        final double              coordinateX;
        final double              coordinateY;
        final int                 orientation;
        
        private InstanceInfo(ConnectionInfo ci,
                             Set/*<CellNet>*/ subcellNets)
        {
            inst = ci;
            setSubcellNets = new LinkedHashSet/*<CellNet>*/(subcellNets);
            coordinateX = ci.xOffset;
            coordinateY = ci.yOffset;

            orientation = ci.orientation;
        }
    }

    /**
     * return a list of connection information for all the instances connectted to this net
     * includes cellnets in instances and x-y coordinates of the instances
     **/
    public List/*<InstanceInfo>*/ getListInstanceInfo()
    {
        LinkedHashMap/*<ConnectionInfo,Set<CellNet>>*/ map =
            getConnectionInfoMap();
        ArrayList/*<InstanceInfo>*/ rv =
            new ArrayList/*<InstanceInfo>*/(map.size());
        for (Iterator i = map.entrySet().iterator(); i.hasNext(); ) {
            Map.Entry/*<ConnectionInfo,Set<CellNet>>*/ entry =
                (Map.Entry) i.next();
            ConnectionInfo ci = (ConnectionInfo) entry.getKey();
            Set s = (Set) entry.getValue();

            rv.add(new InstanceInfo(ci, s));
        }

        return rv;

    }



    /**
     * Make us part of GlobalNet g.
     **/
    public void addGlobalNet(GlobalNet g) {
        globalNets.add(g);
    }

    /**
     * What global nets are we part of?
     **/
    public List<GlobalNet> getGlobalNets() {
        return globalNets;
    }

    /**
     * Get the list of sources
     **/
    public List/*<NetSource>*/ getListSources(){
        return listSources;
    }
    
    /**
     * Get the list of sinks
     **/
    public List/*<NetSink>*/ getListSinks(){
        return listSinks;
    }
    
    public void print( final PrintWriter p ) {
        p.print( toString() );
    }

    /**
     * Dump out information for debug
     @deprecated Library code should not be using System.out directly.
     **/
    public void print()
    {
        final PrintWriter p = new PrintWriter( System.out );
    }


    public static String portDirectionString(int dir) {
        switch(dir){
            case INPUT:  return "INPUT";
            case OUTPUT:  return "OUTPUT";
            case INPUTOUTPUT:  return "IN/OUT";
            case UNKNOWN:  return "UNKNOWN";
            default: throw new AssertionError("Unknown port direction " +
                                              dir);
        }
    }

    public String toString()
    {
        String s = "";

        if(portNames.size() != 0){
            s += portDirectionString(portDirection);
            s += "\tport net" + "\n";
        }
        else{
            s += "\tinternal net" + "\n";
        }


        s += "\tnumber of internal names: " + internalNames.size() + "\n";
        for (Iterator itc = internalNames.iterator(); itc.hasNext(); ) {
            HierName hna = (HierName)itc.next();
            s += "\t\t" + hna.getCadenceString() + "\n";
        }

        s += "\tnumber of port names: " + portNames.size() + "\n";
        for (Iterator itc = portNames.iterator(); itc.hasNext(); ) {
            HierName hna = (HierName)itc.next();
            s += "\t\t" + hna.getCadenceString() + "\n";
        }

        s += "\tnumber of subcellconnection names: " + subcellconnectionNames.size() + "\n";
        for (Iterator itc = subcellconnectionNames.iterator(); itc.hasNext(); ) {
            HierName hna = (HierName)itc.next();
            s += "\t\t" + hna.getCadenceString() + "\n";
        }


        s += "\tnumber of subcell nets: " + getSetSubcellNets().size() + "\n";


        s += "\tnumber of sources: " + listSources.size() + "\n";
        for (Iterator itc = listSources.iterator(); itc.hasNext(); ) {
            NetSource nsra = (NetSource)itc.next();
            s += "\t\ttype: " + nsra.getType() + "\n";
        }

        s += "\tnumber of sinks: " + listSinks.size() + "\n";
        for (Iterator itc = listSinks.iterator(); itc.hasNext(); ) {
            NetSink nska = (NetSink)itc.next();
            s += "\t\ttype: " + nska.getType() + "\n";
        }

        s += "\tnumber of global nets: " + globalNets.size() + "\n";


        s += "\n";

        return s;
    }



    public void dumpInfo(BufferedWriter bw1, String s1)
    {
        try{
            bw1.write(s1 + "CELL_NET " + canonicalName.getCadenceString() + " {\n");
            bw1.write(s1 + "\tis_staticized = " + isStaticized() + ";\n");
            bw1.write(s1 + "\tis_port = " + isPortNet() + ";\n");
            if(isPortNet()){
                bw1.write(s1 + "\tport_direction = " + portDirection + ";\n");
            }
            else{
                bw1.write(s1 + "\tgate_cap = " + getGateCap() + ";\n");
                bw1.write(s1 + "\twire_cap = " + getWireCap() + ";\n");
                bw1.write(s1 + "\twire_res = " + getWireRes() + ";\n");
            }
            bw1.write(s1 + "\tn_sources = " + listSources.size() + ";\n");
            bw1.write(s1 + "\tn_sinks = " + listSinks.size() + ";\n");



            bw1.write(s1 + "\n");

            for (Iterator ita = globalNets.iterator(); ita.hasNext(); ) {
                GlobalNet gna = (GlobalNet)ita.next();
                gna.dumpInfo(bw1, s1 + "\t");
            }

            bw1.write(s1 + "}\n");
        }
        catch(IOException e){
            System.out.println("IO exception thrown: " + e);
        }
    }


    public double getGateCap()
    {
        assert getGlobalNets().size() == 1
            : "Port net should not call this function.";

        GlobalNet gna = (GlobalNet) getGlobalNets().get(0);

        double gateCap = 0.0;

        for (Iterator ita = gna.getListSinks().iterator(); ita.hasNext(); ) {
            NetSink nska = (NetSink)ita.next();
            if(nska.type == NetType.HALF_OPERATOR_TRANSISTOR){
                NetGraph.NetEdge nea = nska.transistor;
                gateCap += nea.width / nea.shareCount;
            }
            assert nska.type != NetType.CELL
                : "Netsink of type CELL should be cleared at this stage";
            if(nska.type == NetType.CAPACITIVE_LOAD){
                // System.out.println("Warning: Netsink of type CAPACITIVE_LOAD should not exist in testbenchs");
            }

        }

        return gateCap;
    }


    public double getWireCap()
    {
        assert getGlobalNets().size() == 1
            : "Port net should not call this function.";

        return ((GlobalNet) getGlobalNets().get(0)).getWireCapacitance();
    }


    public double getWireRes()
    {
        assert getGlobalNets().size() == 1
            : "Port net should not call this function.";

        return ((GlobalNet) getGlobalNets().get(0)).getWireResistance();
    }



    // Used by cast design to correct names that aren't local names or
    // wiring cell names.
    void changeToSubcellconnection(HierName h, boolean wiring) {
        if (wiring) {
            if (wiringConnectionNames == Collections.EMPTY_SET) {
                wiringConnectionNames = new TreeSet();
            }
            wiringConnectionNames.add(h);
        } else if (internalNames.contains(h)) {
            internalNames.remove(h);
            subcellconnectionNames.add(h);
        }
    }

    // We want the same notion of most canonical as the NetGraph uses.
    // While it may be prettier to have names in the port list have
    // priority, it breaks things.
    private HierName getMostCanonical() {
        Set/*<HierName>*/ allNames = new TreeSet/*<HierName>*/();
        allNames.addAll(internalNames);
        allNames.addAll(portNames);
        return (HierName) findCannon(allNames);
    }
    
    private HierName findCannon(Set names) {
        Iterator i = names.iterator();
        HierName best = null;
        if (i.hasNext()) {
            best = (HierName) i.next();
        }
        while (i.hasNext()) {
            HierName next = (HierName) i.next();
            if (next.compareTo(best) < 0) {
                best = next;
            }
        }
        return best;
    }

    /**
     * returns constant capacitance value assigned by load directives
     **/
    public double getAssignedLoad()
    {
        return 0.0;
    }



    public boolean hasOnlyCellTypeSources()
    {
        if(listSources.size() == 0){
            return false;
        }

        for (Iterator ita = listSources.iterator(); ita.hasNext(); ) {
            NetSource nsra = (NetSource)ita.next();
            if(nsra.getType() != NetType.CELL){
                return false;
            }
        }

        return true;
    }



    public Set/*<NetSink>*/ getHalfOperatorSinks()
    {
        Set/*<NetSink>*/ seta = new LinkedHashSet/*<NetSink>*/();

        for (Iterator ita = listSinks.iterator(); ita.hasNext(); ) {
            NetSink nska = (NetSink)ita.next();
            if(nska.getType() == NetType.HALF_OPERATOR_TRANSISTOR){
                seta.add(nska);
            }
        }

        return seta;

    }


    public boolean hasSinkAs(HalfOperator ho1)
    {
        for (Iterator ita = listSinks.iterator(); ita.hasNext(); ) {
            NetSink nska = (NetSink)ita.next();
            if(nska.getType() == NetType.HALF_OPERATOR_TRANSISTOR){
                if(nska.sink == ho1){
                    return true;
                }
            }
        }

        return false;
    }



    /**
       Gets all the ConnectionInfo objects the connect the encapsulated net to a net in a subcell.
       @see com.avlsi.fast.ConnectionInfo
       @return A set containing ConnectionInfo instances for all the subcell instances
       that the encapsulated net connects to.
     */
    public Set<ConnectionInfo> getSubcellsConnectedTo( ) {
        return container.getSubcellsConnectedTo( canonicalName );
    }


    public final boolean isObservable()
    {
        return isObservable;
    }


    public final boolean setIsObservable(boolean b1)
    {
        isObservable = b1;

        return isObservable;
    }


    public final boolean isStaticized()
    {
        LinkedHashSet/*<HalfOperator>*/ seta =
            new LinkedHashSet/*<HalfOperator>*/();

        for (Iterator ita = getGlobalNets().iterator(); ita.hasNext(); ) {
            GlobalNet gna = (GlobalNet)ita.next();

            for (Iterator itb = gna.getListSources().iterator(); itb.hasNext(); ) {
                NetSource nsra = (NetSource)itb.next();
                
                if(nsra.getType() == NetType.HALF_OPERATOR_TRANSISTOR){ //half-operator-type net source
                    seta.add(nsra.source);
                }
            }
        }

        for (Iterator ita = seta.iterator(); ita.hasNext(); ) {
            HalfOperator hoa = (HalfOperator)ita.next();
            NetGraph.NetNode nna = hoa.outputNode;

            if(nna.isStaticized()){
                return true;
            }
        }

        return false;
    }

    public final boolean isCutPath() {
        return cutpath;
    }

    public final boolean setCutPath(boolean v) {
        cutpath = v;
        return cutpath;
    }

    private final Double getLocalNodeDirective(final String dir) {
        final Map m = DirectiveUtils.getSubcellDirective(container.cast_cell, dir, DirectiveConstants.NODE_TYPE);
        for (Iterator i = internalNames.iterator(); i.hasNext(); ) {
            final HierName name = (HierName) i.next();
            final Float f = (Float) m.get(name);
            if (f != null) {
                return new Double(f.doubleValue());
            }
        }
        return null;
    }

    private final Float findMatchingName(final Map prsDirs,
                                         final Map subcellDirs,
                                         final Map topDirs,
                                         final Set/*<HierName>*/ names,
                                         Float val,
                                         final BinaryFunction combine) {
        for (Iterator i = names.iterator(); i.hasNext(); ) {
            final HierName name = (HierName) i.next();

            // Jauto does not model a wire as segments, each of which may have
            // different wiring attributes, as is partially expressible by
            // directives; instead, we use the most conservative wiring
            // attributes, and ignore where the directives are specified.
            final Float prsVal = (Float) prsDirs.get(name);
            if (prsVal != null)
                val = val == null ? prsVal
                                  : (Float) combine.execute(prsVal, val);

            final Float subcellVal = (Float) subcellDirs.get(name);
            if (subcellVal != null)
                val = val == null ? subcellVal
                                  : (Float) combine.execute(subcellVal, val);

            final Float topVal = (Float) topDirs.get(name);
            if (topVal != null)
                val = val == null ? topVal
                                  : (Float) combine.execute(topVal, val);
        }
        return val;
    }

    private final double getWireAttr(final String dir,
                                     final Float def,
                                     final BinaryFunction combine) {
        // Each of the wire attributes may be defined in the top-level cell,
        // the subcells block, and the prs block.
        final Map prs =
            DirectiveUtils.containsDirective(container.cast_cell,
                                             BlockInterface.PRS, dir,
                                             DirectiveConstants.NODE_TYPE) ?
            DirectiveUtils.getPrsDirective(container.cast_cell, dir,
                                           DirectiveConstants.NODE_TYPE)
          : Collections.EMPTY_MAP;

        final Map subcell =
            DirectiveUtils.containsDirective(container.cast_cell,
                                             BlockInterface.SUBCELL, dir,
                                             DirectiveConstants.NODE_TYPE) ?
            DirectiveUtils.getSubcellDirective(container.cast_cell, dir,
                                               DirectiveConstants.NODE_TYPE)
          : Collections.EMPTY_MAP;

        final Map top =
            DirectiveUtils.containsDirective(container.cast_cell,
                                             BlockInterface.CELL, dir,
                                             DirectiveConstants.NODE_TYPE) ?
            DirectiveUtils.getTopLevelDirective(container.cast_cell, dir,
                                                DirectiveConstants.NODE_TYPE)
          : Collections.EMPTY_MAP;

        Float result = null;

        /* XXX: The directives at the top-level can only parameterize on
         * port nodes.  However, what names go into internalNames, and what
         * go into portNames is confusing.  For example, if e1of4 -L is in
         * the port list, then L.d[1] is in portNames, but L.1 is in
         * internalNames.  Thus, we need to check both sets. */
        result = findMatchingName(prs, subcell, top, internalNames, result,
                                  combine);
        result = findMatchingName(prs, subcell, top, portNames, result,
                                  combine);
        result = findMatchingName(prs, subcell, top, subcellconnectionNames,
                                  result, combine);

        if (result == null) result = def;

        return result == null ? -1 : result.doubleValue();
    }

    private final double getWireAttr(final String dir,
                                     final BinaryFunction combine) {
        return getWireAttr(dir, null, combine);
    }

    // Cached wire attributes associated with this net
    private double assignedMinWireLength = Double.NaN;
    private double assignedMinWireSpan = Double.NaN;
    private double assignedWireLength = Double.NaN;
    private double assignedWireSpan = Double.NaN;
    private double internalWireLength = Double.NaN;
    private double assignedWireWidth = Double.NaN;
    private double assignedWireSpace = Double.NaN;
    private double assignedLoadCap = Double.NaN;
    private double resistanceScale = Double.NaN;

    public final double getAssignedMinWireLength(){
        if (Double.isNaN(assignedMinWireLength)) 
            assignedMinWireLength =
                getWireAttr(DirectiveConstants.MIN_WIRELENGTH,
                            DirectiveUtils.MAX);
        assert !Double.isNaN(assignedMinWireLength);
        return assignedMinWireLength;
    }

    public final double getAssignedMinWireSpan(){
        if (Double.isNaN(assignedMinWireSpan)) 
            assignedMinWireSpan =
                getWireAttr(DirectiveConstants.MIN_WIRESPAN,
                            DirectiveUtils.MAX);
        assert !Double.isNaN(assignedMinWireSpan);
        return assignedMinWireSpan;
    }

    public final double getAssignedWireLength(){
        if (Double.isNaN(assignedWireLength))
            assignedWireLength = getWireAttr(DirectiveConstants.WIRELENGTH,
                                             DirectiveUtils.MAX);
        assert !Double.isNaN(assignedWireLength);
        return assignedWireLength;
    }

    public final double getAssignedWireSpan(){
        if (Double.isNaN(assignedWireSpan))
            assignedWireSpan = getWireAttr(DirectiveConstants.WIRESPAN,
                                           DirectiveUtils.MAX);
        assert !Double.isNaN(assignedWireSpan);
        return assignedWireSpan;
    }

    public final double getInternalWireLength() {
        if (Double.isNaN(internalWireLength)) {
            double internal = -1;
            double external = -1;
            if (isPortNet() && !isGND() && !isVdd()) {
                if (container.isInternalEnv()) {
                    final Float defValue = (Float)
                        DirectiveUtils.getTopLevelDirective(
                                container.cast_cell, 
                                DirectiveConstants.INTERNAL_WIRELENGTH);
                    internal =
                        getWireAttr(DirectiveConstants.INTERNAL_WIRELENGTH,
                                    defValue, DirectiveUtils.MAX);
                }
                if (container.design.getRealTopCell() == container) {
                    final Float defValue = (Float)
                        DirectiveUtils.getTopLevelDirective(
                                container.cast_cell, 
                                DirectiveConstants.EXTERNAL_WIRELENGTH);
                    external =
                        getWireAttr(DirectiveConstants.EXTERNAL_WIRELENGTH,
                                    defValue, DirectiveUtils.MAX);
                }
            }
            internalWireLength = Math.max(internal, external);
        }
        assert !Double.isNaN(internalWireLength);
        return internalWireLength;
    }

    public final double getAssignedWireWidth(){
        if (Double.isNaN(assignedWireWidth)) 
            assignedWireWidth = getWireAttr(DirectiveConstants.WIREWIDTH,
                                            DirectiveUtils.MIN);
        assert !Double.isNaN(assignedWireWidth);
        return assignedWireWidth;
    }

    public final double getAssignedWireSpace(){
        if (Double.isNaN(assignedWireSpace)) 
            assignedWireSpace = getWireAttr(DirectiveConstants.WIRESPACE,
                                            DirectiveUtils.MIN);
        assert !Double.isNaN(assignedWireSpace);
        return assignedWireSpace;
    }

    public final double getLoadCapacitance(){
        if (Double.isNaN(assignedLoadCap)) {
            assignedLoadCap = getWireAttr(DirectiveConstants.LOADCAP,
                                          DirectiveUtils.MAX);
            if (assignedLoadCap < 0 && isPortNet() && !isGND() && !isVdd()) {
                String dir = null;
                if (container.isInternalEnv() &&
                    (portDirection == INPUT || portDirection == INPUTOUTPUT)) {
                    dir = DirectiveConstants.INTERNAL_LOADCAP;
                }
                if (container.design.getRealTopCell() == container &&
                    (portDirection == OUTPUT || portDirection == INPUTOUTPUT)) {
                    dir = DirectiveConstants.EXTERNAL_LOADCAP;
                }
                if (dir != null) {
                    final Float defValue = (Float)
                        DirectiveUtils.getTopLevelDirective(
                                container.cast_cell, dir);
                    assignedLoadCap =
                        getWireAttr(dir, defValue, DirectiveUtils.MAX);
                }
            }
        }
        assert !Double.isNaN(assignedLoadCap);
        return assignedLoadCap;
    }

    public final double getResistanceScale() {
        if (Double.isNaN(resistanceScale)) {
            final Double val =
                getLocalNodeDirective(DirectiveConstants.RESISTANCE_SCALE);
            resistanceScale = val == null ? 1.0 : val.doubleValue();
        }
        assert !Double.isNaN(resistanceScale);
        return resistanceScale;
    }

    public final void setUpSize(final double upSize) {
        Debug.assertTrue(upSize >= 0, "Invalid upSize " + upSize);
        this.upSize = upSize;
    }

    public double getUpSize() {
        return upSize;
    }

    public final void setDownSize(final double downSize) {
        Debug.assertTrue(downSize >= 0, "Invalid downSize " + downSize);
        this.downSize = downSize;
    }

    public double getDownSize() {
        return downSize;
    }

    public double getUpMinDelay() {
        Map mindelay = DirectiveUtils.getPrsDirective(container.cast_cell, DirectiveConstants.MINDELAY, DirectiveConstants.HALFOP_TYPE);
        Map ups = DirectiveUtils.getUps(mindelay);
        Map canon = DirectiveUtils.canonizeKey(container.namespace, ups);
        final Float val = (Float) canon.get(canonicalName);
        return val == null ? -1 : val.doubleValue();
    }

    public double getDownMinDelay() {
        Map mindelay = DirectiveUtils.getPrsDirective(container.cast_cell, DirectiveConstants.MINDELAY, DirectiveConstants.HALFOP_TYPE);
        Map downs = DirectiveUtils.getDowns(mindelay);
        Map canon = DirectiveUtils.canonizeKey(container.namespace, downs);
        final Float val = (Float) canon.get(canonicalName);
        return val == null ? -1 : val.doubleValue();
    }

    /**
     * Returns the value of the <code>strengthbias</code> directive for
     * the up half-operator for this node, or <code>1.0</code> if no
     * directive was specified.
     *
     * @return The strenthbias for the up half-operator for this node.
     **/
    public double getUpStrengthBias() {
        Map strengthbias = DirectiveUtils.getPrsDirective(container.cast_cell, DirectiveConstants.STRENGTHBIAS, DirectiveConstants.HALFOP_TYPE);
        Map ups = DirectiveUtils.getUps(strengthbias);
        Map canon = DirectiveUtils.canonizeKey(container.namespace, ups);
        final Float val = (Float) canon.get(canonicalName);
        return val == null ? 1.0 : val.doubleValue();
    }

    /**
     * Returns the value of the <code>strengthbias</code> directive for
     * the down half-operator for this node, or <code>1.0</code> if no
     * directive was specified.
     *
     * @return The strenthbias for the down half-operator for this node.
     **/
    public double getDownStrengthBias() {
        Map strengthbias = DirectiveUtils.getPrsDirective(container.cast_cell, DirectiveConstants.STRENGTHBIAS, DirectiveConstants.HALFOP_TYPE);
        Map downs = DirectiveUtils.getDowns(strengthbias);
        Map canon = DirectiveUtils.canonizeKey(container.namespace, downs);
        final Float val = (Float) canon.get(canonicalName);
        return val == null ? 1.0 : val.doubleValue();
    }

    private double upExtraDelay = Double.NaN;
    private double dnExtraDelay = Double.NaN;

    private void initializeExtraDelay() {
        if (CellUtils.isLeaf(container.cast_cell)) {
            // Do not retrieve extra delay for leaf cell, as that is already
            // taken into account by CellDelay
            upExtraDelay = 0;
            dnExtraDelay = 0;
        } else {
            final Map xdelay =
                DirectiveUtils.getSubcellDirective(container.cast_cell,
                        DirectiveConstants.EXTRA_DELAY,
                        DirectiveConstants.HALFOP_TYPE);
            Map canon;
            Float val;

            canon = DirectiveUtils.canonizeKey(container.namespace,
                                               DirectiveUtils.getUps(xdelay));
            val = (Float) canon.get(canonicalName);
            upExtraDelay = val == null ? 0 : val.doubleValue();

            canon = DirectiveUtils.canonizeKey(container.namespace,
                                               DirectiveUtils.getDowns(xdelay));
            val = (Float) canon.get(canonicalName);
            dnExtraDelay = val == null ? 0 : val.doubleValue();
        }
    }

    public double getUpExtraDelay() {
        if (Double.isNaN(upExtraDelay)) initializeExtraDelay();
        assert !Double.isNaN(upExtraDelay);
        return upExtraDelay;
    }

    public double getDownExtraDelay() {
        if (Double.isNaN(dnExtraDelay)) initializeExtraDelay();
        assert !Double.isNaN(dnExtraDelay);
        return dnExtraDelay;
    }

    public double getUpDelay()
    {
        List/*<GlobalNet>*/ lsta = getGlobalNets();
        if(lsta.size() > 1){ // This function is only valid for internal nets or primary outputs
            return -1.0;
        }

        return ((GlobalNet) lsta.get(0)).getUpDelay();
    }


    public double getDownDelay()
    {
        List/*<GlobalNet>*/ lsta = getGlobalNets();
        if(lsta.size() > 1){ // This function is only valid for internal nets or primary outputs
            return -1.0;
        }

        return ((GlobalNet) lsta.get(0)).getDownDelay();
    }

    public HierName getInternalCanonical() {
        if (internalNames.size() > 0) {
            return findCannon(internalNames);
        } else {
            // A cell was declared without a port list
            final Set/*<HierName>*/ names = new HashSet/*<HierName>*/();
            for (Iterator i = subcellconnectionNames.iterator(); i.hasNext(); )
            {
                final HierName port = (HierName) i.next();
                final ConnectionInfo ci =
                    (ConnectionInfo) container.subcellports.get(port);
                final HierName child = ci.getChildName(port);
                if (ci.child.getNet(child).portNames.contains(child)) {
                    names.add(port);
                }
            }
            return findCannon(names);
        }
    }


    /**
     * Get the strength ratios.  Used to generate a warning message.
     **/
    public Pair/*<Double,Double>*/ getStrengthRatios()
    {
        List/*<NetSource>*/ lsta = new ArrayList/*<NetSource>*/();

        for (Iterator ita = getListSources().iterator(); ita.hasNext(); ) {
            NetSource nsra = (NetSource)ita.next();
            if(nsra.getType() == NetType.HALF_OPERATOR_TRANSISTOR){
                lsta.add(nsra);
            }
        }

        if(lsta.size() < 2){
            Double negOne = new Double(-1.0);
            return new Pair/*<Double,Double>*/(negOne, negOne);
        }

        double maxStrengthP = Double.NEGATIVE_INFINITY;
        double minStrengthP = Double.POSITIVE_INFINITY;
        double maxStrengthN = Double.NEGATIVE_INFINITY;
        double minStrengthN = Double.POSITIVE_INFINITY;

        for (Iterator ita = lsta.iterator(); ita.hasNext(); ) {
            NetSource nsra = (NetSource)ita.next();

            HalfOperator hoa = nsra.source;
            // getStrength() actually returns the "effective resistance", 
            // so we should inverse it to get the "real" strength
            double strength = 1.0 / hoa.getStrength();

            if (hoa.driveDirection == HalfOperator.DriveDirection.PULL_DOWN) {
                if (strength > maxStrengthN)
                    maxStrengthN = strength;

                if (strength < minStrengthN)
                    minStrengthN = strength;
            }
            else{
                if (strength > maxStrengthP)
                    maxStrengthP = strength;

                if (strength < minStrengthP)
                    minStrengthP = strength;
            }
        }

        Double ratioPN;
        if (maxStrengthP == Double.NEGATIVE_INFINITY ||
            minStrengthN == Double.POSITIVE_INFINITY) {
            ratioPN = new Double(-1.0);
        }
        else{
            ratioPN = new Double(maxStrengthP / minStrengthN);
        }

        Double ratioNP;
        if (maxStrengthN == Double.NEGATIVE_INFINITY ||
            minStrengthP == Double.POSITIVE_INFINITY) {
            ratioNP = new Double(-1.0);
        }
        else{
            ratioNP = new Double(maxStrengthN / minStrengthP);
        }

        return new Pair/*<Double,Double>*/(ratioPN, ratioNP);
    }
}
