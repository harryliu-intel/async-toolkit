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

package com.avlsi.tools.jauto;

import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.ArrayList;
import java.util.List;
import java.util.HashSet;
import java.util.*;
import java.io.*;

import com.avlsi.cast.CastFileParser;
import com.avlsi.cast.CastSemanticException;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;
import com.avlsi.fast.*;
import com.avlsi.file.common.DeviceTypes;
import com.avlsi.file.common.HierName;
import com.avlsi.geometry.SteinnerTree;
import com.avlsi.geometry.Point;
import com.avlsi.geometry.BoundingBox;
import com.avlsi.netlist.AbstractDevice;
import com.avlsi.netlist.AbstractDeviceIterator;
import com.avlsi.netlist.AbstractNode;
import com.avlsi.netlist.AbstractNodeIterator;
import com.avlsi.netlist.Visitor;
import com.avlsi.netlist.impl.SimpleAbstractDeviceIterator;
import com.avlsi.netlist.impl.SimpleAbstractNodeIterator;
import com.avlsi.netlist.impl.simple.SimpleNetlist;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.dsim.InstanceData;
import com.avlsi.tools.lvs.NetGraph;
import com.avlsi.tools.jauto.DebugOption;
import com.avlsi.tools.jauto.TechnologyData;
import com.avlsi.tools.jauto.NetSource;
import com.avlsi.tools.jauto.NetSink;
import com.avlsi.tools.jauto.JautoMessageCenter;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.Interpolate;
import com.avlsi.util.container.MultiSet;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.CollectionUtils;
import com.avlsi.util.functions.UnaryFunction;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.flute.Flute;
import com.avlsi.util.tree.*;

/**
 * Class representing a net that crosses cell boundaries.
 *
 * Attached at all ports, and at the net at the top level.
 *
 * Contains pointers to all terminal ports.
 *
 * @author Aaron Denney
 * @version $Date$

 **/

public class GlobalNet {
    /** Subcircuits for wire cap and res **/
    private final SimpleNetlist wireCapModel;
    private final SimpleNetlist wireResModel;

    // PointGeometry needs access to GND, but non-static inner classes
    // cannot have non-constant static members.  We put GND here
    // instead of directly in GlobalNet to avoid polluting the GlobalNet
    // namespace.
    private interface HierNameConstants {
        HierName GND = HierName.makeHierName("GND");
    }

    private class PointGeometry implements CellNet.Geometry,
                                           HierNameConstants {
        private final HierName source, sink;
        private AbstractDevice makeCapacitor(final HierName name,
                                             final AbstractNode node1,
                                             final AbstractNode node2,
                                             final double C,
                                             final double W,
                                             final double S,
                                             final double L) {
            return new AbstractDevice() {
                public void accept(final Visitor visitor) {
                    List nodes = new ArrayList();
                    Map parameters = new HashMap();
                    nodes.add(node1);
                    nodes.add(node2);
                    parameters.put("c",C);
                    parameters.put("w",W);
                    parameters.put("s",S);
                    parameters.put("l",L);
                    visitor.subcircuitCall(name,wireCapModel,
                                           new SimpleAbstractNodeIterator(nodes.iterator()),
                                           parameters);
                }
            };
        }
        private AbstractDevice makeResistor(final HierName name,
                                            final AbstractNode node1,
                                            final AbstractNode node2,
                                            final double R,
                                            final double W,
                                            final double S,
                                            final double L) {
            return new AbstractDevice() {
                public void accept(final Visitor visitor) {
                    List nodes = new ArrayList();
                    Map parameters = new HashMap();
                    nodes.add(node1);
                    nodes.add(node2);
                    parameters.put("r",R);
                    parameters.put("w",W);
                    parameters.put("s",S);
                    parameters.put("l",L);
                    visitor.subcircuitCall(name,wireResModel,
                                           new SimpleAbstractNodeIterator(nodes.iterator()),
                                           parameters);
                }
            };
        }
        private AbstractNode makeNode(final HierName name) {
            return new AbstractNode() {
                public HierName getCanonicalName() {
                    return name;
                }
                public Iterator getAliases() {
                    // XXX: Not implemented
                    return null;
                }
                public AbstractDeviceIterator getDevices() {
                    // XXX: Not implemented
                    return null;
                }
            };
        }
        public PointGeometry() {
            final String suffix = topCellNet.canonicalName.getSuffixString();
            this.source = HierName.makeSiblingName(topCellNet.canonicalName,
                                                   suffix + ":source");
            this.sink = HierName.makeSiblingName(topCellNet.canonicalName,
                                                 suffix); // + ".sink");
        }
        public HierName mapName(final CellNet port, final HierName alias) {
            if (port.portDirection == CellNet.INPUT) return sink;
            else return source;
        }
        public HierName mapName(final CellNet net, final boolean gate) {
            if (net == topCellNet) {
                return gate ? sink : source;
            } else {
                return net.canonicalName;
            }
        }
        public Pair[] getRC(final HierName alias1, final HierName alias2) {
            final Pair[] ret = new Pair[1];
            /*
            if (port.portDirection == isSource(alias2)) {
                ret[0] = new Pair(new Double(0), new Double(0));
            } else {
            }
            ret[0] = new Pair(new Double(R), new Double(C));
            */
            Debug.assertTrue(false, "Don't call getRC!");
            return ret;
        }
        public AbstractDeviceIterator getDeviceIterator() {
            final List/*<AbstractDevice>*/ devices =
                new ArrayList/*<AbstractDevice>*/();
            // create Pi model for wire R/C
            final AbstractNode sourceNode = makeNode(source);
            final AbstractNode sinkNode = makeNode(sink);
            final double C = getWireCapacitance();
            final double R = getWireResistance();
            final double W = getWireWidth();
            final double S = getWireSpace();
            final double CL = getEstimatedWireLength();
            final double RL = getEstimatedWireSpan();
            devices.add(makeCapacitor(source.appendString(":c"),
                                      makeNode(GND),sourceNode,C/2,W,S,CL/2));
            devices.add(makeCapacitor(sink.appendString(":c"),
                                      makeNode(GND),sinkNode,C/2,W,S,CL/2));
            devices.add(makeResistor(sink.appendString(":r"),
                                     sourceNode,sinkNode,R,W,S,RL));
            return new SimpleAbstractDeviceIterator(devices.iterator());
        }
    }

    /**
     * A comparator of NetSource objects.  It first compares type.  Then for
     * NetType.CELL, compares cell name; for HALF_OPERATOR_TRANSISTOR, compares
     * cell name, output node name, and direction in sequence.
     * XXX: Make sure any changes to this are synchronize with
     * {@link NetSourceCellTypeComparator}.
     **/
    private static class NetSourceComparator implements Comparator {
        private static NetSourceComparator singleton = null;
        private NetSourceComparator() { }
        public static NetSourceComparator getInstance() {
            if (singleton == null) singleton = new NetSourceComparator();
            return singleton;
        }
        public int compare(final Object o1, final Object o2) {
            final NetSource src1 = (NetSource) o1;
            final NetSource src2 = (NetSource) o2;
            
            final int type = src1.getType() - src2.getType();
            if (type != 0) return type;

            if (src1.getType() == NetType.CELL) {
                return src1.getCellSource().typeName.compareTo(
                            src2.getCellSource().typeName);
            } else if (src1.getType() == NetType.HALF_OPERATOR_TRANSISTOR) {
                final HalfOperator ho1 = src1.getSource();
                final HalfOperator ho2 = src2.getSource();

                final int cell =
                    ho1.subType.typeName.compareTo(ho2.subType.typeName);
                if (cell != 0) return cell;

                final int output = ho1.outputNode.compareTo(ho2.outputNode);
                if (output != 0) return output;

                return ho1.driveDirection - ho2.driveDirection;
            }

            return 0;
        }
        public boolean equals(final Object o) {
            return o == singleton;
        }
    }
    /**
     * If a MultiSet has been sorted using {@link NetSourceComparator}, then
     * this class can be used to search for a source with the given type and
     * associated cell type.
     * XXX: Make sure changes to this does not conflict with the ordering
     * imposed by NetSourceComparator.
     **/
    private static class NetSourceCellTypeComparator implements Comparator {
        private final Object dummy;
        private final int sourceType;
        private final String cellType;
        public NetSourceCellTypeComparator(final Object dummy,
                                           final int sourceType, 
                                           final String cellType) {
            this.dummy = dummy;
            this.sourceType = sourceType;
            this.cellType = cellType;
        }
        private int compare(final NetSource src) {
            final int type = sourceType - src.getType();
            if (type != 0) return type;

            if (sourceType == NetType.CELL) {
                return cellType.compareTo(src.getCellSource().typeName);
            } else if (sourceType == NetType.HALF_OPERATOR_TRANSISTOR) {
                return cellType.compareTo(src.getSource().subType.typeName);
            }
            return 0;
        }
        public int compare(final Object o1, final Object o2) {
            if (o1 == dummy) return compare((NetSource) o2);
            else return -compare((NetSource) o1);
        }
        public boolean equals(final Object o) {
            if (o instanceof NetSourceCellTypeComparator) {
                final NetSourceCellTypeComparator c =
                    (NetSourceCellTypeComparator) o;
                return dummy == c.dummy && sourceType == c.sourceType &&
                       cellType.equals(c.cellType);
            }
            return false;
        }
    }

    /**
     * A comparator of NetSink objects.  It first compares types.  Then for
     * NetType.CELL, compares cell name; for CAPACITIVE_LOAD, compares load;
     * for HALF_OPERATOR_TRANSISTOR, compare cell name, output node name,
     * direction, and transistor size in sequence.
     **/
    private static class NetSinkComparator implements Comparator {
        private static NetSinkComparator singleton = null;
        private NetSinkComparator() { }
        public static NetSinkComparator getInstance() {
            if (singleton == null) singleton = new NetSinkComparator();
            return singleton;
        }
        public int compare(final Object o1, final Object o2) {
            final NetSink snk1 = (NetSink) o1;
            final NetSink snk2 = (NetSink) o2;
            
            final int type = snk1.getType() - snk2.getType();
            if (type != 0) return type;

            if (snk1.getType() == NetType.CELL) {
                return snk1.getCellSink().typeName.compareTo(
                            snk2.getCellSink().typeName);
            } else if (snk1.getType() == NetType.CAPACITIVE_LOAD) {
                return Double.compare(snk1.loadCapacitance,
                                      snk2.loadCapacitance);
            } else if (snk1.getType() == NetType.HALF_OPERATOR_TRANSISTOR) {
                final HalfOperator ho1 = snk1.getSink();
                final HalfOperator ho2 = snk2.getSink();

                final int cell =
                    ho1.subType.typeName.compareTo(ho2.subType.typeName);
                if (cell != 0) return cell;

                final int output = ho1.outputNode.compareTo(ho2.outputNode);
                if (output != 0) return output;

                final int dir = ho1.driveDirection - ho2.driveDirection;
                if (dir != 0) return dir;

                return Double.compare(snk1.getTransistor().size,
                                      snk2.getTransistor().size);
            }
            return 0;
        }
        public boolean equals(final Object o) {
            return o == singleton;
        }
    }

    // highest-level cell net corresponding to this global net
    private final CellNet           topCellNet;

    // list of sources (drivers) of the net
    private MultiSet/*<NetSource>*/ listSources;

    // list of sinks (loads) of the net
    private MultiSet/*<NetSink>*/   listSinks;

    private Forest<BoundingBox>  steinerTree = new Forest<BoundingBox>();
    private ArrayList/*<Node<BoundingBox>>*/  sourceList = 
                                        new ArrayList/*<Node<BoundingBox>>*/();
    private ArrayList/*<Node<BoundingBox>>*/  sinkList = 
                                        new ArrayList/*<Node<BoundingBox>>*/();

    // estimated total wire length for this global net
    private double                  estimatedWireLength; // in Meter

    // estimated maximum wire span for this global net
    private double                  estimatedWireSpan; // in Meter

    // minimum wire width and wire space of all CellNets that are part
    // of this GlobalNet
    private double minWireWidth = -1, minWireSpace = -1;

    // maximum wire length of all CellNets that are part of this
    // GlobalNet
    private double maxWireLength = -1, maxWireSpan = -1, maxMinWireLength = -1,
                   maxMinWireSpan = -1, maxInternalWireLength = -1;

    private double getAssignedMinDirective(final CellNet net, double val) {
        if (CellUtils.isFixedSize(net.container.cast_cell) &&
            topCellNet.container.isFloorplanned()) {
            val = -1;
        }
        return val;
    }

    private void updateWireParam(final CellNet net) {
        final double w = net.getAssignedWireWidth();
        if (w >= 0)
            minWireWidth = minWireWidth < 0 ? w : Math.min(minWireWidth, w);

        final double s = net.getAssignedWireSpace();
        if (s >= 0)
            minWireSpace = minWireSpace < 0 ? s : Math.min(minWireSpace, s);

        final double l = net.getAssignedWireLength();
        if (l >= 0)
            maxWireLength = maxWireLength < 0 ? l : Math.max(maxWireLength, l);

        final double sp = net.getAssignedWireSpan();
        if (sp >= 0)
            maxWireSpan = maxWireSpan < 0 ? sp : Math.max(maxWireSpan, sp);

        final double il = net.getInternalWireLength();
        if (il >= 0)
            maxInternalWireLength = Math.max(maxInternalWireLength, il);

        final double ml =
            getAssignedMinDirective(net, net.getAssignedMinWireLength());
        if (ml >= 0) maxMinWireLength = Math.max(maxMinWireLength, ml);

        final double ms =
            getAssignedMinDirective(net, net.getAssignedMinWireSpan());
        if (ms >= 0) maxMinWireSpan = Math.max(maxMinWireSpan, ms);
    }

    public CellNet getTopCellNet(){
        return topCellNet;
    }

    public MultiSet/*<NetSource>*/ getListSources() {
        return listSources;
    }

    public MultiSet/*<NetSink>*/ getListSinks() {
        return listSinks;
    }


    public double getWireWidth() {
        double wireWidth = topCellNet.getAssignedWireWidth();

        if (wireWidth <= 0) wireWidth = minWireWidth;

        if (wireWidth <= 0)
            wireWidth = topCellNet.container.getAssignedWireWidth();

        if (wireWidth <= 0)
            wireWidth = getTechnologyData().defaultWireWidth;

        return wireWidth;
    }


    public double getWireSpace() {
        double wireSpace = topCellNet.getAssignedWireSpace();

        if (wireSpace <= 0) wireSpace = minWireSpace;

        if (wireSpace <= 0)
            wireSpace = topCellNet.container.getAssignedWireSpace();

        if (wireSpace <= 0)
            wireSpace = getTechnologyData().defaultWireSpace;

        return wireSpace;
    }

    public double getMinWireLength() {
        double minWireLength =
            getAssignedMinDirective(topCellNet,
                                    topCellNet.getAssignedMinWireLength());

        if (minWireLength <= 0) minWireLength = maxMinWireLength;

        if (minWireLength <= 0)
            minWireLength = getAssignedMinDirective(
                topCellNet, topCellNet.container.getAssignedMinWireLength());

        return minWireLength;
    }

    public double getMinWireSpan() {
        double minWireSpan =
            getAssignedMinDirective(topCellNet,
                                    topCellNet.getAssignedMinWireSpan());

        if (minWireSpan <= 0) minWireSpan = maxMinWireSpan;

        if (minWireSpan <= 0)
            minWireSpan = getAssignedMinDirective(
                topCellNet, topCellNet.container.getAssignedMinWireSpan());

        return minWireSpan;
    }

    public double getResistanceScale() {
        double scale = topCellNet.getResistanceScale();
        return scale;
    }

    public double getWireResistance() {
        final TechnologyData          td = getTechnologyData();

        final double wireWidth = getWireWidth();
        final double unitWireResistance;

        if (td.getWireResistanceTable() == null) {
            unitWireResistance = td.unitWireResistance / wireWidth;
        } else {
            final double wireSpace = getWireSpace();
            unitWireResistance =
                Interpolate.bilinearInterpolate(
                        td.getWireResistanceTable(),
                        wireWidth,
                        wireSpace);
        }

        return getEstimatedWireSpan() *
               unitWireResistance *
               getResistanceScale();
    }

    public double getWireCapacitance() {
        TechnologyData          td = getTechnologyData();

        double wireWidth = getWireWidth();
        double wireSpace = getWireSpace();
        final double unitWireCapacitance;
        if (td.getWireCapacitanceTable() == null) {
            unitWireCapacitance =
                wireWidth * td.unitWireCapacitanceCa +
                2.0 * (td.unitWireCapacitanceCfc_2 / wireSpace / wireSpace +
                       td.unitWireCapacitanceCfc_1 / wireSpace +
                       td.unitWireCapacitanceCfc_0);
        } else {
            unitWireCapacitance =
                Interpolate.bilinearInterpolate(
                        td.getWireCapacitanceTable(),
                        wireWidth,
                        wireSpace);
        }

        return td.capacitanceScalingFactor *
               getEstimatedWireLength() *
               unitWireCapacitance;
    }

    public double getWireRC() {
        return getWireCapacitance() * getWireResistance();
    }

    public double getGateCapacitance() {
        TechnologyData          td = getTechnologyData();

        double nGateCap = 0.0;
        double pGateCap = 0.0;

        for (Iterator ita = getListSinks().iterator(); ita.hasNext(); ) {
            NetSink nska = (NetSink)ita.next();
            if(nska.type == NetType.HALF_OPERATOR_TRANSISTOR){ // transistor type load
                NetGraph.NetEdge nea = nska.transistor;
                double width = nea.width / nea.shareCount;
                if (nea.type == DeviceTypes.N_TYPE)
                    nGateCap += width * nea.length * td.getUnitNmosGateCapacitance(nea.getTransistorType());
                else
                    pGateCap += width * nea.length * td.getUnitPmosGateCapacitance(nea.getTransistorType());
            }
        }

        return nGateCap + pGateCap;
    }


    public double getConstCapacitance() {
        double constCap = 0.0;

        for (Iterator ita = getListSinks().iterator(); ita.hasNext(); ) {
            NetSink nska = (NetSink)ita.next();
            if(nska.type == NetType.CAPACITIVE_LOAD){ // constant capacitance load
                constCap += nska.loadCapacitance;
            }
        }

        return constCap;
    }


    public double getLoadCapacitance() {
        // total cap
        return getWireCapacitance() + getGateCapacitance() + getConstCapacitance();
    }

    /** Used by JautoUI to dump out an info file for SDC generation. **/
    public double getDriveResistance(final HalfOperator ho1) {
        final TechnologyData tech = getTechnologyData();
        final double[] effectiveResistanceFactors;
        if (ho1.driveDirection == HalfOperator.DriveDirection.PULL_DOWN) {
            effectiveResistanceFactors =
                tech.getEffectiveResistanceFactorN(ho1.getTransistorType());
        } else {
            effectiveResistanceFactors =
                tech.getEffectiveResistanceFactorP(ho1.getTransistorType());
        }
        final double effectiveResistanceFactor =
            effectiveResistanceFactors[0] / ho1.getStrengthBias();
        return effectiveResistanceFactor * tech.defaultGateLength /
               ho1.getCurrentSize();
    }

    /** Add up all width driving this node.  Used to size staticizers. **/
    public double getTotalDriverWidth(int type) {
        double total=0;
        for (Iterator ita = getListSources().iterator(); ita.hasNext(); ) {
            NetSource nsrc = (NetSource)ita.next();
            if (nsrc.type != NetType.HALF_OPERATOR_TRANSISTOR) continue;
            HalfOperator ho = nsrc.source;
            for (Iterator itb = ho.transistors.iterator(); itb.hasNext(); ) {
                NetGraph.NetEdge edge = (NetGraph.NetEdge) itb.next();
                if ((edge.type==type) &&
                    (edge.source==ho.outputNode || edge.drain==ho.outputNode))
                    total += edge.width;
            }
        }
        return total;
    }

    /**
     * Construct a GlobalNet from a given top level CellNet.  This constructor
     * is private; use the static method
     * {@link #generateGlobalNets(CellType, List) generateGlobalNets}
     * to generate GlobalNets.
     *
     * @param cn1 The top-level CellNet.
     **/
    private GlobalNet(CellNet cn1) {
        topCellNet = cn1;

        listSources =
            new MultiSet/*<NetSource>*/(NetSourceComparator.getInstance());
        listSinks = new MultiSet/*<NetSink>*/(NetSinkComparator.getInstance());

        estimatedWireLength = -1.0;
        estimatedWireSpan = -1.0;

        // TODO: should be able to do this statically somehow
        // TODO: avoid missing CAST definition by declaring these as "models"
        String[] n1 = {"n1"};
        String[] n2 = {"n2"};
        wireCapModel = new SimpleNetlist(HierName.makeHierName("wirecap"),n1,n2);
        wireResModel = new SimpleNetlist(HierName.makeHierName("wireres"),n1,n2);

        topCellNet.setGeometry(new PointGeometry());
    }

    private CastDesign getCastDesign() {
        return topCellNet.container.design;
    }

    TechnologyData getTechnologyData(){
        return topCellNet.container.design.getTechnologyData();
    }

    JautoMessageCenter getMessageCenter(){
        return topCellNet.container.design.getMessageCenter();
    }

    private Cadencize getCadencize() {
        return getCastDesign().getCadencize();
    }

    public double getEstimatedWireLength() {
        // Cache the result, as this is a time consuming operation
        if (estimatedWireLength < 0) {
            estimatedWireLength = topCellNet.getAssignedWireLength();
            if (estimatedWireLength < 0) {
                estimatedWireLength = maxWireLength;
            }
            if (estimatedWireLength < 0) {
                estimatedWireLength = topCellNet.container.getAssignedWireLength();
            }
            if (estimatedWireLength < 0) {
                estimatedWireLength =
                    // min_wirespan is expected to be less than min_wirelength.
                    // However, use it for the wire cap determination in case
                    // it's larger.
                    Math.max(maxInternalWireLength < 0 ?
                                estimateWireLength() :
                                maxInternalWireLength,
                             Math.max(getMinWireLength(),
                                      getMinWireSpan()));
            }
        }
        assert estimatedWireLength > 0;
        return estimatedWireLength;
    }

    public double getEstimatedWireSpan() {
        // Cache the result, as this is a time consuming operation
        if (estimatedWireSpan < 0) {
            estimatedWireSpan = topCellNet.getAssignedWireSpan();
            if (estimatedWireSpan < 0) {
                estimatedWireSpan = maxWireSpan;
            }
            if (estimatedWireSpan < 0) {
                estimatedWireSpan = topCellNet.container.getAssignedWireSpan();
            }
            if (estimatedWireSpan < 0) {
                estimatedWireSpan = topCellNet.getAssignedWireLength();
            }
            if (estimatedWireSpan < 0) {
                estimatedWireSpan = maxWireLength;
            }
            if (estimatedWireSpan < 0) {
                estimatedWireSpan = topCellNet.container.getAssignedWireLength();
            }
            if (estimatedWireSpan < 0) {
                // If min_wirespan is specified, use it as the cast-specified
                // minimum wire span.  Otherwise, use min_wirelength.
                final double ml = getMinWireLength();
                final double ms = getMinWireSpan();
                estimatedWireSpan =
                    Math.max(maxInternalWireLength < 0 ?
                                estimateWireSpan() :
                                maxInternalWireLength,
                             (ms >= 0.0 ? ms : ml));
            }
        }
        assert estimatedWireSpan > 0;
        return estimatedWireSpan;
    }

    /** get manhattan distance between points with X/Y scaling */
    private double manhattanDistance(double x0, double y0, double x1, double y1) {
        double scaleX = getTechnologyData().layoutScaleX;
        double scaleY = getTechnologyData().layoutScaleY;
        return Math.abs(x1 - x0) * scaleX + Math.abs(y1 - y0) * scaleY;
    }

    private double getFluteLength(Node<BoundingBox> node) {
        double length = 0;
        double scale = 1.0e10;
        ArrayList/*<int>*/ XList = new ArrayList/*<int>*/();
        ArrayList/*<int>*/ YList = new ArrayList/*<int>*/();
        List<Node<BoundingBox>> children = node.getChildren();
        //Hopefully a redundant check
        if(children.size() < 1) {
            return length;
        }
        BoundingBox srcbBox = node.getData();
        XList.add((int)(srcbBox.getLowerLeft().getX()*scale));
        YList.add((int)(srcbBox.getLowerLeft().getY()*scale));
        XList.add((int)(srcbBox.getUpperRight().getX()*scale));
        YList.add((int)(srcbBox.getUpperRight().getY()*scale));

        for (Node<BoundingBox> n : children) {
            BoundingBox childbBox = n.getData();
            if(n.getNumberOfChildren() > 0) {
                XList.add((int)(childbBox.getLowerLeft().getX()*scale));
                YList.add((int)(childbBox.getLowerLeft().getY()*scale));
                XList.add((int)(childbBox.getUpperRight().getX()*scale));
                YList.add((int)(childbBox.getUpperRight().getY()*scale));
                length += getFluteLength(n);
            } else {
                Point center = childbBox.getCenter();
                XList.add((int)(center.getX()*scale));
                YList.add((int)(center.getY()*scale));
                length += childbBox.getHalfPerimeter() * 2 *
                          getTechnologyData().getLeafWireLengthPortScale();
            }
        }
        int count = XList.size();
        if(count > 1) {
            int[] X = CollectionUtils.toIntArray(XList);
            int[] Y = CollectionUtils.toIntArray(YList);
            length += (double)(Flute.getWireLength(X,Y)/scale);
        }
        //if(length > 0) {
        //    System.out.println("flute "+node.getData()+": " + length);
        //}
        return length;
    }

    private double steinerWireLength() {
        double length = 0;
        double scale = 1.0e10;
        ArrayList/*<int>*/ XList = new ArrayList/*<int>*/();
        ArrayList/*<int>*/ YList = new ArrayList/*<int>*/();
        for (Node<BoundingBox> steinerNode : steinerTree.getRootNodes()) {
            BoundingBox bBox = steinerNode.getData();
            if(steinerNode.getNumberOfChildren() > 0) {
                XList.add((int)(bBox.getLowerLeft().getX()*scale));
                YList.add((int)(bBox.getLowerLeft().getY()*scale));
                XList.add((int)(bBox.getUpperRight().getX()*scale));
                YList.add((int)(bBox.getUpperRight().getY()*scale));
                length += getFluteLength(steinerNode);
            } else {
                Point center = bBox.getCenter();
                XList.add((int)(center.getX()*scale));
                YList.add((int)(center.getY()*scale));
                length += bBox.getHalfPerimeter() * 2 *
                          getTechnologyData().getLeafWireLengthPortScale();
            }
        }
        int count = XList.size();
        if(count > 1) {
            int[] X = CollectionUtils.toIntArray(XList);
            int[] Y = CollectionUtils.toIntArray(YList);
            double top_length = (double)(Flute.getWireLength(X,Y)/scale);
            //System.out.println("top_length: " + top_length);
            length += top_length;
        }        

        // handle default
        if (length <= 0.0) {
            length = getTechnologyData().defaultWireLength;
        }

        // handle minimum wire length
        return Math.max(length,getTechnologyData().minimumWireLength);
    }

    private ArrayList<Node<BoundingBox>> getListOfNodesToClosestParent(final Node sourceNode,
                                                                       final Node sinkNode) {
        ArrayList/*Node<BoundingBox>*/ retList = new ArrayList/*Node<BoundingBox>*/();
        ArrayList/*Node<BoundingBox>*/ sourceParents 
                                    = (ArrayList<Node<BoundingBox>>)sourceNode.getAllParents();
        ArrayList/*Node<BoundingBox>*/ sinkParents 
                                    = (ArrayList<Node<BoundingBox>>)sinkNode.getAllParents();
        sourceParents.add(0,sourceNode);
        sinkParents.add(0,sinkNode);
        for (int iSource = 0; iSource < sourceParents.size(); ++iSource) {
            Node<BoundingBox> n1 = (Node<BoundingBox>)sourceParents.get(iSource);
            int index = sinkParents.indexOf(n1);
            if(index != -1) {
                retList.addAll(sourceParents.subList(0,iSource+1));
                retList.addAll(sinkParents.subList(0,index+1));
                return retList;
            }
        }
        retList.addAll(sourceParents);
        retList.addAll(sinkParents);
        return retList;
    }

    private double steinerWireSpan() {
        double span = 0;
        double scale = 1.0e10;

        // if no sources or no sinks, find longest span instead
        ArrayList src = sourceList.size()>0 ? sourceList : sinkList;
        ArrayList snk = sinkList.size()>0   ? sinkList   : sourceList;

        // look for longest path from src to snk
        for (Iterator i = src.iterator(); i.hasNext(); ) {
            Node<BoundingBox> sourceNode = (Node<BoundingBox>) i.next();
            for (Iterator j = snk.iterator(); j.hasNext(); ) {
                Node<BoundingBox> sinkNode = (Node<BoundingBox>) j.next();
                ArrayList/*Node<BoundingBox>*/ spanNodes = 
                    getListOfNodesToClosestParent(sourceNode,
                                                  sinkNode);
                ArrayList/*<int>*/ XList = new ArrayList/*<int>*/();
                ArrayList/*<int>*/ YList = new ArrayList/*<int>*/();
                double local_span = 0;
                for (Iterator k = spanNodes.iterator(); k.hasNext(); ) {
                    Node<BoundingBox> spanNode = (Node<BoundingBox>) k.next();
                    BoundingBox bBox = spanNode.getData();
                    if(spanNode.getNumberOfChildren() > 0) {
                        XList.add((int)(bBox.getLowerLeft().getX()*scale));
                        YList.add((int)(bBox.getLowerLeft().getY()*scale));
                        XList.add((int)(bBox.getUpperRight().getX()*scale));
                        YList.add((int)(bBox.getUpperRight().getY()*scale));
                    } else {
                        Point center = bBox.getCenter();
                        XList.add((int)(center.getX()*scale));
                        YList.add((int)(center.getY()*scale));
                        local_span += bBox.getHalfPerimeter() * 2 *
                            getTechnologyData().getLeafWireLengthPortScale();
                    }
                }
                int[] X = CollectionUtils.toIntArray(XList);
                int[] Y = CollectionUtils.toIntArray(YList);
                local_span += (double)(Flute.getWireLength(X,Y)/scale);
                //System.out.println("local_span: " + local_span);
                span = Math.max(span, local_span);
            }
        }
     
        // handle default
        if (span <= 0.0) {
            span = getTechnologyData().defaultWireLength;
        }

        // handle minimum wire length
        return Math.max(span,getTechnologyData().minimumWireLength);
    }

    /** Estimate wire length inside a leaf cell or other special cases, or -1 **/
    private double specialWireLength() {
        // Use default if cell has floorplan=false
        final TechnologyData tech = getTechnologyData();
        CellType cta = topCellNet.container;
        if(!cta.isFloorplanned()){
            if(DebugOption.printLevel <= 1){
                System.out.println("Cell found not floorplanned: " + 
                                   topCellNet.container.typeName);
            }
            return tech.defaultWireLength;
        }

        // Estimate wire length inside a leaf cell as width plus height
        if(cta.getLevel() == 0){
            if((cta.xSize + cta.ySize) == 0.0){
                return tech.defaultWireLength;
            }
            else{
                final double perimeter =
                    2 * (cta.xSize * tech.layoutScaleX +
                         cta.ySize * tech.layoutScaleY);
                return tech.getLeafWireLengthScale() * perimeter;
            }
        }

        // Use default for "isolated" net in a mid-level cell
        if(listSources.size()==0 && listSinks.size()==0){
            if(DebugOption.printLevel <= 1) {
                System.out.println("WARNING: Global net with no sources or sinks");
                System.out.println("Cell name: " + topCellNet.container.typeName);
                System.out.println("Net name: " + topCellNet.canonicalName.getCadenceString());
            }
            return tech.defaultWireLength;
        }
        
        // not a special case
        return -1;
    }

    /**
     * Estimate total wire length by the "bounding box" algorithm.
     * Finds the minimum rectangle box that covers all the sinks and
     * sources <code>wire_length = width + height</code> of the
     * bounding box (used for capacitance).  If SrcToSnk is true,
     * instead finds the maximum span from the bbox of sources to the
     * bbox of sinks (used for resistance).  This isn't quite
     * equivalent to the maximum from all sources to all sinks, but is
     * much faster and probably good enough.
     **/
    private double bboxWireLength(boolean SrcToSnk) {
        // Calculate bounding box of all sources
        double src_left   = Double.POSITIVE_INFINITY;
        double src_right  = Double.NEGATIVE_INFINITY;
        double src_top    = Double.NEGATIVE_INFINITY;
        double src_bottom = Double.POSITIVE_INFINITY;
        final int numSources = listSources.size();
        for (int iSource = 0; iSource < numSources; ++iSource) {
            NetSource nsra = (NetSource)listSources.get(iSource);
            if(nsra.type != NetType.HALF_OPERATOR_TRANSISTOR) continue;

            // get bounding box of this source
            double sx, sy;
            CellType ctb = nsra.source.subType;
            switch(nsra.orientation){
                case ConnectionInfo.R0:
                case ConnectionInfo.R180:
                case ConnectionInfo.MY:
                case ConnectionInfo.MX:
                    sx = ctb.xSize;
                    sy = ctb.ySize;
                    break;
                
                case ConnectionInfo.R90:
                case ConnectionInfo.R270:
                case ConnectionInfo.MYR90:
                case ConnectionInfo.MXR90:
                    sx = ctb.ySize;
                    sy = ctb.xSize;
                    break;
                
                default: throw new AssertionError();
            }
            double fl = nsra.coordinateX - sx / 2.0;
            double fr = nsra.coordinateX + sx / 2.0;
            double fb = nsra.coordinateY - sy / 2.0;
            double ft = nsra.coordinateY + sy / 2.0;

            // add to bounding box of all sources
            if (fl < src_left)   src_left   = fl;
            if (fr > src_right)  src_right  = fr;
            if (fb < src_bottom) src_bottom = fb;
            if (ft > src_top)    src_top    = ft;
        }

        // Calculate bounding box of all sinks
        double snk_left   = Double.POSITIVE_INFINITY;
        double snk_right  = Double.NEGATIVE_INFINITY;
        double snk_top    = Double.NEGATIVE_INFINITY;
        double snk_bottom = Double.POSITIVE_INFINITY;
        final int numSinks = listSinks.size();
        for (int iSink = 0; iSink < numSinks; ++iSink) {
            NetSink nska = (NetSink) listSinks.get(iSink);
            if(nska.type != NetType.HALF_OPERATOR_TRANSISTOR) continue;
            
            // get bounding box of this sink
            double sx, sy;
            CellType ctb = nska.sink.subType;
            switch(nska.orientation){
                case ConnectionInfo.R0:
                case ConnectionInfo.R180:
                case ConnectionInfo.MY:
                case ConnectionInfo.MX:
                    sx = ctb.xSize;
                    sy = ctb.ySize;
                    break;
                
                case ConnectionInfo.R90:
                case ConnectionInfo.R270:
                case ConnectionInfo.MYR90:
                case ConnectionInfo.MXR90:
                    sx = ctb.ySize;
                    sy = ctb.xSize;
                    break;
                
                default: throw new AssertionError();
            }
            double fl = nska.coordinateX - sx / 2.0;
            double fr = nska.coordinateX + sx / 2.0;
            double fb = nska.coordinateY - sy / 2.0;
            double ft = nska.coordinateY + sy / 2.0;
            
            // add to bounding box of all sources
            if (fl < snk_left)   snk_left   = fl;
            if (fr > snk_right)  snk_right  = fr;
            if (fb < snk_bottom) snk_bottom = fb;
            if (ft > snk_top)    snk_top    = ft;
        }

        // Calculate bounding box of all sources and sinks
        double left   = Math.min(snk_left,src_left);
        double right  = Math.max(snk_right,src_right);
        double bottom = Math.min(snk_bottom,src_bottom);
        double top    = Math.max(snk_top,src_top);
        
        // choose length estimate
        double length = -1;
        if (SrcToSnk && 
            src_left!=Double.POSITIVE_INFINITY && 
            snk_left!=Double.POSITIVE_INFINITY) {
            
            // maximum distance between all corners of both bboxs
            for (int i=0; i<16; i++) {
                double x0 = (i&1)!=0 ? src_left : src_right;
                double y0 = (i&2)!=0 ? src_bottom : src_top;
                double x1 = (i&4)!=0 ? snk_left : snk_right;
                double y1 = (i&8)!=0 ? snk_bottom : snk_top;
                double x = manhattanDistance(x0,y0,x1,y1);
                if (x>length) length = x;
            }
        }
        else if (left != Double.POSITIVE_INFINITY) {
            // maximum distance between any corners of the bbox
            length = manhattanDistance(left,bottom,right,top);
        }

        // handle default wire length
        if (length <= 0.0) {
            length = getTechnologyData().defaultWireLength;
        }
        
        // handle minimum wire length
        if (length < getTechnologyData().minimumWireLength) {
            length = getTechnologyData().minimumWireLength;
        }
        
        return length;
    }

    private double estimateWireLength() {
        double l = specialWireLength();
        if (l>=0) return l;
        if (getCastDesign().useSteinerTree()) return steinerWireLength();
        else return bboxWireLength(false);
    }

    private double estimateWireSpan() {
        double l = specialWireLength();
        if (l>=0) return l;
        if (getCastDesign().useSteinerTree()) return steinerWireSpan();
        else return bboxWireLength(true);
    }

    /**
     * Determines if this global net is driven by the specified
     * half-operator, ie if the half-operator is a source of one
     * of the source nets.  Used only for an assert.
     **/
    private boolean isDrivenByHalfOperator
        (final /*@ non_null @*/ HalfOperator ho1) {
        // find the source in the list
        final int numSources = listSources.size();
        for (int iSource = 0; iSource < numSources; ++iSource) {
            NetSource nsra = (NetSource) listSources.get(iSource);
            if (nsra.type == NetType.HALF_OPERATOR_TRANSISTOR) {
                if (nsra.source == ho1) {
                    return true;
                }
            }
        }

        return false;
    }

    private static double getPrechargeLoad(final HalfOperator op,
                                           final TechnologyData tech) {
        return op.getPrechargeNodes().size() *
               tech.getPrechargeTransistorLoad();
    }

    private static void debugTerm(final String msg, final FunctionTerm ftma) {
        if (DebugOption.printLevel <= 1) 
            System.out.println(msg + ftma.toString());
    }

    private static void debugTerm(final String msg, final AdditiveTerms ftma) {
        if (DebugOption.printLevel <= 1 && !ftma.isEmpty()) 
            System.out.println(msg + ftma.toString());
    }

    /**
     * Returns the gate cap on <code>node</code> due to feedback transistor
     * <code>e</code>.
     **/
    private static double getStaticizerGateCap(final NetGraph.NetEdge e,
                                               final NetGraph.NetNode node,
                                               final TechnologyData tech,
                                               final HierName inst) {
        double result = 0;

        if (e.gate == node && (!e.isUsedLogic() || e.isSmallInverter())) {
            result = e.width * e.length / e.shareCount *
                     tech.getUnitGateCapacitance(e.type, e.getTransistorType());
            if (DebugOption.printLevel <= 1) {
                System.out.format("Staticizer gate cap: inst=%s cap=%g\n%s\n",
                                  inst, result, e);
                                   
            }
        }

        return result;
    }

    /**
     * Returns the diffusion cap on <code>node</code> due to feedback
     * transistors <code>e</code>.
     **/
    private static double getStaticizerDiffusionCap(final NetGraph.NetEdge e,
                                                    final NetGraph.NetNode node,
                                                    final TechnologyData tech,
                                                    final HierName inst) {
        double result = 0;

        if (!e.isUsedLogic() && (e.drain == node || e.source == node)) {
            result = e.width *
                     tech.getUnitDiffusionCapacitance(e.type,
                                                      e.getTransistorType());
            if (DebugOption.printLevel <= 1) {
                System.out.format("Staticizer diff cap: inst=%s cap=%g\n%s\n",
                                  inst, result, e);
            }
        }

        return result;
    }

    /**
     * Returns the total gate and diffusion cap on <code>node</code> due to
     * feedback transistors.
     **/
    private static AdditiveTerms getStaticizerLoad(
            final NetGraph transistors,
            final NetGraph.NetNode node,
            final HierName inst,
            final Set<Pair<HierName,NetGraph.NetNode>> seen,
            final TechnologyData tech) {
        double cap = 0;
        if (seen.add(new Pair<HierName,NetGraph.NetNode>(inst, node))) {
            for (Iterator i = transistors.getEdges().iterator(); i.hasNext(); )
            {
                final NetGraph.NetEdge e = (NetGraph.NetEdge) i.next();
                cap += getStaticizerGateCap(e, node, tech, inst) +
                       getStaticizerDiffusionCap(e, node, tech, inst);
            }
        }

        return cap == 0 ? new AdditiveTerms(0)
                        : new AdditiveTerms(new FunctionTerm(cap));
    }

    /** 
     * get localnode delay function
     *
     * Returns delay function for halfoperator "ho1" to drive this
     * global net.  The delay function is an ArrayList of
     * FunctionTerm's.
     *
     * Uses Elmore delay model with intrinsic delay.
     * Uses Pi model for wiring, with C_wire/2 on either side of R_wire.
     * 
     * delay = intrinsic_delay +
     *         R_tran * (C_wire   + C_tran     + C_diffusion     + C_extra) +
     *         R_wire * (C_wire/2 + C_tran_far + C_diffusion_far + C_extra)
     *
     * C_tran_far includes only sinks that are not in the same leaf
     * instance as the driver.  C_diffusion_far includes only sources
     * that are not in the same leaf instance as the driver.  The
     * driver leaf instance is arbitrarily chosen as the instanceName
     * of the first netSource that maches ho1.
     **/
    public void getDelayFunction(HalfOperator ho1,
                                 List<FunctionTerm> delayFunction) {
        TechnologyData tech = getTechnologyData();

        // get wire capacitance and wire resistance
        double wireCap = getWireCapacitance();
        double wireRes = getWireResistance();
        double totalCap = wireCap;

        // debugging
        if (DebugOption.printLevel <= 1) {
            System.out.println("Computing delay for half-operator: " +
                               ho1.subType.typeName + "/" +
                               ho1.outputNode.name +
                               (ho1.driveDirection ==
                                HalfOperator.DriveDirection.PULL_DOWN ?
                                '-' : '+') + " with top net " +
                               topCellNet.canonicalName + " in " +
                               topCellNet.container.typeName);
            System.out.println("Estimated wire length: " + getEstimatedWireLength());
            System.out.println("Estimated wire capacitance: " + wireCap);
            System.out.println("Estimated wire resistance: " + wireRes);
        }

        // process resistance shielding factor and threshold
        double resistanceShieldingFactor;
        if(wireRes > tech.resistanceShieldingThreshold){
            resistanceShieldingFactor = tech.resistanceShieldingFactor;
        }
        else{
            resistanceShieldingFactor = 1.0;
        }
 
        // error if "ho1" is not a driver of this net
        assert isDrivenByHalfOperator(ho1)
            : "This halfoperator is not a source of the globalnet.\n" +
            "CellName: " + ho1.outputNet.container.typeName + "\n" +
            "NetName: " + ho1.outputNet.canonicalName.getCadenceString() +
            (ho1.driveDirection == HalfOperator.DriveDirection.PULL_DOWN ?
             "-" : "+");

        // check depth
        assert ho1.getMaxDepth() > 0
            : "Incorrect number for maxDepth of a half-operator.\n" +
            "CellName: " + ho1.outputNet.container.typeName + "\n" +
            "NetName: " + ho1.outputNet.canonicalName.getCadenceString() +
            (ho1.driveDirection == HalfOperator.DriveDirection.PULL_DOWN ?
             "-" : "+");

        // get gate parameters if any
        Float directiveIntrinsicDelay = null;
        if (tech.getUseIntrinsicCap()) {
            // Does the half-operator match a gate?
            final NetGraph.GateInstance gate = ho1.outputNode.getGate();
            if (gate != null) {
                final HierName portName =
                    gate.getPortName(ho1.outputNode.getName());

                // find the directives we need
                final CellInterface gateCell =
                    getCastDesign().getCellForGate(gate);
                directiveIntrinsicDelay = (Float)
                    DirectiveUtils.getHalfOpDirectiveValue(gateCell,
                            DirectiveConstants.INTRINSIC_DELAY,
                            portName,
                            ho1.driveDirection ==
                                HalfOperator.DriveDirection.PULL_UP,
                            getCadencize());
            }
        }

        // get stack parameters
        final int stackLimit;
        final double[] gateIntrinsicDelay;
        final double[] effectiveResistanceFactors;
        if (ho1.driveDirection == HalfOperator.DriveDirection.PULL_DOWN) {
            stackLimit = tech.stackLimitN;
            gateIntrinsicDelay = tech.getGateIntrinsicDelayN(ho1.getTransistorType());
            effectiveResistanceFactors = tech.getEffectiveResistanceFactorN(ho1.getTransistorType());
        } else {
            stackLimit = tech.stackLimitP;
            gateIntrinsicDelay = tech.getGateIntrinsicDelayP(ho1.getTransistorType());
            effectiveResistanceFactors = tech.getEffectiveResistanceFactorP(ho1.getTransistorType());
        }

        final double intrinsicDelay;
        if (directiveIntrinsicDelay != null) {
            intrinsicDelay = directiveIntrinsicDelay.floatValue();
        } else {
            final int maxDepth = Math.min(ho1.getMaxDepth(), stackLimit);
            intrinsicDelay = gateIntrinsicDelay[maxDepth - 1];
        }

        // intrinsic delay
        if (intrinsicDelay != 0.0) {
            FunctionTerm ftm = new FunctionTerm();
            ftm.type = FunctionTerm.Type.CONSTANT;
            ftm.coefficient = intrinsicDelay;
            delayFunction.add(ftm);
            if (DebugOption.printLevel <= 1)
                System.out.println("Intrinsic Delay: " + 
                                   ftm.coefficient);
        }

        // Always use effectiveResistanceFactor for depth 1, even
        // when that is not the maxDepth.  This normalizes everything to
        // inverters.
        final double effectiveResistanceFactor =
            effectiveResistanceFactors[0] / ho1.getStrengthBias();

        if (DebugOption.printLevel <= 1) {
            System.out.println("Intrinsic delay is: " + intrinsicDelay);
            System.out.println("initial R_gate: " + effectiveResistanceFactor);
            System.out.println("strength_bias: " + ho1.getStrengthBias());
            System.out.println("R_gate: " + effectiveResistanceFactor);
            System.out.println("half-operator: " + ho1.subType.typeName + "/" +
                               ho1.outputNode.name +
                               (ho1.driveDirection ==
                                HalfOperator.DriveDirection.PULL_DOWN ?
                                '-' : '+'));
        }

        // Find first netSource with ho1 and use its instanceName as
        // sourceInstName.  For localnodes of leaf cells, this is null
        // and all sinks are considered far and sources considered
        // near.
        if (listSources.isEmpty()){
            return;
        }
        NetSource hoNetSource = null;
        for (Iterator i = listSources.iterator(); i.hasNext(); ) {
            NetSource netSource = (NetSource) i.next();
            if (netSource.type == NetType.HALF_OPERATOR_TRANSISTOR &&
                netSource.source == ho1) {
                hoNetSource = netSource;
                break;
            }
        }
        assert hoNetSource != null;
        HierName sourceInstName = hoNetSource.getInstanceName();
        if (DebugOption.printLevel<=1)
          System.out.println("sourceInstName=" + sourceInstName);

        // Find sources on the far side of the resistive wire.
        // Sources connected internally within the driving leaf cell
        // are considered near (before the resistor).
        ArrayList farSources = new ArrayList();
        for (Iterator i = listSources.iterator(); i.hasNext(); ) {
            NetSource netSource = (NetSource) i.next();
            HierName instName = netSource.getInstanceName();
            if (netSource.type == NetType.HALF_OPERATOR_TRANSISTOR &&
                sourceInstName == null) {
                if (DebugOption.printLevel<=1)
                  System.out.println("leafSource: " + netSource);
            }
            else if (netSource.type == NetType.HALF_OPERATOR_TRANSISTOR &&
                instName != null && sourceInstName != null &&
                instName.equals(sourceInstName)) {
                if (DebugOption.printLevel<=1)
                  System.out.println("nearSource: " + netSource);
            }
            else {
                if (DebugOption.printLevel<=1)
                  System.out.println("farSource: " + netSource);
                farSources.add(netSource);
            }
        }

        // Find sinks on the far side of the resistive wire.  Sinks
        // connected internally within the driving leaf cell are
        // considered far (after the resistor).
        ArrayList farSinks = new ArrayList();
        for (Iterator i = listSinks.iterator(); i.hasNext(); ) {
            NetSink netSink = (NetSink) i.next();
            HierName instName = netSink.getInstanceName();
            if (netSink.type == NetType.HALF_OPERATOR_TRANSISTOR &&
                instName != null && sourceInstName != null &&
                instName.equals(sourceInstName)) {
                if (DebugOption.printLevel<=1)
                  System.out.println("nearSink: " + netSink);
            }
            else {
                if (DebugOption.printLevel<=1)
                  System.out.println("farSink: " + netSink);
                farSinks.add(netSink);
            }
        }

        AdditiveTerms ftma;

        // for convenience R_tran also includes elmoreDelayFactor and
        // resistanceShieldingFactor
        final AdditiveTerms R_tran =
            new AdditiveTerms(
                ho1.getResistanceTerm().multiply(
                        new FunctionTerm(tech.elmoreDelayFactor *
                                         resistanceShieldingFactor)));

        // R_tran * C_total
        final FunctionTerm C_total = new FunctionTerm(totalCap);
        ftma = R_tran.multiply(C_total);
        delayFunction.addAll(ftma);
        debugTerm("Delay from R_tran * C_total: ", ftma);

        final Set<Pair<HierName,NetGraph.NetNode>> staticizerProcessed =
            new HashSet<Pair<HierName,NetGraph.NetNode>>();
        for (Iterator i = listSinks.iterator(); i.hasNext(); ) {
            NetSink nska = (NetSink) i.next();

            // R_tran * C_tran
            if (nska.type == NetType.HALF_OPERATOR_TRANSISTOR) {
                // netsink is a halfoperator
                final AdditiveTerms C_tran = nska.getGateCapacitanceTerm();
                ftma = R_tran.multiply(C_tran);
                delayFunction.addAll(ftma);
                debugTerm("Delay from R_tran * C_tran: ", ftma);

                ftma = R_tran.multiply
                    (getStaticizerLoad(nska.sink.subType.transistors,
                                       nska.transistor.gate,
                                       nska.getInstanceName(),
                                       staticizerProcessed,
                                       tech));
                delayFunction.addAll(ftma);
                debugTerm("Delay from R_tran * C_stat: ", ftma);
            }

            // R_tran * C_extra
            if (nska.type == NetType.CAPACITIVE_LOAD) {
                // netsink is a constant capacitive load
                final FunctionTerm C_extra =
                    new FunctionTerm(nska.getLoadCapacitance());
                ftma = R_tran.multiply(C_extra);
                delayFunction.addAll(ftma);
                debugTerm("Delay from R_tran * C_extra: ", ftma);
            }
        }

        // R_tran * C_diffusion
        for (Iterator i = listSources.iterator(); i.hasNext(); ) {
            NetSource nsrca = (NetSource) i.next();
            if (nsrca.type != NetType.HALF_OPERATOR_TRANSISTOR) continue;

            final AdditiveTerms C_diffusion =
                nsrca.source.getDiffusionCapacitanceTerm();
            ftma = R_tran.multiply(C_diffusion);
            delayFunction.addAll(ftma);
            debugTerm("Delay from R_tran * C_diffusion: ", ftma);
            
            ftma = R_tran.multiply
                (getStaticizerLoad(nsrca.source.subType.transistors,
                                   nsrca.source.outputNode,
                                   nsrca.getInstanceName(),
                                   staticizerProcessed,
                                   tech));
            delayFunction.addAll(ftma);
            debugTerm("Delay from R_tran * C_stat: ", ftma);
        }

        // for convenience, R_wire includes wireRCDelayFactor
        final AdditiveTerms R_wire =
            new AdditiveTerms(
                new FunctionTerm(wireRes * tech.wireRCDelayFactor));
        final FunctionTerm C_wire = new FunctionTerm(wireCap);

        // R_wire * C_wire / 2
        ftma = R_wire.multiply(C_wire).multiply(new FunctionTerm(0.5));
        delayFunction.addAll(ftma);
        debugTerm("Delay from R_wire * C_wire / 2: ", ftma);
        
        staticizerProcessed.clear();
        for (Iterator i = farSinks.iterator(); i.hasNext(); ) {
            NetSink nska = (NetSink) i.next();

            // R_wire * C_tran_far
            if (nska.type == NetType.HALF_OPERATOR_TRANSISTOR) {
                // netsink is a halfoperator
                final AdditiveTerms C_tran_far = nska.getGateCapacitanceTerm();
                ftma = R_wire.multiply(C_tran_far);
                delayFunction.addAll(ftma);
                debugTerm("Delay from R_wire * C_tran_far: ", ftma);

                ftma = R_wire.multiply
                    (getStaticizerLoad(nska.sink.subType.transistors,
                                       nska.transistor.gate,
                                       nska.getInstanceName(),
                                       staticizerProcessed,
                                       tech));
                delayFunction.addAll(ftma);
                debugTerm("Delay from R_wire * C_stat_far: ", ftma);
            }

            // R_wire * C_extra
            if (nska.type == NetType.CAPACITIVE_LOAD) {
                final FunctionTerm C_extra =
                    new FunctionTerm(nska.getLoadCapacitance());
                ftma = R_wire.multiply(C_extra);
                delayFunction.addAll(ftma);
                debugTerm("Delay from R_wire * C_extra: ", ftma);
            }
        }

        // R_wire * C_diffusion_far
        for (Iterator i = farSources.iterator(); i.hasNext(); ) {
            NetSource nsrca = (NetSource) i.next();
            if (nsrca.type != NetType.HALF_OPERATOR_TRANSISTOR) continue;

            final AdditiveTerms C_diffusion_far =
                nsrca.source.getDiffusionCapacitanceTerm();
            ftma = R_wire.multiply(C_diffusion_far);
            delayFunction.addAll(ftma);
            debugTerm("Delay from R_wire * C_diffusion_far: ", ftma);

            ftma = R_wire.multiply
                (getStaticizerLoad(nsrca.source.subType.transistors,
                                   nsrca.source.outputNode,
                                   nsrca.getInstanceName(),
                                   staticizerProcessed,
                                   tech));
            delayFunction.addAll(ftma);
            debugTerm("Delay from R_wire * C_stat_far: ", ftma);
        }
    }



    // Global nets generation from the original design
    // * This is be done everytime any sub-typing is done
    // Main algorithm:
    // 1. The objective of this function is to assign a list of "global nets" to "local nets" defined
    // in all sub-types in a design.
    // 2. The "global nets" are used for transistor sizing purpose, for path generation and delay
    // calculation.
    // 3. The outputs of this function are: a list of all global nets, lists of "NetSource" and "NetSink"
    // for each global net, list of global nets for each half-operator in the design.
    // 
    // Description:
    //
    //    L is a sorted list of sub-types, the sub-types in the list are sorted from high to low according
    //    to their level number (number of hierarchies).
    //    L.add(top-level sub-type);
    //    while (L is not empty){
    //        S = L.get(0);
    //        for each not-port net in S {
    //            generate a new global net;
    //            add the pointer of the new global net to the "global net list" of the cell-net;
    //        }
    //        for each net in S {
    //            update the source/sink lists of the corresponding global nets;
    //            propagate the global net list to the lower-level sub-types;
    //        }
    //        add all the sub-types in S to L sorted;
    //    }
    //
    //    * Hope it works fine.


    interface ConstructionListener {
        void addCellNet(GlobalNet gn, CellNet cn);
    }

    public static void generateGlobalNets(
            CellType top,
            List/*<GlobalNet>*/ generatedGlobalNets) {
        generateGlobalNets(top, generatedGlobalNets,
            new ConstructionListener() {
                public void addCellNet(GlobalNet gn, CellNet cn) { }
            });
    }

    public static void generateGlobalNets(
            CellType top,
            List/*<GlobalNet>*/ generatedGlobalNets,
            ConstructionListener listener) {
        // top: pointer to the top-level cell
        // generatedGlobalNets: pointer to the list to store all the global nets
        if(DebugOption.printLevel <= 1){
            System.out.println("******** Start of Global Net Generation *************");
        }

        // sorted list of all the cells to be processed
        List/*<CellType>*/ cellTypes = new ArrayList/*<CellType>*/();

        cellTypes.add(top); // initialize the list with the top-level sub-type
        while (!cellTypes.isEmpty()) {
            CellType sta = (CellType) cellTypes.get(0); // get the first sub-type in the sorted list
            if(DebugOption.printLevel <= 1){
                System.out.println("***********************************************************");
                sta.print();
                System.out.println("***********************************************************");
            }

            // get all the nets in this cell
            Set/*<CellNet>*/ allNets = sta.getAllNets();
            
            // Generate new global nets for needed cell-nets
            for (Iterator ita = allNets.iterator(); ita.hasNext(); ) {
                CellNet cnta = (CellNet)ita.next(); // next net
                if(DebugOption.printLevel <= 1){
                    cnta.print(new PrintWriter(System.out));
                }

                List/*<GlobalNet>*/ globalNetsForCellNet =
                    cnta.getGlobalNets();

                if(cnta.isPortNet()){ // this is a port net
                    // REVIEW: can we make an assertion that the top level cell
                    // never has ports?
                    if (globalNetsForCellNet.isEmpty()) { // this must be the top-level cell
                        GlobalNet gnta = new GlobalNet(cnta); // create a new global net
                        if(DebugOption.printLevel <= 1){
                            System.out.println(cnta.canonicalName.getCadenceString() + " port net, global net created");
                            System.out.println("-------------------------------------------------\n");
                        }
                        generatedGlobalNets.add(gnta);
                        globalNetsForCellNet.add(gnta);
                        gnta.updateWireParam(cnta);
                        listener.addCellNet(gnta, cnta);
                    }
                    else{ // this is not a top-level cell
                        if(DebugOption.printLevel <= 1){
                            System.out.println(cnta.canonicalName.getCadenceString() + " port net, Nothing created");
                            System.out.println("-------------------------------------------------\n");
                        }
                        // will not create new global net for its port nets
                    }
                }
                else{ // this is an internal net

                    // the list must be empty for non-top-level cells
                    assert globalNetsForCellNet.isEmpty()
                        : "Internal net has non-empty list of global " +
                          "nets before being processed for the first time.\n" +
                          "CellName: " + sta.typeName + "\n" +
                          "NetName: " +
                          cnta.canonicalName.getCadenceString();

                    GlobalNet gnta = new GlobalNet(cnta); // create a new global net
                    if(DebugOption.printLevel <= 1){
                        System.out.println(cnta.canonicalName.getCadenceString() +" internal net, global net created");
                        System.out.println("-------------------------------------------------\n");
                    }
                    generatedGlobalNets.add(gnta);
                    globalNetsForCellNet.add(gnta);
                    gnta.updateWireParam(cnta);
                    listener.addCellNet(gnta, cnta);
                }

                // Generate special global net for load directives
                double load = cnta.getAssignedLoad();
                assert load == 0.0;
                // REVIEW: The code below looks completely wrong, hence
                // the above assertion.  The load is dealt with elsewhere,
                // but it's unclear if that handling is correct.
                if(load > 0.0){
                    GlobalNet gnta = new GlobalNet(cnta);
                    if(DebugOption.printLevel <= 1){
                        System.out.println("-------------------------------------------------\n");
                        System.out.println("Special global net generated for load directives");
                        System.out.println("-------------------------------------------------\n");
                    }
                    generatedGlobalNets.add(gnta);
                    globalNetsForCellNet.add(gnta);
                }

            }  // for allNets

            
            // Update source/sink lists, and their coordinates
            if(DebugOption.printLevel <= 1){
                System.out.println("***********************************************************");
                System.out.println("Update list of sources, list of sinks, orientations, and coordinates");
                System.out.println("***********************************************************");
            }

            for (Iterator ita = allNets.iterator(); ita.hasNext(); ) {
                CellNet cnta = (CellNet)ita.next(); // get next net

                List/*<NetSource>*/ sourcesForCellNet = cnta.getListSources(); // list of sources for the cell net
                List/*<NetSink>*/ sinksForCellNet = cnta.getListSinks(); // list of sinks for the cell net

                if(DebugOption.printLevel <= 1){
                    System.out.println("-------------------------------------------------------------");
                    System.out.println(cnta.canonicalName.getCadenceString());
                    System.out.println("n_sources = " +
                                           sourcesForCellNet.size());
                    for (int iSource = 0; iSource < sourcesForCellNet.size(); ++iSource) {
                        NetSource nsra = (NetSource)sourcesForCellNet.get(iSource);
                        System.out.println("nsra "+nsra);
                    }
                    System.out.println("n_sinks = " +
                                           sinksForCellNet.size());
                    for (int iSink = 0; iSink < sinksForCellNet.size(); ++iSink) {
                        NetSink nska = (NetSink)sinksForCellNet.get(iSink);
                        System.out.println("nska "+nska);
                    }
                    System.out.println("n_globalnets = " + cnta.getGlobalNets().size());
                }

                if(cnta.isPortNet()){ // must update x/y coordinates for port nets
                    for (Iterator itb = cnta.getGlobalNets().iterator();
                         itb.hasNext(); ) {
                        GlobalNet gnta = (GlobalNet)itb.next();
                        int numSources = 0;
                        int numSinks = 0;
                        

                        // processing net sources first
                        List/*<NetSource>*/ newSources =
                            new ArrayList/*<NetSource>*/();
                        List/*<NetSink>*/ newSinks =
                            new ArrayList/*<NetSink>*/();

                        for (Iterator itc = gnta.listSources.iterator(); itc.hasNext(); ) {
                            NetSource nsra = (NetSource)itc.next();

                            if(nsra.type == NetType.CELL){
                                if((nsra.cellSource == sta) &&(nsra.setSubcellNets.contains(cnta))){ 
                                        // look for this cell and this net in list

                                    double cx = nsra.coordinateX;
                                    double cy = nsra.coordinateY;

                                    for (Iterator itd = sourcesForCellNet.iterator(); itd.hasNext(); ) {
                                        NetSource nsrb = (NetSource)itd.next();
    
                                        NetSource nsrc = new NetSource(nsrb);

                                        double[] e =
                                            transformCoordinates(nsra.orientation,
                                                                 cx, cy,
                                                                 nsrc.coordinateX,
                                                                 nsrc.coordinateY);
                                        nsrc.coordinateX = e[0];
                                        nsrc.coordinateY = e[1];

                                        nsrc.updateOrientation(nsra.orientation);
                                        nsrc.prefixInstanceName(nsra.getInstanceName());
                                        if(sta.getLevel() > 0){
                                            HierName childNetName = null;
                                            for (Iterator ite = cnta.subcellconnectionNames.iterator(); ite.hasNext(); ) {
                                                HierName h = (HierName) ite.next();
                                                ConnectionInfo ci = (ConnectionInfo) sta.getSubcellConnectedTo(h);
                                                if((ci != null) && (ci.nameInParent.equals(nsrb.getInstanceName()))) {
                                                    childNetName = ci.getChildName(h);
                                                    if(childNetName != null) {
                                                        break;
                                                    }
                                                }
                                            }
                                            assert childNetName != null;
                                            CellType.PortInfo pi = nsrb.cellSource.getPortInfoForPinNamed(childNetName);
                                            assert pi != null;
                                            //System.out.println("Update1 nsrc "+nsrc);
                                            //System.out.println("Update1 bBox "+pi.bBox);
                                            if(pi.bBox != null) {
                                                final Point ll = pi.bBox.getLowerLeft();
                                                final Point ur = pi.bBox.getUpperRight();
                                                double[] ell = transformCoordinates(nsrc.orientation,
                                                                                    nsrc.coordinateX,
                                                                                    nsrc.coordinateY,
                                                                                    ll.getX(),
                                                                                    ll.getY());

                                                double[] eur = transformCoordinates(nsrc.orientation,
                                                                                    nsrc.coordinateX,
                                                                                    nsrc.coordinateY,
                                                                                    ur.getX(),
                                                                                    ur.getY());
                                                BoundingBox NewbBox = new BoundingBox(ell[0],
                                                                                      ell[1],
                                                                                      eur[0],
                                                                                      eur[1]);
                                                //System.out.println("Update1 after bBox "+NewbBox);
                                                Node<BoundingBox> newSrc = new Node(NewbBox);
                                                //Do not start the tree until you find
                                                //routed or a leaf cell
                                                if(CellUtils.isRouted(nsrc.cellSource.cast_cell) ||
                                                   (nsrc.cellSource.getLevel() == 0)) {
                                                    Tree<BoundingBox> newTree = new Tree();
                                                    newTree.setRootElement(newSrc);
                                                    gnta.steinerTree.addRoot(newTree);
                                                    if(nsrc.cellSource.getLevel() == 0) {
                                                        gnta.sourceList.add(newSrc);
                                                    }
                                                }
                                            }
                                        }
                                        newSources.add(nsrc);
                                        ++numSources;
                                    }  // for sourcesForCellNet

                                    if (cnta.portDirection == CellNet.OUTPUT) {
                                        // OUTPUT port with sinks
                                        if (DebugOption.printLevel <= 2) {
                                            if (!sinksForCellNet.isEmpty()) {
                                                System.out.println("NOTE: Found output port with sinks " + sta.typeName 
                                                                    + "    " + cnta.canonicalName.getCadenceString());
                                            }
                                        }

                                        for (Iterator itd = sinksForCellNet.iterator(); itd.hasNext(); ) {
                                            NetSink nskb = (NetSink)itd.next();

                                            assert nskb.type == NetType.CAPACITIVE_LOAD
                                                : "Output port net can only have capacitor-type sink."; 

                                            NetSink nskc = new NetSink(nskb);

                                            double[] e =
                                                transformCoordinates(nsra.orientation,
                                                                     cx, cy,
                                                                     nskc.coordinateX,
                                                                     nskc.coordinateY);
                                            nskc.coordinateX = e[0];
                                            nskc.coordinateY = e[1];

                                            nskc.updateOrientation(nsra.orientation);

                                            newSinks.add(nskc);
                                            ++numSinks;
                                        }  // for sinksForCellNet
                                    }  // if cnta.portDirection == CellNet.OUTPUT
                                }  // if
                            }  // if nsra.type == NetType.CELL
                        }  // for gnta.listSources

                        gnta.listSources.addAll(newSources);
                        gnta.listSinks.addAll(newSinks);


                        if(DebugOption.printLevel <= 1){
                            System.out.println("\t\tNumber of sources: " +
                                               numSources);
                        }



                        // processing net sinks next
                        newSinks.clear();

                        for (Iterator itc = gnta.listSinks.iterator(); itc.hasNext(); ) {
                            NetSink nska = (NetSink)itc.next();
                            if(nska.type == NetType.CELL){
                                if((nska.cellSink == sta) && (nska.setSubcellNets.contains(cnta))){ 
                                        // look for this cell in list

                                    double cx = nska.coordinateX;
                                    double cy = nska.coordinateY;

                                    for (Iterator itd = sinksForCellNet.iterator(); itd.hasNext(); ) {
                                        NetSink nskb = (NetSink)itd.next();

                                        NetSink nskc = new NetSink(nskb);

                                        double[] e =
                                            transformCoordinates(nska.orientation,
                                                                 cx, cy,
                                                                 nskc.coordinateX,
                                                                 nskc.coordinateY);

                                        nskc.coordinateX = e[0];
                                        nskc.coordinateY = e[1];

                                        nskc.updateOrientation(nska.orientation);
                                        nskc.prefixInstanceName(nska.getInstanceName());

                                        if(sta.getLevel() > 0 &&
                                           nskb.type == NetType.CELL){
                                            HierName childNetName = null;
                                            for (Iterator ite = cnta.subcellconnectionNames.iterator(); ite.hasNext(); ) {
                                                HierName h = (HierName) ite.next();
                                                ConnectionInfo ci = (ConnectionInfo) sta.getSubcellConnectedTo(h);
                                                if((ci != null) && (ci.nameInParent.equals(nskb.getInstanceName()))) {
                                                    childNetName = ci.getChildName(h);
                                                    if(childNetName != null) {
                                                        break;
                                                    }
                                                }
                                            }
                                            assert childNetName != null;
                                            CellType.PortInfo pi = nskb.cellSink.getPortInfoForPinNamed(childNetName);
                                            assert pi != null;
                                            if(pi.bBox != null) {
                                                final Point ll = pi.bBox.getLowerLeft();
                                                final Point ur = pi.bBox.getUpperRight();
                                                double[] ell = transformCoordinates(nskc.orientation,
                                                                                    nskc.coordinateX,
                                                                                    nskc.coordinateY,
                                                                                    ll.getX(),
                                                                                    ll.getY());

                                                double[] eur = transformCoordinates(nskc.orientation,
                                                                                    nskc.coordinateX,
                                                                                    nskc.coordinateY,
                                                                                    ur.getX(),
                                                                                    ur.getY());
                                                BoundingBox NewbBox = new BoundingBox(ell[0],
                                                                                      ell[1],
                                                                                      eur[0],
                                                                                      eur[1]);
                                                //System.out.println("Update3 after bBox "+NewbBox);

                                                Node<BoundingBox> newSink = new Node(NewbBox);
                                                if((CellUtils.isRouted(nskc.cellSink.cast_cell) ||
                                                    (nskc.cellSink.getLevel() == 0))) {
                                                    Tree<BoundingBox> newTree = new Tree();
                                                    newTree.setRootElement(newSink);
                                                    gnta.steinerTree.addRoot(newTree);
                                                    if(nskc.cellSink.getLevel() == 0) {
                                                        gnta.sinkList.add(newSink);
                                                    }
                                                }
                                            }
                                        }
                                        newSinks.add(nskc);
                                        ++numSinks;
                                    }  // for sinksForCellNet
                                }  // if
                            }  // if nska.type == NetType.CELL
                        }  // for gnta.listSinks

                        gnta.listSinks.addAll(newSinks);

                        if(DebugOption.printLevel <= 1){
                            System.out.println("\t\tNumber of sinks: " +
                                               numSinks);
                        }
                    }  // for cnta.getGlobalNets()
                }
                else{ // internal nets
                    // there should be only ONE global net for each internal net
                    assert cnta.getGlobalNets().size() == 1;
                    GlobalNet gnta = (GlobalNet) cnta.getGlobalNets().get(0);
                    gnta.listSources.addAll(sourcesForCellNet); // add all sources to the list
                    gnta.listSinks.addAll(sinksForCellNet); // add all sinks to the list

                    if((sta.typeName != "") && (sta.getLevel() > 0)) { // not a top cell

                        for (Iterator itc = gnta.listSources.iterator(); itc.hasNext(); ) {
                            NetSource nsra = (NetSource)itc.next();
                            if(nsra.type == NetType.CELL){
                                //Check if it is routed or a leaf cell
                                if(CellUtils.isRouted(nsra.cellSource.cast_cell) || 
                                   (nsra.cellSource.getLevel() == 0)) {

                                    HierName childNetName = null;
                                    for (Iterator ite = cnta.subcellconnectionNames.iterator(); ite.hasNext(); ) {
                                        HierName h = (HierName) ite.next();
                                        ConnectionInfo ci = (ConnectionInfo) sta.getSubcellConnectedTo(h);
                                        if((ci != null) && (ci.nameInParent.equals(nsra.getInstanceName()))) {
                                            childNetName = ci.getChildName(h);
                                            if(childNetName != null) {
                                                break;
                                            }
                                        }
                                    }
                                    assert childNetName != null;
                                    CellType.PortInfo pi = nsra.cellSource.getPortInfoForPinNamed(childNetName);
                                    assert pi != null;

                                    if(pi.bBox != null) {
                                        final Point ll = pi.bBox.getLowerLeft();
                                        final Point ur = pi.bBox.getUpperRight();
                                        double[] ell = transformCoordinates(nsra.orientation,
                                                                            nsra.coordinateX,
                                                                            nsra.coordinateY,
                                                                            ll.getX(),
                                                                            ll.getY());

                                        double[] eur = transformCoordinates(nsra.orientation,
                                                                            nsra.coordinateX,
                                                                            nsra.coordinateY,
                                                                            ur.getX(),
                                                                            ur.getY());
                                        BoundingBox NewbBox = new BoundingBox(ell[0],
                                                                              ell[1],
                                                                              eur[0],
                                                                              eur[1]);

                                        Node<BoundingBox> rootSrc = new Node(NewbBox);
                                        Tree<BoundingBox> newTree = new Tree();
                                        newTree.setRootElement(rootSrc);
                                        gnta.steinerTree.addRoot(newTree);
                                        if(nsra.cellSource.getLevel() == 0) {
                                            gnta.sourceList.add(rootSrc);
                                        }
                                    }
                                }
                            }
                        }
                        
                        for (Iterator itc = gnta.listSinks.iterator(); itc.hasNext(); ) {
                            NetSink nska = (NetSink)itc.next();
                            if(nska.type == NetType.CELL){
                                //Check if it is routed or a leaf cell
                                if(CellUtils.isRouted(nska.cellSink.cast_cell) || 
                                   (nska.cellSink.getLevel() == 0)) {

                                    HierName childNetName = null;
                                    for (Iterator ite = cnta.subcellconnectionNames.iterator(); ite.hasNext(); ) {
                                        HierName h = (HierName) ite.next();
                                        ConnectionInfo ci = (ConnectionInfo) sta.getSubcellConnectedTo(h);
                                        if((ci != null) && (ci.nameInParent.equals(nska.getInstanceName()))) {
                                            childNetName = ci.getChildName(h);
                                            if(childNetName != null) {
                                                break;
                                            }
                                        }
                                    }
                                    assert childNetName != null;
                                    CellType.PortInfo pi = nska.cellSink.getPortInfoForPinNamed(childNetName);
                                    assert pi != null;

                                    if(pi.bBox != null) {
                                        final Point ll = pi.bBox.getLowerLeft();
                                        final Point ur = pi.bBox.getUpperRight();
                                        double[] ell = transformCoordinates(nska.orientation,
                                                                            nska.coordinateX,
                                                                            nska.coordinateY,
                                                                            ll.getX(),
                                                                            ll.getY());

                                        double[] eur = transformCoordinates(nska.orientation,
                                                                            nska.coordinateX,
                                                                            nska.coordinateY,
                                                                            ur.getX(),
                                                                            ur.getY());
                                        BoundingBox NewbBox = new BoundingBox(ell[0],
                                                                              ell[1],
                                                                              eur[0],
                                                                              eur[1]);

                                        Node<BoundingBox> rootSink = new Node(NewbBox);
                                        Tree<BoundingBox> newTree = new Tree();
                                        newTree.setRootElement(rootSink);
                                        gnta.steinerTree.addRoot(newTree);
                                        if(nska.cellSink.getLevel() == 0) {
                                            gnta.sinkList.add(rootSink);
                                        }
                                    }
                                }
                            }
                        } 
                    }
                }

            }  // for allNets




            // Propagate global net one-level down for non-leaf cells
            if(sta.getLevel() > 0){
                if(DebugOption.printLevel <= 1){
                    System.out.println("***********************************************************");
                    System.out.println("Propagating global nets into lower hierarchies");
                    System.out.println("***********************************************************");
                }
                for (Iterator ita = allNets.iterator(); ita.hasNext(); ) {
                    CellNet cnta = (CellNet)ita.next();
                    if(DebugOption.printLevel <= 1){
                        System.out.println("Net names:");
                        System.out.println(cnta.toString());
                    }

                    if(DebugOption.printLevel <= 1){
                        System.out.println("\tNumber of nets in subcells: " +
                                           cnta.getSetSubcellNets(true).size());
                        System.out.println("\tList of subcell connections:\n");
                    }

                    for (Iterator itb = cnta.getSetSubcellNets(true).iterator();
                         itb.hasNext(); ) {
                        CellNet cntb = (CellNet)itb.next();
                        if(DebugOption.printLevel <= 1){
                            System.out.println(cntb.toString());
                        }

                        List/*<GlobalNet>*/ globalNetsForSubcellNet =
                            cntb.getGlobalNets();
                        for (Iterator itc = cnta.getGlobalNets().iterator(); itc.hasNext(); ) {
                            GlobalNet gna = (GlobalNet)itc.next();
                            if(!globalNetsForSubcellNet.contains(gna)){
                                globalNetsForSubcellNet.add(gna);
                                gna.updateWireParam(cntb);
                                listener.addCellNet(gna, cntb);
                            }
                        }
                    }
                }
            }  // if level > 0





            // Add new sub-types to the list
            for (Iterator itb = sta.getAllSubcells().iterator();
                 itb.hasNext(); ) {
                CellType stb = (CellType)itb.next();
                sortedInsert(cellTypes, stb);
            }

            // remove current sub-type from the list
            cellTypes.remove(0);
        } // while !cellTypes.isEmpty()


        // clean up the source/sink lists for all the global nets
        // i.e. remove sources/sinks of type 1 (CellType source/sink)
        final int numGeneratedGlobalNets = generatedGlobalNets.size();
        for(int i=0;i<numGeneratedGlobalNets;++i){
            GlobalNet gnta = (GlobalNet) generatedGlobalNets.get(i);
            
            final MultiSet/*<NetSource>*/ realSources =
                new MultiSet/*<NetSource>*/(NetSourceComparator.getInstance());
            for (Iterator ita = gnta.listSources.iterator(); ita.hasNext(); ) {
                NetSource nsra = (NetSource)ita.next();
                if(nsra.type != NetType.CELL) realSources.add(nsra);
            }
            gnta.listSources = realSources;

            final MultiSet/*<NetSink>*/ realSinks =
                new MultiSet/*<NetSink>*/(NetSinkComparator.getInstance());
            for (Iterator ita = gnta.listSinks.iterator(); ita.hasNext(); ) {
                NetSink nska = (NetSink)ita.next();
                if(nska.type != NetType.CELL) realSinks.add(nska);
            }
            gnta.listSinks = realSinks;
        }
    }

    private static double[] transformCoordinates(int orientation,
                                                 double cx,
                                                 double cy,
                                                 double dx,
                                                 double dy) {

        double ex, ey;
        switch (orientation) {

            case ConnectionInfo.R0:     
                                        ex = cx + dx;
                                        ey = cy + dy;
                                        break;

            case ConnectionInfo.R90:     
                                        ex = cx - dy;
                                        ey = cy + dx;
                                        break;

            case ConnectionInfo.R180:     
                                        ex = cx - dx;
                                        ey = cy - dy;
                                        break;

            case ConnectionInfo.R270:     
                                        ex = cx + dy;
                                        ey = cy - dx;
                                        break;

            case ConnectionInfo.MY:     
                                        ex = cx - dx;
                                        ey = cy + dy;
                                        break;

            case ConnectionInfo.MYR90:     
                                        ex = cx - dy;
                                        ey = cy - dx;
                                        break;

            case ConnectionInfo.MX:     
                                        ex = cx + dx;
                                        ey = cy - dy;
                                        break;

            case ConnectionInfo.MXR90:     
                                        ex = cx + dy;
                                        ey = cy + dx;
                                        break;


            default:     
                                        throw new AssertionError();

        }  // switch

        return new double[]{ex, ey};
    }



    /**
     * Inserts <code>st1</code> into <code>lst1</code>.  High-level cells
     * appear at the front of the list and leaf-cells at the end.
     **/
    public static void sortedInsert(List/*<CellType>*/ lst1, CellType st1) {
        int k = st1.getLevel(); // get the level number of the sub-type
        int j = lst1.size();
        for(int i=0;i<j;++i){
            CellType sta = (CellType)lst1.get(i);
            int l = sta.getLevel();

            if(l == k){
                if(sta == st1){ // the sub-type is already in the list
                    return;
                }
            }

            if(l < k){
                lst1.add(i, st1); // insert this sub-type at index i
                return;
            }
        }

        lst1.add(st1); // add this sub-type to the end of the list
    }


    public String toString(String linePrefix) {
        String                      s = "";

        double numTransistorSinks = 0.0;
        for (Iterator ita = listSinks.iterator(); ita.hasNext(); ) {
            NetSink nska = (NetSink)ita.next();
            if(nska.type == NetType.HALF_OPERATOR_TRANSISTOR){
                numTransistorSinks += 1.0 / nska.transistor.shareCount;
            }

            if(nska.type == NetType.CAPACITIVE_LOAD){
                numTransistorSinks += 1.0;
            }
        }

        s += linePrefix + "Top level net name: " + topCellNet.canonicalName.getCadenceString() + "\n";
        s += linePrefix + "Top level cell name: " + topCellNet.container.typeName + "\n";
        s += linePrefix + "Number of sources: " + listSources.size() + "\n";
        s += linePrefix + "Number of half-operator sinks: " + listSinks.size() + "\n";
        s += linePrefix + "Number of transistor sinks: " + numTransistorSinks + "\n";
        s += linePrefix + "Estimated wire length: " + getEstimatedWireLength() + "\n";

        return s;
    }


    public String toString() {
        return toString("");
    }



    public void dumpInfo(BufferedWriter bw1, String s1) {
        double numTransistorSinks = 0.0;
        for (Iterator ita = listSinks.iterator(); ita.hasNext(); ) {
            NetSink nska = (NetSink)ita.next();
            if(nska.type == NetType.HALF_OPERATOR_TRANSISTOR){
                numTransistorSinks += 1.0 / nska.transistor.shareCount;
            }

            if(nska.type == NetType.CAPACITIVE_LOAD){
                numTransistorSinks += 1.0;
            }
        }

        try{
            bw1.write(s1 + "Global_NET " + topCellNet.canonicalName.getCadenceString() + " {\n");

            bw1.write(s1 + "\tn_sources = " + listSources.size() + ";\n");
            bw1.write(s1 + "\tn_ho_sinks = " + listSinks.size() + ";\n");
            bw1.write(s1 + "\tn_tr_sinks = " + numTransistorSinks + ";\n");
            bw1.write(s1 + "\twire_length = " + getEstimatedWireLength() + ";\n");
            bw1.write(s1 + "\twire_C = " + getWireCapacitance() + ";\n");
            bw1.write(s1 + "\twire_R = " + getWireResistance() + ";\n");

            /*
            bw1.write(s1 + "\n");

            for (Iterator ita = listSources.iterator(); ita.hasNext(); ) {
                NetSource nsra = (NetSource)ita.next();
                nsra.dumpInfo(bw1, s1 + "\t");
            }

            bw1.write(s1 + "\n");

            for (Iterator ita = listSinks.iterator(); ita.hasNext(); ) {
                nska = (NetSink)ita.next();
                nska.dumpInfo(bw1, s1 + "\t");
            }
            */

            bw1.write(s1 + "}\n");
        }
        catch(IOException e){
            e.printStackTrace(System.out);
        }
    }





    /**
     * Determines if this global net has similar driver and load
     * characteristics as <code>gn1</code>.  They are considered 
     * similar if they have the same source half-operators,
     * the same sink half-operators (not transistors!) with transistor
     * sizes within 0.1%, and capacitive load sinks with caps within 5%.
     * <p> Note that this function may return false even if the GlobalNets
     * are actually similar.  This is acceptable and will merely hurt the
     * runtime.
     **/
    public boolean isSameTypeAs(GlobalNet gn1)
    {
        // must have same extra_delay directives
        if (gn1.topCellNet.getUpExtraDelay() != 
            topCellNet.getUpExtraDelay()) return false;
        if (gn1.topCellNet.getDownExtraDelay() != 
            topCellNet.getDownExtraDelay()) return false;

        // Must have monotonic R vs C, because we use only getWireRC()
        // to rank GlobalNet's and CatPath's.  Only if one GlobalNet
        // has bigger R and bigger C than another, it will have bigger
        // RC, and will in fact have bigger delay.  See BUG 11664 for
        // details.
        double R  = getWireResistance();
        double R1 = gn1.getWireResistance();
        double C  = getWireCapacitance();
        double C1 = gn1.getWireCapacitance();
        if (R<R1 && C>C1 || R>R1 && C<C1) return false;

        // must have same number of sources and sinks
        if (listSources.size() != gn1.listSources.size()) return false;
        if (listSinks.size() != gn1.listSinks.size()) return false;

        final Iterator ita = listSources.iterator();
        final Iterator itb = gn1.listSources.iterator();

        while (ita.hasNext() && itb.hasNext()) {
            final NetSource nsra = (NetSource) ita.next();
            final NetSource nsrb = (NetSource) itb.next();
            if (nsra.getType() == nsrb.getType()) {
                switch (nsra.getType()) {
                  case NetType.CAPACITIVE_LOAD:
                    // CAPACITIVE_LOAD type sources are never generated, and
                    // there isn't even a cap field in NetSource.  When this
                    // changes, we will need to do something different here,
                    // and in the comparator.
                    break;
                  case NetType.HALF_OPERATOR_TRANSISTOR:
                    if (nsra.getSource() != nsrb.getSource()) return false;
                    break;
                  case NetType.CELL:
                    throw new AssertionError("Global net should not have cell-type sink now");
                }
            } else {
                return false;
            }
        }
        if (ita.hasNext() != itb.hasNext()) return false;

        final Iterator itc = listSinks.iterator();
        final Iterator itd = gn1.listSinks.iterator();
        while (itc.hasNext() && itd.hasNext()) {
            final NetSink nskc = (NetSink) itc.next();
            final NetSink nskd = (NetSink) itd.next();
            if (nskc.getType() == nskd.getType()) {
                switch (nskc.getType()) {
                  case NetType.CAPACITIVE_LOAD:
                    // FIXME: hardcoded constant
                    if (Math.abs(nskc.loadCapacitance /
                                 nskd.loadCapacitance - 1.0) > 0.05)
                        return false;
                    break;
                  case NetType.HALF_OPERATOR_TRANSISTOR:
                    // FIXME: hardcoded constant
                    if ((nskc.sink != nskd.sink) ||
                        (Math.abs(nskc.transistor.size - nskd.transistor.size) 
                            > (0.001 * nskc.transistor.size)))
                        return false;
                    break;
                  case NetType.CELL:
                    throw new AssertionError("Global net should not have cell-type sink now");
                }
            } else {
                return false;
            }
        }
        return itc.hasNext() == itd.hasNext();
    }


    /**
     * Returns true if the <code>GlobalNet</code> is driven from
     * two or more cell types.
     **/
    public boolean isShared() {
        CellType foundCell = null;
        for (int i = 0; i < listSources.size(); ++i) {
            final NetSource netSource = (NetSource) listSources.get(i);
            if (netSource.getType() == NetType.HALF_OPERATOR_TRANSISTOR) {
                final CellType cellSource = netSource.source.subType;
                if (foundCell == null)
                    foundCell = cellSource;
                else if (foundCell != cellSource)
                    return true;
            }
        }

        return false;
    }


    public static void debugGeometry(CastDesign design, String outRoot)
    {
    }


    public double getUpOrDownDelay(int driveDirection) {
        double maxDelay = -1.0;

        for (Iterator ita = getListSources().iterator(); ita.hasNext(); ) {
            NetSource nsra = (NetSource)ita.next();
            if (nsra.type == NetType.HALF_OPERATOR_TRANSISTOR) {
                HalfOperator hoa = nsra.source;
                if (hoa.driveDirection == driveDirection) {
                    double f = calculateDelay(hoa);
                    if(f > maxDelay){
                        maxDelay = f;
                    }
                }
            }
        }

        return maxDelay;
    }


    public double getUpDelay() {
        return getUpOrDownDelay(HalfOperator.DriveDirection.PULL_UP);
    }


    public double getDownDelay() {
        return getUpOrDownDelay(HalfOperator.DriveDirection.PULL_DOWN);
    }


    double calculateDelay(HalfOperator ho1)
    {
        double delay = 0.0;
        List<FunctionTerm> lsta = new ArrayList<FunctionTerm>();

        getDelayFunction(ho1, lsta);
        for (Iterator ita = lsta.iterator(); ita.hasNext(); ) {
            FunctionTerm ftma = (FunctionTerm)ita.next();

            assert ftma.type == FunctionTerm.Type.CONSTANT;

            delay += ftma.coefficient;
        }

        return delay;
    }

    public static void updateDelayBias(final CellType top,
                                       final Cadencize cad,
                                       final PrintWriter debug) {
        // first find if any instance based delaybias directives exists, if
        // not, then return immediately, as there would be nothing to do
        final boolean[] hasDirective = new boolean[] { false };
        top.walkOnce(
            new CellTypeProcessor() {
                public void processCellType(final CellType c) {
                    final Map delaybias =
                        DirectiveUtils.getSubcellDirective(
                            c.cast_cell,
                            DirectiveConstants.DELAYBIAS,
                            DirectiveConstants.INSTANCE_TYPE);
                    if (!delaybias.isEmpty()) hasDirective[0] = true;
                }
            }
        );

        if (hasDirective[0]) {
            final InstanceData instData = new InstanceData();
            instData.updateDelayBias(top.cast_cell);
            updateDelayBias(top, null, instData, cad, new HashMap(), debug);
        } else if (debug != null) {
            debug.println("No cells in design specify instance based " +
                          "delaybias directives.");
        }
    }

    /**
     * Set the delaybias associated with each source.
     **/
    private static void updateDelayBias(final CellType cell,
                                        final HierName prefix,
                                        final InstanceData instData,
                                        final Cadencize cad,
                                        final Map instMap,
                                        final PrintWriter debug) {
        instMap.put(cell.typeName, prefix);
        if (cell.getLevel() == 0) {
            // the delaybias calculated from InstanceData includes the top
            // level delaybias specified in the cell; however, this delaybias
            // is already included in the delay calculated by
            // CellDelay.getDelay() which is stored in HalfOperator.delayBias.
            // Therefore, we divide by the top level delaybias.
            final float total = instData.getDelayBias(null);
            final float cellDelay = cell.getDelay().getCellDelayBias();

            // avoid NaNs in case delaybias = 0 in a cell
            final float curr = cellDelay == 0 ? 0 : total / cellDelay;

            if (debug != null)
                debug.println(cell.typeName + " " + prefix +
                              " total=" + total +
                              " instance=" + curr);

            final NetSource needle =
                new NetSource(NetType.HALF_OPERATOR_TRANSISTOR);
            for (Iterator i = cell.getAllNets().iterator(); i.hasNext(); ) {
                final CellNet net = (CellNet) i.next();
                if (net.getListSources().isEmpty()) continue;
                for (Iterator j = net.getGlobalNets().iterator();
                     j.hasNext(); ) {
                    final GlobalNet gn = (GlobalNet) j.next();
                    final CellNet topNet = gn.getTopCellNet();
                    // find the current instance name associated with the
                    // global net
                    final HierName inst =
                        (HierName) instMap.get(topNet.container.typeName);
                    // filter out nets that cannot contain sources in the
                    // current instance
                    if (net != topNet && !prefix.isChildOf(inst)) continue;
                    final Comparator comp = new NetSourceCellTypeComparator(
                            gn, NetType.HALF_OPERATOR_TRANSISTOR,
                            cell.typeName);
                    // narrow the search to all half operator sources that came
                    // from cell, and 
                    for (Iterator k = gn.getListSources().findAll(gn, comp);
                         k.hasNext(); ) {
                        final NetSource src = (NetSource) k.next();
                        // source instance names are relative to the global net
                        // they are on; calculate the full instance name
                        final HierName full = src.getInstanceName() == null ?
                            inst : HierName.append(inst, src.getInstanceName());
                        if (full.equals(prefix)) {
                            src.setDelayBias(curr);
                        }
                    }
                }
            }
        }
        for (Iterator i = cell.getAllSubcellConnections().iterator();
             i.hasNext(); ) {
            final ConnectionInfo ci = (ConnectionInfo) i.next();
            updateDelayBias(ci.child,
                            HierName.append(prefix, ci.nameInParent),
                            instData.translate(cell.cast_cell,
                                               ci.child.cast_cell,
                                               ci.nameInParent, cad),
                            cad, instMap, debug);
        }
    }
}
