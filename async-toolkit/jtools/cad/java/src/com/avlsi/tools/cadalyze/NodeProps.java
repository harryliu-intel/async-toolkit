/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cadalyze;

import java.io.IOException;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.StringTokenizer;

import com.avlsi.cell.CellInterface;
import com.avlsi.fast.ports.PortDefinition;
import com.avlsi.file.common.DeviceTypes;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.util.container.Pair;

import com.avlsi.tools.lvs.NetGraph;
import com.avlsi.tools.lvs.NetGraph.NetNode;
import com.avlsi.tools.lvs.NetGraph.NetEdge;
import com.avlsi.tools.lvs.NetGraph.NetPath;


/**
 * Class to contain Cadalyze node (net) properties.
 * @author Mike Davies
 * @version $Revision$ $Date$
 **/
public final class NodeProps {

    /*************************** STATIC DATA MEMBERS **************************/

    /** Print lots of debugging information? Used by debugPrintln() **/
    public static boolean debug = false;

    /****************************** DATA MEMBERS ******************************/

    /** Number of different cells that drive this node (down/up) **/
    private int[] numDrivers = { 0, 0 };

    /** Total of all driver cell drive strengths (down, up), in squares **/
    private double[] driveStrengthTotal = { 0.0, 0.0 };

    /** Fan-out count to NFET(0), PFET(1), all(2) transistors **/
    private DoubleDistribution[] fanoutDistribution = {
        new DoubleDistribution(0,10,20),
        new DoubleDistribution(0,10,20),
        new DoubleDistribution(0,20,20)
    };
    private int[] fanoutCount = { 0, 0 };

    /** Total of all gate load on this node, NFET(0) PFET(1) (area, m^2) **/
    private double[] gateLoadTotal = { 0.0, 0.0 };

    /** Distribution of total gate load **/
    private DoubleDistribution gateLoadDistribution = 
        new DoubleDistribution(0.0,40e-6*0.13e-6,20);

    /** Is this a local node? **/
    private boolean local = true;

    /** Does this NodeProps come from a leaf cell NetNode? **/
    private boolean leafNode = false;

    /** Number of different leaf cells this node connects **/
    private int numConnections = 0;

    /** Number of nodes represented; greater than 1 only for tallied stats **/
    private int numNodes = 0;

    /** 
     * Number of nodes in this tally group that represent local (internal)
     * leaf cell nodes.  (Needed to adjust averageConnections calculation.)
     **/
    int numLocalLeafNodes = 0;

    private HierName name = null;

    /******************************** METHODS ********************************/

    NodeProps() {}

    NodeProps(NetNode netnode) {
        if (netnode.isOutput()) {
            driveStrengthTotal = netnode.getAverageDriveStrength();
            for (int i=0; i<2; i++) {
                if (driveStrengthTotal[i] != 0.0) numDrivers[i] = 1;
                else debugPrintln("Warning: node "+netnode.name+
                                  " has zero drive strength towards "+i);
            }
        }
        fanoutCount = netnode.getTransistorFanout();
        gateLoadTotal = netnode.getTransistorLoad();
        numNodes = 1;
        numConnections = 1;
        leafNode = true;
        name = netnode.getName();
    }

    /**
     * Called by a parent cell when connecting one of its nodes to a 
     * subcell's port node.  Mark the subcell's node as being non-local
     * since it connects upward in the hierarchy.
     **/
    void connectNode(NodeProps alias) {
        if (alias == null) return;
        for (int i=0; i<2; i++) {
            numDrivers[i]         += alias.numDrivers[i];
            driveStrengthTotal[i] += alias.driveStrengthTotal[i];
            fanoutCount[i]        += alias.fanoutCount[i];
            gateLoadTotal[i]      += alias.gateLoadTotal[i];
        }
        numConnections += alias.numConnections;
        numNodes = 1;
        alias.local = false;
    }

    /**
     * Accumulate an actual node into a node statistics tally.
     * Note that driveStrengthTotal now is treated as the sum of
     * *average* drive strengths per node.
     **/
    void accumulateNode(NodeProps node) {
        if (node == null ) return;
        for (int i=0; i<2; i++) {
            numDrivers[i]         += numDrivers[i];
            driveStrengthTotal[i] += node.numDrivers[i] == 0 ? 0.0 :
                node.driveStrengthTotal[i] / node.numDrivers[i];
            fanoutCount[i]        += node.fanoutCount[i];
            gateLoadTotal[i]      += node.gateLoadTotal[i];
            fanoutDistribution[i].addValue(node.fanoutCount[i]);
        }
        fanoutDistribution[2].addValue(node.fanoutCount[0]+node.fanoutCount[1]);
        gateLoadDistribution.addValue(node.gateLoadTotal[0]+
                                      node.gateLoadTotal[1]);
        numConnections += node.numConnections;
        assert node.numNodes == 1;      // Must be a single node
        numNodes++;
        if (node.leafNode && node.local) numLocalLeafNodes++;
    }

    /**
     * Add a NodeProps statistics set to this one.  Not for use when
     * 'props' is a single node (use accumulateNode() instead).
     **/
    void addNodeProps(int cnt, NodeProps props) {
        if (props == null ) return;
        for (int i=0; i<2; i++) {
            numDrivers[i]         += cnt * props.numDrivers[i];
            driveStrengthTotal[i] += cnt * props.driveStrengthTotal[i];
            fanoutCount[i]        += cnt * props.fanoutCount[i];
            gateLoadTotal[i]      += cnt * props.gateLoadTotal[i];
            fanoutDistribution[i].
                addDistribution(cnt,props.fanoutDistribution[i]);
        }
        fanoutDistribution[2].
                addDistribution(cnt,props.fanoutDistribution[2]);
        gateLoadDistribution.addDistribution(cnt,props.gateLoadDistribution);
        numConnections += cnt * props.numConnections;
        numNodes += cnt * props.numNodes;
        numLocalLeafNodes += cnt * props.numLocalLeafNodes;
    }

    double getDriveStrengthUp() { 
        return numDrivers[1] == 0 ? 0.0 :
            driveStrengthTotal[1]/numDrivers[1]; 
    }
    double getDriveStrengthDn() { 
        return numDrivers[0] == 0 ? 0.0 : 
            driveStrengthTotal[0]/numDrivers[0]; 
    }

    int getTransistorFanout() { 
        return numNodes == 0 ? 0 : 
            (fanoutCount[0] + fanoutCount[1]) / numNodes; 
    }
    int getNfetFanout() { 
        return numNodes == 0 ? 0 :
            fanoutCount[DeviceTypes.N_TYPE] / numNodes; 
    }
    int getPfetFanout() { 
        return numNodes == 0 ? 0 :
            fanoutCount[DeviceTypes.P_TYPE] / numNodes; 
    }

    double getGateLoad() { 
        return numNodes == 0 ? 0.0 : 
            (gateLoadTotal[0] + gateLoadTotal[1]) / numNodes; 
    }
    double getGateLoadPerLength(double length) {
        return getGateLoad() / length;
    }
    double getGateLoadPerLength(double length, int dir) {
        return gateLoadTotal[dir] / (length * numNodes);
    }
    double getNfetLoad() { 
        return numNodes == 0 ? 0.0 :
            gateLoadTotal[DeviceTypes.N_TYPE] / numNodes; 
    }
    double getPfetLoad() { 
        return numNodes == 0 ? 0.0 : 
            gateLoadTotal[DeviceTypes.P_TYPE] / numNodes; 
    }

    int getNumConnections() {
        return leafNode ? 1 : numConnections;
    }

    boolean isLocal() { return local; }

    /** Forces this node to be non-local **/
    void setNonLocal() { local=false; }

    boolean isLeafNode() { return leafNode; }

    boolean isLoaded() { return fanoutCount[0] + fanoutCount[1] > 0; }

    /** Is this cell driven? (both high and low) **/
    boolean isDriven() { return numDrivers[0] > 0 && numDrivers[1] > 0; }

    /** Specifically for statistics gathering (when numNodes > 1) **/
    float getAverageTransistorFanout() {
        return numNodes == 0 ? 0.0F : (float)fanoutDistribution[2].getAverage();
                    //fanoutDistribution[1].getAverage());
    }

    /** Specifically for statistics gathering (when numNodes > 1) **/
    float getAverageNfetFanout() {
        return numNodes == 0 ? 0.0F : 
            (float)fanoutDistribution[DeviceTypes.N_TYPE].getAverage();
    }

    /** Specifically for statistics gathering (when numNodes > 1) **/
    float getAveragePfetFanout() {
        return numNodes == 0 ? 0.0F : 
            (float)fanoutDistribution[DeviceTypes.P_TYPE].getAverage();
    }

    /** Specifically for statistics gathering (when numNodes > 1) **/
    double getAverageDriveStrength() {
        return numNodes == 0 ? 0.0 : 
            (driveStrengthTotal[0]+driveStrengthTotal[1])/(2*numNodes);
    }

    double getAverageDriveStrength(int dir) {
        return numNodes == 0 ? 0.0 : driveStrengthTotal[dir]/numNodes;
    }

    float getAverageConnections() {
        return (float)(numConnections - numLocalLeafNodes) / 
                            (numNodes - numLocalLeafNodes);
    }

    int getNumNodes() { return numNodes; }

    /** medLen: median transistor length [m] **/
    void printHtmlStats(HtmlPage page, float medGateLength) {
        DecimalFormat formFloat = (DecimalFormat)DecimalFormat.getInstance();
        formFloat.applyPattern("#0.00");

        page.summaryTable();
        page.summaryTableLine("Number of local nodes:", getNumNodes());
        if (getAverageConnections() > 1.0F)
            page.summaryTableLine("Average number of leaf cell "+
                                  "connections per node:", 
            String.valueOf(getAverageConnections()));
        page.summaryTableLine("Average transistor fanout per node:",
            formFloat.format(getAverageTransistorFanout()));
        page.summaryTableLine("&nbsp;&nbsp;&nbsp;&nbsp;NFET",
            formFloat.format(getAverageNfetFanout()));
        page.summaryTableLine("&nbsp;&nbsp;&nbsp;&nbsp;PFET",
            formFloat.format(getAveragePfetFanout()));
        page.summaryTableLine("Average drive strength per node:",
            formFloat.format(getAverageDriveStrength()));
        page.summaryTableLine("&nbsp;&nbsp;&nbsp;&nbsp;NFET (to GND)",
            formFloat.format(getAverageDriveStrength(DeviceTypes.N_TYPE)));
        page.summaryTableLine("&nbsp;&nbsp;&nbsp;&nbsp;PFET (to Vdd)",
            formFloat.format(getAverageDriveStrength(DeviceTypes.P_TYPE)));
        page.summaryTableLine("Average gate load per node (per "+
                              formFloat.format(medGateLength)+" um):",
            formFloat.format(getGateLoadPerLength(medGateLength)*1e6));
        page.summaryTableLine("&nbsp;&nbsp;&nbsp;&nbsp;NFET",
            formFloat.format(getGateLoadPerLength(medGateLength,
                                                  DeviceTypes.N_TYPE)*1e6));
        page.summaryTableLine("&nbsp;&nbsp;&nbsp;&nbsp;PFET",
            formFloat.format(getGateLoadPerLength(medGateLength,
                                                  DeviceTypes.P_TYPE)*1e6));
        page.summaryTableEnd();

        /*
        for (int i=0; i<2; i++) {
            page.writer.p();
            fanoutDistribution[i].printHtml(page,1.0);
        }
        */
        printHtmlDistributions(page,medGateLength);
    }

    void printHtmlDistributions(HtmlPage page, float medGateLength) {
        // fanout distribution
        String imgName = page.getPagePath()+".fanout";
        fanoutDistribution[2].plotData(imgName,1.0);
        page.writer.p();
        page.writer.println("Transistor fanout distribution:<br>");
        imgName = imgName.substring(imgName.lastIndexOf('/')+1);
        page.writer.println("<IMG SRC=\""+imgName+".png\">");

        // gate load distribution
        imgName = page.getPagePath()+".load";
        gateLoadDistribution.plotData(imgName,1e6/medGateLength);
        page.writer.p();
        page.writer.println("Gate load distribution:<br>");
        imgName = imgName.substring(imgName.lastIndexOf('/')+1);
        page.writer.println("<IMG SRC=\""+imgName+".png\">");
    }

    /***************************** STATIC METHODS *****************************/

    private static void debugPrintln(final String s) {
        if (debug) System.err.println(s);
    }

    private static void debugPrint(final String s) {
        if (debug) System.err.print(s);
    }

}
