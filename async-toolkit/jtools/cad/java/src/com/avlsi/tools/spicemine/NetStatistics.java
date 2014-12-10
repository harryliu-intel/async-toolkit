/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.spicemine;

import java.io.PrintWriter;
import java.io.BufferedWriter;
import java.io.PrintStream;
import java.io.FileWriter;

import com.avlsi.circuit.LumpedRCGraph;
import com.avlsi.circuit.ResistiveSubnet;
import com.avlsi.util.text.NumberFormatter;
import com.avlsi.util.debug.Debug;

/*********************************************************************
 * Utility class for tallying statistics about various circuit
 * and resistive subnet properties.  Outputs a report to file.
 *
 * @author Mike Davies
 * @version $Revision$ $Date$
 *********************************************************************/
final class NetStatistics {

    private final String fileName;
    private final LumpedRCGraph circuit;

    private double totalMaxRes;
    private double totalWireCap;
    private double totalGateCap;
    private double totalVdnRes;
    private double totalMaxRC;
    private double totalWireGateRatio;
    private float  maxVdnRes;
    private int    numNets;
    private int    numLoadedNets;
    private int    numWithCycles;
    private int    numDegenerate;

    NetStatistics(final LumpedRCGraph circuit, final String fileName) {
        this.circuit = circuit;
        this.fileName = fileName;
    }
    
    void addNet(ResistiveSubnet rsn) {
        numNets++;
        totalMaxRes += rsn.getMaxPathRes();
        totalVdnRes += (rsn.getVdnRes() < 0.0f) ? 0.0 : rsn.getVdnRes();
        if (rsn.getVdnRes() > maxVdnRes) maxVdnRes = rsn.getVdnRes();
        totalWireCap += rsn.getWireCap();
        totalGateCap += rsn.getGateCap();
        if (rsn.getGateCap() > 0.0) {
            totalWireGateRatio += rsn.getWireCap()/rsn.getGateCap();
            numLoadedNets++;
        }
        totalMaxRC += rsn.getMaxRC();
        if (rsn.hasCycles()) numWithCycles++;
        if (rsn.isDegenerate()) numDegenerate++;
    }

    private String reportName() {
        int i0 = fileName.lastIndexOf('/');
        Debug.assertTrue(i0 != fileName.length()-1);
        int i1 = fileName.lastIndexOf('.');
        if (i1 == -1) return fileName.substring(i0+1) + ".report";
        else return fileName.substring(i0+1,i1) + ".report";
    }

    void generateReport() {
        try {
            String aveMaxRes  = NumberFormatter.format(totalMaxRes/numNets,4);
            String aveWireCap = NumberFormatter.format(totalWireCap/numNets,4);
            String aveGateCap = NumberFormatter.format(totalGateCap/numNets,4);
            String aveVdnRes  = NumberFormatter.format(totalVdnRes/numNets,4);
            String maxVdnRes  = NumberFormatter.format(this.maxVdnRes,4);
            String aveMaxRC   = NumberFormatter.format(totalMaxRC/numNets,4);
            String aveWireGate= 
                NumberFormatter.format(totalWireGateRatio/numLoadedNets,4);
            String percCycles = 
                NumberFormatter.format((double)numWithCycles/numNets*100,4);
            String percDegen  = 
                NumberFormatter.format((double)numDegenerate/numNets*100,4);

            String fileName = circuit.getType()+".report";

            PrintWriter outWriter = new PrintWriter(
                                        new BufferedWriter(
                                            new FileWriter(reportName())));
            outWriter.println("REPORT FOR CELL "+circuit.getType()+":");
            outWriter.println("----------------------------------------------");
            outWriter.println("Number of transistors:        "+
                              circuit.getTransistorCount());
            outWriter.println("Number of capacitors:         "+
                              circuit.getCapacitorCount());
            outWriter.println("Number of resistors:          "+
                              circuit.getResistorCount());
            outWriter.println("Number of circuit nodes:      "+
                              circuit.getNodeCount());
            outWriter.println("Number of resistive subnets:  "+numNets);
            outWriter.println("----------------------------------------------");
            outWriter.println("Resistive subnet statistics:");
            outWriter.println(" Average max resistance:      "+aveMaxRes);
            outWriter.println(" Average wire capacitance:    "+aveWireCap);
            outWriter.println(" Average gate capacitance:    "+aveGateCap);
            outWriter.println(" Average wire/gate cap ratio: "+aveWireGate);
            outWriter.println(" Average VDN resistance:      "+aveVdnRes);
            outWriter.println(" Max VDN resistance:          "+maxVdnRes);
            outWriter.println(" Percentage with cycles:      "+percCycles+"%");
            outWriter.println(" Percentage degenerate:       "+percDegen+"%");
            outWriter.close();
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }
}
