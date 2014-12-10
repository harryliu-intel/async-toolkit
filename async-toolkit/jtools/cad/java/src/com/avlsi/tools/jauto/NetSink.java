/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.jauto;


import com.avlsi.fast.*;
import com.avlsi.file.common.DeviceTypes;
import com.avlsi.file.common.HierName;
import com.avlsi.tools.lvs.NetGraph;

import java.util.Set;
import java.io.*;

public class NetSink
{
    public final int            type;    // type of the sink
                                         // 0: half-operator and transistor, by default
                                         // 1: cell-type, only used during globle net generation
                                         // 2: capacitive load, for primary output

    public double               coordinateX;
    public double               coordinateY;

    public int                  orientation;

    public double               loadCapacitance; // load capacitance value for type 2
 
    public CellType             cellSink; // load cell for type 1
    public Set/*<CellNet>*/     setSubcellNets; // set of cell nets in the cellSink for type 1

    public HalfOperator         sink; // load halfoperator for type 0
    public NetGraph.NetEdge     transistor; // load transistor for type 0
    private HierName instanceName;


    public NetSink(final int type) {
        assert type >= 0 && type <= 2 : "invalid type for NetSink";
        this.type = type;
        instanceName = null;
    }


    public int getType()
    {
        return type;
    }


    public double setCoordinateX(double d)
    {
        coordinateX = d;

        return coordinateX;
    }


    public double getCoordinateX()
    {
        return coordinateX;
    }


    public double setCoordinateY(double d)
    {
        coordinateY = d;

        return coordinateY;
    }


    public double getCoordinateY()
    {
        return coordinateY;
    }


    public double setLoadCapacitance(double d)
    {
        assert d >= 0.0 : "invalid capacitance value";

        loadCapacitance = d;

        return loadCapacitance;
    }


    public double getLoadCapacitance()
    {
        return loadCapacitance;
    }


    public CellType setCellSink(CellType ct)
    {
        assert ct != null : "trying to set NULL pointer to cellSink";

        cellSink = ct;
    
        return cellSink;
    }

    
    public CellType getCellSink()
    {
        return cellSink;
    }


    public HalfOperator setSink(HalfOperator ho)
    {
        assert ho != null : "trying to set Null pointer to sink";

        sink = ho;
    
        return sink;
    }


    public HalfOperator getSink()
    {
        return sink;
    }


    public NetGraph.NetEdge setTransistor(NetGraph.NetEdge tr)
    {
        assert tr != null : "trying to set Null pointer to transistor";

        transistor = tr;
    
        return transistor;
    }


    public NetGraph.NetEdge getTransistor()
    {
        return transistor;
    }


    public NetSink()
    {
        type = NetType.HALF_OPERATOR_TRANSISTOR;

        coordinateX = 0.0;
        coordinateY = 0.0;

        orientation = ConnectionInfo.R0;

        // FIXME: hardcoded constant
        loadCapacitance = 20.0E-15;

        cellSink = null;
        setSubcellNets = null;

        sink = null;
        transistor = null;
        instanceName = null;
    }


    public NetSink(NetSink ns)
    {
        type = ns.type;

        coordinateX = ns.coordinateX;
        coordinateY = ns.coordinateY;

        orientation = ns.orientation;

        loadCapacitance = ns.loadCapacitance;

        cellSink = ns.cellSink;
        setSubcellNets = ns.setSubcellNets;
        
        sink = ns.sink;
        transistor = ns.transistor;
        instanceName = ns.instanceName;
    }


    public void print()
    {
        System.out.println("type: " + type);
        System.out.println("coordinate X: " + coordinateX);
        System.out.println("coordinate Y: " + coordinateY);
    }


    public String toString()
    {
        return "type=" + type + " inst=" + instanceName +
            (sink == null ?
             "" : " sink=" +
             sink.subType.typeName + "/" +
             sink.outputNet.canonicalName +
             (sink.driveDirection ==  HalfOperator.DriveDirection.PULL_DOWN ?
              "-" : "+")) +
            " X=" + coordinateX + " Y=" + coordinateY;
    }


    public void dumpInfo(BufferedWriter bw1, String s1)
    {
        double sx = sink.subType.xSize;
        double sy = sink.subType.ySize;

        try{
            bw1.write(s1 + "NET_SINK _ {\n");

            if(type == NetType.HALF_OPERATOR_TRANSISTOR){
                bw1.write(s1 + "\toutput_net = " 
                    + sink.outputNet.canonicalName.getCadenceString() + ";\n");
                bw1.write(s1 + "\tdrive_direction = ");
                if (sink.driveDirection ==
                        HalfOperator.DriveDirection.PULL_DOWN) {
                    bw1.write("-;\n");
                }
                else{
                    bw1.write("+;\n");
                }

                bw1.write(s1 + "\torientation = ");
                double fx, fy;
                switch(orientation){
                    case ConnectionInfo.R0:
                                                bw1.write("R0;\n");
                                                fx = sx / 2.0;
                                                fy = sy / 2.0;
                                                break;

                    case ConnectionInfo.R90:
                                                bw1.write("R90;\n");
                                                fx = sy / 2.0;
                                                fy = sx / 2.0;
                                                break;

                    case ConnectionInfo.R180:
                                                bw1.write("R180;\n");
                                                fx = sx / 2.0;
                                                fy = sy / 2.0;
                                                break;

                    case ConnectionInfo.R270:
                                                bw1.write("R270;\n");
                                                fx = sy / 2.0;
                                                fy = sx / 2.0;
                                                break;

                    case ConnectionInfo.MY:
                                                bw1.write("MY;\n");
                                                fx = sx / 2.0;
                                                fy = sy / 2.0;
                                                break;

                    case ConnectionInfo.MYR90:
                                                bw1.write("MYR90;\n");
                                                fx = sy / 2.0;
                                                fy = sx / 2.0;
                                                break;

                    case ConnectionInfo.MX:
                                                bw1.write("MX;\n");
                                                fx = sx / 2.0;
                                                fy = sy / 2.0;
                                                break;

                    case ConnectionInfo.MXR90:
                                                bw1.write("MXR90;\n");
                                                fx = sy / 2.0;
                                                fy = sx / 2.0;
                                                break;

                    default:
                                                throw new AssertionError();
                }

                bw1.write(s1 + "\tcoordinate = (" + coordinateX + ", " + coordinateY + ");\n");

                bw1.write(s1 + "\tbounding_box = (" 
                    + (coordinateX - fx) + ", "
                    + (coordinateY - fy) + ", "
                    + (coordinateX + fx) + ", "
                    + (coordinateY + fy) + ");\n");

            }

            bw1.write(s1 + "}\n");
        }
        catch(IOException e){
            e.printStackTrace(System.out);
        }
    }


    public final int updateOrientation(int parentOrientation)
    {
        orientation = ConnectionInfo.composeOrientation(parentOrientation, orientation);
        return orientation;
    }

    public HierName getInstanceName() {
        assert type == NetType.CELL || type == NetType.HALF_OPERATOR_TRANSISTOR;
        return instanceName;
    }

    public void prefixInstanceName(final /*@ non_null @*/ HierName prefix) {
        assert type == NetType.CELL || type == NetType.HALF_OPERATOR_TRANSISTOR;
        instanceName =
            instanceName == null ? prefix
                                 : HierName.append(prefix, instanceName);
    }

    public AdditiveTerms getGateCapacitanceTerm() {
        assert type == NetType.HALF_OPERATOR_TRANSISTOR;

        final TechnologyData tech = sink.subType.design.getTechnologyData();
        final double unitGateCapacitance =
            transistor.type == DeviceTypes.N_TYPE ?
                tech.getUnitNmosGateCapacitance(transistor.getTransistorType())
              : tech.getUnitPmosGateCapacitance(transistor.getTransistorType());
        final double f = transistor.size /
                         transistor.shareCount *
                         transistor.length *
                         unitGateCapacitance;

        final AdditiveTerms result;
        if (sink.isVariableFixed()) {
            result =
                new AdditiveTerms(new FunctionTerm(sink.getCurrentSize() * f));
        } else {
            result =
                new AdditiveTerms(
                        new FunctionTerm(
                            FunctionTerm.Type.VAR,
                            sink.getVariableName(),
                            f * sink.getSizeForConductance(1.0) *
                            tech.getWidthRoundingSlope()),
                        new FunctionTerm(f * tech.getWidthRoundingOffset() *
                                         sink.getSymmetrizationFactor()));
        }

        return result;
    }
}
