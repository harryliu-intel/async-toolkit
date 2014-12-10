/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.jauto;

import com.avlsi.fast.*;
import com.avlsi.file.common.HierName;

import java.util.Set;
import java.io.*;

public class NetSource
{
    public final int            type;    // type of the source
                                         // 0: half-operator, by default
                                         // 1: cell-type, only used during global net generation
                                         // 2: primary input

    /**
     * X-coordinate of the net.  Only meaningful for type
     * <code>NetType.CELL</code>.
     **/
    public double               coordinateX;

    /**
     * Y-coordinate of the net.  Only meaningful for type
     * <code>NetType.CELL</code>.
     **/
    public double               coordinateY;

    /**
     * Orientation of the net.  Only meaningful for type
     * <code>NetType.CELL</code>.
     **/
    public int                  orientation;

    /**
     * TODO: Document; purpose unknown.  Only meaningful for type
     * <code>NetType.CELL</code>.
     **/
    public CellType             cellSource;

    /**
     * TODO: Document; purpose unknown.  Only meaningful for type
     * <code>NetType.CELL</code>.
     **/
    public Set/*<CellNet>*/     setSubcellNets;

    /**
     * The half-operator driving this net.  Only meaningful for type
     * <code>NetType.HALF_OPERATOR_TRANSISTOR</code>.
     **/
    public HalfOperator         source;

    private HierName instanceName;

    private float delayBias;

    public NetSource(int type) {
        assert type >= 0 && type <= 2 : "invalid type for NetSource";
        this.type = type;
        delayBias = Float.POSITIVE_INFINITY;
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


    public CellType setCellSource(CellType ct)
    {
        assert ct != null : "trying to set NULL pointer to cellSource";

        cellSource = ct;
    
        return cellSource;
    }

    
    public CellType getCellSource()
    {
        return cellSource;
    }


    public HalfOperator setSource(HalfOperator ho)
    {
        assert ho != null : "trying to set Null pointer to source";

        source = ho;
    
        return source;
    }


    public HalfOperator getSource()
    {
        return source;
    }


    public NetSource()
    {
        type = NetType.HALF_OPERATOR_TRANSISTOR;

        coordinateX = 0.0;
        coordinateY = 0.0;

        orientation = ConnectionInfo.R0;

        cellSource = null;
        setSubcellNets = null;

        source = null;
        instanceName = null;
        delayBias = Float.POSITIVE_INFINITY;
    }

    public NetSource(NetSource ns)
    {
        type = ns.type;
        coordinateX = ns.coordinateX;
        coordinateY = ns.coordinateY;

        orientation = ns.orientation;

        cellSource = ns.cellSource;
        setSubcellNets = ns.setSubcellNets;

        source = ns.source;
        instanceName = ns.instanceName;
        delayBias = ns.delayBias;
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
            (source == null ?
             "" : " source=" + 
             source.subType.typeName + "/" + 
             source.outputNet.canonicalName +
             (source.driveDirection == HalfOperator.DriveDirection.PULL_DOWN ?
              "-" : "+")) +
            " X=" + coordinateX + " Y=" + coordinateY;
    }


    public void dumpInfo(BufferedWriter bw1, String s1)
    {
        double sx = source.subType.xSize;
        double sy = source.subType.ySize;

        try{
            bw1.write(s1 + "NET_SOURCE _ {\n");

            if(type == NetType.HALF_OPERATOR_TRANSISTOR){
                bw1.write(s1 + "\toutput_net = " 
                    + source.outputNet.canonicalName.getCadenceString() + ";\n");
                bw1.write(s1 + "\tdrive_direction = ");
                if (source.driveDirection ==
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

    public float getDelayBias() {
        return delayBias;
    }

    public void setDelayBias(final float x) {
        delayBias = Math.min(x, delayBias);
    }
}
