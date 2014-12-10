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

package com.avlsi.file.aspice;

import com.avlsi.file.common.DeviceTypes;
import com.avlsi.file.common.HierName;
import com.avlsi.util.text.NumberFormatter;
import com.avlsi.circuit.DiodeInterface;

/**
 * Class to represent diodes in .aspice files.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class Diode implements DeviceInterface,DiodeInterface {
    /** type of diode, N_TYPE or P_TYPE. */
    private final int type;

    /** Name of source node. **/
    private final HierName source;

    /** Name of drain node. **/
    private final HierName drain;

    /** Width of diode in meters. **/
    private final double width;

    /** Length of diode in meters. **/
    private final double length;

    /** Area of diode in square meters. **/
    private final double area;

    /** Perimeter of diode in meters. **/
    private final double perimeter;

    /** Number of Ports **/
    private static final int NUM_PORTS = 2;

    /** BSIM3 Device model data **/
    private final BSim3Model m;

    /** BSim3 convergence correction hack-- 0 turns it off. **/
    private static final double gmin = 0;

    /**
     * Class constructor.
     * @param source name of source node
     * @param drain name of drain node
     * @param width of diode in meters
     * @param length of diode in meters
     * @param area of diode in square meters
     * @param perimeter of diode in meters
     * @throws IllegalArgumentException
     **/
    public Diode(final int type,
                 final HierName source,
                 final HierName drain,
                 final double width,
                 final double length,
                 final double area,
                 final double perimeter) {
        if (type != DeviceTypes.N_TYPE && type != DeviceTypes.P_TYPE)
            throw new IllegalArgumentException("Bad diode type: " + type);

        this.type = type;

        this.source = source;
        this.drain = drain;

        this.width = width;
        this.length = length;
        this.area = area;
        this.perimeter = perimeter;

        m = BSim3Model.findModel(type, width, length);
    }
    
    /** Not implemented, returns null **/
    public HierName getName() { return null;}

    /**
     * Get number of ports
     * @return number of ports
     **/
    public int getNumPorts() {
        return NUM_PORTS;
    }   
    
    /**
     * Get the type of the diode, either N_TYPE, or P_TYPE.
     * @return the type of the diode
     **/
    public int getType() {
        return type;
    }

    /**
     * Get name of source node.
     * @return name of source node
     **/
    public HierName getSource() {
        return source;
    }

    /**
     * Get name of drain node.
     * @return name of drain node
     **/
    public HierName getDrain() {
        return drain;
    }

    /**
     * Get width of diode
     * @return width of diode in meters
     **/
    public double getWidth() {
        return width;
    }

    /**
     * Get length of diode
     * @return length of diode in meters
     **/
    public double getLength() {
        return length;
    }

    /**
     * Get area of diode
     * @return area of diode in square meters
     **/
    public double getArea() {
        return area;
    }

    /**
     * Get perimeter
     * @return get diode perimeter in meters
     **/
    public double getPerimeter() {
        return perimeter;
    }

    /**
     * return string suitable for inclusion in an aspice file.
     **/
    public String getAspiceString() {
        return (type == DeviceTypes.N_TYPE ? "n_diode (" : "p_diode (")
            + getSource().getAspiceString() + ","
            + getDrain().getAspiceString() + ") ("
            + NumberFormatter.format(getWidth()) + ","
            + NumberFormatter.format(getLength()) + ","
            + NumberFormatter.format(getArea()) + ","
            + NumberFormatter.format(getPerimeter()) + ");";

    }

    public String toString() {
        return getAspiceString();
    }

    /**
     * Evaluates current, charge, and their derivatives for all ports
     * on a device for a given set of input voltages.
     * @param data array of port data objects to use for return values
     * @param voltage array of voltages connected to device
     * @throws IllegalArgumentException
     * @return array of current/charge/etc. data for each port,
     *         indexed by input voltage
     **/
    public PortData[] evalVoltage(PortData[] data, double[] voltage)
        throws NoModelException {
        if (data.length != NUM_PORTS || voltage.length != NUM_PORTS)
            throw new IllegalArgumentException("Expected " + NUM_PORTS +
                                               " ports");

        if (m == null)
            throw new NoModelException();

        double Vs = voltage[0];
        double Vb = voltage[1];

	double vbs,gbs,cbs,qbs,capbs;
	double Nvtm, Inv_Nvtm;
	double SatCurrent,evbs,vjsm,IsEvjsm,czbs,czbssw,czbsswg;
	double MJ,MJSW,MJSWG;
	double T0,T1,arg,sarg;

	/* precompute some diode expressions */
	vbs = m.type * (Vb - Vs);
	Nvtm = m.vtm * m.jctEmissionCoeff;
	Inv_Nvtm = 1.0 / Nvtm;
	MJ = m.bulkJctBotGradingCoeff;
	MJSW = m.bulkJctSideGradingCoeff;
	MJSWG = m.bulkJctGateSideGradingCoeff;

	/* I and G */
	if ((area <= 0.0) && (perimeter <= 0.0))
	  SatCurrent = 1.0e-14;
	else
	  SatCurrent = area * m.jctTempSatCurDensity 
	      + perimeter * m.jctSidewallTempSatCurDensity;
	if (SatCurrent <= 0.0)
	  {
	  gbs = gmin;
	  cbs = gbs * vbs;
	  }
	else
	  {
	  if (m.ijth == 0.0)
	    {
	    evbs = Math.exp(vbs * Inv_Nvtm);
	    gbs = SatCurrent * evbs * Inv_Nvtm + gmin;
	    cbs = SatCurrent * (evbs - 1.0) + gmin * vbs;
	    }
	  else
	    {
	    vjsm = Nvtm * Math.log(m.ijth / SatCurrent + 1.0);
	    IsEvjsm = SatCurrent * Math.exp(vjsm * Inv_Nvtm);
	    if (vbs < vjsm)
	      {
	      evbs = Math.exp(vbs * Inv_Nvtm);
	      gbs = SatCurrent * evbs * Inv_Nvtm + gmin;
	      cbs = SatCurrent * (evbs - 1.0) + gmin * vbs;
	      }
	    else
	      {
	      T0 = IsEvjsm * Inv_Nvtm;
	      gbs = T0 + gmin;
	      cbs = IsEvjsm - SatCurrent + T0 * (vbs - vjsm) + gmin * vbs;
	      }
	    }
	  }

	/* Q and C */  
	czbs = m.unitAreaJctCap * area;
	if (perimeter < width)
	  {
	  czbssw = 0;
	  czbsswg = m.unitLengthGateSidewallJctCap * perimeter;
	  }
	else
	  {
	  czbssw = m.unitLengthSidewallJctCap * (perimeter - width);
	  czbsswg = m.unitLengthGateSidewallJctCap * width;
	  }

	if (vbs == 0.0)
	  {
	  qbs = 0.0;
	  capbs = czbs + czbssw + czbsswg;
	  }
	else if (vbs < 0.0)
	  {
	  if (czbs > 0.0)
	    {
	    arg = 1.0 - vbs / m.PhiB;
	    if (MJ == 0.5)
	      sarg = 1.0 / Math.sqrt(arg);
	    else
	      sarg = Math.exp(-MJ * Math.log(arg));
	    qbs = m.PhiB * czbs * (1.0 - arg * sarg) / (1.0 - MJ);
	    capbs = czbs * sarg;
	    }
	  else
	    {
	    qbs = 0.0;
	    capbs = 0.0;
	    }
	  if (czbssw > 0.0)
	    {
	    arg = 1.0 - vbs / m.PhiBSW;
	    if (MJSW == 0.5)
	      sarg = 1.0 / Math.sqrt(arg);
	    else
	      sarg = Math.exp(-MJSW * Math.log(arg));
	    qbs += m.PhiBSW * czbssw * (1.0 - arg * sarg) / (1.0 - MJSW);
	    capbs += czbssw * sarg;
	    }
	  if (czbsswg > 0.0)
	    {
	    arg = 1.0 - vbs / m.PhiBSWG;
	    if (MJSWG == 0.5)
	      sarg = 1.0 / Math.sqrt(arg);
	    else
	      sarg = Math.exp(-MJSWG * Math.log(arg));
	    qbs += m.PhiBSWG * czbsswg * (1.0 - arg * sarg) / (1.0 - MJSWG);
	    capbs += czbsswg * sarg;
	    }
	  }
	else
	  {
	  T0 = czbs + czbssw + czbsswg;
	  T1 = vbs * (czbs * MJ / m.PhiB + czbssw * MJSW
		      / m.PhiBSW + czbsswg * MJSWG / m.PhiBSWG);
	  qbs = vbs * (T0 + 0.5 * T1);
	  capbs = T0 + T1;
	  }

	data[0].setVoltage(voltage[0]);
	data[0].setCurrent(m.type * cbs);
	data[0].setCurrentDerivative(0,  capbs);
        data[0].setCurrentDerivative(1, -capbs);
        data[0].setCharge(-m.type * qbs);
        data[0].setChargeDerivative(0, -gbs);
        data[0].setChargeDerivative(1,  gbs);
        
        data[1].setVoltage(voltage[1]);
        data[1].setCurrent(-m.type * cbs);
        data[1].setCurrentDerivative(0, -capbs);
        data[1].setCurrentDerivative(1,  capbs);
        data[1].setCharge(m.type * qbs);
        data[1].setChargeDerivative(0,  gbs);
        data[1].setChargeDerivative(1, -gbs);
        
        return data;
    }

}
