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

package com.avlsi.tools.aspice;

import com.avlsi.file.common.DeviceTypes;
import com.avlsi.file.common.HierName;

/**
 * Class to represent diodes in .aspice files.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class Diode extends AbstractDevice {
    /** type of diode, N_TYPE or P_TYPE. */
    private final int type;

    /** Width of diode in meters. **/
    private final double width;

    /** Length of diode in meters. **/
    private final double length;

    /** Area of diode in square meters. **/
    private final double area;

    /** Perimeter of diode in meters. **/
    private final double perimeter;

    /** BSIM3 Device model data. **/
    private final BSim3Model m;

    /** BSim3 convergence correction hack-- 0 turns it off. **/
    private final double gmin = 0;

    /** Device currents **/
    
    /** Drain node **/
    private double drainI;
    /** Source node **/
    private double sourceI;

    public native void nativeEval(int modelHandle, int type,
                                  double Vsource, double Vdrain,
                                  double area, double perimeter,
                                  double width, double gmin,
                                  double[] charge,
                                  double[] current,
                                  double[] charge_partials,
                                  double[] current_partials);

    /**
     * Class constructor.
     * @param name Name of the device
     * @param source name of source node
     * @param drain name of drain node
     * @param width of diode in meters
     * @param length of diode in meters
     * @param area of diode in square meters
     * @param perimeter of diode in meters
     * @throws IllegalArgumentException
     **/
    public Diode(final HierName name,
                 final int type,
                 final Node source,
                 final Node drain,
                 final double width,
                 final double length,
                 final double area,
                 final double perimeter) {
        super(new Node[] { source, drain });

        if (type != DeviceTypes.N_TYPE && type != DeviceTypes.P_TYPE)
            throw new IllegalArgumentException("Bad diode type: " + type);

        this.type = type;

        this.width = width;
        this.length = length;
        this.area = area;
        this.perimeter = perimeter;
        this.name = name;

        //
        //Loads native or java model depending on native parameter
        //
        m = BSim3Model.findModel(type, width, length);
    }

    /** @throws IllegalArgumentException **/
    public Diode(final int type,
                 final Node source,
                 final Node drain,
                 final double width,
                 final double length,
                 final double area,
                 final double perimeter) {
        this(null, type, source,drain, width,length, area,perimeter);
    }
    
    /**
     * Get the type of the diode, either N_TYPE, or P_TYPE.
     * @return the type of the diode
     **/
    public int getType() {
        return type;
    }

    /**
     * Get source node.
     * @return source node
     **/
    public Node getSource() {
        return nodes[0];
    }

    /**
     * Get drain node.
     * @return drain node
     **/
    public Node getDrain() {
        return nodes[1];
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

    /** returns the device current **/
    public double getCurrent(int type) {
        switch (type) {
          case AbstractDevice.I1:      return drainI;
          case AbstractDevice.I2:      return sourceI;
        }
        return -1;
    }
    
    public String getCode() { return "D";}

    /**
     * Evaluates current, charge, and their derivatives for all ports
     * on a device and then informs its nodes of these values.
     * @param chargeScale scalar for charge calculation
     * @param currentScale scalar for current calculation
     * @param derivChargeScale scalar for charge derivative calculation
     * @param derivCurrentScale scalar for current derivative calculation
     **/
    public void evalVoltage(double chargeScale, double currentScale,
                            double derivChargeScale, double derivCurrentScale,
                            double time) {
        if (BSim3Model.isNativeLoaded()) {
            evalNativeVoltage(chargeScale,currentScale,derivChargeScale,
                              derivCurrentScale,time);
        }
        if (!BSim3Model.isNativeLoaded() || BSim3Model.both) {
            evalJavaVoltage(chargeScale,currentScale,derivChargeScale,
                              derivCurrentScale,time);
        }
    }

    //
    //doubles[] pooled so these don't have to be created every evaluation
    //
    double[] Q = new double[2];
    double[] I = new double[2];
    double[] dQ= new double[4];
    double[] dI= new double[4];
    public void evalNativeVoltage(double chargeScale, double currentScale,
                            double derivChargeScale, double derivCurrentScale,
                            double time) {

        
        Node source = getSource();
        Node drain = getDrain();
        
        nativeEval(m.getHandle(), m.type,
                source.getVoltage(), drain.getVoltage(),
                area, perimeter, width, 0, Q,I,dQ,dI);

        if (BSim3Model.both) return;  //just observe
        drainI = I[1];//-m.type*cbs;
        sourceI = I[0];//m.type*cbs;
        
        source.setResult(chargeScale, Q[0],//-m.type*qbs,
                         currentScale, sourceI /*m.type*cbs*/);
        drain.setResult (chargeScale,  Q[1],//m.type*qbs,
                         currentScale, drainI /*-m.type*cbs*/);
        source.setMatrix(source,
                         derivChargeScale, dQ[0], //capbs,
                         derivCurrentScale, dI[0]);//-gbs);
        source.setMatrix(drain,
                         derivChargeScale, dQ[2], //-capbs,
                         derivCurrentScale, dI[2]);//gbs);
        drain.setMatrix(source,
                        derivChargeScale, dQ[1],//-capbs,
                        derivCurrentScale, dI[1]);//gbs);
        drain.setMatrix(drain,
                        derivChargeScale, dQ[3],//capbs,
                        derivCurrentScale, dI[3]);//-gbs);
    }   

    public void evalJavaVoltage(double chargeScale, double currentScale,
                            double derivChargeScale, double derivCurrentScale,
                            double time)
    {
        Node source = getSource();
        Node drain = getDrain();

        double Vs = source.getVoltage();
        double Vb = drain.getVoltage();

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

        sourceI = m.type*cbs;
        drainI = -m.type*cbs;
        
        source.setResult(chargeScale, -m.type*qbs,
                         currentScale, sourceI/*m.type*cbs*/);
        drain.setResult (chargeScale,  m.type*qbs,
                         currentScale, drainI /*-m.type*cbs*/);

        source.setMatrix(source,
                         derivChargeScale, capbs, derivCurrentScale, -gbs);
        source.setMatrix(drain,
                         derivChargeScale, -capbs, derivCurrentScale, gbs);
        drain.setMatrix(source,
                        derivChargeScale, -capbs, derivCurrentScale, gbs);
        drain.setMatrix(drain,
                        derivChargeScale, capbs, derivCurrentScale, -gbs);
        if (BSim3Model.both && BSim3Model.isNativeLoaded()) {
            /*
            System.out.println("JQ= "+(-m.type*qbs)+","+(m.type*qbs)+
                               " NQ= "+Q[0]+","+Q[1]);
            System.out.println("JI= "+(sourceI)+","+(drainI)+
                               " NI= "+I[0]+","+I[1]);
            System.out.println("J0= "+(capbs)+","+(-gbs)+
                               " N0= "+dQ[0]+","+dI[0]);
            System.out.println("J2= "+(-capbs)+","+(gbs)+
                               " N2= "+dQ[2]+","+dI[2]);
            System.out.println("J1= "+(-capbs)+","+(gbs)+
                               " N1= "+dQ[1]+","+dI[1]);
            System.out.println("J3= "+(capbs)+","+(-gbs)+
                               " N3= "+dQ[3]+","+dI[3]);
            */
            System.out.println(time+"\t"+(-m.type*qbs)+"\t"+(m.type*qbs)+"\t"+
                               sourceI+"\t"+drainI+"\t"+
                               capbs+"\t"+gbs+"\t"+
                               //Q[0] = 8th
                               Q[0]+"\t"+Q[1]+"\t"+I[0]+"\t"+I[1]+"\t"+
                               dQ[0]+"\t"+dI[0]+"\t"+
                               dQ[1]+"\t"+dI[1]+"\t"+
                               dQ[2]+"\t"+dI[2]+"\t"+
                               dQ[3]+"\t"+dI[3]+"\t"+
                               getName());
        }

    }
}


