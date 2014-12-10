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
import com.avlsi.util.debug.Debug;

/**
 * Class to represent a transistor in a circuit
 *
 * @author JTed Vessenes
 * @version $Name:  $ $Date$
 **/
public final class Transistor extends AbstractDevice 
                              implements java.io.Serializable{

    /** type of transistor (N_TYPE or P_TYPE). **/
    private final int type;

    /** Width of transistor in meters. **/
    private final double width;

    /** Length of transistor in meters. **/
    private final double length;

    /**
      * BSIM3 Device model data.
      * This value will be null if no model was found.
      * If so, some class methods such as evaluate() will
      * generate exceptions if called.
      **/
    private final BSim3Model m;

    /** Device currents **/
    
    /** Drain node current **/
    private double drainI;
    /** Gate node current **/
    private double gateI;
    /** Source node current **/
    private double sourceI;
    /** Bulk node current **/
    private double bulkI;
    
//    public static native void loadModel(String filename, double temperature);
    
//    public native int findModel(int type, double width, double height);
    
    public native void nativeEval(int modelHandle, int type,
                                  double Vsource, double Vdrain,
                                  double Vgate, double Vbulk,
                                  double width, double length,
                                  double gmin,
                                  double[] charge,
                                  double[] current,
                                  double[] charge_partials,
                                  double[] current_partials);
   /**
     * Class constructor.  Canonicalizes the source and drain node names
     * by ensuring that source is lexicographically less than drain.
     * @param name HierName name of the device
     * @param type type of transistor, N_TYPE or P_TYPE.
     * @param source name of source node
     * @param drain name of drain node
     * @param gate name of gate node
     * @param bulk name of bulk node
     * @param width of gate in meters
     * @param length of gate in meters
     * @throws IllegalArgumentException
     **/

    public Transistor(final HierName name,
                      final Node source,
                      final Node drain,
                      final Node gate,
                      final Node bulk,
                      final int type,
                      final double width,
                      final double length) {
        super(new Node[] { source, drain, gate, bulk });

        if (type != DeviceTypes.N_TYPE && type != DeviceTypes.P_TYPE)
            throw new IllegalArgumentException("Bad fet type: " + type);

        this.type = type;
        this.width = width;
        this.length = length;
        this.name = name;

        m = BSim3Model.findModel(type, width, length);
    }

    /** @throws IllegalArgumentException **/
    public Transistor(final Node source,
                      final Node drain,
                      final Node gate,
                      final Node bulk,
                      final int type,
                      final double width,
                      final double length) {
        this(null, source, drain, gate, bulk, type, width, length);
    }
        
    /**
     * Get the type of the transistor, either N_TYPE, or P_TYPE.
     * @return the type of the transistor
     **/
    public int getType() {
        return type;
    }

    /**
     * Get source node.
     * @return name of source node
     **/
    public Node getSource() {
        return nodes[0];
    }

    /**
     * Get drain node.
     * @return name of drain node
     **/
    public Node getDrain() {
        return nodes[1];
    }

    /**
     * Get gate node.
     * @return name of gate node
     **/
    public Node getGate() {
        return nodes[2];
    }

    /**
     * Get bulk node.
     * @return name of bulk node
     **/
    public Node getBulk() {
        return nodes[3];
    }

    /**
     * Get width of transistor
     * @return width of transistor in meters
     **/
    public double getWidth() {
        return width;
    }

    /**
     * Get length of transistor
     * @return length of transistor in meters
     **/
    public double getLength() {
        return length;
    }

    /** returns the device current **/
    public double getCurrent(int type) {
        switch (type) {
          case AbstractDevice.I1:      return drainI;
          case AbstractDevice.I2:      return gateI;
          case AbstractDevice.I3:      return sourceI;
          case AbstractDevice.I4:      return bulkI;
        }
        return -1;
    }

    public String getCode() { return "M";}

                                  
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
    //Data values are pooled, so these doubles[] don't need to be created
    //for every run, global for 'both' mode
    //
    private double[] Q = new double[4];
    private double[] I = new double[4];
    private double[] dQ= new double[16];
    private double[] dI= new double[16];
    private int nD=1,nS=0;

    /** Evaluates the voltage natively, using the BSIM3 model written
     * for aspice.
     **/
    public void evalNativeVoltage(double chargeScale, double currentScale,
                            double derivChargeScale, double derivCurrentScale,
                            double time) {

        Node source = getSource();
        Node drain = getDrain();
        Node gate = getGate();
        Node bulk = getBulk();
        double Vs = source.getVoltage();
        double Vd = drain.getVoltage();
        int ntype = m.type;
        nS = 0;
        nD = 1;
        //Native will switch source and drain to make Vds always positive
//        if (ntype*Vd < ntype*Vs) {
//            nD = 0;
//            nS= 1;
//        }
        
        nativeEval(m.getHandle(), ntype,
                Vs, Vd,
                gate.getVoltage(), bulk.getVoltage(),
                width, length, 0, Q, I, dQ, dI);
        
        if (BSim3Model.both) return; //For both mode, just observe
        sourceI = I[nS];//m.type*Ids;
        drainI = I[nD];//-m.type*(Ids+Isub);
        gateI = I[2];//0;
        bulkI = I[3]; //m.type*Isub;
        source.setResult(chargeScale, Q[0]/*m.type*qsrc*/,
                         currentScale,sourceI);
        drain.setResult(chargeScale, Q[1]/*m.type*qdrn*/,
                        currentScale, drainI/*-m.type*(Ids+Isub*/);
        gate.setResult(chargeScale, Q[2]/*m.type*qgate*/,
                       currentScale, gateI/*0*/);
        bulk.setResult(chargeScale, Q[3]/*m.type*qbulk*/,
                       currentScale,bulkI/*m.type*Isub*/);

        source.setMatrix(source, derivChargeScale, dQ[0], //gcssb,
                                 derivCurrentScale, dI[4*nS+nS] //-Gds - Gm - Gmb
                                 );
        source.setMatrix(drain,  derivChargeScale, dQ[4],// gcdsb,
                                 derivCurrentScale, dI[4*nD+nS]//Gds+Gm+Gmb+Gbd+Gbg+Gbb
                                 );
        source.setMatrix(gate,   derivChargeScale, dQ[8], //gcgsb,
                                 derivCurrentScale, dI[8] //0
                                 );
        source.setMatrix(bulk,   derivChargeScale, dQ[12], //gcbsb,
                                 derivCurrentScale, dI[12+nS] //-Gbd - Gbg - Gbb
                                 );
        drain.setMatrix(source,  derivChargeScale, dQ[1], //gcsdb,
                                 derivCurrentScale, dI[4*nS+nD] //Gds
                                 );
        drain.setMatrix(drain,   derivChargeScale, dQ[5], //gcddb,
                                 derivCurrentScale, dI[4*nD+nD] //-Gds - Gbd
                                 );
        drain.setMatrix(gate,    derivChargeScale, dQ[9], //gcgdb,
                                 derivCurrentScale, dI[9]// = 0 //0
                                 );
        drain.setMatrix(bulk,    derivChargeScale, dQ[13], //gcbdb,
                                 derivCurrentScale, dI[12+nD] //Gbd
                                 );
        gate.setMatrix(source,   derivChargeScale, dQ[2], //gcsgb,
                                 derivCurrentScale, dI[4*nS+2] //Gm
                                 );
        gate.setMatrix(drain,    derivChargeScale, dQ[6], //gcdgb,
                                 derivCurrentScale, dI[4*nD+2] //-Gm - Gbg
                                 );
        gate.setMatrix(gate,     derivChargeScale, dQ[10], //gcggb,
                                 derivCurrentScale, dI[10] //0
                                 );
        gate.setMatrix(bulk,     derivChargeScale, dQ[14], //gcbgb,
                                 derivCurrentScale, dI[14] //Gbg
                                 );
        bulk.setMatrix(source,   derivChargeScale, dQ[3], //-gcssb - gcsdb - gcsgb,
                                 derivCurrentScale, dI[4*nS+3] //Gmb
                                 );
        bulk.setMatrix(drain,    derivChargeScale, dQ[7], //-gcdsb - gcddb - gcdgb,
                                 derivCurrentScale, dI[4*nD+3] //-Gmb - Gbb
                                 );
        bulk.setMatrix(gate,     derivChargeScale, dQ[11], //-gcgsb - gcgdb - gcggb,
                                 derivCurrentScale, dI[11] //0
                                 );
        bulk.setMatrix(bulk,     derivChargeScale, dQ[15], //-gcbsb - gcbdb - gcbgb,
                                 derivCurrentScale, dI[15] //Gbb
                                 );
    }
    
    public void evalJavaVoltage(double chargeScale, double currentScale,
                            double derivChargeScale, double derivCurrentScale,
                            double time) {
          double qgd, qgs, qgb, VgstNVt, ExpVgst;
          double Vfbeff, dVfbeff_dVg, dVfbeff_dVb, V3, V4;
          double gcbdb, gcbgb, gcbsb, gcddb, gcdgb, gcdsb, gcgdb, gcggb, gcgsb, gcsdb;
          double gcsgb, gcssb;
          double qgate, qbulk, qdrn, qsrc, qinoi;
          double Vds, Vgs, Vbs;
          double Vgs_eff, Vfb;
          double Phis, dPhis_dVb, sqrtPhis, dsqrtPhis_dVb, Vth, dVth_dVb, dVth_dVd;
          double Vgst, dVgst_dVg, dVgst_dVb, dVgs_eff_dVg, Vtm;
          double n, dn_dVb, dn_dVd, voffcv, noff, dnoff_dVd, dnoff_dVb;
          double ExpArg, V0, CoxWLcen, QovCox, LINK;
          double DeltaPhi, dDeltaPhi_dVg, dDeltaPhi_dVd, dDeltaPhi_dVb;
          double Cox, Tox, Tcen, dTcen_dVg, dTcen_dVd, dTcen_dVb;
          double Ccen, Coxeff, dCoxeff_dVg, dCoxeff_dVd, dCoxeff_dVb;
          double Denomi, dDenomi_dVg, dDenomi_dVd, dDenomi_dVb;
          double ueff, dueff_dVg, dueff_dVd, dueff_dVb;
          double Esat, Vdsat;
          double EsatL, dEsatL_dVg, dEsatL_dVd, dEsatL_dVb;
          double dVdsat_dVg, dVdsat_dVb, dVdsat_dVd, Vasat, dAlphaz_dVg, dAlphaz_dVb;
          double dVasat_dVg, dVasat_dVb, dVasat_dVd, Va, dVa_dVd, dVa_dVg, dVa_dVb;
          double Vbseff, dVbseff_dVb, VbseffCV, dVbseffCV_dVb;
          double Arg1, One_Third_CoxWL, Two_Third_CoxWL, Alphaz, CoxWL;
          double T0, dT0_dVg, dT0_dVd, dT0_dVb;
          double T1, dT1_dVg, dT1_dVd, dT1_dVb;
          double T2, dT2_dVg, dT2_dVd, dT2_dVb;
          double T3, dT3_dVg, dT3_dVd, dT3_dVb;
          double T4, T5, T6, T7, T8, T9, T10, T11, T12;
          double tmp, Abulk, dAbulk_dVb, Abulk0, dAbulk0_dVb;
          double VACLM, dVACLM_dVg, dVACLM_dVd, dVACLM_dVb;
          double VADIBL, dVADIBL_dVg, dVADIBL_dVd, dVADIBL_dVb;
          double Xdep, dXdep_dVb, lt1, dlt1_dVb, ltw, dltw_dVb, Delt_vth, dDelt_vth_dVb;
          double Theta0, dTheta0_dVb;
          double TempRatio, tmp1, tmp2, tmp3, tmp4;
          double DIBL_Sft, dDIBL_Sft_dVd, Lambda, dLambda_dVg;
          double a1;

          double Vgsteff, dVgsteff_dVg, dVgsteff_dVd, dVgsteff_dVb;
          double Vdseff, dVdseff_dVg, dVdseff_dVd, dVdseff_dVb;
          double VdseffCV, dVdseffCV_dVg, dVdseffCV_dVd, dVdseffCV_dVb;
          double diffVds, dAbulk_dVg;
          double beta, dbeta_dVg, dbeta_dVd, dbeta_dVb;
          double gche, dgche_dVg, dgche_dVd, dgche_dVb;
          double fgche1, dfgche1_dVg, dfgche1_dVd, dfgche1_dVb;
          double fgche2, dfgche2_dVg, dfgche2_dVd, dfgche2_dVb;
          double Idl, dIdl_dVg, dIdl_dVd, dIdl_dVb;
          double Idsa, dIdsa_dVg, dIdsa_dVd, dIdsa_dVb;
          double Ids, Gm, Gds, Gmb;
          double Isub, Gbd, Gbg, Gbb;
          double VASCBE, dVASCBE_dVg, dVASCBE_dVd, dVASCBE_dVb;
          double CoxWovL;
          double Rds, dRds_dVg, dRds_dVb, WVCox, WVCoxRds;
          double Vgst2Vtm, VdsatCV, dVdsatCV_dVg, dVdsatCV_dVb;
          double Leff, Weff, dWeff_dVg, dWeff_dVb;
          double AbulkCV, dAbulkCV_dVb;
          double qgdo, qgso, cgdo, cgso;

          double Cgg, Cgd, Cgb;
          double Csg, Csd, Csb, Cbg, Cbd, Cbb;
          double Cgg1, Cgb1, Cgd1, Cbg1, Cbb1, Cbd1, Qac0, Qsub0;
          double dQac0_dVg, dQac0_dVb, dQsub0_dVg, dQsub0_dVd, dQsub0_dVb;

          double thetavth;

          double cggb, cgsb, cgdb, cdgb, cdsb, cddb, cbgb, cbsb, cbdb;
          double qinv;
          double vgd, vgs, vgb;

          final SizedModel s = SizedModel.newSizedModel(m, width, length);

          Node source = getSource();
          Node drain = getDrain();
          Node gate = getGate();
          Node bulk = getBulk();

          if (source.getVoltage() * m.type >
               drain.getVoltage() * m.type)
          {
              Node temp = source;
              source = drain;
              drain = temp;
          }

          /* convert voltages to NMOS conventions */
          double Vs = m.type * source.getVoltage();
          double Vd = m.type * drain.getVoltage();
          double Vg = m.type * gate.getVoltage();
          double Vb = m.type * bulk.getVoltage();

          /* unadulterated voltage differences */
          vgd = Vg - Vd;
          vgs = Vg - Vs;
          vgb = Vg - Vb;

          Vds = Vd - Vs;
          Vgs = Vg - Vs; // Note that this equals vgs
          Vbs = Vb - Vs;

          /* compute Vbseff */
          T0 = Vbs - s.vbsc - 0.001;
          T1 = Math.sqrt(T0 * T0 - 0.004 * s.vbsc);
          Vbseff = s.vbsc + 0.5 * (T0 + T1);
          dVbseff_dVb = 0.5 * (1.0 + T0 / T1);
          if (Vbseff < Vbs)
            Vbseff = Vbs;

          if (Vbseff > 0.0)
            {
            T0 = s.phi / (s.phi + Vbseff);
            Phis = s.phi * T0;
            dPhis_dVb = -T0 * T0;
            sqrtPhis = s.phis3 / (s.phi + 0.5 * Vbseff);
            dsqrtPhis_dVb = -0.5 * sqrtPhis * sqrtPhis / s.phis3;
            }
          else
            {
            Phis = s.phi - Vbseff;
            dPhis_dVb = -1.0;
            sqrtPhis = Math.sqrt(Phis);
            dsqrtPhis_dVb = -0.5 / sqrtPhis;
            }

          /* compute Xdep */
          Xdep = s.Xdep0 * sqrtPhis / s.sqrtPhi;
          dXdep_dVb = (s.Xdep0 / s.sqrtPhi) * dsqrtPhis_dVb;

          /* grab Leff and Vtm */
          Leff = s.leff;
          Vtm = m.vtm;

          /* Vth Calculation */
          T3 = Math.sqrt(Xdep);
          V0 = s.vbi - s.phi;

          T0 = s.dvt2 * Vbseff;
          if (T0 >= - 0.5)
            {
            T1 = 1.0 + T0;
            T2 = s.dvt2;
            }
          else
            {
            /* Added to avoid any discontinuity problems caused by dvt2 */
            T4 = 1.0 / (3.0 + 8.0 * T0);
            T1 = (1.0 + 3.0 * T0) * T4;
            T2 = s.dvt2 * T4 * T4;
            }
          lt1 = m.factor1 * T3 * T1;
          dlt1_dVb = m.factor1 * (0.5 / T3 * T1 * dXdep_dVb + T3 * T2);

          T0 = s.dvt2w * Vbseff;
          if (T0 >= - 0.5)
            {
            T1 = 1.0 + T0;
            T2 = s.dvt2w;
            }
          else
            {
            /* Added to avoid any discontinuity problems caused by dvt2w */
            T4 = 1.0 / (3.0 + 8.0 * T0);
            T1 = (1.0 + 3.0 * T0) * T4;
            T2 = s.dvt2w * T4 * T4;
            }
          ltw = m.factor1 * T3 * T1;
          dltw_dVb = m.factor1 * (0.5 / T3 * T1 * dXdep_dVb + T3 * T2);

          T0 = -0.5 * s.dvt1 * Leff / lt1;
          if (T0 > -BSim3Model.EXP_THRESHOLD)
            {
            T1 = Math.exp(T0);
            Theta0 = T1 * (1.0 + 2.0 * T1);
            dT1_dVb = -T0 / lt1 * T1 * dlt1_dVb;
            dTheta0_dVb = (1.0 + 4.0 * T1) * dT1_dVb;
            }
          else
            {
            T1 = BSim3Model.MIN_EXP;
            Theta0 = T1 * (1.0 + 2.0 * T1);
            dTheta0_dVb = 0.0;
            }

          thetavth = s.dvt0 * Theta0;
          Delt_vth = thetavth * V0;
          dDelt_vth_dVb = s.dvt0 * dTheta0_dVb * V0;

          T0 = -0.5 * s.dvt1w * s.weff * Leff / ltw;
          if (T0 > -BSim3Model.EXP_THRESHOLD)
            {
            T1 = Math.exp(T0);
            T2 = T1 * (1.0 + 2.0 * T1);
            dT1_dVb = -T0 / ltw * T1 * dltw_dVb;
            dT2_dVb = (1.0 + 4.0 * T1) * dT1_dVb;
            }
          else
            {
            T1 = BSim3Model.MIN_EXP;
            T2 = T1 * (1.0 + 2.0 * T1);
            dT2_dVb = 0.0;
            }

          T0 = s.dvt0w * T2;
          T2 = T0 * V0;
          dT2_dVb = s.dvt0w * dT2_dVb * V0;

          TempRatio =  m.temp / m.tnom - 1.0;
          T0 = Math.sqrt(1.0 + s.nlx / Leff);
          T1 = s.k1ox * (T0 - 1.0) * s.sqrtPhi
              + (s.kt1 + s.kt1l / Leff + s.kt2 * Vbseff) * TempRatio;
          tmp2 = m.tox * s.phi / (s.weff + s.w0);

          T3 = s.eta0 + s.etab * Vbseff;
          if (T3 < 1.0e-4)
            {
            /* avoid  discontinuity problems caused by etab */
            T9 = 1.0 / (3.0 - 2.0e4 * T3);
            T3 = (2.0e-4 - T3) * T9;
            T4 = T9 * T9;
            }
          else
            T4 = 1.0;
          dDIBL_Sft_dVd = T3 * s.theta0vb0;
          DIBL_Sft = dDIBL_Sft_dVd * Vds;

          Vth = m.type * s.vth0 - s.k1 * s.sqrtPhi + s.k1ox * sqrtPhis
              - s.k2ox * Vbseff - Delt_vth - T2
              + (s.k3 + s.k3b * Vbseff) * tmp2 + T1 - DIBL_Sft;

          dVth_dVb = s.k1ox * dsqrtPhis_dVb - s.k2ox
              - dDelt_vth_dVb - dT2_dVb + s.k3b * tmp2
              - s.etab * Vds * s.theta0vb0 * T4 + s.kt2 * TempRatio;
          dVth_dVd = -dDIBL_Sft_dVd;

          /* Calculate n */
          tmp2 = s.nfactor * BSim3Model.EPSSI / Xdep;
          tmp3 = s.cdsc + s.cdscb * Vbseff + s.cdscd * Vds;
          tmp4 = (tmp2 + tmp3 * Theta0 + s.cit) / m.cox;
          if (tmp4 >= -0.5)
            {
            n = 1.0 + tmp4;
            dn_dVb = (-tmp2 / Xdep * dXdep_dVb + tmp3 * dTheta0_dVb + s.cdscb * Theta0) / m.cox;
            dn_dVd = s.cdscd * Theta0 / m.cox;
            }
          else
            {
            /* avoid  discontinuity problems caused by tmp4 */
            T0 = 1.0 / (3.0 + 8.0 * tmp4);
            n = (1.0 + 3.0 * tmp4) * T0;
            T0 *= T0;
            dn_dVb = (-tmp2 / Xdep * dXdep_dVb + tmp3 * dTheta0_dVb
                      + s.cdscb * Theta0) / m.cox * T0;
            dn_dVd = s.cdscd * Theta0 / m.cox * T0;
            }

          /* Poly Gate Si Depletion Effect */
          T0 = s.vfb + s.phi;
          if ((s.ngate > 1.e18) && (s.ngate < 1.e25) && (Vgs > T0))
            {
            /* added to avoid the problem caused by ngate */
            T1 = 1.0e6 * BSim3Model.Charge_q * BSim3Model.EPSSI * s.ngate / (m.cox * m.cox);
            T4 = Math.sqrt(1.0 + 2.0 * (Vgs - T0) / T1);
            T2 = T1 * (T4 - 1.0);
            T3 = 0.5 * T2 * T2 / T1; /* T3 = Vpoly */
            T7 = 1.12 - T3 - 0.05;
            T6 = Math.sqrt(T7 * T7 + 0.224);
            T5 = 1.12 - 0.5 * (T7 + T6);
            Vgs_eff = Vgs - T5;
            dVgs_eff_dVg = 1.0 - (0.5 - 0.5 / T4) * (1.0 + T7 / T6);
            }
          else
            {
            Vgs_eff = Vgs;
            dVgs_eff_dVg = 1.0;
            }
          Vgst = Vgs_eff - Vth;

          /* Effective Vgst (Vgsteff) Calculation */
          T10 = 2.0 * n * Vtm;
          VgstNVt = Vgst / T10;
          ExpArg = (2.0 * s.voff - Vgst) / T10;

          /* MCJ: Very small Vgst */
          if (VgstNVt > BSim3Model.EXP_THRESHOLD)
            {
            Vgsteff = Vgst;
            dVgsteff_dVg = dVgs_eff_dVg;
            dVgsteff_dVd = -dVth_dVd;
            dVgsteff_dVb = -dVth_dVb;
            }
          else if (ExpArg > BSim3Model.EXP_THRESHOLD)
            {
            T0 = (Vgst - s.voff) / (n * Vtm);
            ExpVgst = Math.exp(T0);
            Vgsteff = Vtm * s.cdep0 / m.cox * ExpVgst;
            dVgsteff_dVg = Vgsteff / (n * Vtm);
            dVgsteff_dVd = -dVgsteff_dVg * (dVth_dVd + T0 * Vtm * dn_dVd);
            dVgsteff_dVb = -dVgsteff_dVg * (dVth_dVb + T0 * Vtm * dn_dVb);
            dVgsteff_dVg *= dVgs_eff_dVg;
            }
          else
            {
            ExpVgst = Math.exp(VgstNVt);
            T1 = T10 * Math.log(1.0 + ExpVgst);
            dT1_dVg = ExpVgst / (1.0 + ExpVgst);
            dT1_dVb = -dT1_dVg * (dVth_dVb + Vgst / n * dn_dVb) + T1 / n * dn_dVb;
            dT1_dVd = -dT1_dVg * (dVth_dVd + Vgst / n * dn_dVd) + T1 / n * dn_dVd;

            dT2_dVg = -m.cox / (Vtm * s.cdep0) * Math.exp(ExpArg);
            T2 = 1.0 - T10 * dT2_dVg;
            dT2_dVd = -dT2_dVg * (dVth_dVd - 2.0 * Vtm * ExpArg * dn_dVd)
                + (T2 - 1.0) / n * dn_dVd;
            dT2_dVb = -dT2_dVg * (dVth_dVb - 2.0 * Vtm * ExpArg * dn_dVb)
                + (T2 - 1.0) / n * dn_dVb;

            Vgsteff = T1 / T2;
            T3 = T2 * T2;
            dVgsteff_dVg = (T2 * dT1_dVg - T1 * dT2_dVg) / T3 * dVgs_eff_dVg;
            dVgsteff_dVd = (T2 * dT1_dVd - T1 * dT2_dVd) / T3;
            dVgsteff_dVb = (T2 * dT1_dVb - T1 * dT2_dVb) / T3;
            }

          /* Calculate Effective Channel Geometry */
          T9 = sqrtPhis - s.sqrtPhi;
          Weff = s.weff - 2.0 * (s.dwg * Vgsteff + s.dwb * T9);
          dWeff_dVg = -2.0 * s.dwg;
          dWeff_dVb = -2.0 * s.dwb * dsqrtPhis_dVb;

          if (Weff < 2.0e-8)
            {
            /* to avoid the discontinuity problem due to Weff*/
            T0 = 1.0 / (6.0e-8 - 2.0 * Weff);
            Weff = 2.0e-8 * (4.0e-8 - Weff) * T0;
            T0 *= T0 * 4.0e-16;
            dWeff_dVg *= T0;
            dWeff_dVb *= T0;
            }

          T0 = s.prwg * Vgsteff + s.prwb * T9;
          if (T0 >= -0.9)
            {
            Rds = s.rds0 * (1.0 + T0);
            dRds_dVg = s.rds0 * s.prwg;
            dRds_dVb = s.rds0 * s.prwb * dsqrtPhis_dVb;
            }
          else
            {
            /* to avoid the discontinuity problem due to prwg and prwb*/
            T1 = 1.0 / (17.0 + 20.0 * T0);
            Rds = s.rds0 * (0.8 + T0) * T1;
            T1 *= T1;
            dRds_dVg = s.rds0 * s.prwg * T1;
            dRds_dVb = s.rds0 * s.prwb * dsqrtPhis_dVb * T1;
            }

          /* Calculate Abulk */
          T1 = 0.5 * s.k1ox / sqrtPhis;
          dT1_dVb = -T1 / sqrtPhis * dsqrtPhis_dVb;

          T9 = Math.sqrt(s.xj * Xdep);
          tmp1 = Leff + 2.0 * T9;
          T5 = Leff / tmp1;
          tmp2 = s.a0 * T5;
          tmp3 = s.weff + s.b1;
          tmp4 = s.b0 / tmp3;
          T2 = tmp2 + tmp4;
          dT2_dVb = -T9 / tmp1 / Xdep * dXdep_dVb;
          T6 = T5 * T5;
          T7 = T5 * T6;

          Abulk0 = 1.0 + T1 * T2;
          dAbulk0_dVb = T1 * tmp2 * dT2_dVb + T2 * dT1_dVb;

          T8 = s.ags * s.a0 * T7;
          dAbulk_dVg = -T1 * T8;
          Abulk = Abulk0 + dAbulk_dVg * Vgsteff;
          dAbulk_dVb = dAbulk0_dVb - T8 * Vgsteff * (dT1_dVb + 3.0 * T1 * dT2_dVb);

          if (Abulk0 < 0.1)
            {
            /* added to avoid the problems caused by Abulk0 */
            T9 = 1.0 / (3.0 - 20.0 * Abulk0);
            Abulk0 = (0.2 - Abulk0) * T9;
            dAbulk0_dVb *= T9 * T9;
            }

          if (Abulk < 0.1)
            {
            /* added to avoid the problems caused by Abulk */
            T9 = 1.0 / (3.0 - 20.0 * Abulk);
            Abulk = (0.2 - Abulk) * T9;
            T10 = T9 * T9;
            dAbulk_dVb *= T10;
            dAbulk_dVg *= T10;
            }

          T2 = s.keta * Vbseff;
          if (T2 >= -0.9)
            {
            T0 = 1.0 / (1.0 + T2);
            dT0_dVb = -s.keta * T0 * T0;
            }
          else
            {
            /* added to avoid the problems caused by Keta */
            T1 = 1.0 / (0.8 + T2);
            T0 = (17.0 + 20.0 * T2) * T1;
            dT0_dVb = -s.keta * T1 * T1;
            }
          dAbulk_dVg *= T0;
          dAbulk_dVb = dAbulk_dVb * T0 + Abulk * dT0_dVb;
          dAbulk0_dVb = dAbulk0_dVb * T0 + Abulk0 * dT0_dVb;
          Abulk *= T0;
          Abulk0 *= T0;


          /* Mobility calculation */
          if (m.mobMod == 1)
            {
            T0 = Vgsteff + Vth + Vth;
            T2 = s.ua + s.uc * Vbseff;
            T3 = T0 / m.tox;
            T5 = T3 * (T2 + s.ub * T3);
            dDenomi_dVg = (T2 + 2.0 * s.ub * T3) / m.tox;
            dDenomi_dVd = dDenomi_dVg * 2.0 * dVth_dVd;
            dDenomi_dVb = dDenomi_dVg * 2.0 * dVth_dVb + s.uc * T3;
            }
          else if (m.mobMod == 2)
            {
            T5 = Vgsteff / m.tox * (s.ua + s.uc * Vbseff + s.ub * Vgsteff / m.tox);
            dDenomi_dVg = (s.ua + s.uc * Vbseff + 2.0 * s.ub * Vgsteff / m.tox) / m.tox;
            dDenomi_dVd = 0.0;
            dDenomi_dVb = Vgsteff * s.uc / m.tox;
            }
          else
            {
            T0 = Vgsteff + Vth + Vth;
            T2 = 1.0 + s.uc * Vbseff;
            T3 = T0 / m.tox;
            T4 = T3 * (s.ua + s.ub * T3);
            T5 = T4 * T2;
            dDenomi_dVg = (s.ua + 2.0 * s.ub * T3) * T2 / m.tox;
            dDenomi_dVd = dDenomi_dVg * 2.0 * dVth_dVd;
            dDenomi_dVb = dDenomi_dVg * 2.0 * dVth_dVb + s.uc * T4;
            }

          if (T5 >= -0.8)
            Denomi = 1.0 + T5;
          else
            {
            /* Added to avoid the discontinuity problem caused by ua and ub*/
            T9 = 1.0 / (7.0 + 10.0 * T5);
            Denomi = (0.6 + T5) * T9;
            T9 *= T9;
            dDenomi_dVg *= T9;
            dDenomi_dVd *= T9;
            dDenomi_dVb *= T9;
            }

          ueff = s.u0temp / Denomi;
          T9 = -ueff / Denomi;
          dueff_dVg = T9 * dDenomi_dVg;
          dueff_dVd = T9 * dDenomi_dVd;
          dueff_dVb = T9 * dDenomi_dVb;

          /* Saturation Drain Voltage  Vdsat */
          WVCox = Weff * s.vsattemp * m.cox;
          WVCoxRds = WVCox * Rds;

          Esat = 2.0 * s.vsattemp / ueff;
          EsatL = Esat * Leff;
          T0 = -EsatL /ueff;
          dEsatL_dVg = T0 * dueff_dVg;
          dEsatL_dVd = T0 * dueff_dVd;
          dEsatL_dVb = T0 * dueff_dVb;

          /* Sqrt() */
          a1 = s.a1;
          if (a1 == 0.0)
            {
            Lambda = s.a2;
            dLambda_dVg = 0.0;
            }
          else if (a1 > 0.0)
            {
            /* Added to avoid the discontinuity problem caused by a1 and a2 (Lambda) */
            T0 = 1.0 - s.a2;
            T1 = T0 - s.a1 * Vgsteff - 0.0001;
            T2 = Math.sqrt(T1 * T1 + 0.0004 * T0);
            Lambda = s.a2 + T0 - 0.5 * (T1 + T2);
            dLambda_dVg = 0.5 * s.a1 * (1.0 + T1 / T2);
            }
          else
            {
            T1 = s.a2 + s.a1 * Vgsteff - 0.0001;
            T2 = Math.sqrt(T1 * T1 + 0.0004 * s.a2);
            Lambda = 0.5 * (T1 + T2);
            dLambda_dVg = 0.5 * s.a1 * (1.0 + T1 / T2);
            }

          Vgst2Vtm = Vgsteff + 2.0 * Vtm;
          if (Rds > 0)
            {
            tmp2 = dRds_dVg / Rds + dWeff_dVg / Weff;
            tmp3 = dRds_dVb / Rds + dWeff_dVb / Weff;
            }
          else
            {
            tmp2 = dWeff_dVg / Weff;
            tmp3 = dWeff_dVb / Weff;
            }
          if ((Rds == 0.0) && (Lambda == 1.0))
            {
            T0 = 1.0 / (Abulk * EsatL + Vgst2Vtm);
            tmp1 = 0.0;
            T1 = T0 * T0;
            T2 = Vgst2Vtm * T0;
            T3 = EsatL * Vgst2Vtm;
            Vdsat = T3 * T0;

            dT0_dVg = -(Abulk * dEsatL_dVg + EsatL * dAbulk_dVg + 1.0) * T1;
            dT0_dVd = -(Abulk * dEsatL_dVd) * T1;
            dT0_dVb = -(Abulk * dEsatL_dVb + dAbulk_dVb * EsatL) * T1;

            dVdsat_dVg = T3 * dT0_dVg + T2 * dEsatL_dVg + EsatL * T0;
            dVdsat_dVd = T3 * dT0_dVd + T2 * dEsatL_dVd;
            dVdsat_dVb = T3 * dT0_dVb + T2 * dEsatL_dVb;
            }
          else
            {
            tmp1 = dLambda_dVg / (Lambda * Lambda);
            T9 = Abulk * WVCoxRds;
            T8 = Abulk * T9;
            T7 = Vgst2Vtm * T9;
            T6 = Vgst2Vtm * WVCoxRds;
            T0 = 2.0 * Abulk * (T9 - 1.0 + 1.0 / Lambda);
            dT0_dVg = 2.0 * (T8 * tmp2 - Abulk * tmp1
                             + (2.0 * T9 + 1.0 / Lambda - 1.0) * dAbulk_dVg);

            dT0_dVb = 2.0 * (T8 * (2.0 / Abulk * dAbulk_dVb + tmp3)
                             + (1.0 / Lambda - 1.0) * dAbulk_dVb);
            dT0_dVd = 0.0;
            T1 = Vgst2Vtm * (2.0 / Lambda - 1.0) + Abulk * EsatL + 3.0 * T7;

            dT1_dVg = (2.0 / Lambda - 1.0) - 2.0 * Vgst2Vtm * tmp1
                + Abulk * dEsatL_dVg + EsatL * dAbulk_dVg + 3.0
                * (T9 + T7 * tmp2 + T6 * dAbulk_dVg);
            dT1_dVb = Abulk * dEsatL_dVb + EsatL * dAbulk_dVb
                + 3.0 * (T6 * dAbulk_dVb + T7 * tmp3);
            dT1_dVd = Abulk * dEsatL_dVd;

            T2 = Vgst2Vtm * (EsatL + 2.0 * T6);
            dT2_dVg = EsatL + Vgst2Vtm * dEsatL_dVg
                + T6 * (4.0 + 2.0 * Vgst2Vtm * tmp2);
            dT2_dVb = Vgst2Vtm * (dEsatL_dVb + 2.0 * T6 * tmp3);
            dT2_dVd = Vgst2Vtm * dEsatL_dVd;

            T3 = Math.sqrt(T1 * T1 - 2.0 * T0 * T2);
            Vdsat = (T1 - T3) / T0;

            dT3_dVg = (T1 * dT1_dVg - 2.0 * (T0 * dT2_dVg + T2 * dT0_dVg)) / T3;
            dT3_dVd = (T1 * dT1_dVd - 2.0 * (T0 * dT2_dVd + T2 * dT0_dVd)) / T3;
            dT3_dVb = (T1 * dT1_dVb - 2.0 * (T0 * dT2_dVb + T2 * dT0_dVb)) / T3;

            dVdsat_dVg = (dT1_dVg - (T1 * dT1_dVg - dT0_dVg * T2
                                     - T0 * dT2_dVg) / T3 - Vdsat * dT0_dVg) / T0;
            dVdsat_dVb = (dT1_dVb - (T1 * dT1_dVb - dT0_dVb * T2
                                     - T0 * dT2_dVb) / T3 - Vdsat * dT0_dVb) / T0;
            dVdsat_dVd = (dT1_dVd - (T1 * dT1_dVd - T0 * dT2_dVd) / T3) / T0;
            }

          /* Effective Vds (Vdseff) Calculation */
          T1 = Vdsat - Vds - s.delta;
          dT1_dVg = dVdsat_dVg;
          dT1_dVd = dVdsat_dVd - 1.0;
          dT1_dVb = dVdsat_dVb;

          T2 = Math.sqrt(T1 * T1 + 4.0 * s.delta * Vdsat);
          T0 = T1 / T2;
          T3 = 2.0 * s.delta / T2;
          dT2_dVg = T0 * dT1_dVg + T3 * dVdsat_dVg;
          dT2_dVd = T0 * dT1_dVd + T3 * dVdsat_dVd;
          dT2_dVb = T0 * dT1_dVb + T3 * dVdsat_dVb;

          Vdseff = Vdsat - 0.5 * (T1 + T2);
          dVdseff_dVg = dVdsat_dVg - 0.5 * (dT1_dVg + dT2_dVg);
          dVdseff_dVd = dVdsat_dVd - 0.5 * (dT1_dVd + dT2_dVd);
          dVdseff_dVb = dVdsat_dVb - 0.5 * (dT1_dVb + dT2_dVb);
          if (Vds == 0.0)
            {
            /* Added to eliminate non-zero Vdseff at Vds=0.0 */
            Vdseff = 0.0;
            dVdseff_dVg = 0.0;
            dVdseff_dVb = 0.0;
            }

          /* Calculate VAsat */
          tmp4 = 1.0 - 0.5 * Abulk * Vdsat / Vgst2Vtm;
          T9 = WVCoxRds * Vgsteff;
          T8 = T9 / Vgst2Vtm;
          T0 = EsatL + Vdsat + 2.0 * T9 * tmp4;

          T7 = 2.0 * WVCoxRds * tmp4;
          dT0_dVg = dEsatL_dVg + dVdsat_dVg + T7 * (1.0 + tmp2 * Vgsteff)
              - T8 * (Abulk * dVdsat_dVg - Abulk * Vdsat / Vgst2Vtm + Vdsat * dAbulk_dVg);

          dT0_dVb = dEsatL_dVb + dVdsat_dVb + T7 * tmp3 * Vgsteff
              - T8 * (dAbulk_dVb * Vdsat + Abulk * dVdsat_dVb);
          dT0_dVd = dEsatL_dVd + dVdsat_dVd - T8 * Abulk * dVdsat_dVd;

          T9 = WVCoxRds * Abulk;
          T1 = 2.0 / Lambda - 1.0 + T9;
          dT1_dVg = -2.0 * tmp1 +  WVCoxRds * (Abulk * tmp2 + dAbulk_dVg);
          dT1_dVb = dAbulk_dVb * WVCoxRds + T9 * tmp3;

          Vasat = T0 / T1;
          dVasat_dVg = (dT0_dVg - Vasat * dT1_dVg) / T1;
          dVasat_dVb = (dT0_dVb - Vasat * dT1_dVb) / T1;
          dVasat_dVd = dT0_dVd / T1;

          if (Vdseff > Vds)
            Vdseff = Vds;
          diffVds = Vds - Vdseff;

          /* Calculate VACLM */
          if ((s.pclm > 0.0) && (diffVds > 1.0e-10))
            {
            T0 = 1.0 / (s.pclm * Abulk * s.litl);
            dT0_dVb = -T0 / Abulk * dAbulk_dVb;
            dT0_dVg = -T0 / Abulk * dAbulk_dVg;

            T2 = Vgsteff / EsatL;
            T1 = Leff * (Abulk + T2);
            dT1_dVg = Leff * ((1.0 - T2 * dEsatL_dVg) / EsatL + dAbulk_dVg);
            dT1_dVb = Leff * (dAbulk_dVb - T2 * dEsatL_dVb / EsatL);
            dT1_dVd = -T2 * dEsatL_dVd / Esat;

            T9 = T0 * T1;
            VACLM = T9 * diffVds;
            dVACLM_dVg = T0 * dT1_dVg * diffVds - T9 * dVdseff_dVg + T1 * diffVds * dT0_dVg;
            dVACLM_dVb = (dT0_dVb * T1 + T0 * dT1_dVb) * diffVds - T9 * dVdseff_dVb;
            dVACLM_dVd = T0 * dT1_dVd * diffVds + T9 * (1.0 - dVdseff_dVd);
            }
          else
            {
            VACLM = BSim3Model.MAX_EXP;
            dVACLM_dVd = dVACLM_dVg = dVACLM_dVb = 0.0;
            }

          /* Calculate VADIBL */
          if (s.thetaRout > 0.0)
            {
            T8 = Abulk * Vdsat;
            T0 = Vgst2Vtm * T8;
            dT0_dVg = Vgst2Vtm * Abulk * dVdsat_dVg + T8 + Vgst2Vtm * Vdsat * dAbulk_dVg;
            dT0_dVb = Vgst2Vtm * (dAbulk_dVb * Vdsat + Abulk * dVdsat_dVb);
            dT0_dVd = Vgst2Vtm * Abulk * dVdsat_dVd;

            T1 = Vgst2Vtm + T8;
            dT1_dVg = 1.0 + Abulk * dVdsat_dVg + Vdsat * dAbulk_dVg;
            dT1_dVb = Abulk * dVdsat_dVb + dAbulk_dVb * Vdsat;
            dT1_dVd = Abulk * dVdsat_dVd;

            T9 = T1 * T1;
            T2 = s.thetaRout;
            VADIBL = (Vgst2Vtm - T0 / T1) / T2;
            dVADIBL_dVg = (1.0 - dT0_dVg / T1 + T0 * dT1_dVg / T9) / T2;
            dVADIBL_dVb = (-dT0_dVb / T1 + T0 * dT1_dVb / T9) / T2;
            dVADIBL_dVd = (-dT0_dVd / T1 + T0 * dT1_dVd / T9) / T2;

            T7 = s.pdiblb * Vbseff;
            if (T7 >= -0.9)
              {
              T3 = 1.0 / (1.0 + T7);
              VADIBL *= T3;
              dVADIBL_dVg *= T3;
              dVADIBL_dVb = (dVADIBL_dVb - VADIBL * s.pdiblb) * T3;
              dVADIBL_dVd *= T3;
              }
            else
              {
              /* Added to avoid the discontinuity problem caused by pdiblcb */
              T4 = 1.0 / (0.8 + T7);
              T3 = (17.0 + 20.0 * T7) * T4;
              dVADIBL_dVg *= T3;
              dVADIBL_dVb = dVADIBL_dVb * T3 - VADIBL * s.pdiblb * T4 * T4;
              dVADIBL_dVd *= T3;
              VADIBL *= T3;
              }
            }
          else
            {
            VADIBL = BSim3Model.MAX_EXP;
            dVADIBL_dVd = dVADIBL_dVg = dVADIBL_dVb = 0.0;
            }

          /* Calculate VA */
          T8 = s.pvag / EsatL;
          T9 = T8 * Vgsteff;
          if (T9 > -0.9)
            {
            T0 = 1.0 + T9;
            dT0_dVg = T8 * (1.0 - Vgsteff * dEsatL_dVg / EsatL);
            dT0_dVb = -T9 * dEsatL_dVb / EsatL;
            dT0_dVd = -T9 * dEsatL_dVd / EsatL;
            }
          else
            {
            /* Added to avoid the discontinuity problems caused by pvag */
            T1 = 1.0 / (17.0 + 20.0 * T9);
            T0 = (0.8 + T9) * T1;
            T1 *= T1;
            dT0_dVg = T8 * (1.0 - Vgsteff * dEsatL_dVg / EsatL) * T1;

            T9 *= T1 / EsatL;
            dT0_dVb = -T9 * dEsatL_dVb;
            dT0_dVd = -T9 * dEsatL_dVd;
            }

          tmp1 = VACLM * VACLM;
          tmp2 = VADIBL * VADIBL;
          tmp3 = VACLM + VADIBL;

          T1 = VACLM * VADIBL / tmp3;
          tmp3 *= tmp3;
          dT1_dVg = (tmp1 * dVADIBL_dVg + tmp2 * dVACLM_dVg) / tmp3;
          dT1_dVd = (tmp1 * dVADIBL_dVd + tmp2 * dVACLM_dVd) / tmp3;
          dT1_dVb = (tmp1 * dVADIBL_dVb + tmp2 * dVACLM_dVb) / tmp3;

          Va = Vasat + T0 * T1;
          dVa_dVg = dVasat_dVg + T1 * dT0_dVg + T0 * dT1_dVg;
          dVa_dVd = dVasat_dVd + T1 * dT0_dVd + T0 * dT1_dVd;
          dVa_dVb = dVasat_dVb + T1 * dT0_dVb + T0 * dT1_dVb;

          /* Calculate VASCBE */
          if (s.pscbe2 > 0.0)
            {
            if (diffVds > s.pscbe1 * s.litl / BSim3Model.EXP_THRESHOLD)
              {
              T0 =  s.pscbe1 * s.litl / diffVds;
              VASCBE = Leff * Math.exp(T0) / s.pscbe2;
              T1 = T0 * VASCBE / diffVds;
              dVASCBE_dVg = T1 * dVdseff_dVg;
              dVASCBE_dVd = -T1 * (1.0 - dVdseff_dVd);
              dVASCBE_dVb = T1 * dVdseff_dVb;
              }
            else
              {
              VASCBE = BSim3Model.MAX_EXP * Leff/s.pscbe2;
              dVASCBE_dVg = dVASCBE_dVd = dVASCBE_dVb = 0.0;
              }
            }
          else
            {
            VASCBE = BSim3Model.MAX_EXP;
            dVASCBE_dVg = dVASCBE_dVd = dVASCBE_dVb = 0.0;
            }

          /* Calculate Ids */
          CoxWovL = m.cox * Weff / Leff;
          beta = ueff * CoxWovL;
          dbeta_dVg = CoxWovL * dueff_dVg + beta * dWeff_dVg / Weff;
          dbeta_dVd = CoxWovL * dueff_dVd;
          dbeta_dVb = CoxWovL * dueff_dVb + beta * dWeff_dVb / Weff;

          T0 = 1.0 - 0.5 * Abulk * Vdseff / Vgst2Vtm;
          dT0_dVg = -0.5 * (Abulk * dVdseff_dVg
                            - Abulk * Vdseff / Vgst2Vtm + Vdseff * dAbulk_dVg) / Vgst2Vtm;
          dT0_dVd = -0.5 * Abulk * dVdseff_dVd / Vgst2Vtm;
          dT0_dVb = -0.5 * (Abulk * dVdseff_dVb + dAbulk_dVb * Vdseff) / Vgst2Vtm;

          fgche1 = Vgsteff * T0;
          dfgche1_dVg = Vgsteff * dT0_dVg + T0;
          dfgche1_dVd = Vgsteff * dT0_dVd;
          dfgche1_dVb = Vgsteff * dT0_dVb;

          T9 = Vdseff / EsatL;
          fgche2 = 1.0 + T9;
          dfgche2_dVg = (dVdseff_dVg - T9 * dEsatL_dVg) / EsatL;
          dfgche2_dVd = (dVdseff_dVd - T9 * dEsatL_dVd) / EsatL;
          dfgche2_dVb = (dVdseff_dVb - T9 * dEsatL_dVb) / EsatL;

          gche = beta * fgche1 / fgche2;
          dgche_dVg = (beta * dfgche1_dVg + fgche1 * dbeta_dVg
                       - gche * dfgche2_dVg) / fgche2;
          dgche_dVd = (beta * dfgche1_dVd + fgche1 * dbeta_dVd
                       - gche * dfgche2_dVd) / fgche2;
          dgche_dVb = (beta * dfgche1_dVb + fgche1 * dbeta_dVb
                       - gche * dfgche2_dVb) / fgche2;

          T0 = 1.0 + gche * Rds;
          T9 = Vdseff / T0;
          Idl = gche * T9;

          dIdl_dVg = (gche * dVdseff_dVg + T9 * dgche_dVg) / T0
              - Idl * gche / T0 * dRds_dVg ;

          dIdl_dVd = (gche * dVdseff_dVd + T9 * dgche_dVd) / T0;
          dIdl_dVb = (gche * dVdseff_dVb + T9 * dgche_dVb - Idl * dRds_dVb * gche) / T0;

          T9 =  diffVds / Va;
          T0 =  1.0 + T9;
          Idsa = Idl * T0;
          dIdsa_dVg = T0 * dIdl_dVg - Idl * (dVdseff_dVg + T9 * dVa_dVg) / Va;
          dIdsa_dVd = T0 * dIdl_dVd + Idl * (1.0 - dVdseff_dVd - T9 * dVa_dVd) / Va;
          dIdsa_dVb = T0 * dIdl_dVb - Idl * (dVdseff_dVb + T9 * dVa_dVb) / Va;

          T9 = diffVds / VASCBE;
          T0 = 1.0 + T9;
          Ids = Idsa * T0;

          Gm = T0 * dIdsa_dVg - Idsa * (dVdseff_dVg + T9 * dVASCBE_dVg) / VASCBE;
          Gds = T0 * dIdsa_dVd + Idsa * (1.0 - dVdseff_dVd - T9 * dVASCBE_dVd) / VASCBE;
          Gmb = T0 * dIdsa_dVb - Idsa * (dVdseff_dVb + T9 * dVASCBE_dVb) / VASCBE;

          Gds += Gm * dVgsteff_dVd;
          Gmb += Gm * dVgsteff_dVb;
          Gm *= dVgsteff_dVg;
          Gmb *= dVbseff_dVb;

          /* Substrate current begins */
          tmp = s.alpha0 + s.alpha1 * Leff;
          if ((tmp <= 0.0) || (s.beta0 <= 0.0))
            Isub = Gbd = Gbb = Gbg = 0.0;
          else
            {
            T2 = tmp / Leff;
            if (diffVds > s.beta0 / BSim3Model.EXP_THRESHOLD)
              {
              T0 = -s.beta0 / diffVds;
              T1 = T2 * diffVds * Math.exp(T0);
              T3 = T1 / diffVds * (T0 - 1.0);
              dT1_dVg = T3 * dVdseff_dVg;
              dT1_dVd = T3 * (dVdseff_dVd - 1.0);
              dT1_dVb = T3 * dVdseff_dVb;
              }
            else
              {
              T3 = T2 * BSim3Model.MIN_EXP;
              T1 = T3 * diffVds;
              dT1_dVg = -T3 * dVdseff_dVg;
              dT1_dVd = T3 * (1.0 - dVdseff_dVd);
              dT1_dVb = -T3 * dVdseff_dVb;
              }
            Isub = T1 * Idsa;
            Gbg = T1 * dIdsa_dVg + Idsa * dT1_dVg;
            Gbd = T1 * dIdsa_dVd + Idsa * dT1_dVd;
            Gbb = T1 * dIdsa_dVb + Idsa * dT1_dVb;

            Gbd += Gbg * dVgsteff_dVd;
            Gbb += Gbg * dVgsteff_dVb;
            Gbg *= dVgsteff_dVg;
            Gbb *= dVbseff_dVb; /* bug fixing */
            }

          /* Charge/Capacitance computation begins */
          qgate = qdrn = qsrc = qbulk = 0.0;
          cggb = cgsb = cgdb = 0.0;
          cdgb = cdsb = cddb = 0.0;
          cbgb = cbsb = cbdb = 0.0;
          if (m.xpart >= 0 && m.capMod == 0)
            {
            if (Vbseff < 0.0)
              {
              Vbseff = Vbs;
              dVbseff_dVb = 1.0;
              }
            else
              {
              Vbseff = s.phi - Phis;
              dVbseff_dVb = -dPhis_dVb;
              }

            Vfb = s.vfbcv;
            Vth = Vfb + s.phi + s.k1ox * sqrtPhis;
            Vgst = Vgs_eff - Vth;
            dVth_dVb = s.k1ox * dsqrtPhis_dVb;
            dVgst_dVb = -dVth_dVb;
            dVgst_dVg = dVgs_eff_dVg;

            CoxWL = m.cox * s.weffCV * s.leffCV;
            Arg1 = Vgs_eff - Vbseff - Vfb;

            if (Arg1 <= 0.0)
              {
              qgate = CoxWL * Arg1;
              qbulk = -qgate;
              qdrn = 0.0;

              cggb = CoxWL * dVgs_eff_dVg;
              cgdb = 0.0;
              cgsb = CoxWL * (dVbseff_dVb - dVgs_eff_dVg);

              cdgb = 0.0;
              cddb = 0.0;
              cdsb = 0.0;

              cbgb = -CoxWL * dVgs_eff_dVg;
              cbdb = 0.0;
              cbsb = -cgsb;
              qinv = 0.0;
              }
            else if (Vgst <= 0.0)
              {
              T1 = 0.5 * s.k1ox;
              T2 = Math.sqrt(T1 * T1 + Arg1);
              qgate = CoxWL * s.k1ox * (T2 - T1);
              qbulk = -qgate;
              qdrn = 0.0;

              T0 = CoxWL * T1 / T2;
              cggb = T0 * dVgs_eff_dVg;
              cgdb = 0.0;
              cgsb = T0 * (dVbseff_dVb - dVgs_eff_dVg);

              cdgb = 0.0;
              cddb = 0.0;
              cdsb = 0.0;

              cbgb = -cggb;
              cbdb = 0.0;
              cbsb = -cgsb;
              qinv = 0.0;
              }
            else
              {
              One_Third_CoxWL = CoxWL / 3.0;
              Two_Third_CoxWL = 2.0 * One_Third_CoxWL;

              AbulkCV = Abulk0 * s.abulkCVfactor;
              dAbulkCV_dVb = s.abulkCVfactor * dAbulk0_dVb;
              Vdsat = Vgst / AbulkCV;
              dVdsat_dVg = dVgs_eff_dVg / AbulkCV;
              dVdsat_dVb = - (Vdsat * dAbulkCV_dVb + dVth_dVb)/ AbulkCV;

              if (m.xpart > 0.5)
                {
                /* 0/100 Charge partition model */
                if (Vdsat <= Vds)
                  {
                  /* saturation region */
                  T1 = Vdsat / 3.0;
                  qgate = CoxWL * (Vgs_eff - Vfb - s.phi - T1);
                  T2 = -Two_Third_CoxWL * Vgst;
                  qbulk = -(qgate + T2);
                  qdrn = 0.0;

                  cggb = One_Third_CoxWL * (3.0 - dVdsat_dVg) * dVgs_eff_dVg;
                  T2 = -One_Third_CoxWL * dVdsat_dVb;
                  cgsb = -(cggb + T2);
                  cgdb = 0.0;

                  cdgb = 0.0;
                  cddb = 0.0;
                  cdsb = 0.0;

                  cbgb = -(cggb - Two_Third_CoxWL * dVgs_eff_dVg);
                  T3 = -(T2 + Two_Third_CoxWL * dVth_dVb);
                  cbsb = -(cbgb + T3);
                  cbdb = 0.0;
                  qinv = -(qgate + qbulk);
                  }
                else
                  {
                  /* linear region */
                  Alphaz = Vgst / Vdsat;
                  T1 = 2.0 * Vdsat - Vds;
                  T2 = Vds / (3.0 * T1);
                  T3 = T2 * Vds;
                  T9 = 0.25 * CoxWL;
                  T4 = T9 * Alphaz;
                  T7 = 2.0 * Vds - T1 - 3.0 * T3;
                  T8 = T3 - T1 - 2.0 * Vds;
                  qgate = CoxWL * (Vgs_eff - Vfb - s.phi - 0.5 * (Vds - T3));
                  T10 = T4 * T8;
                  qdrn = T4 * T7;
                  qbulk = -(qgate + qdrn + T10);

                  T5 = T3 / T1;
                  cggb = CoxWL * (1.0 - T5 * dVdsat_dVg) * dVgs_eff_dVg;
                  T11 = -CoxWL * T5 * dVdsat_dVb;
                  cgdb = CoxWL * (T2 - 0.5 + 0.5 * T5);
                  cgsb = -(cggb + T11 + cgdb);
                  T6 = 1.0 / Vdsat;
                  dAlphaz_dVg = T6 * (1.0 - Alphaz * dVdsat_dVg);
                  dAlphaz_dVb = -T6 * (dVth_dVb + Alphaz * dVdsat_dVb);
                  T7 = T9 * T7;
                  T8 = T9 * T8;
                  T9 = 2.0 * T4 * (1.0 - 3.0 * T5);
                  cdgb = (T7 * dAlphaz_dVg - T9 * dVdsat_dVg) * dVgs_eff_dVg;
                  T12 = T7 * dAlphaz_dVb - T9 * dVdsat_dVb;
                  cddb = T4 * (3.0 - 6.0 * T2 - 3.0 * T5);
                  cdsb = -(cdgb + T12 + cddb);

                  T9 = 2.0 * T4 * (1.0 + T5);
                  T10 = (T8 * dAlphaz_dVg - T9 * dVdsat_dVg) * dVgs_eff_dVg;
                  T11 = T8 * dAlphaz_dVb - T9 * dVdsat_dVb;
                  T12 = T4 * (2.0 * T2 + T5 - 1.0);
                  T0 = -(T10 + T11 + T12);

                  cbgb = -(cggb + cdgb + T10);
                  cbdb = -(cgdb + cddb + T12);
                  cbsb = -(cgsb + cdsb + T0);
                  qinv = -(qgate + qbulk);
                  }
                }
              else if (m.xpart < 0.5)
                {
                /* 40/60 Charge partition model */
                if (Vds >= Vdsat)
                  {
                  /* saturation region */
                  T1 = Vdsat / 3.0;
                  qgate = CoxWL * (Vgs_eff - Vfb
                                   - s.phi - T1);
                  T2 = -Two_Third_CoxWL * Vgst;
                  qbulk = -(qgate + T2);
                  qdrn = 0.4 * T2;

                  cggb = One_Third_CoxWL * (3.0 - dVdsat_dVg) * dVgs_eff_dVg;
                  T2 = -One_Third_CoxWL * dVdsat_dVb;
                  cgsb = -(cggb + T2);
                  cgdb = 0.0;

                  T3 = 0.4 * Two_Third_CoxWL;
                  cdgb = -T3 * dVgs_eff_dVg;
                  cddb = 0.0;
                  T4 = T3 * dVth_dVb;
                  cdsb = -(T4 + cdgb);

                  cbgb = -(cggb - Two_Third_CoxWL * dVgs_eff_dVg);
                  T3 = -(T2 + Two_Third_CoxWL * dVth_dVb);
                  cbsb = -(cbgb + T3);
                  cbdb = 0.0;
                  qinv = -(qgate + qbulk);
                  }
                else
                  {
                  /* linear region  */
                  Alphaz = Vgst / Vdsat;
                  T1 = 2.0 * Vdsat - Vds;
                  T2 = Vds / (3.0 * T1);
                  T3 = T2 * Vds;
                  T9 = 0.25 * CoxWL;
                  T4 = T9 * Alphaz;
                  qgate = CoxWL * (Vgs_eff - Vfb - s.phi - 0.5 * (Vds - T3));

                  T5 = T3 / T1;
                  cggb = CoxWL * (1.0 - T5 * dVdsat_dVg) * dVgs_eff_dVg;
                  tmp = -CoxWL * T5 * dVdsat_dVb;
                  cgdb = CoxWL * (T2 - 0.5 + 0.5 * T5);
                  cgsb = -(cggb + cgdb + tmp);

                  T6 = 1.0 / Vdsat;
                  dAlphaz_dVg = T6 * (1.0 - Alphaz * dVdsat_dVg);
                  dAlphaz_dVb = -T6 * (dVth_dVb + Alphaz * dVdsat_dVb);

                  T6 = 8.0 * Vdsat * Vdsat - 6.0 * Vdsat * Vds + 1.2 * Vds * Vds;
                  T8 = T2 / T1;
                  T7 = Vds - T1 - T8 * T6;
                  qdrn = T4 * T7;
                  T7 *= T9;
                  tmp = T8 / T1;
                  tmp1 = T4 * (2.0 - 4.0 * tmp * T6 + T8 * (16.0 * Vdsat - 6.0 * Vds));

                  cdgb = (T7 * dAlphaz_dVg - tmp1 * dVdsat_dVg) * dVgs_eff_dVg;
                  T10 = T7 * dAlphaz_dVb - tmp1 * dVdsat_dVb;
                  cddb =  T4 * (2.0 - (1.0 / (3.0 * T1 * T1) + 2.0 * tmp) * T6 + T8
                                   * (6.0 * Vdsat - 2.4 * Vds));
                  cdsb = -(cdgb + T10 + cddb);

                  T7 = 2.0 * (T1 + T3);
                  qbulk = -(qgate - T4 * T7);
                  T7 *= T9;
                  T0 = 4.0 * T4 * (1.0 - T5);
                  T12 = (-T7 * dAlphaz_dVg - cdgb - T0 * dVdsat_dVg) * dVgs_eff_dVg;
                  T11 = -T7 * dAlphaz_dVb - T10 - T0 * dVdsat_dVb;
                  T10 = -4.0 * T4 * (T2 - 0.5 + 0.5 * T5) - cddb;
                  tmp = -(T10 + T11 + T12);

                  cbgb = -(cggb + cdgb + T12);
                  cbdb = -(cgdb + cddb + T11);
                  cbsb = -(cgsb + cdsb + tmp);
                  qinv = -(qgate + qbulk);
                  }
                }
              else
                {
                /* 50/50 partitioning */
                if (Vds >= Vdsat)
                  {
                  /* saturation region */
                  T1 = Vdsat / 3.0;
                  qgate = CoxWL * (Vgs_eff - Vfb - s.phi - T1);
                  T2 = -Two_Third_CoxWL * Vgst;
                  qbulk = -(qgate + T2);
                  qdrn = 0.5 * T2;

                  cggb = One_Third_CoxWL * (3.0 - dVdsat_dVg) * dVgs_eff_dVg;
                  T2 = -One_Third_CoxWL * dVdsat_dVb;
                  cgsb = -(cggb + T2);
                  cgdb = 0.0;

                  cdgb = -One_Third_CoxWL * dVgs_eff_dVg;
                  cddb = 0.0;
                  T4 = One_Third_CoxWL * dVth_dVb;
                  cdsb = -(T4 + cdgb);

                  cbgb = -(cggb - Two_Third_CoxWL * dVgs_eff_dVg);
                  T3 = -(T2 + Two_Third_CoxWL * dVth_dVb);
                  cbsb = -(cbgb + T3);
                  cbdb = 0.0;
                  qinv = -(qgate + qbulk);
                  }
                else
                  {
                  /* linear region */
                  Alphaz = Vgst / Vdsat;
                  T1 = 2.0 * Vdsat - Vds;
                  T2 = Vds / (3.0 * T1);
                  T3 = T2 * Vds;
                  T9 = 0.25 * CoxWL;
                  T4 = T9 * Alphaz;
                  qgate = CoxWL * (Vgs_eff - Vfb - s.phi - 0.5 * (Vds - T3));

                  T5 = T3 / T1;
                  cggb = CoxWL * (1.0 - T5 * dVdsat_dVg) * dVgs_eff_dVg;
                  tmp = -CoxWL * T5 * dVdsat_dVb;
                  cgdb = CoxWL * (T2 - 0.5 + 0.5 * T5);
                  cgsb = -(cggb + cgdb + tmp);

                  T6 = 1.0 / Vdsat;
                  dAlphaz_dVg = T6 * (1.0 - Alphaz * dVdsat_dVg);
                  dAlphaz_dVb = -T6 * (dVth_dVb + Alphaz * dVdsat_dVb);

                  T7 = T1 + T3;
                  qdrn = -T4 * T7;
                  qbulk = - (qgate + qdrn + qdrn);
                  T7 *= T9;
                  T0 = T4 * (2.0 * T5 - 2.0);

                  cdgb = (T0 * dVdsat_dVg - T7 * dAlphaz_dVg) * dVgs_eff_dVg;
                  T12 = T0 * dVdsat_dVb - T7 * dAlphaz_dVb;
                  cddb = T4 * (1.0 - 2.0 * T2 - T5);
                  cdsb = -(cdgb + T12 + cddb);

                  cbgb = -(cggb + 2.0 * cdgb);
                  cbdb = -(cgdb + 2.0 * cddb);
                  cbsb = -(cgsb + 2.0 * cdsb);
                  qinv = -(qgate + qbulk);
                  }
                }
              }
            }
          else if (m.xpart >= 0)
            {
            if (Vbseff < 0.0)
              {
              VbseffCV = Vbseff;
              dVbseffCV_dVb = 1.0;
              }
            else
              {
              VbseffCV = s.phi - Phis;
              dVbseffCV_dVb = -dPhis_dVb;
              }

            CoxWL = m.cox * s.weffCV * s.leffCV;

            /* Seperate VgsteffCV with noff and voffcv */
            noff = n * s.noff;
            dnoff_dVd = s.noff * dn_dVd;
            dnoff_dVb = s.noff * dn_dVb;
            T0 = Vtm * noff;
            voffcv = s.voffcv;
            VgstNVt = (Vgst - voffcv) / T0;

            if (VgstNVt > BSim3Model.EXP_THRESHOLD)
              {
              Vgsteff = Vgst - voffcv;
              dVgsteff_dVg = dVgs_eff_dVg;
              dVgsteff_dVd = -dVth_dVd;
              dVgsteff_dVb = -dVth_dVb;
              }
            else if (VgstNVt < -BSim3Model.EXP_THRESHOLD)
              {
              Vgsteff = T0 * Math.log(1.0 + BSim3Model.MIN_EXP);
              dVgsteff_dVg = 0.0;
              dVgsteff_dVd = Vgsteff / noff;
              dVgsteff_dVb = dVgsteff_dVd * dnoff_dVb;
              dVgsteff_dVd *= dnoff_dVd;
              }
            else
              {
              ExpVgst = Math.exp(VgstNVt);
              Vgsteff = T0 * Math.log(1.0 + ExpVgst);
              dVgsteff_dVg = ExpVgst / (1.0 + ExpVgst);
              dVgsteff_dVd = -dVgsteff_dVg * (dVth_dVd + (Vgst - voffcv)
                                              / noff * dnoff_dVd) + Vgsteff / noff * dnoff_dVd;
              dVgsteff_dVb = -dVgsteff_dVg * (dVth_dVb + (Vgst - voffcv)
                                              / noff * dnoff_dVb) + Vgsteff / noff * dnoff_dVb;
              dVgsteff_dVg *= dVgs_eff_dVg;
              } /* End of VgsteffCV */

            if (m.capMod == 1)
              {
              Vfb = s.vfbzb;
              Arg1 = Vgs_eff - VbseffCV - Vfb - Vgsteff;

              if (Arg1 <= 0.0)
                {
                qgate = CoxWL * Arg1;
                Cgg = CoxWL * (dVgs_eff_dVg - dVgsteff_dVg);
                Cgd = -CoxWL * dVgsteff_dVd;
                Cgb = -CoxWL * (dVbseffCV_dVb + dVgsteff_dVb);
                }
              else
                {
                T0 = 0.5 * s.k1ox;
                T1 = Math.sqrt(T0 * T0 + Arg1);
                T2 = CoxWL * T0 / T1;

                qgate = CoxWL * s.k1ox * (T1 - T0);

                Cgg = T2 * (dVgs_eff_dVg - dVgsteff_dVg);
                Cgd = -T2 * dVgsteff_dVd;
                Cgb = -T2 * (dVbseffCV_dVb + dVgsteff_dVb);
                }
              qbulk = -qgate;
              Cbg = -Cgg;
              Cbd = -Cgd;
              Cbb = -Cgb;

              One_Third_CoxWL = CoxWL / 3.0;
              Two_Third_CoxWL = 2.0 * One_Third_CoxWL;
              AbulkCV = Abulk0 * s.abulkCVfactor;
              dAbulkCV_dVb = s.abulkCVfactor * dAbulk0_dVb;
              VdsatCV = Vgsteff / AbulkCV;
              if (VdsatCV < Vds)
                {
                dVdsatCV_dVg = 1.0 / AbulkCV;
                dVdsatCV_dVb = -VdsatCV * dAbulkCV_dVb / AbulkCV;
                T0 = Vgsteff - VdsatCV / 3.0;
                dT0_dVg = 1.0 - dVdsatCV_dVg / 3.0;
                dT0_dVb = -dVdsatCV_dVb / 3.0;
                qgate += CoxWL * T0;
                Cgg1 = CoxWL * dT0_dVg;
                Cgb1 = CoxWL * dT0_dVb + Cgg1 * dVgsteff_dVb;
                Cgd1 = Cgg1 * dVgsteff_dVd;
                Cgg1 *= dVgsteff_dVg;
                Cgg += Cgg1;
                Cgb += Cgb1;
                Cgd += Cgd1;

                T0 = VdsatCV - Vgsteff;
                dT0_dVg = dVdsatCV_dVg - 1.0;
                dT0_dVb = dVdsatCV_dVb;
                qbulk += One_Third_CoxWL * T0;
                Cbg1 = One_Third_CoxWL * dT0_dVg;
                Cbb1 = One_Third_CoxWL * dT0_dVb + Cbg1 * dVgsteff_dVb;
                Cbd1 = Cbg1 * dVgsteff_dVd;
                Cbg1 *= dVgsteff_dVg;
                Cbg += Cbg1;
                Cbb += Cbb1;
                Cbd += Cbd1;

                if (m.xpart > 0.5)
                  T0 = -Two_Third_CoxWL;
                else if (m.xpart < 0.5)
                  T0 = -0.4 * CoxWL;
                else
                  T0 = -One_Third_CoxWL;

                qsrc = T0 * Vgsteff;
                Csg = T0 * dVgsteff_dVg;
                Csb = T0 * dVgsteff_dVb;
                Csd = T0 * dVgsteff_dVd;
                Cgb *= dVbseff_dVb;
                Cbb *= dVbseff_dVb;
                Csb *= dVbseff_dVb;
                }
              else
                {
                T0 = AbulkCV * Vds;
                T1 = 12.0 * (Vgsteff - 0.5 * T0 + 1.e-20);
                T2 = Vds / T1;
                T3 = T0 * T2;
                dT3_dVg = -12.0 * T2 * T2 * AbulkCV;
                dT3_dVd = 6.0 * T0 * (4.0 * Vgsteff - T0) / T1 / T1 - 0.5;
                dT3_dVb = 12.0 * T2 * T2 * dAbulkCV_dVb * Vgsteff;

                qgate += CoxWL * (Vgsteff - 0.5 * Vds + T3);
                Cgg1 = CoxWL * (1.0 + dT3_dVg);
                Cgb1 = CoxWL * dT3_dVb + Cgg1 * dVgsteff_dVb;
                Cgd1 = CoxWL * dT3_dVd + Cgg1 * dVgsteff_dVd;
                Cgg1 *= dVgsteff_dVg;
                Cgg += Cgg1;
                Cgb += Cgb1;
                Cgd += Cgd1;

                qbulk += CoxWL * (1.0 - AbulkCV) * (0.5 * Vds - T3);
                Cbg1 = -CoxWL * ((1.0 - AbulkCV) * dT3_dVg);
                Cbb1 = -CoxWL * ((1.0 - AbulkCV) * dT3_dVb + (0.5 * Vds - T3) * dAbulkCV_dVb)
                    + Cbg1 * dVgsteff_dVb;
                Cbd1 = -CoxWL * (1.0 - AbulkCV) * dT3_dVd + Cbg1 * dVgsteff_dVd;
                Cbg1 *= dVgsteff_dVg;
                Cbg += Cbg1;
                Cbb += Cbb1;
                Cbd += Cbd1;

                if (m.xpart > 0.5)
                  {
                  /* 0/100 Charge petition model */
                  T1 = T1 + T1;
                  qsrc = -CoxWL * (0.5 * Vgsteff + 0.25 * T0 - T0 * T0 / T1);
                  Csg = -CoxWL * (0.5 + 24.0 * T0 * Vds / T1 / T1 * AbulkCV);
                  Csb = -CoxWL * (0.25 * Vds * dAbulkCV_dVb
                                  - 12.0 * T0 * Vds / T1 / T1 * (4.0 * Vgsteff - T0)
                                  * dAbulkCV_dVb) + Csg * dVgsteff_dVb;
                  Csd = -CoxWL * (0.25 * AbulkCV - 12.0 * AbulkCV * T0
                                  / T1 / T1 * (4.0 * Vgsteff - T0)) + Csg * dVgsteff_dVd;
                  Csg *= dVgsteff_dVg;
                  }
                else if (m.xpart < 0.5)
                  {
                  /* 40/60 Charge petition model */
                  T1 = T1 / 12.0;
                  T2 = 0.5 * CoxWL / (T1 * T1);
                  T3 = Vgsteff * (2.0 * T0 * T0 / 3.0 + Vgsteff * (Vgsteff - 4.0 * T0 / 3.0))
                      - 2.0 * T0 * T0 * T0 / 15.0;
                  qsrc = -T2 * T3;
                  T4 = 4.0 / 3.0 * Vgsteff * (Vgsteff - T0) + 0.4 * T0 * T0;
                  Csg = -2.0 * qsrc / T1 - T2
                      * (Vgsteff * (3.0 * Vgsteff - 8.0 * T0 / 3.0) + 2.0 * T0 * T0 / 3.0);
                  Csb = (qsrc / T1 * Vds + T2 * T4 * Vds) * dAbulkCV_dVb
                      + Csg * dVgsteff_dVb;
                  Csd = (qsrc / T1 + T2 * T4) * AbulkCV + Csg * dVgsteff_dVd;
                  Csg *= dVgsteff_dVg;
                  }
                else
                  {
                  /* 50/50 Charge petition model */
                  qsrc = -0.5 * (qgate + qbulk);
                  Csg = -0.5 * (Cgg1 + Cbg1);
                  Csb = -0.5 * (Cgb1 + Cbb1);
                  Csd = -0.5 * (Cgd1 + Cbd1);
                  }
                Cgb *= dVbseff_dVb;
                Cbb *= dVbseff_dVb;
                Csb *= dVbseff_dVb;
                }
              qdrn = -(qgate + qbulk + qsrc);
              cggb = Cgg;
              cgsb = -(Cgg + Cgd + Cgb);
              cgdb = Cgd;
              cdgb = -(Cgg + Cbg + Csg);
              cdsb = (Cgg + Cgd + Cgb + Cbg + Cbd + Cbb + Csg + Csd + Csb);
              cddb = -(Cgd + Cbd + Csd);
              cbgb = Cbg;
              cbsb = -(Cbg + Cbd + Cbb);
              cbdb = Cbd;
              qinv = -(qgate + qbulk);
              }

            else if (m.capMod == 2)
              {
              Vfb = s.vfbzb;
              V3 = Vfb - Vgs_eff + VbseffCV - BSim3Model.DELTA_3;
              if (Vfb <= 0.0)
                {
                T0 = Math.sqrt(V3 * V3 - 4.0 * BSim3Model.DELTA_3 * Vfb);
                T2 = -BSim3Model.DELTA_3 / T0;
                }
              else
                {
                T0 = Math.sqrt(V3 * V3 + 4.0 * BSim3Model.DELTA_3 * Vfb);
                T2 = BSim3Model.DELTA_3 / T0;
                }

              T1 = 0.5 * (1.0 + V3 / T0);
              Vfbeff = Vfb - 0.5 * (V3 + T0);
              dVfbeff_dVg = T1 * dVgs_eff_dVg;
              dVfbeff_dVb = -T1 * dVbseffCV_dVb;
              Qac0 = CoxWL * (Vfbeff - Vfb);
              dQac0_dVg = CoxWL * dVfbeff_dVg;
              dQac0_dVb = CoxWL * dVfbeff_dVb;

              T0 = 0.5 * s.k1ox;
              T3 = Vgs_eff - Vfbeff - VbseffCV - Vgsteff;
              if (s.k1ox == 0.0)
                {
                T1 = 0.0;
                T2 = 0.0;
                }
              else if (T3 < 0.0)
                {
                T1 = T0 + T3 / s.k1ox;
                T2 = CoxWL;
                }
              else
                {
                T1 = Math.sqrt(T0 * T0 + T3);
                T2 = CoxWL * T0 / T1;
                }

              Qsub0 = CoxWL * s.k1ox * (T1 - T0);

              dQsub0_dVg = T2 * (dVgs_eff_dVg - dVfbeff_dVg - dVgsteff_dVg);
              dQsub0_dVd = -T2 * dVgsteff_dVd;
              dQsub0_dVb = -T2 * (dVfbeff_dVb + dVbseffCV_dVb + dVgsteff_dVb);

              AbulkCV = Abulk0 * s.abulkCVfactor;
              dAbulkCV_dVb = s.abulkCVfactor * dAbulk0_dVb;
              VdsatCV = Vgsteff / AbulkCV;

              V4 = VdsatCV - Vds - BSim3Model.DELTA_4;
              T0 = Math.sqrt(V4 * V4 + 4.0 * BSim3Model.DELTA_4 * VdsatCV);
              VdseffCV = VdsatCV - 0.5 * (V4 + T0);
              T1 = 0.5 * (1.0 + V4 / T0);
              T2 = BSim3Model.DELTA_4 / T0;
              T3 = (1.0 - T1 - T2) / AbulkCV;
              dVdseffCV_dVg = T3;
              dVdseffCV_dVd = T1;
              dVdseffCV_dVb = -T3 * VdsatCV * dAbulkCV_dVb;
              if (Vds == 0.0)
                {
                /* Added to eliminate non-zero VdseffCV at Vds=0.0 */
                VdseffCV = 0.0;
                dVdseffCV_dVg = 0.0;
                dVdseffCV_dVb = 0.0;
                }

              T0 = AbulkCV * VdseffCV;
              T1 = 12.0 * (Vgsteff - 0.5 * T0 + 1e-20);
              T2 = VdseffCV / T1;
              T3 = T0 * T2;

              T4 = (1.0 - 12.0 * T2 * T2 * AbulkCV);
              T5 = (6.0 * T0 * (4.0 * Vgsteff - T0) / (T1 * T1) - 0.5);
              T6 = 12.0 * T2 * T2 * Vgsteff;

              qinoi = -CoxWL * (Vgsteff - 0.5 * T0 + AbulkCV * T3);
              qgate = CoxWL * (Vgsteff - 0.5 * VdseffCV + T3);
              Cgg1 = CoxWL * (T4 + T5 * dVdseffCV_dVg);
              Cgd1 = CoxWL * T5 * dVdseffCV_dVd + Cgg1 * dVgsteff_dVd;
              Cgb1 = CoxWL * (T5 * dVdseffCV_dVb + T6 * dAbulkCV_dVb) + Cgg1 * dVgsteff_dVb;
              Cgg1 *= dVgsteff_dVg;

              T7 = 1.0 - AbulkCV;
              qbulk = CoxWL * T7 * (0.5 * VdseffCV - T3);
              T4 = -T7 * (T4 - 1.0);
              T5 = -T7 * T5;
              T6 = -(T7 * T6 + (0.5 * VdseffCV - T3));
              Cbg1 = CoxWL * (T4 + T5 * dVdseffCV_dVg);
              Cbd1 = CoxWL * T5 * dVdseffCV_dVd + Cbg1 * dVgsteff_dVd;
              Cbb1 = CoxWL * (T5 * dVdseffCV_dVb + T6 * dAbulkCV_dVb) + Cbg1 * dVgsteff_dVb;
              Cbg1 *= dVgsteff_dVg;

              if (m.xpart > 0.5)
                {
                /* 0/100 Charge petition model */
                T1 = T1 + T1;
                qsrc = -CoxWL * (0.5 * Vgsteff + 0.25 * T0 - T0 * T0 / T1);
                T7 = (4.0 * Vgsteff - T0) / (T1 * T1);
                T4 = -(0.5 + 24.0 * T0 * T0 / (T1 * T1));
                T5 = -(0.25 * AbulkCV - 12.0 * AbulkCV * T0 * T7);
                T6 = -(0.25 * VdseffCV - 12.0 * T0 * VdseffCV * T7);
                Csg = CoxWL * (T4 + T5 * dVdseffCV_dVg);
                Csd = CoxWL * T5 * dVdseffCV_dVd + Csg * dVgsteff_dVd;
                Csb = CoxWL * (T5 * dVdseffCV_dVb + T6 * dAbulkCV_dVb) + Csg * dVgsteff_dVb;
                Csg *= dVgsteff_dVg;
                }
              else if (m.xpart < 0.5)
                {
                /* 40/60 Charge petition model */
                T1 = T1 / 12.0;
                T2 = 0.5 * CoxWL / (T1 * T1);
                T3 = Vgsteff * (2.0 * T0 * T0 / 3.0 + Vgsteff * (Vgsteff - 4.0 * T0 / 3.0))
                    - 2.0 * T0 * T0 * T0 / 15.0;
                qsrc = -T2 * T3;
                T7 = 4.0 / 3.0 * Vgsteff * (Vgsteff - T0) + 0.4 * T0 * T0;
                T4 = -2.0 * qsrc / T1 - T2 * (Vgsteff * (3.0 * Vgsteff - 8.0 * T0 / 3.0)
                                              + 2.0 * T0 * T0 / 3.0);
                T5 = (qsrc / T1 + T2 * T7) * AbulkCV;
                T6 = (qsrc / T1 * VdseffCV + T2 * T7 * VdseffCV);
                Csg = (T4 + T5 * dVdseffCV_dVg);
                Csd = T5 * dVdseffCV_dVd + Csg * dVgsteff_dVd;
                Csb = (T5 * dVdseffCV_dVb + T6 * dAbulkCV_dVb) + Csg * dVgsteff_dVb;
                Csg *= dVgsteff_dVg;
                }
              else
                {
                /* 50/50 Charge petition model */
                qsrc = -0.5 * (qgate + qbulk);
                Csg = -0.5 * (Cgg1 + Cbg1);
                Csb = -0.5 * (Cgb1 + Cbb1);
                Csd = -0.5 * (Cgd1 + Cbd1);
                }

              qgate += Qac0 + Qsub0;
              qbulk -= (Qac0 + Qsub0);
              qdrn = -(qgate + qbulk + qsrc);

              Cgg = dQac0_dVg + dQsub0_dVg + Cgg1;
              Cgd = dQsub0_dVd + Cgd1;
              Cgb = dQac0_dVb + dQsub0_dVb + Cgb1;

              Cbg = Cbg1 - dQac0_dVg - dQsub0_dVg;
              Cbd = Cbd1 - dQsub0_dVd;
              Cbb = Cbb1 - dQac0_dVb - dQsub0_dVb;

              Cgb *= dVbseff_dVb;
              Cbb *= dVbseff_dVb;
              Csb *= dVbseff_dVb;

              cggb = Cgg;
              cgsb = -(Cgg + Cgd + Cgb);
              cgdb = Cgd;
              cdgb = -(Cgg + Cbg + Csg);
              cdsb = (Cgg + Cgd + Cgb + Cbg + Cbd + Cbb + Csg + Csd + Csb);
              cddb = -(Cgd + Cbd + Csd);
              cbgb = Cbg;
              cbsb = -(Cbg + Cbd + Cbb);
              cbdb = Cbd;
              qinv = qinoi;
              }

            /* New Charge-Thickness capMod (CTM) begins */
            else if (m.capMod == 3)
              {
              V3 = s.vfbzb - Vgs_eff + VbseffCV - BSim3Model.DELTA_3;
              if (s.vfbzb <= 0.0)
                {
                T0 = Math.sqrt(V3 * V3 - 4.0 * BSim3Model.DELTA_3 * s.vfbzb);
                T2 = -BSim3Model.DELTA_3 / T0;
                }
              else
                {
                T0 = Math.sqrt(V3 * V3 + 4.0 * BSim3Model.DELTA_3 * s.vfbzb);
                T2 = BSim3Model.DELTA_3 / T0;
                }

              T1 = 0.5 * (1.0 + V3 / T0);
              Vfbeff = s.vfbzb - 0.5 * (V3 + T0);
              dVfbeff_dVg = T1 * dVgs_eff_dVg;
              dVfbeff_dVb = -T1 * dVbseffCV_dVb;

              Cox = m.cox;
              Tox = 1.0e8 * m.tox;
              T0 = (Vgs_eff - VbseffCV - s.vfbzb) / Tox;
              dT0_dVg = dVgs_eff_dVg / Tox;
              dT0_dVb = -dVbseffCV_dVb / Tox;

              tmp = T0 * s.acde;
              if ((-BSim3Model.EXP_THRESHOLD < tmp) && (tmp < BSim3Model.EXP_THRESHOLD))
                {
                Tcen = s.ldeb * Math.exp(tmp);
                dTcen_dVg = s.acde * Tcen;
                dTcen_dVb = dTcen_dVg * dT0_dVb;
                dTcen_dVg *= dT0_dVg;
                }
              else if (tmp <= -BSim3Model.EXP_THRESHOLD)
                {
                Tcen = s.ldeb * BSim3Model.MIN_EXP;
                dTcen_dVg = dTcen_dVb = 0.0;
                }
              else
                {
                Tcen = s.ldeb * BSim3Model.MAX_EXP;
                dTcen_dVg = dTcen_dVb = 0.0;
                }

              LINK = 1.0e-3 * m.tox;
              V3 = s.ldeb - Tcen - LINK;
              V4 = Math.sqrt(V3 * V3 + 4.0 * LINK * s.ldeb);
              Tcen = s.ldeb - 0.5 * (V3 + V4);
              T1 = 0.5 * (1.0 + V3 / V4);
              dTcen_dVg *= T1;
              dTcen_dVb *= T1;

              Ccen = BSim3Model.EPSSI / Tcen;
              T2 = Cox / (Cox + Ccen);
              Coxeff = T2 * Ccen;
              T3 = -Ccen / Tcen;
              dCoxeff_dVg = T2 * T2 * T3;
              dCoxeff_dVb = dCoxeff_dVg * dTcen_dVb;
              dCoxeff_dVg *= dTcen_dVg;
              CoxWLcen = CoxWL * Coxeff / Cox;

              Qac0 = CoxWLcen * (Vfbeff - s.vfbzb);
              QovCox = Qac0 / Coxeff;
              dQac0_dVg = CoxWLcen * dVfbeff_dVg + QovCox * dCoxeff_dVg;
              dQac0_dVb = CoxWLcen * dVfbeff_dVb + QovCox * dCoxeff_dVb;

              T0 = 0.5 * s.k1ox;
              T3 = Vgs_eff - Vfbeff - VbseffCV - Vgsteff;
              if (s.k1ox == 0.0)
                {
                T1 = 0.0;
                T2 = 0.0;
                }
              else if (T3 < 0.0)
                {
                T1 = T0 + T3 / s.k1ox;
                T2 = CoxWLcen;
                }
              else
                {
                T1 = Math.sqrt(T0 * T0 + T3);
                T2 = CoxWLcen * T0 / T1;
                }

              Qsub0 = CoxWLcen * s.k1ox * (T1 - T0);
              QovCox = Qsub0 / Coxeff;
              dQsub0_dVg = T2 * (dVgs_eff_dVg - dVfbeff_dVg - dVgsteff_dVg)
                  + QovCox * dCoxeff_dVg;
              dQsub0_dVd = -T2 * dVgsteff_dVd;
              dQsub0_dVb = -T2 * (dVfbeff_dVb + dVbseffCV_dVb + dVgsteff_dVb)
                  + QovCox * dCoxeff_dVb;

              /* Gate-bias dependent delta Phis begins */
              if (s.k1ox <= 0.0)
                {
                Denomi = 0.25 * s.moin * Vtm;
                T0 = 0.5 * s.sqrtPhi;
                }
              else
                {
                Denomi = s.moin * Vtm
                    * s.k1ox * s.k1ox;
                T0 = s.k1ox * s.sqrtPhi;
                }
              T1 = 2.0 * T0 + Vgsteff;

              DeltaPhi = Vtm * Math.log(1.0 + T1 * Vgsteff / Denomi);
              dDeltaPhi_dVg = 2.0 * Vtm * (T1 -T0) / (Denomi + T1 * Vgsteff);
              dDeltaPhi_dVd = dDeltaPhi_dVg * dVgsteff_dVd;
              dDeltaPhi_dVb = dDeltaPhi_dVg * dVgsteff_dVb;
              /* End of delta Phis */

              T3 = 4.0 * (Vth - s.vfbzb - s.phi);
              Tox += Tox;
              if (T3 >= 0.0)
                {
                T0 = (Vgsteff + T3) / Tox;
                dT0_dVd = (dVgsteff_dVd + 4.0 * dVth_dVd) / Tox;
                dT0_dVb = (dVgsteff_dVb + 4.0 * dVth_dVb) / Tox;
                }
              else
                {
                T0 = (Vgsteff + 1.0e-20) / Tox;
                dT0_dVd = dVgsteff_dVd / Tox;
                dT0_dVb = dVgsteff_dVb / Tox;
                }
              tmp = Math.exp(0.7 * Math.log(T0));
              T1 = 1.0 + tmp;
              T2 = 0.7 * tmp / (T0 * Tox);
              Tcen = 1.9e-9 / T1;
              dTcen_dVg = -1.9e-9 * T2 / T1 /T1;
              dTcen_dVd = Tox * dTcen_dVg;
              dTcen_dVb = dTcen_dVd * dT0_dVb;
              dTcen_dVd *= dT0_dVd;
              dTcen_dVg *= dVgsteff_dVg;

              Ccen = BSim3Model.EPSSI / Tcen;
              T0 = Cox / (Cox + Ccen);
              Coxeff = T0 * Ccen;
              T1 = -Ccen / Tcen;
              dCoxeff_dVg = T0 * T0 * T1;
              dCoxeff_dVd = dCoxeff_dVg * dTcen_dVd;
              dCoxeff_dVb = dCoxeff_dVg * dTcen_dVb;
              dCoxeff_dVg *= dTcen_dVg;
              CoxWLcen = CoxWL * Coxeff / Cox;

              AbulkCV = Abulk0 * s.abulkCVfactor;
              dAbulkCV_dVb = s.abulkCVfactor * dAbulk0_dVb;
              VdsatCV = (Vgsteff - DeltaPhi) / AbulkCV;
              V4 = VdsatCV - Vds - BSim3Model.DELTA_4;
              T0 = Math.sqrt(V4 * V4 + 4.0 * BSim3Model.DELTA_4 * VdsatCV);
              VdseffCV = VdsatCV - 0.5 * (V4 + T0);
              T1 = 0.5 * (1.0 + V4 / T0);
              T2 = BSim3Model.DELTA_4 / T0;
              T3 = (1.0 - T1 - T2) / AbulkCV;
              T4 = T3 * ( 1.0 - dDeltaPhi_dVg);
              dVdseffCV_dVg = T4;
              dVdseffCV_dVd = T1;
              dVdseffCV_dVb = -T3 * VdsatCV * dAbulkCV_dVb;
              if (Vds == 0.0)
                {
                /* Added to eliminate non-zero VdseffCV at Vds=0.0 */
                VdseffCV = 0.0;
                dVdseffCV_dVg = 0.0;
                dVdseffCV_dVb = 0.0;
                }

              T0 = AbulkCV * VdseffCV;
              T1 = Vgsteff - DeltaPhi;
              T2 = 12.0 * (T1 - 0.5 * T0 + 1.0e-20);
              T3 = T0 / T2;
              T4 = 1.0 - 12.0 * T3 * T3;
              T5 = AbulkCV * (6.0 * T0 * (4.0 * T1 - T0) / (T2 * T2) - 0.5);
              T6 = T5 * VdseffCV / AbulkCV;

              qgate = qinoi = CoxWLcen * (T1 - T0 * (0.5 - T3));
              QovCox = qgate / Coxeff;
              Cgg1 = CoxWLcen * (T4 * (1.0 - dDeltaPhi_dVg) + T5 * dVdseffCV_dVg);
              Cgd1 = CoxWLcen * T5 * dVdseffCV_dVd + Cgg1
                  * dVgsteff_dVd + QovCox * dCoxeff_dVd;
              Cgb1 = CoxWLcen * (T5 * dVdseffCV_dVb + T6 * dAbulkCV_dVb)
                  + Cgg1 * dVgsteff_dVb + QovCox * dCoxeff_dVb;
              Cgg1 = Cgg1 * dVgsteff_dVg + QovCox * dCoxeff_dVg;

              T7 = 1.0 - AbulkCV;
              T8 = T2 * T2;
              T9 = 12.0 * T7 * T0 * T0 / (T8 * AbulkCV);
              T10 = T9 * (1.0 - dDeltaPhi_dVg);
              T11 = -T7 * T5 / AbulkCV;
              T12 = -(T9 * T1 / AbulkCV + VdseffCV * (0.5 - T0 / T2));

              qbulk = CoxWLcen * T7 * (0.5 * VdseffCV - T0 * VdseffCV / T2);
              QovCox = qbulk / Coxeff;
              Cbg1 = CoxWLcen * (T10 + T11 * dVdseffCV_dVg);
              Cbd1 = CoxWLcen * T11 * dVdseffCV_dVd + Cbg1 * dVgsteff_dVd + QovCox * dCoxeff_dVd;
              Cbb1 = CoxWLcen * (T11 * dVdseffCV_dVb + T12 * dAbulkCV_dVb)
                  + Cbg1 * dVgsteff_dVb + QovCox * dCoxeff_dVb;
              Cbg1 = Cbg1 * dVgsteff_dVg + QovCox * dCoxeff_dVg;

              if (m.xpart > 0.5)
                {
                /* 0/100 partition */
                qsrc = -CoxWLcen * (T1 / 2.0 + T0 / 4.0 - 0.5 * T0 * T0 / T2);
                QovCox = qsrc / Coxeff;
                T2 += T2;
                T3 = T2 * T2;
                T7 = -(0.25 - 12.0 * T0 * (4.0 * T1 - T0) / T3);
                T4 = -(0.5 + 24.0 * T0 * T0 / T3) * (1.0 - dDeltaPhi_dVg);
                T5 = T7 * AbulkCV;
                T6 = T7 * VdseffCV;

                Csg = CoxWLcen * (T4 + T5 * dVdseffCV_dVg);
                Csd = CoxWLcen * T5 * dVdseffCV_dVd + Csg * dVgsteff_dVd + QovCox * dCoxeff_dVd;
                Csb = CoxWLcen * (T5 * dVdseffCV_dVb + T6 * dAbulkCV_dVb)
                    + Csg * dVgsteff_dVb + QovCox * dCoxeff_dVb;
                Csg = Csg * dVgsteff_dVg + QovCox * dCoxeff_dVg;
                }
              else if (m.xpart < 0.5)
                {
                /* 40/60 partition */
                T2 = T2 / 12.0;
                T3 = 0.5 * CoxWLcen / (T2 * T2);
                T4 = T1 * (2.0 * T0 * T0 / 3.0 + T1 * (T1 - 4.0 * T0 / 3.0))
                    - 2.0 * T0 * T0 * T0 / 15.0;
                qsrc = -T3 * T4;
                QovCox = qsrc / Coxeff;
                T8 = 4.0 / 3.0 * T1 * (T1 - T0) + 0.4 * T0 * T0;
                T5 = -2.0 * qsrc / T2 - T3 * (T1 * (3.0 * T1 - 8.0 * T0 / 3.0)
                                              + 2.0 * T0 * T0 / 3.0);
                T6 = AbulkCV * (qsrc / T2 + T3 * T8);
                T7 = T6 * VdseffCV / AbulkCV;

                Csg = T5 * (1.0 - dDeltaPhi_dVg) + T6 * dVdseffCV_dVg;
                Csd = Csg * dVgsteff_dVd + T6 * dVdseffCV_dVd + QovCox * dCoxeff_dVd;
                Csb = Csg * dVgsteff_dVb + T6 * dVdseffCV_dVb
                    + T7 * dAbulkCV_dVb + QovCox * dCoxeff_dVb;
                Csg = Csg * dVgsteff_dVg + QovCox * dCoxeff_dVg;
                }
              else
                {
                /* 50/50 partition */
                qsrc = -0.5 * qgate;
                Csg = -0.5 * Cgg1;
                Csd = -0.5 * Cgd1;
                Csb = -0.5 * Cgb1;
                }

              qgate += Qac0 + Qsub0 - qbulk;
              qbulk -= (Qac0 + Qsub0);
              qdrn = -(qgate + qbulk + qsrc);

              Cbg = Cbg1 - dQac0_dVg - dQsub0_dVg;
              Cbd = Cbd1 - dQsub0_dVd;
              Cbb = Cbb1 - dQac0_dVb - dQsub0_dVb;

              Cgg = Cgg1 - Cbg;
              Cgd = Cgd1 - Cbd;
              Cgb = Cgb1 - Cbb;

              Cgb *= dVbseff_dVb;
              Cbb *= dVbseff_dVb;
              Csb *= dVbseff_dVb;

              cggb = Cgg;
              cgsb = -(Cgg + Cgd + Cgb);
              cgdb = Cgd;
              cdgb = -(Cgg + Cbg + Csg);
              cdsb = (Cgg + Cgd + Cgb + Cbg + Cbd + Cbb + Csg + Csd + Csb);
              cddb = -(Cgd + Cbd + Csd);
              cbgb = Cbg;
              cbsb = -(Cbg + Cbd + Cbb);
              cbdb = Cbd;
              qinv = -qinoi;
              }  /* End of CTM */
            }

          /* bulk and channel charge plus overlaps */
          if (m.capMod == 0)
            {
            if (vgd < 0.0)
              {
              cgdo = s.cgdo;
              qgdo = s.cgdo * vgd;
              }
            else
              {
              cgdo = s.cgdo;
              qgdo =  s.cgdo * vgd;
              }

            if (vgs < 0.0)
              {
              cgso = s.cgso;
              qgso = s.cgso * vgs;
              }
            else
              {
              cgso = s.cgso;
              qgso = s.cgso * vgs;
              }
            }
          else if (m.capMod == 1)
            {
            if (vgd < 0.0)
              {
              T1 = Math.sqrt(1.0 - 4.0 * vgd / s.ckappa);
              cgdo = s.cgdo + s.weffCV * s.cgdl / T1;
              qgdo = s.cgdo * vgd - s.weffCV * 0.5 * s.cgdl * s.ckappa * (T1 - 1.0);
              }
            else
              {
              cgdo = s.cgdo + s.weffCV * s.cgdl;
              qgdo = (s.weffCV * s.cgdl + s.cgdo) * vgd;
              }

            if (vgs < 0.0)
              {
              T1 = Math.sqrt(1.0 - 4.0 * vgs / s.ckappa);
              cgso = s.cgso + s.weffCV * s.cgsl / T1;
              qgso = s.cgso * vgs - s.weffCV * 0.5 * s.cgsl * s.ckappa * (T1 - 1.0);
              }
            else
              {
              cgso = s.cgso + s.weffCV * s.cgsl;
              qgso = (s.weffCV * s.cgsl + s.cgso) * vgs;
              }
            }
          else
            {
            T0 = vgd + BSim3Model.DELTA_1;
            T1 = Math.sqrt(T0 * T0 + 4.0 * BSim3Model.DELTA_1);
            T2 = 0.5 * (T0 - T1);

            T3 = s.weffCV * s.cgdl;
            T4 = Math.sqrt(1.0 - 4.0 * T2 / s.ckappa);
            cgdo = s.cgdo + T3 - T3 * (1.0 - 1.0 / T4)	* (0.5 - 0.5 * T0 / T1);
            qgdo = (s.cgdo + T3) * vgd - T3 * (T2 + 0.5 * s.ckappa * (T4 - 1.0));

            T0 = vgs + BSim3Model.DELTA_1;
            T1 = Math.sqrt(T0 * T0 + 4.0 * BSim3Model.DELTA_1);
            T2 = 0.5 * (T0 - T1);
            T3 = s.weffCV * s.cgsl;
            T4 = Math.sqrt(1.0 - 4.0 * T2 / s.ckappa);
            cgso = s.cgso + T3 - T3 * (1.0 - 1.0 / T4) * (0.5 - 0.5 * T0 / T1);
            qgso = (s.cgso + T3) * vgs - T3 * (T2 + 0.5 * s.ckappa * (T4 - 1.0));
            }

        /*** add up capacitances and charges ***/
        gcggb = cggb + cgdo + cgso + s.cgbo;
        gcgdb = cgdb - cgdo;
        gcgsb = cgsb - cgso;

        gcdgb = cdgb - cgdo;
        gcddb = cddb + cgdo;
        gcdsb = cdsb;

        gcsgb = -(cggb + cbgb + cdgb + cgso);
        gcsdb = -(cgdb + cbdb + cddb);
        gcssb = cgso - (cgsb + cbsb + cdsb);

        gcbgb = cbgb - s.cgbo;
        gcbdb = cbdb;
        gcbsb = cbsb;

        qgd = qgdo;
        qgs = qgso;
        qgb = s.cgbo * vgb;
        qgate += qgd + qgs + qgb;
        qbulk -= qgb;
        qdrn -= qgd;
        qsrc = -(qgate + qbulk + qdrn);

        drainI = -m.type*(Ids+Isub);
        gateI = 0;
        sourceI = m.type*Ids;
        bulkI = m.type*Isub;
        source.setResult(chargeScale, m.type*qsrc, currentScale, sourceI);
        drain.setResult(chargeScale, m.type*qdrn,
                        currentScale, drainI);
        gate.setResult(chargeScale, m.type*qgate, currentScale, gateI);
        bulk.setResult(chargeScale, m.type*qbulk, currentScale, bulkI);

        source.setMatrix(source, derivChargeScale, gcssb,
                                 derivCurrentScale, -Gds - Gm - Gmb);
        source.setMatrix(drain,  derivChargeScale, gcdsb,
                                 derivCurrentScale, Gds+Gm+Gmb+Gbd+Gbg+Gbb);
        source.setMatrix(gate,   derivChargeScale, gcgsb,
                                 derivCurrentScale, 0);
        source.setMatrix(bulk,   derivChargeScale, gcbsb,
                                 derivCurrentScale, -Gbd - Gbg - Gbb);

        drain.setMatrix(source,  derivChargeScale, gcsdb,
                                 derivCurrentScale, Gds);
        drain.setMatrix(drain,   derivChargeScale, gcddb,
                                 derivCurrentScale, -Gds - Gbd);
        drain.setMatrix(gate,    derivChargeScale, gcgdb,
                                 derivCurrentScale, 0);
        drain.setMatrix(bulk,    derivChargeScale, gcbdb,
                                 derivCurrentScale, Gbd);

        gate.setMatrix(source,   derivChargeScale, gcsgb,
                                 derivCurrentScale, Gm);
        gate.setMatrix(drain,    derivChargeScale, gcdgb,
                                 derivCurrentScale, -Gm - Gbg);
        gate.setMatrix(gate,     derivChargeScale, gcggb,
                                 derivCurrentScale, 0);
        gate.setMatrix(bulk,     derivChargeScale, gcbgb,
                                 derivCurrentScale, Gbg);

        bulk.setMatrix(source,   derivChargeScale, -gcssb - gcsdb - gcsgb,
                                 derivCurrentScale, Gmb);
        bulk.setMatrix(drain,    derivChargeScale, -gcdsb - gcddb - gcdgb,
                                 derivCurrentScale, -Gmb - Gbb);
        bulk.setMatrix(gate,     derivChargeScale, -gcgsb - gcgdb - gcggb,
                                 derivCurrentScale, 0);
        bulk.setMatrix(bulk,     derivChargeScale, -gcbsb - gcbdb - gcbgb,
                                 derivCurrentScale, Gbb);
        //Used to debug native models vs java handcoded models
        if (BSim3Model.both && BSim3Model.isNativeLoaded()) {
            System.out.println(time+"\t"+(m.type*qsrc)+"\t"+(m.type*qdrn)+"\t"+
                               (m.type*qgate)+"\t"+(m.type*qbulk)+"\t"+
                               sourceI+"\t"+drainI+"\t"+
                               gateI+"\t"+bulkI+"\t"+
                               gcssb+"\t"+(-Gds - Gm - Gmb)+"\t"+
                               gcsdb+"\t"+(Gds)+"\t"+
                               gcsgb+"\t"+(Gm)+"\t"+
                               (-gcssb - gcsdb - gcsgb)+"\t"+(Gmb)+"\t"+
                               (gcdsb)+"\t"+(Gds+Gm+Gmb+Gbd+Gbg+Gbb)+"\t"+
                               (gcddb)+"\t"+(-Gds - Gbd)+"\t"+
                               (gcdgb)+"\t"+(-Gm - Gbg)+"\t"+
                               (-gcdsb - gcddb - gcdgb)+"\t"+(-gcdsb - gcddb - gcdgb)+"\t"+
                               
                               //Q[0] = 26th
                               Q[0]+"\t"+Q[1]+"\t"+Q[2]+"\t"+Q[3]+"\t"+
                               I[nS]+"\t"+I[nD]+"\t"+I[2]+"\t"+I[3]+"\t"+
                               dQ[0]+"\t"+dI[4*nS+nS]+"\t"+
                               dQ[1]+"\t"+dI[4*nS+nD]+"\t"+
                               dQ[2]+"\t"+dI[4*nS+2]+"\t"+
                               dQ[3]+"\t"+dI[4*nS+3]+"\t"+
                               dQ[4]+"\t"+dI[4*nD+nS]+"\t"+
                               dQ[5]+"\t"+dI[4*nD+nD]+"\t"+
                               dQ[6]+"\t"+dI[4*nD+2]+"\t"+
                               dQ[7]+"\t"+dI[4*nD+3]+"\t"+
                               //dQ[3]+"\t"+dI[3]+"\t"+
                               //s/d = 50 
                               nS+"\t"+nD+"\t"+
                               getName());
        }
    }
}
