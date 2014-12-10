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


public class SizedModel {
    public double Width;
    public double Length;

    /* W,L adjusted parameters */
    public double cdsc;
    public double cdscb;
    public double cdscd;
    public double cit;
    public double nfactor;
    public double xj;
    public double vsat;
    public double at;
    public double a0;
    public double ags;
    public double a1;
    public double a2;
    public double keta;
    public double nsub;
    public double npeak;
    public double ngate;
    public double gamma1;
    public double gamma2;
    public double vbx;
    public double vbi;
    public double vbm;
    public double vbsc;
    public double xt;
    public double phi;
    public double litl;
    public double k1;
    public double kt1;
    public double kt1l;
    public double kt2;
    public double k2;
    public double k3;
    public double k3b;
    public double w0;
    public double nlx;
    public double dvt0;
    public double dvt1;
    public double dvt2;
    public double dvt0w;
    public double dvt1w;
    public double dvt2w;
    public double drout;
    public double dsub;
    public double vth0;
    public double ua;
    public double ua1;
    public double ub;
    public double ub1;
    public double uc;
    public double uc1;
    public double u0;
    public double ute;
    public double voff;
    public double vfb;
    public double delta;
    public double rdsw;
    public double rds0;
    public double prwg;
    public double prwb;
    public double prt;
    public double eta0;
    public double etab;
    public double pclm;
    public double pdibl1;
    public double pdibl2;
    public double pdiblb;
    public double pscbe1;
    public double pscbe2;
    public double pvag;
    public double wr;
    public double dwg;
    public double dwb;
    public double b0;
    public double b1;
    public double alpha0;
    public double alpha1;
    public double beta0;

    /* CV model */
    public double elm;
    public double cgsl;
    public double cgdl;
    public double ckappa;
    public double cf;
    public double clc;
    public double cle;
    public double vfbcv;
    public double noff;
    public double voffcv;
    public double acde;
    public double moin;

    /* Pre-calculated constants */
    public double dw;
    public double dl;
    public double leff;
    public double weff;

    public double dwc;
    public double dlc;
    public double leffCV;
    public double weffCV;
    public double abulkCVfactor;
    public double cgso;
    public double cgdo;
    public double cgbo;
    public double tconst;

    public double u0temp;
    public double vsattemp;
    public double sqrtPhi;
    public double phis3;
    public double Xdep0;
    public double sqrtXdep0;
    public double theta0vb0;
    public double thetaRout;

    public double cof1;
    public double cof2;
    public double cof3;
    public double cof4;
    public double cdep0;
    public double vfbzb;
    public double ldeb;
    public double k1ox;
    public double k2ox;

    private static SizedModel cachedSizedModel = null;
    private static double lastWidth = Double.NaN;
    private static double lastLength = Double.NaN;
    private static BSim3Model lastModel = null;

    public static SizedModel newSizedModel(BSim3Model m, double W, double L) {
        if (m != lastModel || W != lastWidth || L != lastLength ||
            cachedSizedModel == null)
        {
            lastModel = m;
            lastWidth = W;
            lastLength = L;
            cachedSizedModel = new SizedModel(m, W, L);
        }

        return cachedSizedModel;
    }

    /**
     * @throws IllegalArgumentException
     **/
    public SizedModel(BSim3Model m, double W, double L) {

        double Ldrn, Wdrn, T0, T1, T2, T3, T4, T5, Inv_L, Inv_W, Inv_LW;
        double Tnom, TRatio, Vtm0, ni, Eg0;
        double tmp, tmp1, tmp2, tmp3;
      
        /*** adjust W,L ***/
        W += m.xw;
        L += m.xl;
      
        /*** temperature, width, length dependent calculations ***/
        Tnom = m.tnom;
        TRatio = m.temp / Tnom;
      
        Vtm0 = BSim3Model.KboQ * Tnom;
        Eg0 = 1.16 - 7.02e-4 * Tnom * Tnom / (Tnom + 1108.0);
        ni = 1.45e10 * (Tnom / 300.15) * Math.sqrt(Tnom / 300.15)
            * Math.exp(21.5565981 - Eg0 / (2.0 * Vtm0));
      
        Ldrn = L;
        Wdrn = W;
        Length = Ldrn;
        Width = Wdrn;
      	
        T0 = Math.pow(Ldrn, m.Lln);
        T1 = Math.pow(Wdrn, m.Lwn);
        tmp1 = m.Ll / T0 + m.Lw / T1 + m.Lwl / (T0 * T1);
        dl = m.Lint + tmp1;
        tmp2 = m.Llc / T0 + m.Lwc / T1 + m.Lwlc / (T0 * T1);
        dlc = m.dlc + tmp2;
      
        T2 = Math.pow(Ldrn, m.Wln);
        T3 = Math.pow(Wdrn, m.Wwn);
        tmp1 = m.Wl / T2 + m.Ww / T3 + m.Wwl / (T2 * T3);
        dw = m.Wint + tmp1;
        tmp2 = m.Wlc / T2 + m.Wwc / T3 + m.Wwlc / (T2 * T3);
        dwc = m.dwc + tmp2;
      
        leff = L - 2.0 * dl;
        weff = W - 2.0 * dw;
        leffCV = L - 2.0 * dlc;
        weffCV = W - 2.0 * dwc;
      
        if (leff <= 0.0)
             throw new IllegalArgumentException("Effective channel length <= 0");
        if (weff <= 0.0)
             throw new IllegalArgumentException("Effective channel width <= 0");
        if (leffCV <= 0.0)
             throw new IllegalArgumentException("Effective channel length for C-V <= 0");
        if (weffCV <= 0.0)
             throw new IllegalArgumentException("Effective channel width for C-V <= 0");
      
        if (m.binUnit == 1)
          {
          Inv_L = 1.0e-6 / leff;
          Inv_W = 1.0e-6 / weff;
          Inv_LW = 1.0e-12 / (leff * weff);
          }
        else
          {
          Inv_L = 1.0 / leff;
          Inv_W = 1.0 / weff;
          Inv_LW = 1.0 / (leff * weff);
          }
      
        cdsc = m.cdsc
            + m.lcdsc * Inv_L
            + m.wcdsc * Inv_W
            + m.pcdsc * Inv_LW;
        cdscb = m.cdscb
            + m.lcdscb * Inv_L
            + m.wcdscb * Inv_W
            + m.pcdscb * Inv_LW;
      				
        cdscd = m.cdscd
            + m.lcdscd * Inv_L
            + m.wcdscd * Inv_W
            + m.pcdscd * Inv_LW;
      				
        cit = m.cit
            + m.lcit * Inv_L
            + m.wcit * Inv_W
            + m.pcit * Inv_LW;
        nfactor = m.nfactor
            + m.lnfactor * Inv_L
            + m.wnfactor * Inv_W
            + m.pnfactor * Inv_LW;
        xj = m.xj
            + m.lxj * Inv_L
            + m.wxj * Inv_W
            + m.pxj * Inv_LW;
        vsat = m.vsat
            + m.lvsat * Inv_L
            + m.wvsat * Inv_W
            + m.pvsat * Inv_LW;
        at = m.at
            + m.lat * Inv_L
            + m.wat * Inv_W
            + m.pat * Inv_LW;
        a0 = m.a0
            + m.la0 * Inv_L
            + m.wa0 * Inv_W
            + m.pa0 * Inv_LW;
      				
        ags = m.ags
            + m.lags * Inv_L
            + m.wags * Inv_W
            + m.pags * Inv_LW;
      				
        a1 = m.a1
            + m.la1 * Inv_L
            + m.wa1 * Inv_W
            + m.pa1 * Inv_LW;
        a2 = m.a2
            + m.la2 * Inv_L
            + m.wa2 * Inv_W
            + m.pa2 * Inv_LW;
        keta = m.keta
            + m.lketa * Inv_L
            + m.wketa * Inv_W
            + m.pketa * Inv_LW;
        nsub = m.nsub
            + m.lnsub * Inv_L
            + m.wnsub * Inv_W
            + m.pnsub * Inv_LW;
        npeak = m.npeak
            + m.lnpeak * Inv_L
            + m.wnpeak * Inv_W
            + m.pnpeak * Inv_LW;
        ngate = m.ngate
            + m.lngate * Inv_L
            + m.wngate * Inv_W
            + m.pngate * Inv_LW;
        gamma1 = m.gamma1
            + m.lgamma1 * Inv_L
            + m.wgamma1 * Inv_W
            + m.pgamma1 * Inv_LW;
        gamma2 = m.gamma2
            + m.lgamma2 * Inv_L
            + m.wgamma2 * Inv_W
            + m.pgamma2 * Inv_LW;
        vbx = m.vbx
            + m.lvbx * Inv_L
            + m.wvbx * Inv_W
            + m.pvbx * Inv_LW;
        vbm = m.vbm
            + m.lvbm * Inv_L
            + m.wvbm * Inv_W
            + m.pvbm * Inv_LW;
        xt = m.xt
            + m.lxt * Inv_L
            + m.wxt * Inv_W
            + m.pxt * Inv_LW;
        vfb = m.vfb
            + m.lvfb * Inv_L
            + m.wvfb * Inv_W
            + m.pvfb * Inv_LW;
        k1 = m.k1
            + m.lk1 * Inv_L
            + m.wk1 * Inv_W
            + m.pk1 * Inv_LW;
        kt1 = m.kt1
            + m.lkt1 * Inv_L
            + m.wkt1 * Inv_W
            + m.pkt1 * Inv_LW;
        kt1l = m.kt1l
            + m.lkt1l * Inv_L
            + m.wkt1l * Inv_W
            + m.pkt1l * Inv_LW;
        k2 = m.k2
            + m.lk2 * Inv_L
            + m.wk2 * Inv_W
            + m.pk2 * Inv_LW;
        kt2 = m.kt2
            + m.lkt2 * Inv_L
            + m.wkt2 * Inv_W
            + m.pkt2 * Inv_LW;
        k3 = m.k3
            + m.lk3 * Inv_L
            + m.wk3 * Inv_W
            + m.pk3 * Inv_LW;
        k3b = m.k3b
            + m.lk3b * Inv_L
            + m.wk3b * Inv_W
            + m.pk3b * Inv_LW;
        w0 = m.w0
            + m.lw0 * Inv_L
            + m.ww0 * Inv_W
            + m.pw0 * Inv_LW;
        nlx = m.nlx
            + m.lnlx * Inv_L
            + m.wnlx * Inv_W
            + m.pnlx * Inv_LW;
        dvt0 = m.dvt0
            + m.ldvt0 * Inv_L
            + m.wdvt0 * Inv_W
            + m.pdvt0 * Inv_LW;
        dvt1 = m.dvt1
            + m.ldvt1 * Inv_L
            + m.wdvt1 * Inv_W
            + m.pdvt1 * Inv_LW;
        dvt2 = m.dvt2
            + m.ldvt2 * Inv_L
            + m.wdvt2 * Inv_W
            + m.pdvt2 * Inv_LW;
        dvt0w = m.dvt0w
            + m.ldvt0w * Inv_L
            + m.wdvt0w * Inv_W
            + m.pdvt0w * Inv_LW;
        dvt1w = m.dvt1w
            + m.ldvt1w * Inv_L
            + m.wdvt1w * Inv_W
            + m.pdvt1w * Inv_LW;
        dvt2w = m.dvt2w
            + m.ldvt2w * Inv_L
            + m.wdvt2w * Inv_W
            + m.pdvt2w * Inv_LW;
        drout = m.drout
            + m.ldrout * Inv_L
            + m.wdrout * Inv_W
            + m.pdrout * Inv_LW;
        dsub = m.dsub
            + m.ldsub * Inv_L
            + m.wdsub * Inv_W
            + m.pdsub * Inv_LW;
        vth0 = m.vth0
            + m.lvth0 * Inv_L
            + m.wvth0 * Inv_W
            + m.pvth0 * Inv_LW;
        ua = m.ua
            + m.lua * Inv_L
            + m.wua * Inv_W
            + m.pua * Inv_LW;
        ua1 = m.ua1
            + m.lua1 * Inv_L
            + m.wua1 * Inv_W
            + m.pua1 * Inv_LW;
        ub = m.ub
            + m.lub * Inv_L
            + m.wub * Inv_W
            + m.pub * Inv_LW;
        ub1 = m.ub1
            + m.lub1 * Inv_L
            + m.wub1 * Inv_W
            + m.pub1 * Inv_LW;
        uc = m.uc
            + m.luc * Inv_L
            + m.wuc * Inv_W
            + m.puc * Inv_LW;
        uc1 = m.uc1
            + m.luc1 * Inv_L
            + m.wuc1 * Inv_W
            + m.puc1 * Inv_LW;
        u0 = m.u0
            + m.lu0 * Inv_L
            + m.wu0 * Inv_W
            + m.pu0 * Inv_LW;
        ute = m.ute
            + m.lute * Inv_L
            + m.wute * Inv_W
            + m.pute * Inv_LW;
        voff = m.voff
            + m.lvoff * Inv_L
            + m.wvoff * Inv_W
            + m.pvoff * Inv_LW;
        delta = m.delta
            + m.ldelta * Inv_L
            + m.wdelta * Inv_W
            + m.pdelta * Inv_LW;
        rdsw = m.rdsw
            + m.lrdsw * Inv_L
            + m.wrdsw * Inv_W
            + m.prdsw * Inv_LW;
        prwg = m.prwg
            + m.lprwg * Inv_L
            + m.wprwg * Inv_W
            + m.pprwg * Inv_LW;
        prwb = m.prwb
            + m.lprwb * Inv_L
            + m.wprwb * Inv_W
            + m.pprwb * Inv_LW;
        prt = m.prt
            + m.lprt * Inv_L
            + m.wprt * Inv_W
            + m.pprt * Inv_LW;
        eta0 = m.eta0
            + m.leta0 * Inv_L
            + m.weta0 * Inv_W
            + m.peta0 * Inv_LW;
        etab = m.etab
            + m.letab * Inv_L
            + m.wetab * Inv_W
            + m.petab * Inv_LW;
        pclm = m.pclm
            + m.lpclm * Inv_L
            + m.wpclm * Inv_W
            + m.ppclm * Inv_LW;
        pdibl1 = m.pdibl1
            + m.lpdibl1 * Inv_L
            + m.wpdibl1 * Inv_W
            + m.ppdibl1 * Inv_LW;
        pdibl2 = m.pdibl2
            + m.lpdibl2 * Inv_L
            + m.wpdibl2 * Inv_W
            + m.ppdibl2 * Inv_LW;
        pdiblb = m.pdiblb
            + m.lpdiblb * Inv_L
            + m.wpdiblb * Inv_W
            + m.ppdiblb * Inv_LW;
        pscbe1 = m.pscbe1
            + m.lpscbe1 * Inv_L
            + m.wpscbe1 * Inv_W
            + m.ppscbe1 * Inv_LW;
        pscbe2 = m.pscbe2
            + m.lpscbe2 * Inv_L
            + m.wpscbe2 * Inv_W
            + m.ppscbe2 * Inv_LW;
        pvag = m.pvag
            + m.lpvag * Inv_L
            + m.wpvag * Inv_W
            + m.ppvag * Inv_LW;
        wr = m.wr
            + m.lwr * Inv_L
            + m.wwr * Inv_W
            + m.pwr * Inv_LW;
        dwg = m.dwg
            + m.ldwg * Inv_L
            + m.wdwg * Inv_W
            + m.pdwg * Inv_LW;
        dwb = m.dwb
            + m.ldwb * Inv_L
            + m.wdwb * Inv_W
            + m.pdwb * Inv_LW;
        b0 = m.b0
            + m.lb0 * Inv_L
            + m.wb0 * Inv_W
            + m.pb0 * Inv_LW;
        b1 = m.b1
            + m.lb1 * Inv_L
            + m.wb1 * Inv_W
            + m.pb1 * Inv_LW;
        alpha0 = m.alpha0
            + m.lalpha0 * Inv_L
            + m.walpha0 * Inv_W
            + m.palpha0 * Inv_LW;
        alpha1 = m.alpha1
            + m.lalpha1 * Inv_L
            + m.walpha1 * Inv_W
            + m.palpha1 * Inv_LW;
        beta0 = m.beta0
            + m.lbeta0 * Inv_L
            + m.wbeta0 * Inv_W
            + m.pbeta0 * Inv_LW;
        /* CV model */
        elm = m.elm
            + m.lelm * Inv_L
            + m.welm * Inv_W
            + m.pelm * Inv_LW;
        cgsl = m.cgsl
            + m.lcgsl * Inv_L
            + m.wcgsl * Inv_W
            + m.pcgsl * Inv_LW;
        cgdl = m.cgdl
            + m.lcgdl * Inv_L
            + m.wcgdl * Inv_W
            + m.pcgdl * Inv_LW;
        ckappa = m.ckappa
            + m.lckappa * Inv_L
            + m.wckappa * Inv_W
            + m.pckappa * Inv_LW;
        cf = m.cf
            + m.lcf * Inv_L
            + m.wcf * Inv_W
            + m.pcf * Inv_LW;
        clc = m.clc
            + m.lclc * Inv_L
            + m.wclc * Inv_W
            + m.pclc * Inv_LW;
        cle = m.cle
            + m.lcle * Inv_L
            + m.wcle * Inv_W
            + m.pcle * Inv_LW;
        vfbcv = m.vfbcv
            + m.lvfbcv * Inv_L
            + m.wvfbcv * Inv_W
            + m.pvfbcv * Inv_LW;
        acde = m.acde
            + m.lacde * Inv_L
            + m.wacde * Inv_W
            + m.pacde * Inv_LW;
        moin = m.moin
            + m.lmoin * Inv_L
            + m.wmoin * Inv_W
            + m.pmoin * Inv_LW;
        noff = m.noff
            + m.lnoff * Inv_L
            + m.wnoff * Inv_W
            + m.pnoff * Inv_LW;
        voffcv = m.voffcv
            + m.lvoffcv * Inv_L
            + m.wvoffcv * Inv_W
            + m.pvoffcv * Inv_LW;
      
        abulkCVfactor = 1.0 + Math.pow((clc / leffCV), cle);
      
        T0 = (TRatio - 1.0);
        ua = ua + ua1 * T0;
        ub = ub + ub1 * T0;
        uc = uc + uc1 * T0;
        if (u0 > 1.0) u0 = u0 / 1.0e4;
      
        u0temp = u0 * Math.pow(TRatio, ute);
        vsattemp = vsat - at * T0;
        rds0 = (rdsw + prt * T0) / Math.pow(weff * 1E6, wr);
      
        cgdo = (m.cgdo + cf) * weffCV;
        cgso = (m.cgso + cf) * weffCV;
        cgbo = m.cgbo * leffCV;
      
        T0 = leffCV * leffCV;
        tconst = u0temp * elm / (m.cox * weffCV * leffCV * T0);
      
        if (!m.given(m.npeak) && m.given(m.gamma1))
          {
          T0 = gamma1 * m.cox;
          npeak = 3.021E22 * T0 * T0;
          }
      
        phi = 2.0 * Vtm0 * Math.log(npeak / ni);
      
        sqrtPhi = Math.sqrt(phi);
        phis3 = sqrtPhi * phi;
      
        Xdep0 = Math.sqrt(2.0 * BSim3Model.EPSSI / (BSim3Model.Charge_q * npeak * 1.0e6)) * sqrtPhi;
        sqrtXdep0 = Math.sqrt(Xdep0);
        litl = Math.sqrt(3.0 * xj * m.tox);
        vbi = Vtm0 * Math.log(1.0e20 * npeak / (ni * ni));
        cdep0 = Math.sqrt(BSim3Model.Charge_q * BSim3Model.EPSSI * npeak * 1.0e6 / 2.0 / phi);
      
        ldeb = Math.sqrt(BSim3Model.EPSSI * Vtm0 / (BSim3Model.Charge_q * npeak * 1.0e6)) / 3.0;
        acde *= Math.pow((npeak / 2.0e16), -0.25);
      
        if (m.given(m.k1) || m.given(m.k2))
          {
          if (!m.given(m.k1))
            {
            System.out.println("Warning: k1 should be specified with k2.");
            k1 = 0.53;
            }
          if (!m.given(m.k2))
            {
            System.out.println("Warning: k2 should be specified with k1.");
            k2 = -0.0186;
            }
          if (m.given(m.nsub))
            System.out.println("Warning: nsub is ignored because k1 or k2 is given.");
          if (m.given(m.xt))
            System.out.println("Warning: xt is ignored because k1 or k2 is given.");
          if (m.given(m.vbx))
            System.out.println("Warning: vbx is ignored because k1 or k2 is given.");
          if (m.given(m.gamma1))
            System.out.println("Warning: gamma1 is ignored because k1 or k2 is given.");
          if (m.given(m.gamma2))
            System.out.println("Warning: gamma2 is ignored because k1 or k2 is given.");
          }
        else
          {
          if (!m.given(m.vbx))
            vbx = phi - 7.7348e-4 * npeak * xt * xt;
          if (vbx > 0.0)
            vbx = -vbx;
          if (vbm > 0.0)
            vbm = -vbm;
      
          if (!m.given(m.gamma1))
            gamma1 = 5.753e-12 * Math.sqrt(npeak) / m.cox;
          if (!m.given(m.gamma2))
            gamma2 = 5.753e-12 * Math.sqrt(nsub) / m.cox;
      
          T0 = gamma1 - gamma2;
          T1 = Math.sqrt(phi - vbx) - sqrtPhi;
          T2 = Math.sqrt(phi * (phi - vbm)) - phi;
          k2 = T0 * T1 / (2.0 * T2 + vbm);
          k1 = gamma2 - 2.0 * k2 * Math.sqrt(phi - vbm);
          }
      
        if (k2 < 0.0)
          {
          T0 = 0.5 * k1 / k2;
          vbsc = 0.9 * (phi - T0 * T0);
          if (vbsc > -3.0)
            vbsc = -3.0;
          else if (vbsc < -30.0)
            vbsc = -30.0;
          }
        else
          vbsc = -30.0;
      
        if (vbsc > vbm)
          vbsc = vbm;
      
        if (!m.given(m.vfb))
          {
          if (m.given(m.vth0))
            vfb = m.type * vth0 - phi - k1 * sqrtPhi;
          else
            vfb = -1.0;
          }
      
        if (!m.given(m.vth0))
          vth0 = m.type * (vfb + phi + k1 * sqrtPhi);
      
        k1ox = k1 * m.tox / m.toxm;
        k2ox = k2 * m.tox / m.toxm;
      
        T1 = Math.sqrt(BSim3Model.EPSSI / BSim3Model.EPSOX * m.tox * Xdep0);
        T0 = Math.exp(-0.5 * dsub * leff / T1);
        theta0vb0 = (T0 + 2.0 * T0 * T0);
      
        T0 = Math.exp(-0.5 * drout * leff / T1);
        T2 = (T0 + 2.0 * T0 * T0);
        thetaRout = pdibl1 * T2 + pdibl2;
      
        tmp = Math.sqrt(Xdep0);
        tmp1 = vbi - phi;
        tmp2 = m.factor1 * tmp;
      
        T0 = -0.5 * dvt1w * weff * leff / tmp2;
        if (T0 > -BSim3Model.EXP_THRESHOLD)
          {
          T1 = Math.exp(T0);
          T2 = T1 * (1.0 + 2.0 * T1);
          }
        else
          {
          T1 = BSim3Model.MIN_EXP;
          T2 = T1 * (1.0 + 2.0 * T1);
          }
        T0 = dvt0w * T2;
        T2 = T0 * tmp1;
      
        T0 = -0.5 * dvt1 * leff / tmp2;
        if (T0 > -BSim3Model.EXP_THRESHOLD)
          {
          T1 = Math.exp(T0);
          T3 = T1 * (1.0 + 2.0 * T1);
          }
        else
          {
          T1 = BSim3Model.MIN_EXP;
          T3 = T1 * (1.0 + 2.0 * T1);
          }
        T3 = dvt0 * T3 * tmp1;
      
        T4 = m.tox * phi / (weff + w0);
      
        T0 = Math.sqrt(1.0 + nlx / leff);
        T5 = k1ox * (T0 - 1.0) * sqrtPhi + (kt1 + kt1l / leff) * (TRatio - 1.0);
      
        tmp3 = m.type * vth0 - T2 - T3 + k3 * T4 + T5;
        vfbzb = tmp3 - phi - k1 * sqrtPhi;
    }
}
