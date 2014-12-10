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

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StreamTokenizer;
import java.util.Iterator;
import java.util.ArrayList;

import com.avlsi.file.common.DeviceTypes;

/**
 * Read in BSIM3 files and create java Model objects from them.  Will 
 * load the files natively or in java, depending on the parameters set.
 *
 * @author Ted Vessenes, some JNI stuff from Dan Daly
 * @version $Name:  $ $Date$
 **/
public class BSim3Model {
    protected static ArrayList models = new ArrayList();

    private static final int NMOS = 1;
    private static final int PMOS = -1;
    
    /** Loads the model natively (models reside on the C side **/
    private static native void loadModel(String filename, double temperature);
    
    /** Retrieves the pointer to the model on the C side **/
    public static native int findNativeModel(int type, double width, 
                                             double length);
    
    /** Was the native load successful? **/
    private static boolean nativeLoaded = false;
    /** For debugging the native **/
    public static boolean both = false;

    //
    //Instance Vars for Native Implementation
    //
    //
    /** For native models, the handle to the native model **/
    private int nativeModelHandle;
    
    public static boolean isNativeLoaded() { return nativeLoaded; }

    public int getHandle() { return nativeModelHandle; }

    public static void readFile(String filename, double temperature,
                                boolean useNative) {
        if (!nativeLoaded && (useNative || both)) {
            try {
                System.loadLibrary("Aspice");
                loadModel(filename,temperature);
                nativeLoaded = true;
                System.out.println("Native Model Libraries Loaded from "+
                                   filename+" @ "+temperature+" deg C");
            } catch (UnsatisfiedLinkError e) {
                System.out.println("Exception while loading native model library"+
                                   ", UnsatisfiedLinkError:"+e.getMessage()+
                                   "Java Models will be used instead.");
                nativeLoaded = false;
            } catch ( Exception e) {
                System.out.println("Exception while loading native model library :"+
                                   "\t"+e+"\n"+
                                   "\t"+e.getMessage()+
                                   "Java Models will be used instead.");
                nativeLoaded = false;
            }
        } 
        if (!BSim3Model.isNativeLoaded() || BSim3Model.both) {
            readFile(filename, temperature);
        }
    }
    
    public static void readFile(String fileName, double temperature) {
        FileInputStream file = null;
        try {
            file = new FileInputStream(fileName);
            StreamTokenizer st = new StreamTokenizer(
                                     new BufferedReader(
                                         new InputStreamReader(file)
                                     )
                                 );
            // For some reason, the steam tokenizer defaults number
            // processing mode on and has no way of automatically turning
            // it off-- hence this code.
            st.resetSyntax();
            st.wordChars(0, 255);

            st.whitespaceChars(' ', ' ');
            st.whitespaceChars('\t', '\t');
            st.whitespaceChars('\n', '\n');
            st.whitespaceChars('\r', '\r');
            st.whitespaceChars('=', '=');

            st.lowerCaseMode(true);
            st.commentChar('*');

            // Set of data states describing which state machine state
            // we're in (ie. what data we're waiting for)

            final int BSIM_ERROR  = 0; // Found error-- abort
            final int BSIM_IDLE   = 1; // Start state or Found )
            final int BSIM_MODEL  = 2; // Found .MODEL
            final int BSIM_NAME   = 3; // Found CMOSN, CMOSP, etc.
            final int BSIM_TYPE   = 4; // Found NMOS, PMOS, etc.
            final int BSIM_DATA   = 5; // Found ( or field value
            final int BSIM_FIELD  = 6; // Found Field name
            int state = BSIM_IDLE;

            // The for loop below encodes a simple state machine on
            // These states.  They link as follows:
            //   Name -> Type -> Data ---> Field
            //    ^              |  ^          |
            //    |              v  |          |
            //    -- Model <- Idle  ------------
            // 
            // Idle is the start state.  If we reach EOF outside of idle,
            // the file is invalid.

            // Ref to the current model
            BSim3Model model = new BSim3Model(temperature);

            String field = "";

            while (state != BSIM_ERROR && st.nextToken() != st.TT_EOF) {
                if (st.ttype != st.TT_WORD)
                    continue;

                switch (state) {
                case BSIM_IDLE:
                    if (st.sval.equals(".model"))
                        state = BSIM_MODEL;
                    break;

                // Find model name
                case BSIM_MODEL:
                    // Model name is in st.sval but we don't care
                    models.add(model);

                    state = BSIM_NAME;
                    break;

                // Find model type
                case BSIM_NAME:
                    if (st.sval.equals("nmos")) {
                        model.type = NMOS;
                    } else if (st.sval.equals("pmos")) {
                        model.type = PMOS;
                    } else {
                        System.out.println("Invalid Model type: "+st.sval);
                        state = BSIM_ERROR;
                        break;
                    }

                    state = BSIM_TYPE;
                    break;

                // Find data open '('
                case BSIM_TYPE:
                    if (st.sval.equals("("))
                        state = BSIM_DATA;
                    else {
                        System.out.println("Found " + st.sval + " instead " +
                                           "of expected '('");
                        state = BSIM_ERROR;
                    }
                    break;

                // Find field name or ')'
                case BSIM_DATA:
                    if (st.sval.equals(")")) {
                        // Cleanup old model
                        model.setDefaults();
                        model.computeTemperatureData();

                        // Get to work on the new one
                        model = new BSim3Model(temperature);
                        state = BSIM_IDLE;
                    } else {
                        if (st.sval.startsWith("+")) {
                            field = st.sval.substring(1);
                        } else {
                            field = st.sval;
                        }
                        state = BSIM_FIELD;
                    }

                    break;

                // Find field number value
                case BSIM_FIELD:
                    model.set(field, st.sval);
                    state = BSIM_DATA;
                    break;

                // Error out just in case
                default:
                    System.out.println("State machine in unknown state " +
                                       state);
                    state = BSIM_ERROR;
                case BSIM_ERROR:
                    break;
                }
            }
        } catch (IOException e) {
            System.out.println(e);
        } finally {
            try {
                if (file != null)
                    file.close();
            } catch (IOException e) {
                ;
            }
        }
    }

    public BSim3Model(int type, double temperature) {
        this.type = type;
        this.temp = temperature + CONSTCtoK;
    }

    public BSim3Model(double temperature) {
        this.temp = temperature + CONSTCtoK;
    }

    private BSim3Model(int type, int handle) {
        this.type = type;
        this.nativeModelHandle = handle;
    }
    /**
     * Finds a model given a type, width, and length
     * @param type Type of model (NMOS or PMOS)
     * @param width in meters
     * @param length in meters
     * @throws IllegalArgumentException
     * @return BSim3Model if found, or null otherwise
     **/
    public static BSim3Model findModel(int type, final double W, final double L)
    {
        // Convert between normal typing and BSim's type multiplier--
        // -1 for PMOS where everything is inverted.
        if (type == DeviceTypes.N_TYPE) {
            type = NMOS;
        } else if (type == DeviceTypes.P_TYPE) {
            type = PMOS;
        }

        if (isNativeLoaded() && !both) {
            int h = findNativeModel(type, W, L);
            //System.out.println("Native model loaded at "+h+" with type= "+type+
            //                   " width= "+W+" length= "+L);
            return new BSim3Model(type, h);
        }
            
        for (Iterator i = models.iterator(); i.hasNext(); ) {
            final BSim3Model m = (BSim3Model) i.next();
            if (type == m.type && L >= m.Lmin && L < m.Lmax
                               && W >= m.Wmin && W < m.Wmax) {
                //System.out.println("model = "+m.type+" lmin= "+m.Lmin+" lmax= "+
                //               m.Lmax+" Wmin= "+m.Wmin+" Wmax= "+m.Wmax);
                if (isNativeLoaded() && both) {
                    int h = findNativeModel(type, W, L);
                    //System.out.println("Native model loaded at "+h+
                    //                   " with type= "+type+
                    //                   " width= "+W+" length= "+L);
                    m.nativeModelHandle = h;
                }
                return m;
            }
        }

        throw new IllegalArgumentException("Could not find model for type " +
                                           type + ", width " + W + ", and " +
                                           "length " + L + ".");
    }

    public static final double EPSOX = 3.453133e-11;
    public static final double EPSSI = 1.03594e-10;
    public static final double CONSTCtoK = 273.15;
    public static final double Charge_q = 1.60219e-19;
    public static final double CONSTroot2 = 1.41421356237309504880;
    public static final double CONSTboltz = 1.3806226e-23;
    public static final double CONSTvt0 = CONSTboltz * (27 + CONSTCtoK ) / Charge_q;
    public static final double KboQ = 8.617087e-5; /* Kb / q  where q = 1.60219e-19 */

    public static final double MAX_EXP = 5.834617425e14;
    public static final double MIN_EXP = 1.713908431e-15;
    public static final double EXP_THRESHOLD = 34.0;

    public static final double DELTA_1 = 0.02;
    public static final double DELTA_2 = 0.02;
    public static final double DELTA_3 = 0.02;
    public static final double DELTA_4 = 0.02;

    public boolean given(double d) { return !Double.isNaN(d); }
    public boolean given(int i)    { return i != Integer.MIN_VALUE; }
    public boolean given(String s) { return !s.equals(""); }

    public double temp = Double.NaN;
    public double xl = Double.NaN;
    public double xw = Double.NaN;
    public double hdif = Double.NaN;
    public double ldif = Double.NaN;
    public double rs = Double.NaN;
    public double rd = Double.NaN;
    public int acm = Integer.MIN_VALUE;
    public double cta = Double.NaN;
    public double ctp = Double.NaN;
    public double pta = Double.NaN;
    public double ptp = Double.NaN;
    public double n = Double.NaN;
    public int nlev = Integer.MIN_VALUE;
    public double tlev = Double.NaN;
    public double tlevc = Double.NaN;
    public double em = Double.NaN;
    public double ef = Double.NaN;
    public double af = Double.NaN;
    public double kf = Double.NaN;
    public int nqsMod = Integer.MIN_VALUE;
    public int type = Integer.MIN_VALUE;
    public int level = Integer.MIN_VALUE;
    public int capMod = Integer.MIN_VALUE;
    public int mobMod = Integer.MIN_VALUE;
    public int noiMod = Integer.MIN_VALUE;
    public int paramChk = Integer.MIN_VALUE;
    public int binUnit = Integer.MIN_VALUE;
    public String version = "";
    public double tox = Double.NaN;
    public double toxm = Double.NaN;
    public double cdsc = Double.NaN;
    public double cdscb = Double.NaN;
    public double cdscd = Double.NaN;
    public double cit = Double.NaN;
    public double nfactor = Double.NaN;
    public double xj = Double.NaN;
    public double vsat = Double.NaN;
    public double at = Double.NaN;
    public double a0 = Double.NaN;
    public double ags = Double.NaN;
    public double a1 = Double.NaN;
    public double a2 = Double.NaN;
    public double keta = Double.NaN;
    public double nsub = Double.NaN;
    public double npeak = Double.NaN;
    public double ngate = Double.NaN;
    public double gamma1 = Double.NaN;
    public double gamma2 = Double.NaN;
    public double vbx = Double.NaN;
    public double vbm = Double.NaN;
    public double xt = Double.NaN;
    public double k1 = Double.NaN;
    public double kt1 = Double.NaN;
    public double kt1l = Double.NaN;
    public double kt2 = Double.NaN;
    public double k2 = Double.NaN;
    public double k3 = Double.NaN;
    public double k3b = Double.NaN;
    public double w0 = Double.NaN;
    public double nlx = Double.NaN;
    public double dvt0 = Double.NaN;
    public double dvt1 = Double.NaN;
    public double dvt2 = Double.NaN;
    public double dvt0w = Double.NaN;
    public double dvt1w = Double.NaN;
    public double dvt2w = Double.NaN;
    public double drout = Double.NaN;
    public double dsub = Double.NaN;
    public double vth0 = Double.NaN;
    public double ua = Double.NaN;
    public double ua1 = Double.NaN;
    public double ub = Double.NaN;
    public double ub1 = Double.NaN;
    public double uc = Double.NaN;
    public double uc1 = Double.NaN;
    public double u0 = Double.NaN;
    public double ute = Double.NaN;
    public double voff = Double.NaN;
    public double delta = Double.NaN;
    public double rdsw = Double.NaN;
    public double prwg = Double.NaN;
    public double prwb = Double.NaN;
    public double prt = Double.NaN;
    public double eta0 = Double.NaN;
    public double etab = Double.NaN;
    public double pclm = Double.NaN;
    public double pdibl1 = Double.NaN;
    public double pdibl2 = Double.NaN;
    public double pdiblb = Double.NaN;
    public double pscbe1 = Double.NaN;
    public double pscbe2 = Double.NaN;
    public double pvag = Double.NaN;
    public double wr = Double.NaN;
    public double dwg = Double.NaN;
    public double dwb = Double.NaN;
    public double b0 = Double.NaN;
    public double b1 = Double.NaN;
    public double alpha0 = Double.NaN;
    public double alpha1 = Double.NaN;
    public double beta0 = Double.NaN;
    public double ijth = Double.NaN;
    public double vfb = Double.NaN;
    public double elm = Double.NaN;
    public double cgsl = Double.NaN;
    public double cgdl = Double.NaN;
    public double ckappa = Double.NaN;
    public double cf = Double.NaN;
    public double vfbcv = Double.NaN;
    public double clc = Double.NaN;
    public double cle = Double.NaN;
    public double dwc = Double.NaN;
    public double dlc = Double.NaN;
    public double noff = Double.NaN;
    public double voffcv = Double.NaN;
    public double acde = Double.NaN;
    public double moin = Double.NaN;
    public double tcj = Double.NaN;
    public double tcjsw = Double.NaN;
    public double tcjswg = Double.NaN;
    public double tpb = Double.NaN;
    public double tpbsw = Double.NaN;
    public double tpbswg = Double.NaN;
    public double tnom = Double.NaN;
    public double cgso = Double.NaN;
    public double cgdo = Double.NaN;
    public double cgbo = Double.NaN;
    public double xpart = Double.NaN;
    public double sheetResistance = Double.NaN;
    public double jctSatCurDensity = Double.NaN;
    public double jctSidewallSatCurDensity = Double.NaN;
    public double bulkJctPotential = Double.NaN;
    public double bulkJctBotGradingCoeff = Double.NaN;
    public double bulkJctSideGradingCoeff = Double.NaN;
    public double bulkJctGateSideGradingCoeff = Double.NaN;
    public double sidewallJctPotential = Double.NaN;
    public double GatesidewallJctPotential = Double.NaN;
    public double unitAreaJctCap = Double.NaN;
    public double unitLengthSidewallJctCap = Double.NaN;
    public double unitLengthGateSidewallJctCap = Double.NaN;
    public double jctEmissionCoeff = Double.NaN;
    public double jctTempExponent = Double.NaN;
    public double Lint = Double.NaN;
    public double Ll = Double.NaN;
    public double Llc = Double.NaN;
    public double Lln = Double.NaN;
    public double Lw = Double.NaN;
    public double Lwc = Double.NaN;
    public double Lwn = Double.NaN;
    public double Lwl = Double.NaN;
    public double Lwlc = Double.NaN;
    public double Lmin = Double.NaN;
    public double Lmax = Double.NaN;
    public double Wint = Double.NaN;
    public double Wl = Double.NaN;
    public double Wlc = Double.NaN;
    public double Wln = Double.NaN;
    public double Ww = Double.NaN;
    public double Wwc = Double.NaN;
    public double Wwn = Double.NaN;
    public double Wwl = Double.NaN;
    public double Wwlc = Double.NaN;
    public double Wmin = Double.NaN;
    public double Wmax = Double.NaN;
    public double vtm = Double.NaN;
    public double cox = Double.NaN;
    public double cof1 = Double.NaN;
    public double cof2 = Double.NaN;
    public double cof3 = Double.NaN;
    public double cof4 = Double.NaN;
    public double vcrit = Double.NaN;
    public double factor1 = Double.NaN;
    public double PhiB = Double.NaN;
    public double PhiBSW = Double.NaN;
    public double PhiBSWG = Double.NaN;
    public double jctTempSatCurDensity = Double.NaN;
    public double jctSidewallTempSatCurDensity = Double.NaN;
    public double oxideTrapDensityA = Double.NaN;
    public double oxideTrapDensityB = Double.NaN;
    public double oxideTrapDensityC = Double.NaN;
    public double lcdsc = Double.NaN;
    public double lcdscb = Double.NaN;
    public double lcdscd = Double.NaN;
    public double lcit = Double.NaN;
    public double lnfactor = Double.NaN;
    public double lxj = Double.NaN;
    public double lvsat = Double.NaN;
    public double lat = Double.NaN;
    public double la0 = Double.NaN;
    public double lags = Double.NaN;
    public double la1 = Double.NaN;
    public double la2 = Double.NaN;
    public double lketa = Double.NaN;
    public double lnsub = Double.NaN;
    public double lnpeak = Double.NaN;
    public double lngate = Double.NaN;
    public double lgamma1 = Double.NaN;
    public double lgamma2 = Double.NaN;
    public double lvbx = Double.NaN;
    public double lvbm = Double.NaN;
    public double lxt = Double.NaN;
    public double lk1 = Double.NaN;
    public double lkt1 = Double.NaN;
    public double lkt1l = Double.NaN;
    public double lkt2 = Double.NaN;
    public double lk2 = Double.NaN;
    public double lk3 = Double.NaN;
    public double lk3b = Double.NaN;
    public double lw0 = Double.NaN;
    public double lnlx = Double.NaN;
    public double ldvt0 = Double.NaN;
    public double ldvt1 = Double.NaN;
    public double ldvt2 = Double.NaN;
    public double ldvt0w = Double.NaN;
    public double ldvt1w = Double.NaN;
    public double ldvt2w = Double.NaN;
    public double ldrout = Double.NaN;
    public double ldsub = Double.NaN;
    public double lvth0 = Double.NaN;
    public double lua = Double.NaN;
    public double lua1 = Double.NaN;
    public double lub = Double.NaN;
    public double lub1 = Double.NaN;
    public double luc = Double.NaN;
    public double luc1 = Double.NaN;
    public double lu0 = Double.NaN;
    public double lute = Double.NaN;
    public double lvoff = Double.NaN;
    public double ldelta = Double.NaN;
    public double lrdsw = Double.NaN;
    public double lprwg = Double.NaN;
    public double lprwb = Double.NaN;
    public double lprt = Double.NaN;
    public double leta0 = Double.NaN;
    public double letab = Double.NaN;
    public double lpclm = Double.NaN;
    public double lpdibl1 = Double.NaN;
    public double lpdibl2 = Double.NaN;
    public double lpdiblb = Double.NaN;
    public double lpscbe1 = Double.NaN;
    public double lpscbe2 = Double.NaN;
    public double lpvag = Double.NaN;
    public double lwr = Double.NaN;
    public double ldwg = Double.NaN;
    public double ldwb = Double.NaN;
    public double lb0 = Double.NaN;
    public double lb1 = Double.NaN;
    public double lalpha0 = Double.NaN;
    public double lalpha1 = Double.NaN;
    public double lbeta0 = Double.NaN;
    public double lvfb = Double.NaN;
    public double lelm = Double.NaN;
    public double lcgsl = Double.NaN;
    public double lcgdl = Double.NaN;
    public double lckappa = Double.NaN;
    public double lcf = Double.NaN;
    public double lclc = Double.NaN;
    public double lcle = Double.NaN;
    public double lvfbcv = Double.NaN;
    public double lnoff = Double.NaN;
    public double lvoffcv = Double.NaN;
    public double lacde = Double.NaN;
    public double lmoin = Double.NaN;
    public double wcdsc = Double.NaN;
    public double wcdscb = Double.NaN;
    public double wcdscd = Double.NaN;
    public double wcit = Double.NaN;
    public double wnfactor = Double.NaN;
    public double wxj = Double.NaN;
    public double wvsat = Double.NaN;
    public double wat = Double.NaN;
    public double wa0 = Double.NaN;
    public double wags = Double.NaN;
    public double wa1 = Double.NaN;
    public double wa2 = Double.NaN;
    public double wketa = Double.NaN;
    public double wnsub = Double.NaN;
    public double wnpeak = Double.NaN;
    public double wngate = Double.NaN;
    public double wgamma1 = Double.NaN;
    public double wgamma2 = Double.NaN;
    public double wvbx = Double.NaN;
    public double wvbm = Double.NaN;
    public double wxt = Double.NaN;
    public double wk1 = Double.NaN;
    public double wkt1 = Double.NaN;
    public double wkt1l = Double.NaN;
    public double wkt2 = Double.NaN;
    public double wk2 = Double.NaN;
    public double wk3 = Double.NaN;
    public double wk3b = Double.NaN;
    public double ww0 = Double.NaN;
    public double wnlx = Double.NaN;
    public double wdvt0 = Double.NaN;
    public double wdvt1 = Double.NaN;
    public double wdvt2 = Double.NaN;
    public double wdvt0w = Double.NaN;
    public double wdvt1w = Double.NaN;
    public double wdvt2w = Double.NaN;
    public double wdrout = Double.NaN;
    public double wdsub = Double.NaN;
    public double wvth0 = Double.NaN;
    public double wua = Double.NaN;
    public double wua1 = Double.NaN;
    public double wub = Double.NaN;
    public double wub1 = Double.NaN;
    public double wuc = Double.NaN;
    public double wuc1 = Double.NaN;
    public double wu0 = Double.NaN;
    public double wute = Double.NaN;
    public double wvoff = Double.NaN;
    public double wdelta = Double.NaN;
    public double wrdsw = Double.NaN;
    public double wprwg = Double.NaN;
    public double wprwb = Double.NaN;
    public double wprt = Double.NaN;
    public double weta0 = Double.NaN;
    public double wetab = Double.NaN;
    public double wpclm = Double.NaN;
    public double wpdibl1 = Double.NaN;
    public double wpdibl2 = Double.NaN;
    public double wpdiblb = Double.NaN;
    public double wpscbe1 = Double.NaN;
    public double wpscbe2 = Double.NaN;
    public double wpvag = Double.NaN;
    public double wwr = Double.NaN;
    public double wdwg = Double.NaN;
    public double wdwb = Double.NaN;
    public double wb0 = Double.NaN;
    public double wb1 = Double.NaN;
    public double walpha0 = Double.NaN;
    public double walpha1 = Double.NaN;
    public double wbeta0 = Double.NaN;
    public double wvfb = Double.NaN;
    public double welm = Double.NaN;
    public double wcgsl = Double.NaN;
    public double wcgdl = Double.NaN;
    public double wckappa = Double.NaN;
    public double wcf = Double.NaN;
    public double wclc = Double.NaN;
    public double wcle = Double.NaN;
    public double wvfbcv = Double.NaN;
    public double wnoff = Double.NaN;
    public double wvoffcv = Double.NaN;
    public double wacde = Double.NaN;
    public double wmoin = Double.NaN;
    public double pcdsc = Double.NaN;
    public double pcdscb = Double.NaN;
    public double pcdscd = Double.NaN;
    public double pcit = Double.NaN;
    public double pnfactor = Double.NaN;
    public double pxj = Double.NaN;
    public double pvsat = Double.NaN;
    public double pat = Double.NaN;
    public double pa0 = Double.NaN;
    public double pags = Double.NaN;
    public double pa1 = Double.NaN;
    public double pa2 = Double.NaN;
    public double pketa = Double.NaN;
    public double pnsub = Double.NaN;
    public double pnpeak = Double.NaN;
    public double pngate = Double.NaN;
    public double pgamma1 = Double.NaN;
    public double pgamma2 = Double.NaN;
    public double pvbx = Double.NaN;
    public double pvbm = Double.NaN;
    public double pxt = Double.NaN;
    public double pk1 = Double.NaN;
    public double pkt1 = Double.NaN;
    public double pkt1l = Double.NaN;
    public double pkt2 = Double.NaN;
    public double pk2 = Double.NaN;
    public double pk3 = Double.NaN;
    public double pk3b = Double.NaN;
    public double pw0 = Double.NaN;
    public double pnlx = Double.NaN;
    public double pdvt0 = Double.NaN;
    public double pdvt1 = Double.NaN;
    public double pdvt2 = Double.NaN;
    public double pdvt0w = Double.NaN;
    public double pdvt1w = Double.NaN;
    public double pdvt2w = Double.NaN;
    public double pdrout = Double.NaN;
    public double pdsub = Double.NaN;
    public double pvth0 = Double.NaN;
    public double pua = Double.NaN;
    public double pua1 = Double.NaN;
    public double pub = Double.NaN;
    public double pub1 = Double.NaN;
    public double puc = Double.NaN;
    public double puc1 = Double.NaN;
    public double pu0 = Double.NaN;
    public double pute = Double.NaN;
    public double pvoff = Double.NaN;
    public double pdelta = Double.NaN;
    public double prdsw = Double.NaN;
    public double pprwg = Double.NaN;
    public double pprwb = Double.NaN;
    public double pprt = Double.NaN;
    public double peta0 = Double.NaN;
    public double petab = Double.NaN;
    public double ppclm = Double.NaN;
    public double ppdibl1 = Double.NaN;
    public double ppdibl2 = Double.NaN;
    public double ppdiblb = Double.NaN;
    public double ppscbe1 = Double.NaN;
    public double ppscbe2 = Double.NaN;
    public double ppvag = Double.NaN;
    public double pwr = Double.NaN;
    public double pdwg = Double.NaN;
    public double pdwb = Double.NaN;
    public double pb0 = Double.NaN;
    public double pb1 = Double.NaN;
    public double palpha0 = Double.NaN;
    public double palpha1 = Double.NaN;
    public double pbeta0 = Double.NaN;
    public double pvfb = Double.NaN;
    public double pelm = Double.NaN;
    public double pcgsl = Double.NaN;
    public double pcgdl = Double.NaN;
    public double pckappa = Double.NaN;
    public double pcf = Double.NaN;
    public double pclc = Double.NaN;
    public double pcle = Double.NaN;
    public double pvfbcv = Double.NaN;
    public double pnoff = Double.NaN;
    public double pvoffcv = Double.NaN;
    public double pacde = Double.NaN;
    public double pmoin = Double.NaN;

    public void set(String field, String value) {
        try {
            if (field.equals("xl")) xl = Double.parseDouble(value);
            else if (field.equals("xw")) xw = Double.parseDouble(value);
            else if (field.equals("hdif")) hdif = Double.parseDouble(value);
            else if (field.equals("ldif")) ldif = Double.parseDouble(value);
            else if (field.equals("rs")) rs = Double.parseDouble(value);
            else if (field.equals("rd")) rd = Double.parseDouble(value);
            else if (field.equals("acm")) acm = Integer.parseInt(value);
            else if (field.equals("cta")) cta = Double.parseDouble(value);
            else if (field.equals("ctp")) ctp = Double.parseDouble(value);
            else if (field.equals("pta")) pta = Double.parseDouble(value);
            else if (field.equals("ptp")) ptp = Double.parseDouble(value);
            else if (field.equals("n")) n = Double.parseDouble(value);
            else if (field.equals("nlev")) nlev = Integer.parseInt(value);
            else if (field.equals("tlev")) tlev = Double.parseDouble(value);
            else if (field.equals("tlevc")) tlevc = Double.parseDouble(value);
            else if (field.equals("em")) em = Double.parseDouble(value);
            else if (field.equals("ef")) ef = Double.parseDouble(value);
            else if (field.equals("af")) af = Double.parseDouble(value);
            else if (field.equals("kf")) kf = Double.parseDouble(value);
            else if (field.equals("nqsmod")) nqsMod = Integer.parseInt(value);
            else if (field.equals("level")) level = Integer.parseInt(value);
            else if (field.equals("capmod")) capMod = Integer.parseInt(value);
            else if (field.equals("mobmod")) mobMod = Integer.parseInt(value);
            else if (field.equals("noimod")) noiMod = Integer.parseInt(value);
            else if (field.equals("paramchk")) paramChk = Integer.parseInt(value);
            else if (field.equals("binunit")) binUnit = Integer.parseInt(value);
            else if (field.equals("version")) version = value;
            else if (field.equals("tox")) tox = Double.parseDouble(value);
            else if (field.equals("toxm")) toxm = Double.parseDouble(value);
            else if (field.equals("cdsc")) cdsc = Double.parseDouble(value);
            else if (field.equals("cdscb")) cdscb = Double.parseDouble(value);
            else if (field.equals("cdscd")) cdscd = Double.parseDouble(value);
            else if (field.equals("cit")) cit = Double.parseDouble(value);
            else if (field.equals("nfactor")) nfactor = Double.parseDouble(value);
            else if (field.equals("xj")) xj = Double.parseDouble(value);
            else if (field.equals("vsat")) vsat = Double.parseDouble(value);
            else if (field.equals("at")) at = Double.parseDouble(value);
            else if (field.equals("a0")) a0 = Double.parseDouble(value);
            else if (field.equals("ags")) ags = Double.parseDouble(value);
            else if (field.equals("a1")) a1 = Double.parseDouble(value);
            else if (field.equals("a2")) a2 = Double.parseDouble(value);
            else if (field.equals("keta")) keta = Double.parseDouble(value);
            else if (field.equals("nsub")) nsub = Double.parseDouble(value);
            else if (field.equals("nch")) npeak = Double.parseDouble(value);
            else if (field.equals("ngate")) ngate = Double.parseDouble(value);
            else if (field.equals("gamma1")) gamma1 = Double.parseDouble(value);
            else if (field.equals("gamma2")) gamma2 = Double.parseDouble(value);
            else if (field.equals("vbx")) vbx = Double.parseDouble(value);
            else if (field.equals("vbm")) vbm = Double.parseDouble(value);
            else if (field.equals("xt")) xt = Double.parseDouble(value);
            else if (field.equals("k1")) k1 = Double.parseDouble(value);
            else if (field.equals("kt1")) kt1 = Double.parseDouble(value);
            else if (field.equals("kt1l")) kt1l = Double.parseDouble(value);
            else if (field.equals("kt2")) kt2 = Double.parseDouble(value);
            else if (field.equals("k2")) k2 = Double.parseDouble(value);
            else if (field.equals("k3")) k3 = Double.parseDouble(value);
            else if (field.equals("k3b")) k3b = Double.parseDouble(value);
            else if (field.equals("w0")) w0 = Double.parseDouble(value);
            else if (field.equals("nlx")) nlx = Double.parseDouble(value);
            else if (field.equals("dvt0")) dvt0 = Double.parseDouble(value);
            else if (field.equals("dvt1")) dvt1 = Double.parseDouble(value);
            else if (field.equals("dvt2")) dvt2 = Double.parseDouble(value);
            else if (field.equals("dvt0w")) dvt0w = Double.parseDouble(value);
            else if (field.equals("dvt1w")) dvt1w = Double.parseDouble(value);
            else if (field.equals("dvt2w")) dvt2w = Double.parseDouble(value);
            else if (field.equals("drout")) drout = Double.parseDouble(value);
            else if (field.equals("dsub")) dsub = Double.parseDouble(value);
            else if (field.equals("vth0")) vth0 = Double.parseDouble(value);
            else if (field.equals("ua")) ua = Double.parseDouble(value);
            else if (field.equals("ua1")) ua1 = Double.parseDouble(value);
            else if (field.equals("ub")) ub = Double.parseDouble(value);
            else if (field.equals("ub1")) ub1 = Double.parseDouble(value);
            else if (field.equals("uc")) uc = Double.parseDouble(value);
            else if (field.equals("uc1")) uc1 = Double.parseDouble(value);
            else if (field.equals("u0")) u0 = Double.parseDouble(value);
            else if (field.equals("ute")) ute = Double.parseDouble(value);
            else if (field.equals("voff")) voff = Double.parseDouble(value);
            else if (field.equals("delta")) delta = Double.parseDouble(value);
            else if (field.equals("rdsw")) rdsw = Double.parseDouble(value);
            else if (field.equals("prwg")) prwg = Double.parseDouble(value);
            else if (field.equals("prwb")) prwb = Double.parseDouble(value);
            else if (field.equals("prt")) prt = Double.parseDouble(value);
            else if (field.equals("eta0")) eta0 = Double.parseDouble(value);
            else if (field.equals("etab")) etab = Double.parseDouble(value);
            else if (field.equals("pclm")) pclm = Double.parseDouble(value);
            else if (field.equals("pdiblc1")) pdibl1 = Double.parseDouble(value);
            else if (field.equals("pdiblc2")) pdibl2 = Double.parseDouble(value);
            else if (field.equals("pdiblcb")) pdiblb = Double.parseDouble(value);
            else if (field.equals("pscbe1")) pscbe1 = Double.parseDouble(value);
            else if (field.equals("pscbe2")) pscbe2 = Double.parseDouble(value);
            else if (field.equals("pvag")) pvag = Double.parseDouble(value);
            else if (field.equals("wr")) wr = Double.parseDouble(value);
            else if (field.equals("dwg")) dwg = Double.parseDouble(value);
            else if (field.equals("dwb")) dwb = Double.parseDouble(value);
            else if (field.equals("b0")) b0 = Double.parseDouble(value);
            else if (field.equals("b1")) b1 = Double.parseDouble(value);
            else if (field.equals("alpha0")) alpha0 = Double.parseDouble(value);
            else if (field.equals("alpha1")) alpha1 = Double.parseDouble(value);
            else if (field.equals("beta0")) beta0 = Double.parseDouble(value);
            else if (field.equals("ijth")) ijth = Double.parseDouble(value);
            else if (field.equals("vfb")) vfb = Double.parseDouble(value);
            else if (field.equals("elm")) elm = Double.parseDouble(value);
            else if (field.equals("cgsl")) cgsl = Double.parseDouble(value);
            else if (field.equals("cgdl")) cgdl = Double.parseDouble(value);
            else if (field.equals("ckappa")) ckappa = Double.parseDouble(value);
            else if (field.equals("cf")) cf = Double.parseDouble(value);
            else if (field.equals("vfbcv")) vfbcv = Double.parseDouble(value);
            else if (field.equals("clc")) clc = Double.parseDouble(value);
            else if (field.equals("cle")) cle = Double.parseDouble(value);
            else if (field.equals("dwc")) dwc = Double.parseDouble(value);
            else if (field.equals("dlc")) dlc = Double.parseDouble(value);
            else if (field.equals("noff")) noff = Double.parseDouble(value);
            else if (field.equals("voffcv")) voffcv = Double.parseDouble(value);
            else if (field.equals("acde")) acde = Double.parseDouble(value);
            else if (field.equals("moin")) moin = Double.parseDouble(value);
            else if (field.equals("tcj")) tcj = Double.parseDouble(value);
            else if (field.equals("tcjsw")) tcjsw = Double.parseDouble(value);
            else if (field.equals("tcjswg")) tcjswg = Double.parseDouble(value);
            else if (field.equals("tpb")) tpb = Double.parseDouble(value);
            else if (field.equals("tpbsw")) tpbsw = Double.parseDouble(value);
            else if (field.equals("tpbswg")) tpbswg = Double.parseDouble(value);
            else if (field.equals("tnom")) tnom = Double.parseDouble(value);
            else if (field.equals("cgso")) cgso = Double.parseDouble(value);
            else if (field.equals("cgdo")) cgdo = Double.parseDouble(value);
            else if (field.equals("cgbo")) cgbo = Double.parseDouble(value);
            else if (field.equals("xpart")) xpart = Double.parseDouble(value);
            else if (field.equals("rsh")) sheetResistance = Double.parseDouble(value);
            else if (field.equals("js")) jctSatCurDensity = Double.parseDouble(value);
            else if (field.equals("jsw")) jctSidewallSatCurDensity = Double.parseDouble(value);
            else if (field.equals("pb")) bulkJctPotential = Double.parseDouble(value);
            else if (field.equals("mj")) bulkJctBotGradingCoeff = Double.parseDouble(value);
            else if (field.equals("mjsw")) bulkJctSideGradingCoeff = Double.parseDouble(value);
            else if (field.equals("mjswg")) bulkJctGateSideGradingCoeff = Double.parseDouble(value);
            else if (field.equals("pbsw")) sidewallJctPotential = Double.parseDouble(value);
            else if (field.equals("pbswg")) GatesidewallJctPotential = Double.parseDouble(value);
            else if (field.equals("cj")) unitAreaJctCap = Double.parseDouble(value);
            else if (field.equals("cjsw")) unitLengthSidewallJctCap = Double.parseDouble(value);
            else if (field.equals("cjswg")) unitLengthGateSidewallJctCap = Double.parseDouble(value);
            else if (field.equals("nj")) jctEmissionCoeff = Double.parseDouble(value);
            else if (field.equals("xti")) jctTempExponent = Double.parseDouble(value);
            else if (field.equals("lint")) Lint = Double.parseDouble(value);
            else if (field.equals("ll")) Ll = Double.parseDouble(value);
            else if (field.equals("llc")) Llc = Double.parseDouble(value);
            else if (field.equals("lln")) Lln = Double.parseDouble(value);
            else if (field.equals("lw")) Lw = Double.parseDouble(value);
            else if (field.equals("lwc")) Lwc = Double.parseDouble(value);
            else if (field.equals("lwn")) Lwn = Double.parseDouble(value);
            else if (field.equals("lwl")) Lwl = Double.parseDouble(value);
            else if (field.equals("lwlc")) Lwlc = Double.parseDouble(value);
            else if (field.equals("lmin")) Lmin = Double.parseDouble(value);
            else if (field.equals("lmax")) Lmax = Double.parseDouble(value);
            else if (field.equals("wint")) Wint = Double.parseDouble(value);
            else if (field.equals("wl")) Wl = Double.parseDouble(value);
            else if (field.equals("wlc")) Wlc = Double.parseDouble(value);
            else if (field.equals("wln")) Wln = Double.parseDouble(value);
            else if (field.equals("ww")) Ww = Double.parseDouble(value);
            else if (field.equals("wwc")) Wwc = Double.parseDouble(value);
            else if (field.equals("wwn")) Wwn = Double.parseDouble(value);
            else if (field.equals("wwl")) Wwl = Double.parseDouble(value);
            else if (field.equals("wwlc")) Wwlc = Double.parseDouble(value);
            else if (field.equals("wmin")) Wmin = Double.parseDouble(value);
            else if (field.equals("wmax")) Wmax = Double.parseDouble(value);
            else if (field.equals("lcdsc")) lcdsc = Double.parseDouble(value);
            else if (field.equals("lcdscb")) lcdscb = Double.parseDouble(value);
            else if (field.equals("lcdscd")) lcdscd = Double.parseDouble(value);
            else if (field.equals("lcit")) lcit = Double.parseDouble(value);
            else if (field.equals("lnfactor")) lnfactor = Double.parseDouble(value);
            else if (field.equals("lxj")) lxj = Double.parseDouble(value);
            else if (field.equals("lvsat")) lvsat = Double.parseDouble(value);
            else if (field.equals("lat")) lat = Double.parseDouble(value);
            else if (field.equals("la0")) la0 = Double.parseDouble(value);
            else if (field.equals("lags")) lags = Double.parseDouble(value);
            else if (field.equals("la1")) la1 = Double.parseDouble(value);
            else if (field.equals("la2")) la2 = Double.parseDouble(value);
            else if (field.equals("lketa")) lketa = Double.parseDouble(value);
            else if (field.equals("lnsub")) lnsub = Double.parseDouble(value);
            else if (field.equals("lnch")) lnpeak = Double.parseDouble(value);
            else if (field.equals("lngate")) lngate = Double.parseDouble(value);
            else if (field.equals("lgamma1")) lgamma1 = Double.parseDouble(value);
            else if (field.equals("lgamma2")) lgamma2 = Double.parseDouble(value);
            else if (field.equals("lvbx")) lvbx = Double.parseDouble(value);
            else if (field.equals("lvbm")) lvbm = Double.parseDouble(value);
            else if (field.equals("lxt")) lxt = Double.parseDouble(value);
            else if (field.equals("lk1")) lk1 = Double.parseDouble(value);
            else if (field.equals("lkt1")) lkt1 = Double.parseDouble(value);
            else if (field.equals("lkt1l")) lkt1l = Double.parseDouble(value);
            else if (field.equals("lkt2")) lkt2 = Double.parseDouble(value);
            else if (field.equals("lk2")) lk2 = Double.parseDouble(value);
            else if (field.equals("lk3")) lk3 = Double.parseDouble(value);
            else if (field.equals("lk3b")) lk3b = Double.parseDouble(value);
            else if (field.equals("lw0")) lw0 = Double.parseDouble(value);
            else if (field.equals("lnlx")) lnlx = Double.parseDouble(value);
            else if (field.equals("ldvt0")) ldvt0 = Double.parseDouble(value);
            else if (field.equals("ldvt1")) ldvt1 = Double.parseDouble(value);
            else if (field.equals("ldvt2")) ldvt2 = Double.parseDouble(value);
            else if (field.equals("ldvt0w")) ldvt0w = Double.parseDouble(value);
            else if (field.equals("ldvt1w")) ldvt1w = Double.parseDouble(value);
            else if (field.equals("ldvt2w")) ldvt2w = Double.parseDouble(value);
            else if (field.equals("ldrout")) ldrout = Double.parseDouble(value);
            else if (field.equals("ldsub")) ldsub = Double.parseDouble(value);
            else if (field.equals("lvth0")) lvth0 = Double.parseDouble(value);
            else if (field.equals("lua")) lua = Double.parseDouble(value);
            else if (field.equals("lua1")) lua1 = Double.parseDouble(value);
            else if (field.equals("lub")) lub = Double.parseDouble(value);
            else if (field.equals("lub1")) lub1 = Double.parseDouble(value);
            else if (field.equals("luc")) luc = Double.parseDouble(value);
            else if (field.equals("luc1")) luc1 = Double.parseDouble(value);
            else if (field.equals("lu0")) lu0 = Double.parseDouble(value);
            else if (field.equals("lute")) lute = Double.parseDouble(value);
            else if (field.equals("lvoff")) lvoff = Double.parseDouble(value);
            else if (field.equals("ldelta")) ldelta = Double.parseDouble(value);
            else if (field.equals("lrdsw")) lrdsw = Double.parseDouble(value);
            else if (field.equals("lprwg")) lprwg = Double.parseDouble(value);
            else if (field.equals("lprwb")) lprwb = Double.parseDouble(value);
            else if (field.equals("lprt")) lprt = Double.parseDouble(value);
            else if (field.equals("leta0")) leta0 = Double.parseDouble(value);
            else if (field.equals("letab")) letab = Double.parseDouble(value);
            else if (field.equals("lpclm")) lpclm = Double.parseDouble(value);
            else if (field.equals("lpdiblc1")) lpdibl1 = Double.parseDouble(value);
            else if (field.equals("lpdiblc2")) lpdibl2 = Double.parseDouble(value);
            else if (field.equals("lpdiblcb")) lpdiblb = Double.parseDouble(value);
            else if (field.equals("lpscbe1")) lpscbe1 = Double.parseDouble(value);
            else if (field.equals("lpscbe2")) lpscbe2 = Double.parseDouble(value);
            else if (field.equals("lpvag")) lpvag = Double.parseDouble(value);
            else if (field.equals("lwr")) lwr = Double.parseDouble(value);
            else if (field.equals("ldwg")) ldwg = Double.parseDouble(value);
            else if (field.equals("ldwb")) ldwb = Double.parseDouble(value);
            else if (field.equals("lb0")) lb0 = Double.parseDouble(value);
            else if (field.equals("lb1")) lb1 = Double.parseDouble(value);
            else if (field.equals("lalpha0")) lalpha0 = Double.parseDouble(value);
            else if (field.equals("lalpha1")) lalpha1 = Double.parseDouble(value);
            else if (field.equals("lbeta0")) lbeta0 = Double.parseDouble(value);
            else if (field.equals("lvfb")) lvfb = Double.parseDouble(value);
            else if (field.equals("lelm")) lelm = Double.parseDouble(value);
            else if (field.equals("lcgsl")) lcgsl = Double.parseDouble(value);
            else if (field.equals("lcgdl")) lcgdl = Double.parseDouble(value);
            else if (field.equals("lckappa")) lckappa = Double.parseDouble(value);
            else if (field.equals("lcg")) lcf = Double.parseDouble(value);
            else if (field.equals("lclc")) lclc = Double.parseDouble(value);
            else if (field.equals("lcle")) lcle = Double.parseDouble(value);
            else if (field.equals("lvfbcv")) lvfbcv = Double.parseDouble(value);
            else if (field.equals("lnoff")) lnoff = Double.parseDouble(value);
            else if (field.equals("lvoffcv")) lvoffcv = Double.parseDouble(value);
            else if (field.equals("lacde")) lacde = Double.parseDouble(value);
            else if (field.equals("lmoin")) lmoin = Double.parseDouble(value);
            else if (field.equals("wcdsc")) wcdsc = Double.parseDouble(value);
            else if (field.equals("wcdscb")) wcdscb = Double.parseDouble(value);
            else if (field.equals("wcdscd")) wcdscd = Double.parseDouble(value);
            else if (field.equals("wcit")) wcit = Double.parseDouble(value);
            else if (field.equals("wnfactor")) wnfactor = Double.parseDouble(value);
            else if (field.equals("wxj")) wxj = Double.parseDouble(value);
            else if (field.equals("wvsat")) wvsat = Double.parseDouble(value);
            else if (field.equals("wat")) wat = Double.parseDouble(value);
            else if (field.equals("wa0")) wa0 = Double.parseDouble(value);
            else if (field.equals("wags")) wags = Double.parseDouble(value);
            else if (field.equals("wa1")) wa1 = Double.parseDouble(value);
            else if (field.equals("wa2")) wa2 = Double.parseDouble(value);
            else if (field.equals("wketa")) wketa = Double.parseDouble(value);
            else if (field.equals("wnsub")) wnsub = Double.parseDouble(value);
            else if (field.equals("wnch")) wnpeak = Double.parseDouble(value);
            else if (field.equals("wngate")) wngate = Double.parseDouble(value);
            else if (field.equals("wgamma1")) wgamma1 = Double.parseDouble(value);
            else if (field.equals("wgamma2")) wgamma2 = Double.parseDouble(value);
            else if (field.equals("wvbx")) wvbx = Double.parseDouble(value);
            else if (field.equals("wvbm")) wvbm = Double.parseDouble(value);
            else if (field.equals("wxt")) wxt = Double.parseDouble(value);
            else if (field.equals("wk1")) wk1 = Double.parseDouble(value);
            else if (field.equals("wkt1")) wkt1 = Double.parseDouble(value);
            else if (field.equals("wkt1l")) wkt1l = Double.parseDouble(value);
            else if (field.equals("wkt2")) wkt2 = Double.parseDouble(value);
            else if (field.equals("wk2")) wk2 = Double.parseDouble(value);
            else if (field.equals("wk3")) wk3 = Double.parseDouble(value);
            else if (field.equals("wk3b")) wk3b = Double.parseDouble(value);
            else if (field.equals("ww0")) ww0 = Double.parseDouble(value);
            else if (field.equals("wnlx")) wnlx = Double.parseDouble(value);
            else if (field.equals("wdvt0")) wdvt0 = Double.parseDouble(value);
            else if (field.equals("wdvt1")) wdvt1 = Double.parseDouble(value);
            else if (field.equals("wdvt2")) wdvt2 = Double.parseDouble(value);
            else if (field.equals("wdvt0w")) wdvt0w = Double.parseDouble(value);
            else if (field.equals("wdvt1w")) wdvt1w = Double.parseDouble(value);
            else if (field.equals("wdvt2w")) wdvt2w = Double.parseDouble(value);
            else if (field.equals("wdrout")) wdrout = Double.parseDouble(value);
            else if (field.equals("wdsub")) wdsub = Double.parseDouble(value);
            else if (field.equals("wvth0")) wvth0 = Double.parseDouble(value);
            else if (field.equals("wua")) wua = Double.parseDouble(value);
            else if (field.equals("wua1")) wua1 = Double.parseDouble(value);
            else if (field.equals("wub")) wub = Double.parseDouble(value);
            else if (field.equals("wub1")) wub1 = Double.parseDouble(value);
            else if (field.equals("wuc")) wuc = Double.parseDouble(value);
            else if (field.equals("wuc1")) wuc1 = Double.parseDouble(value);
            else if (field.equals("wu0")) wu0 = Double.parseDouble(value);
            else if (field.equals("wute")) wute = Double.parseDouble(value);
            else if (field.equals("wvoff")) wvoff = Double.parseDouble(value);
            else if (field.equals("wdelta")) wdelta = Double.parseDouble(value);
            else if (field.equals("wrdsw")) wrdsw = Double.parseDouble(value);
            else if (field.equals("wprwg")) wprwg = Double.parseDouble(value);
            else if (field.equals("wprwb")) wprwb = Double.parseDouble(value);
            else if (field.equals("wprt")) wprt = Double.parseDouble(value);
            else if (field.equals("weta0")) weta0 = Double.parseDouble(value);
            else if (field.equals("wetab")) wetab = Double.parseDouble(value);
            else if (field.equals("wpclm")) wpclm = Double.parseDouble(value);
            else if (field.equals("wpdiblc1")) wpdibl1 = Double.parseDouble(value);
            else if (field.equals("wpdiblc2")) wpdibl2 = Double.parseDouble(value);
            else if (field.equals("wpdiblcb")) wpdiblb = Double.parseDouble(value);
            else if (field.equals("wpscbe1")) wpscbe1 = Double.parseDouble(value);
            else if (field.equals("wpscbe2")) wpscbe2 = Double.parseDouble(value);
            else if (field.equals("wpvag")) wpvag = Double.parseDouble(value);
            else if (field.equals("wwr")) wwr = Double.parseDouble(value);
            else if (field.equals("wdwg")) wdwg = Double.parseDouble(value);
            else if (field.equals("wdwb")) wdwb = Double.parseDouble(value);
            else if (field.equals("wb0")) wb0 = Double.parseDouble(value);
            else if (field.equals("wb1")) wb1 = Double.parseDouble(value);
            else if (field.equals("walpha0")) walpha0 = Double.parseDouble(value);
            else if (field.equals("walpha1")) walpha1 = Double.parseDouble(value);
            else if (field.equals("wbeta0")) wbeta0 = Double.parseDouble(value);
            else if (field.equals("wvfb")) wvfb = Double.parseDouble(value);
            else if (field.equals("welm")) welm = Double.parseDouble(value);
            else if (field.equals("wcgsl")) wcgsl = Double.parseDouble(value);
            else if (field.equals("wcgdl")) wcgdl = Double.parseDouble(value);
            else if (field.equals("wckappa")) wckappa = Double.parseDouble(value);
            else if (field.equals("wcg")) wcf = Double.parseDouble(value);
            else if (field.equals("wclc")) wclc = Double.parseDouble(value);
            else if (field.equals("wcle")) wcle = Double.parseDouble(value);
            else if (field.equals("wvfbcv")) wvfbcv = Double.parseDouble(value);
            else if (field.equals("wnoff")) wnoff = Double.parseDouble(value);
            else if (field.equals("wvoffcv")) wvoffcv = Double.parseDouble(value);
            else if (field.equals("wacde")) wacde = Double.parseDouble(value);
            else if (field.equals("wmoin")) wmoin = Double.parseDouble(value);
            else if (field.equals("pcdsc")) pcdsc = Double.parseDouble(value);
            else if (field.equals("pcdscb")) pcdscb = Double.parseDouble(value);
            else if (field.equals("pcdscd")) pcdscd = Double.parseDouble(value);
            else if (field.equals("pcit")) pcit = Double.parseDouble(value);
            else if (field.equals("pnfactor")) pnfactor = Double.parseDouble(value);
            else if (field.equals("pxj")) pxj = Double.parseDouble(value);
            else if (field.equals("pvsat")) pvsat = Double.parseDouble(value);
            else if (field.equals("pat")) pat = Double.parseDouble(value);
            else if (field.equals("pa0")) pa0 = Double.parseDouble(value);
            else if (field.equals("pags")) pags = Double.parseDouble(value);
            else if (field.equals("pa1")) pa1 = Double.parseDouble(value);
            else if (field.equals("pa2")) pa2 = Double.parseDouble(value);
            else if (field.equals("pketa")) pketa = Double.parseDouble(value);
            else if (field.equals("pnsub")) pnsub = Double.parseDouble(value);
            else if (field.equals("pnch")) pnpeak = Double.parseDouble(value);
            else if (field.equals("pngate")) pngate = Double.parseDouble(value);
            else if (field.equals("pgamma1")) pgamma1 = Double.parseDouble(value);
            else if (field.equals("pgamma2")) pgamma2 = Double.parseDouble(value);
            else if (field.equals("pvbx")) pvbx = Double.parseDouble(value);
            else if (field.equals("pvbm")) pvbm = Double.parseDouble(value);
            else if (field.equals("pxt")) pxt = Double.parseDouble(value);
            else if (field.equals("pk1")) pk1 = Double.parseDouble(value);
            else if (field.equals("pkt1")) pkt1 = Double.parseDouble(value);
            else if (field.equals("pkt1l")) pkt1l = Double.parseDouble(value);
            else if (field.equals("pkt2")) pkt2 = Double.parseDouble(value);
            else if (field.equals("pk2")) pk2 = Double.parseDouble(value);
            else if (field.equals("pk3")) pk3 = Double.parseDouble(value);
            else if (field.equals("pk3b")) pk3b = Double.parseDouble(value);
            else if (field.equals("pw0")) pw0 = Double.parseDouble(value);
            else if (field.equals("pnlx")) pnlx = Double.parseDouble(value);
            else if (field.equals("pdvt0")) pdvt0 = Double.parseDouble(value);
            else if (field.equals("pdvt1")) pdvt1 = Double.parseDouble(value);
            else if (field.equals("pdvt2")) pdvt2 = Double.parseDouble(value);
            else if (field.equals("pdvt0w")) pdvt0w = Double.parseDouble(value);
            else if (field.equals("pdvt1w")) pdvt1w = Double.parseDouble(value);
            else if (field.equals("pdvt2w")) pdvt2w = Double.parseDouble(value);
            else if (field.equals("pdrout")) pdrout = Double.parseDouble(value);
            else if (field.equals("pdsub")) pdsub = Double.parseDouble(value);
            else if (field.equals("pvth0")) pvth0 = Double.parseDouble(value);
            else if (field.equals("pua")) pua = Double.parseDouble(value);
            else if (field.equals("pua1")) pua1 = Double.parseDouble(value);
            else if (field.equals("pub")) pub = Double.parseDouble(value);
            else if (field.equals("pub1")) pub1 = Double.parseDouble(value);
            else if (field.equals("puc")) puc = Double.parseDouble(value);
            else if (field.equals("puc1")) puc1 = Double.parseDouble(value);
            else if (field.equals("pu0")) pu0 = Double.parseDouble(value);
            else if (field.equals("pute")) pute = Double.parseDouble(value);
            else if (field.equals("pvoff")) pvoff = Double.parseDouble(value);
            else if (field.equals("pdelta")) pdelta = Double.parseDouble(value);
            else if (field.equals("prdsw")) prdsw = Double.parseDouble(value);
            else if (field.equals("pprwg")) pprwg = Double.parseDouble(value);
            else if (field.equals("pprwb")) pprwb = Double.parseDouble(value);
            else if (field.equals("pprt")) pprt = Double.parseDouble(value);
            else if (field.equals("peta0")) peta0 = Double.parseDouble(value);
            else if (field.equals("petab")) petab = Double.parseDouble(value);
            else if (field.equals("ppclm")) ppclm = Double.parseDouble(value);
            else if (field.equals("ppdiblc1")) ppdibl1 = Double.parseDouble(value);
            else if (field.equals("ppdiblc2")) ppdibl2 = Double.parseDouble(value);
            else if (field.equals("ppdiblcb")) ppdiblb = Double.parseDouble(value);
            else if (field.equals("ppscbe1")) ppscbe1 = Double.parseDouble(value);
            else if (field.equals("ppscbe2")) ppscbe2 = Double.parseDouble(value);
            else if (field.equals("ppvag")) ppvag = Double.parseDouble(value);
            else if (field.equals("pwr")) pwr = Double.parseDouble(value);
            else if (field.equals("pdwg")) pdwg = Double.parseDouble(value);
            else if (field.equals("pdwb")) pdwb = Double.parseDouble(value);
            else if (field.equals("pb0")) pb0 = Double.parseDouble(value);
            else if (field.equals("pb1")) pb1 = Double.parseDouble(value);
            else if (field.equals("palpha0")) palpha0 = Double.parseDouble(value);
            else if (field.equals("palpha1")) palpha1 = Double.parseDouble(value);
            else if (field.equals("pbeta0")) pbeta0 = Double.parseDouble(value);
            else if (field.equals("pvfb")) pvfb = Double.parseDouble(value);
            else if (field.equals("pelm")) pelm = Double.parseDouble(value);
            else if (field.equals("pcgsl")) pcgsl = Double.parseDouble(value);
            else if (field.equals("pcgdl")) pcgdl = Double.parseDouble(value);
            else if (field.equals("pckappa")) pckappa = Double.parseDouble(value);
            else if (field.equals("pcg")) pcf = Double.parseDouble(value);
            else if (field.equals("pclc")) pclc = Double.parseDouble(value);
            else if (field.equals("pcle")) pcle = Double.parseDouble(value);
            else if (field.equals("pvfbcv")) pvfbcv = Double.parseDouble(value);
            else if (field.equals("pnoff")) pnoff = Double.parseDouble(value);
            else if (field.equals("pvoffcv")) pvoffcv = Double.parseDouble(value);
            else if (field.equals("pacde")) pacde = Double.parseDouble(value);
            else if (field.equals("pmoin")) pmoin = Double.parseDouble(value);
        } catch (NumberFormatException e) {
            System.out.println(value + " is not a number");
        }
    }

    void setDefaults() {
        if (!given(type)) type = NMOS;
        if (!given(mobMod)) mobMod = 1;
        if (!given(binUnit)) binUnit = 1;
        if (!given(paramChk)) paramChk = 0;
        if (!given(capMod)) capMod = 3;
        if (!given(noiMod)) noiMod = 1;
        if (!given(nqsMod)) nqsMod = 0;
        if (!given(version)) version = "3.2.2";
        if (!given(tox)) tox = 150.0e-10;
        cox = 3.453133e-11 / tox;
        if (!given(toxm)) toxm = tox;

        if (!given(cdsc)) cdsc = 2.4e-4;   /* unit Q/V/m^2  */
        if (!given(cdscb)) cdscb = 0.0;   /* unit Q/V/m^2  */
        if (!given(cdscd)) cdscd = 0.0;   /* unit Q/V/m^2  */
        if (!given(cit)) cit = 0.0;   /* unit Q/V/m^2  */
        if (!given(nfactor)) nfactor = 1;
        if (!given(xj)) xj = .15e-6;
        if (!given(vsat)) vsat = 8.0e4;   /* unit m/s */
        if (!given(at)) at = 3.3e4;   /* unit m/s */
        if (!given(a0)) a0 = 1.0;
        if (!given(ags)) ags = 0.0;
        if (!given(a1)) a1 = 0.0;
        if (!given(a2)) a2 = 1.0;
        if (!given(keta)) keta = -0.047;   /* unit  / V */
        if (!given(nsub) && !given(k1) && !given(k2)) nsub = 6.0e16;   /* unit 1/cm3 */
        if (!given(npeak)) npeak = 1.7e17;   /* unit 1/cm3 */
        if (!given(ngate)) ngate = 0;   /* unit 1/cm3 */
        if (!given(vbm)) vbm = -3.0;
        if (!given(xt) && !given(k1) && !given(k2)) xt = 1.55e-7;
        if (!given(kt1)) kt1 = -0.11;      /* unit V */
        if (!given(kt1l)) kt1l = 0.0;      /* unit V*m */
        if (!given(kt2)) kt2 = 0.022;      /* No unit */
        if (!given(k3)) k3 = 80.0;
        if (!given(k3b)) k3b = 0.0;
        if (!given(w0)) w0 = 2.5e-6;
        if (!given(nlx)) nlx = 1.74e-7;
        if (!given(dvt0)) dvt0 = 2.2;
        if (!given(dvt1)) dvt1 = 0.53;
        if (!given(dvt2)) dvt2 = -0.032;   /* unit 1 / V */

        if (!given(dvt0w)) dvt0w = 0.0;
        if (!given(dvt1w)) dvt1w = 5.3e6;
        if (!given(dvt2w)) dvt2w = -0.032;

        if (!given(drout)) drout = 0.56;
        if (!given(dsub)) dsub = drout;
        if (!given(vth0)) vth0 = (type == NMOS) ? 0.7 : -0.7;
        if (!given(ua)) ua = 2.25e-9;      /* unit m/V */
        if (!given(ua1)) ua1 = 4.31e-9;      /* unit m/V */
        if (!given(ub)) ub = 5.87e-19;     /* unit (m/V)**2 */
        if (!given(ub1)) ub1 = -7.61e-18;     /* unit (m/V)**2 */
        if (!given(uc)) uc = (mobMod == 3) ? -0.0465 : -0.0465e-9;
        if (!given(uc1)) uc1 = (mobMod == 3) ? -0.056 : -0.056e-9;
        if (!given(u0)) u0 = (type == NMOS) ? 0.067 : 0.025;
        if (!given(ute)) ute = -1.5;
        if (!given(voff)) voff = -0.08;
        if (!given(delta)) delta = 0.01;
        if (!given(rdsw)) rdsw = 0;
        if (!given(prwg)) prwg = 0.0;      /* unit 1/V */
        if (!given(prwb)) prwb = 0.0;
        if (!given(prt)) prt = 0.0;
        if (!given(eta0)) eta0 = 0.08;      /* no unit  */
        if (!given(etab)) etab = -0.07;      /* unit  1/V */
        if (!given(pclm)) pclm = 1.3;      /* no unit  */
        if (!given(pdibl1)) pdibl1 = .39;    /* no unit  */
        if (!given(pdibl2)) pdibl2 = 0.0086;    /* no unit  */
        if (!given(pdiblb)) pdiblb = 0.0;    /* 1/V  */
        if (!given(pscbe1)) pscbe1 = 4.24e8;
        if (!given(pscbe2)) pscbe2 = 1.0e-5;
        if (!given(pvag)) pvag = 0.0;
        if (!given(wr)) wr = 1.0;
        if (!given(dwg)) dwg = 0.0;
        if (!given(dwb)) dwb = 0.0;
        if (!given(b0)) b0 = 0.0;
        if (!given(b1)) b1 = 0.0;
        if (!given(alpha0)) alpha0 = 0.0;
        if (!given(alpha1)) alpha1 = 0.0;
        if (!given(beta0)) beta0 = 30.0;
        if (!given(ijth)) ijth = 0.1; /* unit A */

        if (!given(elm)) elm = 5.0;
        if (!given(cgsl)) cgsl = 0.0;
        if (!given(cgdl)) cgdl = 0.0;
        if (!given(ckappa)) ckappa = 0.6;
        if (!given(clc)) clc = 0.1e-6;
        if (!given(cle)) cle = 0.6;
        if (!given(vfbcv)) vfbcv = -1.0;
        if (!given(acde)) acde = 1.0;
        if (!given(moin)) moin = 15.0;
        if (!given(noff)) noff = 1.0;
        if (!given(voffcv)) voffcv = 0.0;
        if (!given(tcj)) tcj = 0.0;
        if (!given(tpb)) tpb = 0.0;
        if (!given(tcjsw)) tcjsw = 0.0;
        if (!given(tpbsw)) tpbsw = 0.0;
        if (!given(tcjswg)) tcjswg = 0.0;
        if (!given(tpbswg)) tpbswg = 0.0;

  /* Length dependence */
        if (!given(lcdsc)) lcdsc = 0.0;
        if (!given(lcdscb)) lcdscb = 0.0;
        if (!given(lcdscd)) lcdscd = 0.0;
        if (!given(lcit)) lcit = 0.0;
        if (!given(lnfactor)) lnfactor = 0.0;
        if (!given(lxj)) lxj = 0.0;
        if (!given(lvsat)) lvsat = 0.0;
        if (!given(lat)) lat = 0.0;
        if (!given(la0)) la0 = 0.0;
        if (!given(lags)) lags = 0.0;
        if (!given(la1)) la1 = 0.0;
        if (!given(la2)) la2 = 0.0;
        if (!given(lketa)) lketa = 0.0;
        if (!given(lnsub)) lnsub = 0.0;
        if (!given(lnpeak)) lnpeak = 0.0;
        if (!given(lngate)) lngate = 0.0;
        if (!given(lvbm)) lvbm = 0.0;
        if (!given(lxt)) lxt = 0.0;
        if (!given(lkt1)) lkt1 = 0.0;
        if (!given(lkt1l)) lkt1l = 0.0;
        if (!given(lkt2)) lkt2 = 0.0;
        if (!given(lk3)) lk3 = 0.0;
        if (!given(lk3b)) lk3b = 0.0;
        if (!given(lw0)) lw0 = 0.0;
        if (!given(lnlx)) lnlx = 0.0;
        if (!given(ldvt0)) ldvt0 = 0.0;
        if (!given(ldvt1)) ldvt1 = 0.0;
        if (!given(ldvt2)) ldvt2 = 0.0;
        if (!given(ldvt0w)) ldvt0w = 0.0;
        if (!given(ldvt1w)) ldvt1w = 0.0;
        if (!given(ldvt2w)) ldvt2w = 0.0;
        if (!given(ldrout)) ldrout = 0.0;
        if (!given(ldsub)) ldsub = 0.0;
        if (!given(lvth0)) lvth0 = 0.0;
        if (!given(lua)) lua = 0.0;
        if (!given(lua1)) lua1 = 0.0;
        if (!given(lub)) lub = 0.0;
        if (!given(lub1)) lub1 = 0.0;
        if (!given(luc)) luc = 0.0;
        if (!given(luc1)) luc1 = 0.0;
        if (!given(lu0)) lu0 = 0.0;
        if (!given(lute)) lute = 0.0;
        if (!given(lvoff)) lvoff = 0.0;
        if (!given(ldelta)) ldelta = 0.0;
        if (!given(lrdsw)) lrdsw = 0.0;
        if (!given(lprwb)) lprwb = 0.0;
        if (!given(lprwg)) lprwg = 0.0;
        if (!given(lprt)) lprt = 0.0;
        if (!given(leta0)) leta0 = 0.0;
        if (!given(letab)) letab = -0.0;
        if (!given(lpclm)) lpclm = 0.0;
        if (!given(lpdibl1)) lpdibl1 = 0.0;
        if (!given(lpdibl2)) lpdibl2 = 0.0;
        if (!given(lpdiblb)) lpdiblb = 0.0;
        if (!given(lpscbe1)) lpscbe1 = 0.0;
        if (!given(lpscbe2)) lpscbe2 = 0.0;
        if (!given(lpvag)) lpvag = 0.0;
        if (!given(lwr)) lwr = 0.0;
        if (!given(ldwg)) ldwg = 0.0;
        if (!given(ldwb)) ldwb = 0.0;
        if (!given(lb0)) lb0 = 0.0;
        if (!given(lb1)) lb1 = 0.0;
        if (!given(lalpha0)) lalpha0 = 0.0;
        if (!given(lalpha1)) lalpha1 = 0.0;
        if (!given(lbeta0)) lbeta0 = 0.0;
        if (!given(lvfb)) lvfb = 0.0;

        if (!given(lelm)) lelm = 0.0;
        if (!given(lcgsl)) lcgsl = 0.0;
        if (!given(lcgdl)) lcgdl = 0.0;
        if (!given(lckappa)) lckappa = 0.0;
        if (!given(lclc)) lclc = 0.0;
        if (!given(lcle)) lcle = 0.0;
        if (!given(lcf)) lcf = 0.0;
        if (!given(lvfbcv)) lvfbcv = 0.0;
        if (!given(lacde)) lacde = 0.0;
        if (!given(lmoin)) lmoin = 0.0;
        if (!given(lnoff)) lnoff = 0.0;
        if (!given(lvoffcv)) lvoffcv = 0.0;

  /* Width dependence */
        if (!given(wcdsc)) wcdsc = 0.0;
        if (!given(wcdscb)) wcdscb = 0.0;
        if (!given(wcdscd)) wcdscd = 0.0;
        if (!given(wcit)) wcit = 0.0;
        if (!given(wnfactor)) wnfactor = 0.0;
        if (!given(wxj)) wxj = 0.0;
        if (!given(wvsat)) wvsat = 0.0;
        if (!given(wat)) wat = 0.0;
        if (!given(wa0)) wa0 = 0.0;
        if (!given(wags)) wags = 0.0;
        if (!given(wa1)) wa1 = 0.0;
        if (!given(wa2)) wa2 = 0.0;
        if (!given(wketa)) wketa = 0.0;
        if (!given(wnsub)) wnsub = 0.0;
        if (!given(wnpeak)) wnpeak = 0.0;
        if (!given(wngate)) wngate = 0.0;
        if (!given(wvbm)) wvbm = 0.0;
        if (!given(wxt)) wxt = 0.0;
        if (!given(wkt1)) wkt1 = 0.0;
        if (!given(wkt1l)) wkt1l = 0.0;
        if (!given(wkt2)) wkt2 = 0.0;
        if (!given(wk3)) wk3 = 0.0;
        if (!given(wk3b)) wk3b = 0.0;
        if (!given(ww0)) ww0 = 0.0;
        if (!given(wnlx)) wnlx = 0.0;
        if (!given(wdvt0)) wdvt0 = 0.0;
        if (!given(wdvt1)) wdvt1 = 0.0;
        if (!given(wdvt2)) wdvt2 = 0.0;
        if (!given(wdvt0w)) wdvt0w = 0.0;
        if (!given(wdvt1w)) wdvt1w = 0.0;
        if (!given(wdvt2w)) wdvt2w = 0.0;
        if (!given(wdrout)) wdrout = 0.0;
        if (!given(wdsub)) wdsub = 0.0;
        if (!given(wvth0)) wvth0 = 0.0;
        if (!given(wua)) wua = 0.0;
        if (!given(wua1)) wua1 = 0.0;
        if (!given(wub)) wub = 0.0;
        if (!given(wub1)) wub1 = 0.0;
        if (!given(wuc)) wuc = 0.0;
        if (!given(wuc1)) wuc1 = 0.0;
        if (!given(wu0)) wu0 = 0.0;
        if (!given(wute)) wute = 0.0;
        if (!given(wvoff)) wvoff = 0.0;
        if (!given(wdelta)) wdelta = 0.0;
        if (!given(wrdsw)) wrdsw = 0.0;
        if (!given(wprwb)) wprwb = 0.0;
        if (!given(wprwg)) wprwg = 0.0;
        if (!given(wprt)) wprt = 0.0;
        if (!given(weta0)) weta0 = 0.0;
        if (!given(wetab)) wetab = 0.0;
        if (!given(wpclm)) wpclm = 0.0;
        if (!given(wpdibl1)) wpdibl1 = 0.0;
        if (!given(wpdibl2)) wpdibl2 = 0.0;
        if (!given(wpdiblb)) wpdiblb = 0.0;
        if (!given(wpscbe1)) wpscbe1 = 0.0;
        if (!given(wpscbe2)) wpscbe2 = 0.0;
        if (!given(wpvag)) wpvag = 0.0;
        if (!given(wwr)) wwr = 0.0;
        if (!given(wdwg)) wdwg = 0.0;
        if (!given(wdwb)) wdwb = 0.0;
        if (!given(wb0)) wb0 = 0.0;
        if (!given(wb1)) wb1 = 0.0;
        if (!given(walpha0)) walpha0 = 0.0;
        if (!given(walpha1)) walpha1 = 0.0;
        if (!given(wbeta0)) wbeta0 = 0.0;
        if (!given(wvfb)) wvfb = 0.0;

        if (!given(welm)) welm = 0.0;
        if (!given(wcgsl)) wcgsl = 0.0;
        if (!given(wcgdl)) wcgdl = 0.0;
        if (!given(wckappa)) wckappa = 0.0;
        if (!given(wcf)) wcf = 0.0;
        if (!given(wclc)) wclc = 0.0;
        if (!given(wcle)) wcle = 0.0;
        if (!given(wvfbcv)) wvfbcv = 0.0;
        if (!given(wacde)) wacde = 0.0;
        if (!given(wmoin)) wmoin = 0.0;
        if (!given(wnoff)) wnoff = 0.0;
        if (!given(wvoffcv)) wvoffcv = 0.0;

  /* Cross-term dependence */
        if (!given(pcdsc)) pcdsc = 0.0;
        if (!given(pcdscb)) pcdscb = 0.0;
        if (!given(pcdscd)) pcdscd = 0.0;
        if (!given(pcit)) pcit = 0.0;
        if (!given(pnfactor)) pnfactor = 0.0;
        if (!given(pxj)) pxj = 0.0;
        if (!given(pvsat)) pvsat = 0.0;
        if (!given(pat)) pat = 0.0;
        if (!given(pa0)) pa0 = 0.0;

        if (!given(pags)) pags = 0.0;
        if (!given(pa1)) pa1 = 0.0;
        if (!given(pa2)) pa2 = 0.0;
        if (!given(pketa)) pketa = 0.0;
        if (!given(pnsub)) pnsub = 0.0;
        if (!given(pnpeak)) pnpeak = 0.0;
        if (!given(pngate)) pngate = 0.0;
        if (!given(pvbm)) pvbm = 0.0;
        if (!given(pxt)) pxt = 0.0;
        if (!given(pkt1)) pkt1 = 0.0;
        if (!given(pkt1l)) pkt1l = 0.0;
        if (!given(pkt2)) pkt2 = 0.0;
        if (!given(pk3)) pk3 = 0.0;
        if (!given(pk3b)) pk3b = 0.0;
        if (!given(pw0)) pw0 = 0.0;
        if (!given(pnlx)) pnlx = 0.0;
        if (!given(pdvt0)) pdvt0 = 0.0;
        if (!given(pdvt1)) pdvt1 = 0.0;
        if (!given(pdvt2)) pdvt2 = 0.0;
        if (!given(pdvt0w)) pdvt0w = 0.0;
        if (!given(pdvt1w)) pdvt1w = 0.0;
        if (!given(pdvt2w)) pdvt2w = 0.0;
        if (!given(pdrout)) pdrout = 0.0;
        if (!given(pdsub)) pdsub = 0.0;
        if (!given(pvth0)) pvth0 = 0.0;
        if (!given(pua)) pua = 0.0;
        if (!given(pua1)) pua1 = 0.0;
        if (!given(pub)) pub = 0.0;
        if (!given(pub1)) pub1 = 0.0;
        if (!given(puc)) puc = 0.0;
        if (!given(puc1)) puc1 = 0.0;
        if (!given(pu0)) pu0 = 0.0;
        if (!given(pute)) pute = 0.0;
        if (!given(pvoff)) pvoff = 0.0;
        if (!given(pdelta)) pdelta = 0.0;
        if (!given(prdsw)) prdsw = 0.0;
        if (!given(pprwb)) pprwb = 0.0;
        if (!given(pprwg)) pprwg = 0.0;
        if (!given(pprt)) pprt = 0.0;
        if (!given(peta0)) peta0 = 0.0;
        if (!given(petab)) petab = 0.0;
        if (!given(ppclm)) ppclm = 0.0;
        if (!given(ppdibl1)) ppdibl1 = 0.0;
        if (!given(ppdibl2)) ppdibl2 = 0.0;
        if (!given(ppdiblb)) ppdiblb = 0.0;
        if (!given(ppscbe1)) ppscbe1 = 0.0;
        if (!given(ppscbe2)) ppscbe2 = 0.0;
        if (!given(ppvag)) ppvag = 0.0;
        if (!given(pwr)) pwr = 0.0;
        if (!given(pdwg)) pdwg = 0.0;
        if (!given(pdwb)) pdwb = 0.0;
        if (!given(pb0)) pb0 = 0.0;
        if (!given(pb1)) pb1 = 0.0;
        if (!given(palpha0)) palpha0 = 0.0;
        if (!given(palpha1)) palpha1 = 0.0;
        if (!given(pbeta0)) pbeta0 = 0.0;
        if (!given(pvfb)) pvfb = 0.0;

        if (!given(pelm)) pelm = 0.0;
        if (!given(pcgsl)) pcgsl = 0.0;
        if (!given(pcgdl)) pcgdl = 0.0;
        if (!given(pckappa)) pckappa = 0.0;
        if (!given(pcf)) pcf = 0.0;
        if (!given(pclc)) pclc = 0.0;
        if (!given(pcle)) pcle = 0.0;
        if (!given(pvfbcv)) pvfbcv = 0.0;
        if (!given(pacde)) pacde = 0.0;
        if (!given(pmoin)) pmoin = 0.0;
        if (!given(pnoff)) pnoff = 0.0;
        if (!given(pvoffcv)) pvoffcv = 0.0;

  /* unit degree celcius */
        if (!given(tnom)) tnom = 27;
        tnom += CONSTCtoK;

        if (!given(Lint)) Lint = 0.0;
        if (!given(Ll)) Ll = 0.0;
        if (!given(Llc)) Llc = Ll;
        if (!given(Lln)) Lln = 1.0;
        if (!given(Lw)) Lw = 0.0;
        if (!given(Lwc)) Lwc = Lw;
        if (!given(Lwn)) Lwn = 1.0;
        if (!given(Lwl)) Lwl = 0.0;
        if (!given(Lwlc)) Lwlc = Lwl;
        if (!given(Lmin)) Lmin = 0.0;
        if (!given(Lmax)) Lmax = 1.0;
        if (!given(Wint)) Wint = 0.0;
        if (!given(Wl)) Wl = 0.0;
        if (!given(Wlc)) Wlc = Wl;
        if (!given(Wln)) Wln = 1.0;
        if (!given(Ww)) Ww = 0.0;
        if (!given(Wwc)) Wwc = Ww;
        if (!given(Wwn)) Wwn = 1.0;
        if (!given(Wwl)) Wwl = 0.0;
        if (!given(Wwlc)) Wwlc = Wwl;
        if (!given(Wmin)) Wmin = 0.0;
        if (!given(Wmax)) Wmax = 1.0;
        if (!given(dwc)) dwc = Wint;
        if (!given(dlc)) dlc = Lint;
        if (!given(cf)) cf = 2.0 * EPSOX / Math.PI * Math.log(1.0 + 0.4e-6 / tox);
        if (!given(cgdo)) {
            if (given(dlc) && (dlc > 0.0))
                cgdo = dlc * cox - cgdl;
            else
                cgdo = 0.6 * xj * cox;
        }
        if (!given(cgso)) {
            if (given(dlc) && (dlc > 0.0))
                cgso = dlc * cox - cgsl;
            else
                cgso = 0.6 * xj * cox;
        }

        if (!given(cgbo)) cgbo = 2.0 * dwc * cox;
        if (!given(xpart)) xpart = 0.0;
        if (!given(sheetResistance)) sheetResistance = 0.0;
        if (!given(unitAreaJctCap)) unitAreaJctCap = 5.0E-4;
        if (!given(unitLengthSidewallJctCap)) unitLengthSidewallJctCap = 5.0E-10;
        if (!given(unitLengthGateSidewallJctCap)) unitLengthGateSidewallJctCap = unitLengthSidewallJctCap;
        if (!given(jctSatCurDensity)) jctSatCurDensity = 1.0E-4;
        if (!given(jctSidewallSatCurDensity)) jctSidewallSatCurDensity = 0.0;
        if (!given(bulkJctPotential)) bulkJctPotential = 1.0;
        if (!given(sidewallJctPotential)) sidewallJctPotential = 1.0;
        if (!given(GatesidewallJctPotential)) GatesidewallJctPotential = sidewallJctPotential;
        if (!given(bulkJctBotGradingCoeff)) bulkJctBotGradingCoeff = 0.5;
        if (!given(bulkJctSideGradingCoeff)) bulkJctSideGradingCoeff = 0.33;
        if (!given(bulkJctGateSideGradingCoeff)) bulkJctGateSideGradingCoeff = bulkJctSideGradingCoeff;
        if (!given(jctEmissionCoeff)) jctEmissionCoeff = 1.0;
        if (!given(jctTempExponent)) jctTempExponent = 3.0;
        if (!given(oxideTrapDensityA)) {
            if (type == NMOS)
                oxideTrapDensityA = 1e20;
            else
                oxideTrapDensityA=9.9e18;
        }
        if (!given(oxideTrapDensityB)) {
            if (type == NMOS)
                oxideTrapDensityB = 5e4;
            else
                oxideTrapDensityB = 2.4e3;
        }
        if (!given(oxideTrapDensityC)) {
            if (type == NMOS)
                oxideTrapDensityC = -1.4e-12;
            else
                oxideTrapDensityC = 1.4e-12;
        }

        if (!given(em)) em = 4.1e7; /* V/m */
        if (!given(ef)) ef = 1.0;
        if (!given(af)) af = 1.0;
        if (!given(kf)) kf = 0.0;

// These lines were added by me (tedv) because I think
// the original bsim3 code had a bug where it didn't check them.
        if (!given(lk2)) lk2 = 0.0;
        if (!given(wk2)) wk2 = 0.0;
        if (!given(pk2)) pk2 = 0.0;
    }

    public void computeTemperatureData() {
        double Temp,Tnom,TRatio,Vtm0,Eg0,ni,Eg,T0,T1,delTemp;
      
        if (bulkJctPotential < 0.1)
          {
          bulkJctPotential = 0.1;
          System.out.println("Given pb is less than 0.1. Pb is set to 0.1.");
          }
        if (sidewallJctPotential < 0.1)
          {
          sidewallJctPotential = 0.1;
          System.out.println("Given pbsw is less than 0.1. Pbsw is set to 0.1.");
          }
        if (GatesidewallJctPotential < 0.1)
          {
          GatesidewallJctPotential = 0.1;
          System.out.println("Given pbswg is less than 0.1. Pbswg is set to 0.1.");
          }
      
        Temp = temp;
        Tnom = tnom;
        TRatio = Temp / Tnom;
      
        vcrit = CONSTvt0 * Math.log(CONSTvt0 / (CONSTroot2 * 1.0e-14));
        factor1 = Math.sqrt(EPSSI / EPSOX * tox);
      
        Vtm0 = KboQ * Tnom;
        Eg0 = 1.16 - 7.02e-4 * Tnom * Tnom / (Tnom + 1108.0);
        ni = 1.45e10 * (Tnom / 300.15) * Math.sqrt(Tnom / 300.15)
            * Math.exp(21.5565981 - Eg0 / (2.0 * Vtm0));
      
        vtm = KboQ * Temp;
        Eg = 1.16 - 7.02e-4 * Temp * Temp / (Temp + 1108.0);
        if (Temp != Tnom)
          {
          T0 = Eg0 / Vtm0 - Eg / vtm + jctTempExponent * Math.log(Temp / Tnom);
          T1 = Math.exp(T0 / jctEmissionCoeff);
          jctTempSatCurDensity = jctSatCurDensity * T1;
          jctSidewallTempSatCurDensity = jctSidewallSatCurDensity * T1;
          }
        else
          {
          jctTempSatCurDensity = jctSatCurDensity;
          jctSidewallTempSatCurDensity = jctSidewallSatCurDensity;
          }
      
        if (jctTempSatCurDensity < 0.0)
          jctTempSatCurDensity = 0.0;
        if (jctSidewallTempSatCurDensity < 0.0)
          jctSidewallTempSatCurDensity = 0.0;
      
        /* Temperature dependence of D/B and S/B diode capacitance begins */
        delTemp = Temp - tnom;
        T0 = tcj * delTemp;
        if (T0 >= -1.0)
          unitAreaJctCap *= 1.0 + T0;
        else if (unitAreaJctCap > 0.0)
          {
          unitAreaJctCap = 0.0;
          System.out.println("Temperature effect has caused cj to be negative. Cj is clamped to zero.");
          }
        T0 = tcjsw * delTemp;
        if (T0 >= -1.0)
          unitLengthSidewallJctCap *= 1.0 + T0;
        else if (unitLengthSidewallJctCap > 0.0)
          {
          unitLengthSidewallJctCap = 0.0;
          System.out.println("Temperature effect has caused cjsw to be negative. Cjsw is clamped to zero.");
          }
        T0 = tcjswg * delTemp;
        if (T0 >= -1.0)
          unitLengthGateSidewallJctCap *= 1.0 + T0;
        else if (unitLengthGateSidewallJctCap > 0.0)
          {
          unitLengthGateSidewallJctCap = 0.0;
          System.out.println("Temperature effect has caused cjswg to be negative. Cjswg is clamped to zero.");
          }
      
        PhiB = bulkJctPotential - tpb * delTemp;
        if (PhiB < 0.01)
          {
          PhiB = 0.01;
          System.out.println("Temperature effect has caused pb to be less than 0.01. Pb is clamped to 0.01.");
          }
        PhiBSW = sidewallJctPotential- tpbsw * delTemp;
        if (PhiBSW <= 0.01)
          {
          PhiBSW = 0.01;
          System.out.println("Temperature effect has caused pbsw to be less than 0.01. Pbsw is clamped to 0.01.");
          }
        PhiBSWG = GatesidewallJctPotential - tpbswg * delTemp;
        if (PhiBSWG <= 0.01)
          {
          PhiBSWG = 0.01;
          System.out.println("Temperature effect has caused pbswg to be less than 0.01. Pbswg is clamped to 0.01.");
          }
    }
}
