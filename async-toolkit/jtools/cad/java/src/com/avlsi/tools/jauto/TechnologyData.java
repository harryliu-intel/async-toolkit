/*
 * Copyright 2002, 2003, 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.tools.jauto;

import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgFormatException;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.CommandLineArgsIterator;
import com.avlsi.util.cmdlineargs.CommandLineArgsUtil;
import com.avlsi.util.cmdlineargs.InvalidCommandLineArgException;
import com.avlsi.util.cmdlineargs.MissingCommandLineArgException;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.ConfigFileListAdapter;
import com.avlsi.util.cmdlineargs.defimpl.TransformCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.UnionCommandLineArgs;

import com.avlsi.util.container.FullTable;
import com.avlsi.util.container.LinearInterpolatable;
import com.avlsi.util.container.SparseTable;

import com.avlsi.file.common.DeviceTypes;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * This class stores technical data related to targeting CMOS process.
 **/
public final class TechnologyData {
    /**
     * This class stores parameters associated with transistors.
     **/
    public final class Transistor {
        /**
         * Cut-off length of stack of transistors.
         **/
        //@ invariant stackLimitN > 0;
        public final int stackLimitN;

        //@ invariant stackLimitP > 0;
        public final int stackLimitP;

        /**
         * Unit gate capacitance for n-transistors in F / m^2.
         **/
        //@ invariant unitNmosGateCapacitance >= 0.0;
        private final double unitNmosGateCapacitance;

        /**
         * Unit gate capacitance for p-transistors in F / m^2.
         **/
        //@ invariant unitPmosGateCapacitance >= 0.0;
        private final double unitPmosGateCapacitance;

        /**
         * Unit gate capacitance per unit transistor width for n-transistors
         * in F / m.
         **/
        //@ invariant unitNmosDiffusionCapacitance >= 0.0;
        private final double unitNmosDiffusionCapacitance;

        /**
         * Unit gate capacitance per unit transistor width for p-transistors
         * in F / m.
         **/
        //@ invariant unitPmosDiffusionCapacitance >= 0.0;
        private final double unitPmosDiffusionCapacitance;

        /**
         * Effective resistance factor of N-type transistor stack.
         * N-type effective R = effectiveResistanceFactorN * gate_length /
         *                      gate_width
         * <p>Entry [n] gives the effective R factor for stack of depth [n].
         **/
        //@ invariant effectiveResistanceFactorN != null;
        //@ invariant effectiveResistanceFactorN.length == stackLimitN;
        //@ invariant (\forall int i;
        //@                    0 <= i && i < effectiveResistanceFactorN.length;
        //@                    effectiveResistanceFactorN[i] >= 0.0);
        public final double[] effectiveResistanceFactorN; // in Ohm

        /**
         * Effective resistance factor of P-type transistor stack.
         * P-type effective R = effectiveResistanceFactorP * gate_length /
         *                      gate_width
         * <p>Entry [n] gives the effective R factor for stack of depth [n].
         **/
        //@ invariant effectiveResistanceFactorP != null;
        //@ invariant effectiveResistanceFactorP.length == stackLimitP;
        //@ invariant (\forall int i;
        //@                    0 <= i && i < effectiveResistanceFactorP.length;
        //@                    effectiveResistanceFactorP[i] >= 0.0);
        public final double[] effectiveResistanceFactorP; // in Ohm
    
        /**
         * Intrinsic delay for N-type transistor stack
         **/
        //@ invariant gateIntrinsicDelayN != null;
        //@ invariant gateIntrinsicDelayN.length == stackLimitN;
        //@ invariant (\forall int i;
        //@                    0 <= i && i < gateIntrinsicDelayN.length;
        //@                    gateIntrinsicDelayN[i] >= 0.0);
        public final double[] gateIntrinsicDelayN; // in Second

        /**
         * Intrinsic delay for P-type transistor stack
         **/
        //@ invariant gateIntrinsicDelayP != null;
        //@ invariant gateIntrinsicDelayP.length == stackLimitP;
        //@ invariant (\forall int i;
        //@                    0 <= i && i < gateIntrinsicDelayP.length;
        //@                    gateIntrinsicDelayP[i] >= 0.0);
        public final double[] gateIntrinsicDelayP; // in Second

        /**
         * Effective resistance of one off NMOS in series
         */
        public final double leakageResistanceN; // in Ohm/square

        /**
         * Effective resistance of one off PMOS in series
         */
        public final double leakageResistanceP; // in Ohm/square

        /** 
         * Array of long channel transistors lengths.
         **/
        public final double [] staticizerLengthN; // in meters
        public final double [] staticizerLengthP; // in meters

        /** 
         * Array of long channel transistors strengths, relative to what
         * simple W/L would predict.
         **/
        public final double [] staticizerStrengthCorrectionN;
        public final double [] staticizerStrengthCorrectionP;

        /**
         * <pre><jml>
         *   normal_behavior
         *     requires args != null;
         *     ensures unitNmosGateCapacitance >= 0.0;
         *     ensures unitPmosGateCapacitance >= 0.0;
         *     ensures unitNmosDiffusionCapacitance >= 0.0;
         *     ensures unitPmosDiffusionCapacitance >= 0.0;
         *     ensures stackLimitN > 0;
         *     ensures stackLimitP > 0;
         *     ensures effectiveResistanceFactorN != null;
         *     ensures effectiveResistanceFactorN.length == stackLimitN;
         *     ensures (\forall int i;
         *                      0 <= i &&
         *                          i < effectiveResistanceFactorN.length;
         *                      effectiveResistanceFactorN[i] >= 0.0);
         *     ensures effectiveResistanceFactorP != null;
         *     ensures effectiveResistanceFactorP.length == stackLimitP;
         *     ensures (\forall int i;
         *                      0 <= i &&
         *                          i < effectiveResistanceFactorP.length;
         *                      effectiveResistanceFactorP[i] >= 0.0);
         *     ensures gateIntrinsicDelayN != null;
         *     ensures gateIntrinsicDelayN.length == stackLimitN;
         *     ensures (\forall int i;
         *                      0 <= i && i < gateIntrinsicDelayN.length;
         *                      gateIntrinsicDelayN[i] >= 0.0);
         *     ensures gateIntrinsicDelayP != null;
         *     ensures gateIntrinsicDelayP.length == stackLimitP;
         *     ensures (\forall int i;
         *                      0 <= i && i < gateIntrinsicDelayP.length;
         *                      gateIntrinsicDelayP[i] >= 0.0);
         * </jml></pre>
         **/
        public Transistor(final /*@ non_null @*/ CommandLineArgs args,
                          final int stackLimitN,
                          final int stackLimitP,
                          final String x)
            throws CommandLineArgFormatException,
                   InvalidCommandLineArgException,
                   MissingCommandLineArgException {

            this.stackLimitN = stackLimitN;
            this.stackLimitP = stackLimitP;

            // The option unitGateCapacitance will eventually be deprecated,
            // but until then it will set both the unitNmosGateCapacitance
            // and unitPmosGateCapacitance, but those values will take
            // precedence.
            if (args.argExists("unitGateCapacitance" + x)) {
                if (args.argExists("unitNmosGateCapacitance" + x))
                    throw new InvalidCommandLineArgException(
                            "Cannot specify both unitGateCapacitance " + x +
                            " and " +
                            "unitNmosGateCapacitance" + x,
                            "unitNmosGateCapacitance" + x,
                            args.getArgValue("unitNmosGateCapacitance" + x,
                                             null));
                if (args.argExists("unitPmosGateCapacitance" + x))
                    throw new InvalidCommandLineArgException(
                            "Cannot specify both unitGateCapacitance" + x +
                            " and " +
                            "unitPmosGateCapacitance" + x,
                            "unitPmosGateCapacitance" + x,
                            args.getArgValue("unitPmosGateCapacitance" + x,
                                             null));

                final double unitGateCapacitance =
                    CommandLineArgsUtil
                        .getRequiredNonNegativeDoubleArgValue(args,
                                "unitGateCapacitance" + x);
                unitNmosGateCapacitance = unitGateCapacitance;
                unitPmosGateCapacitance = unitGateCapacitance;
            } else {
                if (!args.argExists("unitNmosGateCapacitance" + x))
                    throw new MissingCommandLineArgException(
                            "Must specify unitNmosGateCapacitance" + x +
                            " if " +
                            "unitGateCapacitance" + x + " is not specified",
                            "unitNmosGateCapacitance" + x);
                if (!args.argExists("unitPmosGateCapacitance" + x))
                    throw new MissingCommandLineArgException(
                            "Must specify unitPmosGateCapacitance" + x +
                            " if " +
                            "unitGateCapacitance" + x + " is not specified",
                            "unitPmosGateCapacitance" + x);

                unitNmosGateCapacitance =
                    CommandLineArgsUtil
                        .getRequiredNonNegativeDoubleArgValue(args,
                                "unitNmosGateCapacitance" + x);
                unitPmosGateCapacitance =
                    CommandLineArgsUtil
                        .getRequiredNonNegativeDoubleArgValue(args,
                                "unitPmosGateCapacitance" + x);
            }

            unitNmosDiffusionCapacitance =
                CommandLineArgsUtil.getNonNegativeDoubleArgValue(args,
                        "unitNmosDiffusionCapacitance" + x, 0.0);
            unitPmosDiffusionCapacitance =
                CommandLineArgsUtil.getNonNegativeDoubleArgValue(args,
                        "unitPmosDiffusionCapacitance" + x, 0.0);

            effectiveResistanceFactorN = new double[stackLimitN];
            gateIntrinsicDelayN = new double[stackLimitN];

            effectiveResistanceFactorP = new double[stackLimitP];
            gateIntrinsicDelayP = new double[stackLimitP];

            for (int i = 0; i < stackLimitN; ++i) {
                effectiveResistanceFactorN[i] =
                    CommandLineArgsUtil
                        .getRequiredNonNegativeDoubleArgValue(args,
                                "effectiveResistanceFactorN_" + i + x);
                gateIntrinsicDelayN[i] =
                    CommandLineArgsUtil
                        .getRequiredNonNegativeDoubleArgValue(args,
                                "gateIntrinsicDelayN_" + i + x);
            }

            for (int i = 0; i < stackLimitP; ++i) {
                effectiveResistanceFactorP[i] =
                    CommandLineArgsUtil
                        .getRequiredNonNegativeDoubleArgValue(args, 
                                "effectiveResistanceFactorP_" + i + x);
                gateIntrinsicDelayP[i] =
                    CommandLineArgsUtil
                        .getRequiredNonNegativeDoubleArgValue(args, 
                                "gateIntrinsicDelayP_" + i + x);
            }

            leakageResistanceN = CommandLineArgsUtil.
                getNonNegativeDoubleArgValue(args,"leakageResistanceN" + x, 0.0);
            leakageResistanceP = CommandLineArgsUtil.
                getNonNegativeDoubleArgValue(args,"leakageResistanceP" + x, 0.0);

            staticizerLengthN = CommandLineArgsUtil.
                getDoubleArgList(args,"staticizerLengthN" + x);
            staticizerLengthP = CommandLineArgsUtil.
                getDoubleArgList(args,"staticizerLengthP" + x);

            staticizerStrengthCorrectionN = CommandLineArgsUtil.
                getDoubleArgList(args,"staticizerStrengthCorrectionN" + x);
            staticizerStrengthCorrectionP = CommandLineArgsUtil.
                getDoubleArgList(args,"staticizerStrengthCorrectionP" + x);

        }

        //@ ensures \result >= 0.0;
        public final double getUnitNmosGateCapacitance()
        {
            return unitNmosGateCapacitance;
        }

        //@ ensures \result >= 0.0;
        public final double getUnitPmosGateCapacitance()
        {
            return unitPmosGateCapacitance;
        }

        //@ ensures \result >= 0.0;
        public final double getUnitNmosDiffusionCapacitance()
        {
            return unitNmosDiffusionCapacitance;
        }

        //@ ensures \result >= 0.0;
        public final double getUnitPmosDiffusionCapacitance()
        {
            return unitPmosDiffusionCapacitance;
        }

        public final double [] getEffectiveResistanceFactorN()
        {
            return effectiveResistanceFactorN;
        }

        //@ requires 0 <= depth && depth < stackLimitN;
        //@ ensures \result >= 0.0;
        public final double getEffectiveResistanceFactorN(final int depth)
        {
            return effectiveResistanceFactorN[depth];
        }

        public final double [] getEffectiveResistanceFactorP()
        {
            return effectiveResistanceFactorP;
        }

        //@ requires 0 <= depth && depth < stackLimitP;
        //@ ensures \result >= 0.0;
        public final double getEffectiveResistanceFactorP(final int depth)
        {
            return effectiveResistanceFactorP[depth];
        }

        public final double getLeakageResistanceN()
        {
            return leakageResistanceN;
        }

        public final double getLeakageResistanceP()
        {
            return leakageResistanceP;
        }

        public final double [] getGateIntrinsicDelayN()
        {
            return gateIntrinsicDelayN;
        }

        //@ requires 0 <= depth && depth < stackLimitN;
        //@ ensures \result >= 0.0;
        public final double getGateIntrinsicDelayN(final int depth)
        {
            return gateIntrinsicDelayN[depth];
        }

        public final double [] getGateIntrinsicDelayP()
        {
            return gateIntrinsicDelayP;
        }

        //@ requires 0 <= depth && depth < stackLimitP;
        //@ ensures \result > 0.0;
        public final double getGateIntrinsicDelayP(final int depth)
        {
            return gateIntrinsicDelayP[depth];
        }

        public final double[] getStaticizerLengthN() {
            return staticizerLengthN;
        }

        public final double[] getStaticizerLengthP() {
            return staticizerLengthP;
        }

        public final double[] getStaticizerStrengthCorrectionN() {
            return staticizerStrengthCorrectionN;
        }

        public final double[] getStaticizerStrengthCorrectionP() {
            return staticizerStrengthCorrectionP;
        }

        private String arrayString(final double[] array) {
            String s = "";
            for (int i = 0; i < array.length; ++i) {
                if (i > 0) s = s + ", ";
                s = s + array[i];
            }
            return s;
        }

        public final String toString() {
            return "unitNmosGateCapacitance: " +
                   getUnitNmosGateCapacitance() + "\n" +
                   "unitPmosGateCapacitance: " +
                   getUnitPmosGateCapacitance() + "\n" +
                   "unitNmosDiffusionCapacitance: " +
                   getUnitNmosDiffusionCapacitance() + "\n" +
                   "unitPmosDiffusionCapacitance: " +
                   getUnitPmosDiffusionCapacitance() + "\n" +
                   "effectiveResistanceFactorN: " +
                   arrayString(getEffectiveResistanceFactorN()) + "\n" +
                   "effectiveResistanceFactorP: " +
                   arrayString(getEffectiveResistanceFactorP()) + "\n" +
                   "leakageResistanceN: " +
                   getLeakageResistanceN() + "\n" +
                   "leakageResistanceP: " +
                   getLeakageResistanceP() + "\n" +
                   "gateIntrinsicDelayN: " +
                   arrayString(getGateIntrinsicDelayN()) + "\n" +
                   "gateIntrinsicDelayP: " +
                   arrayString(getGateIntrinsicDelayP()) + "\n" +
                   "staticizerLengthN: " +
                   arrayString(getStaticizerLengthN()) + "\n" +
                   "staticizerLengthP: " +
                   arrayString(getStaticizerLengthP()) + "\n" +
                   "staticizerStrengthCorrectionN: " +
                   arrayString(getStaticizerStrengthCorrectionN()) + "\n" +
                   "staticizerStrengthCorrectionP: " +
                   arrayString(getStaticizerStrengthCorrectionP()) + "\n";
        }
    }

    /**
     * Name of the technology.
     * For example, "tsmc12hs_lowk", stands for TSMC 0.13u, high-speed (1.2V), low K technology.
     **/
    //@ invariant technologyName != null;
    private final /*@ non_null @*/ String technologyName;

    /**
     * Unit wire capacitance for metal wires.
     * Ca -> Area Capacitance
     * <p>Cf -> Fringe Capacitance
     * <p>Cc -> Coupling Capacitance
     **/
    //@ invariant unitWireCapacitanceCa >= 0.0;
    public double unitWireCapacitanceCa=0; // in F/Meter^2

    /**
     * Curve-fitting parameters for calculating Cf + Cc when wire space [S] varies.
     * Cf+Cc = Cfc_2 / S / S + Cfc_1 / S + Cfc_0
     **/
    //@ invariant unitWireCapacitanceCfc_2 >= 0.0;
    public double unitWireCapacitanceCfc_2=0; // in F*Meter

    //@ invariant unitWireCapacitanceCfc_1 >= 0.0;
    public double unitWireCapacitanceCfc_1=0; // in Farad

    //@ invariant unitWireCapacitanceCfc_0 >= 0.0;
    public double unitWireCapacitanceCfc_0=0; // in F/Meter

    // Capacitance scaling factor
    /**
     * User specified capacitance scaling factor for wires.
     * The total wire capacitance becomes:
     * <p>C = L * (W * Ca + 2 * Cfc) * SF
     * <p>where L is the wire length, W is the wire width, 
     * SF is the "capacitanceScalingFactor".
     **/
    //@ invariant capacitanceScalingFactor >= 0.0;
    public final double capacitanceScalingFactor;

    /**
     * Unit wire resistance parameter, for calculating wire resistance.
     * Use typical data for M4
     **/
    //@ invariant unitWireResistance >= 0.0;
    public double unitWireResistance=0; // in Ohm/sq

    /**
     * Minimum transistor width allowed in the design flow.
     **/
    //@ invariant minimumTransistorWidth >= 0.0;
    public final double minimumTransistorWidth; // in Meter

    /**
     * Minimum transistor length allowed in the design flow.
     **/
    //@ invariant minimumTransistorLength >= 0.0;
    public final double minimumTransistorLength; // in Meter

    /**
     * Default average diffusion length of transistors, for calculating diffusion cap.
     **/
    //@ invariant defaultDiffusionLength >= 0.0;
    public final double defaultDiffusionLength; // in Meter

    /**
     * Default gate length, usually equals the minimum feature size.
     **/
    //@ invariant defaultGateLength >= 0.0;
    public final double defaultGateLength; // in Meter

    /**
     * Default width of metal wires.
     **/
    //@ invariant defaultWireWidth >= 0.0;
    public final double defaultWireWidth; // in Meter

    /**
     * Default spacing between between metal wires.
     **/
    //@ invariant defaultWireSpace >= 0.0;
    public final double defaultWireSpace; // in Meter

    /**
     * Default length of metal wire.
     * It is used when estimated wire length is zero,
     * which will happen when there is no input about circuit floorplan.
     **/
    //@ invariant defaultWireLength >= 0.0;
    public final double defaultWireLength; // in Meter

    /**
     * The minimum allowed wire width in meters.  Used for electromigration
     * calculations.
     **/
    //@ invariant minimumWireWidth >= 0.0;
    public final double minimumWireWidth;

    /**
     * Minimum length of metal wire, not enforced in the code.
     **/
    //@ invariant minimumWireLength >= 0.0;
    public final double minimumWireLength; // in Meter

    /**
     * User specified layout scaling factor.
     * After sizing, cell sizes will change comparing to the ones used for floorplan.
     * These parameters help user to do next sizing run without redo the floorplan.
     **/
    //@ invariant layoutScaleX >= 0.0;
    public final double layoutScaleX; 

    //@ invariant layoutScaleY >= 0.0;
    public final double layoutScaleY; 

    /**
     * Default load capacitance to be added to the nets without fanout.
     **/
    //@ invariant defaultLoadCapacitance >= 0.0;
    public final double defaultLoadCapacitance; 

    /**
     * Specifies whether to use the intrinsic capacitance delay
     * model, which gets its parameters from directives in the
     * gates.
     **/
    private final boolean useIntrinsicCap;

    /**
     * Elmore delay discount factor, most people use 0.69.
     * delay = R * C * elmoreDelayFactor
     **/
    //@ invariant elmoreDelayFactor >= 0.0;
    public final double elmoreDelayFactor;

    /**
     * Wire RC delay discount factor.
     * From my simulation, and better effect on sizing, 0.4
     **/
    //@ invariant wireRCDelayFactor >= 0.0;
    public final double wireRCDelayFactor;

    /**
     * Wire resistance shielding factor -> effective capacitance seen by the gate.
     **/
    //@ invariant resistanceShieldingFactor >= 0.0;
    public final double resistanceShieldingFactor;

    //@ invariant resistanceShieldingThreshold >= 0.0;
    public final double resistanceShieldingThreshold;

    /** Staticizer parameters **/
    public final double minStaticizerRatio;     // min staticizer G / logic G
    public final double maxStaticizerRatio;     // max staticizer G / logic G
    public final double minCombinationalStaticizerRatio;
    public final double maxCombinationalStaticizerRatio;
    public final double staticizerLeakageRatio; // min staticizer G / leakage G
    public final double staticizerWidthGrid;    // round width of staticizers
    public final int    staticizerMaxDepthN;    // max NMOS_CHAIN for weak staticizer
    public final int    staticizerMaxDepthP;    // max NMOS_CHAIN for weak staticizer

    /**
     * Conversion factor from delay to slew time, unitless.
     * <code>slew_time = delay * delayToSlewTimeConversionFactor</code>.
     * Used for electromigration calculations.
     **/
    //@ invariant delayToSlewTimeConversionFactor >= 0.0;
    private final double delayToSlewTimeConversionFactor;

    /**
     * Nominal cycle time for the process, in seconds.
     * Used for electromigration calculations.
     **/
    //@ invariant cycleTime >= 0.0;
    private final double cycleTime;

    /**
     * Process voltage, in volts.
     * Used for electromigration calculations.
     **/
    //@ invariant voltage >= 0.0;
    private final double voltage;

    /**
     * Current limit per contact, in Amps / via.
     * Used for electromigration calculations.
     **/
    //@ invariant viaAvgCurrentLimit >= 0.0;
    private final double viaAvgCurrentLimit;

    /**
     * Current limit per width of M1, in Amps / meter.
     * Used for electromigration calculations.
     **/
    //@ invariant wireAvgCurrentLimit >= 0.0;
    private final double wireAvgCurrentLimit;

    /**
     * Root-mean-square current limit per width of M2, in Amps / meter.
     * Used for electromigration calculations.
     **/
    //@ invariant wireRMSCurrentLimit >= 0.0;
    private final double wireRMSCurrentLimit;

    /**
     * Peak current limit per width of M1, in Amps / meter.
     * Used for electromigration calculations.
     **/
    //@ invariant wirePeakCurrentLimit >= 0.0;
    private final double wirePeakCurrentLimit;

    /**
     * Multiplier to the average current limits to
     * account for a temperature other than the temperature
     * for which <code>viaAvgCurrentLimit</code> and
     * <code>wireAvgCurrentLimit</code> were computed, unitless.
     * Used for electromigration calculations.
     **/
    //@ invariant avgCurrentLimitTempCorrectionFactor >= 0.0;
    private final double avgCurrentLimitTempCorrectionFactor;

    /**
     * Maximum depth of transistor stacks
     **/
    //@ invariant stackLimitN > 0;
    public final int stackLimitN;

    //@ invariant stackLimitP > 0;
    public final int stackLimitP;

    /**
     * Warn about deep stacks
     **/
    //@ invariant stackWarnLimitN > 0;
    public final int stackWarnLimitN;

    //@ invariant stackWarnLimitP > 0;
    public final int stackWarnLimitP;

    /**
     * The ratio of the precharge transistor strength to logic strength.
     **/
    //@ invariant prechargeRatio > 0;
    public final double prechargeRatio;

    /**
     * If a precharge transistor is longer than the specified length, a warning
     * is emitted
     **/
    //@ invariant maximumPrechargeTransistorLength > 0;
    public final double maximumPrechargeTransistorLength;

    /**
     * The load associated with the precharge transistor.
     **/
    //@ invariant prechargeTransistorLoad > 0;
    public final double prechargeTransistorLoad;

    /**
     * A table used to interpolate unit resistance from wire width and wire
     * spacing.
     **/
    public LinearInterpolatable wireResistanceTable = null;

    /**
     * A table used to interpolate unit capacitance from wire width and wire
     * spacing.
     **/
    public LinearInterpolatable wireCapacitanceTable = null;

    /**
     * Maximum number of transistor types supported.
     **/
    public static final int MAX_TRANSISTOR_TYPES = 256;

    /**
     * A default transistor type, for backward compatibility.  It is only used
     * when the --transistorTypes argument is not specified.
     **/
    private static final int DEFAULT_TRANSISTOR_TYPE = 1;
    public final static int NO_TRANSISTOR_TYPE = 0;
    public final static int MIXED_TRANSISTOR_TYPE = -1;

    /**
     * An array of transistor parameters.  The index is the transistor type.
     **/
    private final Transistor[] transistorParameters =
        new Transistor[MAX_TRANSISTOR_TYPES];

    /**
     * True if parameters for multiple transistor types are given, and false
     * otherwise.
     **/
    private final boolean hasMultipleTransistors;

    /**
     * Various overhead associated with layout, in m.
     **/
    //@ invariant transistorWidthOverhead >= 0.0;
    public final double transistorWidthOverhead;
    //@ invariant transistorLengthOverhead >= 0.0;
    public final double transistorLengthOverhead;

    /**
     * Coefficients used to account for possible rounding effects of transistor
     * widths on gate and diffusion capacitance.  widthRoundingOffset in m.
     **/
    private final double widthRoundingSlope;
    private final double widthRoundingOffset;

    /**
     * Array of legal transistor widths.
     **/
    private final double [] widths; // in meters

    /**
     * Wirelength of an internal net in a leaf cell is:
     * perimeter of container * leafWireLengthScale
     **/
    private final double leafWireLengthScale;

    /**
     * Wirelength contribution of a port net in a leaf cell is:
     * perimeter of container * leafWireLengthPortScale
     **/
    private final double leafWireLengthPortScale;

    /**
     * Class constructor.  The following arguments are required:
     * <ul>
     *   <li><code>technologyName</code>: string</li>
     *   <li><code>minimumTransistorWidth</code>: floating point</li>
     *   <li><code>minimumTransistorLength</code>: floating point</li>
     *   <li><code>defaultGateLength</code>: floating point</li>
     *   <li><code>defaultDiffusionLength</code>: floating point</li>
     *   <li><code>defaultLoadCapacitance</code>: floating point</li>
     *   <li><code>defaultWireWidth</code>: floating point</li>
     *   <li><code>defaultWireLength</code>: floating point</li>
     *   <li><code>defaultWireSpace</code>: floating point</li>
     *   <li><code>minimumWireWidth</code>: floating point</li>
     *   <li><code>minimumWireLength</code>: floating point</li>
     *   <li><code>stackLimitN</code>: floating point</li>
     *   <li><code>stackLimitP</code>: floating point</li>
     *   <li><code>effectiveResistanceFactorN_k</code>: floating point,
     *       required for values of <code>k</code> up to
     *       <code>stackLimitN - 1</code></li>
     *   <li><code>effectiveResistanceFactorP_k</code>: floating point,
     *       required for values of <code>k</code> up to
     *       <code>stackLimitP - 1</code></li>
     *   <li><code>gateIntrinsicDelayN_k</code>: floating point,
     *       required for values of <code>k</code> up to
     *       <code>stackLimitN - 1</code></li>
     *   <li><code>gateIntrinsicDelayP_k</code>: floating point,
     *       required for values of <code>k</code> up to
     *       <code>stackLimitP - 1</code></li>
     *   <li><code>elmoreDelayFactor</code>: floating point</li>
     *   <li><code>wireRCDelayFactor</code>: floating point</li>
     *   <li><code>resistanceShieldingFactor</code>: floating point</li>
     *   <li><code>resistanceShieldingThreshold</code>: floating point</li>
     *   <li><code>delayToSlewTimeConversionFactor</code>:
     *       floating point</li>
     *   <li><code>cycleTime</code>: floating point</li>
     *   <li><code>voltage</code>: floating point</li>
     *   <li><code>viaAvgCurrentLimit</code>: floating point</li>
     *   <li><code>wireAvgCurrentLimit</code>: floating point</li>
     *   <li><code>wireRMSCurrentLimit</code>: floating point</li>
     *   <li><code>wirePeakCurrentLimit</code>: floating point</li>
     *   <li><code>avgCurrentLimitTempCorrectionFactor</code>:
     *       floating point</li>
     * </ul>
     *
     * Specify wire capacitance with either:
     * <ul>
     *   <li><code>unitWireCapacitanceCa</code>: floating point</li>
     *   <li><code>unitWireCapacitanceCfc_0</code>: floating point</li>
     *   <li><code>unitWireCapacitanceCfc_1</code>: floating point</li>
     *   <li><code>unitWireCapacitanceCfc_2</code>: floating point</li>
     * </ul>
     * Or a bi-linear interpolated lookup table:
     * <ul>
     *   <li><code>wireCapacitance(Width,Space)</code>: floating point table</li>
     * </ul>
     *
     * Specify wire resistance with either:
     * <ul>
     *   <li><code>unitWireResistance</code>: floating point</li>
     * </ul>
     * Or a bi-linear interpolated lookup table:
     * <ul>
     *   <li><code>wireResistance(Width,Space)</code>: floating point table</li>
     * </ul>
     *
     * <p>Furthermore, either <code>unitGateCapacitance</code> or both
     * <code>unitNmosGateCapacitance</code> and
     * <code>unitPmosGateCapacitance</code> must be specified.  The use of
     * <code>unitGateCapacitance</code> is discouraged and will soon be
     * illegal.</p>
     *
     * <p>The following arguments are optional, and default values will be
     * supplied if they are not specified:
     * <ul>
     *   <li><code>unitNmosDiffusionCapacitance</code>: floating point,
     *       default 0.0</li>
     *   <li><code>unitPmosDiffusionCapacitance</code>: floating point,
     *       default 0.0</li>
     *   <li><code>capacitanceScalingFactor</code>: floating point,
     *       default 1.0</li>
     *   <li><code>layoutScaleX</code>: floating point, default 1.0</li>
     *   <li><code>layoutScaleY</code>: floating point, default 1.0</li>
     *   <li><code>useIntrinsicCap</code>: only the presence or absence
     *       of this option matters, not the value</li>
     *   <li><code>prechargeRatio</code>: floating point, default 0.1</li>
     * </ul></p>
     *
     * @param args
     *        The command line arguments to use to construct the instance.
     *
     * @throws CommandLineArgs
     *         If an argument value is not of the correct format.
     * @throws InvalidCommandLineArgException
     *         If an argument value is of the correct format, but
     *         out of the required range.
     * @throws MissingCommandLineArgException
     *         If a required argument is not supplied.
     *
     * <pre><jml>
     *   normal_behavior
     *     requires args != null;
     *     ensures technologyName != null;
     *     ensures unitWireCapacitanceCa >= 0.0;
     *     ensures unitWireCapacitanceCfc_0 >= 0.0;
     *     ensures unitWireCapacitanceCfc_1 >= 0.0;
     *     ensures unitWireCapacitanceCfc_2 >= 0.0;
     *     ensures capacitanceScalingFactor >= 0.0;
     *     ensures unitWireResistance >= 0.0;
     *     ensures minimumTransistorWidth >= 0.0;
     *     ensures minimumTransistorLen >= 0.0;
     *     ensures defaultGateLength >= 0.0;
     *     ensures defaultDiffusionLength >= 0.0;
     *     ensures defaultLoadCapacitance >= 0.0;
     *     ensures defaultWireWidth >= 0.0;
     *     ensures defaultWireLength >= 0.0;
     *     ensures defaultWireSpace >= 0.0;
     *     ensures minimumWireWidth >= 0.0;
     *     ensures minimumWireLength >= 0.0;
     *     ensures layoutScaleX >= 0.0;
     *     ensures layoutScaleY >= 0.0;
     *     ensures stackLimitN > 0;
     *     ensures stackLimitP > 0;
     *     ensures elmoreDelayFactor >= 0.0;
     *     ensures wireRCDelayFactor >= 0.0;
     *     ensures resistanceShieldingFactor >= 0.0;
     *     ensures resistanceShieldingThreshold >= 0.0;
     *     ensures delayToSlewTimeConversionFactor >= 0.0;
     *     ensures cycleTime >= 0.0;
     *     ensures voltage >= 0.0;
     *     ensures viaAvgCurrentLimit >= 0.0;
     *     ensures wireAvgCurrentLimit >= 0.0;
     *     ensures wireRMSCurrentLimit >= 0.0;
     *     ensures wirePeakCurrentLimit >= 0.0;
     *     ensures avgCurrentLimitTempCorrectionFactor >= 0.0;
     *     ensures prechargeRatio > 0.0;
     *     ensures maximumPrechargeTransistorLength > 0.0;
     *     ensures prechargeTransistorLoad >= 0.0;
     *     ensures transistorWidthOverhead >= 0.0;
     *     ensures transistorLengthOverhead >= 0.0;
     * </jml></pre>
     **/
    public TechnologyData(final /*@ non_null @*/ CommandLineArgs args)
        throws CommandLineArgFormatException,
               InvalidCommandLineArgException,
               MissingCommandLineArgException {

        technologyName =
            CommandLineArgsUtil.getRequiredArgValue(args, "technologyName");

        // table based estimation of wire capacitance and resistance
        final Pattern wireResistanceRegex = Pattern.compile(
                "(wireResistance)\\((.+),(.+)\\)");
        final Pattern wireCapacitanceRegex = Pattern.compile(
                "(wireCapacitance)\\((.+),(.+)\\)");

        SparseTable sparseResistance = null;
        SparseTable sparseCapacitance = null;
        for (CommandLineArgsIterator i = args.iterator(); i.hasNext(); ) {
            final CommandLineArg arg = i.next();
            final String name = arg.getName();
            SparseTable table = null;
            Matcher m = null;
            if ((m = wireResistanceRegex.matcher(name)).matches()) {
                if (sparseResistance == null)
                    sparseResistance = new SparseTable();
                table = sparseResistance;
            } else if ((m = wireCapacitanceRegex.matcher(name)).matches()) {
                if (sparseCapacitance == null)
                    sparseCapacitance = new SparseTable();
                table = sparseCapacitance;
            }
            if (table != null) {
                final double width =
                    getPositiveDoubleValue(m.group(1), m.group(2));
                final double space =
                    getPositiveDoubleValue(m.group(1), m.group(3));
                final double val =
                    getPositiveDoubleValue(m.group(1), arg.getValue());
                table.putEntry(width, space, val);
            }
        }

        try {
            if (sparseResistance != null)
                wireResistanceTable = new FullTable(sparseResistance);
        } catch (FullTable.IncompleteException e) {
            throw new MissingCommandLineArgException(
                    "wireResistance table incomplete: " + e.getMessage(),
                    "wireResistance");
        }

        try {
            if (sparseCapacitance != null)
                wireCapacitanceTable = new FullTable(sparseCapacitance);
        } catch (FullTable.IncompleteException e) {
            throw new MissingCommandLineArgException(
                    "wireCapacitance table incomplete: " + e.getMessage(),
                    "wireCapacitance");
        }

        capacitanceScalingFactor =
            CommandLineArgsUtil.getNonNegativeDoubleArgValue(args,
                    "capacitanceScalingFactor", 1.0);

        if (wireCapacitanceTable==null) {
            unitWireCapacitanceCa =
                CommandLineArgsUtil.getRequiredNonNegativeDoubleArgValue
                (args,"unitWireCapacitanceCa");
            unitWireCapacitanceCfc_0 =
                CommandLineArgsUtil.getRequiredNonNegativeDoubleArgValue
                (args,"unitWireCapacitanceCfc_0");
            unitWireCapacitanceCfc_1 =
                CommandLineArgsUtil.getRequiredNonNegativeDoubleArgValue
                (args,"unitWireCapacitanceCfc_1");
            unitWireCapacitanceCfc_2 =
                CommandLineArgsUtil.getRequiredNonNegativeDoubleArgValue
                (args,"unitWireCapacitanceCfc_2");
        } else if (args.argExists("unitWireCapacitanceCa") ||
                   args.argExists("unitWireCapacitanceCfc_0") ||
                   args.argExists("unitWireCapacitanceCfc_1") ||
                   args.argExists("unitWireCapacitanceCfc_2"))
            throw new InvalidCommandLineArgException
                ("Cannot specify both wireCapacitance and unitWireCapacitance*",null);

        if (wireResistanceTable==null)
            unitWireResistance =
                CommandLineArgsUtil.getRequiredNonNegativeDoubleArgValue
                (args,"unitWireResistance");
        else if (args.argExists("unitWireResistance"))
            throw new InvalidCommandLineArgException
                ("Cannot specify both wireResistance and unitWireResistance",null);

        minimumTransistorWidth =
            CommandLineArgsUtil.getRequiredNonNegativeDoubleArgValue(args,
                    "minimumTransistorWidth");
        minimumTransistorLength =
            CommandLineArgsUtil.getRequiredNonNegativeDoubleArgValue(args,
                    "minimumTransistorLength");
        defaultGateLength =
            CommandLineArgsUtil.getRequiredNonNegativeDoubleArgValue(args,
                    "defaultGateLength");
        defaultDiffusionLength =
            CommandLineArgsUtil.getRequiredNonNegativeDoubleArgValue(args,
                    "defaultDiffusionLength");
        defaultLoadCapacitance =
            CommandLineArgsUtil.getRequiredNonNegativeDoubleArgValue(args,
                    "defaultLoadCapacitance");

        defaultWireWidth =
            CommandLineArgsUtil.getRequiredNonNegativeDoubleArgValue(args,
                    "defaultWireWidth");
        defaultWireSpace =
            CommandLineArgsUtil.getRequiredNonNegativeDoubleArgValue(args,
                    "defaultWireSpace");
        defaultWireLength =
            CommandLineArgsUtil.getRequiredNonNegativeDoubleArgValue(args,
                    "defaultWireLength");
        minimumWireWidth =
            CommandLineArgsUtil.getRequiredNonNegativeDoubleArgValue(args,
                    "minimumWireWidth");
        minimumWireLength =
            CommandLineArgsUtil.getRequiredNonNegativeDoubleArgValue(args,
                    "minimumWireLength");

        layoutScaleX =
            CommandLineArgsUtil.getNonNegativeDoubleArgValue(args,
                    "layoutScaleX", 1.0);
        layoutScaleY =
            CommandLineArgsUtil.getNonNegativeDoubleArgValue(args,
                    "layoutScaleY", 1.0);

        useIntrinsicCap = args.argExists("useIntrinsicCap");

        stackLimitN =
            CommandLineArgsUtil.getRequiredNonNegativeIntegerArgValue(args,
                    "stackLimitN");
        stackLimitP =
            CommandLineArgsUtil.getRequiredNonNegativeIntegerArgValue(args,
                    "stackLimitP");

        stackWarnLimitN =
            CommandLineArgsUtil.getNonNegativeIntegerArgValue(args,
                    "stackWarnLimitN", stackLimitN);
        stackWarnLimitP =
             CommandLineArgsUtil.getNonNegativeIntegerArgValue(args,
                    "stackWarnLimitP", stackLimitP);

        hasMultipleTransistors = args.argExists("transistorTypes");

        if (hasMultipleTransistors) {
            final String arg = "transistorTypes";
            final String val = args.getArgValue(arg, "");
            final int[] types = CommandLineArgsUtil.getIntArgList(args, arg);
            for (int i = 0; i < types.length; ++i) {
                if (types[i] <= 0 || types[i] > MAX_TRANSISTOR_TYPES - 1) {
                    throw new InvalidCommandLineArgException(
                            "Valid transistor types are in the " +
                            "range from 1.." + (MAX_TRANSISTOR_TYPES - 1),
                            arg, val);
                } else {
                    transistorParameters[types[i]] =
                        new Transistor(args, stackLimitN, stackLimitP,
                                       "[" + types[i] + "]");
                }
            }
        } else {
            transistorParameters[DEFAULT_TRANSISTOR_TYPE] =
                new Transistor(args, stackLimitN, stackLimitP, "");
        }

        elmoreDelayFactor =
            CommandLineArgsUtil.getRequiredNonNegativeDoubleArgValue(args,
                    "elmoreDelayFactor");
        wireRCDelayFactor =
            CommandLineArgsUtil.getRequiredNonNegativeDoubleArgValue(args,
                    "wireRCDelayFactor");
        resistanceShieldingFactor =
            CommandLineArgsUtil.getRequiredNonNegativeDoubleArgValue(args,
                    "resistanceShieldingFactor");
        resistanceShieldingThreshold =
            CommandLineArgsUtil.getRequiredNonNegativeDoubleArgValue(args,
                    "resistanceShieldingThreshold");

        minStaticizerRatio =
            CommandLineArgsUtil.getRequiredNonNegativeDoubleArgValue(args,
                    "minStaticizerRatio");
        maxStaticizerRatio =
            CommandLineArgsUtil.getRequiredNonNegativeDoubleArgValue(args,
                    "maxStaticizerRatio");
        minCombinationalStaticizerRatio =
            CommandLineArgsUtil.getRequiredNonNegativeDoubleArgValue(args,
                    "minCombinationalStaticizerRatio");
        maxCombinationalStaticizerRatio =
            CommandLineArgsUtil.getRequiredNonNegativeDoubleArgValue(args,
                    "maxCombinationalStaticizerRatio");
        staticizerLeakageRatio =
            CommandLineArgsUtil.getRequiredNonNegativeDoubleArgValue(args,
                    "staticizerLeakageRatio");
        staticizerWidthGrid =
            CommandLineArgsUtil.getRequiredNonNegativeDoubleArgValue(args,
                    "staticizerWidthGrid");
        staticizerMaxDepthN =
            CommandLineArgsUtil.getRequiredNonNegativeIntegerArgValue(args,
                    "staticizerMaxDepthN");
        staticizerMaxDepthP =
            CommandLineArgsUtil.getRequiredNonNegativeIntegerArgValue(args,
                    "staticizerMaxDepthP");

        delayToSlewTimeConversionFactor =
            CommandLineArgsUtil.getRequiredNonNegativeDoubleArgValue(args,
                    "delayToSlewTimeConversionFactor");
        cycleTime =
            CommandLineArgsUtil.getRequiredNonNegativeDoubleArgValue(args,
                    "cycleTime");
        voltage =
            CommandLineArgsUtil.getRequiredNonNegativeDoubleArgValue(args,
                    "voltage");
        viaAvgCurrentLimit =
            CommandLineArgsUtil.getRequiredNonNegativeDoubleArgValue(args,
                    "viaAvgCurrentLimit");
        wireAvgCurrentLimit =
            CommandLineArgsUtil.getRequiredNonNegativeDoubleArgValue(args,
                    "wireAvgCurrentLimit");
        wireRMSCurrentLimit =
            CommandLineArgsUtil.getRequiredNonNegativeDoubleArgValue(args,
                    "wireRMSCurrentLimit");
        wirePeakCurrentLimit =
            CommandLineArgsUtil.getRequiredNonNegativeDoubleArgValue(args,
                    "wirePeakCurrentLimit");
        avgCurrentLimitTempCorrectionFactor =
            CommandLineArgsUtil.getRequiredNonNegativeDoubleArgValue(args,
                    "avgCurrentLimitTempCorrectionFactor");
        prechargeRatio =
            CommandLineArgsUtil.getNonNegativeDoubleArgValue(args,
                    "prechargeRatio", 0.1);
        maximumPrechargeTransistorLength =
            CommandLineArgsUtil.getNonNegativeDoubleArgValue(args,
                    "maximumPrechargeTransistorLength", 6.5e-7);
        prechargeTransistorLoad =
            CommandLineArgsUtil.getNonNegativeDoubleArgValue(args,
                    "prechargeTransistorLoad", 1.118e-15);

        transistorWidthOverhead =
            CommandLineArgsUtil.getNonNegativeDoubleArgValue(args,
                    "transistorWidthOverhead", 0);
        transistorLengthOverhead =
            CommandLineArgsUtil.getNonNegativeDoubleArgValue(args,
                    "transistorLengthOverhead", 0);
        widthRoundingSlope =
            CommandLineArgsUtil.getDoubleArgValue(args,
                    "widthRoundingSlope", 1.0);
        widthRoundingOffset =
            CommandLineArgsUtil.getDoubleArgValue(args,
                    "widthRoundingOffset", 0.0);
        widths =
            CommandLineArgsUtil.getDoubleArgList(args, "widths");
        Arrays.sort(widths);

        leafWireLengthScale =
            CommandLineArgsUtil.getDoubleArgValue(args,
                    "leafWireLengthScale", 0.5);
        leafWireLengthPortScale =
            CommandLineArgsUtil.getDoubleArgValue(args,
                    "leafWireLengthPortScale", 0.25);
    }

    public final /*@ non_null @*/ String getTechnologyName()
    {
        return technologyName;
    }


    //@ ensures \result >= 0.0;
    public final double getUnitNmosGateCapacitance(final int type)
    {
        return getTransistorParameter(type).getUnitNmosGateCapacitance();
    }

    //@ ensures \result >= 0.0;
    public final double getUnitPmosGateCapacitance(final int type)
    {
        return getTransistorParameter(type).getUnitPmosGateCapacitance();
    }

    //@ ensures \result >= 0.0;
    public final double getUnitGateCapacitance(final int type, final int ttype)
    {
        return type == DeviceTypes.N_TYPE ? getUnitNmosGateCapacitance(ttype)
                                          : getUnitPmosGateCapacitance(ttype);
    }

    //@ ensures \result >= 0.0;
    public final double getUnitNmosDiffusionCapacitance(final int type)
    {
        return getTransistorParameter(type).getUnitNmosDiffusionCapacitance();
    }

    //@ ensures \result >= 0.0;
    public final double getUnitPmosDiffusionCapacitance(final int type)
    {
        return getTransistorParameter(type).getUnitPmosDiffusionCapacitance();
    }

    //@ ensures \result >= 0.0;
    public final double getUnitDiffusionCapacitance(final int type,
                                                    final int ttype)
    {
        return
            type == DeviceTypes.N_TYPE ? getUnitNmosDiffusionCapacitance(ttype)
                                       : getUnitPmosDiffusionCapacitance(ttype);
    }


    //@ ensures \result >= 0.0;
    public final double getUnitWireCapacitanceCa()
    {
        return unitWireCapacitanceCa;
    }


    //@ ensures \result >= 0.0;
    public final double getUnitWireCapacitanceCfc_2()
    {
        return unitWireCapacitanceCfc_2;
    }


    //@ ensures \result >= 0.0;
    public final double getUnitWireCapacitanceCfc_1()
    {
        return unitWireCapacitanceCfc_1;
    }


    //@ ensures \result >= 0.0;
    public final double getUnitWireCapacitanceCfc_0()
    {
        return unitWireCapacitanceCfc_0;
    }


    //@ ensures \result >= 0.0;
    public final double getCapacitanceScalingFactor()
    {
        return capacitanceScalingFactor;
    }


    //@ ensures \result >= 0.0;
    public final double getUnitWireResistance()
    {
        return unitWireResistance;
    }


    //@ ensures \result >= 0.0;
    public final double getMinimumTransistorWidth()
    {
        return minimumTransistorWidth;
    }


    //@ ensures \result >= 0.0;
    public final double getMinimumTransistorLength()
    {
        return minimumTransistorLength;
    }


    //@ ensures \result >= 0.0;
    public final double getDefaultGateLength()
    {
        return defaultGateLength;
    }


    //@ ensures \result >= 0.0;
    public final double getDefaultDiffusionLength()
    {
        return defaultDiffusionLength;
    }


    //@ ensures \result >= 0.0;
    public final double getDefaultLoadCapacitance()
    {
        return defaultLoadCapacitance;
    }


    //@ ensures \result >= 0.0;
    public final double getDefaultWireWidth()
    {
        return defaultWireWidth;
    }


    //@ ensures \result >= 0.0;
    public final double getDefaultWireSpace()
    {
        return defaultWireSpace;
    }


    //@ ensures \result >= 0.0;
    public final double getDefaultWireLength()
    {
        return defaultWireLength;
    }


    //@ ensures \result >= 0.0;
    public final double getMinimumWireLength()
    {
        return minimumWireLength;
    }


    /**
     * Returns the minimum allowed wire width.  Used for electromigration
     * calculations.
     **/
    //@ ensures \result >= 0.0;
    public final double getMinimumWireWidth() {
        return minimumWireWidth;
    }


    //@ ensures \result >= 0.0;
    public final double getLayoutScaleX()
    {
        return layoutScaleX;
    }


    //@ ensures \result >= 0.0;
    public final double getLayoutScaleY()
    {
        return layoutScaleY;
    }


    /**
     * Returns whether the intrinsic capacitance delay model is being used.
     *
     * @return Whether to use intrinsic capacitance.
     **/
    public boolean getUseIntrinsicCap() {
        return useIntrinsicCap;
    }


    public final double [] getEffectiveResistanceFactorN(final int type)
    {
        return getTransistorParameter(type).getEffectiveResistanceFactorN();
    }

    //@ requires 0 <= depth && depth < stackLimitN;
    //@ ensures \result >= 0.0;
    public final double getEffectiveResistanceFactorN(final int type,
                                                      final int depth)
    {
        return getTransistorParameter(type).getEffectiveResistanceFactorN(depth);
    }

    public final double [] getEffectiveResistanceFactorP(final int type)
    {
        return getTransistorParameter(type).getEffectiveResistanceFactorP();
    }

    //@ requires 0 <= depth && depth < stackLimitP;
    //@ ensures \result >= 0.0;
    public final double getEffectiveResistanceFactorP(final int type,
                                                      final int depth)
    {
        return getTransistorParameter(type).getEffectiveResistanceFactorP(depth);
    }

    //@ ensures \result >= 0.0;
    public final double getLeakageResistanceN(final int type)
    {
        return getTransistorParameter(type).getLeakageResistanceN();
    }

    //@ ensures \result >= 0.0;
    public final double getLeakageResistanceP(final int type)
    {
        return getTransistorParameter(type).getLeakageResistanceP();
    }

    public final double [] getGateIntrinsicDelayN(final int type)
    {
        return getTransistorParameter(type).getGateIntrinsicDelayN();
    }

    //@ requires 0 <= depth && depth < stackLimitN;
    //@ ensures \result >= 0.0;
    public final double getGateIntrinsicDelayN(final int type, final int depth)
    {
        return getTransistorParameter(type).getGateIntrinsicDelayN(depth);
    }

    public final double [] getGateIntrinsicDelayP(final int type)
    {
        return getTransistorParameter(type).getGateIntrinsicDelayP();
    }

    //@ requires 0 <= depth && depth < stackLimitP;
    //@ ensures \result > 0.0;
    public final double getGateIntrinsicDelayP(final int type, final int depth)
    {
        return getTransistorParameter(type).getGateIntrinsicDelayP(depth);
    }


    //@ ensures \result > 0.0;
    public final int getStackLimitN()
    {
        return stackLimitN;
    }


    //@ ensures \result >= 0.0;
    public final int getStackLimitP()
    {
        return stackLimitP;
    }


    //@ ensures \result >= 0.0;
    public final double getElmoreDelayFactor()
    {
        return elmoreDelayFactor;
    }


    //@ ensures \result >= 0.0;
    public final double getWireRCDelayFactor()
    {
        return wireRCDelayFactor;
    }


    //@ ensures \result >= 0.0;
    public final double getResistanceShieldingFactor()
    {
        return resistanceShieldingFactor;
    }


    //@ ensures \result >= 0.0;
    public final double getResistanceShieldingThreshold()
    {
        return resistanceShieldingThreshold;
    }

    public final double getMinStaticizerRatio()
    {
        return minStaticizerRatio;
    }

    public final double getMaxStaticizerRatio()
    {
        return maxStaticizerRatio;
    }

    public final double getMinCombinationalStaticizerRatio()
    {
        return minCombinationalStaticizerRatio;
    }

    public final double getMaxCombinationalStaticizerRatio()
    {
        return maxCombinationalStaticizerRatio;
    }

    public final double getStaticizerLeakageRatio()
    {
        return staticizerLeakageRatio;
    }

    public final double getStaticizerWidthGrid()
    {
        return staticizerWidthGrid;
    }

    public final int getStaticizerMaxDepthN()
    {
        return staticizerMaxDepthN;
    }

    public final int getStaticizerMaxDepthP()
    {
        return staticizerMaxDepthP;
    }

    /**
     * Returns the conversion factor from delay to slew time.
     * <code>slew_time = delay * delayToSlewTimeConversionFactor</code>.
     * Used for electromigration calculations.
     **/
    //@ ensures \result >= 0.0;
    public final double getDelayToSlewTimeConversionFactor() {
        return delayToSlewTimeConversionFactor;
    }

    /**
     * Returns the nominal cycle time for the process.
     * Used for electromigration calculations.
     **/
    //@ ensures \result >= 0.0;
    public final double getCycleTime() {
        return cycleTime;
    }

    /**
     * Returns the process voltage.
     * Used for electromigration calculations.
     **/
    //@ ensures \result >= 0.0;
    public final double getVoltage() {
        return voltage;
    }

    /**
     * Returns the current limit per contact.
     * Used for electromigration calculations.
     **/
    //@ ensures \result >= 0.0;
    public final double getViaAvgCurrentLimit() {
        return viaAvgCurrentLimit;
    }

    /**
     * Returns the current limit per width of M1.
     * Used for electromigration calculations.
     **/
    //@ ensures \result >= 0.0;
    public final double getWireAvgCurrentLimit() {
        return wireAvgCurrentLimit;
    }

    /**
     * Returns the root-mean-square current limit per width of M2.
     * Used for electromigration calculations.
     **/
    //@ ensures \result >= 0.0;
    public final double getWireRMSCurrentLimit() {
        return wireRMSCurrentLimit;
    }

    /**
     * Returns the peak current limit per width of M1.
     * Used for electromigration calculations.
     **/
    //@ ensures \result >= 0.0;
    public final double getWirePeakCurrentLimit() {
        return wirePeakCurrentLimit;
    }

    /**
     * Returns the multiplier to the average current limits to
     * account for a temperature other than the temperature
     * for which <code>viaAvgCurrentLimit</code> and
     * <code>wireAvgCurrentLimit</code> were computed.
     * Used for electromigration calculations.
     **/
    //@ ensures \result >= 0.0;
    public final double getAvgCurrentLimitTempCorrectionFactor() {
        return avgCurrentLimitTempCorrectionFactor;
    }

    /**
     * Returns the ratio of the precharge transistor strength to the logic
     * strength.
     **/
    //@ ensures \result > 0.0;
    public final double getPrechargeRatio() {
        return prechargeRatio;
    }

    /**
     * Return the longest length of the precharge transistor before a warning
     * is emitted.
     **/
    //@ ensures \result >= 0.0;
    public final double getMaximumPrechargeTransistorLength() {
        return maximumPrechargeTransistorLength;
    }

    /**
     * The load associated with the precharge transistor.
     **/
    //@ ensures \result >= 0.0;
    public final double getPrechargeTransistorLoad() {
        return prechargeTransistorLoad;
    }

    public final double[] getStaticizerLengthN(final int type) {
        return getTransistorParameter(type).getStaticizerLengthN();
    }

    public final double[] getStaticizerLengthP(final int type) {
        return getTransistorParameter(type).getStaticizerLengthP();
    }

    public final double[] getStaticizerStrengthCorrectionN(final int type) {
        return getTransistorParameter(type).getStaticizerStrengthCorrectionN();
    }

    public final double[] getStaticizerStrengthCorrectionP(final int type) {
        return getTransistorParameter(type).getStaticizerStrengthCorrectionP();
    }

    public final double getWidthRoundingSlope() {
        return widthRoundingSlope;
    }

    public final double getWidthRoundingOffset() {
        return widthRoundingOffset;
    }

    public final double getLeafWireLengthScale() {
        return leafWireLengthScale;
    }

    public final double getLeafWireLengthPortScale() {
        return leafWireLengthPortScale;
    }

    /**
     * Returns an array of legal transistor widths, in sorted order.
     **/
    public final double[] getWidths() {
        return widths;
    }

    public final LinearInterpolatable getWireResistanceTable() {
        return wireResistanceTable;
    }

    public final LinearInterpolatable getWireCapacitanceTable() {
        return wireCapacitanceTable;
    }

    private double getPositiveDoubleValue(final String arg, final String value)
        throws CommandLineArgFormatException,
               InvalidCommandLineArgException,
               MissingCommandLineArgException {
        if (value == null) {
            throw new MissingCommandLineArgException(arg);
        } else {
            double result;
            try {
                result = Double.parseDouble(value);
            } catch (NumberFormatException e) {
                throw new CommandLineArgFormatException(arg, value, e);
            }
            if (result <= 0) {
                throw new InvalidCommandLineArgException(arg +
                        " must be positive, has value " + value,
                        arg, value);
            }
            return result;
        }
    }

    public class InvalidTransistorTypeException extends RuntimeException {
        private final int type;
        public InvalidTransistorTypeException(final int type) {
            this.type = type;
        }
        public int getType() {
            return type;
        }
    }

    public class MixedTransistorTypeException extends RuntimeException {
        public MixedTransistorTypeException() { }
    }

    private final Transistor getTransistorParameter(int type) {
        // if no additional transistor types are specified, always use the
        // default transistor type even if the the transistor type encoded in
        // the netlist is different; this is the backwards compatible behavior
        if (!hasMultipleTransistors) type = DEFAULT_TRANSISTOR_TYPE;

        if (type == MIXED_TRANSISTOR_TYPE)
            throw new MixedTransistorTypeException();
        else {
            try {
                final Transistor result = transistorParameters[type];
                if (result == null)
                    throw new InvalidTransistorTypeException(type);
                return result;
            } catch (ArrayIndexOutOfBoundsException e) {
                throw new InvalidTransistorTypeException(type);
            }
        }
    }

    /** Name of the standard PDK root argument. **/
    private static final String PDK_ARG_NAME = "fulcrum-pdk-root";

    /** Name of the standard PDK root environment variable. **/
    private static final String PDK_ENV_NAME = "FULCRUM_PDK_ROOT";

    /**
     * Returns a TechnologyData, using --fulcrum-pdk-root and FULCRUM_PDK_ROOT
     * to get the PDK location.  If specified, expand --fulcrum-pdk-root=dir in
     * place to --config=dir/share/Fulcrum/jauto/process.config; otherwise, if
     * the environment variable FULCRUM_PDK_ROOT=dir exists, expand it to
     * --config=dir/share/Fulcrum/jauto/process.config as if it were at the
     * beginning of the commandline.
     **/
    public static TechnologyData getTechnologyData(final CommandLineArgs args)
        throws CommandLineArgFormatException,
               InvalidCommandLineArgException,
               MissingCommandLineArgException {
        try {
            return new TechnologyData(args);
        } catch (MissingCommandLineArgException e) {
            final String cmdRoot = args.getArgValue(PDK_ARG_NAME, null);
            if (cmdRoot != null) {
                final CommandLineArgs substArgs =
                    new TransformCommandLineArgs(args) {
                        protected CommandLineArg[] transform(
                                CommandLineArg arg) {
                            if (arg.getName().equals(PDK_ARG_NAME)) {
                                arg = new CommandLineArgDefImpl(
                                        "config",
                                        getProcessConfig(cmdRoot));
                            }
                            return new CommandLineArg[] { arg };
                        }
                    };
                return new TechnologyData(
                        new CachingCommandLineArgs(
                            new CommandLineArgsWithConfigFiles(substArgs)));
            }

            final String envRoot = System.getenv(PDK_ENV_NAME);
            if (envRoot != null) {
                return new TechnologyData(
                        new CachingCommandLineArgs(
                            new UnionCommandLineArgs(
                                new CommandLineArgsWithConfigFiles(
                                    new ConfigFileListAdapter(
                                        getProcessConfig(envRoot))),
                                args)));
            }

            throw e;
        }
    }

    private static String getProcessConfig(final String pdkRoot) {
        return pdkRoot + "/share/Fulcrum/jauto/process.config";
    }
}
