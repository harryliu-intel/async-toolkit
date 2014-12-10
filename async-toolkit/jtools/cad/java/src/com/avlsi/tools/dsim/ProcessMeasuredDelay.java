/*
 * Copyright 2002, 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

// vim:cin:sw=4:expandtab
/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.dsim;

import java.util.Collection;

import com.avlsi.util.container.Pair;
import com.avlsi.util.functions.NullaryAction;
import com.avlsi.tools.dsim.Node;

/**
 * Functions to process the raw measured_delay data to make it more suitable
 * for ASTA.
 **/
class ProcessMeasuredDelay {
    /**
     * Constrain the derivative to be less than limit.
     **/
    static boolean constrainMaxDerivative(final double[] x,
                                          final double[] delay,
                                          final double limit) {
        boolean changed = false;
        for (int i = x.length - 1; i > 1; --i) {
            final double dx = x[i] - x[i - 1];
            final double deriv = (delay[i] - delay[i - 1]) / dx;
            if (deriv > limit) {
                delay[i - 1] = delay[i] - dx * limit;
                changed = true;
            }
        }
        return changed;
    }

    /**
     * Constrain the derivative to be greater than limit.
     **/
    static boolean constrainMinDerivative(final double[] x,
                                          final double[] delay,
                                          final double limit) {
        boolean changed = false;
        for (int i = 1; i < x.length - 1; ++i) {
            final double dx = x[i + 1] - x[i];
            final double deriv = (delay[i + 1] - delay[i]) / dx;
            if (deriv < limit) {
                delay[i + 1] = delay[i] + dx * limit;
                changed = false;
            }
        }
        return changed;
    }

    static void printTable(final Pair table, final Node target,
                           final int dir, final String label) {
        final Pair p = (Pair) table.getSecond();
        System.err.println("T|" + ((Node) table.getFirst()).getName() +
                           (dir > 0 ? '-' : '+') +
                           " -> " + target.getName() +
                           (dir > 0 ? '+' : '-'));
        printTable((float[]) p.getSecond(), label);
    }

    static void printTable(final Pair[] tables, final Node target,
                           final int dir, final String label) {
        for (Pair table : tables) {
            printTable(table, target, dir, label);
        }
    }

    static void printTable(final float[] table, final String label) {
        final int size = table.length / 3;
        final double[] x = new double[size];
        final double[] delay1 = new double[size];
        final double[] delay2 = new double[size];
        int index = 0;
        for (int i = 0; i < size; ++i) {
            x[i] = table[index++];
            delay1[i] = table[index++];
            delay2[i] = delay1[i] + table[index++];
        }
        printTable(x, delay1, delay2, label);
    }

    static void printTable(final double[] x, final double[] delay1,
                           final double[] delay2, final String label) {
        System.err.println("L|" + label);
        System.err.printf(" |%7s %7s %6s %7s %6s\n",
                          "slew", "delay1", "deriv1", "delay2", "deriv2");
        for (int i = 0; i < x.length; ++i) {
            double deriv1 = 0, deriv2 = 0;
            if (i < x.length - 1) {
                deriv1 = (delay1[i + 1] - delay1[i]) / (x[i + 1] - x[i]);
                deriv2 = (delay2[i + 1] - delay2[i]) / (x[i + 1] - x[i]);
            }
            System.err.printf("D|%7.3f %7.3f %6.3f %7.3f %6.3f\n", x[i],
                              delay1[i], deriv1, delay2[i], deriv2);
        }
        System.err.println();
    }

    /**
     * Print debug information.  Does not change any data.
     **/
    static class PrintTable implements TableProcessor {
        private final String label;
        public PrintTable(final String label) {
            this.label = label;
        }
        public Object update(final int model, final Object trigger,
                             final Object old) {
            if (model == Rule.LINEAR_MODEL) {
                printTable((float[]) old, label + " " + trigger.toString());
            }
            return old;
        }
    }

    /**
     * Correct for discrete data sampling in alint.  alint outputs the voltage
     * every "post_step" time units.  aplot linearly interpolates between
     * observed data points.  Assume input waveform is exponential to correct
     * the aplot measurement.
     *
     * @param tau time constant
     * @param postStep alint .post_step setting
     * @param threshold ratio of voltage to true voltage
     * @return difference between aplot measured time to reach threshold and
     * actual time for input waveform to reach threshold
     **/
    private static double postStepCorrection(final double tau,
                                             final double postStep,
                                             final double threshold) {
        // find t when voltage actually reaches threshold
        // V=Vdd*exp(-t/tau)
        // ln(V/Vdd)=-t/tau
        // t=-ln(threshold)*tau
        final double t = -Math.log(threshold) * tau;

        // find bracketing sampling times
        final double nth = Math.floor(t / postStep);
        final double tl = postStep * nth;
        final double tr = postStep * (nth + 1);

        // find t' when aplot says voltage reaches threshold
        // Vl=Vdd*exp(-tl/tau) Vr=Vdd*exp(-tr/tau)
        // [Vl+(Vr-Vl)*(t'-tl)/postStep]/Vdd=threshold
        // exp(-tl/tau)+[exp(-tr/tau)-exp(-tl/tau)]*(t'-tl)/postStep=threshold
        // t'=[threshold-exp(-tl/tau)]*postStep/[exp(-tr/tau)-exp(-tl/tau)]+tl
        final double thl = Math.exp(-tl/tau);
        final double thr = Math.exp(-tr/tau);
        final double t_ = (threshold - thl) * postStep / (thr - thl) + tl;

        return t_ - t;
    }

    interface TableProcessor {
        Object update(final int model, final Object trigger, final Object old);
    }

    static class LinearTableProcessor implements TableProcessor {
        public Object update(final int model, final Object trigger,
                             final Object old) {
            if (model == Rule.LINEAR_MODEL) {
                final float[] table = (float[]) old;
                return update(table);
            } else {
                return old;
            }
        }
        protected float[] update(final float[] table) {
            return table;
        }
    }

    static Pair[] update(final Pair[] measuredDelay,
                         final Collection<TableProcessor> procs) {
        for (int i = 0; i < measuredDelay.length; ++i) {
            final Object trigger = measuredDelay[i].getFirst();
            final Pair p = (Pair) measuredDelay[i].getSecond();
            final int model = ((Integer) p.getFirst()).intValue();
            final Object oldTable = p.getSecond();
            Object newTable = oldTable;
            for (TableProcessor proc : procs) {
                newTable = proc.update(model, trigger, newTable);
            }
            if (oldTable != newTable) {
                measuredDelay[i] =
                    new Pair(trigger, new Pair(model, newTable));
            }
        }
        return measuredDelay;
    }

    static class PostStepCorrection extends LinearTableProcessor {
        private final double postStep;
        private final double oldth1;
        private final double oldth2;
        public PostStepCorrection(final double postStep,
                                  final double oldth1,
                                  final double oldth2) {
            this.postStep = postStep;
            this.oldth1 = oldth1;
            this.oldth2 = oldth2;
        }
        protected float[] update(final float[] table) {
            final float[] newTable = new float[table.length];
            for (int j = 0; j < table.length; j += 3) {
                final double offset =
                    postStepCorrection(
                            table[j] / Math.log(oldth1 / oldth2),
                            postStep, oldth1);
                newTable[j] = table[j];
                newTable[j + 1] =
                    table[j + 1] + (float) offset;
                newTable[j + 2] = table[j + 2];
            }
            return newTable;
        }
    }

    /**
     * Fit an exponential function of the form f(t)=exp(-(t-t0)/tau).
     * @param t1 f(t1)==th1, t1 &lt; t2
     * @param t2 f(t2)==th2, t2 &gt; t1
     * @param th1 f(t1)==th1
     * @param th2 f(t2)==th2
     * @return { tau, t0 }
     **/
    private static double[] fitExponential(final double t1, final double t2,
                                           final double Th1, final double Th2) {
        if (t1 >= t2) {
            throw new IllegalArgumentException("t1 should be less than t2: " +
                                               t1 + " " + t2);
        }
        // solve for tau and t0
        // th1 = exp(-(t1-t0)/tau)  th2 = exp(-(t2-t0)/tau)
        // tau*ln(th1) = t0-t1      tau*ln(th2) = t0-t2
        // tau*[ln(th1)-ln(th2)] = t2-t1
        // tau = (t2-t1)/ln(th1/th2)
        final double th1 = Th1 > Th2 ? Th1 : 1 - Th1;
        final double th2 = Th1 > Th2 ? Th2 : 1 - Th2;
        final double tau = (t2 - t1) / Math.log(th1 / th2);
        final double t0 = tau * Math.log(th1) + t1;
        return new double[] { tau, t0 };
    }

    private interface Waveform {
        /**
         * Time at which the specified voltage is reached
         **/
        double getTime(final double v);

        /**
         * Voltage reached at the specified time
         **/
        double getVoltage(final double t);
    }

    /**
     * A waveform of the form f(t)=exp(-(t-t0)/tau)
     **/
    private static class Exponential implements Waveform {
        private final double t0;
        private final double tau;
        public Exponential(final double t0, final double tau) {
            this.t0 = t0;
            this.tau = tau;
        }
        public double getTime(final double v) {
            return t0 - tau * Math.log(v);
        }
        public double getVoltage(final double t) {
            return Math.exp(-(t - t0) / tau);
        }
        public String toString() {
            return "Exponential(t0=" + t0 + ", tau=" + tau + ")";
        }
    }

    /**
     * A waveform of the form f(t)=a*t+b
     **/
    private static class Linear implements Waveform {
        private final double a;
        private final double b;
        public Linear(final double a, final double b) {
            this.a = a;
            this.b = b;
        }
        public double getTime(final double v) {
            return (v - b) / a;
        }
        public double getVoltage(final double t) {
            return a * t + b;
        }
        public String toString() {
            return "Linear(a=" + a + ", b=" + b + ")";
        }
    }

    /**
     * Redo alint measurement at arbitrary thresholds, assuming certain
     * waveforms for the input and output.
     **/
    private static void measure(final Waveform input, final Waveform output,
                                final double Th1, final double Th2,
                                final float[] result, final int index) {
        final double th1 = Th1 > Th2 ? Th1 : 1 - Th1;
        final double th2 = Th1 > Th2 ? Th2 : 1 - Th2;

        final double t0 = input.getTime(th1);
        final double t1 = output.getTime(th1);
        final double inputSlew = input.getTime(th2) - t0;
        final double outputDelay = t1 - t0;
        final double outputSlew = output.getTime(th2) - t1;
        result[index] = (float) inputSlew;
        result[index + 1] = (float) outputDelay;
        result[index + 2] = (float) outputSlew;
    }

    /**
     * Fit data to waveforms.
     **/
    private static Waveform[] getWaveforms(final double inputSlew,
                                           final double outputDelay,
                                           final double outputSlew,
                                           final double Oldth1,
                                           final double Oldth2,
                                           final int d2a_shape) {
        final Waveform input, output;
        final double oldth1, oldth2;
        if (d2a_shape == 0) {  // exponential
            if (Oldth1 < Oldth2) {
                oldth1 = 1 - Oldth1;
                oldth2 = 1 - Oldth2;
            } else {
                oldth1 = Oldth1;
                oldth2 = Oldth2;
            }
            input = new Exponential(0, inputSlew / Math.log(oldth1 / oldth2));
            final double[] coef = 
                fitExponential(outputDelay, outputDelay + outputSlew,
                               oldth1, oldth2);
            output = new Exponential(coef[1] + input.getTime(oldth1), coef[0]);
        } else if (d2a_shape == 1) { // linear
            if (Oldth1 < Oldth2) {
                oldth1 = 1 - Oldth1;
                oldth2 = 1 - Oldth2;
            } else {
                oldth1 = Oldth1;
                oldth2 = Oldth2;
            }
            input = new Linear((oldth2 - oldth1) / inputSlew, 1);
            final double m = (oldth2 - oldth1) / outputSlew;
            final double b = oldth1 - m * outputDelay;
            output = new Linear(m, b - m * input.getTime(oldth1));
        } else {
            throw new RuntimeException("Unsupported d2a_shape: " + d2a_shape);
        }
        return new Waveform[] { input, output };
    }

    /**
     * alint data is measured at certain thresholds.  Assume the input and
     * output waveforms are known, recompute the data at different thresholds.
     *
     * @param inputSlew time from input waveform reaching oldth1 to oldth2
     * @param outputDelay time from input waveform reaching oldth1 to output
     * waveform reaching oldth1
     * @param ouputSlew time from output waveform reaching oldth1 to output
     * waveform reaching oldth2
     * @param oldth1 first threshold used for measuring data
     * @param oldth2 second threshold used for measuring data
     * @param newth1 first new threshold for data
     * @param newth2 second new threshold for data
     * @param result table to write new data to
     * @param index index in the table to update
     * @param d2a_shape shape of input waveform
     **/
    private static void shiftThreshold(final double inputSlew,
                                       final double outputDelay,
                                       final double outputSlew,
                                       final double oldth1,
                                       final double oldth2,
                                       final double newTh1,
                                       final double newTh2,
                                       final float[] result,
                                       final int index,
                                       final int d2a_shape) {
        final Waveform[] waves =
            getWaveforms(inputSlew, outputDelay, outputSlew, oldth1, oldth2,
                         d2a_shape);
        measure(waves[0], waves[1], newTh1, newTh2, result, index);
    }

    /**
     * Shift thresholds from oldth1..oldth2 to newth1..newth2, given the input
     * wave form has the specified shape (corresponds to the setting of
     * .d2a_shape in alint.)
     **/
    static class ShiftThreshold extends LinearTableProcessor {
        private final double oldth1;
        private final double oldth2;
        private final double newth1;
        private final double newth2;
        private final int shape;
        public ShiftThreshold(final double oldth1, final double oldth2,
                              final double newth1, final double newth2,
                              final int shape) {
            this.oldth1 = oldth1;
            this.oldth2 = oldth2;
            this.newth1 = newth1;
            this.newth2 = newth2;
            this.shape = shape;
        }

        protected float[] update(final float[] table) {
            final float[] newTable = new float[table.length];
            for (int j = 0; j < table.length; j += 3) {
                shiftThreshold(table[j], table[j + 1], table[j + 2],
                               oldth1, oldth2, newth1, newth2,
                               newTable, j, shape);
            }
            return newTable;
        }
    }

    /**
     * Scales delay by the given scale factor.
     **/
    static class ScaleDelay implements TableProcessor {
        private final double delayScale;
        public ScaleDelay(final double delayScale) {
            this.delayScale = delayScale;
        }
        public Object update(final int model, final Object trigger,
                             final Object old) {
            if (model == Rule.LINEAR_MODEL) {
                final float[] table = (float[]) old;
                final float[] newTable = new float[table.length];
                for (int j = 0; j < table.length; j += 3) {
                    newTable[j] = table[j];
                    newTable[j + 1] = (float) (table[j + 1] * delayScale);
                    newTable[j + 2] = table[j + 2];
                }
                return newTable;
            } else if (model == Rule.POLYNOMIAL_MODEL) {
                final Pair p = (Pair) old;
                final float[] delayCoeff = (float[]) p.getFirst();
                final float[] newCoeff = new float[delayCoeff.length];
                for (int j = 0; j < delayCoeff.length; ++j) {
                    newCoeff[j] = (float) (delayCoeff[j] * delayScale);
                }
                return new Pair(newCoeff, p.getSecond());
            } else {
                return old;
            }
        }
    }

    /**
     * Workaround for bug 19653: remove datapoints with 0 output delay/slew.
     **/
    static class RemoveZeroData implements TableProcessor {
        private final NullaryAction notifier;
        public RemoveZeroData(final NullaryAction notifier) {
            this.notifier = notifier;
        }
        private final boolean isZero(final float[] table, final int idx) {
            return table[idx + 1] == 0 || table[idx + 2] == 0;
        }
        public Object update(final int model, final Object trigger,
                             final Object old) {
            if (model == Rule.LINEAR_MODEL) {
                final float[] table = (float[]) old;

                // scan for entries to eliminate, and notify
                int count = 0;
                for (int j = 0; j < table.length; j += 3) {
                    if (isZero(table, j)) {
                        count++;
                        notifier.execute();
                    }
                }

                // return old table if no changes needed
                if (count == 0) return old;

                // construct new table
                final float[] newTable = new float[table.length - 3 * count];
                int i = 0;
                for (int j = 0; j < table.length; j += 3) {
                    if (!isZero(table, j)) {
                        newTable[i] = table[j];
                        newTable[i + 1] = table[j + 1];
                        newTable[i + 2] = table[j + 2];
                        i += 3;
                    }
                }
                return newTable;
            } else {
                return old;
            }
        }
    }
}
