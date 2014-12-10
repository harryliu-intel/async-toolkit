/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

// vim:sw=4:expandtab:ai:cin:ts=4

package com.avlsi.tools.dsim;

import java.lang.reflect.Constructor;
import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.BrokenBarrierException;
import java.util.concurrent.CyclicBarrier;

import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.tools.cadencize.CadenceInfo;
import com.avlsi.tools.jauto.ConjugateGradientSolver;
import com.avlsi.tools.dsim.DSim.MeasuredDelay;
import com.avlsi.util.container.BoundedSet;
import com.avlsi.util.container.IterableIterator;
import com.avlsi.util.container.MultiMap;
import com.avlsi.util.container.MutableDouble;
import com.avlsi.util.container.MutableInt;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.Triplet;
import com.avlsi.util.math.QuadraticInterpolation;
import com.avlsi.util.functions.UnaryPredicate;
import com.avlsi.util.text.StringUtil;

public class RunStaticTiming {
    private static final double EPSILON = 1e-12;

    public static class SolverException extends RuntimeException {
        public SolverException(final String mesg, final Throwable cause) {
            super(mesg, cause);
        }
    }

    private static boolean isErrorRule(final Rule r) {
        return r.asserted;
    }

    private static class AmplSolver {
        private Map<Node,Integer> nodesMap;
        private Set<Node> observableNodes;
        private int constraints;

        private String getVarName(Node n, int dir) {
            if (observableNodes.contains(n)) {
                return "0";
            } else {
                final Integer idx = nodesMap.get(n);
                final String prefix = dir > 0 ? "up" : "dn";
                return prefix + idx;
            }
        }

        private void addVar(Node n) {
            if (!observableNodes.contains(n)) {
                final Integer idx = nodesMap.get(n);
                if (idx == null) nodesMap.put(n, nodesMap.size());
            }
        }

        public AmplSolver(final DSim dsim,
                          final float defaultSlew,
                          final Set<Node> observableNodes)
        throws FileNotFoundException {
            this.nodesMap = new HashMap<Node,Integer>();
            this.observableNodes = observableNodes;
            this.constraints = 0;

            final PrintStream mod = new PrintStream("asta.mod");
            final PrintStream cmd = new PrintStream("asta.cmd");
            dsim.foreachRule(new Node.RuleFunc() {
                public void accept(Rule r, int sense, Node n) {
                    if (isErrorRule(r)) return;
                    addVar(n);
                    addVar(r.target());
                }
            });
            for (String prefix : new String[] { "up", "dn" }) {
                for (Integer idx : nodesMap.values()) {
                    String id = prefix + idx;
                    mod.println("var " + id + ";");
                    cmd.println("printf '" + id + " = %g\\n', " + id + ";");
                }
            }
            dsim.foreachRule(new Node.RuleFunc() {
                public void accept(Rule r, int sense, Node n) {
                    if (isErrorRule(r)) return;
                    final String guard = getVarName(n, sense);
                    final String target = getVarName(r.target(), r.dir);
                    if (guard.equals("0") && target.equals("0")) return;

                    final String constraint = "c" + constraints;
                    final float[] delaySlew = new float[] { r.delay, 0 };
                    n.setSlew(defaultSlew);
                    r.getMeasured(n, delaySlew);
                    mod.print("/* " + n.getName() + " -> " + r.target().getName() +
                              " " + r.delay + " " + delaySlew[0] + " */\n");
                    mod.print("subject to " + constraint + ": " +
                              target + " - " + guard + " + " +
                              (r.delay - delaySlew[0]) + " >= 0;\n");
                    ++constraints;
                }
            });
            boolean first = true;
            for (String prefix : new String[] { "up", "dn" }) {
                for (Integer idx : nodesMap.values()) {
                    if (first) {
                        mod.print("minimize obj: ");
                        first = false;
                    } else {
                        mod.print(" + ");
                    }
                    String id = prefix + idx;
                    mod.print(id + "**2");
                }
            }
            mod.println(";");
            mod.close();
            cmd.close();
        }
    }

    private static HierName toHier(final String s) {
        try {
            return HierName.makeHierName(s, '.');
        } catch (InvalidHierNameException e) {
            throw new RuntimeException("Cannot create HierName: " + s);
        }
    }

    private static class Offset {
        public final double[] times;
        public Offset() {
            times = new double[4];
            set(true, true, Double.NEGATIVE_INFINITY);
            set(false, true, Double.NEGATIVE_INFINITY);
            set(true, false, Double.POSITIVE_INFINITY);
            set(false, false, Double.POSITIVE_INFINITY);
        }
        private final int index(boolean T1, boolean input) {
            return (T1 ? 2 : 0) + (input ? 1: 0);
        }
        public double get(boolean T1, boolean input) {
            return times[index(T1, input)];
        }
        public void set(boolean T1, boolean input, double value) {
            times[index(T1, input)] = value;
        }
        public void update(boolean T1, double value) {
            set(T1, true, Math.max(get(T1, true), value));
            set(T1, false, Math.min(get(T1, false), value));
        }
        public void updateIfUnset(boolean T1, double value) {
            final double input = get(T1, true);
            final double output = get(T1, false);
            if (Double.isInfinite(input) || Double.isInfinite(output)) {
                update(T1, value);
            }
        }
    }

    private static class CGSolver
    implements ConjugateGradientSolver.Function,
               ConjugateGradientSolver.StopCondition {
        /** A parameter to tune the quadratic interpolation. **/
        private double alpha = 0.5;
        private double minDeriv = Double.NaN;
        private double maxDeriv = Double.NaN;
        private double tolerance = 1e-3;

        /** Parameters related to the solver.**/
        private static final double MIN_GRADIENT = 1e-20;
        private static final double MIN_GRAD_MAG = MIN_GRADIENT * MIN_GRADIENT;

        private static final int UNINIT_INDEX = -1;

        /** Offset of the first time variable (time at 1/3) in the arrray. **/
        private final int T1;

        /** Offset of the second time variable (time at 2/3) in the array. **/
        private final int T2;

        private final int OUTPUT = 0;
        private final int INPUT = 1;

        private final DSim dsim;
        private final ArrayList<Offset> fixed = new ArrayList<Offset>();
        private float tau;
        final float defaultSlew;
        private int iterations = 0;
        private int max_iterations;
        private final ArrayList<Map<Node,QuadraticInterpolation[]>> interpols =
            new ArrayList<Map<Node,QuadraticInterpolation[]>>();
        private double[] X;
        private int constraintCount;
        private int varCount = 0;

        /** Time limit on the solver, in milliseconds since the Unix epoch. **/
        private long endTime = Long.MAX_VALUE;

        /** Multithread support **/
        private Workers<EnergyWorker> energyWorkers = null;
        private Workers<GradientWorker> gradientWorkers = null;

        private Map<Rule,Integer> measureSelected =
            new IdentityHashMap<Rule,Integer>();

        private class Workers<T extends Runnable> implements Iterable<T> {
            final CyclicBarrier barrier;
            final ArrayList<T> workers;
            final String name;
            public Workers(final Class<T> c, final int count,
                           final String name, final Node[][] assignment) {
                this.name = name;
                barrier = new CyclicBarrier(count + 1);
                workers = new ArrayList<T>(count);
                try {
                    final Constructor<T> ctor =
                        c.getConstructor(CGSolver.class,
                                         CyclicBarrier.class,
                                         (new Node[0]).getClass());
                    for (int id = 0; id < count; ++id) {
                        workers.add(
                            ctor.newInstance(CGSolver.this, barrier,
                                             assignment[id]));
                    }
                } catch (Exception e) {
                    throw new RuntimeException("Cannot start workers", e);
                }
            }
            public void start() {
                int i = 0;
                for (T worker : workers) {
                    new Thread(worker, name + " " + i).start();
                    i++;
                }
            }
            public Iterator<T> iterator() {
                return workers.iterator();
            }
        }

        private abstract class Worker implements Runnable {
            private final CyclicBarrier barrier;
            private final Node[] assignment;
            public Worker(final CyclicBarrier barrier,
                          final Node[] assignment) {
                this.barrier = barrier;
                this.assignment = assignment;
            }
            public void run() {
                while (true) {
                    try {
                        barrier.await();
                        for (Node n : assignment) {
                            if (n != null) runTask(n);
                        }
                        barrier.await();
                    } catch (BrokenBarrierException e) {
                        throw new RuntimeException(e);
                    } catch (InterruptedException e) {
                        throw new RuntimeException(e);
                    }
                }
            }
            protected abstract void runTask(final Node n);
        }


        public int getSize() {
            return varCount;
        }

        private final boolean isFixed(final Node n) {
            return n.isRandom();
        }

        private final void setFixed(final Node n) {
            n.setRandom(true);
        }

        private final void setUnfixed(final Node n) {
            n.setRandom(false);
        }

        private final Node canon(final Node n) {
            return n.enabler == null ? n : (Node) n.enabler;
        }

        private void addVar(final Node n, int sense) {
            if (!isFixed(n)) {
                if (n.getTCount() == UNINIT_INDEX) {
                    final Node canon = (Node) n.enabler;
                    if (canon == null || canon == n) {
                        n.setTCount(varCount);
                        varCount += 2;
                    } else {
                        addVar(canon, sense);
                        n.setTCount(canon.getTCount());
                    }
                }
            }
        }

        private final int getIndex(final Node n, final int sense) {
            return n.getTCount() + sense;
        }

        private final int getIndex(final Node n, final int sense,
                                   final int t12) {
            return getIndex(n, sense) + t12;
        }

        private final double getVar(double[] v, Node n, int sense, int t12,
                                    int input) {
            if (isFixed(n)) {
                return fixed.get(getIndex(n, sense))
                            .get(t12 == T1, input == INPUT);
            } else {
                return v[getIndex(n, sense, t12)];
            }
        }

        // return the budgeted time for the production rule in pS
        private final float getBudget(Rule r) {
            return Math.max(1, r.delay) * tau / 100;
        }

        private final void accumVar(double[] accum, Node n, int sense, int t12,
                                    double val) {
            if (!isFixed(n)) {
                accum[getIndex(n, sense, t12)] += val;
            }
        }

        private String strRule(double[] v, Rule r, int sense, boolean rep,
                               Node n) {
            final String detail;
            if (v == null) {
                if (rep && r.falseConjuncts > 1) {
                    detail = " x" + r.falseConjuncts;
                } else {
                    detail = "";
                }
            } else {
                final double it1 = getVar(v, n, sense, T1, INPUT);
                final double it2 = getVar(v, n, sense, T2, INPUT);
                final double ot1 = getVar(v, r.target(), r.dir, T1, OUTPUT);
                final double ot2 = getVar(v, r.target(), r.dir, T2, OUTPUT);
                final double islew = it2 - it1;
                final double oslew = ot2 - ot1;
                detail = " it1=" + it1 +
                         " it2=" + it2 +
                         " islew=" + islew +
                         " budget=" + getBudget(r) +
                         " delay1=" + getDelay(r, n, islew, T1) +
                         " delay2=" + getDelay(r, n, islew, T2) +
                         " ot1=" + ot1 +
                         " ot2=" + ot2 +
                         " oslew=" + oslew +
                         " count=" + r.falseConjuncts;
            }
            return n.getName() + (sense > 0 ? "+" : "-") + " -> " +
                   r.target().getName() + r.getDirection() + detail;
        }

        private QuadraticInterpolation[] getInterpolation(final Rule r,
                                                          final Node n) {
            if (r.transitionCount == UNINIT_INDEX) {
                return null;
            } else {
                final Map<Node,QuadraticInterpolation[]> ruleMap =
                    interpols.get(r.transitionCount);
                return ruleMap.get(canon(n));
            }
        }

        private final double getDelay(final Rule r,
                                      final Node n,
                                      final double slew,
                                      final int ot12) {
            final QuadraticInterpolation[] qi = getInterpolation(r, n);
            if (qi == null) {
                return getBudget(r);
            } else {
                return qi[ot12 == T1 ? 0 : 1].value(slew);
            }
        }

        private final double getDelayGradient(final Rule r,
                                              final Node n,
                                              final double slew,
                                              final int ot12) {
            final QuadraticInterpolation[] qi = getInterpolation(r, n);
            if (qi == null) {
                return 0;
            } else {
                return qi[ot12 == T1 ? 0 : 1].gradient(slew);
            }
        }

        private final double getSlack(final double[] v,
                                      final Rule r,
                                      final int sense,
                                      final Node n,
                                      final int ot12) {
            final double it1 = getVar(v, n, sense, T1, INPUT);
            final double it2 = getVar(v, n, sense, T2, INPUT);
            final double ot = getVar(v, r.target(), r.dir, ot12, OUTPUT);
            final double result = ot - it1 + getBudget(r) -
                                  getDelay(r, n, it2 - it1, ot12);
            return result;
        }

        private double getTau(final double[] v, Rule r, int sense, Node n) {
            final double slack1 = getSlack(v, r, sense, n, T1);
            final double slack2 = getSlack(v, r, sense, n, T2);
            final double slack = Math.min(slack1, slack2);
            final double budget = getBudget(r);
            return ((budget - slack) / budget) * tau;
        }

        private class EnergyWorker extends Worker {
            double[] v = null;
            double energy = 0;
            public EnergyWorker(final CyclicBarrier barrier,
                                final Node[] assignment) {
                super(barrier, assignment);
            }
            protected void runTask(final Node n) {
                n.foreachRule(new Node.RuleFunc() {
                    public void accept(Rule r, int sense, Node n) {
                        final int instances = r.falseConjuncts;
                        if (isErrorRule(r) || instances == 0) return;
                        energy += getValue(v, r, sense, n, instances);
                    }
                });
            }
        }

        private double getValue(final double[] v, final Rule r, final int sense,
                                final Node n, final int instances) {
            double result = 0;

            final double budgetTau = getBudget(r) * tau;
            final double slack1 = getSlack(v, r, sense, n, T1);
            if (slack1 < 0) result += (slack1 * slack1 / budgetTau) * instances;

            final double slack2 = getSlack(v, r, sense, n, T2);
            if (slack2 < 0) result += (slack2 * slack2 / budgetTau) * instances;

            return result;
        }

        public double getValue(final double[] v) {
            final double[] energy = new double[] { 0 };
            if (energyWorkers == null) {
                dsim.foreachRule(new Node.RuleFunc() {
                    public void accept(Rule r, int sense, Node n) {
                        final int instances = r.falseConjuncts;
                        if (isErrorRule(r) || instances == 0) return;
                        energy[0] += getValue(v, r, sense, n, instances);
                    }
                });
            } else {
                try {
                    for (EnergyWorker worker : energyWorkers) {
                        worker.energy = 0;
                        worker.v = v;
                    }
                    energyWorkers.barrier.await();
                    energyWorkers.barrier.await();
                    for (EnergyWorker worker : energyWorkers) {
                        energy[0] += worker.energy;
                    }
                } catch (BrokenBarrierException e) {
                    throw new RuntimeException(e);
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
            }

            //System.out.println("energy = " + energy[0]);
            return energy[0];
        }

        private final void accumNegativeGradient(final double[] v,
                                                 final double[] accum,
                                                 final Rule r,
                                                 final int sense,
                                                 final Node n,
                                                 final int ot12,
                                                 final int instances) {
            // accumulate gradients related to delay constraint
            final double sgrad = -2 * getSlack(v, r, sense, n, ot12) /
                                 getBudget(r) / tau * instances;
            if (sgrad > 0) {
                accumVar(accum, r.target(), r.dir, ot12, sgrad);
                final double it1 = getVar(v, n, sense, T1, INPUT);
                final double it2 = getVar(v, n, sense, T2, INPUT);
                final double dgrad = getDelayGradient(r, n, it2 - it1, ot12);
                accumVar(accum, n, sense, T1, sgrad * (dgrad - 1));
                accumVar(accum, n, sense, T2, sgrad * -dgrad);
            }
        }

        private final void accumNegativeGradient(final double[] v,
                                                 final double[] accum,
                                                 final Rule r,
                                                 final int sense,
                                                 final Node n,
                                                 final int instances) {
            accumNegativeGradient(v, accum, r, sense, n, T1, instances);
            accumNegativeGradient(v, accum, r, sense, n, T2, instances);
        }

        private class GradientWorker extends Worker {
            double[] v = null;
            double[] accum = null;
            public GradientWorker(final CyclicBarrier barrier,
                                  final Node[] assignment) {
                super(barrier, assignment);
            }
            protected void runTask(final Node n) {
                n.foreachRule(new Node.RuleFunc() {
                    public void accept(Rule r, int sense, Node n) {
                        final int instances = r.falseConjuncts;
                        if (isErrorRule(r) || instances == 0) return;
                        accumNegativeGradient(v, accum, r, sense, n, instances);
                    }
                });
            }
        }

        public void getNegativeGradient(final double[] v,
                                        final double[] accum) {
            if (gradientWorkers == null) {
                dsim.foreachRule(new Node.RuleFunc() {
                    public void accept(Rule r, int sense, Node n) {
                        final int instances = r.falseConjuncts;
                        if (isErrorRule(r) || instances == 0) return;
                        accumNegativeGradient(v, accum, r, sense, n, instances);
                    }
                });
            } else {
                try {
                    for (GradientWorker worker : gradientWorkers) {
                        worker.v = v;
                        if (worker.accum == null) {
                            worker.accum = new double[accum.length];
                        }
                        for (int i = 0; i < accum.length; ++i) {
                            worker.accum[i] = 0;
                        }
                    }
                    gradientWorkers.barrier.await();
                    gradientWorkers.barrier.await();
                    for (GradientWorker worker : gradientWorkers) {
                        for (int i = 0; i < accum.length; ++i) {
                            accum[i] += worker.accum[i];
                        }
                    }
                } catch (BrokenBarrierException e) {
                    throw new RuntimeException(e);
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
            }
        }

        private void printFlatViolations(final double[] v,
                                         final double worstTau,
                                         int max,
                                         final boolean verbose,
                                         final boolean canonRulesOnly,
                                         final PrintWriter pw) {
            final Set<Pair<Double,String>> violations =
                new BoundedSet<Pair<Double,String>>(
                        max, new TreeSet<Pair<Double,String>>());
            dsim.foreachRule(new Node.RuleFunc() {
                public void accept(Rule r, int sense, Node n) {
                    if (isErrorRule(r)) return;
                    if (canonRulesOnly && r.getCanonRule() != null) return;
                    final double tau = getTau(v, r, sense, n);
                    if (tau >= worstTau) {
                        violations.add(
                            new Pair<Double,String>(-tau,
                                strRule(verbose ? v : null, r, sense,
                                        canonRulesOnly, n)));
                    }
                }
            });
            for (Pair<Double,String> vio : violations) {
                if (max <= 0) break;
                pw.printf("%.3f %s\n", -vio.getFirst(), vio.getSecond());
                --max;
            }
            if (violations.isEmpty()) {
                pw.printf("No transitions with tau >= %.3f\n", worstTau);
            }
        }

        double getWorstTau() {
            final MutableDouble result =
                new MutableDouble(Double.NEGATIVE_INFINITY);
            dsim.foreachRule(new Node.RuleFunc() {
                public void accept(Rule r, int sense, Node n) {
                    if (isErrorRule(r)) return;
                    if (r.getCanonRule() != null) return;
                    final double tau = getTau(X, r, sense, n);
                    result.max(tau);
                }
            });
            return result.get();
        }

        void printFlatViolations(final double worstTau, final int max,
                                 final boolean verbose,
                                 final boolean canonRulesOnly,
                                 final PrintWriter pw) {
            printFlatViolations(X, worstTau, max, verbose, canonRulesOnly, pw);
        }

        private static class ViolationType
        implements Comparable<ViolationType> {
            private final String fqcn;
            private double worstTau;
            private Map<Pair<HierName,Integer>,Double> violations;
            private Set<HierName> instances;
            public ViolationType(final String fqcn) {
                this.fqcn = fqcn;
                this.worstTau = Double.NEGATIVE_INFINITY;
                this.violations = new HashMap<Pair<HierName,Integer>,Double>();
                this.instances = new HashSet<HierName>();
            }
            public void update(final HierName instance,
                               final Pair<HierName,Integer> target,
                               final double tau) {
                instances.add(instance);
                Double prevTau = violations.get(target);
                if (prevTau == null || tau > prevTau) {
                    worstTau = Math.max(worstTau, tau);
                    violations.put(target, tau);
                }
            }
            public int compareTo(ViolationType o) {
                int result = Double.compare(worstTau, o.worstTau);
                if (result != 0) return -result;
                else return fqcn.compareTo(o.fqcn);
            }
            public void print(final PrintWriter pw) {
                pw.printf("%s %d\n", fqcn, instances.size());
                final Set<Map.Entry<Pair<HierName,Integer>,Double>> sorted =
                    new TreeSet<Map.Entry<Pair<HierName,Integer>,Double>>(
                        new Comparator<Map.Entry<Pair<HierName,Integer>,
                                                 Double>>() {
                            public int compare(
                                Map.Entry<Pair<HierName,Integer>,Double> o1,
                                Map.Entry<Pair<HierName,Integer>,Double> o2) {
                                int x = Double.compare(o1.getValue(),
                                                       o2.getValue());
                                if (x != 0) return -x;
                                return o1.getKey().toString().compareTo(
                                           o2.getKey().toString());
                            }
                        });
                sorted.addAll(violations.entrySet());
                for (Map.Entry<Pair<HierName,Integer>,Double> entry : sorted) {
                    pw.printf("  %.3f %s%c\n", entry.getValue(),
                              entry.getKey().getFirst(),
                              entry.getKey().getSecond() > 0 ? '+' : '-');
                }
            }
        }

        private void printLocalViolations(final double[] v,
                                          final double worstTau,
                                          final PrintWriter pw) {
            final Map<String,ViolationType> types =
                new HashMap<String,ViolationType>();

            dsim.foreachRule(new Node.RuleFunc() {
                private void update(final Rule r, final double tau) {
                    final Triplet<HierName,CadenceInfo,HierName> local =
                        DSim.localize(r.target.getName());
                    final String fqcn = local.getSecond().getType();
                    ViolationType vtype = types.get(fqcn);
                    if (vtype == null) {
                        vtype = new ViolationType(fqcn);
                        types.put(fqcn, vtype);
                    }
                    vtype.update(
                        local.getFirst(),
                        new Pair<HierName,Integer>(local.getThird(), r.dir),
                        tau);
                }
                public void accept(Rule r, int sense, Node n) {
                    if (isErrorRule(r)) return;
                    final double tau = getTau(v, r, sense, n);
                    if (tau >= worstTau) update(r, tau);
                }
            });

            if (types.isEmpty()) {
                pw.printf("No transitions with tau >= %.3f\n", worstTau);
            } else {
                for (ViolationType type :
                        new TreeSet<ViolationType>(types.values())) {
                    type.print(pw);
                }
            }
        }

        void printLocalViolations(final double worstTau, final PrintWriter pw) {
            printLocalViolations(X, worstTau, pw);
        }

        private HierName getLocalName(final Collection<HierName> aliases,
                                      final HierName prefix) {
            final TreeSet<HierName> names = new TreeSet<HierName>();
            final int n = prefix.getNumComponents();
            for (HierName alias : aliases) {
                if (alias.isChildOf(prefix)) {
                    names.add(alias.tail(n));
                }
            }
            return names.first();
        }

        private CellInterface getInstanceType(final CellInterface cell,
                                              final HierName prefix) {
            if (prefix == null) return cell;

            for (Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
                final Pair p = (Pair) i.next();
                final HierName instance = (HierName) p.getFirst();
                if (prefix.isChildOf(instance)) {
                    return getInstanceType(
                            (CellInterface) p.getSecond(),
                            prefix.tail(instance.getNumComponents()));
                } else if (prefix.equals(instance)) {
                    return (CellInterface) p.getSecond();
                }
            }

            return null;
        }

        private static class ViolationValue {
            int count;
            double max;
            public ViolationValue() {
                this.count = 0;
                this.max = Double.NEGATIVE_INFINITY;
            }
            public void update(final double slack) {
                ++count;
                max = Math.max(max, slack);
            }
            public String toString() {
                return String.format("%d %.3f", count, max);
            }
        }

        private static class ViolationKey {
            final CellInterface cell;
            final HierName guard;
            final HierName target;
            public ViolationKey(final CellInterface cell, final HierName guard,
                                final HierName target) {
                this.cell = cell;
                this.guard = guard;
                this.target = target;
            }
            public int hashCode() {
                return guard.hashCode() + target.hashCode() +
                       cell.getFullyQualifiedType().hashCode();
            }
            public boolean equals(final Object o) {
                if (o instanceof ViolationKey) {
                    final ViolationKey k = (ViolationKey) o;
                    return cell == k.cell && guard.equals(k.guard) &&
                           target.equals(k.target);
                } else {
                    return false;
                }
            }
            public String toString() {
                return guard + " -> " + target;
            }
        }

        private void printHierViolations(final double[] v,
                                         final double worstTau,
                                         final PrintWriter pw) {
            final CellInterface dut;
            try {
                dut = dsim.getCell();
            } catch (Exception e) {
                System.err.println("Cannot get top-level cell to print out " +
                                   "hierarchical violations.");
                return;
            }

            final Set<Node> nodes = new HashSet<Node>();
            dsim.foreachRule(new Node.RuleFunc() {
                public void accept(Rule r, int sense, Node n) {
                    if (isErrorRule(r)) return;
                    final double tau = getTau(v, r, sense, n);
                    if (tau >= worstTau) {
                        nodes.add(r.target());
                        nodes.add(n);
                    }
                }
            });

            final MultiMap<Node,HierName> aliases =
                dsim.generateNodeToAliases(nodes);
            final Map<ViolationKey,ViolationValue> violations =
                new HashMap<ViolationKey,ViolationValue>();
            dsim.foreachRule(new Node.RuleFunc() {
                public void accept(Rule r, int sense, Node n) {
                    if (isErrorRule(r)) return;
                    final double tau = getTau(v, r, sense, n);
                    if (tau >= worstTau) {
                        final CellInterface type =
                            getInstanceType(dut, r.prefix.tail(1));
                        final HierName localGuard =
                            getLocalName(aliases.get(n), r.prefix);
                        final HierName localTarget =
                            getLocalName(aliases.get(r.target()), r.prefix);
                        final ViolationKey key =
                            new ViolationKey(type, localGuard, localTarget);
                        ViolationValue val = violations.get(key);
                        if (val == null) {
                            val = new ViolationValue();
                            violations.put(key, val);
                        }
                        val.update(tau);
                    }
                }
            });

            final Map<String,MultiMap<Double,String>> results =
                new TreeMap<String,MultiMap<Double,String>>();
            for (Map.Entry<ViolationKey,ViolationValue> vio :
                    violations.entrySet()) {
                final ViolationKey key = vio.getKey();
                final ViolationValue val = vio.getValue();
                final String type = key.cell.getFullyQualifiedType();
                MultiMap<Double,String> result = results.get(type);
                if (result == null) {
                    result = new MultiMap<Double,String>(
                                 new TreeMap<Double,Collection<String>>());
                    results.put(type, result);
                }
                result.put(val.max, key.toString() + " " + val.toString());
            }

            for (Map.Entry<String,MultiMap<Double,String>> entry :
                    results.entrySet()) {
                pw.println(entry.getKey());
                final MultiMap<Double,String> result = entry.getValue();
                for (Double d : result.keySet()) {
                    for (String s : result.get(d)) {
                        pw.println("  " +  s);
                    }
                }
            }
        }

        void printHierViolations(final double worstTau, final PrintWriter pw) {
            printHierViolations(X, worstTau, pw);
        }

        private MultiMap<Double,HierName> getViolationByPrefix(
                final double[] v, final double worstTau,
                final boolean canonRulesOnly) {
            final Map<HierName,MutableDouble> byPrefix =
                new HashMap<HierName,MutableDouble>();
            dsim.foreachRule(new Node.RuleFunc() {
                public void accept(Rule r, int sense, Node n) {
                    if (isErrorRule(r)) return;
                    if (canonRulesOnly && r.getCanonRule() != null) return;
                    final double tau = getTau(v, r, sense, n);
                    if (tau >= worstTau) {
                        MutableDouble worstSoFar = byPrefix.get(r.prefix);
                        if (worstSoFar == null) {
                            worstSoFar = new MutableDouble(0);
                            byPrefix.put(r.prefix, worstSoFar);
                        }
                        worstSoFar.max(tau);
                    }
                }
            });

            final MultiMap<Double,HierName> result =
                new MultiMap<Double,HierName>(
                    new TreeMap<Double,Collection<HierName>>(
                        new Comparator<Double>() {
                            public int compare(Double d1, Double d2) {
                                return Double.compare(d2, d1);
                            }
                        }));

            for (Map.Entry<HierName,MutableDouble> vio : byPrefix.entrySet()) {
                result.put(vio.getValue().get(), vio.getKey());
            }

            return result;
        }

        MultiMap<Double,HierName> getViolationByPrefix(
                final double worstTau, final boolean canonRulesOnly) {
            return getViolationByPrefix(X, worstTau, canonRulesOnly);
        }

        private void printResult(final double[] v) {
            final Set<Pair<Node,Integer>> seen =
                new HashSet<Pair<Node,Integer>>();
            dsim.foreachRule(new Node.RuleFunc() {
                private void printNode(Node n, int sense) {
                    if (seen.add(new Pair<Node,Integer>(n, sense))) {
                        System.out.print("t(" + n.getName() +
                                         (sense > 0 ? "+" : "-") + ") = ");
                        if (isFixed(n)) {
                            System.out.println("0 (fixed)");
                        } else {
                            final int idx1 = getIndex(n, sense, T1);
                            final int idx2 = getIndex(n, sense, T2);
                            System.out.printf("%.3g %.3g %.3g %d\n",
                                              v[idx1], v[idx2],
                                              v[idx2] - v[idx1],
                                              idx1);
                        }
                    }
                }
                public void accept(Rule r, int sense, Node n) {
                    if (isErrorRule(r)) return;
                    printNode(n, sense);
                    printNode(r.target(), r.dir);
                }
            });
        }

        private static String dotNode(Node n, int sense) {
            return "\"" + n.getName() + (sense > 0 ? "+" : "-") + "\"";
        }

        private void writeDot(final double[] v) throws IOException {
            final PrintWriter pw = new PrintWriter(new FileWriter("sta.dot"));
            pw.println("digraph sta {");
            final Set<Pair<Node,Integer>> seen =
                new HashSet<Pair<Node,Integer>>();
            dsim.foreachRule(new Node.RuleFunc() {
                private void printNode(Node n, int sense) {
                    if (seen.add(new Pair<Node,Integer>(n, sense))) {
                        pw.println(dotNode(n, sense) + ";");
                    }
                }
                public void accept(Rule r, int sense, Node n) {
                    if (isErrorRule(r)) return;
                    printNode(n, sense);
                    printNode(r.target(), r.dir);
                }
            });
            dsim.foreachRule(new Node.RuleFunc() {
                public void accept(Rule r, int sense, Node n) {
                    if (isErrorRule(r)) return;
                    pw.println(dotNode(n, sense) + " -> " +
                               dotNode(r.target(), r.dir) + ";");
                }
            });
            pw.println("}");
            pw.close();
        }

        public boolean evaluate(boolean big_step, double mag) {
            ++iterations;
            boolean result =
                big_step && mag > MIN_GRAD_MAG && iterations < max_iterations;
            if (result) {
                if (System.currentTimeMillis() >= endTime) {
                    System.out.println(
                        "WARNING: Time limit reached; normal convergence " +
                        "criteria not met!");
                    result = false;
                } else if (dsim.isInterrupted() ==
                           DigitalScheduler.InterruptedBy.USER) {
                    System.out.println("*** interrupted by ^C ***");
                    System.out.println("WARNING: normal convergence " +
                        "criteria not met!");
                    result = false;
                }
            }
            return result;
        }

        private Offset update(final int sense, final Node n) {
            if (!isFixed(n)) {
                final Node canon = (Node) n.enabler;
                setFixed(n);
                if (canon == null || canon == n) {
                    n.setTCount(fixed.size());
                    fixed.add(new Offset());
                    fixed.add(new Offset());
                } else {
                    update(sense, canon);
                    n.setTCount(canon.getTCount());
                }
            }

            return fixed.get(getIndex(n, sense));
        }

        private void update(final int sense, final Node n,
                            final boolean T1, final Double val) {
            Offset offset = update(sense, n);
            if (val != null) {
                offset.update(T1, val);
            }
        }

        // TODO: support wild card form of measured_delay directives, i.e.,
        // when the trigger is null
        public void writeContext(final PrintWriter out) {
            final Collection<MeasuredDelay> delays =
                dsim.getBoundaryMeasuredDelays();
            for (MeasuredDelay delay : delays) {
                if (delay.getInstance() == null) continue;

                out.println(delay.getCell().getFullyQualifiedType() + " " + 
                            HierName.append(delay.getPrefix(),
                                            delay.getInstance()) + " " +
                            delay.getDelayBias());

                final Map<Pair<Node,Boolean>,MeasuredDelay.Data> measured =
                    delay.getMeasured();
                final Map<Node,HierName> context = new HashMap<Node,HierName>();
                delay.getContext(context);
                for (Map.Entry<Pair<Node,Boolean>,MeasuredDelay.Data> entry :
                        measured.entrySet()) {
                    final Pair<Node,Boolean> key = entry.getKey();
                    final Node node = key.getFirst();
                    final int sense = key.getSecond() ? 1 : 0;
                    out.printf("%s %d %.3f %.3f\n",
                               delay.getPort(node), sense,
                               getVar(X, node, sense, T1, INPUT),
                               getVar(X, node, sense, T2, INPUT));

                    if (entry.getValue() != null) {
                        final MeasuredDelay.Data data = entry.getValue();
                        final double extraDelay = data.getExtraDelay();
                        final Pair[] value = data.getData();
                        for (Pair v : value) {
                            final Node trigger = (Node) v.getFirst();
                            final HierName tname = context.get(trigger);
                            if (trigger != null && tname != null) {
                                final QuadraticInterpolation[] qi =
                                    Rule.getInterpolation(
                                            value, trigger, node, sense, alpha,
                                            minDeriv, maxDeriv);
                                final double t1 =
                                    getVar(X, node, sense, T1, INPUT) -
                                    qi[0].value(defaultSlew) + extraDelay;
                                out.printf("\t%s %d %.3f %.3f\n",
                                           tname, 1 - sense,
                                           t1, t1 + defaultSlew);
                            }
                        }
                    }
                }
            }
            out.flush();
        }

        private void printNodeInfo(final Node node, final double[] X) {
            final Node canon = (Node) node.enabler;
            System.err.println(
                "node=" + node.getName() + " " +
                "canon=" + (canon == null ? null : canon.getName()) + " " +
                "fixed=" + isFixed(node) + " " +
                "rules=" + node.getBreakpoint() + " " +
                "index=" + node.getTCount() + " " +
                "in(t1-)=" + getVar(X, node, 0, T1, INPUT) + " " +
                "in(t2-)=" + getVar(X, node, 0, T2, INPUT) + " " +
                "out(t1-)=" + getVar(X, node, 0, T1, OUTPUT) + " " +
                "out(t2-)=" + getVar(X, node, 0, T2, OUTPUT) + " " +
                "in(t1+)=" + getVar(X, node, 1, T1, INPUT) + " " +
                "in(t2+)=" + getVar(X, node, 1, T2, INPUT) + " " +
                "out(t1+)=" + getVar(X, node, 1, T1, OUTPUT) + " " +
                "out(t2+)=" + getVar(X, node, 1, T2, OUTPUT));
        }

        private void printNodeInfo(final double[] X) {
            dsim.foreachNode(new UnaryPredicate<Node>() {
                public boolean evaluate(Node n) {
                    printNodeInfo(n, X);
                    return true;
                }
            });
        }

        private double getDouble(final DSim dsim, final String key,
                                 final double def) {
            return dsim.getDouble(key, def);
        }

        public CGSolver(final DSim dsim, final float defaultSlew, 
                        final Context worstContext, final int threads) {
            this.dsim = dsim;
            this.defaultSlew = defaultSlew;
            this.alpha = getDouble(dsim, "sta.alpha", alpha);
            this.minDeriv = getDouble(dsim, "sta.minDeriv", minDeriv);
            this.maxDeriv = getDouble(dsim, "sta.maxDeriv", maxDeriv);
            this.tolerance = getDouble(dsim, "sta.tolerance", tolerance);

            final int nodeCount[] = new int[] { 0 };

            // initialize various indicies
            dsim.foreachNode(new UnaryPredicate<Node>() {
                boolean hasRules;
                public boolean evaluate(Node n) {
                    hasRules = false;
                    n.foreachRule(new Node.RuleFunc() {
                        public void accept(Rule r, int sense, Node n) {
                            if (isErrorRule(r)) return;
                            if (r.falseConjuncts > 0) hasRules = true;
                            n.setTCount(UNINIT_INDEX);
                            setUnfixed(n);
                            r.target.setTCount(UNINIT_INDEX);
                            setUnfixed(r.target);
                            r.transitionCount = UNINIT_INDEX;
                        }
                    });
                    if (hasRules) nodeCount[0]++;
                    n.setBreakpoint(hasRules);
                    return true;
                }
            });

            if (worstContext != null) {
                final MultiMap<HierName,HierName> rules =
                    worstContext.getRules();
                for (HierName trigger : rules.keySet()) {
                    final Node triggerNode = worstContext.lookupNode(trigger);
                    final Set<Node> targets = new HashSet<Node>();
                    for (HierName target : rules.get(trigger)) {
                        final Node targetNode =
                            worstContext.lookupNode(target);
                        targets.add(targetNode);
                    }
                    triggerNode.foreachRule(new Node.RuleFunc() {
                        public void accept(Rule r, int sense, Node n) {
                        /*
                            if (targets.contains(r.target)) {
                                update(fixed[sense],
                                       worstContext.lookupNode(key.getFirst()),
                                       true,
                                       val.minT1);
                                update(fixed[sense],
                                       worstContext.lookupNode(key.getFirst()),
                                       true,
                                       val.maxT1);
                                update(fixed[sense],
                                       worstContext.lookupNode(key.getFirst()),
                                       false,
                                       val.minT2);
                                update(fixed[sense],
                                       worstContext.lookupNode(key.getFirst()),
                                       false,
                                       val.maxT2);
                            }
                            */
                        }
                    });
                }

                for (Map.Entry<Pair<HierName,Boolean>,Context.Data> entry :
                        worstContext.getData().entrySet()) {
                    final Pair<HierName,Boolean> key = entry.getKey();
                    final Context.Data val = entry.getValue();
                    final int sense = key.getSecond() == true ? 1 : 0;
                    update(sense,
                           worstContext.lookupNode(key.getFirst()), true,
                           val.minT1);
                    update(sense,
                           worstContext.lookupNode(key.getFirst()), true,
                           val.maxT1);
                    update(sense,
                           worstContext.lookupNode(key.getFirst()), false,
                           val.minT2);
                    update(sense,
                           worstContext.lookupNode(key.getFirst()), false,
                           val.maxT2);
                }
            }

            // compute the maximum slew rates for the nodes on the boundary of
            // blackboxed cells, using the default slew rates as the slew rate
            // of the predecessor
            final Collection<MeasuredDelay> delays =
                dsim.getBoundaryMeasuredDelays();
            for (MeasuredDelay delay : delays) {
                final Map<Pair<Node,Boolean>,MeasuredDelay.Data> measured =
                    delay.getMeasured();
                for (Map.Entry<Pair<Node,Boolean>,MeasuredDelay.Data> entry :
                        measured.entrySet()) {
                    final Pair<Node,Boolean> key = entry.getKey();
                    final Node node = key.getFirst();
                    final int sense = key.getSecond() ? 1 : 0;
                    if (entry.getValue() == null) {
                        update(0, node);
                        update(1, node);
                    } else {
                        final Pair[] prep = entry.getValue().getData();
                        for (Pair p : prep) {
                            final Node trigger = (Node) p.getFirst();
                            if (trigger != null) {
                                final QuadraticInterpolation[] qi =
                                    Rule.getInterpolation(
                                            prep, trigger, node, sense, alpha,
                                            minDeriv, maxDeriv);
                                final double slew =
                                    qi[1].value(defaultSlew) -
                                    qi[0].value(defaultSlew);
                                update(sense, node, false, slew);
                                update(1-sense, node);
                            }
                        }
                    }
                }
            }

            constraintCount = 0;
            dsim.foreachRule(new Node.RuleFunc() {
                private Map<Node,QuadraticInterpolation[]> map(final Rule r) {
                    final Map<Node,QuadraticInterpolation[]> ruleMap;
                    if (r.transitionCount == UNINIT_INDEX) {
                        r.transitionCount = interpols.size();
                        ruleMap =
                            new IdentityHashMap<Node,QuadraticInterpolation[]>();
                        interpols.add(ruleMap);
                    } else {
                        ruleMap = interpols.get(r.transitionCount);
                    }
                    return ruleMap;
                }
                public void accept(Rule r, int sense, Node n) {
                    if (isErrorRule(r)) return;
                    final Rule canonRule = r.getCanonRule();
                    if (canonRule != null) {
                        map(canonRule);
                        r.transitionCount = canonRule.transitionCount;
                    } else {
                        final Map<Node,QuadraticInterpolation[]> ruleMap =
                            map(r);
                        final QuadraticInterpolation[] qi =
                            r.getInterpolation(n, alpha, minDeriv, maxDeriv, 0);
                        if (qi != null && isFixed(r.target)) {
                            final double slew =
                                qi[1].value(defaultSlew) -
                                qi[0].value(defaultSlew);
                            update(r.dir, r.target, false, slew);
                        }
                        final Node canonNode = canon(n);
                        if (!ruleMap.containsKey(canonNode))
                            ruleMap.put(canonNode, qi);
                    }
                    addVar(n, sense);
                    addVar(r.target(), r.dir);
                    if (r.falseConjuncts > 0) constraintCount += 2;
                }
            });

            for (Node n : dsim.getInteriorOutputNodes()) {
                // in astamizer mode, set T2=defaultSlew when used in a input
                // context, and set T2=0 when used in an output context
                update(0, n, false, (double) defaultSlew);
                update(1, n, false, (double) defaultSlew);
                update(0, n, false, 0.0);
                update(1, n, false, 0.0);
            }

            // initialize any nodes without measured_delay data (such as inputs)
            // to have the default slew rate
            for (Offset offset : fixed) {
                offset.updateIfUnset(false, (double) defaultSlew);
                offset.updateIfUnset(true, 0.0);
            }

            this.T1 = 0;
            this.T2 = getSize();

            X = new double[getSize() * 2];
            for (int i = T2; i < X.length; ++i) {
                X[i] = defaultSlew;
            }
            System.out.printf("%d constraints %d variables\n", constraintCount,
                              X.length);
            dsim.interrupt(DigitalScheduler.InterruptedBy.NONE);
            if (threads > 0) {
                System.out.println("Using " + threads + " threads");
                final int maxPerThread = (nodeCount[0] + threads - 1) / threads;
                final int[] nextIndex = new int[threads];
                final Node[][] assignment = new Node[threads][];
                for (int i = 0; i < threads; ++i) {
                    nextIndex[i] = 0;
                    assignment[i] = new Node[maxPerThread];
                }
                dsim.foreachNode(new UnaryPredicate<Node>() {
                    int count = 0;
                    public boolean evaluate(Node n) {
                        if (n.getBreakpoint()) {
                            assignment[count][nextIndex[count]++] = n;
                            count++;
                            if (count == threads) count = 0;
                        }
                        return true;
                    }
                });
                energyWorkers =
                    new Workers<EnergyWorker>(EnergyWorker.class, threads,
                                              "Energy Worker", assignment);
                gradientWorkers =
                    new Workers<GradientWorker>(GradientWorker.class, threads,
                                                "Gradient Worker", assignment);
                energyWorkers.start();
                gradientWorkers.start();
            }
        }

        public void solve(final float tau, final int max_iterations,
                          final long maxTime) {
            this.tau = tau;
            this.max_iterations = max_iterations;
            this.endTime = maxTime <= 0 ? Long.MAX_VALUE
                                        : System.currentTimeMillis() + maxTime;
            this.iterations = 0;
            new ConjugateGradientSolver(this).solve(X, tolerance, this);
        }

        private MultiMap<HierName,Rule> rulesByPrefix = null;

        private void populateRulesByPrefix() {
            if (rulesByPrefix != null) return;

            rulesByPrefix = new MultiMap<HierName,Rule>();
            dsim.foreachRule(new Node.RuleFunc() {
                public void accept(Rule r, int sense, Node n) {
                    if (isErrorRule(r)) return;
                    rulesByPrefix.put(r.prefix, r);
                }
            });
        }

        public int swap(final HierName instance, final int which) {
            int count = 0;
            populateRulesByPrefix();
            final Collection<Rule> rules = rulesByPrefix.get(instance);
            for (Rule r : rules) {
                final Rule canon = r.getCanonRule();
                if (canon != null) r = canon;

                // already swapped?
                final Integer w = measureSelected.get(r);
                if (w != null && w.intValue() == which) continue;

                final Map<Node,QuadraticInterpolation[]> ruleMap =
                    interpols.get(r.transitionCount);
                boolean updated = false;
                for (Node n : ruleMap.keySet()) {
                    final QuadraticInterpolation[] qi =
                        r.getInterpolation(n, alpha, minDeriv, maxDeriv, which);
                    if (qi != null) {
                        ruleMap.put(n, qi);
                        updated = true;
                    }
                }
                if (updated) {
                    measureSelected.put(r, which);
                    ++count;
                }
            }

            return count;
        }
    }

    private CGSolver solver;
    private Context worstContext;

    public static class InvalidContextFormatException extends RuntimeException {
        public InvalidContextFormatException() { }
    }

    public static class Context {
        public static class Data {
            public final double minT1, minT2;
            public final double maxT1, maxT2;
            public Data(final double minT1, final double minT2,
                        final double maxT1, final double maxT2) {
                this.minT1 = minT1;
                this.minT2 = minT2;
                this.maxT1 = maxT1;
                this.maxT2 = maxT2;
            }
        }

        private final Map<Pair<HierName,Boolean>,Data> data;
        private final MultiMap<HierName,HierName> rules;
        private Map<HierName,Node> nodes;
        private final float delayBias;
        public Context(final float delayBias) {
            this(delayBias, new HashMap<Pair<HierName,Boolean>,Data>());
        }
        private Context(final float delayBias,
                        final Map<Pair<HierName,Boolean>,Data> data) {
            this.data = data;
            this.delayBias = delayBias;
            this.rules = new MultiMap<HierName,HierName>();
        }
        public void addData(HierName node, boolean dir, double T1, double T2) {
            data.put(new Pair<HierName,Boolean>(node, dir),
                     new Data(T1, T2, T1, T2));
        }
        public void addRule(final HierName target, final HierName trigger) {
            rules.put(trigger, target);
        }
        public float getDelayBias() {
            return delayBias;
        }
        private static Data combine(final Data d1, final Data d2) {
            if (d1 == null) return d2;
            else if (d2 == null) return d1;
            else {
                return new Data(Math.min(d1.minT1, d2.minT1),
                                Math.min(d1.minT2, d2.minT2),
                                Math.max(d1.maxT1, d2.maxT1),
                                Math.max(d1.maxT2, d2.maxT2));
            }
        }
        public static Context worstContext(final Context c1, final Context c2) {
            if (c1 == null) return c2;
            else if (c2 == null) return c1;
            else {
                assert c1.data.keySet().equals(c2.data.keySet());
                final float delayBias = Math.min(c1.getDelayBias(),
                                                 c2.getDelayBias());
                final Map<Pair<HierName,Boolean>,Data> data =
                    new HashMap<Pair<HierName,Boolean>,Data>();
                for (Map.Entry<Pair<HierName,Boolean>,Data> entry :
                        c1.data.entrySet()) {
                    final Pair<HierName,Boolean> key = entry.getKey();
                    data.put(key, combine(entry.getValue(), c2.data.get(key)));
                }
                return new Context(delayBias, data);
            }
        }
        public Collection<HierName> getNames() {
            final Collection<HierName> result = new HashSet<HierName>();
            for (Pair<HierName,Boolean> x : data.keySet()) {
                result.add(x.getFirst());
            }
            return result;
        }
        public void setNodes(final Map<HierName,Node> nodes) {
            this.nodes = nodes;
        }
        public Node lookupNode(final HierName name) {
            return nodes.get(name);
        }
        public Map<Pair<HierName,Boolean>,Data> getData() {
            return Collections.unmodifiableMap(data);
        }
        public MultiMap<HierName,HierName> getRules() {
            return rules;
        }
    }


    public RunStaticTiming() {
        solver = null;
        worstContext = null;
    }

    public static class EvaluateOption implements Cloneable {
        DSim dsim;
        float tau;
        float defaultSlew;
        int max_iterations;
        long maxTime;
        int threads;
        float sweep;
        public EvaluateOption(final DSim dsim,
                              final float tau,
                              final float defaultSlew,
                              final int max_iterations,
                              final long maxTime,
                              final int threads,
                              final float sweep) {
            this.dsim = dsim;
            this.tau = tau;
            this.defaultSlew = defaultSlew;
            this.max_iterations = max_iterations;
            this.maxTime = maxTime;
            this.threads = threads;
            this.sweep = sweep;
        }
        public EvaluateOption clone() {
            try {
                return (EvaluateOption) super.clone();
            } catch (CloneNotSupportedException e) {
                throw new AssertionError(e);
            }
        }
    }

    public void evaluate(final DSim dsim,
                         final float tau,
                         final float defaultSlew,
                         final int max_iterations,
                         final long maxTime,
                         final int threads) {
        if (solver == null) {
            solver = new CGSolver(dsim, defaultSlew, getWorstContext(),
                                  threads);
        } else {
            if (solver.defaultSlew != defaultSlew) {
                System.err.println("Can't change defaultSlew without " +
                                   "re-instantiating! Using old defaultSlew: " +
                                   solver.defaultSlew);
            }
        }
        dsim.interrupt(DigitalScheduler.InterruptedBy.NONE);
        solver.solve(tau, max_iterations, maxTime);
    }

    public void sweep(final DSim dsim,
                      final float tau,
                      final float defaultSlew,
                      final int max_iterations,
                      final long maxTime,
                      final int threads,
                      final float window) {
        evaluate(dsim, tau, defaultSlew, max_iterations, maxTime, threads);
        double low = tau;
        double high = solver.getWorstTau();
        while (high - low > window &&
               dsim.isInterrupted() == DigitalScheduler.InterruptedBy.NONE) {
            System.err.printf("Worst tau in (%.3f, %.3f)\n", low, high);
            float mid = (float) ((low + high) / 2);
            evaluate(dsim, mid, defaultSlew, max_iterations, maxTime, threads);
            double worst = solver.getWorstTau();
            if (worst - mid > window) {
                high = Math.min(high, worst);
                low = mid;
            } else {
                high = mid;
            }
        }
        System.err.printf("Worst tau in (%.3f, %.3f)\n", low, high);
    }

    public void evaluate(final EvaluateOption opt) {
        if (Float.isNaN(opt.sweep)) {
            evaluate(opt.dsim, opt.tau, opt.defaultSlew, opt.max_iterations,
                     opt.maxTime, opt.threads);
        } else {
            sweep(opt.dsim, opt.tau, opt.defaultSlew, opt.max_iterations,
                  opt.maxTime, opt.threads, opt.sweep);
        }
    }

    public int swap(final String instance, final int which) {
        return swap(toHier(instance), which);
    }

    public int swap(final HierName instance, final int which) {
        if (solver != null) {
            return solver.swap(instance, which);
        } else {
            return -1;
        }
    }

    public void getFlatReport(final double worstTau, final int max,
                              final boolean verbose,
                              final boolean canonRulesOnly,
                              final PrintWriter pw) {
        if (solver != null) {
            solver.printFlatViolations(worstTau, max, verbose, canonRulesOnly,
                                       pw);
            pw.flush();
        }
    }

    public void getHierReport(final double worstTau, final PrintWriter pw) {
        if (solver != null) {
            solver.printLocalViolations(worstTau, pw);
            pw.flush();
        }
    }

    private int trySwap(final HierName instance,
                        final Set<HierName> seen,
                        final DSim dsim,
                        final double tau,
                        final String context,
                        final boolean printAnyway,
                        final UnaryPredicate<String> replacePredicate,
                        final PrintWriter spec,
                        final PrintWriter pw) {
        int swaps = 0;
        if (seen.add(instance)) {
            final String newtype = dsim.getCandidateType(instance);
            if (replacePredicate.evaluate(newtype)) {
                final int swapped = swap(instance, 1);
                if (swapped > 0 || printAnyway) {
                    spec.printf("# tau=%.3f%s\n", tau, context);
                    spec.println(newtype + " " + instance);
                    pw.printf("Swapped %s (tau=%.3f rules=%d%s)\n",
                              instance, tau, swapped, context);
                    swaps = 1;
                }
            }
        }
        return swaps;
    }

    private void simpleSwap(final double worstTau,
                            final int maxReport,
                            final int maxSwaps,
                            final int maxOuterLoop,
                            final EvaluateOption opt,
                            final Set<HierName> seen,
                            final MutableInt loops,
                            final boolean routed,
                            final UnaryPredicate<String> replacePredicate,
                            final PrintWriter spec,
                            final PrintWriter pw) {
        while (loops.get() < maxOuterLoop) {
            loops.inc();
            pw.println("Swap loop " + loops.get() + " tau=" + worstTau);
            final MultiMap<Double,HierName> byPrefix =
                solver.getViolationByPrefix(worstTau, true);
            final Set<Double> keySet = byPrefix.keySet();
            if (keySet.isEmpty()) {
                pw.println("No more violations; exiting");
                break;
            }

            int swaps = 0;
FixViolations:
            for (Double key : keySet) {
                for (HierName instance : byPrefix.get(key)) {
                    final int swapped =
                        trySwap(instance, seen, opt.dsim, key, "", false,
                                replacePredicate, spec, pw);
                    swaps += swapped;
                    if (swapped > 0) {
                        for (HierName otherInst :
                                new IterableIterator<HierName>(
                                    opt.dsim.getGrayboxInstances(
                                        instance.tail()))) {
                            swaps += trySwap(otherInst, seen, opt.dsim, key,
                                             " graybox", true,
                                             replacePredicate, spec, pw);
                        }

                        if (routed) {
                            for (HierName otherInst :
                                    new IterableIterator<HierName>(
                                        opt.dsim.getRoutedInstances(
                                            instance.tail()))) {
                                swaps += trySwap(otherInst, seen, opt.dsim, key,
                                                 " routed", false,
                                                 replacePredicate, spec, pw);
                            }
                        }
                    }
                    if (swaps >= maxSwaps) break FixViolations;
                }
            }

            if (swaps == 0) {
                pw.println("All instances swapped; exiting");
                break;
            }

            evaluate(opt);
            spec.flush();
            pw.flush();
        }
    }

    public void simpleSwap(final double goalTau,
                           final int maxReport,
                           final int maxSwaps,
                           final int maxOuterLoop,
                           final EvaluateOption model,
                           final boolean routed,
                           final UnaryPredicate<String> replacePredicate,
                           final PrintWriter spec,
                           final PrintWriter pw) {
        final Set<HierName> seen = new HashSet<HierName>();
        final MutableInt loops = new MutableInt(0);

        // find the real worst tau
        final EvaluateOption optSweep = model.clone();
        if (Float.isNaN(optSweep.sweep)) {
            optSweep.sweep = 0.001f;
            optSweep.max_iterations = 1000;
        }
        evaluate(optSweep);
        double tau = solver.getWorstTau();

        // (evaluate, swap) loop toward the goal tau 1 pS at a time
        while (tau - goalTau > 0.001) {
            tau = Math.max(tau - 1, goalTau);
            final EvaluateOption opt = model.clone();
            opt.sweep = Float.NaN;
            opt.tau = (float) tau;
            simpleSwap(tau, maxReport, maxSwaps, maxOuterLoop, opt,
                       seen, loops, routed, replacePredicate, spec, pw);
            getFlatReport(tau, maxReport, false, true, pw);
        }
        pw.flush();
    }

    public void writeContext(final PrintWriter pw) {
        if (solver != null) {
            solver.writeContext(pw);
            pw.flush();
        }
    }

    public void readContext(final BufferedReader br) throws IOException {
        String line;
        Context context = null;
        HierName target = null;
        while ((line = br.readLine()) != null) {
            if (line.startsWith("#")) continue;

            final String[] fields = StringUtil.split(line, ' ');
            if (fields.length == 3) {
                if (context != null) {
                    worstContext = Context.worstContext(worstContext, context);
                }
                context = new Context(Float.parseFloat(fields[2]));
            } else if (fields.length == 4) {
                boolean isTarget = true;
                if (fields[0].startsWith("\t")) {
                    fields[0] = fields[0].substring(1);
                    isTarget = false;
                }
                final HierName node = toHier(fields[0]);
                final boolean dir = Integer.parseInt(fields[1]) == 1;
                final double T1 = Double.parseDouble(fields[2]);
                final double T2 = Double.parseDouble(fields[3]);
                context.addData(node, dir, T1, T2);
                if (isTarget) target = node;
                else if (target != null) {
                    context.addRule(target, node);
                }
            } else {
                throw new InvalidContextFormatException();
            }
        }
        if (context != null) {
            worstContext = Context.worstContext(worstContext, context);
        }
    }

    public Context getWorstContext() {
        return worstContext;
    }
}
