package com.avlsi.tools.jauto;

import java.io.IOException;
import java.io.PrintStream;
import java.io.BufferedReader;
import java.io.FileReader;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.LinkedList;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

import com.avlsi.fast.CellNet;
import com.avlsi.fast.CellType;
import com.avlsi.fast.ConnectionInfo;
import com.avlsi.fast.HalfOperator;
import com.avlsi.file.common.HierName;
import com.avlsi.util.container.MultiMap;
import com.avlsi.util.container.Pair;

public class NegativeDelayBudget implements ConjugateGradientSolver.Function {
    private static final class InstanceIterator<E> implements Iterator<E>{
        private final HierName instName;
        private final Iterator<E> iterator;
        public InstanceIterator(final Iterator<E> iterator,
                                final HierName instName) {
            this.iterator = iterator;
            this.instName = instName;
        }
        public HierName getInstance() {
            return instName;
        }
        public boolean hasNext() {
            return iterator.hasNext();
        }
        public E next() {
            return iterator.next();
        }
        public void remove() {
            throw new UnsupportedOperationException();
        }
    }

    private static final class NetIterator
    implements Iterator<InstanceIterator<GlobalNet>> {
        private final LinkedList<InstanceIterator<ConnectionInfo>> subcells;
        private InstanceIterator<GlobalNet> nets;
        private final MultiMap globalNetMap;
        public NetIterator(final HierName instName,
                           final CellType cell,
                           MultiMap globalNetMap) {
            subcells = new LinkedList<InstanceIterator<ConnectionInfo>>();
            subcells.addLast(
                new InstanceIterator<ConnectionInfo>(
                    cell.getAllSubcellConnections().iterator(),
                    instName));
            this.globalNetMap = globalNetMap;

            Iterator<GlobalNet> gn = (Iterator<GlobalNet>)
                globalNetMap.get(cell.typeName).iterator();
            if (gn.hasNext()) {
                nets = new InstanceIterator<GlobalNet>(gn, null);
            } else {
                nets = null;
            }
        }
        private void skipToQualified() {
            while (nets == null && !subcells.isEmpty()) {
                InstanceIterator<ConnectionInfo> ii = subcells.getLast();
                if (ii.hasNext()) {
                    ConnectionInfo info = ii.next();
                    Iterator<ConnectionInfo> subci =
                        info.child.getAllSubcellConnections().iterator();

                    HierName childName =
                        HierName.append(ii.getInstance(), info.nameInParent);
                    subcells.addLast(
                        new InstanceIterator<ConnectionInfo>(subci, childName));

                    Iterator<GlobalNet> gn = (Iterator<GlobalNet>)
                        globalNetMap.get(info.child.typeName).iterator();
                    if (gn.hasNext()) {
                        nets =
                            new InstanceIterator<GlobalNet>(gn, childName);
                    }
                } else {
                    subcells.removeLast();
                }
            }
        }

        public boolean hasNext() {
            skipToQualified();
            return nets != null;
        }

        public InstanceIterator<GlobalNet> next() {
            if (hasNext()) {
                final InstanceIterator<GlobalNet> result = nets;
                nets = null;
                return result;
            } else {
                throw new NoSuchElementException();
            }
        }

        public void remove() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * Compare GlobalNet objects by their top-level canonical net names.
     **/
    private static class GlobalNetComparator implements Comparator<Object> {
        private HierName toHierName(Object o) {
            if (o instanceof HierName) {
                return (HierName) o;
            } else {
                return ((GlobalNet) o).getTopCellNet().canonicalName;
            }
        }
        public int compare(Object o1, Object o2) {
            return toHierName(o1).compareTo(toHierName(o2));
        }
        public boolean equals(Object o) {
            return o instanceof GlobalNetComparator;
        }
    }

    private static Comparator<Object> GLOBALNET_COMPARATOR =
        new GlobalNetComparator();

    private interface TransitionProcessor {
        void processTransition(HierName src, HierName snk, double delay,
                               int dir);
    }

    private class PrintTransitions implements TransitionProcessor {
        public void processTransition(final HierName src, final HierName snk,
                                      final double delay, final int dir) {
            int odir = getOpposingDir(dir);
            System.err.println("#" + String.format("%.2f", delay) +
                               " " + src + getDirString(odir) +
                               " -> " + snk + getDirString(dir));
            System.err.println("index(" + src + getDirString(odir) +
                               ") = " + getGlobalNetIndex(top, src, odir));
            System.err.println("index(" + snk + getDirString(dir) +
                               ") = " + getGlobalNetIndex(top, snk, dir));
        }
    }

    private class CountTransitions implements TransitionProcessor {
        private int count;
        private int maxvar;
        public CountTransitions() {
            count = 0;
            maxvar = -1;
        }
        public void processTransition(final HierName src, final HierName snk,
                                      final double delay, final int dir) {
            maxvar = Math.max(maxvar, getGlobalNetIndex(top, src));
            maxvar = Math.max(maxvar, getGlobalNetIndex(top, snk));
            count++;
        }
        public int getCount() {
            return count;
        }
        public int getMaxVarIndex() {
            return maxvar;
        }
    }

    private class SetNames extends IndexMapper implements TransitionProcessor {
        private final String[] names;
        public SetNames() {
            names = new String[transitionCount * 2 + variableCount];
        }
        public void processTransition(final HierName src, final HierName snk,
                                      final double delay, final int dir) {
            int odir = getOpposingDir(dir);
            final String srcstr = src + getDirString(odir);
            final String dststr = snk + getDirString(dir);
            final String transition = srcstr + " -> " + dststr;
            names[getDelayIndex()] = "d(" + transition + ")";
            names[getNegativeBudgetIndex()] = "n(" + transition + ")";
            int srcidx = getGlobalNetIndex(top, src, odir);
            int dstidx = getGlobalNetIndex(top, snk, dir);
            names[getArrivalTimeIndex(srcidx)] = "t(" + src.toString() +
                                                 getDirString(odir) + ")";
            names[getArrivalTimeIndex(dstidx)] = "t(" + snk.toString() +
                                                 getDirString(dir) + ")";
            count++;
        }
        public void printValue(final double[] x) {
            for (int i = 0; i < names.length; ++i) {
                if (names[i] != null)
                    System.err.println(String.format("%d %s %g", i, names[i], x[i]));
            }
        }
        public String[] getNames() {
            return names;
        }
    }

    private class IndexMapper {
        protected int count;
        // delay(i,j) => v[0..transitionCount-1]
        // negative(i,j) => v[transitionCount..2*transitionCount-1]
        // t(i) => v[2*transitionCount,2*transitionCount+variableCount-1]
        public final int getDelayIndex() {
            return count;
        }
        public final int getNegativeBudgetIndex() {
            return count + transitionCount;
        }
        public final int getArrivalTimeIndex(int idx) {
            return 2 * transitionCount + idx;
        }
    }

    private class IndexedAccess extends IndexMapper {
        private final double[] v;
        protected final double[] accum;
        public IndexedAccess(final double[] v, final double[] accum) {
            this.v = v;
            this.accum = accum;
        }
        protected double getDelay() {
            return v[getDelayIndex()];
        }
        protected void accumDelay(double x) {
            accum[getDelayIndex()] += x;
        }
        protected double getNegativeBudget() {
            return v[getNegativeBudgetIndex()];
        }
        protected void accumNegativeBudget(double x) {
            accum[getNegativeBudgetIndex()] += x;
        }
        protected double getArrivalTime(int idx) {
            return v[getArrivalTimeIndex(idx)];
        }
        protected void accumArrivalTime(int idx, double x) {
            accum[getArrivalTimeIndex(idx)] += x;
        }
    }

    private final class EnergyCalculator extends IndexedAccess
                                         implements TransitionProcessor {
        private double constraintEnergy, objectiveEnergy;
        private double scaleConstraint;
        private double scaleObjective;
        private boolean verbose;
        private double E_d, E_n, E_t, E_tn, OE_n, OE_d;
        private boolean solveForP;
        public EnergyCalculator(double[] v, double scaleConstraint,
                                double scaleObjective,
                                boolean solveForP,
                                boolean verbose) {
            super(v, null);
            this.scaleConstraint = scaleConstraint;
            this.scaleObjective = scaleObjective;
            this.constraintEnergy = 0;
            this.objectiveEnergy = 0;
            E_d = E_n = E_t = E_tn = 0;
            OE_n = OE_d = 0;

            // t(j) >= 0
            for (int i = 0; i < variableCount; ++i) {
                if (getArrivalTime(i) < 0) {
                    E_t += square(getArrivalTime(i));
                }
            }

            this.solveForP = solveForP;
            this.verbose = verbose;
        }
        private double square(final double x) {
            return x * x;
        }
        private double cube(final double x) {
            return x * x * x;
        }
        public void processTransition(final HierName src, final HierName snk,
                                      double delay, final int dir) {
            // d(i,j) >= D(j)
            if (getDelay() < delay) {
                E_d += square(delay - getDelay());
            }

            // n(i,j) >= 0
            if (getNegativeBudget() < 0) {
                E_n += square(getNegativeBudget());
            }

            // t(j) = t(i) + d(i,j) - n(i,j)
            int odir = getOpposingDir(dir);
            int srcidx = getGlobalNetIndex(top, src, odir);
            int dstidx = getGlobalNetIndex(top, snk, dir);
            E_tn += square(getArrivalTime(srcidx) +
                           getDelay() -
                           getNegativeBudget() -
                           getArrivalTime(dstidx));

            // obj = sum(n(i,j)^2) + sum(d(i,j)^2)
            if (solveForP) {
                OE_d += square(getDelay() - delay);
            } else {
                OE_n += square(getNegativeBudget());
            }

            count++;
        }
        public double getEnergy() {
            constraintEnergy = E_d / variableCount +
                               (E_n + E_t + E_tn) / transitionCount;
            objectiveEnergy = (OE_n + OE_d) / transitionCount;
            if (verbose) {
                System.err.printf("E_d = %g (%g) E_n = %g (%g) " +
                                  "E_t = %g (%g) E_tn = %g (%g)\n",
                                  E_d, E_d * scaleConstraint,
                                  E_n, E_n * scaleConstraint,
                                  E_t, E_t * scaleConstraint,
                                  E_tn, E_tn * scaleConstraint);
                System.err.printf("constraintEnergy = %g (%g)\n",
                                  constraintEnergy,
                                  constraintEnergy * scaleConstraint);
                System.err.printf("OE_n = %g (%g) OE_d = %g (%g)\n",
                                  OE_n, OE_n * scaleObjective,
                                  OE_d, OE_d * scaleObjective);
                System.err.printf("objectiveEnergy = %g (%g)\n",
                                  objectiveEnergy,
                                  objectiveEnergy * scaleObjective);
            }
            return constraintEnergy * scaleConstraint +
                   objectiveEnergy * scaleObjective;
        }
    }

    private final class GradientCalculator extends IndexedAccess
                                           implements TransitionProcessor {
        private double scaleConstraint;
        private double scaleObjective;
        private boolean solveForP;
        public GradientCalculator(double[] v, double[] accum,
                                  double scaleConstraint,
                                  double scaleObjective,
                                  boolean solveForP) {
            super(v, accum);
            this.scaleConstraint = scaleConstraint;
            this.scaleObjective = scaleObjective;
            this.solveForP = solveForP;

            // t(j) >= 0
            for (int i = 0; i < variableCount; ++i) {
                if (getArrivalTime(i) < 0) {
                    accumArrivalTime(i, -2 * getArrivalTime(i)
                                           * scaleConstraint
                                           / variableCount);
                }
            }
        }

        public void processTransition(final HierName src, final HierName snk,
                                      double delay, final int dir) {
            // d(i,j) >= D(j)
            if (getDelay() < delay) {
                accumDelay(2 * (delay - getDelay()) * scaleConstraint
                             / transitionCount);
            }

            // n(i,j) >= 0
            if (getNegativeBudget() < 0) {
                accumNegativeBudget(-2 * getNegativeBudget() * scaleConstraint
                                       / transitionCount);
            }

            // t(j) = t(i) + d(i,j) - n(i,j)
            int odir = getOpposingDir(dir);
            int srcidx = getGlobalNetIndex(top, src, odir);
            int dstidx = getGlobalNetIndex(top, snk, dir);
            double x = 2 * (getArrivalTime(srcidx) + getDelay() -
                            getNegativeBudget() - getArrivalTime(dstidx))
                         / transitionCount;
            accumArrivalTime(srcidx, -x * scaleConstraint);
            accumArrivalTime(dstidx, x * scaleConstraint);
            accumDelay(-x * scaleConstraint);
            accumNegativeBudget(x * scaleConstraint);

            // obj = sum(n(i,j)^2) + sum(d(i,j)^2)
            if (solveForP) {
                accumDelay(-2 * (getDelay() - delay) * scaleObjective
                              / transitionCount);
            } else {
                accumNegativeBudget(-2 * getNegativeBudget() * scaleObjective
                                        / transitionCount);
            }

            if (solveForP) {
                accum[getNegativeBudgetIndex()] = 0;
            }
            count++;
        }
    }

    private final class InitializeVars extends IndexedAccess
                                       implements TransitionProcessor {
        public InitializeVars(double[] accum) {
            super(null, accum);
            for (int i = 0; i < accum.length; ++i) {
                accum[i] = 0;
            }
        }
        public void processTransition(final HierName src, final HierName snk,
                                      double delay, final int dir) {
            accumDelay(delay);
            count++;
        }
    }

    private final class PrintConstraints extends IndexedAccess
                                         implements TransitionProcessor {
        public PrintConstraints(double[] v) {
            super(v, null);

            // t(j) >= 0
            for (int i = 0; i < variableCount; ++i) {
                System.err.printf("%s >= 0: %g >= 0\n",
                                  names[getArrivalTimeIndex(i)],
                                  getArrivalTime(i));
            }
        }
        private double square(final double x) {
            return x * x;
        }
        public void processTransition(final HierName src, final HierName snk,
                                      double delay, final int dir) {
            // d(i,j) >= D(j)
            System.err.printf("%s >= %g: %g >= %g\n",
                              names[getDelayIndex()], delay,
                              getDelay(), delay);

            // n(i,j) >= 0
            System.err.printf("%s >= 0: %g >= 0\n",
                              names[getNegativeBudgetIndex()],
                              getNegativeBudget());

            // t(j) = t(i) + d(i,j) - n(i,j)
            int odir = getOpposingDir(dir);
            int srcidx = getGlobalNetIndex(top, src, odir);
            int dstidx = getGlobalNetIndex(top, snk, dir);
            String srcstr = names[getArrivalTimeIndex(srcidx)];
            String dststr = names[getArrivalTimeIndex(dstidx)];
            System.err.printf("%s = %s + %s - %s: %g = %g + %g - %g\n",
                              dststr, srcstr, names[getDelayIndex()],
                              names[getNegativeBudgetIndex()],
                              getArrivalTime(dstidx),
                              getArrivalTime(srcidx),
                              getDelay(),
                              getNegativeBudget());

            // obj = sum(n(i,j)^2) + sum(d(i,j)^2)
            System.err.printf("objective = %s^2 + %s^2: %g + %g\n",
                              names[getNegativeBudgetIndex()],
                              names[getDelayIndex()],
                              square(getNegativeBudget()),
                              square(getDelay()));

            count++;
        }
    }

    private class PrintResults extends IndexedAccess
                               implements TransitionProcessor {
        public PrintResults(final double[] v) {
            super(v, null);
        }

        public void processTransition(final HierName src, final HierName snk,
                                      final double delay, final int dir) {
            int odir = getOpposingDir(dir);
            int srcidx = getGlobalNetIndex(top, src, odir);
            int dstidx = getGlobalNetIndex(top, snk, dir);
            System.err.println("|" + String.format("%.2f", delay) +
                               " " + src + getOpposingDirString(dir) +
                               " -> " + snk + getDirString(dir) +
                               " tsrc = " + getArrivalTime(srcidx) +
                               " tsnk = " + getArrivalTime(dstidx) +
                               " n = " + getNegativeBudget() +
                               " d = " + getDelay() +
                               " p = " + (getDelay() - delay));
            count++;
        }
    }

    private class GenerateLP extends IndexMapper
                             implements TransitionProcessor {
        private final PrintStream out;
        public GenerateLP(PrintStream out) {
            this.out = out;
            for (int i = 0; i < variableCount; ++i) {
                out.printf("var x%d >= 0;\n", getArrivalTimeIndex(i));
            }
        }
        public void processTransition(final HierName src, final HierName snk,
                                      final double delay, final int dir) {
            out.printf("var x%d >= %g;\n", getDelayIndex(), delay);
            out.printf("var x%d >= 0;\n", getNegativeBudgetIndex());

            int odir = getOpposingDir(dir);
            int srcidx = getGlobalNetIndex(top, src, odir);
            int dstidx = getGlobalNetIndex(top, snk, dir);
            out.printf("subject to c%d: x%d + x%d - x%d - x%d = 0;\n",
                       count, getArrivalTimeIndex(srcidx), getDelayIndex(),
                       getNegativeBudgetIndex(),
                       getArrivalTimeIndex(dstidx));
            count++;
        }
        public void finishOutput() {
            out.print("minimize obj: ");
            for (int i = 0; i < 2*transitionCount; ++i) {
                if (i > 0) {
                    out.print(" + ");
                }
                out.print("x" + i);
            }
            out.println(";");
            out.println("end;");
        }
    }

    private static final Pattern SOLUTION_PATTERN =
        Pattern.compile("^\\s*\\d+ x(\\d+)\\s+\\S+\\s+(\\S+)\\s+\\S+.*");

    private void solveLP(double[] X) throws IOException {
        final PrintStream out = new PrintStream("solve.in");
        final GenerateLP lpWriter = new GenerateLP(out);
        processTransitions(lpWriter);
        lpWriter.finishOutput();
        out.close();

        final ProcessBuilder pb = new ProcessBuilder("sh", "-c", "glpsol --math solve.in --output solve.out >solve.log 2>&1");
        try {
            pb.start().waitFor();
        } catch (InterruptedException e) {
            throw new RuntimeException("Exception running glpsol", e);
        }

        final BufferedReader br =
            new BufferedReader(new FileReader("solve.out"));

        String line;
        while ((line = br.readLine()) != null) {
            final Matcher m = SOLUTION_PATTERN.matcher(line);
            if (m.matches()) {
                final int var = Integer.parseInt(m.group(1));
                final double val = Double.parseDouble(m.group(2));
                X[var] = val;
            }
        }
        br.close();
    }

    public double getValue(double[] v) {
        return getValue(v, false);
    }

    public double getValue(double[] v, boolean verbose) {
        final EnergyCalculator calc =
            new EnergyCalculator(v, scaleConstraint, scaleObjective, solveForP, verbose);
        processTransitions(calc);
        return calc.getEnergy();
    }

    public void getNegativeGradient(double[] v, double[] accum) {
        getAnalyticGradient(v, accum);
    }

    private void checkGradient(double[] v, double[] accum) {
        final double[] nAccum = new double[accum.length];
        System.arraycopy(accum, 0, nAccum, 0, accum.length);
        getNumericalGradient(v, nAccum);

        final double[] aAccum = new double[accum.length];
        System.arraycopy(accum, 0, aAccum, 0, accum.length);
        getAnalyticGradient(v, aAccum);

        for (int i = 0; i < accum.length; i++) {
            if (Math.abs(aAccum[i] - nAccum[i]) / nAccum[i] > 0.01) {
                System.err.printf("Gradient: %d %g %g %g\n", i, nAccum[i], aAccum[i], nAccum[i] - aAccum[i]);
            }
        }
    }

    private void getAnalyticGradient(double[] v, double[] accum) {
        final GradientCalculator calc =
            new GradientCalculator(v, accum, scaleConstraint, scaleObjective, solveForP);
        processTransitions(calc);
    }

    private void getNumericalGradient(double[] v, double[] accum) {
        final double delta = 1e-10;
        for (int i = 0; i < v.length; ++i) {
            final double oldv = v[i];
            v[i] = oldv - delta;
            final double oldE = getValue(v);
            v[i] = oldv + delta;
            final double newE = getValue(v);
            v[i] = oldv;
            final double grad = (newE - oldE) / (2 * delta);
            accum[i] += -grad;
        }
    }

    /**
     * A MultiMap adapted to return an empty list when the key is not in the
     * list.
     **/
    private static class MultiMapWithDefault extends MultiMap {
        public MultiMapWithDefault(final Map map) {
            super(map, MultiMap.ARRAY_LIST_FACTORY);
        }

        public Collection get(Object key) {
            Collection result = super.get(key);
            return result == null ? Collections.EMPTY_LIST : result;
        }
    }

    private final MultiMap c2gmap;
    private final CellType top;
    private final double tau;
    private final int transitionCount;
    private final int globalNetCount;
    private final int variableCount;
    private String[] names;
    private double scaleConstraint, scaleObjective;
    private boolean solveForP;

    private void processTransitions(final TransitionProcessor proc) {
        for (NetIterator i = new NetIterator(null, top, c2gmap);
             i.hasNext(); ) {
            final InstanceIterator<GlobalNet> gni = i.next();
            final HierName inst = gni.getInstance();
            while (gni.hasNext()) {
                final GlobalNet gn = gni.next();
                processTransitions(inst, gn, proc);
            }
        }
    }

    private static class FixedIterations
    implements ConjugateGradientSolver.StopCondition {
        private int count;
        public FixedIterations(int count) {
            this.count = count;
        }
        public boolean evaluate(boolean big_step, double mag) {
            count--;
            return count > 0;
        }
    }

    public NegativeDelayBudget(final CellType top,
                               final List<GlobalNet> globalNets,
                               final double tau) throws IOException {
        this.top = top;
        this.tau = tau;
        c2gmap = new MultiMapWithDefault(new HashMap());
        for (GlobalNet gn : globalNets) {
            final CellNet cn = gn.getTopCellNet();
            c2gmap.put(cn.container.typeName, gn);
        }
        for (String key : (Set<String>) c2gmap.keySet()) {
            Collections.sort((List<GlobalNet>) c2gmap.get(key),
                             GLOBALNET_COMPARATOR);
        }
        getNumVars(top, new HashMap<String,Integer>());

        final CountTransitions counter = new CountTransitions();
        processTransitions(counter);
        transitionCount = counter.getCount();
        globalNetCount = counter.getMaxVarIndex() + 1;
        variableCount = globalNetCount * 2;
        processTransitions(new PrintTransitions());
        System.err.println("Transition count = " + transitionCount +
                           " variable count = " + variableCount);

        final SetNames setNames = new SetNames();
        processTransitions(setNames);
        names = setNames.getNames();

        double[] X = new double[transitionCount * 2 + variableCount];
        solveLP(X);
        setNames.printValue(X);
        processTransitions(new PrintResults(X));

        processTransitions(new InitializeVars(X));
        double tolerance = 0;

        solveForP = false;
        for (int z = 0; z < 2; ++z) {
            scaleObjective = 1e5;
            scaleConstraint = 1e-4;
            getValue(X, true);

            final ConjugateGradientSolver solver =
                new ConjugateGradientSolver(
                    new ConjugateGradientSolver.Parameters() {
                        public double getTolerance() { return 1e-8; }
                    },
                    this);

            for (int i = 1; i < 15; ++i) {
                solver.conjugateGradientDescent(X,tolerance,
                                                new FixedIterations(500));
                System.err.println("Iteration " + i);
                getValue(X, true);
                setNames.printValue(X);
                processTransitions(new PrintConstraints(X));
                scaleConstraint *= 10;
            }
            solveForP = !solveForP;
        }

        processTransitions(new PrintResults(X));
    }

    private HierName append(HierName a, HierName b) {
        return b == null ? a : HierName.append(a, b);
    }

    private Pair<ConnectionInfo,HierName> findInstance(CellType cell,
                                                       HierName inst,
                                                       HierName rest) {
        if (inst == null) {
            return null;
        } else {
            final ConnectionInfo ci = cell.getSubcellNamed(inst);
            if (ci == null) {
                final HierName suffix =
                    HierName.makeHierName(inst.getSuffixString());
                rest = rest == null ? suffix : HierName.append(suffix, rest);
                return findInstance(cell, inst.getParent(), rest);
            } else {
                return new Pair<ConnectionInfo,HierName>(ci, rest);
            }
        }
    }

    private Pair<ConnectionInfo,HierName> findInstance(CellType cell,
                                                       HierName inst) {
        return findInstance(cell, inst, null);
    }

    private List<ConnectionInfo> findPath(final CellType cell,
                                          final HierName inst) {
        final List<ConnectionInfo> results = new ArrayList<ConnectionInfo>();
        Pair<ConnectionInfo, HierName> ch = findInstance(top, inst);
        while (ch != null) {
            ConnectionInfo ci = ch.getFirst();
            results.add(ci);
            ch = findInstance(ci.child, ch.getSecond());
        }
        return results;
    }

    private HierName isSiblingSubcell(final CellType top,
                                      final HierName inst,
                                      final GlobalNet net) {
        final CellType container = net.getTopCellNet().container;
        HierName instance = null;
        if (top != container) {
            List<ConnectionInfo> path = findPath(top, inst);
            for (ConnectionInfo ci : path) {
                instance = HierName.append(instance, ci.nameInParent);
                if (ci.child == container) {
                    break;
                }
            }
        }
        return instance;
    }

    private GlobalNet findSource(final HierName inst, final NetSink sink) {
        final HierName fullInst = append(inst, sink.getInstanceName());
        final HalfOperator halfOp = sink.getSink();
        for (GlobalNet net : halfOp.outputNet.globalNets) {
            final Collection<NetSource> sources =
                (Collection<NetSource>) net.getListSources();
            for (NetSource source : sources) {
                if (source.getSource() == halfOp) {
                    final HierName srcInst = source.getInstanceName();
                    if (srcInst == null || fullInst.endsWith(srcInst)) {
                        return net;
                    }
                }
            }
        }
        return null;
    }

    private int getNumVars(final CellType cell,
                           final Map<String,Integer> cache) {
        final Integer cached = cache.get(cell.typeName);
        int count;
        if (cached == null) {
            count = 0;
            Collection<GlobalNet> globalNets =
                (Collection<GlobalNet>) c2gmap.get(cell.typeName);
            count += globalNets.size();
            for (ConnectionInfo ci : cell.getAllSubcellConnections()) {
                int subcount = getNumVars(ci.child, cache);
                ci.setIndex(count);
                count += subcount;
            }
            cache.put(cell.typeName, count);
        } else {
            count = cached;
        }

        return count;
    }

    private int findNetPosition(final CellType cell, final HierName net) {
        final List<GlobalNet> nets =
            (List<GlobalNet>) c2gmap.get(cell.typeName);
        return Collections.binarySearch(nets, net, GLOBALNET_COMPARATOR);
    }

    private int getGlobalNetIndex(CellType cell, HierName inst, int dir) {
        int index = getGlobalNetIndex(cell, inst);
        if (dir == HalfOperator.DriveDirection.PULL_UP) index += globalNetCount;
        return index;
    }

    private int getGlobalNetIndex(CellType cell, HierName inst) {
        int index = 0;
        int pos = -1;
        Pair<ConnectionInfo, HierName> ch;
        do {
            pos = findNetPosition(cell, inst);
            if (pos >= 0) break;
            ch = findInstance(cell, inst);
            if (ch != null) {
                ConnectionInfo ci = ch.getFirst();
                index += ci.getIndex();
                inst = ch.getSecond();
                cell = ci.child;
            }
        } while (ch != null);
        if (pos < 0) {
            pos = findNetPosition(cell, inst);
        }
        if (pos < 0) {
            System.err.println("Cell = " + cell.typeName + " Rest = " + inst);
        }

        index += pos;
        
        return index;
    }

    private String getDirString(int dir) {
        switch (dir) {
          case HalfOperator.DriveDirection.PULL_DOWN: return "-";
          case HalfOperator.DriveDirection.PULL_UP:   return "+";
          default:                                    return "=";
        }
    }

    private String getOpposingDirString(int dir) {
        switch (dir) {
          case HalfOperator.DriveDirection.PULL_DOWN: return "+";
          case HalfOperator.DriveDirection.PULL_UP:   return "-";
          default:                                    return "=";
        }
    }

    private int getOpposingDir(int dir) {
        switch (dir) {
          case HalfOperator.DriveDirection.PULL_DOWN:
            return HalfOperator.DriveDirection.PULL_UP;
          case HalfOperator.DriveDirection.PULL_UP:
            return HalfOperator.DriveDirection.PULL_DOWN;
          default:
            return dir;
        }
    }

    private double getDelay(final NetSink snk, final GlobalNet gn) {
        final HalfOperator op = snk.getSink();
        final double delay =
            (op.getDelayBias() * tau *
             TransistorSizingTool.findDelayBias(gn, op) +
             DelayCalculator.getExtraDelay(op, gn, tau)) / tau;
        return delay;
    }

    private void processTransitions(final HierName inst, final GlobalNet gn,
                                    final TransitionProcessor proc) {
        for (Iterator j = gn.getListSinks().iterator(); j.hasNext(); ) {
            final NetSink snk = (NetSink) j.next();
            final HierName fullInst = append(inst, snk.getInstanceName());
            if (snk.getType() == NetType.HALF_OPERATOR_TRANSISTOR) {
                CellNet output = snk.getSink().outputNet;
                for (Iterator k = output.globalNets.iterator(); k.hasNext(); ) {
                    CellNet top = ((GlobalNet) k.next()).getTopCellNet();
                }
                final GlobalNet other = findSource(inst, snk);
                final HierName hsrc =
                    append(inst, gn.getTopCellNet().canonicalName);
                final HierName hsnk =
                    append(isSiblingSubcell(top, fullInst, other),
                           other.getTopCellNet().canonicalName);
                proc.processTransition(hsrc, hsnk, getDelay(snk, other),
                                       snk.getSink().driveDirection);
            }
        }
    }
}
