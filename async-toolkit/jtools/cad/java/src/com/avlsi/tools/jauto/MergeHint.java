/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.jauto;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeSet;
import java.util.TreeMap;

import com.avlsi.cast.CastCacheManager;
import com.avlsi.cast.CastFileParser;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.directive.impl.DirectiveComparator;
import com.avlsi.cast2.directive.impl.DirectiveTable;
import com.avlsi.cast2.util.DirectiveActionFilter;
import com.avlsi.cast2.util.DirectiveActionInterface;
import com.avlsi.cast2.util.DirectiveFilter;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cast2.util.StandardParsingOption;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;
import com.avlsi.cell.InstanceTrace;
import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.BlockIterator;
import com.avlsi.fast.DirectiveBlock;
import com.avlsi.file.cdl.parser.CDLSimpleFilter;
import com.avlsi.file.cdl.util.rename.CDLRenameException;
import com.avlsi.file.common.HierName;
import com.avlsi.geometry.BoundingBox;
import com.avlsi.geometry.Point;
import com.avlsi.io.FileSearchPath;
import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.CommandLineArgsIterator;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.container.CollectionUtils;
import com.avlsi.util.container.StringContainerIterator;
import com.avlsi.util.container.Pair;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.functions.UnaryPredicate;
import com.avlsi.util.text.StringUtil;

public final class MergeHint {
    private MergeHint() {
        throw new AssertionError();
    }
    private static void usage() {
        System.err.print(
            "java com.avlsi.tools.jauto.MergeHint\n" +
            "\t[ --cast-path=<path> (defaults to .) ]\n" +
            "\t[ --cast-version=[ 1 | 2 ] (defaults to 2) ]\n" +
            "\t--cell=<mod.ule.cell.subtype:...> (top level cell)\n" +
            "\t[ --many (read arguments from stdin) ]\n" +
            "\t[\n" +
            "\t--max-width-diff=<max-width> (maximum difference in widths)\n" +
            "\t--max-avg-width-diff=<avg-width> (maximum average difference in widths)\n" +
            "\t--max-fixed-width-diff=<width> (defaults to max-width)\n" +
            "\t--max-fixed-avg-width-diff=<width> (defaults to avg-width) ]\n" +
            "\t]\n" +
            "\t[ --threshold=<number> (parameter for new distance function) ]\n" +
            "\t[\n" +
            "\t--max-width-ratio=<max-ratio> (maximum ratio of widths)\n" +
            "\t--max-fixed-width-ratio=<ratio> (defaults to max-ratio)\n" +
            "\t]\n" +
            "\t[ --directive (only merge cells with compatible directives) ]\n" +
            "\t[ --merge_target=<cell:cell:...> (also merge against listed cells) ]\n" +
            "\t[ --instance-dir=<path> (choose smallest cell when several possible ]\n" +
            "\t[\n" +
            "\t  --merge-cells=<cell:cell:...> (only merge listed cells)\n" +
            "\t| --exclude-cells=<cell:cell:...> (exclude listed cells)\n" +
            "\t]\n"
        );
        System.exit(1);
    }

    private static final Map prefix = new HashMap();
    static {
        prefix.put(new Character('f'), new Double(1e-15));
        prefix.put(new Character('p'), new Double(1e-12));
        prefix.put(new Character('n'), new Double(1e-9));
        prefix.put(new Character('u'), new Double(1e-6));
        prefix.put(new Character('m'), new Double(1e-3));
        prefix.put(new Character('k'), new Double(1e3));
        prefix.put(new Character('M'), new Double(1e6));
        prefix.put(new Character('G'), new Double(1e9));
        prefix.put(new Character('T'), new Double(1e12));
    }

    private static Double getDoubleValue(String arg) {
        if (arg == null) return null;

        Double scale = null;
        for (int i = 0; i < arg.length(); ++i) {
            final char c = arg.charAt(i);
            if (Character.isLetter(c)) {
                scale = (Double) prefix.get(new Character(c));
                if (scale != null) {
                    arg = arg.substring(0, i);
                    break;
                }
            }
        }

        try {
            double value = Double.parseDouble(arg);
            return new Double(scale == null ? value : value * scale.doubleValue());
        } catch (NumberFormatException e) {
            return null;
        }
    }

    private static Double getDoubleValue(String arg, Double def) {
        final Double result = getDoubleValue(arg);
        return result == null ? def : result;
    }

    private static Double getDoubleValue(final CommandLineArgs args,
                                         final String arg,
                                         final Double def) {
        return getDoubleValue(args.getArgValue(arg, null), def);
    }

    private static void prepare(final CellInterface cell, 
                                final SortedMap typeMap,
                                final Set seen,
                                final InstanceTrace it) {
        if (CellUtils.isWiring(cell)) return;

        final String type = cell.getFullyQualifiedType();
        if (!seen.add(type)) return;

        if (CellUtils.isLeaf(cell)) {
            if (cell.containsNetlist()) {
                final int index = type.lastIndexOf('.');
                final String base = type.substring(0, index);
                final String subtype = type.substring(index + 1);

                if (!typeMap.containsKey(base)) {
                    typeMap.put(base, new TreeMap());
                }
                final SortedMap subtypeMap = (SortedMap) typeMap.get(base);
                if (subtypeMap.containsKey(subtype)) return;
                subtypeMap.put(subtype, new Pair(cell, subtype));
            } else {
                if (!CellUtils.isInternalEnv(cell)) {
                    System.err.println("WARNING: No netlist block");
                    System.err.print(it);
                }
            }
        }

        for (Iterator i = cell.getLocalSubcellPairs(); i.hasNext(); ) {
            final Pair p = (Pair) i.next();
            final HierName instName = (HierName) p.getFirst();
            final CellInterface subcell = (CellInterface) p.getSecond();
            it.enter(subcell.getFullyQualifiedType(),
                     instName.getAsString('.'));
            prepare(subcell, typeMap, seen, it);
            it.leave();
        }
    }

    private static void prepare(final CellInterface cell, 
                                final SortedMap typeMap) {
        final InstanceTrace it = new InstanceTrace();
        it.enter(cell.getFullyQualifiedType(), "top level");
        prepare(cell, typeMap, new HashSet(), it);
        it.leave();
    }

    private static void instance(final CellInterface cell,
                                 final Map instanceMap) {
        if (CellUtils.isWiring(cell)) return;

        final String type = cell.getFullyQualifiedType();

        if (instanceMap.containsKey(type)) {
            final int val = ((Integer) instanceMap.get(type)).intValue();
            instanceMap.put(type, new Integer(val + 1));
        } else {
            instanceMap.put(type, new Integer(1));
        }

        for (Iterator i = cell.getLocalSubcellPairs(); i.hasNext(); ) {
            final Pair p = (Pair) i.next();
            final CellInterface subcell = (CellInterface) p.getSecond();
            instance(subcell, instanceMap);
        }
    }

    private static HashMap cache = new HashMap();
    private static CastFileParser cfp = null;
    private static CDLSimpleFilter ignoreStaticizer = null;
    private static class StaticizerFilter extends CDLSimpleFilter {
        public void makeCall(HierName name, String subName, HierName[] args,
                             Map parameters) {
            try {
                final CellInterface ci = cfp.getFullyQualifiedCell(subName);
                if (((Boolean) DirectiveUtils.getTopLevelDirective(ci,
                    DirectiveConstants.STATICIZER_GATE)).booleanValue())
                    return;
            } catch (Exception e) {
            }
            super.makeCall(name, subName, args, parameters);
        }
    }
    private static NetlistDistance.CompiledForm getCompiledForm(CellInterface c)
    {
        final NetlistDistance.CompiledForm cf;
        if (cache.containsKey(c)) {
            cf = (NetlistDistance.CompiledForm) cache.get(c);
        } else {
            cf = NetlistDistance.getCompiledForm(c, ignoreStaticizer, cfp);
            cache.put(c, cf);
        }
        return cf;
    }

    private interface Cluster {
        /**
         * Returns the score of adding <code>cell</code> to this cluster.  A
         * large score will make it more like the cell will be added to this
         * cluster.
         * @return Score of adding the cell, or <code>null</code> if the cell
         * cannot be added.
         **/
        Double score(final CellInterface cell);

        /**
         * Add <code>cell</code> to this cluster.
         * @param cell Cell to add
         * @param subtype Subtype of the cell
         **/
        void add(final CellInterface cell, final String subtype);

        /**
         * Returns the canonical subtype of this cluster.
         **/
        String canonical();

        /**
         * Returns an iterator over subtypes that are part of this cluster, not
         * including the canonical subtype.
         **/
        Iterator iterator();

        /**
         * Returns the number of subtypes that are part of this cluster, not
         * including the canonical subtype.
         **/
        int size();
    }

    private interface ClusterFactory {
        /**
         * Returns a new cluster.
         **/
        Cluster create(final CellInterface cell, final String subtype);
    }

    private interface Selector {
        /**
         * Given a number of clusters that a subtype might merge to, return the
         * one that is most appropriate.
         **/
        Cluster select(final String type, final SortedMap scoreMap);
    }

    private static class ScoreSelector implements Selector {
        public Cluster select(final String type, final SortedMap scoreMap) {
            final Double minScore = (Double) scoreMap.firstKey();
            return (Cluster) ((List) scoreMap.get(minScore)).get(0);
        }
    }

    private static class SizeSelector implements Selector {
        private final File dir;
        private final Map cache;
        private final Selector fallback;
        public SizeSelector(final File dir) {
            this.dir = dir;
            this.cache = new HashMap();
            this.fallback = new ScoreSelector();
        }
        private BoundingBox getBoundingBox(final String type)
            throws IOException, CDLRenameException {
            if (!cache.containsKey(type)) {
                final File fp =
                    new File(dir, Floorplan.getInstanceFilename(type));
                final Reader rin = new BufferedReader(new FileReader(fp));
                Floorplan.readHierarchy(rin,
                    new SubcellProcessor() {
                        public void process(final BoundingBox bBox) {
                            cache.put(type, bBox);
                        }
                        public void process(String pinName, BoundingBox bBox)
                        { }
                        public void process(String typeName,
                                            String instanceName,
                                            int orientation, BoundingBox bBox)
                        { }
                    });
                rin.close();
            }
            return (BoundingBox) cache.get(type);
        }
        private double getArea(final BoundingBox bBox) {
            return bBox.getWidth() * bBox.getHeight();
        }
        public Cluster select(final String type, final SortedMap scoreMap) {
            if (scoreMap.size() == 1 &&
                ((List) scoreMap.get(scoreMap.firstKey())).size() == 1) {
                return (Cluster) ((List) scoreMap.get(scoreMap.firstKey())).get(0);
            } else {
                double minArea = Double.MAX_VALUE;
                Cluster minCluster = null;
                for (Iterator i = scoreMap.entrySet().iterator(); i.hasNext(); )
                {
                    final Map.Entry entry = (Map.Entry) i.next();
                    final List clusters = (List) entry.getValue();
                    
                    for (Iterator j = clusters.iterator(); j.hasNext(); ) {
                        final Cluster cluster = (Cluster) j.next();
                        final String fulltype = type + "." + cluster.canonical();
                        final BoundingBox bbox;
                        try {
                            bbox = getBoundingBox(fulltype);
                        } catch (IOException e) {
                            System.err.println("Error reading instances file for " + fulltype + ": " + e);
                            continue;
                        } catch (CDLRenameException e) {
                            System.err.println("Cannot translate " + fulltype +
                                               " to Cadence name:" + e);
                            continue;
                        }
                        if (bbox == null) {
                            System.err.println("Instances file for " + fulltype + " does not contain a bounding box");
                            continue;
                        }

                        final double area = getArea(bbox);
                        if (area < minArea) {
                            minArea = area;
                            minCluster = cluster;
                        }
                    }
                }
                if (minCluster == null) {
                    return fallback.select(type, scoreMap);
                } else {
                    return minCluster;
                }
            }
        }
    }

    /**
     * The stage 2 distance function.
     **/
    private static final class SimpleCluster implements Cluster {
        private final NetlistDistance.CompiledForm canonical;
        private final String subtype;
        private final List subtypes;
        private final double D_a_max, D_m_max;
        private static final double S = 0.5;

        private SimpleCluster(final CellInterface cell, final String subtype,
                              final double D_a_max, final double D_m_max) {
            canonical = getCompiledForm(cell);
            this.subtype = subtype;
            this.subtypes = new ArrayList();
            this.D_a_max = D_a_max;
            this.D_m_max = D_m_max;
        }

        public Double score(final CellInterface cell) {
            final NetlistDistance.CompiledForm cf = getCompiledForm(cell);
            final NetlistDistance.DefaultCalculator dc =
                new NetlistDistance.DefaultCalculator();

            NetlistDistance.getDistance(canonical, cf, dc);

            final Pair p = dc.getResult();
            if (p == null) {
                return null;
            } else {
                final double D_a = ((Double) p.getFirst()).doubleValue();
                final double D_m = ((Double) p.getSecond()).doubleValue();
                if (D_a <= D_a_max && D_m <= D_m_max)
                    return new Double(S * D_a + (1 - S) * D_m);
                else
                    return null;
            }
        }

        public void add(final CellInterface cell, final String subtype) {
            subtypes.add(subtype);
        }

        public String canonical() {
            return subtype;
        }

        public Iterator iterator() {
            return subtypes.iterator();
        }

        public int size() {
            return subtypes.size();
        }

        public static ClusterFactory getFactory(final double avg,
                                                final double max,
                                                final double fixedAvg,
                                                final double fixedMax) {
            return new ClusterFactory() {
                public Cluster create(final CellInterface cell,
                                      final String subtype) {
                    final double D_m_max, D_a_max;
                    if (CellUtils.isFixedSize(cell)) {
                        D_m_max = fixedMax;
                        D_a_max = fixedAvg;
                    } else {
                        D_m_max = max;
                        D_a_max = avg;
                    }
                    return new SimpleCluster(cell, subtype, D_a_max, D_m_max);
                }
            };
        }
    }

    private static abstract class SimpleMergePolicy implements NetlistDistance.CompiledForm.MergePolicy {
        public abstract double transistor(final double widthR,
                                          final double widthS);
        public Map gate(final String type, final Map paramsR,
                        final Map paramsS) {
            final Map result = new HashMap();
            for (Iterator i = paramsR.entrySet().iterator(); i.hasNext(); )
            {
                final Map.Entry entry = (Map.Entry) i.next();
                final Double r = (Double) entry.getValue();
                final Double s = (Double) paramsS.get(entry.getKey());
                final Double d = new Double(transistor(r.doubleValue(), s.doubleValue()));
                result.put(entry.getKey(), d);
            }
            return result;
        }
    }

    private static final class MinMergePolicy extends SimpleMergePolicy {
        public double transistor(final double widthR, final double widthS) {
            return Math.min(widthR, widthS);
        }
    }

    private static final class MaxMergePolicy extends SimpleMergePolicy {
        public double transistor(final double widthR, final double widthS) {
            return Math.max(widthR, widthS);
        }
    }

    /**
     * Cluster that groups by relative size.
     **/
    private static final class RatioCluster implements Cluster {
        private NetlistDistance.CompiledForm maxCanonical;
        private NetlistDistance.CompiledForm minCanonical;
        private final String subtype;
        private final List subtypes;
        private final double D_m_max;

        private static final class Calculator implements NetlistDistance.DistanceCalculator {
            private double score;
            public Calculator() {
                this.score = 0;
            }

            public void transistor(final double widthR, final double widthS) {
                // Skip over evil floating transistors. 
                if (widthR == 0 && widthS == 0) return;

                // But if only one is zero, then that is an error.
                assert widthR != 0 && widthS != 0;

                final double large, small;
                if (widthR > widthS) {
                    large = widthR;
                    small = widthS;
                } else {
                    small = widthR;
                    large = widthS;
                }
                score = Math.max(score, large / small);
            }

            public void gate(final String type, final Map paramsR,
                             final Map paramsS) {
                for (Iterator i = paramsR.entrySet().iterator(); i.hasNext(); )
                {
                    final Map.Entry entry = (Map.Entry) i.next();
                    final Double r = (Double) entry.getValue();
                    final Double s = (Double) paramsS.get(entry.getKey());
                    transistor(r.doubleValue(), s.doubleValue());
                }
            }

            public void mismatch() {
                score = Double.NaN;
            }

            public Double getResult() {
                return Double.isNaN(score) ? null : new Double(score);
            }
        }

        private RatioCluster(final CellInterface cell, final String subtype,
                              final double D_m_max) {
            maxCanonical = minCanonical = getCompiledForm(cell);
            this.subtype = subtype;
            this.subtypes = new ArrayList();
            this.D_m_max = D_m_max;
        }

        public Double score(final CellInterface cell) {
            final NetlistDistance.CompiledForm cf = getCompiledForm(cell);

            final Calculator minCalc = new Calculator();
            NetlistDistance.getDistance(minCanonical, cf, minCalc);
            final Double minResult = minCalc.getResult();
            if (minResult == null || minResult.doubleValue() >= D_m_max) {
                return null;
            }

            final Calculator maxCalc = new Calculator();
            NetlistDistance.getDistance(maxCanonical, cf, maxCalc);
            final Double maxResult = maxCalc.getResult();
            if (maxResult == null || maxResult.doubleValue() >= D_m_max) {
                return null;
            } else {
                return maxResult;
            }
        }

        public void add(final CellInterface cell, final String subtype) {
            final NetlistDistance.CompiledForm cf = getCompiledForm(cell);
            minCanonical = NetlistDistance.CompiledForm.merge(minCanonical, cf, new MinMergePolicy());
            maxCanonical = NetlistDistance.CompiledForm.merge(maxCanonical, cf, new MaxMergePolicy());
            subtypes.add(subtype);
        }

        public String canonical() {
            return subtype;
        }

        public Iterator iterator() {
            return subtypes.iterator();
        }

        public int size() {
            return subtypes.size();
        }

        public static ClusterFactory getFactory(final double max,
                                                final double fixedMax) {
            return new ClusterFactory() {
                public Cluster create(final CellInterface cell,
                                      final String subtype) {
                    final double D_m_max;
                    if (CellUtils.isFixedSize(cell)) {
                        D_m_max = fixedMax;
                    } else {
                        D_m_max = max;
                    }
                    return new RatioCluster(cell, subtype, D_m_max);
                }
            };
        }
    }

    /**
     * Cluster that groups by directive equivalence.  In particular, it
     * compares that directives in the PRS blocks, and directives in the
     * top-level cells are equivalent.  Certain directives require special
     * action, and are exempt from the equivalence check:
     * <ul>
     * <li> <code>fixed_size</code>: ignored when comparing directives.
     * <li> <code>density_scale_factor</code>: when merging A to B, B's value
     * of this directive must less or equal to A's.  See
     * <a href="http://internal/bugzilla/show_bug.cgi?id=2042">bug 2042</a>.
     * </ul>
     **/
    private static final class DirectiveCluster implements Cluster {
        private final static Double OKAY = new Double(0);
        private static final Set<String> RELEVANT = (Set<String>)
            CollectionUtils.addAll(
                new HashSet<String>(),
                DirectiveConstants.HEIGHT,
                DirectiveConstants.WIDTH,
                DirectiveConstants.TRANSISTOR_TYPE,
                DirectiveConstants.EXTRA_DELAY,
                DirectiveConstants.DEFAULT_UP_DELAY,
                DirectiveConstants.DEFAULT_DN_DELAY,
                DirectiveConstants.FRAGMENT,
                DirectiveConstants.CELLNONOBSERVABLE,
                DirectiveConstants.CUTPATH,
                DirectiveConstants.ROUTED);

        private final static DirectiveComparator COMP =
            new DirectiveComparator() {
                public boolean compare(String block, String type,
                                       Object value1, Object value2) {
                    if (value1 == null || value2 == null) {
                        return value1 == value2;
                    } else {
                        return value1.equals(value2);
                    }
                }
            };
        private final String subtype;
        private final List subtypes;
        private final CellInterface canonical;
        private final Set relevant;
        private final Map specialAction;

        private interface SpecialAction {
            boolean isMergeable(String block, String type, Object canon,
                                Object other);
        }

        private static class OrAction implements SpecialAction {
            private final SpecialAction sa1;
            private final SpecialAction sa2;
            public OrAction(final SpecialAction sa1, final SpecialAction sa2) {
                this.sa1 = sa1;
                this.sa2 = sa2;
            }
            public boolean isMergeable(String block, String type, Object canon,
                                       Object other) {
                return sa1.isMergeable(block, type, canon, other) ||
                       sa2.isMergeable(block, type, canon, other);
            }
        }

        private static class IgnoreAction implements SpecialAction {
            public IgnoreAction() { }
            public boolean isMergeable(String block, String type, Object canon,
                                       Object other) {
                return true;
            }
        }

        private static class NumberAction implements SpecialAction {
            public final static int LT = -1;
            public final static int EQ = 0;
            public final static int GT = 1;

            private final int relation;

            public NumberAction(final int relation) {
                this.relation = relation;
            }

            private static int sgn(int x) {
                if (x == 0) return 0;
                else if (x > 0) return 1;
                else return -1;
            }

            public boolean isMergeable(String block, String type, Object canon,
                                       Object other) {
                final Number n1 = (Number) canon;
                final Number n2 = (Number) other;
                return relation ==
                       sgn(Double.compare(n1.doubleValue(), n2.doubleValue()));
            }
        }

        private DirectiveCluster(final CellInterface cell,
                                 final String subtype,
                                 final Set relevant,
                                 final Map specialAction) {
            this.subtype = subtype;
            this.subtypes = new ArrayList();
            this.canonical = cell;
            this.relevant = relevant;
            this.specialAction = specialAction;
        }

        public Double score(final CellInterface cell) {
            // Compare top-level directives
            final BlockInterface cb1 = canonical.getBlockInterface();
            final BlockInterface cb2 = cell.getBlockInterface();
            final DirectiveActionFilter topf =
                new DirectiveFilter.ByDirective(true, relevant);
            if (!DirectiveUtils.equalDirectiveBlock(cb1, topf, cb2, topf,
                                                    COMP)) {
                return null;
            }

            // Compare PRS directives
            final BlockIterator bit1 = cb1.iterator(BlockInterface.PRS);
            final BlockIterator bit2 = cb2.iterator(BlockInterface.PRS);
            if (bit1.hasNext() != bit2.hasNext()) {
                return null;
            }
            final DirectiveActionFilter prsf =
                new DirectiveFilter.ByDirective(true, relevant);
            if (bit1.hasNext()) {
                final BlockInterface prs1 = bit1.next();
                final BlockInterface prs2 = bit2.next();
                if (!DirectiveUtils.equalDirectiveBlock(prs1, prsf, prs2, prsf,
                                                        COMP)) {
                    return null;
                }
            }

            // Special actions
            for (Iterator i = specialAction.entrySet().iterator();
                 i.hasNext(); ) {
                final Map.Entry entry = (Map.Entry) i.next();
                final String key = (String) entry.getKey();
                final Pair p =
                    DirectiveTable.lookupDirective(BlockInterface.CELL, key);
                assert p != null;

                final String valueType = (String) p.getFirst();
                final SpecialAction sa = (SpecialAction) entry.getValue();
                final Object canon =
                    DirectiveUtils.getTopLevelDirective(canonical, key);
                final Object other =
                    DirectiveUtils.getTopLevelDirective(cell, key);
                if (!sa.isMergeable(BlockInterface.CELL, valueType, canon,
                                    other)) {
                    return null;
                }
            }
            return OKAY;
        }

        public void add(final CellInterface cell, final String subtype) {
            subtypes.add(subtype);
        }

        public String canonical() {
            return subtype;
        }

        public Iterator iterator() {
            return subtypes.iterator();
        }

        public int size() {
            return subtypes.size();
        }

        public static ClusterFactory getFactory() {
            return new ClusterFactory() {
                public Cluster create(final CellInterface cell,
                                      final String subtype) {
                    return new DirectiveCluster(cell, subtype, RELEVANT,
                                                Collections.EMPTY_MAP);
                }
            };
        }
    }

    /**
     * The stage 3 distance function.
     **/
    private static final class BetterCluster implements Cluster {
        private final Map instancesMap;
        private CellInterface cellR;
        private NetlistDistance.CompiledForm canonical;
        private int instances;
        private final String subtype;
        private final List subtypes;
        private final double threshold;

        private static final class Calculator implements NetlistDistance.DistanceCalculator {
            private final int instanceR, instanceS;
            private final CellInterface cellR, cellS;
            private double score;

            public Calculator(final int instanceR, final int instanceS,
                              final CellInterface cellR,
                              final CellInterface cellS) {
                this.instanceR = instanceR;
                this.instanceS = instanceS;
                this.cellR = cellR;
                this.cellS = cellS;
                this.score = 0;
            }

            private double size_est(final double widthR, final double widthS) {
                return Math.max(widthR, widthS);
            }

            private double penalty(final CellInterface R, final CellInterface S,
                                   final double widthR, final double widthS) {
                // Do not allow fixed size transistors to get larger
                /* See bug 3380
                if (CellUtils.isFixedSize(R) && widthR < widthS)
                    return Double.POSITIVE_INFINITY;
                else
                */
                    return 0;
            }

            private double layout_cost(final CellInterface R) {
                return 1;
            }

            public void transistor(final double widthR, final double widthS) {
                if (CellUtils.isFixedSize(cellR)) {
                    score +=   instanceS * Math.abs(widthR - widthS);
                } else {
                    score +=  (instanceR + instanceS) * size_est(widthR, widthS)
                            - instanceR * widthR
                            - instanceS * widthS
                            + instanceR * penalty(cellS, cellR, widthS, widthR)
                            + instanceS * penalty(cellR, cellS, widthR, widthS);
                }
            }

            public void gate(final String type, final Map paramsR,
                             final Map paramsS) {
                for (Iterator i = paramsR.entrySet().iterator(); i.hasNext(); )
                {
                    final Map.Entry entry = (Map.Entry) i.next();
                    final Double r = (Double) entry.getValue();
                    final Double s = (Double) paramsS.get(entry.getKey());
                    transistor(r.doubleValue(), s.doubleValue());
                }
            }

            public void mismatch() {
                score = Double.NaN;
            }

            public Double getResult() {
                return Double.isNaN(score) ? null : new Double(score / layout_cost(cellR));
            }
        }

        private int getInstanceCount(final CellInterface cell) {
            return ((Integer) instancesMap.get(cell.getFullyQualifiedType())).intValue();
        }

        private BetterCluster(final CellInterface cell, final String subtype,
                              final Map instancesMap, final double threshold) {
            this.instancesMap = instancesMap;
            this.cellR = cell;
            this.threshold = threshold;
            this.canonical = getCompiledForm(cell);
            this.instances = getInstanceCount(cell);
            this.subtype = subtype;
            this.subtypes = new ArrayList();
        }

        public Double score(final CellInterface cellS) {
            final NetlistDistance.CompiledForm cf = getCompiledForm(cellS);
            final Calculator calc =
                new Calculator(instances, getInstanceCount(cellS), cellR, cellS);
            NetlistDistance.getDistance(canonical, cf, calc);
            final Double result = calc.getResult();
            if (result == null || result.doubleValue() >= threshold) {
                return null;
            } else {
                return result;
            }
        }

        public void add(final CellInterface cell, final String subtype) {
            if (!CellUtils.isFixedSize(cellR)) {
                final NetlistDistance.CompiledForm cf = getCompiledForm(cell);
                canonical = NetlistDistance.CompiledForm.merge(
                        canonical, cf, new MaxMergePolicy());
                instances += getInstanceCount(cell);
            }
            subtypes.add(subtype);
        }

        public String canonical() {
            return subtype;
        }

        public Iterator iterator() {
            return subtypes.iterator();
        }

        public int size() {
            return subtypes.size();
        }

        public static ClusterFactory getFactory(final Map instances,
                                                final double threshold) {
            return new ClusterFactory() {
                public Cluster create(final CellInterface cell,
                                      final String subtype) {
                    return new BetterCluster(cell, subtype, instances,
                                             threshold);
                }
            };
        }
    }

    /**
     * Combines any number of <i>predicate</i> cluster and a single
     * <i>scoring</i> cluster into a cluster.  Cells are mergeable if all
     * predicate clusters agree they are mergeable.  The ordering of possible
     * merge targets is determined by the scoring cluster.
     **/
    private static final class CombinedCluster implements Cluster {
        private final Cluster[] predicates;
        private final Cluster scorer;
        
        private CombinedCluster(final CellInterface cell, final String subtype,
                                final ClusterFactory[] predicateFactory,
                                final ClusterFactory scorerFactory) {
            predicates = new Cluster[predicateFactory.length];
            for (int i = 0; i < predicateFactory.length; ++i) {
                predicates[i] = predicateFactory[i].create(cell, subtype);
            }

            scorer = scorerFactory.create(cell, subtype);
        }

        public Double score(final CellInterface cell) {
            for (int i = 0; i < predicates.length; ++i) {
                if (predicates[i].score(cell) == null) return null;
            }
            return scorer.score(cell);
        }

        public void add(final CellInterface cell, final String subtype) {
            for (int i = 0; i < predicates.length; ++i) {
                predicates[i].add(cell, subtype);
            }
            scorer.add(cell, subtype);
        }

        public String canonical() {
            return scorer.canonical();
        }

        public Iterator iterator() {
            return scorer.iterator();
        }

        public int size() {
            return scorer.size();
        }

        public static ClusterFactory getFactory(final ClusterFactory[] preds,
                                                final ClusterFactory scorer) {
            return new ClusterFactory() {
                public Cluster create(final CellInterface cell,
                                      final String subtype) {
                    return new CombinedCluster(cell, subtype, preds, scorer);
                }
            };
        }
    }

    private static List analyze(final String type,
                                final SortedMap subtypeMap,
                                final ClusterFactory factory,
                                final Selector selector) {
        final List clusters = new ArrayList();

        // Put each fixed sized subtype into its own new cluster
        for (Iterator i = subtypeMap.entrySet().iterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            final Pair p = (Pair) entry.getValue();
            final CellInterface cell = (CellInterface) p.getFirst();
            if (CellUtils.isFixedSize(cell)) {
                clusters.add(factory.create(cell, (String) p.getSecond()));
            }
        }

        final SortedMap scoreMap = new TreeMap();
        for (Iterator i = subtypeMap.entrySet().iterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            final Pair p = (Pair) entry.getValue();
            final CellInterface unfixed = (CellInterface) p.getFirst();
            // Skip over fixed sized subtypes which are already in a cluster
            if (CellUtils.isFixedSize(unfixed)) continue;

            // Calculate which cluster is the best place for this cell
            scoreMap.clear();
            for (int j = 0; j < clusters.size(); ++j) {
                final Cluster cluster = (Cluster) clusters.get(j);
                final Double s = cluster.score(unfixed);
                if (s != null) {
                    if (!scoreMap.containsKey(s))
                        scoreMap.put(s, new ArrayList());
                    ((List) scoreMap.get(s)).add(cluster);
                }
            }

            final String subtype = (String) p.getSecond();
            if (scoreMap.isEmpty()) {
                // Wasn't close enough to anything, create a new cluster
                clusters.add(factory.create(unfixed, subtype));
            } else {
                selector.select(type, scoreMap).add(unfixed, subtype);
            }
        }
        return clusters;
    }

    private static void output(final Map mergedMaps, final Writer w)
        throws IOException {
        for (Iterator i = mergedMaps.entrySet().iterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            final List clusters = (List) entry.getValue();
            for (int j = 0; j < clusters.size(); ++j) {
                final Cluster cluster = (Cluster) clusters.get(j);
                final Iterator k = cluster.iterator();
                if (!k.hasNext()) continue;

                w.write((String) entry.getKey());
                w.write(" ");
                w.write(cluster.canonical());

                while (k.hasNext()) {
                    w.write(" ");
                    w.write((String) k.next());
                }

                w.write("\n");
            }
        }
    }

    private static Map process(final SortedMap typeMap,
                               final ClusterFactory factory,
                               final Selector selector) {
        final Map result = new HashMap();
        for (Iterator i = typeMap.entrySet().iterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            final List clusters =
                analyze((String) entry.getKey(), (SortedMap) entry.getValue(),
                        factory, selector);
            result.put(entry.getKey(), clusters);
        }
        return result;
    }

    private static void stats(final SortedMap typeMap,
                              final Map result,
                              final Writer w) throws IOException {
        w.write("# Total number of leaf types: " + typeMap.size() + "\n");
        int subtypes = 0;
        int mergedCount = 0;
        final Map typeStat = new TreeMap();
        for (Iterator i = typeMap.entrySet().iterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            final String type = (String) entry.getKey();
            final Map subtypeMap = (Map) entry.getValue();
            final int beforeSize = subtypeMap.size();
            final List clusters = (List) result.get(type);
            int merged = 0;
            if (clusters != null) {
                for (int j = 0; j < clusters.size(); ++j) {
                    merged += ((Cluster) clusters.get(j)).size();
                }
            }
            subtypes += beforeSize;
            mergedCount += merged;
            typeStat.put(type, new Pair(new Integer(beforeSize),
                                        new Integer(beforeSize - merged)));
        }
        w.write("# Total number of leaf subtypes: " + subtypes + "\n");
        w.write("# Number of leaf cells after merging: " + (subtypes - mergedCount) + "\n");
        for (Iterator i = typeStat.entrySet().iterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            final Pair p = (Pair) entry.getValue();
            w.write("# " + entry.getKey() + " " + p.getFirst() + " " + p.getSecond() + "\n");
        }
    }

    private static void process(final SortedMap typeMap,
                                final ClusterFactory factory,
                                final Selector selector,
                                final Writer w) throws IOException {
        final Map result = process(typeMap, factory, selector);
        stats(typeMap, result, w);
        output(result, w);
        w.flush();
    }

    private static Pair validArgs(final CommandLineArgs args) {
        // Choose one scorer
        final Double maxWidth = getDoubleValue(args, "max-width-diff", null);
        final Double maxAvg = getDoubleValue(args, "max-avg-width-diff", null);
        final Double maxFixedWidth =
            getDoubleValue(args, "max-fixed-width-diff", maxWidth);
        final Double maxFixedAvg =
            getDoubleValue(args, "max-fixed-avg-width-diff", maxAvg);
        final boolean simple = maxWidth != null && maxAvg != null &&
                               maxFixedWidth != null && maxFixedAvg != null;

        final Double threshold = getDoubleValue(args, "threshold", null);

        // Only one scoring method can be chosen
        if (simple == (threshold != null)) return null;

        final Double[] scorer = simple ?
            new Double[] { maxAvg, maxWidth, maxFixedAvg, maxFixedWidth } :
            new Double[] { threshold };

        // Choose a set of predicates
        final List pred = new ArrayList();

        final Double maxRatio = getDoubleValue(args, "max-width-ratio", null);
        final Double maxFixedRatio =
            getDoubleValue(args, "max-fixed-width-ratio", maxRatio);
        if (maxRatio != null && maxFixedRatio != null) {
            pred.add(RatioCluster.getFactory(maxRatio.doubleValue(),
                                             maxFixedRatio.doubleValue()));
        }

        final boolean directive = args.argExists("directive");
        if (directive) {
            pred.add(DirectiveCluster.getFactory());
        }

        return new Pair(scorer, (ClusterFactory[]) pred.toArray(new ClusterFactory[0]));
    }

    private static ClusterFactory getFactory(final Pair distanceType,
                                             final CommandLineArgs args,
                                             final CellInterface[] cells,
                                             final Map instanceMap) {
        final ClusterFactory scorer;

        final Double[] scorerArgs = (Double[]) distanceType.getFirst();
        if (scorerArgs.length == 4) {
            scorer = SimpleCluster.getFactory(scorerArgs[0].doubleValue(),
                                              scorerArgs[1].doubleValue(),
                                              scorerArgs[2].doubleValue(),
                                              scorerArgs[3].doubleValue());
        } else {
            if (instanceMap.isEmpty()) {
                for (int i = 0; i < cells.length; ++i) {
                    instance(cells[i], instanceMap);
                }
            }
            scorer = BetterCluster.getFactory(instanceMap,
                                              scorerArgs[0].doubleValue());
        }

        final ClusterFactory[] pred =
            (ClusterFactory[]) distanceType.getSecond();

        if (pred.length > 0) {
            return CombinedCluster.getFactory(pred, scorer);
        } else {
            return scorer;
        }
    }

    private static Writer getWriter(final CommandLineArgs args) throws IOException {
        final String output = args.getArgValue("output", null);
        if (output == null) {
            return new OutputStreamWriter(System.out);
        } else {
            return new FileWriter(output);
        }
    }

    private static CellInterface getCell(final CastFileParser cfp,
                                         final String fqcn) throws Exception {
        final CellInterface result = cfp.getFullyQualifiedCell(fqcn);

        if (!CellUtils.isSubtype(result)) {
            System.err.println("ERROR: Cell " + fqcn + " does not appear to " +
                               "be a subtype.");
            usage();
        }

        return result;
    }

    public static void main(String[] args) throws Exception {
        final CommandLineArgs theArgs =
            new CachingCommandLineArgs(
                    new CommandLineArgsWithConfigFiles(
                        new CommandLineArgsDefImpl(args)));

        final String castRoot = theArgs.getArgValue("cast-path", ".");
        final String castVersion = theArgs.getArgValue("cast-version", "2");
        final String cellName = theArgs.getArgValue("cell", null);
        final Pair distanceType = validArgs(theArgs);
        final boolean checkDirectives = theArgs.argExists("check-directives");
        final boolean many = theArgs.argExists("many");

        if (cellName == null) {
            System.err.println("ERROR: You must specify a cellname.");
            usage();
        } else if (!checkDirectives && distanceType == null && !many) {
            System.err.println("ERROR: Parameters missing or specified incorrectly.");
            usage();
        }

        cfp = CastCacheManager.getDefault().getCastFileParser(
                new FileSearchPath(castRoot), castVersion,
                new StandardParsingOption(theArgs));

        if (theArgs.argExists("ignore-staticizer"))
            ignoreStaticizer = new StaticizerFilter();
        final String[] cellNames = StringUtil.split(cellName, ':');
        final String mergeTarget = theArgs.getArgValue("merge_target", "");
        final String[] mergeTargets = StringUtil.split(mergeTarget, ':');

        final CellInterface[] cells = new CellInterface[cellNames.length];
        for (int i = 0; i < cells.length; ++i) {
            cells[i] = getCell(cfp, cellNames[i]);
        }

        final CellInterface[] mergeCells =
            new CellInterface[mergeTargets.length];
        for (int i = 0; i < mergeTargets.length; ++i) {
            mergeCells[i] = getCell(cfp, mergeTargets[i]);
        }

        if (checkDirectives) {
            final Writer w = getWriter(theArgs);
            for (CellInterface cell : cells) {
                w.write(cell.getFullyQualifiedType());
                final Cluster cluster =
                    DirectiveCluster.getFactory().create(cell, cell.getType());
                for (CellInterface mergeCell : mergeCells) {
                    if (cluster.score(mergeCell) != null) {
                        w.write(' ' + mergeCell.getFullyQualifiedType());
                    }
                }
                w.write('\n');
            }
            w.flush();
            return;
        }

        final CellInterface[] allCells =
            new CellInterface[cells.length + mergeCells.length];
        System.arraycopy(cells, 0, allCells, 0, cells.length);
        System.arraycopy(mergeCells, 0, allCells, cells.length,
                         mergeCells.length);

        final SortedMap typeMap = new TreeMap();
        for (CellInterface cell : allCells) prepare(cell, typeMap);

        final String instances = theArgs.getArgValue("instance-dir", null);
        final Selector selector;
        if (instances == null) {
            selector = new ScoreSelector();
        } else {
            selector = new SizeSelector(new File(instances));
        }

        final Collection<Pair<String,Boolean>> mergeCandidates =
            new ArrayList<Pair<String,Boolean>>();
        Boolean defaultInclude = null;
        boolean onlyExclude = true;
        for (CommandLineArgsIterator i = theArgs.iterator(); i.hasNext(); ) {
            final CommandLineArg arg = i.next();
            Boolean include = null;
            if (arg.getName().equals("merge-cells")) {
                include = Boolean.FALSE;
                onlyExclude = false;
            } else if (arg.getName().equals("exclude-cells")) {
                include = Boolean.TRUE;
            }
            if (include != null) {
                final String listCell = arg.getValue();
                if (listCell == null) {
                    defaultInclude = include;
                } else {
                    final String[] listCells = StringUtil.split(listCell, ':');
                    for (String c : listCells) {
                        mergeCandidates.add(
                            new Pair<String,Boolean>(c, include));
                    }
                }
            }
        }

        if (!mergeCandidates.isEmpty()) {
            if (defaultInclude == null) {
                if (onlyExclude) defaultInclude = Boolean.FALSE;
                else defaultInclude = Boolean.TRUE;
            }
            final boolean defInclude = defaultInclude.booleanValue();

            final UnaryPredicate pred = new UnaryPredicate() {
                public boolean evaluate(final Object o) {
                    final CellInterface cell = (CellInterface) o;
                    for (Pair<String,Boolean> mergeCandidate :
                            mergeCandidates) {
                        final UnaryPredicate matcher =
                            CellUtils.getTypeMatcher(
                                Collections.singleton(
                                    mergeCandidate.getFirst()));
                        if (matcher.evaluate(cell)) {
                            return mergeCandidate.getSecond().booleanValue();
                        }
                    }
                    return defInclude;
                }
            };
            for (Iterator i = typeMap.entrySet().iterator(); i.hasNext(); ) {
                final Map.Entry entry = (Map.Entry) i.next();
                final Map subtypeMap = (Map) entry.getValue();
                for (Iterator j = subtypeMap.entrySet().iterator(); j.hasNext(); ) {
                    final Map.Entry subentry = (Map.Entry) j.next();
                    final Pair p = (Pair) subentry.getValue();
                    final CellInterface c = (CellInterface) p.getFirst();
                    if (pred.evaluate(c)) j.remove();
                }
                if (subtypeMap.size() == 0) i.remove();
            }
        }

        if (many) { 
            final Map instanceMap = new HashMap();
            final BufferedReader br =
                new BufferedReader(new InputStreamReader(System.in));
            String line;
            while ((line = br.readLine()) != null) {
                System.out.println("Processing " + line);
                final String[] tokens = StringUtil.tokenize(line);

                final CommandLineArgs lineArgs =
                    new CachingCommandLineArgs(
                            new CommandLineArgsWithConfigFiles(
                                new CommandLineArgsDefImpl(tokens)));

                final Pair lineDistance = validArgs(lineArgs);

                if (lineDistance == null) {
                    System.out.println("WARNING: Invalid parameter specification.  Line \"" + line + "\" ignored.");
                    continue;
                }

                try {
                    final Writer w = getWriter(lineArgs);
                    final ClusterFactory factory =
                        getFactory(lineDistance, lineArgs, allCells, instanceMap);
                    process(typeMap, factory, selector, w);
                    w.close();
                } catch (IOException e) {
                    System.out.println("WARNING: Cannot write to output file.  Line \"" + line + "\" ignored.");
                }
            }
        } else {
            final Writer w = getWriter(theArgs);
            final ClusterFactory factory =
                getFactory(distanceType, theArgs, allCells, new HashMap());
            process(typeMap, factory, selector, w);
            w.close();
        }
    }
}
