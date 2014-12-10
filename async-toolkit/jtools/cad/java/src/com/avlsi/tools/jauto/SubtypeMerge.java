/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.jauto;

import java.io.BufferedReader;
import java.io.FileWriter;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.Writer;
import java.text.DateFormat;
import java.text.Format;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.avlsi.cast.CastCacheManager;
import com.avlsi.cast.CastFileParser;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cast2.util.StandardParsingOption;                             
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;
import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.CadenceNameInterface;
import com.avlsi.file.common.HierName;
import com.avlsi.geometry.BoundingBox;
import com.avlsi.geometry.Point;
import com.avlsi.io.FileSearchPath;
import com.avlsi.io.NullWriter;
import com.avlsi.tools.jauto.Floorplan;
import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.cmdlineargs.defimpl.PedanticCommandLineArgs;
import com.avlsi.util.container.NaturalOrderComparator;
import com.avlsi.util.container.StringContainerIterator;
import com.avlsi.util.container.Pair;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.functions.UnaryPredicate;
import com.avlsi.util.text.StringUtil;

public final class SubtypeMerge {

    private SubtypeMerge() {
        throw new AssertionError();
    }

    private static Format getHeader(final String cellName) {
        final DateFormat dateFormat = DateFormat.getDateInstance(DateFormat.MEDIUM);
        final DateFormat timeFormat = DateFormat.getTimeInstance(DateFormat.LONG);
        final Date now = new Date();
        final StringBuffer header = new StringBuffer();
        header.append(cellName);
        header.append(":");
        header.append(dateFormat.format(now));
        header.append(":");
        header.append(timeFormat.format(now));
        header.append(":");
        header.append(System.getProperty("user.name"));
        header.append("\n");
        return new MessageFormat(header.toString());
    }

    private static Pair getSubtypePair(final SortedMap typeMap,
                                       final String type) {
        final int index = type.lastIndexOf('.');
        Debug.assertTrue(index > 0, type + " is an invalid type name!");
        final String base = type.substring(0, index);
        final String subtype = type.substring(index + 1);

        final SortedMap subtypeMap = (SortedMap) typeMap.get(base);
        return (Pair) subtypeMap.get(subtype);
    }

    private static String mapType(final SortedMap typeMap, final String type) {
        final Pair p = getSubtypePair(typeMap, type);

        return (String) p.getSecond();
    }

    private static CellInterface getType(final SortedMap typeMap,
                                         final String type) {
        final Pair p = getSubtypePair(typeMap, type);
        return p == null ? null : (CellInterface) p.getFirst();
    }

    private static void setType(final SortedMap typeMap, final String base,
                                final String fromType, final String toType) {
        final SortedMap subtypeMap = (SortedMap) typeMap.get(base);
        final Pair old = (Pair) subtypeMap.get(fromType);
        subtypeMap.put(fromType, new Pair(old.getFirst(), toType));
    }

    private static Map getLocalSubcellMap(final CellInterface ci) {
        final Map m = new TreeMap();
        for (Iterator i = ci.getLocalSubcellPairs(); i.hasNext(); ) {
            final Pair p = (Pair) i.next();
            assert !m.containsKey(p.getFirst()) : "Duplicate instance names " + p.getFirst() + " in cell " + ci.getFullyQualifiedType();
            m.put(p.getFirst(), p.getSecond());
        }
        return m;
    }

    /**
     * Should only be used to compare midlevel cells a and b that pass
     * MidlevelComparator.compareTo(a,b) == 0.
     **/
    private static class GeometryComparator implements Comparator {
        private static int compareDouble(final double a, final double b) {
            if (a < b) return -1;
            else if (a > b) return 1;
            else return 0;
        }
        private static class OrderComparator implements Comparator {
            private final Map map;
            public OrderComparator(final Map map) {
                this.map = map;
            }
            public int compare(final Object o1, final Object o2) {
                final Point p1 = ((Info) map.get(o1)).bBox.getLowerLeft();
                final Point p2 = ((Info) map.get(o2)).bBox.getLowerLeft();

                int result;
                result = compareDouble(p1.getY(), p2.getY());
                if (result != 0) return result;

                result = compareDouble(p1.getX(), p2.getX());
                return result;
            }
        }

        private static class Info {
            public final int orientation;
            public final BoundingBox bBox;
            public Info(final int orientation, final BoundingBox bBox) {
                this.orientation = orientation;
                this.bBox = bBox;
            }
            public String toString() {
                return "<" + orientation + " : " + bBox + ">";
            }
        }

        private final Floorplan fp;
        private final Map cache;
        public GeometryComparator(final Floorplan fp) {
            this.fp = fp;
            this.cache = new HashMap();
        }
        private Map readInstanceMap(final String type) {
            final Map result = new TreeMap();
            fp.readHierarchy(type, new SubcellProcessor() {
                public void process(final BoundingBox bBox) { }
                public void process(final String pinName, final BoundingBox bBox) { }
                public void process(final String typeName,
                                    final String instanceName,
                                    final int orientation,
                                    final BoundingBox bBox) {
                    result.put(instanceName, new Info(orientation, bBox));
                }
            });
            return result;
        }

        private Map getInstanceMap(final String type) {
            if (!cache.containsKey(type)) {
                cache.put(type, readInstanceMap(type));
            }
            return (Map) cache.get(type);
        }

        public int compare(final Object o1, final Object o2) {
            if (o1 == o2) return 0;

            int result;

            final Map m1 =
                getInstanceMap(((CellInterface) o1).getFullyQualifiedType());
            final Map m2 =
                getInstanceMap(((CellInterface) o2).getFullyQualifiedType());

            // geometry information may have different number of instances when
            // the CAST does not, due to, for example, user error
            result = m1.size() - m2.size();
            if (result != 0) return result;

            for (Iterator i = m1.values().iterator(),
                          j = m2.values().iterator(); i.hasNext(); ) {
                final Info i1 = (Info) i.next();
                final Info j1 = (Info) j.next();

                // orientation must match
                result = i1.orientation - j1.orientation;
                if (result != 0) return result;

                // y coordinates must match
                result = compareDouble(i1.bBox.getUpperRight().getY(),
                                       j1.bBox.getUpperRight().getY());
                if (result != 0) return result;

                result = compareDouble(i1.bBox.getLowerLeft().getY(),
                                       j1.bBox.getLowerLeft().getY());
                if (result != 0) return result;
            }

            /**
             * Sort the instances by Y then X, and compare the order of the
             * instances.  They must be the same.
             **/
            final TreeMap x1 = new TreeMap(new OrderComparator(m1));
            x1.putAll(m1);
            final TreeMap x2 = new TreeMap(new OrderComparator(m2));
            x2.putAll(m2);

            for (Iterator i = x1.keySet().iterator(),
                          j = x2.keySet().iterator(); i.hasNext(); ) {
                final String i1 = (String) i.next();
                final String j1 = (String) j.next();

                result = i1.compareTo(j1);
                if (result != 0) return result;
            }

            return 0;
        }
    }

    private static class MidlevelComparator implements Comparator {
        private final SortedMap typeMap;
        public MidlevelComparator(final SortedMap typeMap) {
            this.typeMap = typeMap;
        }
        public int compare(final Object o1, final Object o2) {
            int result;

            final Map m1 = getLocalSubcellMap((CellInterface) o1);
            final Map m2 = getLocalSubcellMap((CellInterface) o2);

            // Both cells must have the same number of subcells
            result = m1.size() - m2.size();
            if (result != 0) return result;

            // The instance names of the subcells must be identical
            for (Iterator i = m1.keySet().iterator(),
                          j = m2.keySet().iterator(); i.hasNext(); ) {
                final HierName h1 = (HierName) i.next();
                final HierName h2 = (HierName) j.next();
                result = h1.compareTo(h2);
                if (result != 0) return result;
            }

            // The types of the subcells must also match
            for (Iterator i = m1.keySet().iterator(); i.hasNext(); ) {
                final HierName key = (HierName) i.next();
                final CellInterface sc1 = (CellInterface) m1.get(key);
                final CellInterface sc2 = (CellInterface) m2.get(key);

                assert sc1 != null && sc2 != null;

                final String type1 = sc1.getFullyQualifiedType();
                final String type2 = sc2.getFullyQualifiedType();

                // Wiring cells are not present in typeMap
                if (CellUtils.isWiring(sc1) || CellUtils.isWiring(sc2)) {
                    result = type1.compareTo(type2);
                } else {
                    final String mt1 = mapType(typeMap, type1);
                    final String mt2 = mapType(typeMap, type2);
                    result = mt1.compareTo(mt2);
                }
                if (result != 0) return result;
            }
            return 0;
        }
    }

    private static class MidlevelPairComparator implements Comparator {
        private final Comparator midlevel;
        private final Comparator subtype;
        public MidlevelPairComparator(final Comparator midlevel) {
            this(midlevel, new NaturalOrderComparator());
        }
        public MidlevelPairComparator(final Comparator midlevel,
                                      final Comparator subtype) {
            this.midlevel = midlevel;
            this.subtype = subtype;
        }
        public int compare(final Object o1, final Object o2) {
            final Pair p1 = (Pair) o1;
            final Pair p2 = (Pair) o2;
            final CellInterface c1 = (CellInterface) p1.getFirst();
            final CellInterface c2 = (CellInterface) p2.getFirst();
            final int comp = midlevel.compare(c1, c2);
            if (comp != 0) return comp;
            final String s1 = (String) p1.getSecond();
            final String s2 = (String) p2.getSecond();
            return subtype.compare(s1, s2);
        }
    }

    /**
     * Assume when/if cell names can be Unicode, they won't begin with
     * '\uFFFF'.
     **/
    private static Pair getSuccessorPair(final Pair p) {
        return new Pair(p.getFirst(), Character.toString(Character.MAX_VALUE));
    }


    /**
     * typeMap is a sorted map: cell type -&gt; (sorted map: subtype -&gt;
     * (cell, subtype))
     **/
    static void prepare(final CellInterface cell, final SortedMap typeMap) {
        if (CellUtils.isWiring(cell)) return;

        final String type = cell.getFullyQualifiedType();
        final int index = type.lastIndexOf('.');
        final String base = type.substring(0, index);
        final String subtype = type.substring(index + 1);

        if (!typeMap.containsKey(base)) {
            typeMap.put(base, new TreeMap());
        }
        final SortedMap subtypeMap = (SortedMap) typeMap.get(base);
        if (subtypeMap.containsKey(subtype)) return;
        subtypeMap.put(subtype, new Pair(cell, subtype));

        for (Iterator i = cell.getLocalSubcellPairs(); i.hasNext(); ) {
            final Pair p = (Pair) i.next();
            final CellInterface subcell = (CellInterface) p.getSecond();
            prepare(subcell, typeMap);
        }
    }

    private static List getPairPartition(SortedSet pairSet) {
        final List result = new ArrayList();
        while (!pairSet.isEmpty()) {
            final Pair first = (Pair) pairSet.first();
            final Pair succ = getSuccessorPair(first);
            result.add(pairSet.headSet(succ));
            pairSet = pairSet.tailSet(succ);
        }
        return result;
    }

    private static void doMerge(final SortedSet mergedSet,
                                final SortedMap typeMap,
                                final Comparator subtypeComp,
                                final String base) {
        if (mergedSet.size() > 1) {
            final TreeSet candidates = new TreeSet(subtypeComp);
            for (Iterator i = mergedSet.iterator(); i.hasNext(); ) {
                final Pair p = (Pair) i.next();
                final CellInterface ci = (CellInterface) p.getFirst();
                // XXX: Don't use CellUtils.isFixedSize because that also
                // checks to see if the cell has a netlist block
                if (((Boolean) DirectiveUtils.getTopLevelDirective(ci, DirectiveConstants.FIXED_SIZE)).booleanValue()) {
                    candidates.add(p);
                }
            }
            if (candidates.isEmpty()) {
                candidates.addAll(mergedSet);
            }
            final Pair canonical = (Pair) candidates.first();
            final String subtype = (String) canonical.getSecond();
            for (Iterator j = mergedSet.iterator(); j.hasNext(); ) {
                final Pair p = (Pair) j.next();
                if (!subtype.equals(p.getSecond()))
                    setType(typeMap, base, (String) p.getSecond(), subtype);
            }
        }
    }

    /**
     * Find midlevel cells that instantiate the same subcells and merge them
     * together, into the subtype that is lexicographically the smallest.
     * @param cell Root cell to descend into
     * @param typeMap Subtype to subtype mapping
     * @param comp A comparator capable of comparing Pair(CellInterface, String)
     * @param seen All the types already completed
     **/
    private static void mergeAll(final CellInterface cell, 
                                 final SortedMap typeMap,
                                 final Comparator comp,
                                 final Comparator subtypeComp,
                                 final Comparator fp,
                                 final Set seen) {
        if (CellUtils.isWiring(cell) || CellUtils.isLeaf(cell)) return;

        final String type = cell.getFullyQualifiedType();
        final int index = type.lastIndexOf('.');

        final String base = type.substring(0, index);
        // Process each type only once.
        if (!seen.add(base)) return;

        for (Iterator i = cell.getLocalSubcellPairs(); i.hasNext(); ) {
            final Pair p = (Pair) i.next();
            final CellInterface subcell = (CellInterface) p.getSecond();
            mergeAll(subcell, typeMap, comp, subtypeComp, fp, seen);
        }

        final SortedMap subtypeMap = (SortedMap) typeMap.get(base);
        if (subtypeMap.size() <= 1) return;

        final SortedSet pairSet = new TreeSet(comp);
        pairSet.addAll(subtypeMap.values());
        for (Iterator i = getPairPartition(pairSet).iterator();
             i.hasNext(); ) {
            final SortedSet mergedSet = (SortedSet) i.next();
            if (fp == null) {
                doMerge(mergedSet, typeMap, subtypeComp, base);
            } else {
                final SortedSet fpSet = new TreeSet(fp);
                fpSet.addAll(mergedSet);
                final List fpList = getPairPartition(fpSet);
                if (fpList.size() > 1) {
                    System.out.print("Warning: Due to floorplanning, the following subtypes of " + base + " will not be merged together:");
                    for (Iterator j = fpList.iterator(); j.hasNext(); ) {
                        final SortedSet fpMerge = (SortedSet) j.next();
                        final Pair p = (Pair) fpMerge.first();
                        System.out.print(" " + p.getSecond());
                        doMerge(fpMerge, typeMap, subtypeComp, base);
                    }
                    System.out.println();
                } else {
                    doMerge(mergedSet, typeMap, subtypeComp, base);
                }
            }
        }
    }

    private static final Pattern SUBTYPE_PATTERN =
        Pattern.compile("^(\\d+)(.*)");

    /**
     * Merge all cells.
     * @param cell Root cell to traverse over.
     * @param typeMap Mapping of subtypes.
     * @param comp A comparator capable of comparing CellInterfaces
     **/
    private static void mergeAll(final CellInterface cell,
                                 final SortedMap typeMap,
                                 final Comparator comp,
                                 final Floorplan fp) {
        final Comparator subtypeComp = new MidlevelPairComparator(
            new Comparator() {
                public int compare(final Object o1, final Object o2) {
                    return 0;
                }
            },
            new Comparator() {
                private Pair<Integer,String> getParts(final String s) {
                    final Matcher m = SUBTYPE_PATTERN.matcher(s);
                    final Integer subtype;
                    final String suffix;
                    if (m.matches()) {
                        subtype = Integer.valueOf(m.group(1));
                        suffix = m.group(2);
                    } else {
                        subtype = 0;
                        suffix = s;
                    }
                    return new Pair<Integer,String>(subtype, suffix);
                }
                public int compare(final Object o1, final Object o2) {
                    final String s1 = (String) o1;
                    final String s2 = (String) o2;
                    return getParts(s1).compareTo(getParts(s2));
                }
            });
        mergeAll(cell, typeMap, new MidlevelPairComparator(comp), subtypeComp,
                 fp == null ? null :
                       new MidlevelPairComparator(new GeometryComparator(fp)),
                 new HashSet());
    }
    
    /**
     *
     **/
    private static boolean mergeCell(final String type,
                                     final Map typeMap,
                                     final String toSubtype,
                                     final String minSubtype,
                                     final String maxSubtype,
                                     final Comparator comp) {
        final Map subtypeMap = (Map) typeMap.get(type);
        boolean success = false;
        if (subtypeMap != null) {
            final Pair toPair = (Pair) subtypeMap.get(toSubtype);
            for (final Iterator i = subtypeMap.entrySet().iterator();
                 i.hasNext(); ) {
                final Map.Entry entry = (Map.Entry) i.next();
                final Pair pair = (Pair) entry.getValue();
                if (toPair == pair) continue;
                final String value = (String) pair.getSecond();
                if (value.compareTo(minSubtype) >= 0 &&
                    value.compareTo(maxSubtype) <= 0 &&
                    (comp == null ||
                     comp.compare((CellInterface) pair.getFirst(),
                                  (CellInterface) toPair.getFirst()) == 0)) {
                    success = true;
                    subtypeMap.put(entry.getKey(),
                                   new Pair(pair.getFirst(), toSubtype));
                }
            }
        }
        return success;
    }

    private static void usage() {
        System.err.println("java com.avlsi.tools.jauto.SubtypeMerge");
        System.err.println("\t[ --cast-path=<path> ] (defaults to .)");
        System.err.println("\t[ --cast-version=[ 1 | 2 ] (defaults to 2)");
        System.err.println("\t[ --subtype-path=<path> ] (defaults to .)");
        System.err.println("\t--cell=<fqcn1:fqcn2:...> (top level cells)");
        System.err.println("\t--merge_target=<fqcn1:fqcn2:...> (merge target cells)");
        System.err.println("\t[ --allow-merge-fixed ] (allow merging of fixed size subtypes)");
        System.err.println("\t[ --all (try to merge all midlevel cells after processing input spec)");
        System.err.println("\t  [ --instance-dir=<path> ] (take midlevel floorplan into account)");
        System.err.println("\t]");
        System.err.println("\t[ --output-leaves ] (write leaf cells as well)");
        System.exit(1);
    }

    public static void main(String[] args) throws Exception {
        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
        final CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles( parsedArgs ); 

        final CommandLineArgs cachedArgs = 
            new CachingCommandLineArgs( argsWithConfigs );
        final PedanticCommandLineArgs pedanticArgs =
            new PedanticCommandLineArgs( cachedArgs );

        final CommandLineArgs theArgs = pedanticArgs;
        final String castRoot = theArgs.getArgValue("cast-path", ".");
        final String castVersion = theArgs.getArgValue("cast-version", "2");
        final String subtypePath = theArgs.getArgValue("subtype-path", ".");
        final String output = theArgs.getArgValue("output", null);
        final boolean mergeFixed = theArgs.argExists("allow-merge-fixed");
        final boolean mergeMid = theArgs.argExists("all");
        final boolean outputLeaves = theArgs.argExists("output-leaves");

        // combine --cell and --merge_target lists
        final String cellName = theArgs.getArgValue("cell", null);

        if (cellName == null) {
            System.err.println("ERROR: You must specify a cellname.");
            usage();
        } else if (subtypePath == null) {
            System.err.println("ERROR: You must specify a subtype output path.");
            usage();
        }

        final String[] cellNames = StringUtil.split(cellName, ':');
        final String mergeTarget = theArgs.getArgValue("merge_target", "");
        final String[] mergeTargets = StringUtil.split(mergeTarget, ':');
        final String[] allCellNames = new String[cellNames.length + mergeTargets.length];
        System.arraycopy(cellNames, 0, allCellNames, 0, cellNames.length);
        System.arraycopy(mergeTargets, 0, allCellNames, cellNames.length, mergeTargets.length);

        Floorplan fp = null;
        if (mergeMid) {
            final String s = theArgs.getArgValue("instance-dir", null);
            if (s != null) {
                try {
                    fp = new Floorplan(s);
                } catch (Floorplan.Exception e) {
                    System.err.println("ERROR: Specified instance directory invalid.");
                    usage();
                }
            }
        }

        final CastFileParser cfp =
            CastCacheManager.getDefault().getCastFileParser(
                new FileSearchPath(castRoot), castVersion,
                new StandardParsingOption(theArgs));

        if (!pedanticArgs.pedanticOK(false, true)) {
            System.err.println(pedanticArgs.pedanticString());
            usage();
        }

        final CellInterface[] cells = new CellInterface[allCellNames.length];
        final SortedMap typeMap = new TreeMap();
        for (int i = 0; i < allCellNames.length; ++i) {
            cells[i] = cfp.getFullyQualifiedCell(allCellNames[i]);

            if (!CellUtils.isSubtype(cells[i])) {
                System.err.println("ERROR: Cell " + allCellNames[i] + " does not appear to be a subtype.");
                usage();
            }

            prepare(cells[i], typeMap);
        }

        final BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        String line;
        final Comparator midlevel = new MidlevelComparator(typeMap);
        while ((line = br.readLine()) != null) {
            if (line.startsWith("#")) continue;
            final String[] tokens = StringUtil.tokenize(line);
            if (tokens.length < 3) {
                System.err.println("WARNING: Ignoring invalid merge specification: " + line);
                continue;
            }
            final String type = tokens[0];
            final String toSubtype = tokens[1];
            final CellInterface target =
                getType(typeMap, type + "." + toSubtype);
            if (target == null) {
                System.err.println("WARNING: Cannot find cell to merge to: " + line);
                continue;
            }
            final Comparator comp = CellUtils.isLeaf(target) ?  null : midlevel;
            for (int i = 2; i < tokens.length; ++i) {
                final String minSubtype, maxSubtype;
                final int index = tokens[i].indexOf("..");
                if (index > 0) {
                    minSubtype = tokens[i].substring(0, index);
                    maxSubtype = tokens[i].substring(index + 2);
                } else {
                    minSubtype = maxSubtype = tokens[i];
                }
                if (!mergeCell(type, typeMap, toSubtype, minSubtype, maxSubtype, comp)) {
                    System.err.println("WARNING: No cells mergeable: " + type + " " + toSubtype + " " + tokens[i]);
                }
            }
        }

        if (mergeMid) {
            for (int i = 0; i < cells.length; ++i) {
                mergeAll(cells[i], typeMap, midlevel, fp);
            }
        }

        final Collection<SubtypeSplit.Spec> spec =
            new HashSet<SubtypeSplit.Spec>();
        SubtypeOutput.Policy policy;
        policy = new SubtypeOutput.Merge(subtypePath, getHeader(cellName),
                                         typeMap, outputLeaves, spec);
        for (int i = 0; i < cellNames.length; ++i) {
            SubtypeOutput.writeSubtype(policy, cells[i]);
        }

        final Writer specWriter = output == null ? NullWriter.getInstance()
                                                 : new FileWriter(output);
        final CDLNameInterface renamer = new CadenceNameInterface();
        for (SubtypeSplit.Spec s : spec) {
            specWriter.write(s.getSpec(renamer));
            specWriter.write('\n');
        }
        specWriter.close();
    }
}
