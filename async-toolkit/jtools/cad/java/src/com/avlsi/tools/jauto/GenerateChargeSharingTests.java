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
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import com.avlsi.cast.CastFileParser;
import com.avlsi.cast.CastSyntaxException;
import com.avlsi.cast.CastSemanticException;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.directive.DirectiveInterface;
import com.avlsi.cast2.directive.impl.DirectiveSource;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.ExclusiveNodeSets;
import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.BlockIterator;
import com.avlsi.fast.CastDesign;
import com.avlsi.fast.CellType;
import com.avlsi.fast.DirectiveBlock;
import com.avlsi.fast.MergeDirective;
import com.avlsi.file.common.DeviceTypes;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.io.FileSearchPath;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cadencize.CadenceInfo;
import com.avlsi.tools.lvs.NetGraph;
import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgFormatException;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.InvalidCommandLineArgException;
import com.avlsi.util.cmdlineargs.MissingCommandLineArgException;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.container.AliasedMap;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.NaturalOrderComparator;
import com.avlsi.util.text.StringUtil;

public class GenerateChargeSharingTests {

    /**
     * This class cannot be instantiated.
     **/
    private GenerateChargeSharingTests() {
        throw new AssertionError();
    }

    /**
     * Returns the intersection of 2 sets.
     * @param s1 A set.
     * @param s2 A set.
     *
     * @return The intersection of s1 and s2.
     **/
    private static Set intersect(final Set s1, final Set s2) {
        final HashSet set = new HashSet();
        set.addAll(s1);
        set.retainAll(s2);
        return set;
    }

    private static int findLargestShareNumber(final CellInterface ci) {

        final Map m =
            DirectiveUtils.getPrsDirective(ci, DirectiveConstants.SHARED,
                                           DirectiveConstants.HALFOP_TYPE);
        int max = 10000;
        for (Iterator i = m.values().iterator(); i.hasNext(); ) {
            final Integer share = (Integer) i.next();
            max = Math.max(max, share.intValue());
        }
        return max;
    }

    /**
     * Partition the nodes of interest into groups that share transistors.
     * @param graph The transistor NetGraph.
     * @param interest The nodes of interest.
     *
     * @return An AliasedMap with nodes that share transistors represented as
     * aliases.
     **/
    private static AliasedMap getSharingGroups(final NetGraph graph,
                                               final Set interest) {
        final AliasedMap map =
            new AliasedMap(
                new AliasedMap.MergeFunction() {
                    public Object merge(Object o1, Object o2)
                    throws AliasedMap.MergeFailedException {
                        final Set s = new HashSet();
                        s.addAll((Set) o1);
                        s.addAll((Set) o2);
                        return s;
                    }
                },
                new NaturalOrderComparator()
            );
        for (Iterator i = interest.iterator(); i.hasNext(); ) {
            final HierName name = (HierName) i.next();
            final NetGraph.NetNode node = graph.findNetNode(name);
            assert node != null : "Cannot find " + name + " in NetGraph " + graph;
            final Set edgeSet = new HashSet();
            for (Iterator j = node.getPaths().iterator(); j.hasNext(); ) {
                final NetGraph.NetPath path = (NetGraph.NetPath) j.next();
                edgeSet.addAll(path.getEdges());
            }
            try {
                map.addData(name, edgeSet);
                for (Iterator j = map.getCanonicalKeys(); j.hasNext(); ) {
                    final Object key = j.next();
                    if (key == name) continue;

                    final Set old = (Set) map.getValue(key);
                    if (!intersect(old, edgeSet).isEmpty()) {
                        map.makeEquivalent(key, name);
                        break;
                    }
                }
            } catch (AliasedMap.MergeFailedException e) {
                throw new AssertionError("Merge failed.  Cannot happen!");
            }
        }
        return map;
    }

    private static String halfopstr(final HierName name, final Boolean dir) {
        final String str = name.getCadenceString();
        return dir == null ? str: str + (dir.booleanValue() ? "+" : "-");
    }

    private static class GroupInfo {
        public final NetGraph graph;
        public final Group group;
        public final Pair stat;
        private final Set pathSet;
        public GroupInfo(final NetGraph graph, final Group group) {
            this.graph = graph;
            this.group = group;
            this.stat = getStatistic(graph, group);
            // the default compare method for a NetPath does not take into
            // account the ordering of the gates on the path, which is exactly
            // what we do care about
            this.pathSet = new TreeSet(new Comparator() {
                    public int compare(final Object o1, final Object o2) {
                        final NetGraph.NetPath p1 = (NetGraph.NetPath) o1;
                        final NetGraph.NetPath p2 = (NetGraph.NetPath) o2;
                        int i = p1.compareTo(p2);
                        if (i != 0) return i;
                        for (Iterator i1 = p1.getGatesArrayList().iterator(),
                                      i2 = p2.getGatesArrayList().iterator();
                             i1.hasNext() && i2.hasNext(); ) {
                            final HierName h1 = (HierName) i1.next();
                            final HierName h2 = (HierName) i2.next();
                            i = h1.compareTo(h2);
                            if (i != 0) return i;
                        }
                        return 0;
                    }
                });
            for (Iterator i = group.iterator(); i.hasNext(); ) {
                final HierName name = (HierName) i.next();
                final NetGraph.NetNode node = graph.findNetNode(name);
                assert node != null : "Cannot find " + name + " in NetGraph " + graph;
                pathSet.addAll(node.getPaths());
            }
        }
        public boolean equals(final Object o) {
            boolean result = false;
            if (o instanceof GroupInfo) {
                final GroupInfo gi = (GroupInfo) o;
                return stat.equals(gi.stat) && pathSet.equals(gi.pathSet);
            }
            return result;
        }
        public String toString() {
            return "GroupInfo: group = " + outputGroup(group) +
                   " pathSet = " + pathSet;
        }
    }

    /**
     * Represents a set of nodes that are in a single symmetrize group.
     **/
    private static class Group extends ArrayList {
        public Group(final HierName name) {
            assert name != null;
            add(name);
        }
        public Group(final Group group) {
            super(group);
        }
        private void addDirectives(final DirectiveSource source,
                                   final Collection directives,
                                   final String dir,
                                   final Boolean updn,
                                   final Integer val,
                                   final Object sval) {
            for (Iterator i = iterator(); i.hasNext(); ) {
                final HierName name = (HierName) i.next();
                source.definition(dir, DirectiveConstants.HALFOP_TYPE,
                                  makeHalfOp(name, updn), val);
                directives.add(dir + "(" + halfopstr(name, updn) + ")=" + sval);
            }
        }
        public void addSharedDirectives(final DirectiveSource source,
                                        final Collection directives,
                                        final String dir,
                                        final Boolean updn,
                                        final Integer val) {
            addDirectives(source, directives, dir, updn, val, val);
        }
        public void addSymmetrizeDirectives(final DirectiveSource source,
                                            final Collection directives,
                                            final String dir,
                                            final Boolean updn,
                                            final Integer val) {
            if (val != null)
                addDirectives(source, directives, dir, updn, val,
                              symnams.get(val));
        }
    }

    private static int shareId = 10000;
    private static class Sharing extends ArrayList {
        public Sharing(final Group group) {
            assert group != null;
            add(group);
        }
        public Sharing(final Sharing sharing) {
            super(sharing);
        }
        public void addSharedDirectives(final DirectiveSource source,
                                        final Collection directives) {
            for (Iterator i = iterator(); i.hasNext(); ) {
                final Group group = (Group) i.next();
                group.addSharedDirectives(source, directives,
                                          DirectiveConstants.SHARED, null,
                                          new Integer(shareId++));
            }
        }
        public void addSymmetrizeDirectives(final DirectiveSource source,
                                            final Collection directives,
                                            final Integer up,
                                            final Integer dn) {
            for (int i = 0; i < size(); ++i) {
                final Group group = (Group) get(i);
                if (up.equals(dn)) {
                    group.addSymmetrizeDirectives(source, directives,
                                                  DirectiveConstants.SYMMETRIZE,
                                                  null, (Integer) up);
                } else {
                    group.addSymmetrizeDirectives(source, directives,
                                                  DirectiveConstants.SYMMETRIZE,
                                                  Boolean.TRUE, (Integer) up);
                    group.addSymmetrizeDirectives(source, directives,
                                                  DirectiveConstants.SYMMETRIZE,
                                                  Boolean.FALSE, (Integer) dn);
                }
            }
        }
    }

    /**
     * Return all possible ways to break apart an array of nodes.
     *
     * @param names Names of the nodes to group.
     * @param index The index to start at.
     * @return A list of sharings.
     **/
    private static List getPossibleSharing(final HierName[] names,
                                           final int index) {
        assert names.length > 0;
        final List result = new ArrayList();
        if (names.length == index + 1) {
            result.add(new Sharing(new Group(names[index])));
        } else {
            final List sharings = getPossibleSharing(names, index + 1);
            for (Iterator i = sharings.iterator(); i.hasNext(); ) {
                final Sharing sharing = (Sharing) i.next();
                for (int j = 0; j < sharing.size(); ++j) {
                    final Sharing added = new Sharing(sharing);
                    final Group g = new Group((Group) added.get(j));
                    g.add(names[index]);
                    added.set(j, g);
                    result.add(added);
                }
                final Sharing seperate = new Sharing(sharing);
                seperate.add(new Group(names[index]));
                result.add(seperate);
            }
        }
        return result;
    }

    private static List getPossibleSharing(final Iterator aliases) {
        final Set s = new HashSet();
        while (aliases.hasNext()) {
            s.add(aliases.next());
        }
        return getPossibleSharing((HierName[]) s.toArray(new HierName[0]), 0);
    }

    /**
     * Get sharings of the groups to test.  All groups are tested at least
     * once, and as few sharings are tested as possible.
     *
     * @param sharings All possible sharings.
     * @return List of sharings that test all groups.
     **/
    private static List getConfigurations(final List sharings) {
        /* Only sharings with 2 groups matter, since all other sharings can be
         * constructed from them. */
        final List result = new ArrayList();
        for (Iterator i = sharings.iterator(); i.hasNext(); ) {
            final Sharing s = (Sharing) i.next();
            if (s.size() <= 2) result.add(s);
        }
        return result;
    }

    private static HierName Vdd = HierName.makeHierName("Vdd");
    private static HierName GND = HierName.makeHierName("GND");
    private static HierName _RESET = HierName.makeHierName("_RESET");

    public static class Option {
        public final double cutoff, cutoffmin;
        public final Double diffusionLength, mfggrid;
        public final String staticizer, weakInverter, smallInverter;
        public final String lengthUnit, hsizes, outdir;
        public final String gates;
        public final float tau;
        public final TechnologyData tdata;
        public final Map stackMap;
        public final float minWidth;
        private Double getDouble(final CommandLineArgs args, final String arg,
                                 final Double def) {
            final String str = args.getArgValue(arg, null);
            if (str == null) return def;
            else return Double.valueOf(str);
        }
        private double getDouble(final CommandLineArgs args, final String arg,
                                 final double def) {
            final String str = args.getArgValue(arg, null);
            if (str == null) return def;
            else return Double.parseDouble(str);
        }
        private float getFloat(final CommandLineArgs args, final String arg,
                               final float def) {
            final String str = args.getArgValue(arg, null);
            if (str == null) return def;
            else return Float.parseFloat(str);
        }
        private int getInteger(final CommandLineArgs args, final String arg,
                               final int def) {
            final String str = args.getArgValue(arg, null);
            if (str == null) return def;
            else return Integer.parseInt(str);
        }
        public Option(final CommandLineArgs args, final CastFileParser cfp,
                      final Cadencize cad)
            throws CommandLineArgFormatException,
                   InvalidCommandLineArgException,
                   MissingCommandLineArgException {
            cutoff = getDouble(args, "cutoff", Double.MAX_VALUE);
            cutoffmin = getDouble(args, "cutoffmin", 0);
            diffusionLength = getDouble(args, "diffusionLength", null);
            mfggrid = getDouble(args, "mfggrid", null);
            staticizer = args.getArgValue("staticizer", null);
            weakInverter = args.getArgValue("weak-inverter", null);
            smallInverter = args.getArgValue("small-inverter", null);
            lengthUnit = args.getArgValue("lengthUnit", "u");
            hsizes = args.getArgValue("hsizes", "hsizes.out");
            outdir = args.getArgValue("outdir", ".");
            gates = args.getArgValue("gates", null);
            tau = getFloat(args, "tau", 40);
            tdata = new TechnologyData(args);

            final String pStacks = args.getArgValue("pStacks", null);
            final String nStacks = args.getArgValue("nStacks", null);
            if (pStacks != null && nStacks != null) {
                stackMap = new HashMap();
                Jauto.makeStackMap(stackMap, new Integer(DeviceTypes.P_TYPE),
                                   cfp, pStacks, Vdd, GND, cad);
                Jauto.makeStackMap(stackMap, new Integer(DeviceTypes.N_TYPE),
                                   cfp, nStacks, Vdd, GND, cad);
            } else {
                stackMap = null;
            }
            minWidth = getFloat(args, "minGateWidth", Float.NaN);
        }
    }

    /**
     * Invoke Jauto and return the final NetGraph.
     **/
    private static NetGraph runJauto(final CastFileParser cfp,
                                     final CellInterface cell,
                                     final String outdir,
                                     final String outFile,
                                     final Cadencize cad,
                                     final Reader hsizes,
                                     final Option opt) throws Exception {
        final CastDesign design = new CastDesign(cell, Vdd, GND, _RESET, opt.tau, opt.tdata, cfp);
        final JautoMessageCenter jmc = new JautoMessageCenter();
        design.setMessageCenter(jmc);
        final CellType top = design.getTopLevelCell();
        final String[] gates = opt.gates == null ? new String[0] : StringUtil.split(opt.gates, ':');
        final CellType newtop = top.instanceMinimalSubTypes(CastDesign.TRANSISTORS,
                                                            opt.staticizer, opt.weakInverter, opt.smallInverter, gates, cfp);
        GlobalNet.generateGlobalNets(newtop, new ArrayList());
        Jauto.readHalfOperatorSize(hsizes, design, 1);
        Jauto.setHalfOperatorSize(design);
        SizeStaticizers.sizeStaticizers(design);
        Jauto.writeOut(newtop,
                       outdir,
                       cell.getFullyQualifiedType(),
                       outFile,
                       opt.cutoff,
                       opt.cutoffmin,
                       opt.diffusionLength == null ? new Double(opt.tdata.getDefaultDiffusionLength()) : opt.diffusionLength,
                       true,
                       opt.lengthUnit,
                       1,
                       1,
                       opt.mfggrid,
                       null,
                       cfp,
                       null,
                       null,
                       false,
                       opt.stackMap,
                       cad,
                       null,
                       null,
                       null,
                       design,
                       Collections.EMPTY_LIST,
                       (float) opt.tdata.getDefaultLoadCapacitance(),
                       opt.minWidth,
                       true,
                       new PartialExtract.CellPlusMinus(cell.getFullyQualifiedType(), PartialExtract.Info.INCLUDE),
                       opt.tdata.getTechnologyName(),
                       null);
        final CellType ct = design.getCell(cell.getFullyQualifiedType());
        return ct.transistors;
    }

    /**
     * Set the directive block of a given block.
     **/
    private static void setDirective(final BlockInterface block,
                                     final DirectiveBlock newdir) {
        final BlockIterator bi = block.iterator(BlockInterface.DIRECTIVE);
        if (bi.hasNext()) {
            bi.next();
            bi.set(newdir);
        } else {
            bi.add(newdir);
        }
    }

    /**
     * Merge the given directives with the directives of the given block.
     **/
    private static void mergeDirective(final BlockInterface block,
                                       final DirectiveInterface newdir) {
        final DirectiveBlock old = DirectiveUtils.getDirectiveBlock(block);
        final DirectiveBlock merged =
            old == null ? new DirectiveBlock(newdir) :
                          new MergeDirective(old, newdir);
        setDirective(block, merged);
    }

    private static Pair makeHalfOp(final HierName name, final Boolean dir) {
        return new Pair(name, dir);
    }

    private static void addSymmetrize(final DirectiveSource source,
                                      final Pair halfop,
                                      final Integer level) {
        source.definition(DirectiveConstants.SYMMETRIZE,
                          DirectiveConstants.HALFOP_TYPE,
                          halfop, level);
    }

    private static void addShared(final DirectiveSource source,
                                  final Pair halfop,
                                  final Integer group) {
        source.definition(DirectiveConstants.SHARED,
                          DirectiveConstants.HALFOP_TYPE,
                          halfop, group);
    }

    /**
     * Find the set of transistors connected to the collection of nodes, and
     * return the number of transistors, and their total width, 
     **/
    private static Pair getStatistic(final NetGraph graph,
                                     final Collection nodes) {
        int transistors = 0;
        double width = 0;
        final Set edgeSet = new HashSet();
        for (Iterator i = nodes.iterator(); i.hasNext(); ) {
            final HierName name = (HierName) i.next();
            final NetGraph.NetNode node = graph.findNetNode(name);
            assert node != null : "Cannot find " + name + " in NetGraph " + graph;
            for (Iterator j = node.getPaths().iterator(); j.hasNext(); ) {
                final NetGraph.NetPath path = (NetGraph.NetPath) j.next();
                edgeSet.addAll(path.getEdges());
            }
        }
        for (Iterator i = edgeSet.iterator(); i.hasNext(); ) {
            final NetGraph.NetEdge edge = (NetGraph.NetEdge) i.next();
            ++transistors;
            width += edge.width;
        }
        //return new Pair(new Integer(transistors), new Double(width));
        return new Pair(new Integer(transistors), new Integer((int) Math.round(width * 1e10)));
    }
                                
    private static Integer SYM_NONE = new Integer(0);
    private static Integer SYM_PARTIAL = new Integer(1);
    private static Integer SYM_SKIP_FIRST = new Integer(2);
    private static Integer SYM_FULL = new Integer(3);
    private static Integer SYM_TRUNK = new Integer(4);
    private static Integer SYM_FULL_X = new Integer(5);
    private static List symdirs = new ArrayList();
    private static Map symnams = new HashMap();
    static {
        symdirs.add(SYM_PARTIAL);
        symdirs.add(SYM_SKIP_FIRST);
        symdirs.add(SYM_FULL);
        symdirs.add(SYM_FULL_X);
        symnams.put(SYM_NONE, "SYM_NONE");
        symnams.put(SYM_PARTIAL, "SYM_PARTIAL");
        symnams.put(SYM_SKIP_FIRST, "SYM_SKIP_FIRST");
        symnams.put(SYM_FULL, "SYM_FULL");
        symnams.put(SYM_TRUNK, "SYM_TRUNK");
        symnams.put(SYM_FULL_X, "SYM_FULL_X");
    }

    private static void setDirectives(final Sharing[] sharing,
                                      final Integer up,
                                      final Integer dn,
                                      final Map store) {
        final Collection directives = new TreeSet();
        final DirectiveSource source = new DirectiveSource(BlockInterface.PRS);
        for (int i = 0; i < sharing.length; ++i) {
            sharing[i].addSharedDirectives(source, directives);
            sharing[i].addSymmetrizeDirectives(source, directives, up, dn);
        }
        store.put(source, new Pair(directives, sharing));
    }

    private static void getDirectives(final Sharing[] sharing,
                                      final Map result,
                                      final Set badup,
                                      final Set baddn) {
        boolean upbad = false;
        boolean dnbad = false;
        for (int i = 0; i < sharing.length; ++i) {
            for (Iterator j = sharing[i].iterator(); j.hasNext(); ) {
                final Group g = (Group) j.next();
                for (Iterator k = g.iterator(); k.hasNext(); ) {
                    final HierName h = (HierName) k.next();
                    upbad |= badup.contains(h);
                    dnbad |= baddn.contains(h);
                }
            }
        }

        /* If there are charge sharing problems in either direction, then the
         * up direction should be SYM_PARTIAL, because it would also help for
         * the down direction by exposing additional capacitance.  Ask Theron
         * for details. */
        final Integer up = upbad || dnbad ? SYM_PARTIAL : SYM_NONE;

        final List dndirs = dnbad ? symdirs : Collections.EMPTY_LIST;
        /* Loop over the symmetrize directives for down directions */
        for (Iterator j = dndirs.iterator(); j.hasNext(); ) {
            final Integer dn = (Integer) j.next();
            setDirectives(sharing, up, dn, result);
        }

        /* If there are only up problems, then try SYM_PARTIAL; this is
         * exclusive with the above loop */
        if (upbad && !dnbad) {
            setDirectives(sharing, up, null, result);
        }

        if (upbad || dnbad)
            setDirectives(sharing, SYM_FULL_X, SYM_FULL_X, result);
    }
    
    /**
     * Read the section in hsizes.out for a given cell, and return that string,
     * without the "CELL" line as a string.  This is used to reduce I/O.
     **/
    private static String readSizes(final Reader in, final String child, final String parent)
        throws IOException {
        final BufferedReader br = new BufferedReader(in);
        final StringBuffer result = new StringBuffer();
        boolean accum = false;
        String line;
        while ((line = br.readLine()) != null) {
            if (line.startsWith("CELL")) {
                final String[] tokens = StringUtil.tokenize(line);
                assert tokens.length == 3;
                if (tokens[1].equals(child)) {
                    accum = true;
                    line = tokens[0] + " " + parent + " " + tokens[2];
                }
            }
            if (accum) {
                result.append(line);
                result.append("\n");
                if (line.equals("}")) break;
            }
        }
        return result.toString();
    }

    private static int groupId = 0;
    private static int getGroupId(final Map groupMap, final Group group) {
        return ((Integer) ((Pair) groupMap.get(group)).getSecond()).intValue();
    }

    private static void addGroupMap(final Map groupMap,
                                    final Group group,
                                    final Collection directives,
                                    final NetGraph graph) {
        // If this is a new group, create a new AliasedMap
        if (!groupMap.containsKey(group)) {
            groupMap.put(group,
                new Pair(new AliasedMap(
                    new AliasedMap.MergeFunction() {
                        public Object merge(Object o1, Object o2)
                        throws AliasedMap.MergeFailedException {
                            return o1;
                        }
                    },
                    new Comparator() {
                        public int compare(final Object o1, final Object o2) {
                            final Collection c1 = (Collection) o1;
                            final Collection c2 = (Collection) o2;
                            int i = c1.size() - c2.size();
                            if (i != 0) return i;
                            for (Iterator j = c1.iterator(), k = c2.iterator();
                                 j.hasNext(); ) {
                                final String s1 = (String) j.next();
                                final String s2 = (String) k.next();
                                i = s1.compareTo(s2);
                                if (i != 0) return i;
                            }
                            return 0;
                        }
                    }
                ),
                new Integer(groupId++)));
        }

        final GroupInfo newinfo = new GroupInfo(graph, group);

        final AliasedMap map =
            (AliasedMap) ((Pair) groupMap.get(group)).getFirst();
        try {
            map.addData(directives, newinfo);
        } catch (AliasedMap.MergeFailedException e) {
            throw new AssertionError("Merge failed.  Cannot happen!");
        }
        for (Iterator i = map.getCanonicalKeys(); i.hasNext(); ) {
            final Object key = i.next();
            final GroupInfo val = (GroupInfo) map.getValue(key);
            if (!key.equals(directives) && val.equals(newinfo)) {
                try {
                    map.makeEquivalent(key, directives);
                } catch (AliasedMap.MergeFailedException e) {
                    throw new AssertionError("Merge failed.  Cannot happen!");
                }
                break;
            }
        }
    }
    
    private static Map directiveIds = new HashMap();
    private static int lastId = 0;
    private static int getDirectiveId(final Collection directives) {
        if (!directiveIds.containsKey(directives)) {
            directiveIds.put(directives, new Integer(lastId++));
        }
        return ((Integer) directiveIds.get(directives)).intValue();
    }

    private static void outputDirectiveIds(final Writer w) throws IOException {
        for (Iterator i = directiveIds.entrySet().iterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            w.write(entry.getValue() + ":");
            outputDirectives((Collection) entry.getKey(), w);
            w.write("\n");
        }
    }

    private static void tryDirectives(final CastFileParser cfp,
                                      final CellInterface cell,
                                      final Map directives,
                                      final StringReader hin,
                                      final Cadencize cad,
                                      final Map groupMap,
                                      final Writer w,
                                      final Option opt) throws IOException {
        final BlockInterface block = cell.getBlockInterface();
        final BlockInterface prs = block.iterator(BlockInterface.PRS).next();
        final DirectiveBlock original = DirectiveUtils.getDirectiveBlock(prs);
        int id = 0;

        for (Iterator i = directives.entrySet().iterator(); i.hasNext(); ++id) {
            final Map.Entry entry = (Map.Entry) i.next();

            final DirectiveSource source = (DirectiveSource) entry.getKey();
            final DirectiveInterface newdir = source.getDirectiveInterface();
            mergeDirective(prs, newdir);

            final Pair value = (Pair) entry.getValue();
            final Sharing[] sharings = (Sharing[]) value.getSecond();
            final String outFile = Integer.toString(id);
            w.write(outFile + ":");

            final NetGraph transistors;
            try {
                hin.reset();
                transistors = runJauto(cfp, cell, opt.outdir,
                                       outFile.toString(), cad, hin, opt);
            } catch (Exception e) {
                throw new RuntimeException("Cannot generate CDL for charge sharing for cell: " + cell.getFullyQualifiedType(), e);
            }
            final Collection dirs = (Collection) value.getFirst();
            for (int j = 0; j < sharings.length; ++j) {
                for (Iterator k = sharings[j].iterator(); k.hasNext(); ) {
                    final Group group = (Group) k.next();

                    addGroupMap(groupMap, group, dirs, transistors);
                    w.write(getGroupId(groupMap, group) + " " + getDirectiveId(dirs));
                    if (k.hasNext() || j + 1 < sharings.length) w.write(":");
                }
            }
            w.write("\n");
            setDirective(prs, original);
        }
    }

    /**
     * Generate an ordered list of configurations.
     **/
    private static void outputPossibleSharing(final Sharing share,
                                              final Map groupMap,
                                              final Writer w) throws IOException
    {
        int transistors = 0;
        double width = 0;
        for (Iterator i = share.iterator(); i.hasNext(); ) {
            final Group group = (Group) i.next();
            w.write(Integer.toString(getGroupId(groupMap, group)));
            if (i.hasNext()) w.write(" ");
        }
    }

    private static void outputPossibleSharing(final AliasedMap sharing,
                                              final Map groupMap,
                                              final Writer w) throws IOException {
        for (Iterator i = sharing.getCanonicalKeys(); i.hasNext(); ) {
            final HierName key = (HierName) i.next();
            final List sharings = (List) sharing.getValue(key);
            for (Iterator j = sharings.iterator(); j.hasNext(); ) {
                outputPossibleSharing((Sharing) j.next(), groupMap, w);
                if (j.hasNext()) w.write(":");
            }
            w.write("\n");
        }
    }

    private static String outputGroup(final Group group) {
        final StringBuffer buf = new StringBuffer();
        for (Iterator i = group.iterator(); i.hasNext(); ) {
            final HierName name = (HierName) i.next();
            buf.append(" " + name.getCadenceString());
        }
        return buf.toString();
    }

    private static void outputGroup(final Group group, final Writer w) throws IOException {
        for (Iterator i = group.iterator(); i.hasNext(); ) {
            final HierName name = (HierName) i.next();
            w.write(" " + name.getCadenceString());
        }
    }

    private static void outputDirectives(final Collection dir, final Writer w) throws IOException {
        for (Iterator i = dir.iterator(); i.hasNext(); ) {
            final String dirline = (String) i.next();
            w.write(dirline);
            if (i.hasNext()) w.write(":");
        }
    }

    private static void outputStat(final Pair p, final Writer w) throws IOException {
        w.write(p.getFirst() + " " + p.getSecond());
    }

    private static void outputGroups(final Map groupMap,
                                     final Writer w) throws IOException {
        for (Iterator i = groupMap.entrySet().iterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            final Group group = (Group) entry.getKey();
            final Pair p = (Pair) entry.getValue();
            w.write(Integer.toString(getGroupId(groupMap, group)));
            outputGroup(group, w);
            w.write(":");
            final AliasedMap amap = (AliasedMap) p.getFirst();
            for (Iterator j = amap.getCanonicalKeys(); j.hasNext(); ) {
                final Object key = j.next();
                for (Iterator k = amap.getAliases(key); k.hasNext(); ) {
                    w.write(Integer.toString(getDirectiveId((Collection) k.next())));
                    if (k.hasNext()) w.write("=");
                }
                w.write(" ");
                final GroupInfo ginfo = (GroupInfo) amap.getValue(key);
                outputStat(ginfo.stat, w);
                w.write(":");
            }
            w.write("\n");
        }
    }

    /**
     * Create an empty NetGraph, and return it.
     **/
    private static NetGraph createNetGraph(final CellInterface cell,
                                           final Cadencize cad) {
        final CadenceInfo ci = cad.convert(cell);
        final ExclusiveNodeSets exclusives = new ExclusiveNodeSets();
        final AliasedSet namespace = ci.getLocalNodes();
        exclusives.merge(ci.getPortExclusiveNodeSets());
        exclusives.merge(ci.getLocalExclusiveNodeSets());
        final Map nostats = DirectiveUtils.getPrsDirective(cell, DirectiveConstants.NO_STAT, DirectiveConstants.NODE_TYPE);
        final Set nostatnodes = DirectiveUtils.canonize(namespace, DirectiveUtils.getExplicitTrues(nostats));
        return new NetGraph(namespace, exclusives, new ArrayList(), Vdd, GND,
                            nostatnodes);
    }

    private static void process(final CellInterface child, final Set interest,
                                final Set badup, final Set baddn,
                                final Cadencize cad,
                                final CastFileParser cfp,
                                final Option opt) throws IOException {
        final CellInterface cell = child.getDirectRefinementParent();
        shareId = findLargestShareNumber(cell) + 1;
        final NetGraph graph = createNetGraph(cell, cad);
        try {
            graph.addCellInterfacePrs(cell, new NetGraph[0], cfp, cad);
        } catch (Exception e) {
            throw new RuntimeException("Cannot create netgraph from " + cell.getFullyQualifiedType(), e);
        }
        graph.prepareForLvs();
        final AliasedMap sharing = getSharingGroups(graph, interest);

        /* Accumulate nodes that are by themselves.  The only directive to try
         * on them are the symmetrize directives. */
        final List singletons = new ArrayList();

        final Map directives = new HashMap();
        for (Iterator i = sharing.getCanonicalKeys(); i.hasNext(); ) {
            final HierName key = (HierName) i.next();
            final List possible = getPossibleSharing(sharing.getAliases(key));
            sharing.setValue(key, possible);
            if (possible.size() == 1) {
                singletons.add(possible.get(0));
                continue;
            }
            final List config = getConfigurations(possible);
            for (Iterator j = config.iterator(); j.hasNext(); ) {
                final Sharing s = (Sharing) j.next();
                getDirectives(new Sharing[] { s }, directives, badup, baddn);
            }
        }
        getDirectives((Sharing[]) singletons.toArray(new Sharing[0]),
                      directives, badup, baddn);
        final StringReader hin;
        try {
            hin = new StringReader(readSizes(new FileReader(opt.hsizes), child.getFullyQualifiedType(), cell.getFullyQualifiedType()));
        } catch (Exception e) {
            throw new RuntimeException("Cannot read in half operator sizes: " + opt.hsizes, e);
        }
        final Map groupMap = new HashMap();
        Writer w = new FileWriter(new File(opt.outdir, "configs"));
        tryDirectives(cfp, cell, directives, hin, cad, groupMap, w, opt);
        w.close();

        w = new FileWriter(new File(opt.outdir, "all-configs"));
        outputPossibleSharing(sharing, groupMap, w);
        w.close();

        w = new FileWriter(new File(opt.outdir, "groups"));
        outputGroups(groupMap, w);
        w.close();

        w = new FileWriter(new File(opt.outdir, "directives"));
        outputDirectiveIds(w);
        w.close();

        w = new FileWriter(new File(opt.outdir, "parent"));
        w.write(cell.getFullyQualifiedType() + "\n");
        w.close();
    }

    public static void main(String[] args) throws Exception {
        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
        final CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles( parsedArgs ); 

        final CommandLineArgs cachedArgs = 
            new CachingCommandLineArgs( argsWithConfigs );

        final CommandLineArgs theArgs = cachedArgs;

        final String castPath = theArgs.getArgValue("cast-path", ".");
        final String castVersion = theArgs.getArgValue("cast-version", "2");
        final CastFileParser cfp =
            new CastFileParser(new FileSearchPath(castPath), castVersion);

        final String output = theArgs.getArgValue("output", ".");

        final Cadencize cad = new Cadencize(true);
        final BufferedReader fin =
            new BufferedReader(new InputStreamReader(System.in));
        final Option opt = new Option(theArgs, cfp, cad);
        String line;
        while ((line = fin.readLine()) != null) {
            final String[] tokens = StringUtil.tokenize(line);
            final CellInterface ci;
            try {
                ci = cfp.getFullyQualifiedCell(tokens[0]);
            } catch (Exception e) {
                throw new RuntimeException("Cannot load cell " + tokens[0], e);
            }
            final Set interest = new HashSet();
            final Set badup = new HashSet();
            final Set baddn = new HashSet();
            try {
                for (int i = 1; i < tokens.length; ++i) {
                    final char updn = tokens[i].charAt(0);
                    final HierName tok =
                        HierName.makeHierName(tokens[i].substring(1), '.');
                    if (updn == '+') {
                        badup.add(tok);
                    } else if (updn == '-') {
                        baddn.add(tok);
                    } else {
                        throw new RuntimeException("Invalid input: " + line);
                    }
                    interest.add(tok);
                }
            } catch (InvalidHierNameException e) {
                throw new RuntimeException("Cannot convert to HierName", e);
            }
            process(ci, interest, badup, baddn, cad, cfp, opt);
        }
    }
}
