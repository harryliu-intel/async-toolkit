/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.jauto;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import com.avlsi.cast.CastFileParser;
import com.avlsi.cast.CastSyntaxException;
import com.avlsi.cast.CastSemanticException;
import com.avlsi.cell.CellInterface;
import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.BlockIterator;
import com.avlsi.fast.NetlistBlock;
import com.avlsi.file.cdl.parser.CDLSimpleFilter;
import com.avlsi.file.cdl.parser.CDLSimpleImpl;
import com.avlsi.file.cdl.parser.CDLSimpleInterface;
import com.avlsi.file.cdl.parser.Inline;
import com.avlsi.file.cdl.parser.Template;
import com.avlsi.file.common.HierName;
import com.avlsi.io.FileSearchPath;
import com.avlsi.io.SearchPath;
import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.StringContainerIterator;
import com.avlsi.util.text.NumberFormatter;
import com.avlsi.util.text.StringUtil;

import com.avlsi.file.cdl.util.rename.CadenceReverseNameInterface;

/**
 * Class to calculate the distance metric between 2 leaf cells with netlist
 * bodies.  The netlist bodies must have transistors and gates in the exact
 * same order.  Names must also be identical.
 *
 * @author Harry Liu
 **/
public final class NetlistDistance {

    private NetlistDistance() {
        throw new AssertionError();
    }

    /**
     * Interface used to calculate the metric.  This should provide some 
     * modularity should the calculation be changed.
     **/
    public interface DistanceCalculator {
        /**
         * Called when 2 corresponding transistors are found in the cells.
         * @param width1 Width, in meters, of the transistor in the first cell.
         * @param width2 Width, in meters, of the transistor in the second cell.
         **/
        void transistor(final double width1, final double width2);

        /**
         * Called when 2 corresponding gates are found in the cells.
         * @param type Fully qualified type of the gate.
         * @param params1 Parameters to the gate in the first cell.
         * @param params2 Parameters to the gate in the second cell.
         **/
        void gate(final String type, final Map params1, final Map params2);

        /**
         * Called when a mismatch between the two cells are found.  No other
         * calls to the calculator will be made after a mismatch is detected.
         **/
        void mismatch();
    }

    /**
     * The default calculator calculates:
     * <ol>
     * <li> Average absolute difference in transistor widths
     * <li> Maximum absolute difference in transistor widths
     * </ol>.
     * To make things easy, gates are handled as if each parameter to the gate
     * is a transistor.
     **/
    static class DefaultCalculator implements DistanceCalculator {
        private int transistors;
        private double maxWidthDiff;
        private double widthDiffSum;
        private boolean error;

        public DefaultCalculator() {
            this.transistors = 0;
            this.maxWidthDiff = 0;
            this.widthDiffSum = 0;
            this.error = false;
        }

        public void transistor(final double width1, final double width2) {
            ++transistors;
            final double diff = Math.abs(width1 - width2);
            maxWidthDiff = Math.max(maxWidthDiff, diff);
            widthDiffSum += diff;
        }

        public void gate(final String type, final Map params1,
                         final Map params2) {
            for (Iterator i = params1.entrySet().iterator(); i.hasNext(); ) {
                final Map.Entry entry = (Map.Entry) i.next();
                final Double d1 = (Double) entry.getValue();
                final Double d2 = (Double) params2.get(entry.getKey());
                transistor(d1.doubleValue(), d2.doubleValue());
            }
        }

        public void mismatch() {
            error = true;
        }

        /**
         * Returns the result of the calculation.  The first element of the
         * Pair is the average absolute difference in transistor widths, and
         * the second is the maximum absolute difference in transistor widths.
         * If the cells are found to be different, <code>null</code> is
         * returned.
         **/
        public Pair getResult() {
            if (error) {
                return null;
            } else {
                return new Pair(new Double(widthDiffSum / transistors),
                                new Double(maxWidthDiff));
            }
        }
    }

    /**
     * A compiled form of a netlist body.  It is good to avoid constructing the
     * CompiledForm for a cell multiple times, because that is a potentially
     * expensive process.  Namely, the Template kept in the NetlistBlock
     * contains raw tokens, which must be evaluated to get a double.  This
     * evaluation process involves constructing a CastTwoTreeParser, and using
     * the expression rule, which is not very efficient.
     **/
    static class CompiledForm {
        private static class Helper extends CDLSimpleImpl {
            private final Set result;
            public Helper(final Set result) {
                this.result = result;
            }
            public void makeTransistor(HierName name, int type, HierName ns,
                                       HierName nd, HierName ng,
                                       HierName nb, double w, double l) {
                result.add(new Transistor(name, type, ns, nd, ng, nb, w));
            }
            public void makeCall(HierName name, String subName,
                                 HierName[] args, Map parameters) {
                result.add(new Call(name, subName, args, parameters));
            }
        }

        interface Isomorphic {
            /**
             * Indicates whether some other object is isomorphic to this one,
             * given a one-to-one mapping of names.
             *
             * @param o the object with which to compare
             * @param to a mapping of names from the namespace of the current
             * object to the namespace of the other object
             * @param from a mapping of names from the namespace of the other
             * object to the namespace of the current object
             *
             * @return true if this object is isomorphic to <code>o</code>;
             * false otherwise.
             **/
            boolean isIsomorphic(Object o, Map to, Map from);
        }

        /**
         * Determine if two names are equivalent under the given mapping.  If
         * the two names are not defined in the mapping, they are added.  The
         * mapping defines a one-to-one correspondence between names.
         *
         * @param n1 First name.
         * @param n2 Second name.
         * @param to Mapping from n1 to n2.
         * @param from Mapping from n2 to n1.
         * @return true if the names are equivalent and consistent with the
         * given mapping.
         **/
        private static boolean isomorphic(final HierName n1, final HierName n2,
                                          final Map to, final Map from) {
            assert n1 != null && n2 != null;
            if (!to.containsKey(n1) && !from.containsKey(n2)) {
                to.put(n1, n2);
                from.put(n2, n1);
                return true;
            } else if (n2.equals(to.get(n1)) && n1.equals(from.get(n2))) {
                return true;
            } else {
                return false;
            }
        }

        static class Transistor implements Isomorphic {
            private final HierName name, ns, nd, ng, nb;
            private final int type;
            final double w;

            public Transistor(HierName name, int type, HierName ns,
                              HierName nd, HierName ng, HierName nb,
                              double w) {
                this.name = name;
                this.ns = ns;
                this.nd = nd;
                this.ng = ng;
                this.nb = nb;
                this.type = type;
                this.w = w;
            }
            public boolean equals(final Object o) {
                if (o instanceof Transistor) {
                    final Transistor t = (Transistor) o;
                    return name.equals(t.name) &&
                           ns.equals(t.ns) &&
                           nd.equals(t.nd) &&
                           ng.equals(t.ng) &&
                           nb.equals(t.nb) &&
                           type == t.type;
                } else {
                    return false;
                }
            }
            public boolean isIsomorphic(final Object o, final Map to,
                                        final Map from) {
                if (o instanceof Transistor) {
                    final Transistor t = (Transistor) o;
                    return name.equals(t.name) &&
                           isomorphic(ns, t.ns, to, from) &&
                           isomorphic(nd, t.nd, to, from) &&
                           isomorphic(ng, t.ng, to, from) &&
                           isomorphic(nb, t.nb, to, from) &&
                           type == t.type;
                } else {
                    return false;
                }
            }
            public String toString() {
                final StringBuffer buf = new StringBuffer();
                buf.append("M");
                buf.append(name.toString());
                buf.append(" ");
                buf.append(ns.toString());
                buf.append(" ");
                buf.append(ng.toString());
                buf.append(" ");
                buf.append(nd.toString());
                buf.append(" ");
                buf.append(nb.toString());
                buf.append(" ");
                buf.append(type);
                buf.append(" ");
                buf.append("W=");
                buf.append(w);
                return buf.toString();
            }
        }

        static class Call implements Isomorphic {
            private final HierName name;
            final String subName;
            final HierName[] args;
            final Map parameters;
            public Call(HierName name, String subName, HierName[] args,
                        Map parameters) {
                this.name = name;
                this.subName = subName;
                this.args = args;
                this.parameters = parameters;
            }
            public boolean equals(final Object o) {
                if (o instanceof Call) {
                    final Call c = (Call) o;
                    return name.equals(c.name) &&
                           subName.equals(c.subName) &&
                           parameters.keySet().equals(c.parameters.keySet());
                } else {
                    return false;
                }
            }
            public boolean isIsomorphic(final Object o, final Map to,
                                        final Map from) {
                if (o instanceof Call) {
                    final Call c = (Call) o;
                    for (int i = 0; i < args.length; ++i) {
                        if (!isomorphic(args[i], c.args[i], to, from)) {
                            return false;
                        }
                    }
                    return name.equals(c.name) &&
                           subName.equals(c.subName) &&
                           parameters.keySet().equals(c.parameters.keySet());
                } else {
                    return false;
                }
            }
            public String toString() {
                final StringBuffer buf = new StringBuffer();
                buf.append(name.toString());
                buf.append(" ");
                buf.append(parameters.toString());
                buf.append(" ");
                buf.append(subName);
                return buf.toString();
            }
        }

        interface MergePolicy {
            /**
             * Called when 2 corresponding transistors are found in the cells.
             * @param width1 Width, in meters, of the transistor in the first
             * cell.
             * @param width2 Width, in meters, of the transistor in the second
             * cell.
             * @return Width, in meters, of the merged transistor
             **/
            double transistor(final double width1, final double width2);

            /**
             * Called when 2 corresponding gates are found in the cells.
             * @param type Fully qualified type of the gate.
             * @param params1 Parameters to the gate in the first cell.
             * @param params2 Parameters to the gate in the second cell.
             * @return Parameters to the merged gate.
             **/
            Map gate(final String type, final Map params1, final Map params2);
        }

        /**
         * A sorted list of devices.  This ensures that we compare things in
         * the correct order, and do not depend on consistent ordering in
         * netlist blocks.  The compareTo operations of the devices do assume
         * that the name of the devices are consistent, but that is easier to
         * achieve.
         **/
        final SortedSet devices;

        public CompiledForm(final CellInterface cell) {
            this(cell, null);
        }

        private static class DeviceComparator implements Comparator {
            public int compare(final Object o1, final Object o2) {
                if (o1 instanceof Call) {
                    final Call c1 = (Call) o1;
                    if (o2 instanceof Transistor) {
                        return -1;
                    } else {
                        final Call c2 = (Call) o2;
                        return c1.name.compareTo(c2.name);
                    }
                } else {
                    final Transistor t1 = (Transistor) o1;
                    if (o2 instanceof Call) {
                        return 1;
                    } else {
                        final Transistor t2 = (Transistor) o2;
                        return t1.name.compareTo(t2.name);
                    }
                }
            }
        }

        /**
         * Constructs a CompiledForm for a CellInterface.  This is equivalent
         * to <code>CompiledForm(cell, null, cfp)</code>.
         * @param cell The cell to construct from.
         * @param cfp If not null, read gate definitions from it, and inline
         * them.
         * @throws RuntimeException If the cell does not contain a netlist
         * body.
         **/
        public CompiledForm(final CellInterface cell,
                            final CastFileParser cfp) {
            this(cell, null, cfp);
        }

        /**
         * Constructs a CompiledForm for a CellInterface.
         * @param cell The cell to construct from.
         * @param filter A filter that is used modify the input netlist
         * @param cfp If not null, read gate definitions from it, and inline
         * them.
         * @throws RuntimeException If the cell does not contain a netlist
         * body.
         **/
        public CompiledForm(final CellInterface cell,
                            final CDLSimpleFilter filter,
                            final CastFileParser cfp) {
            final NetlistBlock nb =
                (NetlistBlock) cell.getBlockInterface()
                                   .iterator(BlockInterface.NETLIST)
                                   .next();
            if (nb.getCDLTemplate() == null) {
                devices = null;
                throw new RuntimeException("Cell " + cell.getFullyQualifiedType() + " does not have a netlist block");
            } else {
                devices = new TreeSet(new DeviceComparator());
                final Helper helper = new Helper(devices);

                final CDLSimpleInterface inliner;
                if (cfp == null) {
                    inliner = helper;
                } else {
                    inliner = new Inline(false, new Inline.RetrieveFromCast(cfp));
                    ((Inline) inliner).setProxy(helper);
                }

                final CDLSimpleInterface executor;
                if (filter == null) {
                    executor = inliner;
                } else {
                    executor = filter;
                    filter.setOutput(inliner);
                }

                nb.getCDLTemplate().execute(Collections.EMPTY_MAP, executor);
            }
        }

        private CompiledForm() {
            devices = new TreeSet(new DeviceComparator());
        }

        /**
         * Merge two compatible CompiledForm according to a MergePolicy.  The
         * result is undefined if the two CompiledForms are not compatible.
         * @param form1 The first CompiledForm.
         * @param form2 The second CompiledForm.
         * @param policy MergePolicy to control how to merge.
         * @return New merged CompiledForm.
         **/
        public static CompiledForm merge(final CompiledForm form1,
                                         final CompiledForm form2,
                                         final MergePolicy policy) {
            final CompiledForm result = new CompiledForm();
            for (Iterator i = form1.devices.iterator(),
                          j = form2.devices.iterator(); i.hasNext(); ) {
                final Object ii = i.next();
                final Object elem;
                if (ii instanceof Transistor) {
                    final Transistor jj = (Transistor) j.next();
                    final double w =
                        policy.transistor(((Transistor) ii).w, jj.w);
                    elem = new Transistor(jj.name, jj.type, jj.ns, jj.nd, jj.ng,
                                          jj.nb, w);
                                                        
                } else {
                    final Call jj = (Call) j.next();
                    final Map params = policy.gate(jj.subName,
                                                   ((Call) ii).parameters,
                                                   jj.parameters);
                    elem = new Call(jj.name, jj.subName, jj.args, params);
                }
                result.devices.add(elem);
            }
            return result;
        }
    }

    public static CompiledForm getCompiledForm(final CellInterface cell) {
        return getCompiledForm(cell, null);
    }

    /**
     * Convert a CellInterface to its corresponding CompiledForm.
     * @param cell The cell to convert.
     * @param cfp If not null, the file parser to read gate definitions from.
     * @return The compiled form of the CellInterface.
     **/
    public static CompiledForm getCompiledForm(final CellInterface cell,
                                               final CastFileParser cfp) {
        return new CompiledForm(cell, cfp);
    }

    /**
     * Convert a CellInterface to its corresponding CompiledForm.
     * @param cell The cell to convert.
     * @param filter A filter that is used modify the input netlist.
     * @param cfp If not null, the file parser to read gate definitions from.
     * @return The compiled form of the CellInterface.
     **/
    public static CompiledForm getCompiledForm(final CellInterface cell,
                                               final CDLSimpleFilter filter,
                                               final CastFileParser cfp) {
        return new CompiledForm(cell, filter, cfp);
    }

    /**
     * Convert the values in a Map of String to Doubles to a double[].
     **/
    private static double[] gateParameters(final Map parameters) {
        final double[] result = new double[parameters.size()];
        int k = 0;
        for (Iterator i = parameters.values().iterator(); i.hasNext(); ++k) {
            result[k] = ((Double) i.next()).doubleValue();
        }
        return result;
    }

    /**
     * Calculate the distance between 2 cells given their CompiledForms using
     * the given distance calculator.  If the distance to a cell is to be
     * computed multiple times, it would be more efficient to convert the cell
     * to CompiledForm first, and use this form of the call.
     * @param cell1 The first cell.
     * @param cell2 The second cell.
     * @param dc The distance calculator.
     **/
    public static void getDistance(final CompiledForm cell1,
                                   final CompiledForm cell2,
                                   final DistanceCalculator dc) {
        final Map to = new HashMap();
        final Map from = new HashMap();

        final Set devices1 = cell1.devices;
        final Set devices2 = cell2.devices;
        if (devices1.size() != devices2.size()) {
            dc.mismatch();
        } else {
            for (Iterator i = devices1.iterator(), j = devices2.iterator();
                 i.hasNext(); ) { 
                final CompiledForm.Isomorphic ii =
                    (CompiledForm.Isomorphic) i.next();
                final CompiledForm.Isomorphic jj =
                    (CompiledForm.Isomorphic) j.next();
                if (ii.isIsomorphic(jj, to, from)) {
                    if (ii instanceof CompiledForm.Call) {
                        final Map mi = ((CompiledForm.Call) ii).parameters;
                        final Map mj = ((CompiledForm.Call) jj).parameters;
                        final String subName = ((CompiledForm.Call) ii).subName;
                        dc.gate(subName, mi, mj);
                    } else {
                        dc.transistor(((CompiledForm.Transistor) ii).w,
                                      ((CompiledForm.Transistor) jj).w);
                    }
                } else {
                    dc.mismatch();
                    break;
                }
            }
        }
    }

    /**
     * Calculate the distance between 2 cells given their CellInterfaces using
     * the given distance calculator.  The cells are automatically converted to
     * their CompiledForms first.
     * @param cell1 The first cell.
     * @param cell2 The second cell.
     * @param dc The distance calculator.
     **/
    public static void getDistance(final CellInterface cell1,
                                   final CellInterface cell2,
                                   final DistanceCalculator dc) {
        getDistance(getCompiledForm(cell1), getCompiledForm(cell2), dc);
    }

    /**
     * Return the distance between 2 cells given their CellInterfaces using the
     * default distance calculator.  The cells are automatically converted to
     * their CompiledForms first.
     * @param cell1 The first cell.
     * @param cell2 The second cell.
     * @return The distance metric between the 2 cells as a Pair, or
     * <code>null</code> if the cells are incompatible.  The first element of
     * the Pair is the average absolute difference in transistor widths, and
     * the second is the maximum absolute difference in transistor widths.
     **/
    public static Pair getDistance(final CellInterface cell1,
                                   final CellInterface cell2) {
        final DefaultCalculator dc = new DefaultCalculator();
        getDistance(cell1, cell2, dc);
        return dc.getResult();
    }

    private static CellInterface loadCell(final String castPath,
                                          final String cellName,
                                          final String castVersion) 
        throws CastSyntaxException, CastSemanticException, IOException {
        final CastFileParser castParser =
            new CastFileParser(new FileSearchPath(castPath), castVersion);
        return castParser.getFullyQualifiedCell(cellName);
    }
    
    private static void usage() {
        System.err.println("java com.avlsi.tools.jauto.NetlistDistance");
        System.err.println("[ --cast-path=<path> ] (defaults to .)");
        System.err.println("[ --cast-version=[ 1 | 2 ] ] (defaults to 2)");
        System.err.println("--cell=<mod.ule.cell.subtype> (cell to compare)");
        System.err.println("[ --library-cells=<mod.ule.cell.subtype>:... ] (cells to compare against");
        System.err.println("[ --library-cells-file=<file> ] (file listing cells to compare against");
        System.exit(1);
    }

    private static String printField(final String s, final int length,
                                     final boolean right) {
        if (s.length() < length) {
            final String sp = StringUtil.repeatString(" ", length - s.length());
            return right ? sp + s : s + sp;
        } else {
            return s;
        }
    }

    private static String printLeft(final String s, final int length) {
        return printField(s, length, false);
    }

    private static String printRight(final String s, final int length) {
        return printField(s, length, true);
    }

    public static void main(String[] args) throws Exception {
        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl(args);
        final CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles(parsedArgs); 

        final CommandLineArgs cachedArgs = 
            new CachingCommandLineArgs(argsWithConfigs);

        final CommandLineArgs theArgs = cachedArgs;
        final String castPath = theArgs.getArgValue("cast-path", ".");
        final String castVersion = theArgs.getArgValue("cast-version", "2");
        final String cadenceName = theArgs.getArgValue("cell", null);
        final String libraryCells = theArgs.getArgValue("library-cells", null);
        final String libraryFile =
            theArgs.getArgValue("library-cells-file", null);

        final CadenceReverseNameInterface crni = new CadenceReverseNameInterface();
        final String cellName = crni.renameCell(cadenceName);

        if (cellName == null) {
            System.err.println("ERROR: You must specify the cell to compare.");
            usage();
        }

        final SearchPath sp = new FileSearchPath(castPath);
        final CastFileParser cfp = new CastFileParser(sp, castVersion);
        final CellInterface cell = cfp.getFullyQualifiedCell(cellName);
        final Collection library = new ArrayList();

        if (libraryCells != null) {
            final String[] cells = StringUtil.split(libraryCells, ':');
            for (int i = 0; i < cells.length; ++i) {
                library.add(cfp.getFullyQualifiedCell(cells[i]));
            }
        }

        if (libraryFile != null) {
            final BufferedReader fr =
                new BufferedReader(new FileReader(libraryFile));
            String line;
            while ((line = fr.readLine()) != null) {
                CellInterface currCellInterface = null;
                try {
                    currCellInterface = cfp.getFullyQualifiedCell(line);
                }
                catch ( Exception e ) {}
                if ( currCellInterface != null ) {
                    library.add( currCellInterface );
                }
            }
            fr.close();
        }

        for (Iterator i = library.iterator(); i.hasNext(); ) {
            final CellInterface target = (CellInterface) i.next();
            final Pair result = getDistance(cell, target);
            if (result != null) {
                final StringBuffer buf = new StringBuffer();
                double avgW = ((Double) result.getFirst()).doubleValue() * 1e9;
                double maxW = ((Double) result.getSecond()).doubleValue() * 1e9;
                buf.append(printLeft(target.getFullyQualifiedType(), 40));
                buf.append('\t');
                buf.append(printRight(Long.toString(Math.round(avgW)), 10));
                buf.append('\t');
                buf.append(printRight(Long.toString(Math.round(maxW)), 10));
                System.out.println(buf.toString());
            }
        }
    }
}
