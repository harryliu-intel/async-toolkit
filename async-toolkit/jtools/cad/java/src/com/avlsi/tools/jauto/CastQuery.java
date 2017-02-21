/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.jauto;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import antlr.RecognitionException;
import antlr.TokenStreamException;

import com.avlsi.util.container.SortingIterator;


import com.avlsi.util.functions.UnaryAction;
import com.avlsi.util.functions.UnaryPredicate;
import com.avlsi.util.mathexpression.variable.VariableDictionary;
import com.avlsi.util.functions.UnaryFunction;
import com.avlsi.file.cdl.parser.CDLSimpleImpl;
import com.avlsi.file.cdl.parser.CDLInterfaceSimplifier;
import com.avlsi.file.cdl.parser.CDLLexer;
import com.avlsi.file.cdl.parser.CDLstat;

import com.avlsi.cast.CastCacheManager;
import com.avlsi.cast.impl.Environment;
import com.avlsi.cast2.util.DirectiveActionInterface;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cast2.util.DirectiveWalker;
import com.avlsi.cast2.util.NetProperty;
import com.avlsi.cast2.util.StandardParsingOption;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.directive.UnknownDirectiveException;
import com.avlsi.cast2.directive.impl.CastEmitter;
import com.avlsi.cast2.directive.impl.DirectiveEmitter;
import com.avlsi.cast2.directive.impl.DirectiveTable;
import com.avlsi.cell.CellDelay;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;
import com.avlsi.cell.ChildrenFirstCellInterfaceIterator;
import com.avlsi.cell.NoSuchEnvironmentException;
import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.DirectiveBlock;
import com.avlsi.fast.EnvBlock;
import com.avlsi.fast.NetlistAdapter;
import com.avlsi.fast.NetlistBlock;
import com.avlsi.fast.ports.PortDefinition;
import com.avlsi.fast.ports.NodeType;
import com.avlsi.fast.ports.ChannelType;
import com.avlsi.file.cdl.parser.CDLFactoryAdaptor;
import com.avlsi.file.cdl.parser.CDLInlineFactory;
import com.avlsi.file.cdl.parser.Template;
import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.CDLRenameException;
import com.avlsi.file.cdl.util.rename.CadenceNameInterface;
import com.avlsi.file.cdl.util.rename.CadenceReverseNameInterface;
import com.avlsi.file.cdl.util.rename.GDS2NameInterface;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.io.FileSearchPath;
import com.avlsi.prs.ProductionRule;
import com.avlsi.prs.ProductionRuleSet;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cadencize.CadenceInfo;
import com.avlsi.tools.dsim.ExceptionPrettyPrinter;
import com.avlsi.tools.lvs.NetGraph;
import com.avlsi.tools.prs2verilog.verilog.VerilogUtil;
import com.avlsi.util.bool.BooleanExpressionInterface;
import com.avlsi.util.bool.OrBooleanExpressionInterface;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.cmdlineargs.defimpl.PedanticCommandLineArgs;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.AliasedMap;
import com.avlsi.util.container.FilteringIterator;
import com.avlsi.util.container.FlatteningIterator;
import com.avlsi.util.container.MappingIterator;
import com.avlsi.util.container.MultiMap;
import com.avlsi.util.container.MultiSet;
import com.avlsi.util.container.ObjectUtils;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.Triplet;
import com.avlsi.util.functions.BinaryFunction;
import com.avlsi.util.functions.BinaryPredicate;
import com.avlsi.util.functions.NumericPredicate;


import com.avlsi.util.mathexpression.MathExpression;
import com.avlsi.util.mathexpression.MathExpressionFactory;
import com.avlsi.util.mathexpression.NotAConstantValueException;
import com.avlsi.util.mathexpression.impl.MathExpressionFactoryImpl;
import com.avlsi.util.mathexpression.impl.parser.MathExpressionLexer;
import com.avlsi.util.mathexpression.impl.parser.MathExpressionParser;
import com.avlsi.util.mathexpression.variable.Variable;

import com.avlsi.util.mathexpression.variable.VariableDictionaryIterator;
import com.avlsi.util.text.NumberFormatter;
import com.avlsi.util.text.StringUtil;

import com.avlsi.cast.CastFileParser;
import com.avlsi.cast.CastSemanticException;


/**
 * A class that generate various flat or hierarchical statistics about cells in
 * an easily parsable format.  It combines the functionalities of CountPrs,
 * CDLstat, SubtypeQuery and others.
 **/
public final class CastQuery {
    /**
     * This class should not be instantiated.
     **/
    private CastQuery() { }

    public static class RuntimeException extends java.lang.RuntimeException {
        public RuntimeException(final String msg, final Throwable cause) {
            super(msg, cause);
        }
        public RuntimeException(final String msg) {
            super(msg);
        }
        public RuntimeException() {
            super();
        }
    }

    private static void usage( String m ) {
        System.err.print(
            "java com.avlsi.tools.jauto.CastQuery\n" +
            "    --cast-path=<path> (defaults to .)\n" +
            "    --cast-version=[ 1 | 2 ] (defaults to 2)\n" +
            "    --config=<config> (file containing options)\n" +
            "    --cell=<cell> (fully qualified cell name)\n" +
            "    --task=<task,task,...> tasks are:\n" +
            "        subcells (generate a list of cells)\n" +
            "        subcell_tree (generate a tree display)\n" +
            "        tau (print value of the tau directive)\n" +
            "        density (print value of the density directive)\n" +
            "        directive=dir (print value of the specified cell level directive)\n" +
            "        paramdirective=dir (value of the specified cell level parameterized directive)\n" +
            "        instances (print instance count)\n" +
            "        instance_list[ =verilog:skip-wiring ]\n" +
            "            (print pairs of cell name and full instance names, optionally\n" +
            "             using Verilog syntax, or skipping wiring instances)\n" +
            "        prs[ =dnf ] (print production rule count, optionally convert rules\n" +
            "                     to DNF form to count number of disjuncts, like DSim)\n" +
            "        transistors[ =<gate:...> ] (print transistor stats, w/o gates in list)\n" +
            "        dynamic_nodes[ =alias ] (print dynamic nodes, with aliases)\n" +
            "        count_dynamic_nodes (count the number dynamic nodes)\n" +
            "        external_nodes[ =[write]im[plied]:[write]di[rection]:al[iases]:\n" +
            "                         xr[ef]:all[_aliases]:re[alonly] ]\n" +
            "            (implied=>include implied nodes)\n" +
            "            (direction=>include node direction)\n" +
            "            (aliases=>print aliases at the current level)\n" +
            "            (all_aliases=>print all aliases)\n" +
            "            (xref=>cross ref of Canonical names vs cast name)\n" +
            "        local_nodes[ =all_aliases:alias:used:drivers:em ]\n" +
            "            (print local nodes and directives, optionally all aliases,\n" +
            "             or aliases visible at this level, only nodes used, drivers,\n" +
            "             or EM related specs)\n" +
            "        em_spec (print electromigration related directives)\n" +
            "        routing (print number of subcells, local nets, and connections)\n" +
            "        env[ =prs:rte_ignore:ntpc_spec:aspice_ignore ]\n" +
            "            (print all environments, optionally must have prs impl,\n"+
            "             rte_ignore=false, have an ntpc_spec, or aspice_ignore=false)\n" +
            "        refinement_lineage (print out refinement ancestors)\n" +
            "        railstat (print out number of channels and rails)\n" +
            "        attribute_list (print out inheritance ancestors)\n" +
            "        canonical_name[=file[:skill]] (translate node names from stdin [or file]\n" +
            "                               to canonical names, skill format if specified)\n" +
            "        initial_floorplan[ =<regex>]\n" +
            "            (write out a file suitable as input to create an initial\n" +
            "             floorplan, excluding instances whose type match the\n" +
            "             specified regular expression)\n" +
            "        enumerate_scan_chains[=dft | sram]\n" +
            "            (write out scan chains in a cell)\n" +
            "        trace_connection=<fqcn>|<left>|<right>|<generic>[:...]\n" +
            "            (trace chains of specified cells; left and right are ports;\n" +
            "             generic is either true or false, chains of all generic cells\n" +
            "             are not reported)\n" +
            "        level (write out the maximum depth to a leaf cell)\n" +
            "        ports [ =primitive ]\n" +
            "            (write out the port list in CAST order, optionally flatten\n" +
            "             defchans to nodes and e1ofN channels)\n" +
            "    [ --filter=<expression> ] expression is composed of\n" +
            "        <math expression> < | <= | == | >= | > <math expression>\n" +
            "            variables available: prs (production rule count of a cell)\n" +
            "        leaf (true if a cell is a leaf cell)\n" +
            "        fixed (true if a cell, leaf or mid-level, is fixed size)\n" +
            "        netlist (true if a cell has a netlist block)\n" +
            "        routed (true if a cell is routed)\n" +
            "        tau=<double> (true if a cell has specified tau)\n" +
            "        shared=<root> (true if cells are shared with root)\n" +
            "        one-level (true if cell is a local subcell of the top cell)\n" +
            "        env[ =prs:rte_ignore:ntpc_spec ]\n" +
            "            (true if a cell has any environment that is prs implementable, has\n" +
            "             rte_ignore=false, has an ntpc_spec, or has aspice_ignore=false)\n" +
            "        directive=[block:]<name>[:<value>] (true if the directive is defined\n" +
            "             in a comma separated list of specified blocks (cell, subcell,\n" +
            "             prs, env), or if not specified, any block of the cell, optionally\n" +
            "             require it to be set to value)\n" +
            "        graybox-list=<file>\n" +
            "            (true, if a cell is listed in file or if a leaf cell is\n" +
            "             instantiated by a cell not listed in file; file contains a list\n" +
            "             of cells, one per line; the cell can be a refinement or\n" +
            "             inheritance parent, and metaparameters may be omitted)\n" +
            "        ancestor=<cell>\n" +
            "            (true, if a cell or any of its refinement or inheritance\n" +
            "             ancestors refines or inherits the specified cell; metaparameters\n" +
            "             may be omitted to match any parameterization)\n" +
            "        block=< prs | subcells | csp | java | verilog | netlist >\n" +
            "            (true if a cell directly contains the block; in addition, prs and\n" +
            "             subcells require the block be completed (not fragments), and csp\n" +
            "             requires runnable CSP, not just structure and function definitions)\n" +
            "        & (and) | (or) ! (not) () (parenthesis)\n" +
            "    [ --prune=<expression> ] expression same as --filter\n" +
            "    [ --cadence-name ] (cell name is a Cadence name)\n" +
            "    [ --no-recurse ]\n" +
            "    [ --translate=(cadence | gdsII) ] (for hierarchical tasks)\n" +
            "    [ --no-header ] (do not print header for flat tasks)\n" +
            "    [ --instantiation-order ] (run hierarchical tasks in instantiation order)\n" +
            "    [ --output=<file> ] (write result to file instead of standard out)\n" +
            "    [ --routed ] (interpret the routed directives)\n" +
            "    [ --separator=<string> ] (delimiter string between hierarchical tasks)\n" +
            "(see http://internal/tree/sw/cad/doc/specs/misc/CastQuery.txt)\n");
        if (m != null && m.length() > 0) 
            System.err.print ( m );
        System.exit(1);
    }

    private static void usage( ) {
        usage ( null );
    }

    private static void instanceSet(final CellInterface ci, final Set seen,
                                    final Set skip) {
        final ChildrenFirstCellInterfaceIterator iter =
            new ChildrenFirstCellInterfaceIterator( ci );
        while ( iter.hasNext() ) {
            final CellInterface currCell = (CellInterface) iter.next();
            final String subType = currCell.getFullyQualifiedType();
            if ( ( ! skip.contains( subType ) ) &&
                 ( ! seen.contains( subType ) ) &&
                 ( ! CellUtils.isWiring( currCell ) ) ) {
                seen.add( subType );
            }
        }
    }

    /**
     * Generates a set of cell types instantiated by a cell, skipping the given
     * set cells.
     * @param ci CellInterface to generate data from.
     * @param skip Set of fully qualified cell names to skip.
     * @return Set of fully qualified cell names instantiated by a cell.
     **/
    private static Set instanceSet(final CellInterface ci, final Set skip) {
        final Set seen = new TreeSet();
        instanceSet(ci, seen, skip);
        seen.add(ci.getFullyQualifiedType());
        return seen;
    }


    /**
     * Convert double to string.  Use reasonable format.
     **/
    public static String printDouble(final double val) {
        return NumberFormatter.format(val, 4);
    }

    /**
     * An interface to invoke a task.
     **/
    public interface TaskInterface {
        /**
         * Do the task on the given cell, and write the result to the given
         * writer.
         * @param cell CellInterface of the cell.
         * @param w Where to write the result to.
         **/
        void doTask(final CellInterface cell, final Writer w)
            throws IOException;
        /**
         * Is this a flat task?
         * @return true if the task is flat, false otherwise.
         **/
        boolean isFlat();
    }

    public abstract static class FlatTask implements TaskInterface {
        public boolean printHeader = true;
        public abstract void doTask(final CellInterface cell, final Writer w)
            throws IOException;
        public abstract void doTask(final CellInterface cell, final Writer w,
                                    boolean noRecurse) throws IOException;
        protected void header(final String s, final Writer w)
            throws IOException {
            if (printHeader) w.write(s);
        }
        public boolean isFlat() {
            return true;
        }
    }

    public abstract static class HierTask implements TaskInterface {
        public abstract void doTask(final CellInterface cell, final Writer w)
            throws IOException;
        public boolean isFlat() {
            return false;
        }
    }

    private static class BaseAlias implements Iterable<HierName> {
        private Collection<HierName> names = new TreeSet<HierName>();
        private MultiSet subcells = null;

        public void add(final HierName name) {
            names.add(name);
        }

        public void add(final HierName inst, final BaseAlias aliases) {
            if (subcells == null) {
                subcells = new MultiSet(
                    new Comparator<Pair<HierName,BaseAlias>>() {
                        public int compare(Pair<HierName,BaseAlias> p1,
                                           Pair<HierName,BaseAlias> p2) {
                            return p1.getFirst().compareTo(p2.getFirst());
                        }
                    });
            }
            subcells.add(new Pair<HierName,BaseAlias>(inst, aliases));
        }

        public Iterator<HierName> iterator() {
            final Collection<Iterator<HierName>> its =
                new ArrayList<Iterator<HierName>>();
            its.add(names.iterator());
            if (subcells != null) {
                for (Iterator i = subcells.iterator(); i.hasNext(); ) {
                    final Pair<HierName,BaseAlias> p =
                        (Pair<HierName,BaseAlias>) i.next();
                    final HierName prefix = p.getFirst();
                    its.add(new MappingIterator<HierName,HierName>(
                                p.getSecond().iterator(),
                                new UnaryFunction<HierName,HierName>() {
                                    public HierName execute(final HierName h) {
                                        return HierName.append(prefix, h);
                                    }
                                })
                            );
                }
            }
            return new FlatteningIterator<HierName>(its.iterator());
        }
    }

    private static class GetAliases {
        private final Cadencize cad;
        private final Map<String,Map<HierName,BaseAlias>> cache;

        public GetAliases(final Cadencize cad) {
            this.cad = cad;
            this.cache = new HashMap<String,Map<HierName,BaseAlias>>();
        }

        private BaseAlias helper(final CellInterface cell,
                                 final HierName node) {
            final String fqcn = cell.getFullyQualifiedType();
            Map<HierName,BaseAlias> typecache = cache.get(fqcn);
            if (typecache == null) {
                typecache = new HashMap<HierName,BaseAlias>();
                cache.put(fqcn, typecache);
            }

            BaseAlias ba = typecache.get(node);
            if (ba == null) {
                ba = new BaseAlias();
                final AliasedSet nodes = cad.convert(cell).getLocalNodes();
                final Set seen = new HashSet();
                for (Iterator i = nodes.getAliases(node); i.hasNext(); ) {
                    final HierName alias = (HierName) i.next();
                    boolean foundInstance = false;
                    for (Iterator j = cell.getSubcellPairs(); j.hasNext(); ) {
                        final Pair p = (Pair) j.next();
                        final CellInterface subcell =
                            (CellInterface) p.getSecond();
                        if (subcell.isNode() || subcell.isChannel()) continue;
                        final HierName instance = (HierName) p.getFirst();
                        if (alias.isChildOf2(instance)) {
                            final int n = instance.getNumComponents();
                            final HierName port = alias.tail(n);
                            final AliasedSet subnodes =
                                cad.convert(subcell).getLocalNodes();
                            if (seen.add(
                                    new Pair(instance,
                                             subnodes.getCanonicalKey(port)))) {
                                ba.add(instance, helper(subcell, port));
                            }
                            foundInstance = true;
                            break;
                        }
                    }
                    if (!foundInstance) ba.add(alias);
                }
                typecache.put(node, ba);
            }

            return ba;
        }

        /**
         * Return all aliases that exist in all levels of the hierarchy.  The
         * aliases returned are not necessarily in any specific order.
         **/
        public Iterator<HierName> getAliases(final CellInterface cell,
                                             final HierName node) {
            return helper(cell, node).iterator();
        }
    }

    /**
     * Print out the external nodes of the cell.
     **/
    public static class ExternalNodes extends FlatTask {

        private final boolean writeImplied;
        private final boolean writeDirection;
        private final boolean writeAliases;
        private final boolean writeAllAliases;
        private final boolean writeXref;
        private final boolean writeRealOnly;

        public ExternalNodes(final boolean writeImplied,
                             final boolean writeDirection,
                             final boolean writeAliases,
                             final boolean writeAllAliases,
                             final boolean writeXref,
                             final boolean writeRealOnly) {
            this.writeImplied = writeImplied;
            this.writeDirection = writeDirection;
            this.writeAliases = writeAliases;
            this.writeAllAliases = writeAllAliases;
            this.writeXref = writeXref;
            this.writeRealOnly = writeRealOnly;
        }

        public ExternalNodes(final boolean writeImplied,
                             final boolean writeDirection,
                             final boolean writeAliases,
                             final boolean writeAllAliases,
                             final boolean writeXref) {
            this.writeImplied = writeImplied;
            this.writeDirection = writeDirection;
            this.writeAliases = writeAliases;
            this.writeAllAliases = writeAllAliases;
            this.writeXref = writeXref;
            this.writeRealOnly = false;
        }

        public ExternalNodes(final boolean writeImplied,
                             final boolean writeDirection,
                             final boolean writeAliases) {
            this.writeImplied = writeImplied;
            this.writeDirection = writeDirection;
            this.writeAliases = writeAliases;
            this.writeAllAliases = false;
            this.writeXref = false;
            this.writeRealOnly = false;
        }

        public ExternalNodes(final boolean writeImplied,
                             final boolean writeDirection) {
            this.writeImplied = writeImplied;
            this.writeDirection = writeDirection;
            this.writeAliases = false;
            this.writeAllAliases = false;
            this.writeXref = false;
            this.writeRealOnly = false;
        }

        /** Non-recursive by definition **/
        public void doTask(final CellInterface cell, final Writer w,
                           boolean noRecurse)
            throws IOException { doTask(cell, w); }

        /**
         * Just write out the port nodes of the cell
         **/
        public void doTask(final CellInterface cell, final Writer w)
            throws IOException {

            final Map mp = CellUtils.markPorts(cell);

            final Cadencize cad = new Cadencize (true);
            final CadenceInfo cinfo = cad.convert(cell);
            final AliasedMap ports = cinfo.getPortNodes();
            final AliasedSet nodes = cinfo.getLocalNodes();
            final Set seen = new HashSet();
            final GetAliases getAliases = new GetAliases(cad);

            header("External nodes of " + cell.getFullyQualifiedType() + ":\n",
                   w);
            for (Iterator i = new SortingIterator(mp.keySet().iterator());
                 i.hasNext(); ) {
                final String s = (String) i.next();
                final HierName hs;
                try {
                    hs = HierName.makeHierName(s, '.');
                } catch (InvalidHierNameException e) {
                    throw new RuntimeException("Cannot create HierName: " + s,
                                               e);
                }
                final HierName canon = (HierName) ports.getCanonicalKey(hs);
                if (!writeXref && !seen.add(canon)) continue;
                if (!((Boolean) ports.getValue(hs)).booleanValue() && writeRealOnly) continue;
                if (writeImplied || !cell.isImpliedPort(s) ) {
                    if (writeDirection) {
                        switch(Integer.parseInt((mp.get(s)).toString())) {
                        case PortDefinition.IN:
                            w.write(PortDefinition.INSTRING);
                            break;
                        case PortDefinition.OUT:
                            w.write(PortDefinition.OUTSTRING);
                            break;
                        case PortDefinition.INOUT:
                            w.write(PortDefinition.INOUTSTRING);
                            break;
                        default:
                            w.write("?");
                            break;
                        }
                    }
                    if (writeXref) {
                        w.write(s);
                        w.write(" ");
                        w.write(canon.getCadenceString());
                    }
                    else {
                        w.write(canon.getCadenceString());
                        if (writeAliases || writeAllAliases) {
                            for (Iterator j = (writeAllAliases ?
                                        getAliases.getAliases(cell, canon)
                                      : nodes.getAliases(canon));
                                 j.hasNext(); ) {
                                final HierName aliasName = (HierName) j.next();
                                if (!aliasName.equals(canon)) {
                                    w.write("=" + aliasName.getCadenceString());
                                }
                            }
                        }
                    }
                    w.write("\n");
                }
            }
        }
    }
    /**
     * Print out the local nodes in the cell, as well as wirewidth and
     * wirespace directives associated with them.  Optionally output the
     * aliases.
     **/
    public static class LocalNodes extends FlatTask {
        private final CastFileParser cfp;
        private final Cadencize cad;

        /** Output aliases? */
        private final boolean alias;

        /** Output all aliases? */
        private final boolean all_alias;

        /** Output only nodes that are used? */
        private final boolean used;

        /** Output drivers of a net? */
        private final boolean drivers;

        /** Output EM signoff directives */
        private final boolean EMReport;

        /** Default tau if no tau directive is specified in a leaf cell */
        private final float defaultTau;

        /** Cache containing delay information for ports of a cell */
        private final Map<String, Map<HierName, PropagateInfo>> cache;

        /** Output all nodes, include ports? */
        private final boolean includePorts;

        /** output include staticizer nodes? **/
        private final boolean staticizer;        

        /** Set of possible nodes **/
        private final Set nodeSet;
        
        public LocalNodes(final CastFileParser cfp, final Cadencize cad,
                          final boolean alias, final boolean used, 
                          final boolean includePorts, final boolean staticizer,
                          Set nodes) {
            this(cfp, cad, alias, used, false, true, false, includePorts,
                 staticizer, nodes);
        }

        public LocalNodes(final CastFileParser cfp, final Cadencize cad,
                          final boolean alias, final boolean used,
                          final boolean drivers, final boolean EMReport,
                          final boolean all_alias, final boolean includePorts,
                          final boolean staticizer, final Set nodeSet) {
            this(cfp, cad, alias, used, drivers, EMReport, all_alias,
                 includePorts, staticizer, new HashMap(), 100e-12F, nodeSet);
        }

        public LocalNodes(final CastFileParser cfp, final Cadencize cad,
                          final boolean alias, final boolean used,
                          final boolean drivers, final boolean EMReport,
                          final boolean all_alias, final boolean includePorts,
                          final Map cache, final float defaultTau) {
            this(cfp, cad, alias, used, drivers, EMReport, all_alias,
                 includePorts, false, cache, defaultTau, null);
        }

        public LocalNodes(final CastFileParser cfp, final Cadencize cad,
                final boolean alias, final boolean used,
                final boolean drivers, final boolean EMReport,
                final boolean all_alias, final boolean includePorts,
                final boolean staticizer, final Map cache,
                final float defaultTau) {
           this(cfp, cad, alias, used, drivers, EMReport, all_alias,
                includePorts, staticizer, cache, defaultTau, null);
        }
        

        public LocalNodes(final CastFileParser cfp, final Cadencize cad,
                          final boolean alias, final boolean used,
                          final boolean drivers, final boolean EMReport,
                          final boolean all_alias, final boolean includePorts,
                          final boolean staticizer, final Map cache,
                          final float defaultTau, final Set nodeSet) {
            this.cfp = cfp;
            this.cad = cad;
            this.alias = alias;
            this.used = used;
            this.drivers = drivers;
            this.EMReport = EMReport;
            this.all_alias = all_alias;
            this.includePorts = includePorts;
            this.staticizer = staticizer;
            this.cache = (Map<String, Map<HierName, PropagateInfo>>) cache;
            this.defaultTau = defaultTau;
            this.nodeSet = nodeSet;
        }

        private class DriversCallback implements PartialExtract.LeafCallback {
            public Object leaf(final CellInterface cell, final HierName inst,
                               final HierName canon) {
                final Collection result = new HashSet();
                if (!cell.containsNetlist()) return null;
                final NetlistBlock nb =
                    (NetlistBlock) cell.getBlockInterface()
                                       .iterator(BlockInterface.NETLIST)
                                       .next();
                nb.getCDLTemplate().execute(Collections.EMPTY_MAP, new CDLSimpleImpl() {
                    public void makeCall(final HierName name,
                                         final String subName,
                                         final HierName[] args,
                                         final Map parameters) {
                        final CellInterface gate;
                        try {
                            gate = cfp.getFullyQualifiedCell(subName);
                        } catch (CastSemanticException e) {
                            System.err.println("Cannot load cell: " + subName);
                            ExceptionPrettyPrinter.printException(e,
                                                                  System.err);
                            return;
                        }
                        if (((Boolean) DirectiveUtils.getTopLevelDirective(gate, DirectiveConstants.STATICIZER_GATE)).booleanValue()) return;
                        final Collection ports =
                            NetlistAdapter.getParameterList(gate, cad);
                        if (ports.size() != args.length) {
                            System.err.print("Number of gate ports does not match with instantiation: X" + name + " / " + subName);
                            for (int i = 0; i < args.length; ++i) {
                                System.err.print(" " + args[i]);
                            }
                            System.err.println(" Expecting " + ports.size());
                            return;
                        }
                        final Map marked = CellUtils.markPorts(gate);
                        int i = 0;
                        for (Iterator j = ports.iterator(); j.hasNext(); ++i) {
                            final String sport =
                                ((HierName) j.next()).getCadenceString();
                            if (canon.equals(args[i])) {
                                final Integer dir = (Integer) marked.get(sport);
                                assert dir != null;
                                if (dir.intValue() == PortDefinition.OUT ||
                                    dir.intValue() == PortDefinition.INOUT) {
                                    final boolean stack = ((Boolean) DirectiveUtils.getTopLevelDirective(gate, DirectiveConstants.STACK_GATE)).booleanValue();
                                    if (stack) result.add("stack");
                                    else result.add(subName);
                                }
                                break;
                            }
                        }
                    }
                });
                return result.size() == 0 ? null : new Triplet(cell, canon, result);
            }
            public Object midlevel(final Object o, final CellInterface cell,
                                   final HierName inst, final HierName canon) {
                return o;
            }
        }

        private static String printBoolean(final boolean x) {
            return x ? "1" : "0";
        }

        private static float update(boolean top, float current, float other) {
            if (Float.isNaN(current)) {
                return other;
            } else if (Float.isNaN(other)) {
                return current;
            } else {
                return top ? other : Math.max(current, other);
            }
        }

        private static class Signoff {
            public static final Signoff UNSPECIFIED = new Signoff();
            public final float alintSignoff;
            public final float maxBumpFanin;
            public final float upSlew, dnSlew;
            public final float upSkew, dnSkew;
            public final float upBump, dnBump;
            public final float upThreshBump, dnThreshBump;
            public final float upDelay, dnDelay;
            public final float leakage;
            private Signoff() {
                this(Float.NaN, Float.NaN,
                     Float.NaN, Float.NaN,
                     Float.NaN, Float.NaN,
                     Float.NaN, Float.NaN,
                     Float.NaN, Float.NaN,
                     Float.NaN, Float.NaN,
                     Float.NaN);
            }
            private Signoff(float alintSignoff, float maxBumpFanin,
                            float upSlew, float dnSlew,
                            float upSkew, float dnSkew,
                            float upBump, float dnBump,
                            float upThreshBump, float dnThreshBump,
                            float upDelay, float dnDelay,
                            float leakage) {
                this.alintSignoff = alintSignoff;
                this.maxBumpFanin = maxBumpFanin;
                this.upSlew = upSlew;
                this.dnSlew = dnSlew;
                this.upSkew = upSkew;
                this.dnSkew = dnSkew;
                this.upBump = upBump;
                this.dnBump = dnBump;
                this.upThreshBump = upThreshBump;
                this.dnThreshBump = dnThreshBump;
                this.upDelay = upDelay;
                this.dnDelay = dnDelay;
                this.leakage = leakage;
            }
            public static Signoff getInstance(float alintSignoff,
                                              float maxBumpFanin,
                                              float upSlew, float dnSlew,
                                              float upSkew, float dnSkew,
                                              float upBump, float dnBump,
                                              float upThreshBump,
                                              float dnThreshBump,
                                              float upDelay, float dnDelay,
                                              float leakage) {
                if (Float.isNaN(alintSignoff) && Float.isNaN(maxBumpFanin) &&
                    Float.isNaN(upSlew) && Float.isNaN(dnSlew) &&
                    Float.isNaN(upSkew) && Float.isNaN(dnSkew) &&
                    Float.isNaN(upBump) && Float.isNaN(dnBump) &&
                    Float.isNaN(upThreshBump) && Float.isNaN(dnThreshBump) &&
                    Float.isNaN(upDelay) && Float.isNaN(dnDelay) &&
                    Float.isNaN(leakage)) {
                    return UNSPECIFIED;
                } else {
                    return new Signoff(alintSignoff, maxBumpFanin,
                                       upSlew, dnSlew,
                                       upSkew, dnSkew,
                                       upBump, dnBump,
                                       upThreshBump, dnThreshBump,
                                       upDelay, dnDelay,
                                       leakage);
                }
            }

            public Signoff update(Signoff other, boolean top) {
                return getInstance(
                    LocalNodes.update(top, alintSignoff, other.alintSignoff),
                    LocalNodes.update(top, maxBumpFanin, other.maxBumpFanin),
                    LocalNodes.update(top, upSlew, other.upSlew),
                    LocalNodes.update(top, dnSlew, other.dnSlew),
                    LocalNodes.update(top, upSkew, other.upSkew),
                    LocalNodes.update(top, dnSkew, other.dnSkew),
                    LocalNodes.update(top, upBump, other.upBump),
                    LocalNodes.update(top, dnBump, other.dnBump),
                    LocalNodes.update(top, upThreshBump, other.upThreshBump),
                    LocalNodes.update(top, dnThreshBump, other.dnThreshBump),
                    LocalNodes.update(top, upDelay, other.upDelay),
                    LocalNodes.update(top, dnDelay, other.dnDelay),
                    LocalNodes.update(top, leakage, other.leakage)
                    );
            }
        }

        private static class PropagateInfo {
            public final float upPrsDelay;
            public final float dnPrsDelay;
            public final Signoff signoff;

            public PropagateInfo() {
                this(Float.NaN, Float.NaN, Signoff.UNSPECIFIED);
            }

            public PropagateInfo(final float upPrsDelay,
                                 final float dnPrsDelay,
                                 final Signoff signoff) {
                this.upPrsDelay = upPrsDelay;
                this.dnPrsDelay = dnPrsDelay;
                this.signoff = signoff;
            }

            public PropagateInfo update(PropagateInfo other, boolean top) {
                return new PropagateInfo(
                    LocalNodes.update(top, upPrsDelay, other.upPrsDelay),
                    LocalNodes.update(top, dnPrsDelay, other.dnPrsDelay),
                    signoff.update(other.signoff, top));
            }
        }

        private static float undef2NaN(final Map m, final HierName key) {
            final Number val = (Number) m.get(key);
            return val == null ? Float.NaN : val.floatValue();
        }

        private static float useDef(final float val, final float def) {
            return Float.isNaN(val) ? def : val;
        }

        private static float useDef(final float val) {
            return useDef(val, -1);
        }

        /**
         * Update the value associated with key <code>key</code> in map
         * <code>map</code>.  If <code>key</code> does not exist in
         * <code>map</code>, associate <code>val</code> with it.  Otherwise,
         * merge the old value and <code>val</code>, and associate
         * <code>key</code> with a new array that contains the maximum of the
         * new and old values.  This is similar to
         * <code>AliasedMap.MergeFunction</code>.
         **/
        private void update(final Map<HierName,PropagateInfo> map,
                            final HierName key, final PropagateInfo val,
                            final boolean top) {
            assert key != null && val != null;
            final PropagateInfo old = map.get(key);
            map.put(key, old == null ? val : old.update(val, top));
        }

        /**
         * Propagate delay information from leaf cells up.  For the top-level
         * cell, return a map from local nodes to signoff directives.
         * Otherwise, return a map from port nodes to signoff directives.
         **/
        private Map<HierName,PropagateInfo>
        propagateUp(final CellInterface cell, final boolean top) {
            final Map<HierName,PropagateInfo> cached =
                cache.get(cell.getFullyQualifiedType());
            if (cached != null) return cached;

            final Map<HierName,PropagateInfo> result =
                new HashMap<HierName,PropagateInfo>();
            final CadenceInfo cinfo = cad.convert(cell);
            final AliasedSet locals = cinfo.getLocalNodes();
            final AliasedMap ports = cinfo.getPortNodes();
            for (Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
                final Pair p = (Pair) i.next();

                final CellInterface subcell = (CellInterface) p.getSecond();
                final Map<HierName,PropagateInfo> subcellResult =
                    propagateUp(subcell, false);

                // Convert subcell names to canonical names in this cell
                final HierName inst = (HierName) p.getFirst();
                for (Map.Entry<HierName,PropagateInfo> entry :
                        subcellResult.entrySet()) {
                    final HierName key =
                        HierName.prefixName(inst, entry.getKey());
                    final HierName local =
                        (HierName) locals.getCanonicalKey(key);

                    // If local is null, then the name must be a local node of
                    // the subcell, so its information need not be propagated
                    // up any further
                    if (local != null && (top || ports.contains(local))) {
                        update(result, local, entry.getValue(), false);
                    }
                }
            }

            final Float tauF = (Float)
                DirectiveUtils.getTopLevelDirective(cell,
                                                    DirectiveConstants.TAU);
            final float tau = tauF == null ? defaultTau : tauF.floatValue();

            final CellDelay cellDelay = new CellDelay(cell, cad);

            final BlockInterface cb = cell.getBlockInterface();
            final List<BlockInterface> db = Arrays.asList(
                DirectiveUtils.getUniqueBlock(cb, BlockInterface.PRS),
                DirectiveUtils.getUniqueBlock(cb, BlockInterface.SUBCELL)
            );

            final Map alintSignoff = DirectiveUtils.canonizeKey(locals,
                DirectiveUtils.getMultipleBlockDirective
                ( db,
                  DirectiveConstants.ALINT_SIGNOFF,
                  DirectiveConstants.NODE_TYPE ));

            final Map maxBumpFaninMap = DirectiveUtils.canonizeKey(locals,
                DirectiveUtils.getMultipleBlockDirective
                ( db,
                  DirectiveConstants.ALINT_MAX_BUMP_FANIN,
                  DirectiveConstants.NODE_TYPE ));

            final Map delaySignoffMap =
                DirectiveUtils.getMultipleBlockDirective
                ( db,
                  DirectiveConstants.ESTIMATED_DELAY_SIGNOFF,
                  DirectiveConstants.HALFOP_TYPE );

            final Map slewSignoffMap =
                DirectiveUtils.getMultipleBlockDirective
                ( db,
                  DirectiveConstants.SLEW_SIGNOFF,
                  DirectiveConstants.HALFOP_TYPE );

            final Map skewSignoffMap =
                DirectiveUtils.getMultipleBlockDirective
                ( db,
                  DirectiveConstants.SKEW_SIGNOFF,
                  DirectiveConstants.HALFOP_TYPE );

            final Map bumpSignoffMap =
                DirectiveUtils.getMultipleBlockDirective
                ( db,
                  DirectiveConstants.BUMP_SIGNOFF,
                  DirectiveConstants.HALFOP_TYPE );

            final Map threshBumpSignoffMap =
                DirectiveUtils.getMultipleBlockDirective
                ( db,
                  DirectiveConstants.THRESH_BUMP_SIGNOFF,
                  DirectiveConstants.HALFOP_TYPE );

            final Map leakageSignoffMap = DirectiveUtils.canonizeKey(locals,
                DirectiveUtils.getMultipleBlockDirective
                ( db,
                  DirectiveConstants.LEAKAGE_SIGNOFF,
                  DirectiveConstants.NODE_TYPE ));

            final Map delaySignoffUpMap =
                DirectiveUtils.canonizeKey(locals,
                    DirectiveUtils.getUps(delaySignoffMap));
            final Map delaySignoffDnMap =
                DirectiveUtils.canonizeKey(locals,
                    DirectiveUtils.getDowns(delaySignoffMap));
            
            final Map slewSignoffUpMap =
                DirectiveUtils.canonizeKey(locals,
                    DirectiveUtils.getUps(slewSignoffMap));
            final Map slewSignoffDnMap =
                DirectiveUtils.canonizeKey(locals,
                    DirectiveUtils.getDowns(slewSignoffMap));
            
            final Map skewSignoffUpMap =
                DirectiveUtils.canonizeKey(locals,
                    DirectiveUtils.getUps(skewSignoffMap));
            final Map skewSignoffDnMap =
                DirectiveUtils.canonizeKey(locals,
                    DirectiveUtils.getDowns(skewSignoffMap));

            final Map bumpSignoffUpMap =
                DirectiveUtils.canonizeKey(locals,
                    DirectiveUtils.getUps(bumpSignoffMap));
            final Map bumpSignoffDnMap =
                DirectiveUtils.canonizeKey(locals,
                    DirectiveUtils.getDowns(bumpSignoffMap));
                
            final Map threshBumpSignoffUpMap =
                DirectiveUtils.canonizeKey(locals,
                    DirectiveUtils.getUps(threshBumpSignoffMap));
            final Map threshBumpSignoffDnMap =
                DirectiveUtils.canonizeKey(locals,
                    DirectiveUtils.getDowns(threshBumpSignoffMap));
                
            // Find all targets of production rules
            final Set<HierName> prsTarget = new HashSet<HierName>();
            for (Iterator i = cell.getProductionRuleSet().getProductionRules();
                 i.hasNext(); ) {
                final ProductionRule pr = (ProductionRule) i.next();
                final HierName target =
                    (HierName) locals.getCanonicalKey(pr.getTarget());
                prsTarget.add(target);
            }

            // Look at port nodes which are used, since only they can be
            // connected to local nodes of an instantiating cell.  Unless we
            // are examining the top-level cell, then we are interested only in
            // the local nodes.
            for (Iterator i = top ? locals.getCanonicalKeys() :
                                    ports.getCanonicalKeys(); i.hasNext(); ) {
                final HierName port = (HierName) i.next();
                final Boolean aso = (Boolean) alintSignoff.get(port);
                final Signoff signoff = Signoff.getInstance(
                    aso == null ? Float.NaN : (aso ? 1 : 0),
                    undef2NaN(maxBumpFaninMap, port),
                    undef2NaN(slewSignoffUpMap, port),
                    undef2NaN(slewSignoffDnMap, port),
                    undef2NaN(skewSignoffUpMap, port),
                    undef2NaN(skewSignoffDnMap, port),
                    undef2NaN(bumpSignoffUpMap, port),
                    undef2NaN(bumpSignoffDnMap, port),
                    undef2NaN(threshBumpSignoffUpMap, port),
                    undef2NaN(threshBumpSignoffDnMap, port),
                    undef2NaN(delaySignoffUpMap, port),
                    undef2NaN(delaySignoffDnMap, port),
                    undef2NaN(leakageSignoffMap, port));
                final PropagateInfo other = new PropagateInfo(
                    prsTarget.contains(port) ?
                        cellDelay.getDelay(port, true, tau) : Float.NaN,
                    prsTarget.contains(port) ?
                        cellDelay.getDelay(port, false, tau) : Float.NaN,
                    signoff);
                update(result, port, other, top);
            }
            cache.put(cell.getFullyQualifiedType(), result);
            return result;
        }
        private boolean getAlintSignoff(final CellInterface cell,
                                        final HierName canon,
                                        final float nodeSignoff) {
            //sign off all the nodes in the entire cell
            if(((Boolean) DirectiveUtils.getTopLevelDirective
                ( cell,DirectiveConstants.ALINT_IGNORE )).booleanValue())
                return true;
                       
            return nodeSignoff == 1f;
        }
        public static double getDoubleDirective(final Map /*<HierName,Float>*/ delay,
                                                final HierName canon) {
            final Float d = (Float) delay.get(canon);
            return d == null ? -1 : d.doubleValue();
        }

        public static int getIntDirective(final Map /*<HierName,Float>*/ map,
                                          final HierName canon) {
            final Integer i = (Integer) map.get(canon);
            return i == null ? -1 : i.intValue();
        }

        public Map getDelay(final CellInterface cell, final boolean up) {
            final AliasedSet locals = cad.convert(cell).getLocalNodes();

            // Get estimated delay; canonize the directives
            final Map delayMap = CellUtils.isLeaf(cell) ?
                DirectiveUtils.getPrsDirective(cell,
                        DirectiveConstants.ESTIMATED_DELAY,
                        DirectiveConstants.HALFOP_TYPE) :
                DirectiveUtils.getSubcellDirective(cell,
                        DirectiveConstants.ESTIMATED_DELAY,
                        DirectiveConstants.HALFOP_TYPE);
            final Map delayDirMap = DirectiveUtils.canonizeKey(locals,
                    up ? DirectiveUtils.getUps(delayMap)
                       : DirectiveUtils.getDowns(delayMap));

            // Get extra delay in mid-level cells; canonize the directives
            final Map extraMap = CellUtils.isLeaf(cell) ?
                Collections.EMPTY_MAP :
                DirectiveUtils.getSubcellDirective(cell,
                        DirectiveConstants.EXTRA_DELAY,
                        DirectiveConstants.HALFOP_TYPE);
            final Map extraDirMap = DirectiveUtils.canonizeKey(locals,
                    up ? DirectiveUtils.getUps(extraMap)
                       : DirectiveUtils.getDowns(extraMap));

            // Calculate the maximum delay on a net from DSim delays
            final Map<HierName,PropagateInfo> cellDelay =
                propagateUp(cell, true);

            // Combine estimated delay and DSim delays.  Let estimated_delay
            // override DSim delays.
            final Map result = new HashMap();
            final Float tauF = (Float)
                DirectiveUtils.getTopLevelDirective(cell,
                                                    DirectiveConstants.TAU);
            final float tau = tauF == null ? defaultTau : tauF.floatValue();
            for (Map.Entry<HierName,PropagateInfo> entry :
                    cellDelay.entrySet()) {
                final HierName canon = entry.getKey();
                double delay = useDef(up ? entry.getValue().upPrsDelay
                                         : entry.getValue().dnPrsDelay,
                                      0);
                final Float extraDelay = (Float) extraDirMap.get(canon);
                if (extraDelay != null)
                    delay += extraDelay.floatValue() * tau / 100;
                result.put(canon, new Float(delay));
            }
            result.putAll(delayDirMap);

            return result;
        }
        private double getDir(final Map dirs, final HierName node, double old) {
            if (dirs.containsKey(node)) {
                return ((Float) dirs.get(node)).doubleValue();
            } else {
                return old;
            }
        }
        private Set prsUsed(final CellInterface cell, final AliasedSet nodes) {
            final Set result = new HashSet();
            for (Iterator i = cell.getProductionRuleSet().getProductionRules();
                i.hasNext(); ) {
                final ProductionRule prs = (ProductionRule) i.next();
                prs.foreachHierName(new UnaryAction() {
                    public void execute(final Object o) {
                        final HierName canon =
                            (HierName) nodes.getCanonicalKey((HierName) o);
                        assert canon != null;
                        result.add(canon);
                    }
                });
            }
            return result;
        }
        private boolean subcellUsed(final CellInterface cell, HierName name) {
            HierName port = null;
            CellInterface subcell = null;
            while (name != null) {
                subcell = cell.getSubcell(name);
                if (subcell != null) break;
                final HierName suffix =
                    HierName.makeHierName(name.getSuffixString());
                port = port == null ? suffix : HierName.append(suffix, port);
                name = name.getParent();
            }
            assert name != null : "Node " + name + " does not exist in cell " +
                                  cell.getFullyQualifiedType();
            if (port == null) return false;
            else {
                final AliasedMap ports = cad.convert(subcell).getPortNodes();
                final Boolean use = (Boolean) ports.getValue(port);
                return use.booleanValue();
            }
        }
        private boolean subcellUsed(final CellInterface cell,
                                    final Iterator alias) {
            while (alias.hasNext()) {
                final HierName name = (HierName) alias.next();
                if (subcellUsed(cell, name)) return true;
            }
            return false;
        }
        private static final BinaryFunction EMSpecUpdater = new BinaryFunction()
        {
            public Object execute(final Object a, final Object b) {
                final Object[] o1 = (Object[]) a;
                final Object[] o2 = (Object[]) b;
                assert o1.length == o2.length;
                final Object[] result = new Object[o1.length];
                for (int i = 0; i < result.length; ++i) {
                    final Number n1 = (Number) o1[i];
                    final Number n2 = (Number) o2[i];
                    result[i] = n1.doubleValue() > n2.doubleValue() ? n1 : n2;
                }
                return result;
            }
        };
        
        private static HierName toHier(final String s) {
            try {
                return HierName.makeHierName(s, '.');
            } catch (InvalidHierNameException e) {
                throw new RuntimeException("Can't convert to HierName: " + s);
            }
        }

        /** Non-recursive by definition **/
        public void doTask(final CellInterface cell, final Writer w,
                           boolean noRecurse)
            throws IOException { doTask(cell, w); }

        public void doTask(final CellInterface cell, final Writer w)
            throws IOException {

            final CadenceInfo cinfo = cad.convert(cell);
            final AliasedSet nodes = cinfo.getLocalNodes();
            final AliasedMap ports = cinfo.getPortNodes();
            final GetAliases getAliases = new GetAliases(cad);

            final BlockInterface cellBlock = cell.getBlockInterface();

            final Map widthMap = 
                DirectiveUtils.getMultipleBlockDirective
                ( Arrays.asList
                  ( new BlockInterface[]
                      { cellBlock,
                        DirectiveUtils.getUniqueBlock
                        (cellBlock,
                         BlockInterface.SUBCELL) } ),
                  DirectiveConstants.WIREWIDTH,
                  DirectiveConstants.NODE_TYPE );

            final Map spaceMap = 
                DirectiveUtils.getMultipleBlockDirective
                ( Arrays.asList
                  ( new BlockInterface[]
                      { cellBlock,
                        DirectiveUtils.getUniqueBlock
                        (cellBlock,
                         BlockInterface.SUBCELL) } ),
                  DirectiveConstants.WIRESPACE,
                  DirectiveConstants.NODE_TYPE );

            final double widthCell = 
                ( (Float) DirectiveUtils.getTopLevelDirective
                  ( cell,
                    DirectiveConstants.WIREWIDTH ) ).doubleValue();
            final double spaceCell =
                ( (Float) DirectiveUtils.getTopLevelDirective
                  ( cell,
                    DirectiveConstants.WIRESPACE ) ).doubleValue();

            final Map activityFactor;
            if (EMReport) {
                activityFactor =
                    NetProperty.getNodeProperties(
                            new NetProperty.Cache(), cell, cad,
                            NetProperty.WireProperty.getFactory(
                                new NetProperty.WireProperty.EMDirective(
                                    cad,
                                    DirectiveConstants.ACTIVITY_FACTOR,
                                    NetProperty.WireProperty.MAX),
                                NetProperty.WireProperty.MAX));
            } else {
                activityFactor = null;
            }

            final Set dynamicNodes = new HashSet();
            try {
                final Map portNodesCache = new HashMap();
                final Map nodesCache = new HashMap();
                dynamicNodes.addAll(
                    CastStat.getDynamicNodes(cell, cad, cfp, portNodesCache,
                                             nodesCache));
                dynamicNodes.addAll(
                    CastStat.getDynamicPortNodes(cell, cad, cfp, portNodesCache,
                                                 nodesCache));
            } catch (InvalidHierNameException e) {
                throw new RuntimeException("Cannot find dynamic nodes", e);
            }

            final Set prsSet = used ? prsUsed(cell, nodes) : null;
            final Map cellDelayUp = getDelay(cell, true);
            final Map cellDelayDn = getDelay(cell, false);

            final Map driversCache = drivers ? new HashMap() : null;
            final PartialExtract.LeafCallback cb =
                drivers ? new DriversCallback() : null;

            final Map<HierName,PropagateInfo> propagateInfo =
                propagateUp(cell, true);

            final Map<HierName,Integer> portDirs =
                CellUtils.getCanonicalDir(new HashMap<HierName,Integer>(),
                                          cell, nodes);

            final Set<HierName> powerNets = DirectiveUtils.canonize(
                    nodes,
                    DirectiveUtils.getExplicitTrues(
                        DirectiveUtils.getTopLevelDirective(cell,
                            DirectiveConstants.POWER_NET,
                            DirectiveConstants.NODE_TYPE)));
            final Set<HierName> groundNets = DirectiveUtils.canonize(
                    nodes,
                    DirectiveUtils.getExplicitTrues(
                        DirectiveUtils.getTopLevelDirective(cell,
                            DirectiveConstants.GROUND_NET,
                            DirectiveConstants.NODE_TYPE)));

            header("Local nodes of " + cell.getFullyQualifiedType() + ":\n", w);
            final PropagateInfo defSignoff = new PropagateInfo();
            Iterator i = new SortingIterator(nodes.getCanonicalKeys());
            final Iterator<NetGraph.NetNode> sinv_i =
                staticizer ? new NetGraph(cell, cad, cfp).getStaticizerNodes()
                                                         .iterator()
                           : Collections.emptyIterator();
            while (i.hasNext() || sinv_i.hasNext()) {
                HierName canon;
                boolean isStaticizer = false;
                if (i.hasNext()){
                    canon = (HierName) i.next();
                    if (nodeSet!=null && 
                        !nodeSet.contains(canon.toString())) continue;
                    if (!includePorts && ports.getCanonicalKey(canon) != null)
                        continue;
                    if (prsSet != null && !prsSet.contains(canon) &&
                        !subcellUsed(cell, nodes.getAliases(canon))) continue;
                } else {
                    //now process staticizer inverter nodes
                    canon = ((NetGraph.NetNode) sinv_i.next()).getName();
                    isStaticizer = true;
                }
                final PropagateInfo pinfo = propagateInfo.get(canon);
                final Signoff signoff = pinfo == null ? Signoff.UNSPECIFIED
                                                      : pinfo.signoff;

                double width = widthCell, space = spaceCell;
                final double delayUp        = getDoubleDirective(cellDelayUp, canon);
                final double delayDn        = getDoubleDirective(cellDelayDn, canon);
                final double delaySignoffUp = useDef(signoff.upDelay);
                final double delaySignoffDn = useDef(signoff.dnDelay);
                final double slewSignoffUp  = useDef(signoff.upSlew);
                final double slewSignoffDn  = useDef(signoff.dnSlew);
                final double skewSignoffUp  = useDef(signoff.upSkew);
                final double skewSignoffDn  = useDef(signoff.dnSkew);
                final double bumpSignoffUp  = useDef(signoff.upBump);
                final double bumpSignoffDn  = useDef(signoff.dnBump);
                final double threshBumpSignoffUp  = useDef(signoff.upThreshBump);
                final double threshBumpSignoffDn  = useDef(signoff.dnThreshBump);
                final int maxBumpFanin   = (int) useDef(signoff.maxBumpFanin);
                final double leakageSignoff = useDef(signoff.leakage);
                if (getAlintSignoff(cell, canon, useDef(signoff.alintSignoff)))
                {
                    w.write("SIGNOFF ");
                }
                if (!isStaticizer) {
                    w.write(canon.getCadenceString());
                } else {
                    w.write(CellUtils.getCastNodeName(canon).getCadenceString()); 
                }
                for (Iterator j = nodes.getAliases(canon); j!=null && j.hasNext(); ) {
                    final HierName aliasName = (HierName) j.next();
                    if (alias && !all_alias && !aliasName.equals(canon)) {
                        w.write("=" + aliasName.getCadenceString());
                    }
                    width = getDir(widthMap, aliasName, width);
                    space = getDir(spaceMap, aliasName, space);
                }
                if (all_alias) {
                    for (Iterator j = getAliases.getAliases(cell, canon);
                         j!=null && j.hasNext(); ) {
                        final HierName aliasName = (HierName) j.next();
                        if (!aliasName.equals(canon)) {
                            w.write("=" + aliasName.getCadenceString());
                        }
                    }
                }
                w.write(" ");
                w.write(dynamicNodes.contains(canon.toString()) ? "1" : "0");
                w.write(" " + printDouble(delayUp) + " " + printDouble(delayDn));
                w.write(" " + printDouble(delaySignoffUp) + " " + printDouble(delaySignoffDn));
                w.write(" " + printDouble(slewSignoffUp) + " " + printDouble(slewSignoffDn));
                w.write(" " + printDouble(skewSignoffUp) + " " + printDouble(skewSignoffDn));
                w.write(" " + printDouble(bumpSignoffUp) + " " + printDouble(bumpSignoffDn));
                w.write(" " + printDouble(width) + " " + printDouble(space));
                if (driversCache != null) {
                    final Set drvs = PartialExtract.getLeafDrivers(cell, canon, cad, driversCache, cb);
                    for (Iterator j = drvs.iterator(); j.hasNext(); ) {
                        w.write(" ");
                        final Triplet t = (Triplet) j.next();
                        final CellInterface c = (CellInterface) t.getFirst();
                        w.write(c.getFullyQualifiedType() + "/" + t.getSecond() + "/");
                        final Collection coll = (Collection) t.getThird();
                        for (Iterator k = coll.iterator(); k.hasNext(); ) {
                            w.write((String) k.next());
                            if (k.hasNext()) w.write(",");
                        }
                    }
                }
                if (activityFactor != null) {
                    final NetProperty.Property activity = (NetProperty.Property) activityFactor.get(canon);
                    final Number n = activity == null ? null : (Number) activity.getValue();
                    w.write(" " + (n == null ? "-1"
                                             : printDouble(n.doubleValue())));
                }
                w.write(" " + maxBumpFanin);
                
                if (includePorts) {
                    final String dirStr;
                    final Integer portDir = portDirs.get(canon);
                    if (portDir == null) {
                        dirStr = "INTERNAL";
                    } else {
                        switch (portDir) {
                          case PortDefinition.IN: dirStr = "IN"; break;
                          case PortDefinition.OUT: dirStr = "OUT"; break;
                          case PortDefinition.INOUT: dirStr = "INOUT"; break;
                          default: dirStr = "NONE"; break;
                        }
                    }
                    w.write(" " + dirStr);
                }

                w.write(" " + printDouble(threshBumpSignoffUp));
                w.write(" " + printDouble(threshBumpSignoffDn));
                w.write(" " + printDouble(leakageSignoff));
                w.write(" " + printBoolean(isStaticizer));
                w.write(" " + printBoolean(groundNets.contains(canon)));
                w.write(" " + printBoolean(powerNets.contains(canon)));

                w.write("\n");
            }
        }
    }

    /**
     * Print out the directives related to EM on all nodes in the cell.
     **/
    public static class EMSpec extends FlatTask {
        private final CastFileParser cfp;
        private final Cadencize cad;

        public EMSpec(final CastFileParser cfp, final Cadencize cad) {
            this.cfp = cfp;
            this.cad = cad;
        }

        private static final BinaryFunction EMSpecUpdater = new BinaryFunction()
        {
            public Object execute(final Object a, final Object b) {
                final Object[] o1 = (Object[]) a;
                final Object[] o2 = (Object[]) b;
                assert o1.length == o2.length;
                final Object[] result = new Object[o1.length];
                for (int i = 0; i < result.length; ++i) {
                    final Number n1 = (Number) o1[i];
                    final Number n2 = (Number) o2[i];
                    result[i] = n1.doubleValue() > n2.doubleValue() ? n1 : n2;
                }
                return result;
            }
        };

        /** Non-recursive by definition **/
        public void doTask(final CellInterface cell, final Writer w, 
                           boolean noRecurse)
            throws IOException { doTask(cell, w); }

        public void doTask(final CellInterface cell, final Writer w)
            throws IOException {

            final CadenceInfo cinfo = cad.convert(cell);
            final AliasedSet nodes = cinfo.getLocalNodes();
            final AliasedMap ports = cinfo.getPortNodes();

            final Map activityFactor =
                    NetProperty.getNodeProperties(
                            new NetProperty.Cache(), cell, cad,
                            NetProperty.WireProperty.getFactory(
                                new NetProperty.WireProperty.EMDirective(
                                    cad,
                                    DirectiveConstants.ACTIVITY_FACTOR,
                                    NetProperty.WireProperty.MIN),
                                NetProperty.WireProperty.MIN,
                                true));
            final Map unidirSpec =
                    NetProperty.getNodeProperties(
                            new NetProperty.Cache(), cell, cad,
                            NetProperty.WireProperty.getFactory(
                                new NetProperty.WireProperty.EMDirective(
                                    cad,
                                    DirectiveConstants.DC_WIRING_SPEC,
                                    EMSpecUpdater),
                                EMSpecUpdater,
                                true));
            final Map bidirSpec =
                    NetProperty.getNodeProperties(
                            new NetProperty.Cache(), cell, cad,
                            NetProperty.WireProperty.getFactory(
                                new NetProperty.WireProperty.EMDirective(
                                    cad,
                                    DirectiveConstants.AC_WIRING_SPEC,
                                    EMSpecUpdater),
                                EMSpecUpdater,
                                true));

            final Map wireWidth =
                    NetProperty.getNodeProperties(
                            new NetProperty.Cache(), cell, cad,
                            NetProperty.WireProperty.getFactory(
                                new NetProperty.WireProperty.WireDirective(
                                    cad,
                                    DirectiveConstants.WIREWIDTH,
                                    NetProperty.WireProperty.MAX),
                                NetProperty.WireProperty.MAX));

            header("EM spec of " + cell.getFullyQualifiedType() + ":\n", w);
            for (Iterator i = new SortingIterator(nodes.getCanonicalKeys());
                 i.hasNext(); ) {
                final HierName canon = (HierName) i.next();

                final Number activity = (Number) ((NetProperty.Property) activityFactor.get(canon)).getValue();
                final List dc = (List) ((NetProperty.Property) unidirSpec.get(canon)).getValue();
                final List ac = (List) ((NetProperty.Property) bidirSpec.get(canon)).getValue();
                final Number width = (Number) ((NetProperty.Property) wireWidth.get(canon)).getValue();

                if (activity == null && dc == null && ac == null &&
                    width == null) continue;

                w.write(canon.getCadenceString());

                w.write(" ");
                w.write(activity == null ? "-1"
                                         : printDouble(activity.doubleValue()));

                w.write(" ");
                w.write(dc == null ? "-1 -1" : printDouble(((Number) dc.get(0)).doubleValue()) + " " + printDouble(((Number) dc.get(1)).doubleValue()));

                w.write(" ");
                w.write(ac == null ? "-1 -1" : printDouble(((Number) ac.get(0)).doubleValue()) + " " + printDouble(((Number) ac.get(1)).doubleValue()));

                w.write(" ");
                w.write(width == null ? "-1"
                                      : printDouble(width.doubleValue()));
                w.write("\n");
            }
        }
    }

    /**
     * Print out the dynamic nodes in the cell.  Optionally output the aliases.
     **/
    public static class DynamicNodes extends FlatTask {
        private final CastFileParser cfp;
        private final Cadencize cad;
        private final boolean alias, leaky;
        public DynamicNodes(final CastFileParser cfp, final Cadencize cad,
                            final boolean alias, final boolean leaky) {
            this.cfp = cfp;
            this.cad = cad;
            this.alias = alias;
            this.leaky = leaky;
        }
        /**
         * Output all dynamic nodes that are internal to leaf cells.
         **/
        private void leakyDynamic(final CellInterface cell,
                                  final HierName prefix, final Map cache,
                                  final Writer w) throws IOException {
            if (CellUtils.isLeaf(cell)) {
                Set nodes = (Set) cache.get(cell.getFullyQualifiedType());
                final AliasedSet locals = cad.convert(cell).getLocalNodes();
                if (nodes == null) {
                    nodes = new TreeSet();
                    CastStat.getDynamicNodes(cell, cfp, cad, nodes);
                    final AliasedMap ports = cad.convert(cell).getPortNodes();
                    // remove dynamic nodes that are in the port list
                    for (Iterator i = nodes.iterator(); i.hasNext(); ) {
                       if (ports.contains(i.next())) i.remove();
                    }

                    // the leaky directive is used to override the default
                    // behavior of excluding ports
                    final Set leaky =
                        DirectiveUtils.getExplicitTrues(
                            DirectiveUtils.canonizeKey(
                                locals,
                                DirectiveUtils.getPrsDirective(cell,
                                    DirectiveConstants.LEAKY,
                                    DirectiveConstants.NODE_TYPE)));
                    nodes.addAll(leaky);
                    cache.put(cell.getFullyQualifiedType(), nodes);
                }
                for (Iterator i = nodes.iterator(); i.hasNext(); ) {
                    final HierName node = (HierName) i.next();
                    w.write(HierName.prefixName(prefix, node).toString());
                    if (alias) {
                        for (Iterator j = locals.getAliases(node);
                             j.hasNext(); ) {
                            final HierName connected = (HierName) j.next();
                            if (!node.equals(connected))
                                w.write("=" + HierName.prefixName(prefix, connected).toString());
                        }
                    }
                    w.write("\n");
                }
            } else {
                for (Iterator i = new SortingIterator(
                        cell.getSubcellPairs(),
                        new Comparator() {
                            public int compare(Object o1, Object o2) {
                                final HierName h1 =
                                    (HierName) ((Pair) o1).getFirst();
                                final HierName h2 =
                                    (HierName) ((Pair) o2).getFirst();
                                return h1.compareTo(h2);
                            }
                        }); i.hasNext(); ) {
                    final Pair p = (Pair) i.next();
                    final CellInterface subcell = (CellInterface) p.getSecond();
                    if (subcell.isChannel() || subcell.isNode()) continue;
                    final HierName name = (HierName) p.getFirst();
                    leakyDynamic(subcell, HierName.prefixName(prefix, name),
                                 cache, w);
                }
            }
        }
        private void allDynamic(final CellInterface cell,
                                final Writer w) throws IOException {
            final Map port = CellUtils.markPorts(cell);
            // XXX: the two argument version of getDynamicNodes() is actually
            // very inefficient, as it flattens the cell to resolve aliases
            for (Iterator i = CastStat.getDynamicNodes(cell, cfp);
                 i.hasNext(); ) {
                final Pair p = (Pair) i.next();
                final HierName canon = (HierName) p.getFirst();
                w.write(canon.getCadenceString());
                boolean outputNode = false;
                for (Iterator j = (Iterator) p.getSecond(); j.hasNext(); ) {
                    final HierName aliasName = (HierName) j.next();
                    final String aliasStr = aliasName.getCadenceString();
                    if (alias && !canon.equals(aliasName)) {
                        w.write("=" + aliasStr);
                    }
                    final Integer portDir = (Integer) port.get(aliasStr);
                    outputNode |= portDir != null &&
                                  (portDir.intValue() == PortDefinition.OUT ||
                                   portDir.intValue() == PortDefinition.INOUT);
                }
                if (outputNode) w.write(" *");
                w.write("\n");
            }
        }

        /** Non-recursive by definition **/
        public void doTask(final CellInterface cell, final Writer w,
                           boolean noRecurse)
            throws IOException { doTask(cell, w); }

        public void doTask(final CellInterface cell, final Writer w)
            throws IOException {
            header("Dynamic nodes of " + cell.getFullyQualifiedType() + ":\n", w);
            if (leaky) {
                leakyDynamic(cell, null, new HashMap(), w);
            } else {
                allDynamic(cell, w);
            }
        }
    }

    /**
     * Gather transistor statistics: number of transistors, total width, total
     * area.
     **/
    private static class TransistorStat extends HierTask {
        private static class Stat {
            private int number;
            private double width;
            private double area;
            public Stat(final int number, final double width,
                        final double area) {
                this.number = number;
                this.width = width;
                this.area = area;
            }
            public void add(final int number, final double width,
                            final double area) {
                this.number += number;
                this.width += width;
                this.area += area;
            }
            public int getNumber() {
                return number;
            }
            public double getWidth() {
                return width;
            }
            public double getArea() {
                return area;
            }
            public String toString() {
                return Integer.toString(getNumber()) + " " +
                       printDouble(getWidth()) + " " +
                       printDouble(getArea());
            }
        }

        private static class Stats {
            private final Map<String,Stat> stats;
            public Stats() {
                this.stats = new TreeMap<String,Stat>();
            }
            public Stat getStat(final String key) {
                Stat s = stats.get(key);
                if (s == null) {
                    s = new Stat(0, 0, 0);
                    stats.put(key, s);
                }
                return s;
            }
            public void combine(final Stats o) {
                for (Map.Entry<String,Stat> entry : o.stats.entrySet()) {
                    final Stat prev = getStat(entry.getKey());
                    final Stat curr = entry.getValue();
                    prev.add(curr.getNumber(), curr.getWidth(), curr.getArea());
                }
            }
            public Stat total() {
                final Stat result = new Stat(0, 0, 0);
                for (Map.Entry<String,Stat> entry : stats.entrySet()) {
                    final Stat val = entry.getValue();
                    result.add(val.getNumber(), val.getWidth(), val.getArea());
                }
                return result;
            }
            public String toString() {
                final StringBuilder sb = new StringBuilder();
                boolean first = true;
                for (Map.Entry<String,Stat> entry : stats.entrySet()) {
                    if (first) {
                        first = false;
                    } else {
                        sb.append(' ');
                    }
                    sb.append(entry.getKey());
                    sb.append('/');
                    sb.append(entry.getValue());
                }
                return sb.toString();
            }
        }

        private static class Helper extends CDLFactoryAdaptor {
            private final Stats stats;
            public Helper() {
                this.stats = new Stats();
            }
            public void makeTransistor(HierName name, String type,
                                       HierName ns, HierName nd, HierName ng,
                                       HierName nb, CDLLexer.InfoToken w,
                                       CDLLexer.InfoToken l, Map parameters,
                                       Environment env) {
                final double width = CDLInterfaceSimplifier.getValue(w, env);
                final double length = CDLInterfaceSimplifier.getValue(l, env);
                // Skip over floating transistors
                if (width > 0) {
                    stats.getStat(type).add(1, width, length * width);
                }
            }
            public Stats getStats() {
                return stats;
            }
        }

        private static Stats getStats(final CellInterface cell, final Map cache,
                                      final UnaryPredicate<CellInterface> filter,
                                      final CDLInlineFactory.Retriever retr) {
            final String type = cell.getFullyQualifiedType();
            if (cache.containsKey(type)) {
                return (Stats) cache.get(type);
            } else {
                final Stats result = new Stats();
                if (cell.containsNetlist()) {
                    final NetlistBlock nb =
                        (NetlistBlock) cell.getBlockInterface()
                                           .iterator(BlockInterface.NETLIST)
                                           .next();
                    final Helper helper = new Helper();
                    final CDLInlineFactory inliner =
                        new CDLInlineFactory(false, retr);
                    inliner.setProxy(helper);
                    nb.getCDLTemplate().execute(inliner);
                    result.combine(helper.getStats());
                }
                for (Iterator i = CellUtils.filterSubcellPairs(cell, filter);
                     i.hasNext(); ) {
                    final Pair p = (Pair) i.next();
                    final CellInterface subcell = (CellInterface) p.getSecond();
                    final Stats stat = getStats(subcell, cache, filter, retr);
                    result.combine(stat);
                }
                cache.put(type, result);
                return result;
            }
        }

        private final Map cache;
        private final CDLInlineFactory.Retriever retr;
        private final boolean detail;
        private final UnaryPredicate<CellInterface> filter;
        public TransistorStat(final CastFileParser cfp, final Set skip,
                              final boolean detail,
                              final UnaryPredicate<CellInterface> filter) {
            cache = new HashMap();
            retr = new CDLInlineFactory.RetrieveFromCast(cfp) {
                public Template getTemplate(final String subName) {
                    if (skip.contains(subName)) {
                        return null;
                    } else {
                        return super.getTemplate(subName);
                    }
                }
            };
            this.detail = detail;
            this.filter = filter;
        }

        public void doTask(final CellInterface cell, final Writer w)
            throws IOException {
            final Stats stats = getStats(cell, cache, filter, retr);
            if (detail) {
                w.write(stats.toString());
            } else {
                final Stat total = stats.total();
                w.write(total.toString());
            }
        }
    }

    /**
     * Count the number of production rules.  Code adopted from Abe's original
     * CountPrs.
     **/
    private static class PrsCount extends HierTask {
        private static boolean isRealCell(final CellInterface cell) {
             return cell.containsCompletePrs() ||
                    cell.containsCsp() ||
                    cell.containsJava() ||
                    cell.containsCompleteSubcells();
        }
        private static int count(final ProductionRuleSet prsSet,
                                 final boolean useDnf) {
            int result = 0;
            if (useDnf) {
                for (Iterator i = prsSet.getProductionRules(); i.hasNext(); ) {
                    final ProductionRule pr = (ProductionRule) i.next();
                    final BooleanExpressionInterface b = pr.getGuard();
                    final OrBooleanExpressionInterface dnf = b.DNFForm();
                    result += dnf.getDisjuncts().size();
                }
            } else {
                result = prsSet.size();
            }
            return result;
        }

        public static int count(final CellInterface cell, final Map cache,
                                final boolean useDnf) {
            final String type = cell.getFullyQualifiedType();
            if (cache.containsKey(type)) {
                return ((Integer) cache.get(type)).intValue();
            } else {
                int result = count(cell.getProductionRuleSet(), useDnf);
                for (Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
                    final Pair p = (Pair) i.next();
                    final CellInterface subcell = (CellInterface) p.getSecond();
                    if (isRealCell(subcell)) {
                        result += count(subcell, cache, useDnf);
                    }
                }
                cache.put(type, new Integer(result));
                return result;
            }
        }
        private Map cache;
        private final boolean useDnf;

        public PrsCount(final boolean useDnf) {
            cache = new HashMap();
            this.useDnf = useDnf;
        }

        public void doTask(final CellInterface cell, final Writer w)
            throws IOException {
            w.write(Integer.toString(count(cell, cache, useDnf)));
        }
    }


     /**
     * Count the number dynamicNodes for a all down the hierarchy
     *
     **/
    private static class CountDynamicNodes extends HierTask {
        private final CastFileParser cfp;
            
        public int count(final CellInterface cell) {
            int result = 0;
            for (Iterator i = CastStat.getDynamicNodes(cell, cfp);
                 i.hasNext(); ) { result++; i.next(); }
            return result;
        }
        public CountDynamicNodes(final CastFileParser cfp) {
            this.cfp = cfp;
        }
        public void doTask(final CellInterface cell, final Writer w)
            throws IOException {
            w.write(Integer.toString(count(cell)));
        }
    }

    /**
     * Generate instance counts.
     **/
    private static class InstanceCount extends HierTask {
        private Map counts = null;
        private final String cellName;
        private final CastFileParser cfp;
        public InstanceCount(final String cellName, final CastFileParser cfp) {
            this.cellName = cellName;
            this.cfp = cfp;
        }
        private void traverse(final CellInterface cell, final Map count) {
            final String fullName = cell.getFullyQualifiedType();
            if (!count.containsKey(fullName)) {
                count.put(fullName, new int[] { 0 });
            }
            ((int[]) count.get(fullName))[0]++;
            for (Iterator i = cell.getLocalSubcellPairs(); i.hasNext(); ) {
                final Pair pair = (Pair) i.next();
                final CellInterface subcell = (CellInterface) pair.getSecond();
                final String subtype = subcell.getFullyQualifiedType();
                if (CellUtils.isWiring(subcell)) continue;
                traverse(subcell, count);
            }
        }
        public void doTask(final CellInterface cell, final Writer w)
            throws IOException {
            if (counts == null) {
                counts = new HashMap();
                try {
                    traverse(cfp.getFullyQualifiedCell(cellName), counts);
                } catch (Exception e) {
                    throw new RuntimeException("Cannot happen!");
                }
            }
            final int[] c = (int[]) counts.get(cell.getFullyQualifiedType());
            if (c == null) {
                w.write("null");
            } else {
                w.write(Integer.toString(c[0]));
            }
        }
    }

    private static class InstanceList extends FlatTask {
        private final String cellName;
        private final CastFileParser cfp;
        private final boolean verilog;
        private final boolean skipWiring;
        private final CDLNameInterface renamer;
        public InstanceList(final String cellName, final CastFileParser cfp,
                            final boolean verilog, final boolean skipWiring,
                            final CDLNameInterface renamer) {
            this.cellName = cellName;
            this.cfp = cfp;
            this.verilog = verilog;
            this.skipWiring = skipWiring;
            this.renamer = renamer;
        }
        private void traverse(final CellInterface cell, final LinkedList path,
                              final Writer w, boolean noRecurse) 
            throws IOException {
            for (Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
                final Pair p = (Pair) i.next();
                final HierName instance = (HierName) p.getFirst();
                final CellInterface subcell = (CellInterface) p.getSecond();
                if (skipWiring && CellUtils.isWiring(subcell)) continue;
                if (!subcell.isNode() && !subcell.isChannel()) {
                    path.addLast(instance);
                    w.write(translateCell(subcell.getFullyQualifiedType(),
                                          renamer));
                    w.write(" ");
                    writePath(path, w);
                    w.write("\n");
                    if (!noRecurse) {
                        traverse(subcell, path, w, noRecurse);
                    }
                    path.removeLast();
                }
            }
        }
        private void writePath(final LinkedList path, final Writer w)
            throws IOException {
            final StringBuffer buf = verilog ? null : new StringBuffer();
            for (Iterator i = path.iterator(); i.hasNext(); ) {
                final String part = i.next().toString();
                if (verilog) {
                    w.write(VerilogUtil.escapeIfNeeded(part));
                    if (i.hasNext()) w.write('.');
                } else {
                    buf.append(part);
                    if (i.hasNext()) buf.append('.');
                }
            }
            if (!verilog) {
                w.write(translateInstance(buf.toString(), renamer));
            }
        }
        public void doTask(final CellInterface cell, final Writer w,
                           boolean noRecurse)
            throws IOException {
            traverse(cell, new LinkedList(), w, noRecurse);
        }
        public void doTask(final CellInterface cell, final Writer w)
            throws IOException { doTask(cell, w, false); }
    }

    /**
     * Print out top-level ParamDirectives in a cell.
     * only really works with --no-recurse
     **/
    private static class ParamDirectiveValue extends HierTask {
        private final String dir;
        private final String valType;
        public ParamDirectiveValue(final String dir, final String valType) {
            this.dir = dir;
            this.valType = valType;
        }
        public void doTask(final CellInterface cell, final Writer w)
            throws IOException {
            final Map dm = DirectiveUtils.getTopLevelDirective(cell, dir, valType);
            if (dm == null) {
                w.write("null");
            } else {
                // identify the directive
                w.write(dir+":");
                // write the directive parameter and the value
                for (Iterator i = dm.keySet().iterator(); i.hasNext();) {
                    Object key = i.next();
                    w.write(key+"="+dm.get(key));
                    if (i.hasNext()) w.write(":");
                }
            }
        }
    }

    /**
     * Print out top-level directives in a cell.
     **/
    private static class DirectiveValue extends HierTask {
        private final String dir;
        private final String valType;
        public DirectiveValue(final String dir, final String valType) {
            this.dir = dir;
            this.valType = valType;
        }
        public void doTask(final CellInterface cell, final Writer w)
            throws IOException {
            final Object o = DirectiveUtils.getTopLevelDirective(cell, dir);
            if (o == null) {
                w.write("null");
            } else {
                final DirectiveEmitter emitter = CastEmitter.getInstance();
                w.write(emitter.emit(BlockInterface.CELL, valType, o));
            }
        }
    }

    private static class RailStats extends HierTask {
        private final Cadencize cad;
        public RailStats(final Cadencize cad) {
            this.cad = cad;
        }
        private static class MarkPort extends CellUtils.MarkPort {
            private static final Pattern DATARAIL =
                Pattern.compile(".*\\.d(\\[\\d+\\])?");

            public int inputChannels = 0, outputChannels = 0;
            public Set inputRails = new HashSet(), outputRails = new HashSet();
            private final AliasedSet aliases;
            private boolean okay = false;
            public MarkPort(final AliasedSet aliases) {
                this.aliases = aliases;
            }
            protected void mark(final ChannelType channelType,
                                final String name, final int direction) {
                final boolean old = okay;
                okay = true;
                super.mark(channelType, name, direction);
                okay = old;
                if (direction < 0) ++inputChannels;
                else if (direction > 0) ++outputChannels;
            }
            protected void mark(final NodeType nodeType, final String name,
                                final int direction) {
                if (okay && DATARAIL.matcher(name).matches()) {
                    final Object canon;
                    try {
                        canon = aliases.getCanonicalKey(HierName.makeHierName(name, '.'));
                    } catch (InvalidHierNameException e) {
                        throw new RuntimeException("Cannot convert " + name + " to HierName", e);
                    }

                    if (direction < 0) {
                        inputRails.add(canon);
                    } else if (direction > 0) {
                        outputRails.add(canon);
                    }
                }
            }
        }

        public void doTask(final CellInterface cell, final Writer w)
            throws IOException {
            final MarkPort mp = new MarkPort(cad.convert(cell).getLocalNodes());
            mp.mark(cell);
            w.write(mp.inputChannels + " " + mp.outputChannels + " " +
                    mp.inputRails.size() + " " + mp.outputRails.size());
        }
    }

    /**
    /**
     * Print out the routine complexity of a cell: number of subcells, number
     * of local nets, and total number of connections.
     **/
    private static class RoutingComplexity extends HierTask {
        private final Cadencize cad;
        public RoutingComplexity(final Cadencize cad) {
            this.cad = cad;
        }

        private int countKeys(final AliasedSet nodes, final AliasedMap ports) {
            int count = 0;
            for (Iterator i = nodes.getCanonicalKeys(); i.hasNext(); ) {
                Object node = i.next();
                if (ports.getCanonicalKey(node) == null) ++count;
            }
            return count;
        }

        private int getConnections(final AliasedMap ports) {
            int result = 0;
            for (Iterator i = ports.getValues(); i.hasNext(); ) {
                if (((Boolean) i.next()).booleanValue()) ++result;
            }
            return result;
        }

        public void doTask(final CellInterface cell, final Writer w)
            throws IOException {
            final CadenceInfo cinfo = cad.convert(cell);

            int subcells = 0;
            int connections = 0;
            int nets = 0;
            int ports = 0;

            for (Iterator i = cinfo.getPortNodes().getValues();
                 i.hasNext();
                 i.next() ) {
                ++ports;
            }

            if (cell.containsNetlist()) { //leaf cell
                final NetlistBlock nb =
                    (NetlistBlock) cell.getBlockInterface()
                    .iterator(BlockInterface.NETLIST).next();
                
                final Map templateMap = new HashMap();
                templateMap.put(cell.getFullyQualifiedType(),
                                nb.getCDLTemplate());
                final CDLstat.CellStat cellStat = 
                    CDLstat.getCellStat(cell.getFullyQualifiedType(),
                                        templateMap);

                connections = cellStat.getNumConnections();
                subcells = cellStat.instances;
                nets = cellStat.getNumLocalNets();
            } else {
                for (Iterator i = cinfo.getSubcellPairIterator(); i.hasNext(); ) { 
                    final Pair p = (Pair) i.next();
                    final CadenceInfo sinfo = (CadenceInfo) p.getSecond();
                    final AliasedMap sports = sinfo.getPortNodes();
                    connections += getConnections(sports);
                    ++subcells;
                }
                nets = countKeys(cinfo.getLocalNodes(),
                                 cinfo.getPortNodes());

            }
            w.write(Integer.toString(subcells) + " " +
                    Integer.toString(nets) + " " + 
                    Integer.toString(ports) + " " + 
                    Integer.toString(connections));
        }
    }

    /**
     * Do nothing.  Useful for just getting a list of cells.
     **/
    private static class HierNop extends HierTask {
        public HierNop() {
        }
        public void doTask(final CellInterface ci, final Writer w)
            throws IOException {
        }
    }

    /**
     * Print out a list of cells instantiated, in ASCII tree form.
     **/
    private static class SubcellTree extends FlatTask {
        private static void instanceTree(final CellInterface ci,
                                         final String space,
                                         final String header,
                                         final Writer w,
                                         boolean noRecurse) throws IOException {
            final Map seen = new TreeMap();
            for (Iterator i = ci.getLocalSubcellPairs(); i.hasNext(); ) {
                final Pair pair = (Pair) i.next();
                final CellInterface subcell = (CellInterface) pair.getSecond();
                final String subtype = subcell.getFullyQualifiedType();
                if (CellUtils.isWiring(subcell) || seen.containsKey(subtype)) {
                    continue;
                }
                seen.put(subtype, subcell);
            }

            final String type = ci.getFullyQualifiedType();
            w.write(header); w.write(type); w.write("\n");
            if (noRecurse && !space.equals("")) return;
            for (Iterator i = seen.entrySet().iterator(); i.hasNext(); ) {
                final Map.Entry entry = (Map.Entry) i.next();
                final CellInterface subcell = (CellInterface) entry.getValue();
                final String s = space + (i.hasNext() ? "  |" : "   ");
                final String h = space + "  +--";
                w.write(space + "  |");
                w.write("\n");
                instanceTree(subcell, s + "  ", h, w, noRecurse);
            }
        }

        public void doTask(final CellInterface ci, final Writer w, 
                           boolean noRecurse)
            throws IOException {
            header("Subcell tree of " + ci.getFullyQualifiedType() + ":\n", w);
            instanceTree(ci, "", "", w, noRecurse);
        }

        public void doTask(final CellInterface ci, final Writer w)
            throws IOException {
            doTask(ci, w, false);
        }
    }

    /**
     * Print out a list of environments.
     **/
    private static class PrintEnv extends HierTask {
        /**
         * Differ from CellUtils.isWiring in that this only returns true for
         * nodes and channels.
         **/
        private static boolean isWiring(final CellInterface cell) {
            return cell.isChannel() || cell.isNode();
        }
        private static boolean hasPrsImpl(final CellInterface cell) {
            if (cell.containsCompletePrs()) {
                return true;
            } else if (cell.containsCompleteSubcells()) {
                for (Iterator i = cell.getLocalSubcellPairs(); i.hasNext(); ) {
                    final Pair p = (Pair) i.next();
                    final CellInterface subcell = (CellInterface) p.getSecond();
                    if (!isWiring(subcell) && !hasPrsImpl(subcell)) {
                        return false;
                    }
                }
                return true;
            } else {
                return false;
            }
        }

        private static boolean hasNtpcSpec(final CellInterface cell) {
            Map quest =
                DirectiveUtils.getEnvDirective(cell,
                                               DirectiveConstants.NTPC_SPEC,
                                               DirectiveConstants.NODE_TYPE);
            return (quest.size() > 0);
        }

        private static boolean isIgnore(final String dir,
                                        final CellInterface cell,
                                        final CellInterface env) {
            return ((Boolean) DirectiveUtils.getEnvDirective(env, dir)).booleanValue() || ((Boolean) DirectiveUtils.getTopLevelDirective(cell, dir)).booleanValue();
        }

        public static Collection getEnvs(final CellInterface cell,
                                         final boolean prsOnly,
                                         final boolean rte_ignore,
                                         final boolean ntpc_spec,
                                         final boolean aspice_ignore) {
            final Collection results = new ArrayList();
            final EnvBlock env = cell.getEnvironments();
            for (Iterator i = env.getNames(); i.hasNext(); ) {
                final String name = (String) i.next();
                final CellInterface ci;
                try {
                    ci = env.getNamedEnvironment(name);
                } catch (CastSemanticException e) {
                    throw new RuntimeException("Unable to load environment: " +
                                               name, e);
                }
                assert ci != null : "Cannot get environment " + name +
                                    " from " + cell.getFullyQualifiedType();
                if ((!prsOnly || hasPrsImpl(ci)) && 
                    (!rte_ignore || !isIgnore(DirectiveConstants.RTE_IGNORE, cell, ci)) &&
                    (!ntpc_spec || hasNtpcSpec(ci)) &&
                    (!aspice_ignore || !isIgnore(DirectiveConstants.ASPICE_IGNORE, cell, ci))) {
                    results.add(name);
                }
            }
            return results;
        }
        private final boolean prsOnly;
        private final boolean rte_ignore;
        private final boolean ntpc_spec;
        private final boolean aspice_ignore;
        public PrintEnv(final boolean prsOnly, final boolean rte_ignore,
                        final boolean ntpc_spec, final boolean aspice_ignore) {
            this.prsOnly = prsOnly;
            this.rte_ignore = rte_ignore;
            this.ntpc_spec = ntpc_spec;
            this.aspice_ignore = aspice_ignore;
        }
        public void doTask(final CellInterface cell, final Writer w)
            throws IOException {
            for (Iterator i = getEnvs(cell, prsOnly, rte_ignore,
                                      ntpc_spec, aspice_ignore).iterator();
                 i.hasNext(); ) {
                w.write((String) i.next());
                if (i.hasNext()) w.write(',');
            }
        }
    }

    private static class RefinementLineage extends FlatTask {
        private static void followRefinement(final CellInterface ci,
                                             final Writer w) throws IOException
        {
            if (ci != null) {
                final CellInterface parent = ci.getDirectRefinementParent();
                followRefinement(parent, w);
                final String type = ci.getFullyQualifiedType();
                w.write(type + "\n");
            }
        }
        public void doTask(final CellInterface ci, final Writer w, 
                           boolean noRecurse) throws IOException { 
            doTask(ci, w); 
        }

        public void doTask(final CellInterface ci, final Writer w)
            throws IOException {
            header("Lineage of " + ci.getFullyQualifiedType() + ":\n", w);
            followRefinement(ci, w);
        }
    }

    private static class AttributeList extends FlatTask {
        public void doTask(final CellInterface ci, final Writer w, 
                           boolean noRecurse) throws IOException { 
            doTask(ci, w); 
        }

        public void doTask(final CellInterface ci, final Writer w)
            throws IOException {
            header("Attributes of " + ci.getFullyQualifiedType() + ":\n", w);
            final Collection attribs = new LinkedHashSet();
            CellUtils.matchInheritance(ci,
                new UnaryPredicate() {
                    public boolean evaluate(final Object o) {
                        final CellInterface attrib = (CellInterface) o;
                        attribs.add(attrib.getFullyQualifiedType());
                        return false;
                    }
                }
            );
            for (Iterator i = attribs.iterator(); i.hasNext(); )
                w.write(i.next() + "\n");
        }
    }

    private static class CanonicalName extends FlatTask {
        private final Cadencize cad;
        private final Reader input;
        private final CDLNameInterface renamer;
        private final Boolean doskill;
        public CanonicalName(final Cadencize cad, final Reader input,
                            final CDLNameInterface renamer, boolean doskill) {
            this.cad = cad;
            this.input = input;
            this.renamer = renamer;
            this.doskill = doskill;
        }

        public void doTask(final CellInterface ci, final Writer w, 
                           boolean noRecurse) throws IOException { 
            doTask(ci, w); 
        }

        public void doTask(final CellInterface ci, final Writer w)
            throws IOException {
            if( ! doskill )
                header("Canonical mapping for " + ci.getFullyQualifiedType() +
                       ":\n", w);
            final BufferedReader br = new BufferedReader(input);
            final AliasedSet locals = cad.convert(ci).getLocalNodes();
            String node;
            int tablenr=1;
            int linenr=0;
            if (doskill) {
                w.write("(defun NodeCanonicalTable1 ( )\n");
                w.write("  (let (\n");
                w.write("      ( Table ( makeTable `pd nil ) ) )\n");
            }
            while ((node = br.readLine()) != null) {
                final HierName canon;
                try {
                    canon = (HierName) locals.getCanonicalKey(
                                HierName.makeHierName(node, '.'));
                } catch (InvalidHierNameException e) {
                    throw new RuntimeException("Cannot convert " + node +
                                               " to HierName", e);
                }
                final String canonname;
                node=translateNode(node, renamer);
                if(canon==null) canonname="";
                else canonname=translateNode(canon.getAsString('.'), renamer);
                if ( doskill ) {
                    if(canon != null) {
                        if (linenr > 3000) {
                            tablenr++;
                            linenr=0;
                            w.write("    Table ) )\n\n");
                            w.write("(defun NodeCanonicalTable"+tablenr+" ( )\n");
                            w.write("  (let (\n");
                            w.write("      ( Table ( makeTable `pd nil ) ) )\n");
                        }
                        w.write("          ( setarray Table \""+node+"\" \""+canonname+"\" )\n");
                        linenr++;
                   }
                }
                else {
                    w.write(node + "=" + canonname + "\n");
                }
            }
            br.close();
            if (doskill) {
                w.write("    Table ) )\n\n");
                w.write("(defun NodeCanonicalTable ( )\n");
                w.write("  (let ( Key map\n");
                w.write("    ( Table ( makeTable `pd nil ) ) )\n");
                for ( int i = 1; i <= tablenr; i++) {
                    w.write("        map  = NodeCanonicalTable"+i+"( )\n");
                    w.write("        foreach( Key map\n");
                    w.write("            ( setarray Table Key arrayref( map Key ) ) )\n");
                }
                w.write("    Table ) )\n");
            }
        }
    }

    private static class InitialFloorplan extends FlatTask {
        private final CDLNameInterface renamer;
        private final Pattern regex;
        public InitialFloorplan(final CDLNameInterface renamer,
                                final Pattern regex) {
            this.renamer = renamer;
            this.regex = regex;
        }
        public void doTask(final CellInterface ci, final Writer w, 
                           boolean noRecurse) throws IOException { 
            doTask(ci, w); 
        }

        public void doTask(final CellInterface ci, final Writer w)
            throws IOException {
            header("Initial floorplan of " + ci.getFullyQualifiedType() + ":\n", w);
            for (Iterator i = ((com.avlsi.cell.CellImpl) ci).getFloorplanInstances();
                 i.hasNext(); ) {
                final Collection column = (Collection) i.next();
                boolean first = true;
                for (Iterator j = column.iterator(); j.hasNext(); ) {
                    final HierName instance = (HierName) j.next();
                    final CellInterface subcell = ci.getSubcell(instance);
                    if (CellUtils.isWiring(subcell)) continue;
                    if (regex != null) {
                        if (regex.matcher(subcell.getFullyQualifiedType())
                                 .matches()) continue;
                    }

                    if (first) {
                        first = false;
                    } else {
                        w.write(' ');
                    }
                    w.write(translateInstance(instance.toString(), renamer));
                }
                if (!first) w.write('\n');
            }
        }
    }

    private static class EnumerateScanChain extends FlatTask {
        private class ScanType {
            private final UnaryPredicate matcher;
            private final HierName leftPort, rightPort;
            private final boolean generic;
            public ScanType(final String typeName,
                            final HierName leftPort,
                            final HierName rightPort,
                            final boolean generic) {
                this.leftPort = leftPort;
                this.rightPort = rightPort;
                this.matcher =
                    CellUtils.getTypeMatcher(Collections.singleton(typeName));
                this.generic = generic;
            }
            public HierName getLeftPort() {
                return leftPort;
            }
            public HierName getRightPort() {
                return rightPort;
            }
            public boolean matches(final CellInterface cell) {
                return matcher.evaluate(cell);
            }
            public boolean isGeneric() {
                return generic;
            }
        }
        private class ScanBuf {
            private final HierName instanceName;
            private final CellInterface scanCell;
            private HierName leftName, rightName;
            private final boolean generic;
            public ScanBuf(final HierName instanceName,
                           final HierName leftName,
                           final HierName rightName,
                           final CellInterface scanCell,
                           final boolean generic) {
                this.instanceName = instanceName;
                this.scanCell = scanCell;
                this.leftName = leftName;
                this.rightName = rightName;
                this.generic = generic;
            }
            public CellInterface getCell() {
                return scanCell;
            }
            public HierName getInstance() {
                return instanceName;
            }
            public HierName getLeftName() {
                return leftName;
            }
            public HierName getRightName() {
                return rightName;
            }
            public String toString() {
                return getInstance() + " " +
                       getCell().getFullyQualifiedType() + " " +
                       getLeftName() + " " +
                       getRightName();
            }
            public boolean isGeneric() {
                return generic;
            }
        }
        private class ScanChain {
            private final LinkedList<ScanBuf> scanBufs;
            boolean generic;
            public ScanChain(final ScanBuf scanBuf) {
                this.scanBufs = new LinkedList<ScanBuf>();
                scanBufs.add(scanBuf);
                this.generic = scanBuf.isGeneric();
            }
            public boolean catChain(final ScanChain chain) {
                if (getLeftName().equals(chain.getRightName())) {
                    scanBufs.addAll(0, chain.scanBufs);
                    generic &= chain.isGeneric();
                    return true;
                } else if (getRightName().equals(chain.getLeftName())) {
                    scanBufs.addAll(chain.scanBufs);
                    generic &= chain.isGeneric();
                    return true;
                } else {
                    return false;
                }
            }
            public HierName getLeftName() {
                return scanBufs.getFirst().getLeftName();
            }
            public HierName getRightName() {
                return scanBufs.getLast().getRightName();
            }
            public boolean isGeneric() {
                return generic;
            }
            public String toString() {
                final StringBuffer buf = new StringBuffer();
                for (ScanBuf scanBuf : scanBufs) {
                    buf.append(scanBuf.toString());
                    buf.append('\n');
                }
                return buf.toString();
            }
        }
        private final Cadencize cad;
        private final Set skipSet;
        private final List<ScanType> scanTypes;
        private final List<ScanBuf> scanBufs;
        private final Map<HierName,ScanChain> scanChains;
        public EnumerateScanChain(final Object[] baseTypes,
                                  final Cadencize cad) {
            this.scanTypes = new ArrayList<ScanType>();
            try {
                for (int i = 0; i < baseTypes.length; i += 4) {
                    final String typeName = (String) baseTypes[i];
                    final HierName leftPort =
                        HierName.makeHierName((String) baseTypes[i + 1], '.');
                    final HierName rightPort =
                        HierName.makeHierName((String) baseTypes[i + 2], '.');
                    final Boolean generic = (Boolean) baseTypes[i + 3];
                    scanTypes.add(new ScanType(typeName, leftPort, rightPort,
                                               generic.booleanValue()));
                }
            } catch (InvalidHierNameException e) {
                throw new RuntimeException("Cannot create HierName", e);
            }
            this.cad = cad;
            this.skipSet = new HashSet();
            this.scanBufs = new ArrayList<ScanBuf>();
            this.scanChains = new HashMap<HierName,ScanChain>();
        }
        public void doTask(final CellInterface ci, final Writer w, 
                           boolean noRecurse) throws IOException { 
            doTask(ci, w); 
        }
        private HierName resolveName(final List path, final HierName portName) {
            HierName name = portName;
            CellInterface parent = null;
            Pair curr = null, prev = null;
            HierName left = null;
            for (Iterator i = path.iterator(); i.hasNext(); ) {
                prev = curr;
                curr = (Pair) i.next();
                if (prev == null) continue;

                final HierName instance = (HierName) prev.getFirst();
                final CellInterface cell = (CellInterface) curr.getSecond();
                if (left != null) {
                    left = HierName.append(instance, left);
                } else {
                    final AliasedSet aliases =
                        cad.convert(cell).getLocalNodes();
                    name = HierName.append(instance, name);
                    final HierName canon =
                        (HierName) aliases.getCanonicalKey(name);
                    if (canon == null) {
                        left = instance;
                    } else {
                        name = canon;
                    }
                }
            }
            return HierName.append(left, name);
        }
        private boolean processCell(final LinkedList path,
                                    final HierName fullName) {
            final Pair last = (Pair) path.getFirst();
            final CellInterface cell = (CellInterface) last.getSecond();
            if (skipSet.contains(cell.getFullyQualifiedType())) return false;

            boolean found = false;
            final Set processed = new HashSet();
            for (Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
                final Pair p = (Pair) i.next();
                final CellInterface subcell = (CellInterface) p.getSecond();
                if (subcell.isNode() || subcell.isChannel() ||
                    skipSet.contains(subcell.getFullyQualifiedType())) continue;
                path.addFirst(p);
                final HierName instance = (HierName) p.getFirst();
                final HierName newName = HierName.append(fullName, instance);
                for (ScanType scanType : scanTypes) {
                    if (scanType.matches(subcell)) {
                        found = true;
                        processed.add(instance);
                        final HierName leftName =
                            resolveName(path, scanType.getLeftPort());
                        final HierName rightName =
                            resolveName(path, scanType.getRightPort());
                        scanBufs.add(new ScanBuf(newName, leftName, rightName,
                                                 subcell,
                                                 scanType.isGeneric()));
                        break;
                    }
                }
                path.removeFirst();
            }

NextPair:   for (Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
                final Pair p = (Pair) i.next();
                final CellInterface subcell = (CellInterface) p.getSecond();
                if (subcell.isNode() || subcell.isChannel() ||
                    skipSet.contains(subcell.getFullyQualifiedType())) continue;

                final HierName instance = (HierName) p.getFirst();
                if (processed.contains(instance)) continue;
                for (Iterator j = processed.iterator(); j.hasNext(); ) {
                    if (instance.isChildOf((HierName) j.next()))
                        continue NextPair;
                }
                path.addFirst(p);
                final HierName newName = HierName.append(fullName, instance);
                found |= processCell(path, newName);
                path.removeFirst();
            }
            if (!found) skipSet.add(cell.getFullyQualifiedType());
            return found;
        }
        private void makeChains() {
            for (ScanBuf buf : scanBufs) {
                final ScanChain chain = new ScanChain(buf);
                final ScanChain l = scanChains.remove(chain.getLeftName());
                final ScanChain r = scanChains.remove(chain.getRightName());
                if (l != null) chain.catChain(l);
                if (r != null) chain.catChain(r);
                scanChains.put(chain.getLeftName(), chain);
                scanChains.put(chain.getRightName(), chain);
            }
        }
        public void doTask(final CellInterface ci, final Writer w)
            throws IOException {
            header("Tracing chains in " + ci.getFullyQualifiedType() + ":\n", w);
            final LinkedList path = new LinkedList();
            path.add(new Pair(null, ci));
            processCell(path, null);
            makeChains();
            for (ScanChain chain : new HashSet<ScanChain>(scanChains.values()))
            {
                if (!chain.isGeneric()) {
                    w.write(chain.toString());
                    w.write('\n');
                }
            }
        }
    }

    private static class HierarchyLevel extends HierTask {
        final Map cache;
        public HierarchyLevel() {
            cache = new HashMap();
        }
        private int getLevel(final CellInterface ci) {
            final Integer cached =
                (Integer) cache.get(ci.getFullyQualifiedType());
            if (cached != null) return cached.intValue();
            int level = -1;
            for (Iterator i = ci.getSubcellPairs(); i.hasNext(); ) {
                final Pair p = (Pair) i.next();
                final CellInterface subcell = (CellInterface) p.getSecond();
                if (!CellUtils.isWiring(subcell)) {
                    level = Math.max(level, getLevel(subcell));
                }
            }
            level++;
            cache.put(ci.getFullyQualifiedType(), new Integer(level));
            return level;
        }
        public void doTask(final CellInterface ci, final Writer w)
            throws IOException {
            w.write(Integer.toString(getLevel(ci)));
        }
    }

    private static class PortList extends FlatTask {
        private final boolean primitive;
        public PortList(final boolean primitive) {
            this.primitive = primitive;
        }
        public void doTask(final CellInterface ci, final Writer w, 
                           boolean noRecurse) throws IOException { 
            doTask(ci, w); 
        }
        public void doTask(final CellInterface ci, final Writer w)
            throws IOException {
            for (Iterator i = ci.getPortDefinitions(); i.hasNext(); ) {
                final PortDefinition port = (PortDefinition) i.next();
                final String implied =
                    ci.isImpliedPort(port.getName()) ? "1 " : "0 ";
                Iterator<PortDefinition> j =
                    Collections.singletonList(port).iterator();
                if (primitive) {
                    j = CellUtils.flattenPorts(j,
                                               CellUtils.WIDE_CHANNEL_TYPE |
                                               CellUtils.CHANNEL_TYPE |
                                               CellUtils.WIDE_NODE_TYPE |
                                               CellUtils.NODE_TYPE);
                }
                while (j.hasNext()) {
                    w.write(implied);
                    w.write(SubtypeOutput.portString(j.next(), true) + "\n");
                }
            }
        }
    }

    /**
     * Map of static predicates that depend only on the CellInterface.
     **/
    private static final Map predicateMap = new HashMap();

    static {
        predicateMap.put("leaf", new UnaryPredicate() {
            public boolean evaluate(Object a) {
                final CellInterface x = (CellInterface) a;
                return CellUtils.isLeaf(x);
            }
        });
        predicateMap.put("fixed", new UnaryPredicate() {
            public boolean evaluate(Object a) {
                final CellInterface x = (CellInterface) a;
                return CellUtils.isFixed(x);
            }
        });
        predicateMap.put("netlist", new UnaryPredicate() {
            public boolean evaluate(Object a) {
                final CellInterface x = (CellInterface) a;
                return x.containsNetlist();
            }
        });
        predicateMap.put("routed", new UnaryPredicate() {
            public boolean evaluate(Object a) {
                final CellInterface x = (CellInterface) a;
                return CellUtils.isRouted(x);
            }
        });
    }

    /**
     * Returns an appropriate renamer given the target name, or null if the
     * translation scheme is unknown.
     **/
    private static CDLNameInterface getRenamer(final String scheme) {
        final CDLNameInterface result;
        if (scheme.equals("gdsII") || scheme.equals("gds2")) {
            result = new GDS2NameInterface();
        } else if (scheme.equals("cadence")) {
            result = new CadenceNameInterface();
        } else {
            result = null;
        }
        return result;
    }

    private static class MathFunction implements UnaryFunction {
        private final Map dictCache;
        private final MathExpression expr;
        private final MathExpressionFactory factory;
        public MathFunction(final Map dictCache, final MathExpression expr,
                            final MathExpressionFactory factory) {
            this.dictCache = dictCache;
            this.expr = expr;
            this.factory = factory;
        }
        public Object execute(final Object o) {
            final CellInterface x = (CellInterface) o;
            final String type = x.getFullyQualifiedType();
            if (!dictCache.containsKey(type)) {
                dictCache.put(type, new AttributeDictionary(x, factory));
            }
            final MathExpression eval =
                expr.evaluate((VariableDictionary) dictCache.get(type));
            try {
                return new Double(eval.getConstantValue());
            } catch (NotAConstantValueException e) {
                throw new RuntimeException("Cannot evaluate expression", e);
            }
        }
    }

    private final static Set KNOWN_VARIABLES =
        new HashSet(Arrays.asList(new String[] {
            "prs"
        }));
    private static class AttributeDictionary implements VariableDictionary {
        private final CellInterface cell;
        private final MathExpressionFactory factory;
        private final Map cache;
        public AttributeDictionary(final CellInterface cell,
                                   final MathExpressionFactory factory) {
            this.cell = cell;
            this.factory = factory;
            this.cache = new HashMap();
            // Initialize the cache
            for (Iterator i = KNOWN_VARIABLES.iterator(); i.hasNext(); ) {
                cache.put(i.next(), null);
            }
        }
        private MathExpression getVar(final String name) {
            if (name.equals("prs")) {
                final int count = PrsCount.count(cell, new HashMap(), false);
                return factory.makeConstant(count);
            } else {
                throw new AssertionError("Trying to evaluate unknown attribute: " + name);
            }
        }
        public MathExpression getVariableValue(final String name) {
            if (!cache.containsKey(name)) return null;
            MathExpression me = (MathExpression) cache.get(name);
            if (me == null) {
                me = getVar(name);
                cache.put(name, me);
            }
            return me;
        }
        public VariableDictionaryIterator getIterator() {
            final Iterator i = cache.entrySet().iterator();
            return new VariableDictionaryIterator() {
                public Variable next() {
                    final Map.Entry entry = (Map.Entry) i.next();
                    final String name = (String) entry.getKey();
                    MathExpression val = (MathExpression) entry.getValue();
                    if (val == null) {
                        val = getVar(name);
                        cache.put(name, val);
                    }
                    return new Variable() {
                        public String getName() { return name; }
                        public MathExpression getValue() {
                            return (MathExpression) entry.getValue();
                        }
                    };
                }
                public boolean hasNext() {
                    return i.hasNext();
                }
            };
        }
    }

    /**
     * Given a <code>String</code>, parse it as a math expression, and return
     * the associated <code>MathExpression</code>.  Parsing exceptions are
     * caught, and a <code>RuntimeException</code> will be rethrown with the
     * original exception as the cause.
     **/
    private static MathExpression getMathExpression(final String expr,
                                                    final MathExpressionFactory mf) {
        final MathExpressionLexer lexer =
            new MathExpressionLexer(new StringReader(expr));
        final MathExpressionParser parser = new MathExpressionParser(lexer, mf);
        final MathExpression me;
        try {
            me = parser.goal();
        } catch (Exception e) {
            throw new RuntimeException("Cannot parse expression: " + expr, e);
        }
        final String[] vars = me.getVariableNames();
        for (int i = 0; i < vars.length; ++i) {
            if (!KNOWN_VARIABLES.contains(vars[i]))
                throw new RuntimeException("Unknown attribute: " + vars[i]);
        }
        return me;
    }

    /**
     * Returns the appropriate predicate for relational operators.
     **/
    private static UnaryPredicate getPredicate(final String expr1,
                                               final String op,
                                               final String expr2,
                                               final Map dictCache,
                                               final MathExpressionFactory mf) {
        final BinaryPredicate bop = NumericPredicate.getPredicate(op);
        final UnaryFunction val1 =
            new MathFunction(dictCache, getMathExpression(expr1, mf), mf);
        final UnaryFunction val2 =
            new MathFunction(dictCache, getMathExpression(expr2, mf), mf);
        return new UnaryPredicate() {
            public boolean evaluate(final Object o) {
                return bop.evaluate(val1.execute(o), val2.execute(o));
            }
        };
    }

    private static class DirectivePredicateAction implements DirectiveActionInterface {
        private final String dir;
        private final String val;
        private boolean status = false;

        private boolean equalValue(final String valType, final Object value) {
            if (val == null) return true;
            final Object myval = DirectiveUtils.parseDirective(valType, val);
            if (myval == null) return false;
            return ObjectUtils.equals(myval, value);
        }

        public DirectivePredicateAction(final String dir, final String val) {
            this.dir = dir;
            this.val = val;
        }

        public boolean getResult() {
            return status;
        }

        public void reset() {
            status = false;
        }

        public void doUnParameterizedDirective(BlockInterface block,
                                               DirectiveBlock db,
                                               String directive,
                                               Object value,
                                               String valueType )
            throws IOException {
            if (directive.equals(dir) && equalValue(valueType, value))
                status = true;
        }

        public void doParameterizedDirectiveValue(BlockInterface block,
                                                  DirectiveBlock db,
                                                  String directive,
                                                  Object parameter,
                                                  Object value,
                                                  String parameterType,
                                                  String valueType)
            throws IOException {
            if (directive.equals(dir) && equalValue(valueType, value))
                status = true;
        }

        public void doParameterizedDirectiveType(BlockInterface block,
                                                 DirectiveBlock db,
                                                 String directive,
                                                 String parameterType,
                                                 String valueType)
            throws IOException { }
        public void doBlockInterface(BlockInterface block)
            throws IOException { }
    }

    private static UnaryPredicate getDirectivePredicate(final String blocks,
                                                        final String dir,
                                                        final String val) {
        final Set<String> wantBlock = blocks == null ?
            null
          : new HashSet<String>(Arrays.asList(StringUtil.split(blocks, ',')));
        final DirectivePredicateAction action =
            new DirectivePredicateAction(dir, val);
        final DirectiveWalker walker = new DirectiveWalker(action);
        return new UnaryPredicate() {
            private boolean valid(final String block) {
                return 
                    (DirectiveTable.lookupDirective(block, dir) != null ||
                     DirectiveTable.lookupParameterizedDirective(block, dir)
                        != null) &&
                    (wantBlock == null || wantBlock.contains(block));
            }
            private boolean walk(final BlockInterface bi, final String block) {
                try {
                    if (valid(block)) {
                        if (block == BlockInterface.CELL) walker.walk(bi);
                        else walker.walk(bi, block);
                    } else {
                        return false;
                    }
                } catch (IOException e) {
                } catch (UnknownDirectiveException e) {
                }
                return action.getResult();
            }
            public boolean evaluate(Object a) {
                action.reset();
                final CellInterface x = (CellInterface) a;
                final BlockInterface cellBlock = x.getBlockInterface();
                if (walk(cellBlock, BlockInterface.CELL) ||
                    walk(cellBlock, BlockInterface.PRS) ||
                    walk(cellBlock, BlockInterface.SUBCELL))
                    return true;

                if (valid(BlockInterface.ENV)) {
                    final EnvBlock envBlock = x.getEnvironments();
                    for (Iterator i = envBlock.getNames(); i.hasNext(); ) {
                        final String name = (String) i.next();
                        final CellInterface ex;
                        try {
                            ex = envBlock.getNamedEnvironment(name);
                        } catch (CastSemanticException e) {
                            throw new RuntimeException(
                                "Unable to load environment: " +
                                x.getFullyQualifiedType() + ":" + name, e);
                        }
                        walk(ex.getBlockInterface(), BlockInterface.ENV);
                        if (action.getResult()) return true;
                    }
                }
                return false;
            }
        };
    }

    private static void getGrayboxList(final CellInterface cell,
                                       final UnaryPredicate typeCheck,
                                       final Set seen,
                                       final Set includedSet) {
        final String type = cell.getFullyQualifiedType();
        if (!seen.add(type)) return;
        if (typeCheck.evaluate(cell) || CellUtils.isLeaf(cell)) {
            includedSet.add(type);
            return;
        }
        for (Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
            final Pair p = (Pair) i.next();
            final CellInterface subcell = (CellInterface) p.getSecond();
            getGrayboxList(subcell, typeCheck, seen, includedSet);
        }
    }

    private static boolean isValidBlock(final String block) {
        return block.equals(BlockInterface.CELL) ||
               block.equals(BlockInterface.PRS) ||
               block.equals(BlockInterface.SUBCELL) ||
               block.equals(BlockInterface.ENV);
    }

    private static final String PARENT_PREFIX = "parent:";
    /**
     * Returns the appropriate predicate given the name of the predicate.
     **/
    private static UnaryPredicate getPredicate(final String test,
                                               final String val,
                                               final CastFileParser cfp,
                                               final CellInterface cell) {
        // if a test is prefixed with "parent:", then the test is applied to
        // the refinement parent; this doesn't always make sense, e.g.,
        // "shared" or "one-level" predicates.
        if (test.startsWith(PARENT_PREFIX)) {
            final UnaryPredicate pred =
                getPredicate(test.substring(PARENT_PREFIX.length()), val,
                             cfp, cell);
            return new UnaryPredicate<CellInterface>() {
                public boolean evaluate(CellInterface x) {
                    final CellInterface parent = x.getDirectRefinementParent();
                    if (parent == null) {
                        return false;
                    } else {
                        return pred.evaluate(parent);
                    }
                }
            };
        }

        if (predicateMap.containsKey(test)) {
            return (UnaryPredicate) predicateMap.get(test);
        } else if (test.equals("tau")) {
            return getDirectivePredicate(null, DirectiveConstants.TAU, val);
        } else if (test.equals("directive")) {
            final String[] words = StringUtil.split(val, ':');
            final String blocks, dir, value;
            if (words.length == 1) {
                blocks = null;
                dir = words[0];
                value = null;
            } else if (words.length == 2) {
                if (words[0].indexOf(',') > 0 || isValidBlock(words[0])) {
                    blocks = words[0];
                    dir = words[1];
                    value = null;
                } else {
                    blocks = null;
                    dir = words[0];
                    value = words[1];
                }
            } else if (words.length == 3) {
                blocks = words[0];
                dir = words[1];
                value = words[2];
            } else {
                throw new RuntimeException("Invalid directive option specified: " + val);
            }
            return getDirectivePredicate(blocks, dir, value);
        } else if (test.equals("shared")) {
            return new UnaryPredicate() {
                private Set sharedInstance = null;
                public boolean evaluate(Object a) {
                    if (sharedInstance == null) {
                        final CellInterface shared;
                        try {
                            shared = cfp.getFullyQualifiedCell(val);
                        } catch (Exception e) {
                            throw new RuntimeException("Unable to load cell: " + val, e);
                        }
                        final Set skip = new HashSet();
                        skip.add(cell.getFullyQualifiedType());
                        sharedInstance = instanceSet(shared, skip);
                    }
                    final CellInterface x = (CellInterface) a;
                    return sharedInstance.contains(x.getFullyQualifiedType());
                }
            };
        } else if (test.equals("env")) {
            final boolean[] opt = { false, false, false, false };
            if (val != null) {
                final String[] rest = StringUtil.split(val, ':');
                for (int i = 0; i < rest.length; ++i) {
                    if (rest[i].equals("prs")) opt[0] = true;
                    else if (rest[i].equals("rte_ignore")) opt[1] = true;
                    else if (rest[i].equals("ntpc_spec"))  opt[2] = true;
                    else if (rest[i].equals("aspice_ignore"))  opt[3] = true;
                    else throw new RuntimeException("Invalid env option specified: " + rest[i]);
                }
            }
            return new UnaryPredicate() {
                public boolean evaluate(Object a) {
                    final CellInterface x = (CellInterface) a;
                    return PrintEnv.getEnvs(x, opt[0], opt[1],
                                            opt[2], opt[3]).size() > 0;
                }
            };
        } else if (test.equals("one-level")) {
            final Set localSubcells = new HashSet();
            for (Iterator i = cell.getLocalSubcellPairs(); i.hasNext(); ) {
                final Pair p = (Pair) i.next();
                final CellInterface subcell = (CellInterface) p.getSecond();
                localSubcells.add(subcell.getFullyQualifiedType());
            }
            return new UnaryPredicate() {
                public boolean evaluate(Object a) {
                    final CellInterface x = (CellInterface) a;
                    return localSubcells.contains(x.getFullyQualifiedType());
                }
            };
        } else if (test.equals("graybox-list")) {
            if (val == null) {
                throw new RuntimeException("Graybox list file not specified");
            }

            final Set grayBoxList = new HashSet();
            try {
                final Pattern space = Pattern.compile("\\s+");
                final BufferedReader br =
                    new BufferedReader(new FileReader(val));
                String line;
                int lineno = 0;
                while ((line = br.readLine()) != null) {
                    ++lineno;
                    // remove all whitespace
                    final String cellName = space.matcher(line).replaceAll("");
                    grayBoxList.add(cellName);
                }
                br.close();
            } catch (FileNotFoundException notFound) {
                throw new RuntimeException("Graybox list file " + val +
                                           " does not exist", notFound);
            } catch (IOException ioError) {
                throw new RuntimeException("Unable to read graybox list file " +
                                           val, ioError);
            }

            final UnaryPredicate typeCheck =
                CellUtils.getTypeMatcher(grayBoxList);

            final Set includedSet = new HashSet();
            getGrayboxList(cell, typeCheck, new HashSet(), includedSet);
            return new UnaryPredicate() {
                public boolean evaluate(Object a) {
                    final CellInterface x = (CellInterface) a;
                    return includedSet.contains(x.getFullyQualifiedType());
                }
            };
        } else if (test.equals("ancestor")) {
            if (val == null) {
                throw new RuntimeException("Ancestor not specified");
            }
            return CellUtils.getTypeMatcher(Collections.singleton(val));
        } else if (test.equals("block")) {
            if (val == null) {
                throw new RuntimeException("Block not specified");
            } else if (val.equals("prs")) {
                return new UnaryPredicate<CellInterface>() {
                    public boolean evaluate(CellInterface ci) {
                        return ci.containsCompletePrs();
                    }
                };
            } else if (val.equals("subcells")) {
                return new UnaryPredicate<CellInterface>() {
                    public boolean evaluate(CellInterface ci) {
                        return ci.containsCompleteSubcells();
                    }
                };
            } else if (val.equals("csp")) {
                return new UnaryPredicate<CellInterface>() {
                    public boolean evaluate(CellInterface ci) {
                        return ci.containsRunnableCsp();
                    }
                };
            } else if (val.equals("verilog")) {
                return new UnaryPredicate<CellInterface>() {
                    public boolean evaluate(CellInterface ci) {
                        return ci.containsVerilog();
                    }
                };
            } else if (val.equals("netlist")) {
                return new UnaryPredicate<CellInterface>() {
                    public boolean evaluate(CellInterface ci) {
                        return ci.containsNetlist();
                    }
                };
            } else {
                throw new RuntimeException(
                        "Invalid block " + val + " specified");
            }
        } else {
            throw new RuntimeException("Invalid predicate specified: " + test);
        }
    }

    private static ParamDirectiveValue getParamDirectiveValue(final String dir) {
        final Triplet[] p = DirectiveTable.lookupParameterizedDirective(BlockInterface.CELL, dir);
        if (p == null) {
            System.err.println("Invalid directive: " + dir);
            return null;
        } else {
            return new ParamDirectiveValue(dir, (String) p[0].getFirst());
        }
    }

    private static DirectiveValue getDirectiveValue(final String dir) {
        final Pair p = DirectiveTable.lookupDirective(BlockInterface.CELL, dir);
        if (p == null) {
            System.err.println("Invalid directive: " + dir);
            return null;
        } else {
            return new DirectiveValue(dir, (String) p.getFirst());
        }
    }

    private static String[] getArgs(final String name, final char sep) {
        final int index = name.indexOf('=');
        if (index > 0) {
            final String[] rest =
                StringUtil.split(name.substring(index + 1), sep);
            return rest;
        } else {
            return new String[0];
        }
    }

    private static TaskInterface getTaskInterface(
            final String name,
            final CastFileParser cfp,
            final String cellName,
            final UnaryPredicate<CellInterface> selectionPredicate,
            final UnaryPredicate<CellInterface> prunePredicate,
            final CDLNameInterface cellRenamer) {
        if (name.equals("subcell_tree")) {
            return new SubcellTree();
        } else if (name.equals("subcells")) {
            return new HierNop();
        } else if (name.equals("tau")) {
            return getDirectiveValue(DirectiveConstants.TAU);
        } else if (name.equals("density")) {
            return getDirectiveValue(DirectiveConstants.DENSITY_FACTOR);
        } else if (name.startsWith("paramdirective")) {
            final int index = name.indexOf('=');
            if (index < 0) {
                System.err.println("No directive specified");
                return null;
            } else {
                final String dir = name.substring(index + 1);
                return getParamDirectiveValue(dir);
            }
        } else if (name.startsWith("directive")) {
            final int index = name.indexOf('=');
            if (index < 0) {
                System.err.println("No directive specified");
                return null;
            } else {
                final String dir = name.substring(index + 1);
                return getDirectiveValue(dir);
            }
        } else if (name.equals("instances")) {
            return new InstanceCount(cellName, cfp);
        } else if (name.startsWith("instance_list")) {
            final int index = name.indexOf('=');
            boolean verilog = false, skipWiring = false;
            if (index > 0) {
                final String[] rest =
                    StringUtil.split(name.substring(index + 1), ':');
                for (int i = 0; i < rest.length; ++i) {
                    if (rest[i].equals("verilog")) verilog = true;
                    else if (rest[i].equals("skip-wiring")) skipWiring = true;
                }
            }
            return new InstanceList(cellName, cfp, verilog, skipWiring,
                                    cellRenamer);
        } else if (name.equals("prs")) {
            return new PrsCount(false);
        } else if (name.equals("prs=dnf")) {
            return new PrsCount(true);
        } else if (name.startsWith("transistors")) {
            final int index;
            final boolean detailed;
            if (name.startsWith("transistors=detailed")) {
                index = name.indexOf('=',name.indexOf('=') + 1);
                detailed = true;
            } else {
                index = name.indexOf('=');
                detailed = false;
            }
            final Set skip;
            if (index < 0) {
                skip = Collections.EMPTY_SET;
            } else {
                skip = new HashSet();
                final String skips = name.substring(index + 1);
                final StringTokenizer tok = new StringTokenizer(skips, ":");
                while (tok.hasMoreTokens()) {
                    skip.add(tok.nextToken());
                }
            }
            return new TransistorStat(cfp, skip, detailed,
                    new UnaryPredicate.Not<CellInterface>(prunePredicate));
        } else if (name.startsWith("dynamic_nodes")) {
            final int index = name.indexOf('=');
            boolean alias = false, leaky = false;
            if (index > 0) {
                final String[] rest =
                    StringUtil.split(name.substring(index + 1), ':');
                for (int i = 0; i < rest.length; ++i) {
                    if (rest[i].equals("alias")) alias = true;
                    else if (rest[i].equals("leaky")) leaky = true;
                }
            }
            return new DynamicNodes(cfp, new Cadencize(true), alias, leaky);
        } else if (name.startsWith("count_dynamic_nodes")) {
            return new CountDynamicNodes(cfp);
        } else if (name.startsWith("external_nodes")) {
            boolean writeImplied = false;
            boolean writeDirection = false;
            boolean writeAliases = false;
            boolean writeAllAliases = false;
            boolean writeXref = false;
            boolean writeRealOnly = false;
            final int index = name.indexOf('=');
            if (index > 0) {
                final String s = name.toLowerCase();
                final String[] rest =
                    StringUtil.split(s.substring(index + 1), ':');
                for (int i = 0; i < rest.length; ++i) {
                    if (rest[i].startsWith("writeim"))
                        writeImplied = true;
                    else if (rest[i].startsWith("writedi"))
                        writeDirection = true;
                    else if (rest[i].startsWith("im"))
                        writeImplied = true;
                    else if (rest[i].startsWith("di"))
                        writeDirection = true;
                    else if (rest[i].startsWith("all"))
                        writeAllAliases = true;
                    else if (rest[i].startsWith("al"))
                        writeAliases = true;
                    else if (rest[i].startsWith("xr"))
                        writeXref = true;
                    else if (rest[i].startsWith("co"))
                        writeRealOnly = true;
                    else if (rest[i].startsWith("re"))
                        writeRealOnly = true;
                }
            }
            return new ExternalNodes(writeImplied, writeDirection, writeAliases,
                                     writeAllAliases, writeXref, writeRealOnly);
        } else if (name.startsWith("local_nodes")) {
            final int index = name.indexOf('=');
            boolean alias = false, used = false, drivers = false,
                    EMReport = true, all_alias = false;
            if (index > 0) {
                final String[] rest =
                    StringUtil.split(name.substring(index + 1), ':');
                for (int i = 0; i < rest.length; ++i) {
                    if (rest[i].equals("alias")) alias = true;
                    else if (rest[i].equals("all_aliases")) all_alias = true;
                    else if (rest[i].equals("used")) used = true;
                    else if (rest[i].equals("drivers")) drivers = true;
                }
            }
            return new LocalNodes(cfp, new Cadencize(true), alias, used,
                                  drivers, EMReport, all_alias, false, false, null);
        } else if (name.startsWith("routing")) {
            return new RoutingComplexity(new Cadencize(false));
        } else if (name.startsWith("env")) {
            boolean prs = false, rte_ignore = false, ntpc_spec = false;
            boolean aspice_ignore = false;
            final int index = name.indexOf('=');
            if (index > 0) {
                final String[] rest =
                    StringUtil.split(name.substring(index + 1), ':');
                for (int i = 0; i < rest.length; ++i) {
                    if (rest[i].equals("prs")) prs = true;
                    else if (rest[i].equals("rte_ignore")) rte_ignore = true;
                    else if (rest[i].equals("ntpc_spec"))  ntpc_spec  = true;
                    else if (rest[i].equals("aspice_ignore"))
                        aspice_ignore = true;
                }
            }
            return new PrintEnv(prs, rte_ignore, ntpc_spec, aspice_ignore);
        } else if (name.equals("refinement_lineage")) {
            return new RefinementLineage();
        } else if (name.equals("em_spec")) {
            return new EMSpec(cfp, new Cadencize(true));
        } else if (name.equals("railstat")) {
            return new RailStats(new Cadencize(true));
        } else if (name.equals("attribute_list")) {
            return new AttributeList();
        } else if (name.startsWith("canonical_name=") ||
                   name.equals("canonical_name")) {
            final int index = name.indexOf('=');
            final Reader r;
            Boolean doskill=false;
            if (index < 0) {
                r = new InputStreamReader(System.in);
            } else {
                final String filearg = name.substring(index + 1);
                final int index2 = filearg.indexOf(':');
                final String file;
                if (index2 >= 0) {
                    if(filearg.substring(index2+1).equals("skill")) {
                        doskill=true;
                        file=filearg.substring(0,index2);
                    }
                    else {
                        file=filearg;
                    }
                }
                else {
                    file = filearg;
                }
                try {
                    r = new FileReader(file);
                } catch (FileNotFoundException e) {
                    System.err.println("Invalid file specified for " +
                                       "canonical_name task: " + file);
                    return null;
                }
            }
            return new CanonicalName(new Cadencize(true), r, cellRenamer, doskill);
        } else if (name.startsWith("initial_floorplan=") ||
                   name.equals("initial_floorplan")) {
            final int index = name.indexOf('=');
            final Pattern regex;
            if (index < 0) {
                regex = null;
            } else {
                final String re = name.substring(index + 1);
                try {
                    regex = Pattern.compile(re);
                } catch (PatternSyntaxException e) {
                    System.err.println("Invalid regular expression: " + re);
                    return null;
                }
            }
            return new InitialFloorplan(cellRenamer, regex);
        } else if (name.startsWith("enumerate_scan_chains")) {
            final Object[] dft = new Object[] {
                // custom scan chain
                "lib.serial.scan.ABSTRACT_RESET_SCANBUF", "LS.D.e", "RS.D.e",
                    Boolean.FALSE,
                "lib.serial.scan.RESET_SCANBUF_MIXED", "LS.D.e", "RS.D.e",
                    Boolean.FALSE,
                "lib.buffer.voltage.RS_VCBUF_ChanDft", "L.D.e", "R.D.e",
                    Boolean.FALSE,
                "lib.serial.scan.SCAN_RESET", "LS.D.e", "RS.D.e",
                    Boolean.TRUE,
                "lib.serial.scan.SBUF_ChanDft", "L.D.e", "R.D.e",
                    Boolean.TRUE,
                "lib.serial.scan.MBUF_ChanDft", "L.D.e", "R.D.e",
                    Boolean.TRUE,
                "lib.serial.scan.RBUF_ChanDft", "L.D.e", "R.D.e",
                    Boolean.TRUE,
                "lib.buffer.half.BUF_1of2", "L.e", "R.e",
                    Boolean.TRUE,
                // proteus scan chain
                "synthesis.qdi.special.SCAN_CONFIG", "LS.e", "RS.e",
                    Boolean.FALSE,
                "synthesis.qdi.special.SCAN_CONFIG2", "LS.e", "RS.e",
                    Boolean.FALSE,
                "synthesis.qdi.special.SCAN_BUF", "LS.e", "RS.e",
                    Boolean.FALSE,
                "synthesis.qdi.special.SCAN_TOK_BUF", "LS.e", "RS.e",
                    Boolean.FALSE,
                "synthesis.qdi.special.SCAN_TOK_EDFF", "LS.e", "RS.e",
                    Boolean.FALSE,
                "synthesis.qdi.special.SCAN_EDFF", "LS.e", "RS.e",
                    Boolean.FALSE,
                "synthesis.qdi.base.BUF_1of", "L.e", "R.e",
                    Boolean.TRUE,
                "synthesis.qdi.special.INVINV", "a", "x",
                    Boolean.TRUE
            };

            final Object[] sram = new Object[] {
                "lib.serial.interrupt.REPORT_INTERRUPT", "LINT.e", "RINT.e",
                    Boolean.FALSE,
                "chip.alta.fc.ffu.util.FFU_REPORT_INTERRUPT", "LINT.e", "RINT.e",
                    Boolean.FALSE,
                "lib.buffer.half.BUF_1of2", "L.e", "R.e",
                    Boolean.TRUE,
                "synthesis.qdi.base.BUF_1of", "L.e", "R.e",
                    Boolean.TRUE,
                "synthesis.qdi.special.INVINV", "a", "x",
                    Boolean.TRUE
            };

            final String[] args = getArgs(name, '\0');
            final Object[] scan;
            if (args.length == 0) {
                scan = dft;
            } else if (args.length == 1) {
                if (args[0].equals("dft")) {
                    scan = dft;
                } else if (args[0].equals("sram")) {
                    scan = sram;
                } else {
                    System.err.println("Invalid scan chain type: " + args[0]);
                    return null;
                }
            } else {
                System.err.println("enumerate_scan_chains: Too many arguments");
                return null;
            }

            return new EnumerateScanChain(scan, new Cadencize(true));
        } else if (name.startsWith("trace_connection")) {
            final String[] cells = getArgs(name, ':');
            final Object[] scan = new Object[cells.length * 4];
            int index = 0;
            for (String cell : cells) {
                final String[] parts = StringUtil.split(cell, '|');
                if (parts.length == 4) {
                    scan[index++] = parts[0];
                    scan[index++] = parts[1];
                    scan[index++] = parts[2];
                    scan[index++] = Boolean.valueOf(parts[3]);
                } else {
                    System.err.println(
                        "Invalid specification: " + cell);
                    return null;
                }
            }

            return new EnumerateScanChain(scan, new Cadencize(true));
        } else if (name.equals("level")) {
            return new HierarchyLevel();
        } else if (name.equals("ports") || name.equals("ports=primitive")) {
            return new PortList(name.equals("ports=primitive"));
        } else {
            return null;
        }
    }

    private static String translateNode(final String node,
                                        final CDLNameInterface renamer) {
        if (renamer == null) {
            return node;
        } else {
            try {
                return renamer.renameNode(node);
            } catch (CDLRenameException e) {
                System.err.println("Cannot translate node: " + node);
                return node;
            }
        }
    }

    private static String translateCell(final String cell,
                                        final CDLNameInterface renamer) {
        if (renamer == null) {
            return cell;
        } else {
            try {
                return renamer.renameCell(cell);
            } catch (CDLRenameException e) {
                System.err.println("Cannot translate cell: " + cell);
                return cell;
            }
        }
    }

    private static String translateInstance(final String instance,
                                            final CDLNameInterface renamer) {
        if (renamer == null) {
            return instance;
        } else {
            try {
                return renamer.renameSubCellInstance(instance);
            } catch (CDLRenameException e) {
                System.err.println("Cannot translate instance: " + instance);
                return instance;
            }
        }
    }
    
    private interface Container {
        Collection/*<Container>*/ getChildren();
    }
    private static void sortContainerByLevel(final Container top,
                                             final Integer currentLevel,
                                             final Map levels) {
        final Integer maxLevel = (Integer) levels.get(top);
        if (maxLevel == null || currentLevel.intValue() > maxLevel.intValue()) {
            levels.put(top, currentLevel);
            final Integer nextLevel = new Integer(currentLevel.intValue() + 1);
            for (Iterator i = top.getChildren().iterator(); i.hasNext(); ) {
                final Container child = (Container) i.next();
                sortContainerByLevel(child, nextLevel, levels);
            }
        }
    }
    private static MultiMap sortContainerByLevel(
            final Container top, final MultiMap.CollectionFactory factory) {
        final Map levels = new HashMap();
        sortContainerByLevel(top, new Integer(Integer.MIN_VALUE), levels);
        final MultiMap byLevels = new MultiMap(new TreeMap(), factory);
        for (Iterator i = levels.entrySet().iterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            byLevels.put(entry.getValue(), entry.getKey());
        }
        return byLevels;
    }
    private static class CellInterfaceContainer implements Container,
                                                           Comparable {
        private final CellInterface cell;
        private final UnaryPredicate<CellInterface> filter;
        private Collection children;
        public CellInterfaceContainer(
                final CellInterface cell,
                final UnaryPredicate<CellInterface> filter) {
            this.cell = cell;
            this.filter = filter;
            this.children = null;
        }
        public Collection/*<Container>*/ getChildren() {
            if (children == null) {
                final Map subcells = new HashMap();
                for (Iterator i = CellUtils.filterSubcellPairs(cell, filter);
                     i.hasNext(); ) {
                    final Pair p = (Pair) i.next();
                    final CellInterface subcell = (CellInterface) p.getSecond();
                    subcells.put(subcell.getFullyQualifiedType(), subcell);
                }
                children = new ArrayList(subcells.size());
                for (Iterator i = subcells.entrySet().iterator();
                     i.hasNext(); ) {
                    final Map.Entry entry = (Map.Entry) i.next();
                    children.add(new CellInterfaceContainer(
                                    (CellInterface) entry.getValue(),
                                    filter));
                }
            }
            return children;
        }
        public boolean equals(final Object o) {
            if (o instanceof CellInterfaceContainer) {
                final CellInterfaceContainer other = (CellInterfaceContainer) o;
                return cell.getFullyQualifiedType()
                           .equals(other.cell.getFullyQualifiedType());
            } else {
                return false;
            }
        }
        public int hashCode() {
            return cell.getFullyQualifiedType().hashCode();
        }
        public CellInterface getCellInterface() {
            return cell;
        }
        public int compareTo(final Object o) {
            final CellInterfaceContainer other = (CellInterfaceContainer) o;
            return getCellInterface().getFullyQualifiedType().compareTo(
                        other.getCellInterface().getFullyQualifiedType());
        }
    }
    private static Iterator sortCellInterfaceByLevel(
            final CellInterface top,
            final UnaryPredicate<CellInterface> filter) {
        final MultiMap sorted =
            sortContainerByLevel(
                    new CellInterfaceContainer(top, filter),
                    new MultiMap.CollectionFactory() {
                        public Collection newCollection() {
                            return new TreeSet();
                        }
                    });
        return new MappingIterator(sorted.values().iterator(),
                new UnaryFunction() {
                    public Object execute(Object o) {
                        return ((CellInterfaceContainer) o).getCellInterface();
                    }
                });
    }

    public static void query( final CellInterface rootCell,
                              final CastFileParser castParser,
                              final UnaryPredicate selectionPredicate,
                              final UnaryPredicate prunePredicate,
                              final List listOfTasks,
                              final CDLNameInterface renamer,
                              final boolean printHeader,
                              final boolean noRecurse,
                              final boolean instantiationOrder,
                              final boolean inclusivePrune,
                              final String outputSeperator,
                              final Writer outputWriter ) 
    throws IOException {

        final Writer w = new BufferedWriter( outputWriter ) ;
        final ArrayList flatTasks = new ArrayList();
        final ArrayList hierTasks = new ArrayList();
        final Iterator taskIter = listOfTasks.iterator();
        while ( taskIter.hasNext() ) {
            final String name = ( String ) taskIter.next();
            final TaskInterface task = 
                getTaskInterface( name,
                                  castParser,
                                  rootCell.getFullyQualifiedType(),
                                  selectionPredicate,
                                  prunePredicate,
                                  renamer );
            if (task == null) {
                System.err.println("Ignoring unknown task: " + name);
            } else {
                if (task.isFlat()) {
                    ((FlatTask) task).printHeader = printHeader;
                    flatTasks.add(task);
                } else {
                    hierTasks.add(task);
                }
            }
        }

        // Exit if there are no valid tasks specified.
        if (flatTasks.size() == 0 && hierTasks.size() == 0) return;

        // Do flat tasks
        for (Iterator i = flatTasks.iterator(); i.hasNext(); ) {
            final TaskInterface task = (TaskInterface) i.next();
            ((FlatTask) task).doTask( rootCell, w, noRecurse);
        }

        if (hierTasks.size() > 0) {
            // Generate the set of cells for hierarchical tasks to work on.  If
            // --no-recurse, only do the top-level cell.
            final Map workMap;
            if (noRecurse) {
                workMap = new HashMap();
                if (selectionPredicate.evaluate(rootCell))
                    workMap.put( rootCell.getFullyQualifiedType(), rootCell );
            } else {
                
                workMap = instantiationOrder ? (Map) new LinkedHashMap()
                                             : (Map) new TreeMap();
                final UnaryPredicate<CellInterface> unprune =
                    new UnaryPredicate.Not<CellInterface>( prunePredicate );

                final BinaryPredicate<List<CellInterface>,CellInterface> exprune =
                    (p, c) -> {
                        if (inclusivePrune) {
                            return unprune.evaluate(c);
                        } else {
                            return unprune.evaluate(p.get(p.size() - 1));
                        }
                    };
                final Iterator cellIter = instantiationOrder ?
                      sortCellInterfaceByLevel( rootCell, unprune )
                    : new ChildrenFirstCellInterfaceIterator(
                            rootCell, exprune );

                while ( cellIter.hasNext() ) {
                    final CellInterface currCell =
                        (CellInterface) cellIter.next();
                    
                    if ( ( ! CellUtils.isWiring( currCell ) ) && 
                         ( selectionPredicate.evaluate(currCell) ) ) {
                        workMap.put( currCell.getFullyQualifiedType(), currCell );
                    }
                    
                }

               
            }

            final Iterator workMapEntryIter = workMap.entrySet().iterator();

            while ( workMapEntryIter.hasNext() ) {
                final Map.Entry workMapEntry = 
                    ( Map.Entry ) workMapEntryIter.next();
                
                final CellInterface cell =
                    ( CellInterface ) workMapEntry.getValue();

                w.write( translateCell( cell.getFullyQualifiedType(),
                                        renamer ) ) ;

                for ( Iterator i = hierTasks.iterator(); i.hasNext(); ) {
                    w.write( outputSeperator );
                    final TaskInterface task = ( TaskInterface ) i.next();
                    task.doTask( cell, w);
                }
                w.write("\n");
            }
        }

        w.flush();
        
    }

    public static void query( final CastFileParser castParser,
                              final String cellName,
                              final String envName,
                              final String tasks,
                              final String filter,
                              final String prune,
                              final boolean noRecurse,
                              final String translatorName,
                              final boolean noHeader,
                              final boolean instantiationOrder,
                              final boolean inclusivePrune,
                              final boolean routed,
                              final String separator,
                              final Writer output )
        throws CastSemanticException,RecognitionException, TokenStreamException, NoSuchEnvironmentException, IOException {
        
        final CellInterface cell = castParser.getFullyQualifiedCell( cellName );
        final CellInterface ci;
        if (envName == null) {
            ci = cell;
        } else {
            ci = cell.getEnvironment(envName);
        }

        query( castParser,
               routed ? ci.routedSubcells(true) : ci,
               tasks,
               filter,
               prune,
               noRecurse,
               translatorName,
               noHeader,
               instantiationOrder,
               inclusivePrune,
               separator,
               output );
    }

    private static UnaryPredicate getPredicate(final CastFileParser castParser,
                                               final CellInterface cell,
                                               final String filter,
                                               final boolean def)
    throws CastSemanticException, RecognitionException, TokenStreamException,
           IOException {
        // Get the selection predicate, if --filter is specified, or set it to
        // the true predicate
        final UnaryPredicate selectionPredicate;
        if ( filter.equals( "" ) ) {
            selectionPredicate = new UnaryPredicate.Constant(def) ;
        } else {
            final CastQueryLexer cqLexer =
                new CastQueryLexer( new StringReader( filter ) );
            final CastQueryParser cqParser = new CastQueryParser( cqLexer );
            final Map dictCache = new HashMap();
            final MathExpressionFactory mf = new MathExpressionFactoryImpl();
            selectionPredicate =
                cqParser.goal( new CastQueryParser.QueryConstructor() {
                        public UnaryPredicate getClause( final String test,
                                                         final String op,
                                                         final String arg ) {
                            return op == null || op.equals("=") ?
                                getPredicate(test, arg, castParser, cell) :
                                getPredicate(test, op, arg, dictCache, mf);
                        }
                    } );
        }
        return selectionPredicate;
    }

    public static void query( final CastFileParser castParser,
                              final CellInterface cell,
                              final String tasks,
                              final String filter,
                              final String prune,
                              final boolean noRecurse,
                              final String translatorName,
                              final boolean noHeader,
                              final boolean instantiationOrder,
                              final boolean inclusivePrune,
                              final String separator,
                              final Writer output ) 
    throws CastSemanticException, RecognitionException, TokenStreamException,
           IOException {
        // Get the selection predicate, if --filter is specified, or set it to
        // the true predicate
        final UnaryPredicate selectionPredicate =
            getPredicate(castParser, cell, filter, true);
        final UnaryPredicate prunePredicate =
            getPredicate(castParser, cell, prune, false);

        // Get the list of tasks, and make sure they are valid.
        final StringTokenizer tokenizer = new StringTokenizer( tasks, "," );
        final List tasksList = new LinkedList();
        
        while ( tokenizer.hasMoreTokens() ) {
            final String name = tokenizer.nextToken();
            tasksList.add( name );
        }
        
        // Get the optional renamer.
        CDLNameInterface renamer;
        if ( translatorName.equals("") ) {
            renamer = null;
        } else {
            renamer = getRenamer( translatorName );
        }
        
        query( cell,
               castParser,
               selectionPredicate,
               prunePredicate,
               tasksList, 
               renamer, 
               ! noHeader,
               noRecurse,
               instantiationOrder,
               inclusivePrune,
               separator,
               output );
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
        final String cellEnvName = theArgs.getArgValue("cell", null);
        final String expr = theArgs.getArgValue("filter", "");
        final String prune = theArgs.getArgValue("prune", "");
        final String taskNames = theArgs.getArgValue("task", null);
        final boolean noRecurse = theArgs.argExists("no-recurse");
        final boolean printHeader = !theArgs.argExists("no-header");
        final String output = theArgs.getArgValue("output", null);
        final boolean routed = theArgs.argExists("routed");
        final String hierSep;
        if ( theArgs.argExists("seperator"))
            hierSep = theArgs.getArgValue("seperator", " ");
        else
            hierSep = theArgs.getArgValue("separator", " ");
        final String translateScheme = theArgs.getArgValue("translate", "");

        if (theArgs.argExists("version")) {
            System.out.println(
                com.avlsi.util.debug.VersionInfo.getVersionString(
                    CastQuery.class));
        }

        if (cellEnvName == null) {
            //System.err.println("ERROR: You must specify a cell name.");
            usage("ERROR: You must specify a cell name.\n");
        } else if (taskNames == null) {
            //System.err.println("ERROR: You must specify tasks to perform.");
            usage("ERROR: You must specify tasks to perform.\n");
        }

        final CastFileParser castParser =
            CastCacheManager.getDefault().getCastFileParser(
                new FileSearchPath(castRoot), castVersion,
                new StandardParsingOption(theArgs) );

        // cover future arg loading
        pedanticArgs.argTag( "cadence-name" );
        pedanticArgs.argTag( "instantiation-order" );

        if ( ! pedanticArgs.pedanticOK( false, true ) )
            usage( pedanticArgs.pedanticString() );

        final Writer w = output == null ?
            new OutputStreamWriter(System.out) :
            new FileWriter(output);

        final int colon = cellEnvName.indexOf(':');
        final String cellName;
        final String envName;
        if (colon == -1) {
            cellName = cellEnvName;
            envName = null;
        } else {
            cellName = cellEnvName.substring(0, colon);
            envName = cellEnvName.substring(colon + 1);
        }

        final String realName;
        if (theArgs.argExists("cadence-name")) {
            final CadenceReverseNameInterface crni =
                new CadenceReverseNameInterface();
            // Should envName also be renamed?  The environment name never
            // appears in a Cadence context.  It's inconsistent, but likely
            // more conveninent to not.
            realName = crni.renameCell(cellName);
        } else {
            realName = cellName;
        }
        
        final boolean instantiationOrder =
            theArgs.argExists("instantiation-order");

        final boolean inclusivePrune =
            !theArgs.argExists("exclusive-prune");

        try {
            query( castParser,
                   realName,
                   envName,
                   taskNames,
                   expr,
                   prune,
                   noRecurse,
                   translateScheme,
                   ! printHeader,
                   instantiationOrder,
                   inclusivePrune,
                   routed,
                   hierSep,
                   w );
        } catch (CastSemanticException e) {
            ExceptionPrettyPrinter.printException(e, System.err);
            System.exit(1);
        } catch (NoSuchEnvironmentException e) {
            System.err.println("Environment " + e.getEnvironmentName() +
                               " not found in cell " + e.getCellName());
            System.exit(1);
        } catch (CastQuery.RuntimeException e) {
            if (e.getMessage() != null) System.err.println(e.getMessage());
            final Throwable c = e.getCause();
            if (c instanceof CastSemanticException) {
                ExceptionPrettyPrinter.printException((CastSemanticException)c,
                                                      System.err);
            }
            System.exit(1);
        }
    }
}
