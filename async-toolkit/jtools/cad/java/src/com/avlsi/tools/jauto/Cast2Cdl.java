/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.jauto;

import java.io.IOException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.StringReader;
import java.io.Writer;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.TreeSet;
import java.util.SortedSet;
import java.util.Comparator;
import java.util.List;
import java.util.ArrayList;
import java.util.Set;
import java.util.Stack;
import java.util.StringTokenizer;

import com.avlsi.cast.CastFileParser;
import com.avlsi.cast.CastSyntaxException;
import com.avlsi.cast.CastSemanticException;
import com.avlsi.cast.impl.Environment;
import com.avlsi.cast.impl.LocalEnvironment;
import com.avlsi.cast.impl.NullEnvironment;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;
import com.avlsi.cell.InstanceTrace;
import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.NetlistAdapter;
import com.avlsi.fast.NetlistBlock;
import com.avlsi.file.common.HierName;
import com.avlsi.file.cdl.parser.CDLFactoryAdaptor;
import com.avlsi.file.cdl.parser.CDLFactoryEmitter;
import com.avlsi.file.cdl.parser.CDLFactoryFilter;
import com.avlsi.file.cdl.parser.CDLFactoryInterface;
import com.avlsi.file.cdl.parser.CDLInlineFactory;
import com.avlsi.file.cdl.parser.CDLLexer;
import com.avlsi.file.cdl.parser.CDLSimpleInterface;
import com.avlsi.file.cdl.parser.Inline;
import com.avlsi.file.cdl.parser.Template;
import com.avlsi.file.cdl.parser.LVSNodesCDLFactory;
import com.avlsi.file.cdl.parser.LVSNodesNullHandler;
import com.avlsi.file.cdl.util.rename.CadenceNameInterface;
import com.avlsi.file.cdl.util.rename.CadenceReverseNameInterface;
import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.CDLRenameFactory;
import com.avlsi.file.cdl.util.rename.GDS2NameInterface;
import com.avlsi.file.cdl.util.rename.IdentityNameInterface;
import com.avlsi.file.cdl.util.rename.TrivialCDLNameInterfaceFactory;
import com.avlsi.file.cdl.util.rename.ReloadableNameInterface;
import com.avlsi.io.FileSearchPath;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cadencize.CadenceInfo;
import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.PedanticCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.container.CollectionUtils;
import com.avlsi.util.container.Pair;
import com.avlsi.util.functions.BinaryFunction;
import com.avlsi.util.functions.UnaryPredicate;
import com.avlsi.util.text.StringUtil;

import com.avlsi.layout.LVSNodes;
import com.avlsi.layout.LVSNodesForExtract;
import com.avlsi.layout.LVSNodesForHierarchy;

public final class Cast2Cdl {
    /**
     * This class should not be instantiated.
     **/
    private Cast2Cdl() { }

    /**
     * Translates process independent transistor type into real transistor
     * model names.
     **/
    public static class RealTransistorNames extends CDLFactoryFilter {
        /**
         * Name of the subcircuit currently being processed.  <code>null</code>
         * if not inside a subcircuit.  Does not handle nested subcircuits.
         **/
        private String currentSubcircuit = null;

        /**
         * A function that transforms the transistor model.  The
         * <code>execute</code> method of the <code>BinaryFunction</code> is
         * called with the name of the subcircuit where the transistor lives,
         * and the transistor model name.
         **/
        private final BinaryFunction /*<String,String>*/ transformer;

        /**
         * Translates model name by looking at the
         * <code>transistor_names</code> directive in a cell.  If the
         * transistor does not live in a subcircuit definition, or if a
         * <code>CellInterface</code> cannot be obtained for the subcircuit
         * where it does live, then the original model name is used.
         **/
        public RealTransistorNames(final CDLFactoryInterface inner,
                                   final CastFileParser cfp) {
            this(inner, new BinaryFunction() {
                public Object execute(final Object a, final Object b) {
                    final String subName = (String) a;
                    if (subName == null) return b;

                    final CellInterface ci;
                    try {
                        ci = loadCell(cfp, subName);
                    } catch (Exception e) {
                        return b;
                    }

                    final String model = (String) b;
                    return CellUtils.getTransistorModelName(ci, model);
                }
            });
        }
        public RealTransistorNames(final CDLFactoryInterface inner,
                                   final BinaryFunction transformer) {
            super(inner);
            this.transformer = transformer;
        }
        public void makeTransistor(HierName name, String type, HierName ns,
                                   HierName nd, HierName ng, HierName nb,
                                   CDLLexer.InfoToken w, CDLLexer.InfoToken l,
                                   Map parameters, Environment env) {
            final String realtype =
                (String) transformer.execute(currentSubcircuit, type);
            inner.makeTransistor(name, realtype, ns, nd, ng, nb, w, l,
                                 parameters, env);
        }
        public void beginSubcircuit(String subName, String[] in, String[] out,
                                    Map parameters, Environment env) {
            currentSubcircuit = subName;
            inner.beginSubcircuit(subName, in, out, parameters, env);
        }
        public void endSubcircuit(String subName, Environment env) {
            currentSubcircuit = null;
            inner.endSubcircuit(subName, env);
        }
    }

    /**
     * Filter transistor parameters based on a user given predicate.
     **/
    public static class MosParameterFilter extends CDLFactoryFilter {
        private final UnaryPredicate<String> want;
        public MosParameterFilter(final CDLFactoryInterface inner,
                                  final UnaryPredicate<String> want) {
            super(inner);
            this.want = want;
        }
        public MosParameterFilter(final CDLFactoryInterface inner,
                                  final String[] keys) {
            this(inner,
                 new UnaryPredicate<String>() {
                    final SortedSet<String> wantSet = (SortedSet<String>)
                        CollectionUtils.addAll(
                            new TreeSet<String>(
                                new Comparator<String>() {
                                    public int compare(String s1, String s2) {
                                        return s1.compareToIgnoreCase(s2);
                                    }
                                }),
                            keys);
                    public boolean evaluate(String key) {
                        return wantSet.contains(key);
                    }
                 });
        }
        public void makeTransistor(HierName name, String type, HierName ns,
                                   HierName nd, HierName ng, HierName nb,
                                   CDLLexer.InfoToken w, CDLLexer.InfoToken l,
                                   Map parameters, Environment env) {
            final LinkedHashMap filtered = new LinkedHashMap();
            for (Iterator i = parameters.entrySet().iterator(); i.hasNext(); ) {
                final Map.Entry entry = (Map.Entry) i.next();
                final String key = (String) entry.getKey();
                if (want.evaluate(key)) {
                    filtered.put(key, entry.getValue());
                }
            }
            inner.makeTransistor(name, type, ns, nd, ng, nb, w, l, filtered,
                                 env);
        }
    }

    /**
     * A factory that accumulates the names of cells being instantiated.
     **/
    private static class GateCalls extends CDLFactoryAdaptor {
        private final List s;
        public GateCalls(List s) {
            this.s = s;
        }
        public void makeCall(HierName name, String subName, HierName[] args,
                             Map parameters, Environment env) {
            s.add(subName);
        }
    }

    /**
     * Returns the names of gates used in the netlist block of a cell.
     **/
    public static List getGateInstantiations(final CellInterface cell) {
        final List result = new ArrayList();
        if (cell.containsNetlist()) {
            final NetlistBlock block = (NetlistBlock) cell.getBlockInterface().iterator(BlockInterface.NETLIST).next();
            block.getCDLTemplate().execute(new GateCalls(result));
        }
        return result;
    }

    /**
     * Get a CellInterface given a CAST path, the cell name, and CAST version.
     **/
    private static CellInterface loadCell(final CastFileParser castParser,
                                          final String cellName)
        throws CastSyntaxException, CastSemanticException, IOException {
        return castParser.getFullyQualifiedCell(cellName);
    }

    private static void usage( String m ) {
        
        System.err.println( "Usage: cast2cdl\n" );
        System.err.println("  --cast-path=<cast-path> (defaults to .)");
        System.err.println("  --cast-version=<version> (defaults to 2)");
        System.err.println("  --cell=<cell> (name of cell to process, can be FQCN+-)");
        System.err.println("  --output=<file> (defaults to <cell>.cdl)");
        System.err.println("  --translate=[ cadence | gds2 | none ] (defaults to cadence)");
        System.err.println("  [ --cadence-name ]");
        System.err.println("  [ --inline-layout (respect the inline_layout directive) ]"); 
        System.err.println("  [ --flatten (flatten the netlist) ]");
        System.err.println("  [ --lvs-nodes=[none,hierarchy,extract] (how to deal with lvs_nodes directive) ]");
        System.err.println("  [ --process-dependent-name (emit process dependent transistor types) ]");
        System.err.println("  [ --name-map=<file> (read name mapping from file) ]");
        System.err.println("  [ --cdl-mos-parameters=<param,param,...> (parameters to emitted for M) ]");
        if (m != null && m.length() > 0)
            System.err.print( m );
        System.exit(1);
    }

    private static void usage() {
        usage ( null );
    }

    /**
     * Emit a subcircuit call.
     **/
    private static void emitCall(final HierName instance,
                                 final CellInterface subcircuit,
                                 final Cadencize cadencizer,
                                 final CadenceInfo cadenceInfo,
                                 final CDLFactoryInterface factory) {
        final Set ports = NetlistAdapter.getParameterList(subcircuit, cadencizer);
        final HierName[] args = new HierName[ports.size()];
        int j = 0;
        for (Iterator i = ports.iterator(); i.hasNext(); ++j) {
            final HierName port = (HierName) i.next();
            final HierName net = (HierName) cadenceInfo.getLocalNodes().getCanonicalKey(HierName.append(instance, port));
            args[j] = net;
        }

        factory.makeCall(instance, subcircuit.getFullyQualifiedType(), args, Collections.EMPTY_MAP, new LocalEnvironment() );
    }

    private static Set inlineLayoutSet(final CellInterface cell) {
        final Map m =
           DirectiveUtils.getSubcellDirective(cell,
                                              DirectiveConstants.INLINE_LAYOUT,
                                              DirectiveConstants.INSTANCE_TYPE);
        return DirectiveUtils.getExplicitTrues(m);
    }

    /**
     * Emit a mid-level cell.
     * @param inline_layout Respect the inline_layout directive?
     **/
    private static void writeMidlevel(final CellInterface cell,
                                      final Cadencize cadencizer,
                                      final CDLFactoryInterface factory,
                                      final boolean inline_layout,
                                      final boolean handleVerilog,
                                      final boolean ignoreUnimpl) {
        final Set inlined =
            inline_layout ? inlineLayoutSet(cell) : Collections.EMPTY_SET;

        CDLOutput.templateHeader(cell, Collections.EMPTY_MAP, factory,
                                 cadencizer);

        final CadenceInfo cadenceInfo = cadencizer.convert(cell);
        for (Iterator i = cell.getLocalSubcellPairs(); i.hasNext(); ) {
            final Pair p = (Pair) i.next();
            final CellInterface subcell = (CellInterface) p.getSecond();
            final boolean verilogBlock =
                subcell.containsVerilog() && handleVerilog;
            if (!verilogBlock &&
                    (CellUtils.isWiring(subcell) ||
                    (ignoreUnimpl && !subcell.hasNetlistBody())))
                continue;
            final HierName name = (HierName) p.getFirst();
            final CDLFactoryInterface callEmitter;
            if (inlined.contains(name)) {
                final Set ports = NetlistAdapter.getParameterList(subcell);
                final String[] out = new String[ports.size()];
                int j = 0;
                for (Iterator k = ports.iterator(); k.hasNext(); ++j) {
                    out[j] = ((HierName) k.next()).getCadenceString();
                }
                final NetlistBlock nb =
                    (NetlistBlock) subcell.getBlockInterface()
                                          .iterator(BlockInterface.NETLIST)
                                          .next();
                if (nb.getCDLTemplate() == null) {
                    throw new RuntimeException("In cell " + cell.getFullyQualifiedType() + ", inline_layout directive specified on instance " + name + ", which is not a leaf cell.");
                }
                final CDLInlineFactory inliner =
                    new CDLInlineFactory(true, null, false);
                inliner.addTarget(subcell.getFullyQualifiedType(),
                                  Template.setInOut(nb.getCDLTemplate(),
                                                    new String[0], out));
                inliner.setProxy(factory);
                callEmitter = inliner;
            } else {
                callEmitter = factory;
            }
            emitCall(name, subcell, cadencizer, cadenceInfo, callEmitter);
        }

        factory.endSubcircuit(cell.getFullyQualifiedType(), null);
    }

    /**
     * Emit a leaf cell.
     **/
    private static void writeLeaf(final CellInterface cell,
                                  final Cadencize cadencizer,
                                  final CDLFactoryInterface emitter, 
                                  final boolean sortNetlist) {
        if(sortNetlist) {
            final NetlistBlock block = 
                (NetlistBlock) cell.getBlockInterface().iterator(BlockInterface.NETLIST).next();
            block.getCDLTemplate().sortStatements();
        }
        CDLOutput.writeCDL(cell, cadencizer, emitter);
    }

    /**
     * Output the CDL for the gates instantiated in a cell, if they haven't
     * been outputted before.
     **/
    private static void outputGates(final CellInterface cell,
                                    final CastFileParser castParser,
                                    final CDLFactoryInterface emitter,
                                    final Cadencize cadencizer,
                                    final Set seen,
                                    final boolean sortNetlist) throws IOException {
        final List gates = getGateInstantiations(cell);
        final SortedSet sorted = new TreeSet();
        sorted.addAll(gates);
        for (Iterator i = sorted.iterator(); i.hasNext(); ) {
            final String gateName = (String) i.next();
            if (!seen.add(gateName)) continue;
            try {
                final CellInterface gate = loadCell(castParser, gateName);
                writeLeaf(gate, cadencizer, emitter, sortNetlist);
            } catch (Exception e) {
                System.err.println("ERROR: Cannot load gate " + gateName + "!");
                e.printStackTrace();
                System.exit(2);
            }
        }
    }

    public static void outputCDL(final CellInterface cell,
                                 final CastFileParser castParser,
                                 final CDLFactoryInterface emitter,
                                 final Cadencize cadencizer,
                                 final boolean inline_layout,
                                 final boolean sortNetlist) throws IOException {
        outputCDL(cell, castParser, emitter, cadencizer, inline_layout,
                  sortNetlist, false);
    }

    public static void outputCDL(final CellInterface cell,
                                 final CastFileParser castParser,
                                 final CDLFactoryInterface emitter,
                                 final Cadencize cadencizer,
                                 final boolean inline_layout,
                                 final boolean sortNetlist,
                                 final boolean handleVerilog)
    throws IOException {
        outputCDL(cell, castParser, emitter, cadencizer, inline_layout,
                  sortNetlist, handleVerilog, true);
    }

    public static void outputCDL(final CellInterface cell,
                                 final CastFileParser castParser,
                                 final CDLFactoryInterface emitter,
                                 final Cadencize cadencizer,
                                 final boolean inline_layout,
                                 final boolean sortNetlist,
                                 final boolean handleVerilog,
                                 final boolean exitOnError)
    throws IOException {
        outputCDL(cell, castParser, emitter, cadencizer, inline_layout,
                  sortNetlist, handleVerilog, exitOnError, false);
    }

    public static void outputCDL(final CellInterface cell,
                                 final CastFileParser castParser,
                                 final CDLFactoryInterface emitter,
                                 final Cadencize cadencizer,
                                 final boolean inline_layout,
                                 final boolean sortNetlist,
                                 final boolean handleVerilog,
                                 final boolean exitOnError,
                                 final boolean ignoreUnimpl)
    throws IOException {
        final InstanceTrace it = new InstanceTrace();
        it.enter(cell.getFullyQualifiedType(), "top level");
        outputCDL(cell, castParser, emitter, cadencizer, inline_layout,
                  new HashSet(), sortNetlist, handleVerilog, it, exitOnError,
                  ignoreUnimpl);
        it.leave();
    }

    /**
     * Output CDL such that subcircuits are defined before being used.
     **/
    private static void outputCDL(final CellInterface cell,
                                  final CastFileParser castParser,
                                  final CDLFactoryInterface emitter,
                                  final Cadencize cadencizer,
                                  final boolean inline_layout,
                                  final Set seen,
                                  final boolean sortNetlist,
                                  final boolean handleVerilog,
                                  final InstanceTrace it,
                                  final boolean exitOnError,
                                  final boolean ignoreUnimpl)
    throws IOException {
        final boolean verilogBlock =
            cell.containsVerilog() && handleVerilog && !cell.containsNetlist();

        if ((CellUtils.isWiring(cell) && !verilogBlock) ||
            (ignoreUnimpl && !seen.isEmpty() && !cell.hasNetlistBody() &&
             !verilogBlock) ||
            !seen.add(cell.getFullyQualifiedType())) return;

        outputGates(cell, castParser, emitter, cadencizer, seen, sortNetlist);

        boolean leaf = true;
        final Set inlined =
            inline_layout ? inlineLayoutSet(cell) : Collections.EMPTY_SET;
        
        SortedSet sorted = new TreeSet();
                
        for (Iterator i = cell.getLocalSubcellPairs(); i.hasNext(); ) {
            sorted.add(i.next());
        }

        for (Iterator i = sorted.iterator(); i.hasNext(); ) {
            final Pair p = (Pair) i.next();
            final CellInterface subcell = (CellInterface) p.getSecond();
            if (!inlined.contains(p.getFirst())) {
                it.enter(p);
                outputCDL(subcell, castParser, emitter, cadencizer,
                          inline_layout, seen, sortNetlist, handleVerilog, it,
                          exitOnError, ignoreUnimpl);
                it.leave();
            } else {
                outputGates(subcell, castParser, emitter, cadencizer, seen,
                            sortNetlist);
            }
            leaf = false;
        }

        if (leaf && !verilogBlock) {
            if (!cell.containsNetlist() && !ignoreUnimpl) {
                if (exitOnError) {
                    System.err.println("ERROR: No netlist block found ");
                    System.err.print(it);
                    System.exit(1);
                } else {
                    System.err.println("Warning: No netlist block found ");
                    System.err.print(it);
                    return;
                }
            }
            writeLeaf(cell, cadencizer, emitter, sortNetlist);
        } else {
            writeMidlevel(cell, cadencizer, emitter, inline_layout,
                          handleVerilog, ignoreUnimpl);
        }
    }

    private static Set colonToSet(final String s) {
        return new HashSet(Arrays.asList(StringUtil.split(s, ':')));
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
        final String cellNameArg = theArgs.getArgValue("cell", null);

        if (cellNameArg == null) {
            usage("ERROR: You must specify a cell name.\n");
        }
        
        final String fqcnm;
        if( theArgs.argExists( "cadence-name" ) ) {
            final CadenceReverseNameInterface crni =
                new CadenceReverseNameInterface();
            fqcnm = crni.renameCell(cellNameArg);
        }
        else {
            fqcnm = cellNameArg;
        }

        final CastFileParser castParser =
            new CastFileParser(new FileSearchPath(castRoot), castVersion);
                
        final Cadencize cadencizer =
            new Cadencize(false, Cadencize.NETLIST_PRIORITY);

        final PartialExtract.CellPlusMinus fqcnSpec =
            new PartialExtract.CellPlusMinusKeyword(fqcnm,
                                             PartialExtract.Info.INCLUDE,
                                             castParser, cadencizer);
        final String cellName = fqcnSpec.getTop();
            
        final String translate = theArgs.getArgValue("translate", "cadence");
        final boolean bFlatten = theArgs.argExists("flatten");
        final boolean inline_layout = theArgs.argExists("inline-layout");
        final boolean ignoreUnimpl = theArgs.argExists("ignore-unimplemented");

        final CellInterface ci = loadCell(castParser, cellName);

        final String output = theArgs.getArgValue("output", ci.getFullyQualifiedType() + fqcnSpec.getRest() + ".cdl");

        // list future legal args for pedantic to work.
        pedanticArgs.argTag( "name-map" );
        pedanticArgs.argTag( "lvs-nodes" );
        pedanticArgs.argTag( "process-dependent-name" );

        // put this before any file output is started
        if (! pedanticArgs.pedanticOK( false, true ) ) {
            usage ( pedanticArgs.pedanticString() );
        }

        final FileWriter writer = new FileWriter(output);
       
        // Filter out zero width transistors, 79 columns per line
        final CDLFactoryInterface cdlEmitterFactory =
            new CDLFactoryEmitter(writer, true, 79);

        // Determine the renaming method to use
        final CDLNameInterface cdlNamer;
        if (translate.equals("cadence")) {
            cdlNamer = new CadenceNameInterface();
        } else if (translate.equals("gds2")) {
            cdlNamer = new GDS2NameInterface();
        } else {
            cdlNamer = new IdentityNameInterface();
        }

        // Reload any name mapping
        final String argNameMap = theArgs.getArgValue("name-map", null);
        final String pdkRoot = theArgs.getArgValue("fulcrum-pdk-root", null);

        String nameMap = null;
        if (pdkRoot != null && argNameMap == null) {
            nameMap = pdkRoot + "/share/Fulcrum/lve/transistor.map";
        }
        else if (argNameMap != null) {
            nameMap = argNameMap;
        }
        final CDLNameInterface mapNamer;
        if (nameMap == null) {
            mapNamer = cdlNamer;
        } else {
            mapNamer = new ReloadableNameInterface(cdlNamer);
            try {
                final FileReader r = new FileReader(nameMap);
                ((ReloadableNameInterface) mapNamer).load(r);
                r.close();
            } catch (IOException e) {
                System.err.println("Cannot read from name map file " +
                                   nameMap + ": " + e.getMessage());
                System.exit(2);
            } catch (ReloadableNameInterface.FileFormatException e) {
                System.err.println("Name map file " + nameMap +
                                   " is invalid: " + e.getMessage());
                System.exit(2);
            }
        }

        final CDLFactoryInterface renamerFactory =
            new CDLRenameFactory(cdlEmitterFactory,
                new TrivialCDLNameInterfaceFactory(mapNamer));

        final CDLFactoryInterface templateAccumulatorFactory;
        final Map templates;
        if ( ( fqcnSpec.isEmpty() ) && ( ! bFlatten ) ) {
            templateAccumulatorFactory = renamerFactory;
            templates = null;
        }
        else {
            templates = new LinkedHashMap();
            templateAccumulatorFactory = new Template( templates );
        }

        final CDLFactoryInterface lvsNodesEmitter;
        final String lvsNodesOption = theArgs.getArgValue("lvs-nodes","hierarchy");
        if(lvsNodesOption.equals("none")) {
            lvsNodesEmitter = templateAccumulatorFactory;
        } else {
            final LVSNodes lvsNodesInfo;
            if ( lvsNodesOption.equals("extract"))  {
                lvsNodesInfo = new LVSNodesForExtract( castParser,
                                                       cadencizer );
            } else if(lvsNodesOption.equals("hierarchy")) {
                lvsNodesInfo = new LVSNodesForHierarchy( castParser,
                                                         cadencizer );
            } else {
                lvsNodesInfo = null;
                System.err.println("invalid lvs-nodes option: " + lvsNodesOption );
                System.exit(2);
            }
            lvsNodesEmitter = 
                new LVSNodesCDLFactory( lvsNodesInfo,
                                        templateAccumulatorFactory,
                                        new LVSNodesNullHandler() );
        }


        final CDLFactoryInterface trueNamesEmitter =
            theArgs.argExists("process-dependent-name") ?
                new RealTransistorNames(lvsNodesEmitter, castParser) :
                lvsNodesEmitter;

        final String mosParameters =
            theArgs.getArgValue("cdl-mos-parameters", null);
        final CDLFactoryInterface mosParameterFilter =
            mosParameters == null ? trueNamesEmitter
                                  : new MosParameterFilter(
                                          trueNamesEmitter,
                                          StringUtil.split(mosParameters, ','));

        outputCDL(ci, castParser, mosParameterFilter, 
                  cadencizer, inline_layout, false,
                  false, true, ignoreUnimpl);
        
        final CDLFactoryInterface flattenerFactory;
        if ( bFlatten ) {
            assert templates != null;
            final CDLInlineFactory flattener =
                new CDLInlineFactory( false, null, true );
            flattener.addTargets( templates );
            flattener.setProxy( renamerFactory );
            flattenerFactory = flattener;
        }
        else {
            flattenerFactory = renamerFactory; 
        }

        if (!fqcnSpec.isEmpty()) {
            final PartialExtract pe =
                new PartialExtract(templates, cellName, fqcnSpec);
            pe.execute( flattenerFactory );
        }
        else if ( bFlatten ) {
            final Template topLevel = ( Template ) templates.get( cellName );
            topLevel.execute( flattenerFactory, 
                              Collections.EMPTY_MAP, 
                              NullEnvironment.getInstance(),
                              cellName );
        }
        writer.close();
    }
}
