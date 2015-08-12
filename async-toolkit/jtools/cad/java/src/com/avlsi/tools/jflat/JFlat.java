/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.jflat;


import java.util.Arrays;
import java.util.Comparator;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.OutputStreamWriter;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.HashSet;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.regex.Matcher;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import antlr.RecognitionException;
import antlr.TokenStreamException;

import com.avlsi.cast.CastCacheManager;
import com.avlsi.cast.CastFile;
import com.avlsi.cast.CastFileParser;
import com.avlsi.cast.CastSemanticException;
import com.avlsi.cast.impl.AlintFaninValue;
import com.avlsi.cast.impl.CellInterfaceCollectionIterator;
import com.avlsi.cast.impl.NodeValue;
import com.avlsi.cast.impl.LocalEnvironment;
import com.avlsi.cast.impl.Environment;
import com.avlsi.cast.impl.TupleValue;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.directive.impl.DirectiveTable;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cast2.util.StandardParsingOption;
import com.avlsi.cell.CellDelay;
import com.avlsi.cell.CellImpl;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;
import com.avlsi.cell.ChildrenFirstCellInterfaceIterator;
import com.avlsi.cell.ExclusiveNodeSet;
import com.avlsi.cell.ExclusiveNodeSets;
import com.avlsi.cell.NoSuchEnvironmentException;
import com.avlsi.csp.csp2java.CSP2Class;
import com.avlsi.csp.csp2java.NoCSPBlockException;
import com.avlsi.csp.csp2java.SemanticException;
import com.avlsi.fast.BlockIterator;
import com.avlsi.fast.EnvBlock;
import com.avlsi.fast.NetlistAdapter;
import com.avlsi.fast.NetlistBlock;
import com.avlsi.fast.ports.ChannelType;
import com.avlsi.fast.ports.NodeType;
import com.avlsi.fast.ports.PortDefinition;
import com.avlsi.fast.ports.PortTypeInterface;
import com.avlsi.file.cdl.parser.CDLFactoryAdaptor;
import com.avlsi.file.cdl.parser.CDLSubcktFilter;
import com.avlsi.file.cdl.parser.LVSNodesCDLFactory;
import com.avlsi.file.cdl.parser.LVSNodesNullHandler;
import com.avlsi.file.cdl.util.CDLWriter;
import com.avlsi.file.cdl.util.rename.CadenceNameInterface;
import com.avlsi.file.cdl.util.rename.CDLRenameFactory;
import com.avlsi.file.cdl.util.rename.TrivialCDLNameInterfaceFactory;
import com.avlsi.file.cdl.util.rename.CDLNameInterfaceFactory;
import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.GDS2NameInterface;
import com.avlsi.file.cdl.util.rename.IdentityNameInterface;
import com.avlsi.file.cdl.util.rename.ReloadableNameInterface;
import com.avlsi.file.common.DeviceTypes;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.io.FileSearchPath;
import com.avlsi.io.NullWriter;
import com.avlsi.io.IndentWriter;
import com.avlsi.io.SearchPathFile;
import com.avlsi.io.TrivialSearchPathFile;
import com.avlsi.prs.ProductionRule;
import com.avlsi.prs.ProductionRuleSet;
import com.avlsi.tools.cadencize.CadenceInfo;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.lvs.NetGraph;
import com.avlsi.tools.jauto.PartialExtract;
import com.avlsi.tools.cosim.CoSimParameters;
import com.avlsi.util.bool.BooleanExpressionInterface;
import com.avlsi.util.bool.AndBooleanExpressionInterface;
import com.avlsi.util.bool.OrBooleanExpressionInterface;
import com.avlsi.util.bool.HierNameAtomicBooleanExpression;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.exception.AssertionFailure;
import com.avlsi.util.container.MappingIterator;
import com.avlsi.util.functions.UnaryFunction;
import com.avlsi.util.functions.UnaryPredicate;
import com.avlsi.util.container.AliasedMap;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.CollectionUtils;
import com.avlsi.util.container.FilteringIterator;
import com.avlsi.util.container.MultiMap;
import com.avlsi.util.container.SortingIterator;
import com.avlsi.util.container.StringRepComparator;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.Triplet;
import com.avlsi.util.container.StringContainerIterator;
import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgFormatException;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.CommandLineArgsUtil;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.cmdlineargs.defimpl.PedanticCommandLineArgs;
import com.avlsi.util.text.StringUtil;
import com.avlsi.tools.dsim.InstanceData;
import com.avlsi.tools.jauto.CastQuery.LocalNodes;
import com.avlsi.tools.jauto.CastQuery;

import java.text.MessageFormat;
import com.avlsi.layout.SkillDirectiveEmitter;
import com.avlsi.layout.LVSNodes;
import com.avlsi.layout.LVSNodesForExtract;
import com.avlsi.cast2.util.DirectiveWalker;
import com.avlsi.cast2.util.DirectiveActionInterface;
import com.avlsi.cast2.directive.impl.DirectiveEmitter;
import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.DirectiveBlock;
import com.avlsi.cast2.directive.UnknownDirectiveException;
import com.avlsi.file.cdl.parser.CDLLexer;
import com.avlsi.file.cdl.parser.CDLFactoryInterface;
import com.avlsi.file.cdl.parser.CDLFactoryEmitter;
import com.avlsi.file.cdl.parser.Template;
import com.avlsi.tools.jauto.Cast2Cdl;
import com.avlsi.util.container.ObjectUtils;

/**
 * Java re-implementation of <code>cflat</code>.  
 * <p>
 * Supports two modes: 
 * <ol>
 *   <li> lvs: flattens cell hierarchy, prints prs and connections
 *   <li> csim: flattens cell hierarchy, prints prs and connections in the
 *        csim format, suitable for the dsim program.
 *   <li> auto: prints prs and connections only in the top cell.
 *   <li> instance: prints the names and types of all instantiated cells
 * </ol>
 * <p>
 * Optionally, a specified cell type may be processed, rather
 * than the default of the environment cell.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class JFlat {
    /**
     * This class should not be instantiated.
     **/
    private JFlat() { }

    /**
     * Prints usage message and exits with the specified status.
     **/
    private static void usage( String m ) {
        System.err.println("Usage:\n"
                           + " [ --cast-path=castpath ]\n"
                           + " [ --cast-version=1 | --cast-version=2 ]\n" 
                           + " [ --verbose | --no-verbose ]\n"
                           + " [ --print-stack-trace ]\n"
                           + " [ --output-file=file | --output-dir=dir]\n"
                           + " [ --cast-deps=file --cast-deps-target=file ] "
                           + " [ --output-suffix=string \n"
                           + " [ --cell=cellname[:env|*] | --module=mod.ule ]\n"
                           + " [ --routed ]\n"
                           + " [ --tool=newlvs | --tool=newauto | --tool=dsim\n"
                           + " | --tool=lvs | --tool=auto | --tool=instance\n"
                           + " | --tool=aspice [--internalWires] [--internalRules]\n"
                           + " | --tool=check [--check-all-csp]\n"
                           + " | --tool=new-aspice [--internalRules=[0|1]]\n"
                           + " | --tool=hsim [ --hsim-translate=[ cadence | gds2 | none ]\n"
                           + "                 --hsim-rand-seed=<seed> --hsim-rand-length=<length> ]\n"
                           + " | --tool=cdl [ --cdl-translate=[ cadence | gds2 | none ]\n"
                           + "              [ --cdl-name-map=<name map file> ]\n"
                           + "              [ --cdl-mos-parameters=<param,param,...> ]\n"
                           + "              [ --cdl-call-delimiter=<string> ]\n"
                           + "              [ --cdl-cells=<string> ]\n"
                           + " | --tool=cosim --cosim-merge=<cell> [ --cosim-analog=<cell> ]\n"
                           + " | --tool=env-ntpc \n"
                           + " | --tool=local-nodes [ --nodes=<node,node,...> ] \n" 
                           + " | --tool=dynamic-nodes\n"
                           + " | --tool=signature \n"
                           + " | --tool=fanout [ --ignore-feedback ]\n"
                           + " | --tool=fanin [ --ignore-feedback ]\n"
                           + " [ --tool=query --query-tasks=<task,task,...>\n"
                           + "                --query-filter=<expression>\n"
                           + "                --query-no-recurse\n"
                           + "                --query-translate=[ cadence | gds2 | none ]\n"
                           + "                --query-no-header\n"
                           + "                --query-separator=<string> ]\n"
                           + " [ [--all-cells ] file.cast ]");
        if (m != null && m.length() > 0)
            System.err.print( m );
        System.exit(1);
    }

    private static void usage() {
        usage( null );
    }


    private interface CellFormatterFactory {
        CellFormatterInterface getFormatter(String toolName, String cellName, String envName);
    }

    private static abstract class SimpleCellFormatterFactory implements CellFormatterFactory {
        private final PrintWriterFactory pwf;
        public SimpleCellFormatterFactory(PrintWriterFactory pwf) {
            this.pwf = pwf;
        }
        public CellFormatterInterface getFormatter(String toolName, String cellName, String envName) {
            final Pair<PrintWriter,File> pw =
                pwf.getPrintWriter(toolName,cellName,envName);
            return getFormatter(pw.getFirst());
        }
        public abstract CellFormatterInterface getFormatter(PrintWriter pw);
    }

    private interface PrintWriterFactory {
        Pair<PrintWriter,File>
        getPrintWriter(String toolName, String cellName, String envName);
    }

    private static class FilePrintWriterFactory implements PrintWriterFactory {
        private final PrintWriter pw;
        private final File file;
        public FilePrintWriterFactory(File file) throws IOException {
            this.file = file;
            this.pw = new PrintWriter( new BufferedWriter( new FileWriter( file ) ) );
        }
        public Pair<PrintWriter,File> getPrintWriter(String toolName, String cellName, String envName) {
            return new Pair<PrintWriter,File>(pw, file);
        }
    }

    private static class SimplePrintWriterFactory implements PrintWriterFactory {
        private final PrintWriter pw;
        public SimplePrintWriterFactory(PrintWriter pw) {
            this.pw = pw;
        }
        public Pair<PrintWriter,File> getPrintWriter(String toolName, String cellName, String envName) {
            return new Pair<PrintWriter,File>(pw, null);
        }
    }

    private static CellFormatterFactory
        getCellFormatterFactory( final String tool, 
                                 final String castVersion,
                                 final PrintWriterFactory pwf,
                                 final CastFileParser castParser,
                                 final CommandLineArgs args,
                                 final PartialExtract.CellPlusMinus fqcnSpec,
                                 final Cadencize cadencizer,
                                 final Cadencize cdlCadencizer )
        throws CommandLineArgFormatException {
        if (tool.equals("newlvs")) {
            return new SimpleCellFormatterFactory(pwf) {
                    public CellFormatterInterface getFormatter(PrintWriter pw) {
                        return new LVSFormatter(pw, false, cadencizer );
                    }
                };
        } else if (tool.equals("newauto")) {
            return new SimpleCellFormatterFactory(pwf) {
                    public CellFormatterInterface getFormatter(PrintWriter pw) {
                        return new AutoFormatter(pw, cadencizer );
                    }
                };
        } else if (tool.equals("lvs")) {
            return new SimpleCellFormatterFactory(pwf) {
                    public CellFormatterInterface getFormatter(PrintWriter pw) {
                        return new OldLVSFormatter(pw,cadencizer);
                    }
                };
        } else if (tool.equals("auto")) {
            return new SimpleCellFormatterFactory(pwf) {
                    public CellFormatterInterface getFormatter(PrintWriter pw) {
                        return new OldAutoFormatter(pw, cadencizer);
                    }
                };
        } else if (tool.equals("instance")) {
            return new SimpleCellFormatterFactory(pwf) {
                    public CellFormatterInterface getFormatter(PrintWriter pw) {
                        return new InstanceFormatter(pw);
                    }
                };
        } else if (tool.equals("signature")) {
            return new SimpleCellFormatterFactory(pwf) {
                    public CellFormatterInterface getFormatter(PrintWriter pw) {
                        return new SignatureFormatter(pw, castParser, cadencizer );
                    }
                };
        } else if (tool.equals("aspice")) {
            final boolean iw = args.argExists("internalWires");
            final boolean ir = args.argExists("internalRules");
            return new SimpleCellFormatterFactory(pwf) {
                    public CellFormatterInterface getFormatter(PrintWriter pw) {
                        return new AspiceFormatter(pw, iw, ir, fqcnSpec, castParser, cadencizer, false );
                    }
                };
        } else if (tool.equals("new-aspice")) {
            final Integer ir = CommandLineArgsUtil.getIntegerArgValue(args,
                    "internalRules", new Integer(1));
            final String appendColon =
                args.argExists("no-append-colon") ? "" : ":";
            return new CellFormatterFactory() {
                    public CellFormatterInterface getFormatter(String toolName,
                                                               String cellName,
                                                               String envName){
                        try {
                            final CellInterface cell =
                                castParser.getFullyQualifiedCell( cellName );
                            final CellInterface envCell =
                                (envName == "default") ? null :
                                cell.getEnvironment( envName );
                        
                            // Honor aspice-ignore directive here
                            // by not returning a formatter
                            if( ( envCell != null && ((Boolean) DirectiveUtils.getEnvDirective(envCell, DirectiveConstants.ASPICE_IGNORE)).booleanValue() ) || 
                                ((Boolean) DirectiveUtils.getTopLevelDirective(cell, DirectiveConstants.ASPICE_IGNORE)).booleanValue()) {
                                return null;
                            }
                        } catch (Exception e) {
                            throw new RuntimeException("Cannot load cell: " + cellName, e);
                        }

                        if (envName.equals("default")) {
                            final PrintWriter pw1 = (PrintWriter)
                                pwf.getPrintWriter("aspice",
                                                   cellName,
                                                   envName + 
                                                   File.separator + "prs.asp")
                                   .getFirst();
                            final CellFormatterInterface f1 = 
                                new NewAspiceFormatter(pw1,
                                                       true,
                                                       ir.intValue() != 0,
                                                       fqcnSpec,
                                                       null,
                                                       castParser,
                                                       cdlCadencizer,
                                                       cadencizer,
                                                       false,
                                                       appendColon);
                            final PrintWriter pw2 = (PrintWriter)
                                pwf.getPrintWriter("aspice",
                                                   cellName,
                                                   envName + 
                                                   File.separator + "noprs.asp")
                                   .getFirst();
                            final Pair modifyPair;
                            final PartialExtract.CellPlusMinusKeyword partialSimSpec;
                            if (fqcnSpec.isEmpty()) {
                                partialSimSpec =
                                    new PartialExtract.CellPlusMinusKeyword(
                                            cellName + "-+localportnodes",
                                            PartialExtract.Info.INCLUDE);
                                modifyPair =
                                    pwf.getPrintWriter("aspice", cellName,
                                            envName + File.separator +
                                            "noprs.modify.asp");
                            } else {
                                partialSimSpec = null;
                                modifyPair = null;
                            }

                            final CellFormatterInterface f2 = 
                                new NewAspiceFormatter(pw2,
                                                       modifyPair,
                                                       true,
                                                       false, 
                                                       fqcnSpec,
                                                       partialSimSpec,
                                                       castParser,
                                                       cdlCadencizer,
                                                       cadencizer,
                                                       false,
                                                       appendColon);
                            return new AggregateFormatter(f1, f2);
                        } else {
                            final PrintWriter pw = (PrintWriter)
                                pwf.getPrintWriter("aspice", cellName, envName + File.separator + "env.asp").getFirst();
                            return new NewAspiceFormatter(pw, true, true, fqcnSpec, null, castParser, cdlCadencizer, cadencizer, false, appendColon);
                        }
                    }
                };
        } else if (tool.equals("dsim")) {
            return new SimpleCellFormatterFactory(pwf) {
                    public CellFormatterInterface getFormatter(PrintWriter pw) {
                        return new AspiceFormatter( pw, true, true, fqcnSpec,
                                                    castParser, cadencizer,
                                                    true );
                    }
                };
        } else if (tool.equals("new-dsim")) {
            final String appendColon =
                args.argExists("no-append-colon") ? "" : ":";
            return new SimpleCellFormatterFactory(pwf) {
                    public CellFormatterInterface getFormatter(PrintWriter pw) {
                        return new NewAspiceFormatter( pw, true, true, fqcnSpec,
                                                       null, castParser,
                                                       cdlCadencizer,
                                                       cadencizer, true,
                                                       appendColon);
                    }
                };
        } else if (tool.equals("local-nodes") || tool.equals("node-props")) {
            return new SimpleCellFormatterFactory(pwf) {
                    public CellFormatterInterface getFormatter(PrintWriter pw) {
                        final String nodesStr = 
                            args.getArgValue("nodes", "");
                        final Set nodes = (nodesStr.equals("")) ? null :
                            new HashSet(Arrays.asList( nodesStr.split(",")));
                        return new LocalNodesFormatter(pw,castParser,cadencizer,tool.equals("node-props"),nodes);
                        
                    }
                };
        } else if (tool.equals("local-aliases")) {
            return new SimpleCellFormatterFactory(pwf) {
                    public CellFormatterInterface getFormatter(PrintWriter pw) {
                        return new LocalAliasesFormatter(pw,castParser,cadencizer);
                        
                    }
                };
        } else if (tool.equals("leaky-nodes")) {
            return new SimpleCellFormatterFactory(pwf) {
                public CellFormatterInterface getFormatter(PrintWriter pw) {
                    return new LeakyNodesFormatter(pw,castParser,cadencizer);
                    
                }
            };
        } else if (tool.equals("alint-scenarios")) {
            return new SimpleCellFormatterFactory(pwf) {
                public CellFormatterInterface getFormatter(PrintWriter pw) {
                    return new AlintScenariosFormatter(pw, cadencizer);
                    
                }
            };
        } else if (tool.equals("cdl")) {

            final String renameStr = 
                args.getArgValue("cdl-translate", "cadence");
            final CDLNameInterface nameInterface;
            if(renameStr.equals("cadence")) {
                nameInterface = new CadenceNameInterface( true );
            }
            else if(renameStr.equals("gds2")) {
                nameInterface = new GDS2NameInterface();
            }
            else {
                nameInterface = new IdentityNameInterface();
            }

            final String argNameMap = args.getArgValue("cdl-name-map", null);
            final String argPdkRoot = args.getArgValue("fulcrum-pdk-root", null);
            String nameMap = null;
            if (argPdkRoot != null && argNameMap == null) {
                nameMap = argPdkRoot + "/share/Fulcrum/lve/transistor.map";
            }
            else if (argNameMap != null) {
                nameMap = argNameMap;
            }
            final CDLNameInterface mapNamer;
            if (nameMap == null) {
                mapNamer = nameInterface;
            } else {
                mapNamer = new ReloadableNameInterface(nameInterface);
                try {
                    final FileReader r = new FileReader((String) nameMap);
                    ((ReloadableNameInterface) mapNamer).load(r);
                    r.close();
                } catch (IOException e) {
                    System.err.println("Cannot read from CDL name map file " +
                                       nameMap + ": " + e.getMessage());
                    return null;
                } catch (ReloadableNameInterface.FileFormatException e) {
                    System.err.println("CDL name map file " + nameMap +
                                       " is invalid: " + e.getMessage());
                    return null;
                }
            }

            final CDLNameInterfaceFactory nameInterfaceFactory =
                new TrivialCDLNameInterfaceFactory(mapNamer);

            final String mosParamStr =
                args.getArgValue("cdl-mos-parameters", null);
            final String[] mosParams =
                mosParamStr == null ? null
                                    : StringUtil.split(mosParamStr, ',');
            final String callDelimiter =
                args.getArgValue("cdl-call-delimiter", "/");
            final String cdlCellStr = args.getArgValue("cdl-cells", null);
            final String[] cdlCells =
                cdlCellStr == null ? new String[0]
                                   : StringUtil.split(cdlCellStr, ':');

            return new CellFormatterFactory () {
                    public CellFormatterInterface 
                        getFormatter(String toolName,
                                     String cellName,
                                     String envName) {
                        if ( envName == "default" ) {
                            final PrintWriter pw = (PrintWriter)
                                pwf.getPrintWriter("cdl", cellName, envName)
                                   .getFirst();
                            // Use a new instance of Cadencize, because the
                            // CDLFormatter never takes into account routed
                            return new CDLFormatter(
                                pw, fqcnSpec, castParser, nameInterfaceFactory,
                                mosParams, callDelimiter, cdlCells,
                                cdlCadencizer);
                        } else {
                            return null;
                        }
                    }
                };
        } else if (tool.equals("hsim")) {
            final String lenStr = args.getArgValue("hsim-rand-length", "10");
            final String seedStr = args.getArgValue("hsim-rand-seed", "0");
            final int len;
            final long seed;
            try {
                len = Integer.parseInt(lenStr);
                seed = Long.parseLong(seedStr);
            } catch (NumberFormatException e) {
                System.err.println("Invalid rand-length or rand-seed options specified.");
                return null;
            }
            final String renameStr = args.getArgValue("hsim-translate", "cadence");
            final CDLNameInterface renamer;
            if (renameStr.equals("cadence")) {
                renamer = new CadenceNameInterface(true);
            } else if (renameStr.equals("gds2")) {
                renamer = new GDS2NameInterface();
            } else if (renameStr.equals("none")) {
                renamer = new IdentityNameInterface();
            } else {
                System.err.println("Invalid name translator specified: " + renameStr);
                return null;
            }
            return new SimpleCellFormatterFactory(pwf) {
                    public CellFormatterInterface getFormatter(PrintWriter pw) {
                        return new HSIMFormatter(pw, castParser, len, seed, renamer, cadencizer );
                    }
                };
        } else if (tool.equals("env-ntpc")) {
            return new SimpleCellFormatterFactory(pwf) {
                    public CellFormatterInterface getFormatter(PrintWriter pw) {
                        return new EnvNTPCFormatter(pw, cadencizer);
                    }
                };
        } else if (tool.equals("check")) {
            final boolean checkAll = args.argExists("check-all-csp");
            final boolean verbose = args.argExists("verbose");
            return new CellFormatterFactory () {
                    public CellFormatterInterface getFormatter(String toolName, 
                                                               String cellName,
                                                               String envName) {
                        return new CheckFormatter(checkAll, verbose);
                    }
                };
        } else if (tool.equals("cosim")) {
            final String mergeCell = args.getArgValue("cosim-merge", null);
            if (mergeCell == null) {
                System.err.println("cosim-merge must be specified");
                return null;
            }
            final String analogCell = args.getArgValue("cosim-analog", null);
            if (analogCell == null) {
                System.err.println("cosim-analog must be specified");
                return null;
            }
            
            final CellInterface merge;
            try {
                merge = castParser.getFullyQualifiedCell(mergeCell);
            } catch (Exception e) {
                throw new RuntimeException("Cannot load cell: " + mergeCell, e);
            }
            
            return new SimpleCellFormatterFactory(pwf) {
                    public CellFormatterInterface getFormatter(PrintWriter pw) {
                        return new CosimFormatter(pw, analogCell, merge, cadencizer );
                    }
                };
        }
        else if ( tool.equals( "query" ) ) {
            final String tasks = args.getArgValue( "query-tasks", "" );
            final String filter = args.getArgValue( "query-filter", "" );
            final String prune = args.getArgValue( "query-prune", "" );
            final boolean noRecurse = args.argExists( "query-no-recurse" );
            final String translatorName = args.getArgValue( "query-translate", "none" );
            final boolean noHeader = args.argExists( "query-no-header" );
            // accept misspelled arg and correctly spelled arg too
            final String sep;
            if ( args.argExists( "query-seperator" ) )
                sep = args.getArgValue( "query-seperator", ",");
            else
                sep = args.getArgValue( "query-separator", ",");
            
            return new SimpleCellFormatterFactory( pwf ) {
                    public CellFormatterInterface getFormatter( final PrintWriter pw ) {
                        return new SimpleCellFormatter() {
                                public void outputCell( final CellInterface cell,
                                                        final CellInterface envCell ) {
                                    if ( envCell == null ) {
                                        try {
                                            CastQuery.query( castParser,
                                                             cell,
                                                             tasks,
                                                             filter,
                                                             prune,
                                                             noRecurse,
                                                             translatorName,
                                                             noHeader,
                                                             false,
                                                             sep,
                                                             pw );
                                        }
                                        catch ( RecognitionException e ) {
                                            throw new RuntimeException( e );
                                        }
                                        catch ( TokenStreamException e ) {
                                            throw new RuntimeException( e );
                                        }
                                        catch ( IOException e ) {
                                            throw new RuntimeException( e );
                                        }
                                        catch ( CastSemanticException e ) {
                                            throw new RuntimeException( e );
                                        }
                                    }
                                }
                                public CellInterface prepCell( final CellInterface cell ) {
                                    return cell;
                                }
                            };
                    }
                };
        } else if (tool.equals("fanout") || tool.equals("fanin")) {
            return new SimpleCellFormatterFactory(pwf) {
                public CellFormatterInterface getFormatter(PrintWriter pw) {
                    final String nodesStr = args.getArgValue("nodes", "");
                    final Set<String> nodes = (nodesStr.equals("")) ? null :
                        new HashSet<String>(Arrays.asList(nodesStr.split(":")));
                    return tool.equals("fanin") ?
                        new FaninFormatter (pw, castParser, cdlCadencizer,
                            cadencizer, nodes, args.argExists("ignore-feedback"),
                            args.argExists("routed")) :
                        new FanoutFormatter(pw, castParser, cdlCadencizer,
                            cadencizer, nodes, args.argExists("ignore-feedback"),
                            args.argExists("routed"));
                    
                }
            };
        }
        usage();
        return null; // Sop to the compiler.            
    }

    /**
     * Given a cell and the name of an environment, return the specified
     * environment, or return <code>null</code> if the environment does not
     * exist.
     **/
    private static CellInterface getEnvironment(final CellInterface cell,
                                                final String name) {
        try {
            return cell.getEnvironment(name);
        } catch (NoSuchEnvironmentException e) {
            return null;
        }
    }
    
    /**
     * Main method.
     **/
    public static void main(String[] args) throws Exception {
        
        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
        final CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles( parsedArgs ); 

        final CommandLineArgs cachedArgs = 
            new CachingCommandLineArgs( argsWithConfigs );

        final PedanticCommandLineArgs pedanticArgs = 
            new PedanticCommandLineArgs( cachedArgs );

        final CommandLineArgs theArgs = pedanticArgs;
        // pick apart the command line
        
        if (theArgs.argExists("version")) {
            System.out.println(
                               com.avlsi.util.debug.VersionInfo.getVersionString(JFlat.class));
        }
        
        final String toolNames = theArgs.getArgValue("tool", null );  

        final String cellAndEnv = theArgs.getArgValue("cell", null );
        final String moduleName = theArgs.getArgValue("module", null );
        final String castVersion= theArgs.getArgValue("cast-version",null);
        final String outputFile = theArgs.getArgValue("output-file",null );
        final String outputDir = theArgs.getArgValue("output-dir",null );
        final String castDepsFile = theArgs.getArgValue("cast-deps", null);
        final String castDepsTarget = theArgs.getArgValue("cast-deps-target", castDepsFile);
        final String outputSuffix = theArgs.getArgValue("output-suffix","" );
        final boolean verbose = ! theArgs.argExists("no-verbose");
        final boolean routed = theArgs.argExists("routed");
        final boolean printStackTrace = theArgs.argExists("print-stack-trace");

        /** @review jmr Should we use --cell=mod.ule.* for --module? **/
        if ((cellAndEnv != null) && (moduleName != null)) {
            usage ("Can't specify both --cell and --module\n");
        }

        final Collection castDeps = new HashSet();
        final FileSearchPath.SearchPathFileFactory spfFactory;
        if (castDepsFile == null) {
            spfFactory = new FileSearchPath.DefaultSearchPathFileFactory();
        } else {
            spfFactory = new FileSearchPath.TrackingSearchPathFileFactory(castDeps);
        }

        final FileSearchPath castSearchPath = 
            new FileSearchPath( theArgs.getArgValue( "cast-path", "." ),
                                System.getProperty( "user.home" ),
                                spfFactory );
        
        final boolean doAllCells= theArgs.argExists("all-cells");

        if ( toolNames == null || ! pedanticArgs.pedanticOK( false, true )) {
            usage( pedanticArgs.pedanticString() );
        }

        final PrintWriterFactory pwFactory;

        if ( outputFile != null ) {
            pwFactory = new FilePrintWriterFactory( new File(outputFile) );
        }
        else if ( outputDir != null ) {
            pwFactory = new PrintWriterFactory() {
                    public Pair<PrintWriter,File>
                    getPrintWriter(String toolName, String cellName,
                                   String envName ) {
                        Pair<PrintWriter,File> ret;
                        try {
                            final File dir = new File(outputDir, toolName);
                            final File file = new File(dir,envName+outputSuffix);
                            final File fileDir = file.getParentFile();
                            if (fileDir != null) fileDir.mkdirs();
                            ret = new Pair<PrintWriter,File>(new PrintWriter( new BufferedWriter( new FileWriter( file ) ) ), file );
                        } catch( IOException e ) {
                            throw new RuntimeException(e);
                        }
                        return ret;
                    }
                };
        }
        else {
            pwFactory = new SimplePrintWriterFactory( new PrintWriter(System.out) );
        }

        final Cadencize cadencizer = new Cadencize( true );

        // Cadencize instance used for cdl and fanout tasks, which do not want
        // a routed view and needs the netlist block to have priority
        final Cadencize cdlCadencizer =
            new Cadencize(true, Cadencize.NETLIST_PRIORITY);

        final CastFileParser castParser =
            CastCacheManager.getDefault().getCastFileParser(
                    castSearchPath, castVersion, verbose,
                    new StandardParsingOption(theArgs));

        //try everything we do with a castParser
        //and catch CastSemanticException so we can pretty print
        try {
            // split cellAndEnv into cellName and envName
            
            final PartialExtract.CellPlusMinusKeyword fqcnSpec;
            boolean fqcnSpecInit;
            final String cellName;
            final String envName;
            if (cellAndEnv == null) {
                fqcnSpec = null;
                cellName = null;
                envName = null;
                fqcnSpecInit = true;
            } else {
                fqcnSpec = new PartialExtract.CellPlusMinusKeyword(cellAndEnv, PartialExtract.Info.INCLUDE);
                cellName = fqcnSpec.getTop();
                envName = fqcnSpec.getEnvironment();
                fqcnSpecInit = false;
            }

            final Map toolMap = new LinkedHashMap();
            final String[] tools = toolNames.split(",");
            for(int i=0; i<tools.length; i++ ) {
                final CellFormatterFactory cellFormatterFactory = 
                    getCellFormatterFactory(tools[i],
                                            castVersion,
                                            pwFactory,
                                            castParser,
                                            theArgs,
                                            fqcnSpec,
                                            cadencizer,
                                            cdlCadencizer);
                toolMap.put(tools[i],cellFormatterFactory);
                // check that a valid tool option has been specified
                if ( cellFormatterFactory == null ) {
                    usage();
                }
            }

        
            final CellInterface cellToProcess;
            final CellInterface envCellToProcess;

            //Get the file name of a cast file if one was specified.
        
            final String castFileName;
            StringContainerIterator castFilesIter = theArgs.nonParsedArgumentsIterator();
            if ( castFilesIter.hasNext() ) {
                castFileName = castFilesIter.next();
            
                //Can only specify one file now.
                if ( castFilesIter.hasNext() ) {
                    usage();
                }
            }
            else {
                castFileName = null;
            }

            final HashSet cellEnvSet = new HashSet();
            
            if ( ! doAllCells ) {
                if (moduleName != null) {
                    final CastFile cf = castParser.parseModule(moduleName);
                    
                    CellInterfaceCollectionIterator cellsIter =
                        cf.getAllCellsPossible();
                    
                    while (cellsIter.hasNext()) {
                        final CellInterface currCellToProcess = cellsIter.next();
                        cellEnvSet.add( new Triplet( currCellToProcess, getEnvironment(currCellToProcess, "default"), "default") );
                    }
                } else {
                    if ( cellName != null ) { //If a cell name was specified then get a CellInterface
                        //for that cell.                            
                        if ( cellName.lastIndexOf( '.' ) >= 0 ) {
                            //We have fully qualified cell name, thus we let
                            //CastFileParser figure out which cast file to
                            //parse and return a CellInterface.
                            cellToProcess = castParser.getFullyQualifiedCell( cellName );
                        }
                        else {
                            //The cell name was not fully qualified.
                            
                            //Make sure that they gave the name of a file
                            //in which to find the cell they asked for.
                            if ( castFileName == null ) {
                                usage();
                            }
                            
                            //Parse the cast file that was specified, and get
                            //a CellInterface for the cell they asked for.
                            final CastFile cf = castParser.parse(castFileName);
                            cellToProcess = cf.getCell(cellName); 
                        }
                        if (envName != null && envName.equals("*")) {
                            EnvBlock envBlock = cellToProcess.getEnvironments();
                            cellEnvSet.add( new Triplet(cellToProcess, getEnvironment(cellToProcess, "default") ,"default"));
                            Set<String> envs = new HashSet<String>();
                            for (Iterator i = envBlock.getNames(); i.hasNext(); ) {
                                // get the environment name
                                final String existingEnvName = (String) i.next();
                                envs.add(existingEnvName);
                                // get the environment cell
                                final CellInterface existingEnvCell =
                                    envBlock.getNamedEnvironment(existingEnvName);
                                cellEnvSet.add( new Triplet(cellToProcess,existingEnvCell,existingEnvName));
                            }
                            if (!envs.contains("leakage")) {
                                final CellInterface existingEnvCell =
                                    envBlock.getNamedEnvironment("leakage");
                                if (existingEnvCell != null) {
                                    cellEnvSet.add( new Triplet(cellToProcess,existingEnvCell,"leakage"));
                                }
                            }
                        }
                        else {
                            if(envName != null) {
                                envCellToProcess = cellToProcess.getEnvironment(envName);
                                cellEnvSet.add( new Triplet(cellToProcess,envCellToProcess,envName) );
                            }
                            else {
                                envCellToProcess = null;
                                cellEnvSet.add( new Triplet(cellToProcess,envCellToProcess,"default") );
                            }
                            
                        }
                    }
                    else {
                        //No cell name was specified, thus get the CellInterface for
                        //the environment of the cast file they specified.

                        //make sure a cast file name was specified.
                        if ( castFileName == null ) {
                            usage();
                        }

                        // do the parsing thang
                        final CastFile cf = castParser.parse(castFileName);
                        
                        // get the appropriate cell, either environment or named
                        final CellInterface cell;
                        cellToProcess = cf.getEnvironmentCell();
                        envCellToProcess = getEnvironment(cellToProcess, "default");
                        
                        cellEnvSet.add( new Triplet(cellToProcess,envCellToProcess,"default"));
                    }
                }
            }
            else {
                if ( castFileName == null ) {
                    usage();
                }

                final CastFile cf = castParser.parse( castFileName );                
                CellInterfaceCollectionIterator cellsIter = cf.getAllCellsPossible();

                while ( cellsIter.hasNext() ) {
                    final CellInterface currCellToProcess = cellsIter.next();
                    cellEnvSet.add( new Triplet( currCellToProcess, null, "default") );
                }
            }
            final Map routedCells = new HashMap();
            for (Iterator i = cellEnvSet.iterator(); i.hasNext(); ) {
                final Triplet cellEnvName = (Triplet) i.next();
                final CellInterface cell = 
                    (CellInterface) cellEnvName.getFirst();
                final String type = cell.getFullyQualifiedType();
                if (!routedCells.containsKey(type)) {
                    routedCells.put(type, routed ? cell.routedSubcells(false)
                                                 : cell);
                }
            }
            for (final Iterator j = toolMap.entrySet().iterator(); j.hasNext(); ) {

                Map.Entry tool_Formatter = (Map.Entry) j.next();
                final String toolName = (String) tool_Formatter.getKey();
                final CellFormatterFactory cellFormatterFactory = 
                    (CellFormatterFactory) tool_Formatter.getValue();

                for (final Iterator i = cellEnvSet.iterator(); i.hasNext(); ) {
                    final Triplet cellEnvName = (Triplet) i.next();
                    final CellInterface cell = 
                        (CellInterface) cellEnvName.getFirst();
                    final CellInterface routedCell = (CellInterface)
                        routedCells.get(cell.getFullyQualifiedType());
                    if (!fqcnSpecInit) {
                        fqcnSpec.initialize(cell, cadencizer);
                        fqcnSpecInit = true;
                    }
                    final CellInterface envCell =
                        (CellInterface) cellEnvName.getSecond();
                    final String envCellName = 
                        (String) cellEnvName.getThird();
                    
                    final CellFormatterInterface formatter = 
                        cellFormatterFactory.
                        getFormatter(toolName,
                                     routedCell.getFullyQualifiedType(),
                                     envCellName );
                    if(formatter != null ) {
                        final CellInterface currPrepedCell =
                            formatter.prepCell(cell, routedCell);
                        formatter.outputCell(currPrepedCell,envCell);
                    }
                }
            }
            
            if (castDepsFile != null) {
                final FileWriter w = new FileWriter(castDepsFile);
                w.write("CASTDEP_TARGET := "+castDepsTarget+"\n");
                w.write("$(CASTDEP_TARGET) : \\\n");
                for (Iterator i = castDeps.iterator(); i.hasNext(); ) {
                    w.write(i.next() + " \\\n");
                }
                w.close();
            }
        } catch (CastSemanticException e) {
            // Pretty print CAST exceptions
            com.avlsi.tools.dsim.ExceptionPrettyPrinter.printException(e, System.err);
            if (printStackTrace) e.printStackTrace(System.err);
            System.exit(1);
        } catch (NoSuchEnvironmentException e) {
            System.err.println("Cannot instantiate environment " +
                               e.getEnvironmentName() + " from cell " +
                               e.getCellName());
            com.avlsi.tools.dsim.ExceptionPrettyPrinter.printException(e, System.err);
            if (printStackTrace) e.printStackTrace(System.err);
            System.exit(1);
        } catch (CommandLineArgFormatException e) {
            System.err.println("Invalid argument " + e.getMessage() + " for switch " + e.getArgName());
            System.exit(1);
        }
    }

    public interface CellProcessor {
        void process( HierName prefix, CellInterface cell, 
                      AliasedSet superNamespace, int depth,
                      final Cadencize cadencizer );
    }

    private interface CellFormatterInterface {
        void outputCell(CellInterface cell, CellInterface envCell)
            throws CellFormatterException, IOException;
        CellInterface prepCell(CellInterface cell, CellInterface routed);
    }

    private abstract static class SimpleCellFormatter
        implements CellFormatterInterface {
        public abstract void outputCell(CellInterface cell,
                                        CellInterface envCell)
            throws CellFormatterException, IOException;
        public CellInterface prepCell(CellInterface cell,
                                      CellInterface routed) {
            return prepCell(routed);
        }
        public CellInterface prepCell(CellInterface cell) {
            return cell;
        }
    }

    private static class CellFormatterException extends Exception {
        CellFormatterException(final String message) { super(message); }
        CellFormatterException() { super(); }
    }

    private static HierName makeHierName(final String s, final char sep) {
        try {
            return HierName.makeHierName(s, sep);
        } catch (InvalidHierNameException e) {
            throw new AssertionError("Cannot construct HierName from " + s +
                                     " with separator " + sep);
        }
    }

    private static HierName makeHierName(final String s) {
        return makeHierName(s, '.');
    }

    /**
     * Prints out any nodes with the <code>ntpc_spec</code> directive set,
     * and their number of transitions per cycle.
     **/
    private static class EnvNTPCFormatter extends SimpleCellFormatter {
        private final PrintWriter pw;
        private final Cadencize cad;

        public EnvNTPCFormatter(PrintWriter pw, Cadencize cad) {
            this.pw = pw;
            this.cad = cad;
        }

        public CellInterface prepCell(CellInterface cell) {
            return cell;
        }

        public void outputCell(CellInterface cell, CellInterface envCell)
            throws IOException {
            
            if(envCell != null ) {
                printEnvCell(cell, envCell, null);
            }
            else {
                // for all environments
                final EnvBlock envBlock = cell.getEnvironments();
                for (Iterator i = envBlock.getNames(); i.hasNext(); ) {
                    // get the environment name
                    final String envName = (String) i.next();
                    // get the environment cell
                    final CellInterface env;
                    try {
                        env = envBlock.getNamedEnvironment(envName);
                    } catch (CastSemanticException e) {
                        throw new RuntimeException("Cannot load environment " +
                                                   envName, e);
                    }
                    
                    printEnvCell(cell, env, envName);
                }
            }
            // checkError also flushes
            if (pw.checkError())
                throw new IOException();
        }

        private void printInfo(final String envName, 
                               final HierName nodeName,
                               final Float ntpcVal,
                               final Float ntpcSignoffVal,
                               final Integer digitalCycles,
                               final Integer analogCycles,
                               final Float timeMax,
                               final HierName canonicalName
                               ) {
            pw.println( ( envName == null ? "" : envName + " " ) +
                        nodeName.getAspiceString() + " " +
                        ( ntpcVal == null ? "-1" : "" + ntpcVal ) + " " +
                        ( ntpcSignoffVal == null ? "-1" : "" + ntpcSignoffVal ) + " " +
                        ( digitalCycles == null ? "-1" : "" + digitalCycles ) + " " +
                        ( analogCycles == null ? "-1" : "" + analogCycles ) + " " +
                        // the directive is specified in ns, we output in s
                        ( timeMax == null ? "-1" : "" + timeMax.floatValue() * 1e-9 ) + " " +
                        ( canonicalName == null ? nodeName.getAspiceString()
                                                : canonicalName.getAspiceString()) );
        }

        private void printEnvCell(CellInterface cell,
                                  CellInterface envCell,
                                  String envName) throws IOException {

            // get all ntpc_spec nodes and print the ntpc value
            final Map ntpcMap =
                DirectiveUtils.scaleNtpcSpec
                (cell,
                 DirectiveUtils.getEnvDirective
                 (envCell,
                  DirectiveConstants.NTPC_SPEC,
                  DirectiveConstants.NODE_TYPE ) );

            final float scaleSignoff =
                ((Float) DirectiveUtils.getTopLevelDirective
                 (cell,
                  DirectiveConstants.NTPC_SCALING_SIGNOFF)).floatValue();

            final Map ntpcSignoffMap =
                DirectiveUtils.scaleFloatMap
                ( ntpcMap,
                  scaleSignoff );

            final Map digitalCyclesMap = 
                DirectiveUtils.getMultipleBlockDirective
                ( Arrays.asList( new BlockInterface[]
                    { cell.getBlockInterface(),
                      DirectiveUtils.getUniqueBlock
                      ( envCell.getBlockInterface(),
                        BlockInterface.ENV ) } ),
                  DirectiveConstants.ASPICE_DIGITAL_CYCLES,
                  DirectiveConstants.NODE_TYPE);

            final Map analogCyclesMap = 
                DirectiveUtils.getMultipleBlockDirective
                ( Arrays.asList( new BlockInterface[]
                    { cell.getBlockInterface(),
                      DirectiveUtils.getUniqueBlock
                      ( envCell.getBlockInterface(),
                        BlockInterface.ENV ) } ),
                  DirectiveConstants.ASPICE_ANALOG_CYCLES,
                  DirectiveConstants.NODE_TYPE);

            final Set nodeSet = new HashSet();
            nodeSet.addAll(analogCyclesMap.keySet());
            nodeSet.addAll(digitalCyclesMap.keySet());
            nodeSet.addAll(ntpcMap.keySet());

            final HierName cycleNode = 
                (HierName) DirectiveUtils.getEnvDirective
                (envCell, 
                 DirectiveConstants.CYCLE_NODE);            
            if(cycleNode != null ) nodeSet.add(cycleNode);

            final Float timeMax = (Float) DirectiveUtils.getEnvDirective
                (envCell,
                 DirectiveConstants.ASPICE_TIME_MAX);

            final AliasedSet localNodes = cad.convert(cell).getLocalNodes();
            for (final Iterator i = 
                     new SortingIterator( nodeSet.iterator() );
                 i.hasNext(); ) {
                final HierName nodeName = (HierName) i.next();
                printInfo(envName, 
                          nodeName,
                          (Float)ntpcMap.get(nodeName),
                          (Float)ntpcSignoffMap.get(nodeName),
                          (Integer)digitalCyclesMap.get(nodeName),
                          (Integer)analogCyclesMap.get(nodeName),
                          timeMax,
                          (HierName) localNodes.getCanonicalKey(nodeName));
            }
        }
    }

    public static class CheckFormatter extends SimpleCellFormatter {
        private final boolean checkAll;
        private final boolean verbose;
        public CheckFormatter(final boolean checkAll, final boolean verbose) {
            this.checkAll = checkAll;
            this.verbose = verbose;
        }

        private void process(final boolean top, final Set<String> seen,
                             final CellInterface cell) {
            if (!seen.add(cell.getFullyQualifiedType())) return;

            final String type = cell.getFullyQualifiedType();
            if (top || verbose) System.out.print("Checking " + type + "...");
            compileCsp(top, cell);
            if (top || verbose) System.out.println("Checked.");

            if (checkAll) {
                for (Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
                    Pair p = (Pair) i.next();
                    CellInterface subcell = (CellInterface) p.getSecond();
                    process(false, seen, subcell);
                }
            }
        }

        private void compileCsp(final boolean top, final CellInterface cell) {
            final String cellName = cell.getFullyQualifiedType();
            if (cell.containsRunnableCsp()) {
                try {
                    final PrintWriter myNullPrintWriter =
                        new PrintWriter( NullWriter.getInstance() );
                    final PrintWriter myStdOutputWriter =
                        new PrintWriter( new OutputStreamWriter( System.out ) );

                    // XXX: what should value for emitCoverageProbes be?
                    new CSP2Class(myNullPrintWriter, myStdOutputWriter,
                                  myNullPrintWriter, false).compile(cell);
                }
                catch( NoCSPBlockException e ) {
                    throw new AssertionError("Runnable CSP in " + cellName +
                                             " but no CSP block!");
                }
                catch( SemanticException e ) {
                    System.out.println( "CSP for " + cellName +
                                        " failed to compile:\n" + e );
                }
                catch( IOException e ) {
                    System.out.println( "CSP for " + cellName +
                                        " failed to compile:\n" + e );
                }
            } else if (top) {
                // Excuse the lack of CSP if this is a fragment cell
                if (!Boolean.TRUE
                    .equals((Boolean)DirectiveUtils
                            .getTopLevelDirective(cell,
                                                  DirectiveConstants.FRAGMENT)))

                    System.out.println("no csp in " + cellName);
            }
        }

        public CellInterface prepCell( CellInterface cell ) {
            process(true, new HashSet<String>(), cell);
            
            return null;
        }

        public void outputCell(CellInterface cell, CellInterface envCell)
            throws CellFormatterException, IOException {
            if (envCell != null) {
                prepCell(envCell);
            }
        }
    }

    /**
     * A helper class for AspiceFormatter and NewAspiceFormatter.  It converts
     * transistors, resistors, and capacitors in a netlist block to aspice.
     **/
    private static class Netlist2Aspice implements CDLFactoryInterface {
        private final HierName prefix;
        private final PrintWriter pw;
        private final int[] errors;
        public Netlist2Aspice(final HierName prefix, final PrintWriter pw,
                              int[] errors) {
            this.prefix = prefix;
            this.pw = pw;
            this.errors = errors;
        }
        private String soft(CDLLexer.InfoToken val, Environment env) {
            final Double eval = val.getValue(env);
            if (eval == null) return val.getText();
            else return eval.toString();
        }
        private String param(Map parameters, Environment env, String name,
                             String def) {
            Object token = parameters.get(name);
            if (token == null) token = parameters.get(name.toUpperCase());
            return token == null ? def
                : soft((CDLLexer.InfoToken) token, env);
        }
        public void makeResistor(HierName name, HierName n1, HierName n2,
                                 CDLLexer.InfoToken val, Map parameters,
                                 Environment env) {
            pw.print("res(");
            OldLVSFormatter.printName(prefix, n1, pw);
            pw.print(", ");
            OldLVSFormatter.printName(prefix, n2, pw);
            pw.println(") (" + soft(val, env) + ")");
        }

        public void makeCapacitor(HierName name, HierName npos,
                                  HierName nneg, CDLLexer.InfoToken val,
                                  Map parameters, Environment env) {
            pw.print("cap(");
            OldLVSFormatter.printName(prefix, npos, pw);
            pw.print(", ");
            OldLVSFormatter.printName(prefix, nneg, pw);
            pw.println(") (" + soft(val, env) + ")");
        }

        public void makeTransistor(HierName name, String type, HierName ns,
                                   HierName nd, HierName ng, HierName nb,
                                   CDLLexer.InfoToken w,
                                   CDLLexer.InfoToken l, Map parameters,
                                   Environment env) {
            pw.print(type.charAt(0) == 'n' || type.charAt(0) == 'N' ?
                     "nch_mac" : "pch_mac");
            pw.print(" (");
            OldLVSFormatter.printName(prefix, ns, pw);
            pw.print(", ");
            OldLVSFormatter.printName(prefix, ng, pw);
            pw.print(", ");
            OldLVSFormatter.printName(prefix, nd, pw);
            pw.print(", ");
            OldLVSFormatter.printName(prefix, nb, pw);
            pw.println(") (" + soft(w, env) + ", " + soft(l, env) + ", " +
                       param(parameters, env, "as", "0") + ", " +
                       param(parameters, env, "ps", "0") + ", " +
                       param(parameters, env, "nrs", "0") + ", " +
                       param(parameters, env, "ad", "0") + ", " +
                       param(parameters, env, "pd", "0") + ", " +
                       param(parameters, env, "nrd", "0") + ")");
        }

        private void error(final String device, final HierName name) {
            ++errors[0];
            //System.err.println("aspice output ignoring " + device + " named " + prefix + "." + name + " in the netlist block");
        }

        public void makeDiode(HierName name, String type, HierName npos,
                              HierName nneg, CDLLexer.InfoToken area,
                              Map parameters, Environment env) {
            error("diode", name);
        }

        public void makeInductor(HierName name, HierName npos,
                                 HierName nneg, CDLLexer.InfoToken val,
                                 Map parameters, Environment env) {
            error("inductor", name);
        }

        public void makeBipolar(HierName name, String type, HierName nc,
                                HierName nb, HierName ne,
                                CDLLexer.InfoToken area,
                                Map parameters, Environment env) {
            error("bipolar", name);
        }

        public void makeCall(HierName name, String subName, HierName[] args,
                             Map parameters, Environment env) {
            error("subcell instantiation", name);
        }

        public void beginSubcircuit(String subName, String[] in,
                                    String[] out, Map parameters,
                                    Environment env) {
            throw new AssertionError();
        }

        public void endSubcircuit(String subName, Environment env) {
            throw new AssertionError();
        }
    }

    private static class NewAspiceFormatter extends SimpleCellFormatter {
        final PrintWriter pw, oldpw, modifyWriter;
        final IndentWriter iw;
        final File modifyFile;
        private final boolean internalWires;
        private final boolean internalRules;
        private final PartialExtract.CellPlusMinus fqcnSpec;
        private final PartialExtract.CellPlusMinusKeyword partialSimSpec;
        private final Cadencize cadencizer;
        private final Cadencize routedCadencizer;
        private CellInterface routedCell;
        private final CastFileParser cfp;
        private final TreeMap impliedPorts;
        private final boolean dsimMode;
        private final String appendColon;
        private final Set warned = new HashSet();
        private final Map cache = new HashMap();
        private final Map typesMap = new HashMap();
        private Set unusedSet = Collections.EMPTY_SET;
        private final static String ENV_SUFFIX = ".env";
        private final static String ASPICE_RESET = "$_RESET";
        private final static HierName ENV_RESET =
            HierName.makeHierName("_RESET");

        NewAspiceFormatter(final PrintWriter pw,
                           final boolean internalWires,
                           final boolean internalRules,
                           final PartialExtract.CellPlusMinus fqcnSpec,
                           final PartialExtract.CellPlusMinusKeyword pSS,
                           final CastFileParser cfp,
                           final Cadencize cadencizer,
                           final Cadencize routedCadencizer,
                           final boolean dsimMode,
                           final String appendColon) {
            this(pw, new Pair(pw, null), internalWires, internalRules,
                 fqcnSpec, pSS, cfp, cadencizer, routedCadencizer, dsimMode,
                 appendColon);
        }
        NewAspiceFormatter(final PrintWriter pw,
                           final Pair modifyPair,
                           final boolean internalWires,
                           final boolean internalRules,
                           final PartialExtract.CellPlusMinus fqcnSpec,
                           final PartialExtract.CellPlusMinusKeyword pSS,
                           final CastFileParser cfp,
                           final Cadencize cadencizer,
                           final Cadencize routedCadencizer,
                           final boolean dsimMode,
                           final String appendColon) {
            this.oldpw = pw;
            this.iw = new IndentWriter(pw);
            this.pw = new PrintWriter(iw, true);
            this.modifyWriter = (PrintWriter) modifyPair.getFirst();
            this.modifyFile = (File) modifyPair.getSecond();
            this.internalWires = internalWires;
            this.internalRules = internalRules;
            this.fqcnSpec = fqcnSpec;
            this.partialSimSpec = pSS;
            this.cadencizer = cadencizer;
            this.routedCadencizer = routedCadencizer;
            this.cfp = cfp;
            this.impliedPorts = new TreeMap();
            this.dsimMode = dsimMode;
            this.appendColon = appendColon;
        }

        /**
         * XXX: This is modified from com.avlsi.fast.NetlistAdapter.  The
         * change should be made there and no existing code should be affected,
         * as they all filter out wiring cells anyway.  But to be completely
         * safe for the pending tape-out, it is here instead.
         **/
        private static java.util.SortedSet
            getParameterList(final CellInterface ci, final Cadencize c) {
            /* Cadencize to find the port list. */
            final CadenceInfo cinfo = c.convert(ci);
            java.util.TreeSet port = new java.util.TreeSet();
            AliasedMap portNodes = cinfo.getPortNodes();
            for (Iterator i = portNodes.getCanonicalKeys(); i.hasNext(); ) {
                final HierName h = (HierName) i.next();
                if (((Boolean) portNodes.getValue(h)).booleanValue() ||
                    ci.isChannel()) port.add(h);
            }
            return port;
        }

        public CellInterface prepCell(CellInterface cell,
                                      CellInterface routed) {
            this.routedCell = routed;
            routedCadencizer.convert(routed);
            return prepCell(cell);
        }

        public CellInterface prepCell(CellInterface cell) {
            final CellImpl fake =
                new CellImpl("fake", null, CellImpl.SYNTHETIC_CELL);
            
            final CellInterface dummy;
            try {
                dummy = cfp.getFullyQualifiedCell(cell.getFullyQualifiedType(),
                                                  fake,
                                                  HierName.makeHierName("X"));
            } catch (Exception e) {
                throw new RuntimeException("Cannot load cell " +
                                           cell.getFullyQualifiedType(), e);
            }
            for (Iterator i = fake.getCanonicalNodes(); i.hasNext(); ) {
                final HierName node = (HierName) i.next();
                for (Iterator j = fake.getConnectedNodes(node); j.hasNext();) {
                    final HierName alias = (HierName) j.next();
                    if (!alias.equals(node)) {
                        final String salias = alias.getAspiceString();
                        assert salias.startsWith("X.");
                        impliedPorts.put("$" + node.getAspiceString(),
                                         salias.substring(2));
                    }
                }
            }
            return cell;
        }

        private void quote(String s, PrintWriter pw) {
            pw.print("\"" + s + "\"");
        }

        private void quote(String s) {
            quote(s, pw);
        }

        private void quote(HierName s, PrintWriter pw) {
            OldLVSFormatter.printName(null, s, pw);
        }

        private void quote(HierName s) {
            quote(s, pw);
        }

        /**
         * @pre cell != null
         **/
        public void outputCell(final CellInterface cell,
                               final CellInterface envCell)
            throws CellFormatterException, IOException {
            
            final AliasedSet aliases = cadencizer.convert(cell).getLocalNodes();
            final InstanceData instData;
            if (envCell == null && !internalRules && fqcnSpec.isEmpty()) {
                instData = null;
            } else {
                instData = new InstanceData();
                instData.updateExtraDelay(cell, aliases);
            }
           
            if (envCell == null) {
                if (modifyFile != null)
                    pw.println(".include \"" + modifyFile.getName() + "\";");

                for (Iterator i = impliedPorts.entrySet().iterator();
                     i.hasNext(); ) {
                    final Map.Entry entry = (Map.Entry) i.next();
                    pw.print("wire(");
                    OldLVSFormatter.printName(null, (String) entry.getKey(),
                                              pw);
                    pw.print(",");
                    OldLVSFormatter.printName(null, (String) entry.getValue(),
                                              pw);
                    pw.println(")");
                }
                if (partialSimSpec != null) unusedSet = new TreeSet();
                final String name = processCell(cell, null, false, instData);

                if (!internalWires && !internalRules)
                    processPortConnections(cell);

                if (partialSimSpec != null) {
                    partialSimSpec.initialize(routedCell, routedCadencizer);
                    if (!partialSimSpec.isEmpty()) {
                        pruneUsedTypes(typesMap, cell, name, null);
                        for (Iterator j = unusedSet.iterator(); j.hasNext(); ) {
                            final String type = (String) j.next();
                            skip(type, modifyWriter);
                        }
                        processPartialSimulation(typesMap, cell, name, null,
                                                 modifyWriter);
                    }
                }
            } else {
                final String env = "$env";
                final String envName =
                    processCell(envCell, HierName.makeHierName(env), true,
                                null);

                // Instantiate the environment
                quote(envName);
                pw.print(" \"$env\"(");
                final AliasedMap envLocals =
                    cadencizer.convert(envCell).getPortNodes();
                final Map ports = CellUtils.markPorts(cell);
                final Map envPorts = new HashMap();
                for (Iterator i = ports.keySet().iterator(); i.hasNext(); ) {
                    final String sname = (String) i.next();
                    final HierName hname;
                    try {
                        hname = HierName.makeHierName(sname, '.');
                    } catch (InvalidHierNameException e) {
                        throw new RuntimeException(e);
                    }
                    final Object canon = envLocals.getCanonicalKey(hname);
                    assert canon != null : "No canonical key for " + sname + " in " + envCell.getFullyQualifiedType();
                    envPorts.put(canon, sname);
                }
                for (Iterator i = getParameterList(envCell, cadencizer).iterator(); i.hasNext(); ) {
                    quote((String) envPorts.get(i.next()));
                    if (i.hasNext()) pw.print(", ");
                }
                pw.println(")");
                for (Iterator i = 
                         new SortingIterator(ports.keySet().iterator());
                     i.hasNext(); ) {
                    final String port = (String) i.next();
                    pw.print("wire(");
                    OldLVSFormatter.printName(null, port, pw);
                    pw.print(",");
                    OldLVSFormatter.printName(env, port, pw);
                    pw.println(")");
                }
                // if _RESET is not an implied port, look for _RESET inside env
                if (!impliedPorts.containsKey(ASPICE_RESET)) {
                    final HierName canonReset = (HierName)
                        cadencizer.convert(envCell).getLocalNodes()
                                                   .getCanonicalKey(ENV_RESET);
                    if (canonReset != null) {
                        pw.print("wire(");
                        OldLVSFormatter.printName(null, ASPICE_RESET, pw);
                        pw.print(",");
                        OldLVSFormatter.printName(
                                env, ENV_RESET.getAspiceString(), pw);
                        pw.println(")");
                    }
                }
            }

            // checkError also flushes
            if (pw.checkError() || oldpw.checkError() ||
                modifyWriter.checkError())
                throw new IOException();
        }

        private int getAction(final HierName instance) {
            return fqcnSpec == null ? -1 : fqcnSpec.getAction(instance);
        }

        /**
         * Output the exclusion properties, connections, and, if 
         * <code>isEnv</code> the prs for the cell.
         *
         * @param isEnv  If the cell results from a named environment.
         **/
        protected String processCell(final CellInterface cell,
                                     final HierName prefix,
                                     final boolean isEnv,
                                     final InstanceData instData) {
            final String type = cell.getFullyQualifiedType();
            if (!cache.containsKey(type)) cache.put(type, new HashMap());
            final Map perTypeMap = (Map) cache.get(type);
            final boolean addUnused =
                unusedSet != Collections.EMPTY_SET && !CellUtils.isWiring(cell);
            if (addUnused) unusedSet.add(type);

            final CadenceInfo cinfo = cadencizer.convert(cell);
            final AliasedSet localNodes = cinfo.getLocalNodes();
            final AliasedMap portNodes = cinfo.getPortNodes();

            cell.getProductionRuleSet().canonicalizeNames(localNodes);
            //cell.getAssertedProductionRuleSet().canonicalizeNames(localNodes);

            // Determine the extra_delay directives
            Map extra_delays = Collections.EMPTY_MAP;
            final ProductionRuleSet prsRules = cell.getProductionRuleSet();
            if (instData != null && prsRules.size() > 0) {
                extra_delays = new HashMap();
                for (final Iterator i = prsRules.getProductionRules();
                     i.hasNext(); ) {
                    final HierName target =
                        ((ProductionRule) i.next()).getTarget();
                    final Pair p =
                        (Pair) instData.get(DirectiveConstants.EXTRA_DELAY)
                                       .getAttribute(target);
                    if (p != null) extra_delays.put(target, p);
                }
                if (extra_delays.isEmpty())
                    extra_delays = Collections.EMPTY_MAP;
            }

            // Determine the actual types of the subcells
            final Map instances = new TreeMap();
            for (final Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
                final Pair p = (Pair) i.next();
                final HierName subcellName = (HierName) p.getFirst();
                final CellInterface subcell = (CellInterface) p.getSecond();
                if (subcell.isNode()) continue;

                final InstanceData newInstData = instData == null ? null :
                    instData.translate(cell, subcell, subcellName, cadencizer);

                final HierName fullName = HierName.append(prefix, subcellName);
                final String newType =
                    processCell(subcell, fullName, isEnv, newInstData);

                instances.put(subcellName, newType);
            }

            final int action = getAction(prefix);

            final Triplet key =
                new Triplet(new Pair(isEnv ? Boolean.TRUE : Boolean.FALSE,
                                     new Integer(action)),
                            extra_delays,
                            instances);

            final String cached = (String) perTypeMap.get(key);
            if (cached != null) return cached;

            final String result = type + "." + perTypeMap.size() +
                                  (isEnv ? ENV_SUFFIX : "");
            perTypeMap.put(key, result);
            typesMap.put(result, instances);
            if (addUnused) unusedSet.add(result);

            // Emit the definition for this cell; but do not define a top-level
            // cell
            final Set<HierName> ports = new HashSet<HierName>();
            if (prefix != null) {
                pw.print("define ");
                quote(result);
                pw.print("(");
                for (Iterator i = getParameterList(cell, cadencizer).iterator(); i.hasNext(); ) {
                    final HierName p = (HierName) i.next();
                    ports.add(p);
                    quote(p);
                    if (i.hasNext()) pw.print(", ");
                }
                pw.println(") {");
                iw.nextLevel();
            }

            final CadenceInfo routedInfo =
                routedCadencizer.getExistingCadenceInfo(cell.getFullyQualifiedType());
            Collection staticizerNodes = (Collection) Collections.EMPTY_LIST;
            if (routedInfo != null) {
                final AliasedSet locals = routedInfo.getLocalNodes();
                for (Iterator i = locals.getCanonicalKeys(); i.hasNext(); ) {
                    final HierName node = (HierName) i.next();
                    if (prefix == null || !ports.contains(node)) {
                        final String snode = node.getAspiceString();
                        pw.print("wire(");
                        OldLVSFormatter.printName(null, snode, pw);
                        pw.println(");");
                    }
                }
                //Get staticizer inverter node from netgraph
                staticizerNodes = new NetGraph(cell, routedCadencizer, cfp).getStaticizerNodes();
                for (Iterator i = staticizerNodes.iterator(); i.hasNext(); ) {
                    final NetGraph.NetNode node = (NetGraph.NetNode) i.next();
                    final String snode = CellUtils.getCastNodeName(node.getName()).getAspiceString();
                    pw.print("wire(");
                    OldLVSFormatter.printName(null, snode, pw);
                    pw.println(");");
                    
                }
            }

            final CellDelay delay = new CellDelay(cell, cadencizer);
            
            // print out the rules
            if (isEnv || internalRules || action == PartialExtract.Info.EXCLUDE)
                processPRS(cell.getProductionRuleSet(), delay,
                           dsimMode || isEnv || action == PartialExtract.Info.EXCLUDE,
                           false, instData, localNodes);

            // print out ERROR rules
            if (isEnv || internalRules)
                processPRS(cell.getAssertedProductionRuleSet(), delay,
                           true, true, instData, localNodes);
 
            // print out exclusion directives
            if (fqcnSpec == null ||
                !fqcnSpec.getRest().equals("+localportnodes") ||
                action == PartialExtract.Info.INCLUDE) {
                processExclusives(localNodes, cell.getLocalExclusiveNodeSets());
                processExclusives(cell);
            }
           
            // print out aliases
            if (isEnv || internalWires)
                processConnections(cell);

            // emit any netlist blocks
            if (isEnv && !dsimMode) processNetlist(cell);

            // print subcell instantiations
            for (final Iterator i = instances.entrySet().iterator();
                 i.hasNext(); ) {
                final Map.Entry entry = (Map.Entry) i.next();
                final HierName subcellName = (HierName) entry.getKey();
                final String typeName = (String) entry.getValue();
                final CellInterface subcell = cell.getSubcell(subcellName);

                quote(typeName);
                pw.print(" ");
                quote(subcellName);
                pw.print("(");

                for (final Iterator j = getParameterList(subcell, cadencizer).iterator(); j.hasNext(); ) {
                    final HierName port = (HierName) j.next();
                    final HierName net = (HierName) localNodes.getCanonicalKey(HierName.append(subcellName, port));
                    quote(net);
                    if (j.hasNext()) pw.print(", ");
                }

                pw.println(")");
            }

            if (!isEnv && !appendColon.equals("")) {
                emitAnalogDigitalBindings(localNodes, portNodes,
                                          staticizerNodes, prefix == null);
            }

            if (prefix != null) {
                iw.prevLevel();
                pw.println("}");
            }
            return result;
        }

        private void emitAnalogDigitalBindings(final AliasedSet localNodes,
                                               final AliasedMap portNodes,
                                               final Collection staticizerNodes,
                                               final boolean all) {
            for (Iterator i = localNodes.getCanonicalKeys(); i.hasNext(); ) {
                final HierName node = (HierName) i.next();
                if (all || !portNodes.contains(node)) {
                    final String snode = node.getAspiceString();
                    pw.print("wire(");
                    OldLVSFormatter.printName(null, snode, pw);
                    pw.print(",");
                    OldLVSFormatter.printName(null, snode + appendColon, pw);
                    pw.println(");");
                }
            }
            for (Iterator i = staticizerNodes.iterator(); i.hasNext(); ) {
                final NetGraph.NetNode node = (NetGraph.NetNode) i.next();
                final String snode = node.getName().getAspiceString();                
                final String snode_digital = CellUtils.getCastNodeName(node.getName()).getAspiceString();               
                pw.print("wire(");
                OldLVSFormatter.printName(null, snode_digital, pw);
                pw.print(",");
                OldLVSFormatter.printName(null, snode + appendColon, pw);
                pw.println(");");
            }
        }

        private void processPRS(final ProductionRuleSet prs,
                                final CellDelay delay,
                                final boolean isEnv,
                                final boolean isErr,
                                final InstanceData instData,
                                final AliasedSet localNodes) {
            boolean firstP = true;           

            for (final Iterator prsIterator = 
                     new SortingIterator(prs.getProductionRules(),
                                         new StringRepComparator() );
                 prsIterator.hasNext(); ) {
                final ProductionRule pr = (ProductionRule) prsIterator.next();
                if (firstP) {
                    firstP = false;
                    pw.println("dsim {");
                    iw.nextLevel();
                }

                printPR(pr, delay, isEnv, isErr, instData, localNodes);
            }
            if (!firstP) {
                iw.prevLevel();
                pw.println('}');
            }
        }

        protected void printPR(final ProductionRule pr,
                               final CellDelay delay,
                               final boolean isEnv,
                               final boolean isErr,
                               final InstanceData instData,
                               final AliasedSet localNodes) {

            
            final Iterator iD =
                pr.getGuard().DNFForm().getDisjuncts().iterator();
            while (iD.hasNext()) {
                final AndBooleanExpressionInterface disj =
                    (AndBooleanExpressionInterface) iD.next();
                final Iterator iC = disj.getConjuncts().iterator();

                // modifiers
                pw.print((isEnv ? "env " : "") + pr.flagsString());
                final boolean up = pr.getDirection() == ProductionRule.UP;
                final int extraDelay = instData == null ? 0 :
                    Math.round(instData.getExtraDelay(up, pr.getTarget()));
                // This is fine for after_ps, because getDelay() divides the
                // specified number by 100, and multiply by 100 to recover the
                // old value.
                final int after =
                    (int) delay.getDelay(pr.getTarget(), up, 100) + extraDelay;
                pw.print(pr.isAbsolute() ? "after_ps" : "after");
                pw.print(" " + after + " ");

                // guard
                boolean firstP = true;
                while (iC.hasNext()) {
                    final HierNameAtomicBooleanExpression conj =
                        (HierNameAtomicBooleanExpression) iC.next();
                    if (firstP)
                        firstP = false;
                    else
                        pw.print(" & ");

                    if (!conj.getSense())
                        pw.print('~');
                    OldLVSFormatter.printName(null,
                                              conj.getName(),
                                              pw);
                }

                // target
                pw.print(" -> ");
                if (isErr) pw.print("\"ERROR\"");
                else OldLVSFormatter.printName(null,
                                               pr.getTarget(),
                                               pw);

                // direction
                pw.println(pr.getDirection() == ProductionRule.UP ?  '+' : '-');
            }
        }

        private void processExclusives(final List group1, final List group2) {
            final ArrayList pair = new ArrayList(2);
            pair.add(null); pair.add(null);
            for (Iterator g1 = group1.iterator(); g1.hasNext(); ) {
                pair.set(0, g1.next());
                for (Iterator g2 = group2.iterator(); g2.hasNext(); ) {
                    pair.set(1, g2.next());
                    final ExclusiveNodeSet ens =
                        new ExclusiveNodeSet(ExclusiveNodeSet.CC, pair);
                    OldLVSFormatter.printExclusive(ens, null, pw);
                }
            }
        }

        private void processExclusives(final ArrayList groups) {
            for (int i = 0; i < groups.size(); i++) {
                final List group1 = (List) groups.get(i);
                for (int j = i + 1; j < groups.size(); j++) {
                    final List group2 = (List) groups.get(j);
                    processExclusives(group1, group2);
                }
            }
        }

        private void processExclusives(final CellInterface cell) {
            final BlockInterface cellBlock = cell.getBlockInterface();

            // process exclcc directives
            final Map exclMap =
                DirectiveUtils.getMultipleBlockDirective
                ( Arrays.asList
                  ( new BlockInterface[]
                      { DirectiveUtils.getUniqueBlock
                        (cellBlock,
                         BlockInterface.PRS),
                        DirectiveUtils.getUniqueBlock
                        (cellBlock,
                         BlockInterface.SUBCELL)
                      } ),
                  DirectiveConstants.EXCLCC,
                  DirectiveTable.arrayify(
                      DirectiveConstants.UNCHECKED_NODE_TYPE)
                );

            // group exclusive nodes by their group number
            final MultiMap exclGroups =
                new MultiMap(new HashMap(), MultiMap.ARRAY_LIST_FACTORY);
            for (Iterator i = exclMap.entrySet().iterator(); i.hasNext(); ) {
                final Map.Entry entry = (Map.Entry) i.next();
                exclGroups.put(entry.getValue(), entry.getKey());
            }

            for (Iterator i = exclGroups.keySet().iterator(); i.hasNext(); ) {
                final ArrayList excls = (ArrayList) exclGroups.get(i.next());
                processExclusives(excls);
            }

            // process nocc_nodes directives
            final List prsNocc = (List) DirectiveUtils.getPrsDirective(
                    cell, DirectiveConstants.NOCC_NODES);
            final List subcellNocc = (List) DirectiveUtils.getSubcellDirective(
                    cell, DirectiveConstants.NOCC_NODES);
            if (prsNocc != null || subcellNocc != null) {
                final Collection allNocc = new ArrayList();
                if (prsNocc != null) allNocc.addAll(prsNocc);
                if (subcellNocc != null) allNocc.addAll(subcellNocc);
                final ExclusiveNodeSet ens =
                    new ExclusiveNodeSet(ExclusiveNodeSet.NOCC, allNocc);
                OldLVSFormatter.printExclusive(ens, null, pw);
            }
        }

        private void processExclusives(final AliasedSet localNodes,
                                       final ExclusiveNodeSets enss) {
            OldLVSFormatter.printExclusives(localNodes, enss, null, pw);
        }

        private void processConnections(final CellInterface cell) {
            for (final Iterator iCanonNode = 
                     new SortingIterator(cell.getCanonicalNodes());
                 iCanonNode.hasNext(); ) {
                final HierName canonNode = (HierName) iCanonNode.next();
                boolean firstP = true;
                for (final Iterator iConnNode =
                         cell.getConnectedNodes(canonNode);
                     iConnNode.hasNext(); ) {
                    final HierName connNode = (HierName) iConnNode.next();
                    // don't print self-connections
                    if (connNode != canonNode) {
                        if (firstP) {
                            firstP = false;
                            pw.print("wire(");
                            OldLVSFormatter.printName(null, canonNode, pw);
                        }

                        pw.print(',');
                        OldLVSFormatter.printName(null, connNode, pw);
                    }
                }
                if (!firstP)
                    pw.println(')');
            }
        }

        private void processPortConnections(final CellInterface cell) {
            final AliasedMap portNodes =
                cadencizer.convert(cell).getPortNodes();
            for (final Iterator iCanonNode = 
                     new SortingIterator(portNodes.getCanonicalKeys());
                 iCanonNode.hasNext(); ) {
                final HierName canonNode = (HierName) iCanonNode.next();
                boolean firstP = true;
                for (final Iterator iConnNode =
                         portNodes.getAliases(canonNode);
                     iConnNode.hasNext(); ) {
                    final HierName connNode = (HierName) iConnNode.next();
                    // don't print self-connections
                    if (connNode != canonNode) {
                        if (firstP) {
                            firstP = false;
                            pw.print("wire(");
                            OldLVSFormatter.printName(null, canonNode, pw);
                        }

                        pw.print(',');
                        OldLVSFormatter.printName(null, connNode, pw);
                    }
                }
                if (!firstP)
                    pw.println(')');
            }
        }

        private void processNetlist(final CellInterface ci) {

            if (!ci.containsNetlist()) return;
            final BlockIterator bi =
                ci.getBlockInterface().iterator(BlockInterface.NETLIST);
            assert bi.hasNext() : "No netlist block found in " + ci.getFullyQualifiedType();
            final NetlistBlock nb = (NetlistBlock) bi.next();
            final Template templ = nb.getCDLTemplate();
            int[] errors = new int[] { 0 };
            templ.execute(new Netlist2Aspice(null, pw, errors),
                          new LocalEnvironment(nb.getParams()), null);
            if (errors[0] > 0 && warned.add(ci)) {
                System.err.println("AspiceFormatter: " + errors[0] + " circuit elements ignored in the netlist block for " + ci.getFullyQualifiedType());
            }
        }

        private void pruneUsedTypes(
                final Map typesMap,
                final CellInterface cell,
                final String cellName,
                final HierName prefix) {
            final Map subcells = (Map) typesMap.get(cellName);
            if (subcells == null) return;

            for (Iterator i = subcells.entrySet().iterator(); i.hasNext(); ) {
                final Map.Entry entry = (Map.Entry) i.next();
                final HierName subInst = (HierName) entry.getKey();
                final CellInterface subcell = cell.getSubcell(subInst);
                final String subcellName = (String) entry.getValue();
                if (subcell.isChannel() || subcell.isNode()) continue;
                pruneUsedTypes(typesMap, subcell, subcellName,
                               HierName.prefixName(prefix, subInst));
            }
            final int action = partialSimSpec.getAction(prefix);
            if (prefix == null || action != PartialExtract.Info.EXCLUDE) {
                unusedSet.remove(cellName);

                if (!CellUtils.isWiring(cell)) {
                    unusedSet.remove(cell.getFullyQualifiedType());
                }
            }
        }

        private void skip(final String type, final PrintWriter pw) {
            pw.print(".modify skip ");
            quote(type, pw);
            pw.println(";");
        }

        private void skip(final String type, final HierName prefix,
                          final PrintWriter pw) {
            pw.print(".modify skip ");
            quote(type, pw);
            pw.print(" ");
            quote(prefix, pw);
            pw.println(";");
        }

        private void processPartialSimulation(
                final Map typesMap,
                final CellInterface cell,
                final String cellName,
                final HierName prefix,
                final PrintWriter pw) {
            final Map subcells = (Map) typesMap.get(cellName);
            if (subcells == null) return;

            for (Iterator i = subcells.entrySet().iterator(); i.hasNext(); ) {
                final Map.Entry entry = (Map.Entry) i.next();
                final HierName subInst = (HierName) entry.getKey();
                final CellInterface subcell = cell.getSubcell(subInst);
                final String subcellName = (String) entry.getValue();
                if (subcell.isChannel() || subcell.isNode()) continue;
                processPartialSimulation(typesMap, subcell, subcellName,
                                         HierName.prefixName(prefix, subInst),
                                         pw);
            }
            final int action = partialSimSpec.getAction(prefix);
            if (prefix != null && action == PartialExtract.Info.EXCLUDE) {
                if (!unusedSet.contains(cellName))
                    skip(cellName, prefix, pw);

                final String type = cell.getFullyQualifiedType();
                if (!CellUtils.isWiring(cell) && !unusedSet.contains(type))
                    skip(type, prefix, pw);
            }
        }
    }

    /**
     * Aspice output formatter.  Processes hierarchically, in contrast to
     * all the other formatters in this class.  The names that are output
     * will not be canonical.  The wire statements are needed to make
     * sense of the output circuit.
     **/
    private static class AspiceFormatter extends SimpleCellFormatter {
        final PrintWriter pw;
        private final boolean internalWires;
        private final boolean internalRules;
        private final PartialExtract.CellPlusMinus fqcnSpec;
        private final Cadencize cadencizer;
        private final CastFileParser cfp;
        private final TreeMap impliedPorts;
        private final boolean dsimMode;
        private boolean alintMode = false;
        private final Set warned = new HashSet();

        AspiceFormatter(final PrintWriter pw,
                        final boolean internalWires,
                        final boolean internalRules,
                        final PartialExtract.CellPlusMinus fqcnSpec,
                        final CastFileParser cfp,
                        final Cadencize cadencizer,
                        final boolean dsimMode) {
            this.pw = pw;
            this.internalWires = internalWires;
            this.internalRules = internalRules;
            this.fqcnSpec = fqcnSpec;
            this.cadencizer = cadencizer;
            this.cfp = cfp;
            this.impliedPorts = new TreeMap();
            this.dsimMode = dsimMode;
        }

        public CellInterface prepCell(CellInterface cell) {
            final CellImpl fake =
                new CellImpl("fake", null, CellImpl.SYNTHETIC_CELL);
            
            final CellInterface dummy;
            try {
                dummy = cfp.getFullyQualifiedCell(cell.getFullyQualifiedType(),
                                                  fake,
                                                  HierName.makeHierName("X"));
            } catch (Exception e) {
                throw new RuntimeException("Cannot load cell " +
                                           cell.getFullyQualifiedType(), e);
            }
            for (Iterator i = fake.getCanonicalNodes(); i.hasNext(); ) {
                final HierName node = (HierName) i.next();
                for (Iterator j = fake.getConnectedNodes(node); j.hasNext();) {
                    final HierName alias = (HierName) j.next();
                    if (!alias.equals(node)) {
                        final String salias = alias.getAspiceString();
                        assert salias.startsWith("X.");
                        impliedPorts.put("$" + node.getAspiceString(),
                                         salias.substring(2));
                    }
                }
            }
            return cell;
        }

        /**
         * @pre cell != null
         **/
        public void outputCell(
                               final CellInterface cell,
                               final CellInterface envCell)
            throws CellFormatterException, IOException {
            
            final AliasedSet aliases = cadencizer.convert(cell).getLocalNodes();
            final InstanceData instData = new InstanceData();
            instData.updateExtraDelay(cell, aliases);

            for (Iterator i = impliedPorts.entrySet().iterator(); i.hasNext(); )
                {
                    final Map.Entry entry = (Map.Entry) i.next();
                    pw.print("wire(");
                    OldLVSFormatter.printName(null, (String) entry.getKey(), pw);
                    pw.print(",");
                    OldLVSFormatter.printName(null, (String) entry.getValue(), pw);
                    pw.println(")");
                }
            alintMode = envCell == null;
            processCell(cell, null, false, instData);
            if (envCell != null) {
                final String env = "$env";
                processCell(envCell, HierName.makeHierName(env), true, null);
                final Map ports = CellUtils.markPorts(cell);
                for (Iterator i = 
                         new SortingIterator(ports.keySet().iterator());
                     i.hasNext(); ) {
                    final String port = (String) i.next();
                    pw.print("wire(");
                    OldLVSFormatter.printName(null, port, pw);
                    pw.print(",");
                    OldLVSFormatter.printName(env, port, pw);
                    pw.println(")");
                }
            }
            if (/* !alintMode && */ !internalWires && !internalRules)
                processPortConnections(cell, null);

            // checkError also flushes
            if (pw.checkError())
                throw new IOException();
        }
        

        /**
         * Output the exclusion properties, connections, and, if 
         * <code>isEnv</code> the prs for the cell.
         *
         * @param isEnv  If the cell results from a named environment.
         **/
        protected void processCell(
                                   final CellInterface cell,
                                   final HierName prefix,
                                   final boolean isEnv,
                                   final InstanceData instData) {

            final AliasedSet localNodes =
                cadencizer.convert(cell).getLocalNodes();

            cell.getProductionRuleSet().canonicalizeNames(localNodes);
            //cell.getAssertedProductionRuleSet().canonicalizeNames(localNodes);
            final CellDelay delay = new CellDelay(cell, cadencizer);
            
            final int action =
                fqcnSpec == null ? -1 : fqcnSpec.getAction(prefix);

            // print out the rules
            if (!alintMode && (isEnv ||
                               internalRules ||
                               action == PartialExtract.Info.EXCLUDE))
                processPRS(cell.getProductionRuleSet(), prefix, delay,
                           dsimMode || isEnv || action == PartialExtract.Info.EXCLUDE,
                           false, instData, localNodes);

            // print out ERROR rules
            if (!alintMode && (isEnv || internalRules))
                processPRS(cell.getAssertedProductionRuleSet(), prefix, delay,
                           true, true, instData, localNodes);
 
            // print out exclusion directives
            if (!alintMode || fqcnSpec == null ||
                !fqcnSpec.getRest().equals("+localnodes") ||
                action == PartialExtract.Info.INCLUDE)
                processExclusives(localNodes, cell.getLocalExclusiveNodeSets(), prefix);
           
            // print out aliases
            if (/* !alintMode && */ (isEnv || internalWires))
                processConnections(cell, prefix);

            // emit any netlist blocks
            if (isEnv && !dsimMode)
                processNetlist(cell, prefix);

            // recurse to subcells
            for (final Iterator i = 
                     new SortingIterator(cell.getSubcellPairs()); 
                 i.hasNext(); ) {
                final Pair p = (Pair) i.next();
                final HierName subcellName = (HierName) p.getFirst();
                final CellInterface subcell = (CellInterface) p.getSecond();

                /*
                  if (alintMode && subcell.isChannel()) {
                  final ExclusiveNodeSets flat = new ExclusiveNodeSets();
                  flattenExcl(subcell, subcellName, flat);
                  processExclusives(localNodes, flat, prefix);
                  continue;
                  }
                */

                final InstanceData newInstData = instData == null ? null :
                    instData.translate(cell, subcell, subcellName, cadencizer);

                processCell(subcell, HierName.append(prefix, subcellName),
                            isEnv, newInstData);
            }
        }

        private void flattenExcl(final CellInterface wiring,
                                 final HierName prefix,
                                 final ExclusiveNodeSets result) {
            for (Iterator i = wiring.getLocalExclusiveNodeSets().getIterator();
                 i.hasNext(); ) {
                final ExclusiveNodeSet ens = (ExclusiveNodeSet) i.next();
                result.addExclusiveNodeSet(ens.prefixNames(prefix));
            }
            for (final Iterator i = wiring.getSubcellPairs(); i.hasNext(); ) {
                final Pair p = (Pair) i.next();
                final HierName subcellName = (HierName) p.getFirst();
                final CellInterface subcell = (CellInterface) p.getSecond();
                flattenExcl(subcell, HierName.append(prefix, subcellName),
                            result);
            }
        }

        private void processPRS(
                                final ProductionRuleSet prs,
                                final HierName prefix,
                                final CellDelay delay,
                                final boolean isEnv,
                                final boolean isErr,
                                final InstanceData instData,
                                final AliasedSet localNodes) {
            boolean firstP = true;           

            for (final Iterator prsIterator = 
                     new SortingIterator(prs.getProductionRules(),
                                         new StringRepComparator() );
                 prsIterator.hasNext(); ) {
                final ProductionRule pr = (ProductionRule) prsIterator.next();
                if (firstP) {
                    firstP = false;
                    pw.println("dsim {");
                }

                printPR(prefix, pr, delay, isEnv, isErr, instData, localNodes);
            }
            if (!firstP)
                pw.println('}');
        }

        protected void printPR(
                               final HierName prefix,
                               final ProductionRule pr,
                               final CellDelay delay,
                               final boolean isEnv,
                               final boolean isErr,
                               final InstanceData instData,
                               final AliasedSet localNodes) {

            
            final Iterator iD =
                pr.getGuard().DNFForm().getDisjuncts().iterator();
            while (iD.hasNext()) {
                final AndBooleanExpressionInterface disj =
                    (AndBooleanExpressionInterface) iD.next();
                final Iterator iC = disj.getConjuncts().iterator();

                // modifiers
                pw.print((isEnv ? "env " : "") + pr.flagsString());
                final boolean up = pr.getDirection() == ProductionRule.UP;
                final HierName canonTarget =
                    (HierName) localNodes.getCanonicalKey(pr.getTarget());
                final int extraDelay = instData == null ? 0 :
                    Math.round(instData.getExtraDelay(up, canonTarget));
                // This is fine for after_ps, because getDelay() divides the
                // specified number by 100, and multiply by 100 to recover the
                // old value.
                final int after =
                    (int) delay.getDelay(pr.getTarget(), up, 100) + extraDelay;
                pw.print(pr.isAbsolute() ? "after_ps" : "after");
                pw.print(" " + after + " ");

                // guard
                boolean firstP = true;
                while (iC.hasNext()) {
                    final HierNameAtomicBooleanExpression conj =
                        (HierNameAtomicBooleanExpression) iC.next();
                    if (firstP)
                        firstP = false;
                    else
                        pw.print(" & ");

                    if (!conj.getSense())
                        pw.print('~');
                    OldLVSFormatter.printName(prefix,
                                              conj.getName(),
                                              pw);
                }

                // target
                pw.print(" -> ");
                if (isErr) pw.print("\"ERROR\"");
                else OldLVSFormatter.printName(prefix,
                                               pr.getTarget(),
                                               pw);

                // direction
                pw.println(pr.getDirection() == ProductionRule.UP ?  '+' : '-');
            }
        }

        private void processExclusives(
                                       final AliasedSet localNodes,
                                       final ExclusiveNodeSets enss,
                                       final HierName prefix) {

            OldLVSFormatter.printExclusives(localNodes,enss, prefix, pw);
        }

        private void processConnections(
                                        final CellInterface cell,
                                        final HierName prefix) {
            for (final Iterator iCanonNode = 
                     new SortingIterator(cell.getCanonicalNodes());
                 iCanonNode.hasNext(); ) {
                final HierName canonNode = (HierName) iCanonNode.next();
                boolean firstP = true;
                for (final Iterator iConnNode =
                         cell.getConnectedNodes(canonNode);
                     iConnNode.hasNext(); ) {
                    final HierName connNode = (HierName) iConnNode.next();
                    // don't print self-connections
                    if (connNode != canonNode) {
                        if (firstP) {
                            firstP = false;
                            pw.print("wire(");
                            OldLVSFormatter.printName(prefix, canonNode, pw);
                        }

                        pw.print(',');
                        OldLVSFormatter.printName(prefix, connNode, pw);
                    }
                }
                if (!firstP)
                    pw.println(')');
            }
        }

        private void processPortConnections(
                                            final CellInterface cell,
                                            final HierName prefix) {
            final AliasedMap portNodes =
                cadencizer.convert(cell).getPortNodes();
            for (final Iterator iCanonNode = 
                     new SortingIterator(portNodes.getCanonicalKeys());
                 iCanonNode.hasNext(); ) {
                final HierName canonNode = (HierName) iCanonNode.next();
                boolean firstP = true;
                for (final Iterator iConnNode =
                         portNodes.getAliases(canonNode);
                     iConnNode.hasNext(); ) {
                    final HierName connNode = (HierName) iConnNode.next();
                    // don't print self-connections
                    if (connNode != canonNode) {
                        if (firstP) {
                            firstP = false;
                            pw.print("wire(");
                            OldLVSFormatter.printName(prefix, canonNode, pw);
                        }

                        pw.print(',');
                        OldLVSFormatter.printName(prefix, connNode, pw);
                    }
                }
                if (!firstP)
                    pw.println(')');
            }
        }

        private void processNetlist(final CellInterface ci,
                                    final HierName prefix) {

            if (!ci.containsNetlist()) return;
            final BlockIterator bi =
                ci.getBlockInterface().iterator(BlockInterface.NETLIST);
            assert bi.hasNext() : "No netlist block found in " + ci.getFullyQualifiedType();
            final NetlistBlock nb = (NetlistBlock) bi.next();
            final Template templ = nb.getCDLTemplate();
            int[] errors = new int[] { 0 };
            templ.execute(new Netlist2Aspice(prefix, pw, errors),
                          new LocalEnvironment(nb.getParams()), null);
            if (errors[0] > 0 && warned.add(ci)) {
                System.err.println("AspiceFormatter: " + errors[0] + " circuit elements ignored in the netlist block for " + ci.getFullyQualifiedType());
            }
        }
    }

    /** Local nodes formatter
     *  Same as cast-query --task=local_nodes=used
     **/
    private static class LocalNodesFormatter extends SimpleCellFormatter {
        private final PrintWriter pw;
        private final CastFileParser cfp;
        private final Cadencize mCadencizer;
        private final boolean includePorts;
        private final Set nodes;

        public LocalNodesFormatter( final PrintWriter pw, 
                                    final CastFileParser cfp,
                                    final Cadencize cadencizer,
                                    final boolean includePorts,
                                    final Set nodes) {
            this.pw = pw;
            this.cfp = cfp;
            this.nodes = nodes;
            this.includePorts = includePorts;
            mCadencizer = cadencizer;
        }
        
        public CellInterface prepCell(CellInterface cell) {
            return cell;
        }
        
        public void outputCell(final CellInterface cell,
                               final CellInterface envCell)
            throws CellFormatterException, IOException {

            if(envCell != null ) {
                return;
            }

            LocalNodes localNodes = 
                new LocalNodes(cfp,
                               mCadencizer,
                               false,
                               true,
                               includePorts,
                               true,
                               nodes);
            localNodes.printHeader = false;
            localNodes.doTask(cell,pw);

            // checkError also flushes
            if (pw.checkError())
                throw new IOException();
        }
    }

    /**
     * Export information from CAST to alint that isn't appropriate to put into
     * the local nodes formatter.
     **/
    private static class AlintScenariosFormatter extends SimpleCellFormatter {
        private final PrintWriter pw;
        private final Cadencize cad;

        // Outputs fully formatted scenario lines for alint; make_alint_in will
        // only need to accumulate lines per victim, and then write them out
        // before alinting the victim
        private void alintScenario(final Map<Object,Object> dirs,
                                   final String type,
                                   final String dir,
                                   final Set<HierName> noDefaultUp,
                                   final Set<HierName> noDefaultDn,
                                   final HierName canon) throws IOException {
            final Collection<TupleValue> tuples =
                (Collection<TupleValue>) dirs.get(canon);
            if (tuples != null) {
                pw.write("victim " + canon + "\n");
                if (noDefaultUp.add(canon)) {
                    pw.write("scenario:up\n");
                }
                if (noDefaultDn.add(canon)) {
                    pw.write("scenario:dn\n");
                }
                for (TupleValue tuple : tuples) {
                    pw.write("scenario" + type + dir);
                    for (Iterator i = tuple.getIterator(); i.hasNext(); ) {
                        final AlintFaninValue fanin =
                            (AlintFaninValue) i.next();
                        pw.write(" " + fanin.getNode() + fanin.getState());
                    }
                    pw.write("\n");
                }
            }
        }

        private Pair alintScenario(final String type,
                                   final CellInterface cell,
                                   final AliasedSet locals) {
            final BlockInterface cellBlock = cell.getBlockInterface();
            final Map m =
                DirectiveUtils.getMultipleBlockDirective
                ( Arrays.asList
                  ( new BlockInterface[]
                      { DirectiveUtils.getUniqueBlock
                        (cellBlock,
                         BlockInterface.PRS),
                        DirectiveUtils.getUniqueBlock
                        (cellBlock,
                         BlockInterface.SUBCELL) } ),
                  type,
                  DirectiveConstants.HALFOP_TYPE );
            return new Pair(
                    DirectiveUtils.canonizeKey(locals,
                                               DirectiveUtils.getUps(m)),
                    DirectiveUtils.canonizeKey(locals,
                                               DirectiveUtils.getDowns(m)));
        }

        public AlintScenariosFormatter(final PrintWriter pw,
                                      final Cadencize cad) {
            this.pw = pw;
            this.cad = cad;
        }
        
        public CellInterface prepCell(CellInterface cell) {
            return cell;
        }
        
        public void outputCell(final CellInterface cell,
                               final CellInterface envCell)
            throws CellFormatterException, IOException {

            if (envCell != null) return;

            final AliasedSet nodes = cad.convert(cell).getLocalNodes();

            final Pair defScenario =
                alintScenario(DirectiveConstants.ALINT_DEFAULT_SCENARIOS, cell,
                              nodes);
            final Set<HierName> noDefaultUp = (Set<HierName>)
                DirectiveUtils.getExplicitFalses((Map) defScenario.getFirst());
            final Set<HierName> noDefaultDn = (Set<HierName>)
                DirectiveUtils.getExplicitFalses((Map) defScenario.getSecond());

            final Map<String,Map<Object,Object>> upScenarioMaps =
                new LinkedHashMap<String,Map<Object,Object>>();
            final Map<String,Map<Object,Object>> dnScenarioMaps =
                new LinkedHashMap<String,Map<Object,Object>>();
            final Map<String,String> scenarioDirs = (Map<String,String>)
                CollectionUtils.mapify(new Object[] {
                    DirectiveConstants.ALINT_SCENARIO, "",
                    DirectiveConstants.ALINT_DELAY_SCENARIO, ":delay",
                    DirectiveConstants.ALINT_LEAK_SCENARIO, ":leak",
                    DirectiveConstants.ALINT_BUMP_SCENARIO, ":bump" });
            for (Map.Entry<String,String> dir : scenarioDirs.entrySet()) {
                final Pair p = alintScenario(dir.getKey(), cell, nodes);
                upScenarioMaps.put(dir.getValue(),
                                   (Map<Object,Object>) p.getFirst());
                dnScenarioMaps.put(dir.getValue(),
                                   (Map<Object,Object>) p.getSecond());
            }

            for (Iterator i = new SortingIterator(nodes.getCanonicalKeys());
                 i.hasNext(); ) {
                final HierName canon = (HierName) i.next();
                for (Map.Entry<String,Map<Object,Object>> entry :
                        upScenarioMaps.entrySet()) {
                    alintScenario(entry.getValue(), entry.getKey(), ":up",
                                  noDefaultUp, noDefaultDn, canon);
                }
                for (Map.Entry<String,Map<Object,Object>> entry :
                        dnScenarioMaps.entrySet()) {
                    alintScenario(entry.getValue(), entry.getKey(), ":dn",
                                  noDefaultUp, noDefaultDn, canon);
                }
            }

            // checkError also flushes
            if (pw.checkError())
                throw new IOException();
        }
    }

    private static class LocalAliasesFormatter extends SimpleCellFormatter {
        private final PrintWriter pw;
        private final CastFileParser cfp;
        private final Cadencize mCadencizer;

        public LocalAliasesFormatter( final PrintWriter pw, 
                                      final CastFileParser cfp,
                                      final Cadencize cadencizer) {
            this.pw = pw;
            this.cfp = cfp;
            mCadencizer = cadencizer;
        }
        
        public CellInterface prepCell(CellInterface cell) {
            return cell;
        }
        
        public void outputCell(final CellInterface cell,
                               final CellInterface envCell)
            throws CellFormatterException, IOException {

            if(envCell != null ) {
                return;
            }

            pw.println(cell.getFullyQualifiedType());

            final AliasedSet locals = mCadencizer.convert(cell).getLocalNodes();
            for (Iterator i = locals.getCanonicalKeys(); i.hasNext(); ) {
                final HierName canon = (HierName) i.next();
                pw.print(canon.getCadenceString());
                for (Iterator j = locals.getAliases(canon); j.hasNext(); ) {
                    final HierName alias = (HierName) j.next();
                    if (!alias.equals(canon)) {
                        pw.print("=" + alias.getCadenceString());
                    }
                }
                pw.println();
            }

            // checkError also flushes
            if (pw.checkError())
                throw new IOException();
        }
    }

    /** Dynamic nodes formatter
     *  Same as cast-query --task=dynamic_nodes=leaky
     **/
    private static class LeakyNodesFormatter extends SimpleCellFormatter {
        private final PrintWriter pw;
        private final CastFileParser cfp;
        private final Cadencize mCadencizer;

        public LeakyNodesFormatter( final PrintWriter pw, 
                                    final CastFileParser cfp,
                                    final Cadencize cadencizer) {
            this.pw = pw;
            this.cfp = cfp;
            mCadencizer = cadencizer;
        }
        
        public CellInterface prepCell(CellInterface cell) {
            return cell;
        }
        
        public void outputCell(final CellInterface cell,
                               final CellInterface envCell)
            throws CellFormatterException, IOException {

            if(envCell != null ) {
                return;
            }

            CastQuery.DynamicNodes dynamicNodes = 
                new CastQuery.DynamicNodes(cfp, mCadencizer, false, true);
            dynamicNodes.printHeader = false;
            dynamicNodes.doTask(cell,pw);

            // checkError also flushes
            if (pw.checkError())
                throw new IOException();
        }
    }

    /** CDL formatter
     *  Same as Cast2Cdl
     **/
    private static class CDLFormatter extends SimpleCellFormatter {
        
        private final CDLNameInterfaceFactory nameInterfaceFactory;
        private final PrintWriter pw;
        private final CastFileParser cfp;
        private final PartialExtract.CellPlusMinus fqcnSpec;
        private final String[] mosParams;
        private final String callDelimiter;
        private final String[] cdlCells;
        private final Cadencize mCadencizer;

        public CDLFormatter( final PrintWriter pw,
                             final PartialExtract.CellPlusMinus fqcnSpec,
                             final CastFileParser cfp,
                             final CDLNameInterfaceFactory nameInterfaceFactory,
                             final String[] mosParams,
                             final String callDelimiter,
                             final String[] cdlCells,
                             final Cadencize cadencizer ) { 
            this.fqcnSpec = fqcnSpec;
            this.pw = pw;
            this.cfp = cfp;
            this.nameInterfaceFactory = nameInterfaceFactory;
            this.mosParams = mosParams;
            this.callDelimiter = callDelimiter;
            this.cdlCells = cdlCells;
            mCadencizer = cadencizer;
        }

        public CellInterface prepCell(CellInterface cell,
                                      CellInterface routed) {
            return cell;
        }
        
        public void outputCell(final CellInterface cell,
                               final CellInterface envCell)
            throws CellFormatterException, IOException {
            
            if ( envCell != null ) {
                throw new CellFormatterException("I can't format envs");
            }
            
            final CDLFactoryInterface cdlEmitterFactory =
                new CDLFactoryEmitter(pw, true, 79, true, false, callDelimiter);

            final CDLFactoryInterface renamerFactory =
                new CDLRenameFactory( cdlEmitterFactory,
                                      nameInterfaceFactory );


            final CDLFactoryInterface templateAccumulatorFactory;
            final Map templates;
            if ( fqcnSpec.isEmpty() ) {
                templateAccumulatorFactory = renamerFactory;
                templates = null;
            }
            else {
                templates = new LinkedHashMap();
                templateAccumulatorFactory = new Template( templates );
            }

            /* lvs-nodes hack not needed w/ assura3.1.2 - clayton
            final LVSNodes lvsNodes = new LVSNodesForExtract( cfp,
                                                              mCadencizer );
            final CDLFactoryInterface lvsNodesEmitter =
                new LVSNodesCDLFactory( lvsNodes,
                                        templateAccumulatorFactory,
                                        new LVSNodesNullHandler() );
            final CDLFactoryInterface realNamesEmitter = 
                //new Cast2Cdl.RealTransistorNames(lvsNodesEmitter, cfp);
            */

            final CDLFactoryInterface realNamesEmitter = 
                new Cast2Cdl.RealTransistorNames(templateAccumulatorFactory,
                                                 cfp);
            final CDLFactoryInterface mosParameterFilter =
                mosParams == null ? realNamesEmitter
                                  : new Cast2Cdl.MosParameterFilter(
                                          realNamesEmitter,
                                          mosParams);

            final Set<String> emitted = new HashSet<String>();
            final CDLFactoryInterface noop = new CDLFactoryAdaptor();
            final CDLFactoryInterface uniqFilter =
                new CDLSubcktFilter(
                    new UnaryFunction<String,CDLFactoryInterface>() {
                        public CDLFactoryInterface execute(String subName) {
                            return emitted.add(subName) ? mosParameterFilter
                                                        : noop;
                        }
                    }, noop);

            Cast2Cdl.outputCDL( cell,
                                cfp, 
                                uniqFilter,
                                mCadencizer,
                                false,
                                true );

            for (String c : cdlCells) {
                Cast2Cdl.outputCDL( cfp.getFullyQualifiedCellPretty(c),
                                    cfp, 
                                    uniqFilter,
                                    mCadencizer,
                                    false,
                                    true );
            }


            final String cellName = fqcnSpec.getTop();


            if ( ! fqcnSpec.isEmpty() ) {
                final PartialExtract pe =
                    new PartialExtract(templates, cellName, fqcnSpec);
                pe.execute( renamerFactory );
            }


            // checkError also flushes
            if (pw.checkError())
                throw new IOException();
        }
    }

    /** Signature formatter
     *  Intended for use with a build-system.  Creates a file that should 
     *  change iff when environment or circuit changes. i.e comments and 
     *  irrelevent blocks are ignored
     **/
    private static class SignatureFormatter extends SimpleCellFormatter {
        private final PrintWriter pw;
        private final CastFileParser cp;
        private Set seen;
        private final Cadencize mCadencizer;

        public SignatureFormatter( final PrintWriter pw, 
                                   final CastFileParser cp,
                                   final Cadencize cadencizer ) {
            this.pw = pw;
            this.cp = cp;
            mCadencizer = cadencizer;
        }

        public CellInterface prepCell(CellInterface cell) {
            return cell;
        }

        /**
         * @pre cell != null
         **/
        public void outputCell(
                               final CellInterface cell,
                               final CellInterface envCell)
            throws CellFormatterException, IOException {

            seen = new HashSet();
            try {
                printNetlist(cell);
                printPRS(cell,seen);
                printDirectives(cell);
                printExclusives(cell);
                pw.println("env:");
                if(envCell != null ) {
                    printPRS(envCell,seen);
                    printDirectives(envCell);
                    printExclusives(envCell);
                }
            }
            catch(UnknownDirectiveException e) {
                throw new CellFormatterException("unknown directive");
            }

            // checkError also flushes
            if (pw.checkError())
                throw new IOException();
        }

        //block: directive(paramter) = value [type]
        private static MessageFormat parameterizedDirectiveFormat = 
            new MessageFormat("{0}: {1}({2}[{3}]) = {4}[{5}]" );
        //block: directive = value
        private static MessageFormat unparameterizedDirectiveFormat = 
            new MessageFormat("{0}: {1} = {2}" );

        
        void printDirectives(final CellInterface cell) 
            throws IOException, UnknownDirectiveException  {
            final DirectiveEmitter de = new SkillDirectiveEmitter();
            final DirectiveActionInterface dai = new DirectiveActionInterface() {
                    public void doUnParameterizedDirective(BlockInterface block,
                                                           DirectiveBlock db,
                                                           String directive,
                                                           Object value,
                                                           String valueType ) throws IOException {
                        final String[] args = new String[] 
                            { block.getType(),
                              directive,
                              de.emit(block.getType(),valueType,value) };
                        final String str =
                            unparameterizedDirectiveFormat.format(args);
                        pw.println(str);
                    }
                    public void doParameterizedDirectiveValue(BlockInterface block,
                                                              DirectiveBlock db,
                                                              String directive,
                                                              Object parameter,
                                                              Object value,
                                                              String parameterType,
                                                              String valueType) throws IOException {
                        final String[] args = new String[] 
                            { block.getType(), 
                              directive,
                              de.emit(block.getType(),parameterType, parameter),
                              parameterType,
                              de.emit(block.getType(),valueType, value),
                              valueType };
                        final String str =
                            parameterizedDirectiveFormat.format(args);
                        pw.println(str);
                    }
                    public void doParameterizedDirectiveType(BlockInterface block,
                                                             DirectiveBlock db,
                                                             String directive,
                                                             String parameterType,
                                                             String valueType) throws IOException {}
                    public void doBlockInterface(BlockInterface block) throws IOException {}
                };
            final DirectiveWalker dw = new DirectiveWalker(dai);
            final BlockInterface cellBlock = cell.getBlockInterface();
            dw.walk(cellBlock);
            dw.walk(cellBlock, BlockInterface.PRS);
            dw.walk(cellBlock, BlockInterface.SUBCELL);
            dw.walk(cellBlock, BlockInterface.ENV);
        }
        
        void printNetlist(CellInterface cell) throws IOException {
            CDLFactoryInterface factory =
                new CDLFactoryEmitter(pw, true, 79);
            
            Cast2Cdl.outputCDL(cell,
                               cp, 
                               factory,
                               mCadencizer,
                               false,
                               true);
        }


        void printPRS(final CellInterface cell, Set seen) {

            if (CellUtils.isWiring(cell) ||
                !seen.add(cell.getFullyQualifiedType())) return;

            final AliasedSet localNodes = mCadencizer.convert(cell).getLocalNodes();
            cell.getProductionRuleSet().canonicalizeNames(localNodes);

            printPRS(cell.getAssertedProductionRuleSet());
            printPRS(cell.getProductionRuleSet());
            
            for (final Iterator i = 
                     new SortingIterator(cell.getLocalSubcellPairs())
                     ; i.hasNext(); ) {
                final Pair p = (Pair) i.next();
                final HierName subcellName = (HierName) p.getFirst();
                final CellInterface subcell = (CellInterface) p.getSecond();
                pw.println("prs(" + subcell.getFullyQualifiedType() + ":" + 
                           subcellName.getCadenceString() + ")" );
                printPRS(subcell,seen);
            }
        }
        
        void printPRS(final ProductionRuleSet prs) {
            for (final Iterator i = 
                     new SortingIterator(prs.getProductionRules(),
                                         new StringRepComparator());
                 i.hasNext();) {
                ProductionRule pr = (ProductionRule) i.next();
                pw.println(pr.toString());
            }
        }

        void printExclusives(final CellInterface cell) {
            final AliasedSet localNodes = mCadencizer.convert(cell).getLocalNodes();
            OldLVSFormatter.printExclusives(localNodes,
                                            cell.getLocalExclusiveNodeSets(),
                                            HierName.makeHierName(""),
                                            pw
                                            );
        }
    }

    /**
     * HSIM output formatter.  Processes hierarchically, in contrast to
     * all the other formatters in this class.  The names that are output
     * are canonical.
     **/
    private static class HSIMFormatter extends SimpleCellFormatter {
        final CDLWriter pw;
        final PrintWriter w;
        private final CastFileParser cfp;
        private final int randSeqLen;
        private final Cadencize cadencizer;
        private final Set seen;
        private int deviceId;
        private final CDLNameInterface renamer;
        private String GND;
        private String Vdd;
        private final static Pattern RSOURCE = Pattern.compile("standard\\.random\\.rsource_([ae])1of\\(?(\\d+)\\)?");
        private final static Pattern BD_RSOURCE = Pattern.compile("standard\\.bd\\.rsource_bd\\((\\d+)\\)");
        private final Random random;

        HSIMFormatter( final PrintWriter pw,
                       final CastFileParser cfp, 
                       final int randSeqLen,
                       final long seed,
                       final CDLNameInterface renamer,
                       final Cadencize cadencizer ) {
            this.w = pw;
            this.pw = new CDLWriter(pw, 79);
            this.cfp = cfp;
            this.randSeqLen = randSeqLen;
            this.cadencizer = cadencizer;
            this.seen = new HashSet();
            this.deviceId = 0;
            this.renamer = renamer;
            this.random = new Random(seed);
        }

        private String printNode(final HierName hn) {
            String name = hn.getCadenceString();
            if (name.startsWith("$")) {
                name = "anon_" + name.substring(1);
            }

            try {
                return renamer.renameNode(name);
            } catch (Exception e) {
                throw new RuntimeException("Cannot rename node: " + name, e);
            }
        }

        private String printInstance(final HierName hn) {
            String name = hn.getCadenceString();
            if (name.startsWith("$")) {
                name = "anon_" + name.substring(1);
            }

            try {
                return renamer.renameSubCellInstance(name);
            } catch (Exception e) {
                throw new RuntimeException("Cannot rename instance: " + name, e);
            }
        }

        private String printCell(final String name) {
            try {
                return renamer.renameCell(name);
            } catch (Exception e) {
                throw new RuntimeException("Cannot rename cell: " + name, e);
            }
        }

        private String nextDevice() {
            return "env" + Integer.toString(++deviceId);
        }

        public CellInterface prepCell(CellInterface cell) {
            return cell;
        }

        /**
         * @pre cell != null
         **/
        public void outputCell(final CellInterface cell,
                               final CellInterface envCell)
            throws CellFormatterException, IOException {
            if (envCell == null) {
                if (cell != null) processCell(cell);
            } else {
                final AliasedSet aliases = cadencizer.convert(cell).getLocalNodes();
                final InstanceData instData = new InstanceData();
                instData.updateExtraDelay(cell, aliases);
                processCell(envCell, cell.getFullyQualifiedType() + "_env", cell, instData);
            }

            // checkError also flushes
            if (w.checkError())
                throw new IOException();
        }

        private void processCell(final CellInterface cell) {
            processCell(cell, null, null, null);
        }

        /**
         * Transform <code>rsource</code>s to <code>source</code>s, since
         * randomness cannot be simulated this way in SPICE.
         **/
        private CellInterface fixupRandom(final CellInterface cell) {
            final Matcher mat = RSOURCE.matcher(cell.getFullyQualifiedType());
            final Matcher bdmat = BD_RSOURCE.matcher(cell.getFullyQualifiedType());
            final StringBuffer sb = new StringBuffer();
            Stream<Number> rstream = null;
            if (mat.matches()) {
                sb.append("standard.source.source_");
                sb.append(mat.group(1)); // a or e
                sb.append("1of(");
                sb.append(mat.group(2)); // rails
                final int rails;
                try {
                    rails = Integer.parseInt(mat.group(2));
                } catch (NumberFormatException e) {
                    throw new RuntimeException("Invalid number of rails in rsource: " + cell.getFullyQualifiedType(), e);
                }
                rstream = Stream.generate(() -> random.nextInt(rails));
            } else if (bdmat.matches()) {
                sb.append("standard.bd.source_bd(");
                sb.append(bdmat.group(1)); // width
                final int width;
                try {
                    width = Integer.parseInt(bdmat.group(1));
                } catch (NumberFormatException e) {
                    throw new RuntimeException("Invalid width in bd_rsource: " + cell.getFullyQualifiedType(), e);
                }
                rstream = Stream.generate(() -> new BigInteger(width, random));
            } else {
                return cell;
            }

            sb.append(",");
            sb.append(randSeqLen);
            sb.append(",");
            sb.append(rstream.limit(randSeqLen)
                             .map(o -> o.toString())
                             .collect(Collectors.joining(",", "{", "}")));
            sb.append(")");

            final CellInterface src;
            try {
                src = cfp.getFullyQualifiedCell(sb.toString());
            } catch (Exception e) {
                System.err.println("Warning: Cannot convert " + cell.getFullyQualifiedType() + " to " + sb + ": " + e);
                return cell;
            }
            return src;
        }

        /**
         * Even though both a random source and a source have in the port list
         * GND, Vdd, _RESET, and an e1of(N) channel, when the cells are
         * emitted, the actual port ordering maybe different, depending on the
         * canonical names.  We want to substitute all random sources with
         * sources by handling everything normally, but printing out the
         * corresponding source cell name in place of the random source cell
         * name in the X line of the SPICE file.  Given a random source, and
         * the corresponding substitute source, this function returns a port
         * list for the random source cell that is consistent with the port
         * list of the source cell, so that it is safe to do the name
         * substitution.  See bug 9815.
         **/
        private String matchPorts(final CellInterface rsrc,
                                  final CellInterface src,
                                  final Collection result) {
            // an error message on why rsrc and src cannot be swapped for each
            // other
            String reason = null;

            final Set rnodes = new HashSet();
            PortDefinition rchan = null;
            reason = "the port list of " + rsrc.getFullyQualifiedType() +
                     " does not match the expected port list of a random" +
                     " source";
            for (Iterator i = rsrc.getPortDefinitions(); i.hasNext(); ) {
                final PortDefinition def = (PortDefinition) i.next();
                final PortTypeInterface type = def.getType();
                if (type instanceof NodeType) {
                    rnodes.add(def.getName());
                } else if (type instanceof ChannelType) {
                    if (rchan == null) rchan = def;
                    else return reason;
                } else {
                    return reason;
                }
            }

            final Set nodes = new HashSet();
            PortDefinition chan = null;
            reason = "the port list of " + src.getFullyQualifiedType() +
                     " does not match the expected port list of a source";
            for (Iterator i = src.getPortDefinitions(); i.hasNext(); ) {
                final PortDefinition def = (PortDefinition) i.next();
                final PortTypeInterface type = def.getType();
                if (type instanceof NodeType) {
                    nodes.add(def.getName());
                } else if (type instanceof ChannelType) {
                    if (chan == null) chan = def;
                    else return reason;
                } else {
                    return reason;
                }
            }

            reason = "the port lists of " + rsrc.getFullyQualifiedType() +
                     " and " + src.getFullyQualifiedType() +
                     " are incompatible";
            // Allow any number of nodes, as long as both cells have them;
            // this is meant to handle implied nodes like GND, Vdd, _RESET
            if (!rnodes.containsAll(nodes) || !nodes.containsAll(rnodes)) {
                return reason;
            }

            final Set rsubnodes = CellUtils.markPorts(((ChannelType) rchan.getType()).iterator()).keySet();
            final Set subnodes = CellUtils.markPorts(((ChannelType) chan.getType()).iterator()).keySet();
            // Ensure the channels in the port lists are identical
            if (!rsubnodes.containsAll(subnodes) ||
                !subnodes.containsAll(rsubnodes)) {
                return reason;
            }

            final AliasedMap rsrcPorts =
                cadencizer.convert(rsrc).getPortNodes();
            final AliasedMap srcPorts =
                cadencizer.convert(src).getPortNodes();
            // create a mapping from source port name to random source port name
            final Map mapping = new HashMap();
            try {
                for (Iterator i = nodes.iterator(); i.hasNext(); ) {
                    final String s = (String) i.next();
                    final HierName h = HierName.makeHierName(s, '.');
                    mapping.put(srcPorts.getCanonicalKey(h),
                                rsrcPorts.getCanonicalKey(h));
                }
                for (Iterator i = subnodes.iterator(); i.hasNext(); ) {
                    final String port = (String) i.next();
                    final String s = chan.getName() + "." + port;
                    final String rs = rchan.getName() + "." + port;
                    final HierName h = HierName.makeHierName(s, '.');
                    final HierName rh = HierName.makeHierName(rs, '.');
                    mapping.put(srcPorts.getCanonicalKey(h),
                                rsrcPorts.getCanonicalKey(rh));
                }
            } catch (InvalidHierNameException e) {
                return "cannot create HierName: " + e.getMessage();
            }

            for (Iterator i = NetlistAdapter.getParameterList(src, cadencizer)
                                            .iterator(); i.hasNext(); ) {
                final HierName h = (HierName) i.next();
                result.add(mapping.get(h));
            }

            return null;
        }

        private void copyAliases(final AliasedMap from, final AliasedSet to) {
            for (Iterator i = from.getCanonicalKeys(); i.hasNext(); ) {
                final Object key = i.next();
                for (Iterator j = from.getAliases(key); j.hasNext(); ) {
                    final Object alias = j.next();
                    to.makeEquivalent(key, alias);
                }
            }
        }

        private String[] getActuals(final CellInterface cell,
                                    final AliasedSet aliases) {
            return NetlistAdapter.getParameterList(cell, cadencizer)
                                 .stream()
                                 .map(p -> printNode((HierName)
                                             aliases.getCanonicalKey(p)))
                                 .toArray(String[]::new);
        }

        private String getAliases(final AliasedSet aliases,
                                  final HierName key) {
            final HierName canon = (HierName) aliases.getCanonicalKey(key);
            final Iterable<HierName> alias = CollectionUtils.iterable(
                        (Iterator<HierName>) aliases.getAliases(canon));
            return Stream.concat(Stream.of(canon),
                                 StreamSupport.stream(alias.spliterator(),
                                                      false)
                                              .filter(p -> p != canon))
                         .map(p -> printNode(p))
                         .collect(Collectors.joining("="));
        }

        private Stream<HierName> getNetDirectives(final CellInterface cell,
                                                  final AliasedSet aliases,
                                                  final String directive) {
            return ((Set<HierName>) DirectiveUtils.canonize(
                           aliases,
                           DirectiveUtils.getExplicitTrues(
                               DirectiveUtils.getTopLevelDirective(cell,
                                   directive, DirectiveConstants.NODE_TYPE))))
                       .stream()
                       .filter(p -> aliases.getCanonicalKey(p) != null);
        }

        private Stream<String> getNetString(final CellInterface cell,
                                            final AliasedSet aliases,
                                            final String directive) {
            return getNetDirectives(cell, aliases, directive).map(
                    p -> directive + ":" + getAliases(aliases, p));
        }

        private Stream<String> getNetString(final CellInterface cell,
                                            final AliasedSet aliases) {
            return Stream.of(
                    getNetString(cell, aliases, DirectiveConstants.GROUND_NET),
                    getNetString(cell, aliases, DirectiveConstants.POWER_NET),
                    getNetString(cell, aliases, DirectiveConstants.RESET_NET))
                  .reduce(Stream.empty(), Stream::concat);
        }

        private String pickSupplyName(final CellInterface cell,
                                      final AliasedSet aliases,
                                      final String directive,
                                      final String def) {
            final TreeSet<HierName> candidates =
                getNetDirectives(cell, aliases, directive).collect(
                        Collectors.toCollection(TreeSet::new));
            final String result;
            if (candidates.isEmpty()) {
                result = def;
                System.err.println("Warning: No " + directive + " in " +
                        cell.getFullyQualifiedType() + ", using default " +
                        def);
            } else {
                final HierName first = candidates.first();
                result = printNode(first);
                if (candidates.size() > 1) {
                    System.err.println("Warning: Too many " + directive + " in " +
                            cell.getFullyQualifiedType() + ": " + candidates +
                            " using " + first);
                }
            }
            return result;
        }

        /**
         * Output the exclusion properties, connections, and, if 
         * <code>isEnv</code> the prs for the cell.
         *
         * @param isEnv  If the cell results from a named environment.
         **/
        private void processCell(final CellInterface cell,
                                 final String envName,
                                 final CellInterface parentCell,
                                 final InstanceData instData) {
            if (!seen.add(cell.getFullyQualifiedType())) return;

            // XXX: we can't print out rules involving ERROR because
            // that handling is all messed up.  See Bug 1047.
            // processPRS(cell.getAssertedProductionRuleSet(), prefix);

            /* Save a copy of fully qualified type names of the fixed up cells
             * for use later.  In particular, this is necessary since the meta
             * parameter to the sources are randomly generated. */
            final Map fixupCells = new HashMap();

            AliasedSet combined = null;
            if (parentCell != null) {
                final AliasedMap dutPorts =
                    cadencizer.convert(parentCell).getPortNodes();
                final AliasedMap envPorts =
                    cadencizer.convert(cell).getPortNodes();
                combined = new AliasedSet(dutPorts.getComparator());
                copyAliases(dutPorts, combined);
                copyAliases(envPorts, combined);

                w.println("** JFlat:begin");
                w.print(getNetString(parentCell, combined)
                            .map(s -> "** JFlat:" + s + "\n")
                            .collect(Collectors.joining("")));
                w.println("** JFlat:end");
            }

            // recurse to subcells
            for (final Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
                final Pair p = (Pair) i.next();
                final CellInterface subcell = (CellInterface) p.getSecond();
                if (!CellUtils.isWiring(subcell)) {
                    final CellInterface fixed = fixupRandom(subcell);
                    if (fixed != subcell) fixupCells.put(p.getFirst(), fixed);
                    processCell(fixed);
                }
            }

            final AliasedSet localNodes =
                cadencizer.convert(cell).getLocalNodes();

            // XXX: we might want to do error checking, ie error
            // if !hasCompletePrs && !hasSubcells
            final String[] portNames = 
                 NetlistAdapter.getParameterList(cell, cadencizer)
                               .stream()
                               .map(p -> printNode(p))
                               .toArray(String[]::new);

            final String cellName = (envName == null ? 
                                     cell.getFullyQualifiedType() :
                                     envName);
            pw.subckt(printCell(cellName),portNames,new String[0], new String[0]);

            // Canonicalize the production rules
            final ProductionRuleSet prs = cell.getProductionRuleSet();
            if (prs.size() > 0) {
                prs.canonicalizeNames(localNodes);
                final CellDelay delay = new CellDelay(cell, cadencizer);
                GND = pickSupplyName(cell, localNodes,
                                     DirectiveConstants.GROUND_NET, "GND");
                Vdd = pickSupplyName(cell, localNodes,
                                     DirectiveConstants.POWER_NET, "Vdd");
                processPRS(prs,delay,instData);
            }

            // Emit subcircuit calls
            for (final Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
                final Pair p = (Pair) i.next();
                final HierName instance = (HierName) p.getFirst();
                final CellInterface subcell = (CellInterface) p.getSecond();
                final CellInterface realSubcell =
                    (CellInterface) fixupCells.get(instance);
                if (CellUtils.isWiring(subcell)) continue;

                final List subports = new ArrayList();
                final Collection paramList;
                if (realSubcell == null) {
                    paramList = NetlistAdapter.getParameterList(subcell,
                                                                cadencizer);
                } else {
                    paramList = new ArrayList();
                    final String failed =
                        matchPorts(subcell, realSubcell, paramList);
                    if (failed != null) {
                        throw new RuntimeException(
                            "Cannot substitute a rsource (" + instance +
                            ") with a source: " + failed);
                    }
                }
                for (Iterator j = paramList.iterator(); j.hasNext(); ) {
                    final HierName subportName =
                        HierName.append(instance, (HierName) j.next());
                    final HierName canon =
                        (HierName) localNodes.getCanonicalKey(subportName);
                    subports.add(printNode(canon));
                }

                pw.X(printInstance(instance),
                     (String[]) subports.toArray(new String[0]),
                     printCell(realSubcell == null ?
                                  subcell.getFullyQualifiedType() :
                                  realSubcell.getFullyQualifiedType()),
                     new String[0]);
            }

            pw.ends(printCell(envName == null ?  cell.getFullyQualifiedType() :
                              envName));

            // Emit dut and env instantiations
            if (parentCell != null) {
                pw.X("dut", getActuals(parentCell, combined),
                     printCell(parentCell.getFullyQualifiedType()),
                     new String[0]);
                pw.X("env", getActuals(cell, combined), printCell(envName),
                     new String[0]);
            }
        }

        private void processPRS(final ProductionRuleSet prs,
                                final CellDelay delay,
                                final InstanceData instData) {
            final Set targets = new HashSet();
            // isolate current draw of PRS from power supplies
            pw.E(nextDevice(), "ENV_PRS_TRUE",  "0", "VCVS", new String[]{Vdd,"0","gain=1"});
            pw.E(nextDevice(), "ENV_PRS_FALSE", "0", "VCVS", new String[]{GND,"0","gain=1"});
            // emit the PRS
            for (final Iterator i = prs.getProductionRules(); i.hasNext(); ) {
                final ProductionRule pr = (ProductionRule) i.next();
                printPR(pr, delay, instData, targets);
            }
        }

        protected void printPR(final ProductionRule pr,
                               final CellDelay delay,
                               final InstanceData instData,
                               final Set targets) {
            final List args = new ArrayList();
            final Iterator iD =
                pr.getGuard().DNFForm().getDisjuncts().iterator();
            pw.comment(pr.toString());
            while (iD.hasNext()) {
                final AndBooleanExpressionInterface disj =
                    (AndBooleanExpressionInterface) iD.next();
                final Iterator iC = disj.getConjuncts().iterator();
                int count = 0;

                // guard
                while (iC.hasNext()) {
                    final HierNameAtomicBooleanExpression conj =
                        (HierNameAtomicBooleanExpression) iC.next();

                    final String cName = printNode(conj.getName());
                    if (conj.getSense()) {
                        args.add(cName);
                        args.add(GND);
                    } else {
                        args.add(Vdd);
                        args.add(cName);
                    }
                    ++count;
                }

                final String target = printNode(pr.getTarget());
                final String targ_src = target + "_3a_source";
                final String np, nn;
                if (pr.getDirection() == ProductionRule.UP) {
                    np = "ENV_PRS_TRUE";
                    nn = targ_src;
                } else {
                    np = targ_src;
                    nn = "ENV_PRS_FALSE";
                }

                args.add(0, "AND(" + count + ")");
                args.add("0,PrsMaxRes");
                args.add("Vlo,PrsMaxRes");
                args.add("Vhi,PrsMinRes");
                args.add("true,PrsMinRes");

                pw.G(nextDevice(), np, nn, "vcr",
                     (String[]) args.toArray(new String[0]));

                args.clear();

                if (targets.add(target)) {
                    pw.C(nextDevice(), targ_src, GND, null, "PrsCap",
                         new String[] { "$MODEL=prescap" });
                    final boolean up = pr.getDirection() == ProductionRule.UP;
                    final int extraDelay = instData == null ? 0 :
                        Math.round(instData.getExtraDelay(up, pr.getTarget()));
                    int after = (int) delay.getDelay(pr.getTarget(), up, 100) + extraDelay;
                    after = (after>0 ? after : 1); // avoid 0 delays, because they don't work
                    String afterStr = pr.isAbsolute() ? after + "ps" : "'" + after/100.0 + "*PrsDelay'";
                    pw.E(nextDevice(), target, GND, "delay",
                         new String[] { targ_src, GND, "TD=" + afterStr});
                }
            }
        }
    }
    public static class CellTraverser {
        final CellProcessor proc;
        final Cadencize cadencizer;
        final boolean useSuperNamespace;

        public CellTraverser(final CellProcessor p,
                             final Cadencize cadencizer,
                             final boolean useSuperNamespace) {
            this.proc = p;
            this.cadencizer = cadencizer;
            this.useSuperNamespace = useSuperNamespace;
        }

        public void traverseCell(final CellInterface cell) {
            traverseIter(null, cell,
                         useSuperNamespace ?
                         new AliasedSet(HierName.getComparator()) :
                         null, 0);
        }

        private void traverseIter(final HierName prefix,
                                  final CellInterface cell, 
                                  final AliasedSet superNamespace,
                                  final int currentDepth) {
            proc.process(prefix, cell, superNamespace, currentDepth,
                         cadencizer);

            // recurse to subcells
            for (Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
                final Pair p = (Pair) i.next();
                final HierName subcellName = (HierName) p.getFirst();
                final CellInterface subcell = (CellInterface) p.getSecond();
                final AliasedSet subcellSuperNamespace;
                final HierName fullName = HierName.append(prefix, subcellName);

                if (subcell.isNode())
                    continue;

                if (superNamespace != null) {
                    subcellSuperNamespace =
                        computeSubcellSuperNamespace(prefix, cell, 
                                                     subcell, subcellName, superNamespace);
                } else
                    subcellSuperNamespace = null;

                traverseIter(fullName, subcell, subcellSuperNamespace, 
                             currentDepth + 1);
            }
        }

        private AliasedSet computeSubcellSuperNamespace(
                                                        final HierName prefix,
                                                        final CellInterface cell, final CellInterface subcell,
                                                        final HierName subcellName,
                                                        final AliasedSet superNamespace) {
            final CadenceInfo ci = cadencizer.convert(cell);
            final CadenceInfo subci = cadencizer.convert(subcell);
            final AliasedMap subPorts = subci.getPortNodes();

            // for all ports of cell, find their canonical local name,
            // then look that up in the super name space to get the
            // global canonical name.
            final AliasedSet subcellSuperNamespace =
                new AliasedSet(HierName.getComparator());

            for (final Iterator iSubPortCanon = subPorts.getCanonicalKeys();
                 iSubPortCanon.hasNext(); ) {

                final HierName subCanon = (HierName) iSubPortCanon.next();

                for (final Iterator iSubPortConn =
                         subPorts.getAliases(subCanon);
                     iSubPortConn.hasNext(); ) {

                    final HierName subConn = (HierName) iSubPortConn.next();
                    // @review jmr I'm really not sure if I need to
                    // be calling prefixName, or if append suffices.
                    // It can't hurt to call prefixName, though.
                    final HierName prefixedSubConn =
                        HierName.prefixName(subcellName, subConn);

                    final HierName localCanon = (HierName)
                        ci.getLocalNodes().getCanonicalKey(prefixedSubConn);

                    Debug.assertTrue(localCanon != null);

                    final HierName globalCanon = (HierName)
                        superNamespace.getCanonicalKey(localCanon);

                    if (globalCanon != null)
                        subcellSuperNamespace.makeEquivalent(globalCanon,
                                                             HierName.prefixName(prefix, prefixedSubConn));
                    else if (localCanon != prefixedSubConn)
                        subcellSuperNamespace.makeEquivalent(
                                                             HierName.prefixName(prefix, localCanon),
                                                             HierName.prefixName(prefix, prefixedSubConn));
                }
            }

            return subcellSuperNamespace;
        }
    }

    private static class LVSFormatter
        extends SimpleCellFormatter
        implements CellProcessor {
        protected final PrintWriter pw;
        protected final boolean printExclusivesP;
        protected final boolean useSuperNamespace;
        private final Cadencize mCadencizer;

        public LVSFormatter( final PrintWriter pw, 
                             final boolean useSuperNamespace,
                             final Cadencize cadencizer ) {
            this.pw = pw;
            this.printExclusivesP = true;
            this.useSuperNamespace = useSuperNamespace;
            mCadencizer = cadencizer;
        }

        public CellInterface prepCell(CellInterface cell) {
            return cell;
        }

        /**
         * Prints prs (if non-env), exclhi/excllo, and connections defined
         * in the local node.  Does not traverse to subcells.  Does not
         * print connections to subcells.
         **/
        public void outputCell(CellInterface cell, CellInterface envCell)
            throws CellFormatterException, IOException {
            if (envCell != null)
                throw new CellFormatterException(
                                                 "Environment not supported.");
            new CellTraverser(this, mCadencizer, useSuperNamespace)
                .traverseCell(prepCell(cell));
            if (pw.checkError())
                throw new IOException();
        }

        public void process(final HierName prefix, final CellInterface cell,
                            final AliasedSet superNamespace, final int currentDepth,
                            final Cadencize cadencizer) {
            final CadenceInfo ci = cadencizer.convert(cell);

            if (printExclusivesP)
                printExclusives(prefix, cell.getLocalExclusiveNodeSets(), ci);
            printPRS(prefix, cell.getProductionRuleSet(), ci);
            printConnections(prefix, cell, ci);
        }

        protected void printExclusives(final HierName prefix,
                                       final ExclusiveNodeSets excls,
                                       final CadenceInfo ci) {
            for (final Iterator iENS = excls.getIterator(); iENS.hasNext(); ) {
                final ExclusiveNodeSet ens = (ExclusiveNodeSet) iENS.next();

                switch (ens.getHiLo()) {
                case ExclusiveNodeSet.HI: pw.print("exclhi("); break;
                case ExclusiveNodeSet.LO: pw.print("excllo("); break;
                case ExclusiveNodeSet.CC: pw.print("exclcc("); break;
                default: throw new AssertionError("Invalid ExclusiveNodeSet hiLo value: " + ens.getHiLo());
                }

                boolean firstP = true;
                for (final Iterator iNode = ens.getNodes(); iNode.hasNext();) {
                    final HierName hn = (HierName) iNode.next();

                    if (firstP) 
                        firstP = false;
                    else 
                        pw.print(',');

                    pw.print('"');
                    pw.print(prefix.toString());
                    pw.print('.');
                    pw.print(canonicalName(ci, hn).getAspiceString());
                    pw.print('"');
                }

                pw.println(')');
            }
        }

        protected void printPRS(final HierName prefix, final ProductionRuleSet prs,
                                final CadenceInfo ci) {
            for (final Iterator iPr = prs.getProductionRules(); iPr.hasNext();) {
                ProductionRule pr = (ProductionRule) iPr.next();
                pr = pr.mapNames(new UnaryFunction() {
                        public Object execute(final Object o) {
                            final HierName n =
                                canonicalName(ci, (HierName) o);
                            if (prefix == null)
                                return n;
                            else
                                return HierName.append(
                                                       prefix,
                                                       n);
                        }});

                printPR(pr);
            }
        }

        protected void printPR(final ProductionRule pr) {
            final BooleanExpressionInterface be = pr.getGuard();
            final HierName hn = pr.getTarget();
            final int dir = pr.getDirection();
            pw.println(be.toUserVisibleString()
                       + " -> \""
                       + hn.getAspiceString()
                       + (dir == ProductionRule.UP ? "\"+" : "\"-"));
        }

        protected void printConnections(final HierName prefix,
                                        final CellInterface cell,
                                        final CadenceInfo ci) {
            for (final Iterator iCanonNode = cell.getCanonicalNodes();
                 iCanonNode.hasNext(); ) {
                final HierName canonNode = (HierName) iCanonNode.next();
                final String canonName =
                    HierName.prefixName(prefix, canonNode).getAspiceString();

                for (final Iterator iConnNode =
                         cell.getConnectedNodes(canonNode);
                     iConnNode.hasNext(); ) {
                    final HierName connNode = (HierName) iConnNode.next();
                    final String connName =
                        HierName.prefixName(prefix, connNode)
                        .getAspiceString();

                    // don't print self-connections
                    if (connNode != canonNode)
                        pw.println("connect \"" + canonName
                                   + "\" \"" + connName + '"');
                }
            }
        }

        protected HierName canonicalName(final CadenceInfo ci, final HierName n) {
            final HierName c =
                (HierName) ci.getLocalNodes().getCanonicalKey(n);

            if (c == null)
                return n;
            else
                return c;
        }
    }

    private static class OldLVSFormatter extends SimpleCellFormatter {
        protected final PrintWriter pw;
        protected final Cadencize cadencizer;
        public OldLVSFormatter(PrintWriter pw, Cadencize cadencizer) {
            this.pw = pw;
            this.cadencizer = cadencizer;
        }

        public CellInterface prepCell(CellInterface cell) { return cell.flatten(); }
        /**
         * Prints prs (if non-env), exclhi/excllo, and connections defined in the
         * local node.  Does not traverse to subcells.  Does not print
         * connections to subcells.
         **/
        public void outputCell(
                               final CellInterface cell,
                               final CellInterface envCell) 
            throws CellFormatterException, IOException {
            if (envCell != null)
                throw new CellFormatterException(
                                                 "Environment not supported.");
            printExclusives(cell.getLocalExclusiveNodeSets());
            final ProductionRuleSet prs = cell.getProductionRuleSet();
            printPRS(prs, pw);
            printConnections(cell, pw);
            // checkError also flushes
            if (pw.checkError()) { throw new IOException(); }
        }

        void printExclusives(final ExclusiveNodeSets enss) {
            printExclusives(enss, null, pw);
        }

        public static void printExclusives(
                                           final ExclusiveNodeSets enss,
                                           final HierName prefix,
                                           final PrintWriter pw) {
            printExclusives(null,enss,prefix, pw);
        }

        //if cadencizer and cell are given non-null, it uses canonical names
        //on cell namespace
        public static void printExclusives(final AliasedSet localNodes,
                                           final ExclusiveNodeSets enss,
                                           final HierName prefix,
                                           final PrintWriter pw) {
            Iterator iENS = enss.getIterator();
            if(localNodes != null) {
                iENS =
                    new SortingIterator(
                                        new MappingIterator(
                                                            iENS,
                                                            new UnaryFunction() {
                                                                public Object execute(Object o) {
                                                                    return ((ExclusiveNodeSet)o).canonicalizeNames(localNodes);
                                                                }
                                                            }
                                                            ),
                                        new StringRepComparator()
                                        );
            }
            for (;iENS.hasNext();) {
                final ExclusiveNodeSet ens = (ExclusiveNodeSet) iENS.next();
                printExclusive(ens, prefix, pw);
            }
        }

        public static void printExclusive(
                                          final ExclusiveNodeSet ens,
                                          final HierName prefix,
                                          final PrintWriter pw) {
            switch (ens.getHiLo()) {
            case ExclusiveNodeSet.HI: pw.print("exclhi("); break;
            case ExclusiveNodeSet.LO: pw.print("excllo("); break;
            case ExclusiveNodeSet.CC: pw.print("exclcc("); break;
            case ExclusiveNodeSet.NOCC: pw.print("nocc("); break;
            default: throw new AssertionError("Invalid ExclusiveNodeSet hiLo value: " + ens.getHiLo());
            }

            boolean firstP = true;
            for (final Iterator iNode = 
                     new SortingIterator(ens.getNodes());
                 iNode.hasNext();) {
                final HierName hn = (HierName) iNode.next();

                if (firstP)
                    firstP = false;
                else
                    pw.print(',');

                printName(prefix,
                          hn,
                          pw);
            }

            pw.println(')');
        }

        public static void printName(
                                     final HierName prefix,
                                     final HierName hn,
                                     final PrintWriter pw) {
            printName(prefix == null ? null : prefix.getAspiceString(),
                      hn.getAspiceString(), pw);
        }

        public static void printName(
                                     final String prefix,
                                     final String hn,
                                     final PrintWriter pw) {
            pw.print('"');
            if (prefix != null) {
                pw.print(prefix);
                pw.print('.');
            }
            pw.print(hn);
            pw.print('"');
        }

        void printPRS(final ProductionRuleSet prs, final PrintWriter pw) {
            for (final Iterator iPr = prs.getProductionRules(); iPr.hasNext();) {
                final ProductionRule pr = (ProductionRule) iPr.next();
                final BooleanExpressionInterface be = pr.getGuard();
                final HierName hn = pr.getTarget();
                final int dir = pr.getDirection();
                pw.println(be.toUserVisibleString()
                           + " -> \"" + hn.getAspiceString() 
                           + (pr.getDirection() == ProductionRule.UP ? "\"+" : "\"-"));
            }
        }
        void printConnections(final CellInterface cell, final PrintWriter pw) {
            for (final Iterator iCanonNode = cell.getCanonicalNodes(); iCanonNode.hasNext(); ) {
                final HierName canonNode = (HierName) iCanonNode.next();
                for (final Iterator iConnNode = cell.getConnectedNodes(canonNode);
                     iConnNode.hasNext(); ) {
                    final HierName connNode = (HierName) iConnNode.next();
                    // don't print self-connections
                    if (connNode != canonNode)
                        pw.println("connect \"" + canonNode.getAspiceString()
                                   + "\" \"" + connNode.getAspiceString() + '"');
                }
            }
        }
    }

    private static class AutoFormatter extends LVSFormatter {

        public AutoFormatter(PrintWriter pw, Cadencize cadencizer ) {
            super(pw, false, cadencizer);
        }

        public void process(final HierName prefix, final CellInterface cell,
                            final AliasedSet superNamespace, final int currentDepth,
                            final Cadencize cadencizer) {
            final CadenceInfo ci = cadencizer.convert(cell);

            printExclusives(prefix, cell.getLocalExclusiveNodeSets(), ci);
            if (currentDepth <= 1) {
                printPRS(prefix, cell.getProductionRuleSet(), ci);
            }
            printConnections(prefix, cell, ci);
        }
    }

    private static class OldAutoFormatter extends OldLVSFormatter {
        public OldAutoFormatter(PrintWriter pw, Cadencize cadencizer) {
            super(pw, cadencizer);
        }
        public CellInterface prepCell(CellInterface cell) { return cell.flatten(1); }
    }

    private static class InstanceFormatter extends SimpleCellFormatter {
        private PrintWriter pw;

        public InstanceFormatter(PrintWriter pw) {
            this.pw = pw;
        }
        
        public CellInterface prepCell(CellInterface cell) { return cell; }
        /**
         * Prints cell type and full name for whole hierarchy, one per line.
         **/
        public void outputCell(
                               final CellInterface cell,
                               final CellInterface envCell)
            throws CellFormatterException, IOException {
            if (envCell != null)
                throw new CellFormatterException(
                                                 "Environment not supported.");

            printInstanceInfo(null, cell, pw);

            // checkError also flushes
            if (pw.checkError())
                throw new IOException();
        }

        private void printInstanceInfo(final String prefix, final CellInterface cell, final PrintWriter pw) 
            throws IOException {
            for (final Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
                final Pair p = (Pair) i.next();
                final HierName subcellName = (HierName) p.getFirst();
                final CellInterface subcell = (CellInterface) p.getSecond();

                final String fullName;
                if (prefix == null) {
                    fullName = subcellName.getAspiceString();
                } else {
                    fullName = prefix + "." + subcellName.getAspiceString();
                }

                if (subcell.hasRealProductionRule()) {
                    pw.println(subcell.getFullyQualifiedType() + " " + fullName);
                    printInstanceInfo(fullName, subcell, pw);
                }
            }
        }
    }

    /**
     * Cosim output formatter.  Processes hierarchically, in contrast to
     * all the other formatters in this class.  The names that are output
     * will not be canonical.  The wire statements are needed to make
     * sense of the output circuit.
     **/
    private static class CosimFormatter extends SimpleCellFormatter {
        final PrintWriter pw;
        private final Cadencize cadencizer;
        private final HierName analogCell;
        private final CellInterface merge;

        CosimFormatter( final PrintWriter pw,
                        final String analogCell,
                        final CellInterface merge,
                        final Cadencize cadencizer ) {
            this.pw = pw;
            this.cadencizer = cadencizer;
            this.analogCell = makeHierName(analogCell, '.');
            this.merge = merge;
        }

        public CellInterface prepCell(CellInterface cell) {
            return cell;
        }

        public void printName(final HierName hn) {
            OldLVSFormatter.printName(null, hn, pw);
        }

        /**
         * @pre cell != null
         **/
        public void outputCell(final CellInterface cell,
                               final CellInterface envCell)
            throws CellFormatterException, IOException {
            assert cell != null && envCell != null;

            final Set seen = new HashSet();
            processCell(cell, seen);
            processCell(envCell, seen);
            processCell(merge, seen);
            processGlue(cadencizer.convert(cell).getPortNodes(),
                        cell,
                        HierName.makeHierName("D"),
                        analogCell,
                        HierName.makeHierName("A"),
                        envCell,
                        HierName.makeHierName("env"));

            // checkError also flushes
            if (pw.checkError())
                throw new IOException();
        }

        /**
         * Output the exclusion properties, connections, and, if 
         * <code>isEnv</code> the prs for the cell.
         **/
        private void processCell(final CellInterface cell, final Set seen) {
            final String type = cell.getFullyQualifiedType();
            if (CellUtils.isWiring(cell) || !seen.add(type)) return;

            // recurse to subcells
            for (final Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
                final Pair p = (Pair) i.next();
                final CellInterface subcell = (CellInterface) p.getSecond();
                processCell(subcell, seen);
            }

            final Collection ports = 
                NetlistAdapter.getParameterList(cell, cadencizer);

            // Emit cell header
            pw.print("define \"" + type + "\" (");
            for (Iterator i = ports.iterator(); i.hasNext(); ) {
                printName((HierName) i.next());
                if (i.hasNext()) pw.print(", ");
            }
            pw.println(")() {");

            final CellDelay delay = new CellDelay(cell, cadencizer);


            processPRS(cell.getProductionRuleSet(), delay);
            // XXX: we can't print out rules involving ERROR because
            // that handling is all messed up.  See Bug 1047.
            processPRS(cell.getAssertedProductionRuleSet(), delay);

            // we don't handle netlist/cdl blocks yet...
            processExclusives(cell.getLocalExclusiveNodeSets(), null);
            for (final Iterator i = cell.getPortSubcellPairs(); i.hasNext(); ) {
                final Pair p = (Pair) i.next();
                final HierName subcellName = (HierName) p.getFirst();
                final CellInterface subcell = (CellInterface) p.getSecond();
                processExclusives(subcell.getLocalExclusiveNodeSets(),
                                  subcellName);
            }

            // emit instances
            final AliasedSet as = cadencizer.convert(cell).getLocalNodes();
            for (final Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
                final Pair p = (Pair) i.next();
                final HierName subcellName = (HierName) p.getFirst();
                final CellInterface subcell = (CellInterface) p.getSecond();

                if (!CellUtils.isWiring(subcell)) 
                    processInstance(as, subcellName, subcell);
            }

            processConnections(cell);

            processPortConnections(cell);

            // Emit cell footer
            pw.println('}');
        }

        private void processPRS(final ProductionRuleSet prs,
                                final CellDelay delay) {
            if (prs.size() > 0) {
                pw.println("dsim {");
                for (final Iterator i = prs.getProductionRules(); i.hasNext(); )
                    {
                        final ProductionRule pr = (ProductionRule) i.next();
                        printPR(pr, delay);
                    }
                pw.println('}');
            }
        }

        protected void printPR(final ProductionRule pr, final CellDelay delay) {
            final Iterator iD =
                pr.getGuard().DNFForm().getDisjuncts().iterator();
            while (iD.hasNext()) {
                final AndBooleanExpressionInterface disj =
                    (AndBooleanExpressionInterface) iD.next();
                final Iterator iC = disj.getConjuncts().iterator();

                // modifiers
                pw.print("env " + pr.flagsString());

                final int after = (int)
                    delay.getDelay(pr.getTarget(), 
                                   pr.getDirection() == ProductionRule.UP,
                                   100);

                pw.print("after " + after + " ");

                // guard
                boolean firstP = true;
                while (iC.hasNext()) {
                    final HierNameAtomicBooleanExpression conj =
                        (HierNameAtomicBooleanExpression) iC.next();

                    if (firstP) firstP = false;
                    else pw.print('&');

                    if (!conj.getSense()) pw.print('~');
                    printName(conj.getName());
                }

                pw.print(" -> ");

                // target
                printName(pr.getTarget());

                // direction
                pw.println(pr.getDirection() == ProductionRule.UP ? '+' : '-');
            }
        }

        private void processExclusives(final ExclusiveNodeSets enss,
                                       final HierName prefix) {
            OldLVSFormatter.printExclusives(enss, prefix, pw);
        }

        private void processConnections(final CellInterface cell) {
            for (final Iterator iCanonNode = cell.getCanonicalNodes();
                 iCanonNode.hasNext(); ) {
                final HierName canonNode = (HierName) iCanonNode.next();
                boolean firstP = true;
                for (final Iterator iConnNode =
                         cell.getConnectedNodes(canonNode);
                     iConnNode.hasNext(); ) {
                    final HierName connNode = (HierName) iConnNode.next();
                    // don't print self-connections
                    if (connNode != canonNode) {
                        if (firstP) {
                            firstP = false;
                            pw.print("wire(");
                            printName(canonNode);
                        }

                        pw.print(',');
                        printName(connNode);
                    }
                }
                if (!firstP)
                    pw.println(')');
            }
        }

        private void processInstance(final AliasedSet as,
                                     final HierName inst,
                                     final CellInterface subcell) {
            final Collection ports = 
                NetlistAdapter.getParameterList(subcell, cadencizer);
            pw.print("\"" + subcell.getFullyQualifiedType() + "\" ");
            printName(inst);
            pw.print(" (");
            for (Iterator i = ports.iterator(); i.hasNext(); ) {
                final HierName port = (HierName) i.next();
                final HierName canon = 
                    (HierName) as.getCanonicalKey(HierName.append(inst, port));
                assert canon != null :
                    "Cannot get canonical name for: " + inst + "." + port;

                printName(canon);
                if (i.hasNext()) pw.print(", ");
            }
            pw.println(") ()");
        }

        private void processGlue(final AliasedMap am,
                                 final CellInterface cell,
                                 final HierName digitalInst,
                                 final HierName analogCell,
                                 final HierName analogInst,
                                 final CellInterface envCell,
                                 final HierName envInst) {
            final Collection ports = 
                NetlistAdapter.getParameterList(cell, cadencizer);

            final HierName htype =
                makeHierName(cell.getFullyQualifiedType(), '.');
            processTopLevel(ports, am, digitalInst, htype);
            processTopLevel(ports, am, analogInst, analogCell);

            final Collection envPorts = 
                NetlistAdapter.getParameterList(envCell, cadencizer);
            final CadenceInfo ci = cadencizer.convert(envCell);
            processTopLevel(envPorts, ci.getPortNodes(), envInst, makeHierName(envCell.getFullyQualifiedType(), '.'));

            final Map portDir = CellUtils.markPorts(envCell);
            final Map canonPortDir = new HashMap();
            final AliasedMap envam =
                cadencizer.convert(envCell).getPortNodes();
            for (Iterator i = portDir.entrySet().iterator(); i.hasNext(); ) {
                final Map.Entry entry = (Map.Entry) i.next();
                final HierName port =
                    makeHierName((String) entry.getKey(), '.');
                final Object canon = envam.getCanonicalKey(port);
                assert canon != null :
                    "Cannot get canonical name for: " + port;

                canonPortDir.put(canon, entry.getValue());
            }

            final Map envAliases =
                getEnvAliases(envPorts, portDir, ci.getLocalNodes());
            final Map cellAliases = getCellAliases(ports, portDir, cadencizer.convert(cell).getLocalNodes());

            for (Iterator i = envPorts.iterator(); i.hasNext(); ) {
                final HierName port = (HierName) i.next();
                final HierName canon = (HierName) envam.getCanonicalKey(port);
                assert canon != null :
                    "Cannot get canonical name for: " + port;

                final Integer dir = (Integer) canonPortDir.get(canon);
                assert dir != null :
                    "Cannot get port direction for: " + canon;

                final Collection aliases =
                    getEnvCellAliases(envAliases, cellAliases, port);

                for (Iterator j = aliases.iterator(); j.hasNext(); ) {
                    final HierName da = (HierName) j.next();
                    if (dir.intValue() == PortDefinition.OUT) {
                        processSplit(HierName.append(digitalInst, da),
                                     HierName.append(analogInst, da),
                                     HierName.append(envInst, canon));
                    } else if (dir.intValue() == PortDefinition.IN) {
                        processMerge(merge,
                                     HierName.append(digitalInst, da),
                                     HierName.append(analogInst, da),
                                     HierName.append(envInst, canon));
                    } else if (dir.intValue() == PortDefinition.INOUT) {
                        throw new RuntimeException("Do not know how to handle bi-directional port: " + port);
                    } else {
                        throw new AssertionError("Invalid port direction: " + dir);
                    }
                }
            }
        }

        private Collection getEnvCellAliases(final Map envAliases,
                                             final Map cellAliases,
                                             final HierName envPort) {
            final Set cellPorts = new HashSet();
            final List castPorts = (List) envAliases.get(envPort);
            for (Iterator j = castPorts.iterator(); j.hasNext(); ) {
                final String s = (String) j.next();
                cellPorts.addAll((List) cellAliases.get(s));
            }
            return cellPorts;
        }

        private Map getCellAliases(final Collection actualPorts,
                                   final Map castPorts,
                                   final AliasedSet nodes) {
            final Map result = new HashMap();
            final Set portSet = new HashSet(actualPorts);
            for (Iterator i = castPorts.entrySet().iterator(); i.hasNext(); )
                {
                    final Map.Entry entry = (Map.Entry) i.next();
                    final HierName port = makeHierName((String) entry.getKey(), '.');
                    final HierName canon = (HierName) nodes.getCanonicalKey(port);
                    final List aliases = new ArrayList();
                    for (Iterator j = nodes.getAliases(canon); j.hasNext(); ) {
                        final HierName portAlias = (HierName) j.next();
                        if (portSet.contains(portAlias)) {
                            aliases.add(portAlias);
                        }
                    }
                    result.put(entry.getKey(), aliases);
                }
            return result;
        }

        private Map getEnvAliases(final Collection actualPorts,
                                  final Map castPorts,
                                  final AliasedSet nodes) {
            final Map result = new HashMap();
            for (Iterator i = actualPorts.iterator(); i.hasNext(); ) {
                final HierName port = (HierName) i.next();
                final HierName canon = (HierName) nodes.getCanonicalKey(port);
                final List aliases = new ArrayList();
                for (Iterator j = nodes.getAliases(canon); j.hasNext(); ) {
                    final String portAlias =
                        ((HierName) j.next()).getAsString('.');

                    if (castPorts.containsKey(portAlias))
                        aliases.add(portAlias);
                }
                result.put(port, aliases);
            }
            return result;
        }

        private void processMerge(final CellInterface merge,
                                  final HierName digital,
                                  final HierName analog,
                                  final HierName env) {
            final Collection ports = 
                NetlistAdapter.getParameterList(merge, cadencizer);
            pw.print("\"" + merge.getFullyQualifiedType() + "\" ");
            pw.print("(");
            for (Iterator i = ports.iterator(); i.hasNext(); ) {
                final String s = ((HierName) i.next()).getAsString('.');
                if (s.equals("digital")) {
                    printName(digital);
                } else if (s.equals("analog")) {
                    printName(analog);
                } else if (s.equals("environ")) {
                    printName(env);
                } else if (s.equals("Vdd")) {
                    pw.print("Vdd");
                } else if (s.equals("GND")) {
                    pw.print("GND");
                } else if (s.equals("_RESET")) {
                    pw.print("_RESET");
                } else {
                    throw new AssertionError("Unknown port " + s + " in merge cell " + merge.getFullyQualifiedType());
                }
                if (i.hasNext()) pw.print(", ");
            }
            pw.println(") ()");
        }

        private void processSplit(final HierName digital,
                                  final HierName analog,
                                  final HierName env) {
            pw.print("wire(");
            printName(env);
            pw.print(", ");
            printName(digital);
            pw.print(", ");
            printName(analog);
            pw.println(")");
        }

        private void processTopLevel(final Collection ports,
                                     final AliasedMap am,
                                     final HierName inst,
                                     final HierName cell) {
            printName(cell);
            pw.print(" ");
            printName(inst);
            pw.print(" (");
            for (Iterator i = ports.iterator(); i.hasNext(); ) {
                final HierName port = (HierName) i.next();
                final HierName canon = (HierName) am.getCanonicalKey(port);
                assert canon != null :
                    "Cannot get canonical name for: " + port;
                printName(HierName.append(inst, canon));
                if (i.hasNext()) pw.print(", ");
            }
            pw.println(") ()");
        }

        private void processPortConnections(final CellInterface cell) {
            final AliasedMap portNodes =
                cadencizer.convert(cell).getPortNodes();
            for (final Iterator iCanonNode = portNodes.getCanonicalKeys();
                 iCanonNode.hasNext(); ) {
                final HierName canonNode = (HierName) iCanonNode.next();
                boolean firstP = true;
                for (final Iterator iConnNode =
                         portNodes.getAliases(canonNode);
                     iConnNode.hasNext(); ) {
                    final HierName connNode = (HierName) iConnNode.next();
                    // don't print self-connections
                    if (connNode != canonNode) {
                        if (firstP) {
                            firstP = false;
                            pw.print("wire(");
                            printName(canonNode);
                        }

                        pw.print(',');
                        printName(connNode);
                    }
                }
                if (!firstP)
                    pw.println(')');
            }
        }
    }

    private abstract static class FanFormatter implements CellFormatterInterface {
        private final PrintWriter pw;
        private final CastFileParser cfp;
        private final Cadencize cad;
        private final Cadencize routedCad;

        /**
         * If non-null, specify the set of nodes to determine fanout for.
         **/
        private final Set<String> nodes;
        
        /**
         * Whether to ignore feedback NetPaths when determining fanout.
         **/
        private final boolean ignoreFeedback;

        /**
         * Output information for "routed" local nodes?
         **/
        private final boolean routed;

        // fqcn -> (fanin -> fanouts)
        private final Map<String,Map<HierName,Set<HierName>>> cache =
            new HashMap<String,Map<HierName,Set<HierName>>>();

        private CadenceInfo topInfo = null;

        protected interface Result {}

        public FanFormatter(final PrintWriter pw, 
                            final CastFileParser cfp,
                            final Cadencize cad,
                            final Cadencize routedCad,
                            final Set<String> nodes,
                            final boolean ignoreFeedback,
                            final boolean routed) {
            this.pw = pw;
            this.cfp = cfp;
            this.cad = cad;
            this.routedCad = routedCad;
            this.nodes = nodes;
            this.ignoreFeedback = ignoreFeedback;
            this.routed = routed;
        }
        
        public CellInterface prepCell(CellInterface cell,
                                      CellInterface routed) {
            routedCad.convert(routed);
            // handle routed hierarchy independently, because the flattening of
            // unrouted cells with netlist blocks does not work properly, and
            // even if it did, NetGraph does not handle netlists with multiple
            // Vdds
            return cell;
        }

        protected abstract Result resolveNames(final HierName path,
                                               final HierName canon,
                                               final HierName related,
                                               final Cadencize cad,
                                               final CellInterface topCell,
                                               final CadenceInfo topInfo);

        protected abstract void getFan(final NetGraph ng,
                                       final AliasedSet localNodes,
                                       final Map<HierName,Set<HierName>> result,
                                       final boolean ignoreFeedback);

        private Map<HierName,Set<HierName>> getFan(final CellInterface cell) {
            Map<HierName,Set<HierName>> result =
                cache.get(cell.getFullyQualifiedType());
            if (result == null) {
                final CadenceInfo ci = cad.convert(cell);
                final ArrayList problems = new ArrayList();
                final ExclusiveNodeSets exclusives = new ExclusiveNodeSets();
                exclusives.merge(ci.getPortExclusiveNodeSets());
                exclusives.merge(ci.getLocalExclusiveNodeSets());
                final AliasedSet localNodes = ci.getLocalNodes();
                final NetGraph ng = new NetGraph(
                        localNodes,
                        exclusives,
                        problems,
                        HierName.makeHierName("Vdd"),
                        HierName.makeHierName("GND"),
                        Collections.emptySet());
                try {
                    ng.addCellInterface(cell, new NetGraph[0], cfp, cad);
                } catch (com.avlsi.prs.UnimplementableProductionRuleException e)
                {
                    throw new RuntimeException("Can't happen");
                }
                ng.prepareForLvs();
                assert problems.isEmpty();

                result = new HashMap<HierName,Set<HierName>>();
                getFan(ng, localNodes, result, ignoreFeedback);
                cache.put(cell.getFullyQualifiedType(), result);
            }
            return result;
        }

        protected Triplet<HierName,CellInterface,HierName>
        findRouted(CellInterface cell, HierName remain) {
            HierName instRouted = null;
            HierName instSinceRouted = null;
            CellInterface routed = cell;
            Pair<HierName,HierName> parts;

            do {
                parts = CellUtils.getFirstInstance(cell, remain, true);
                final HierName inst = parts.getFirst();
                if (inst != null) {
                    instSinceRouted = HierName.append(instSinceRouted, inst);

                    final CellInterface subcell = cell.getSubcell(inst);
                    if (!this.routed || CellUtils.isRouted(subcell)) {
                        instRouted =
                            HierName.append(instRouted, instSinceRouted);
                        instSinceRouted = null;
                        routed = subcell;
                    }
                    remain = parts.getSecond();
                    cell = subcell;
                }
            } while (parts.getFirst() != null && remain != null);

            return new Triplet<HierName,CellInterface,HierName>(
                    instRouted,
                    routed,
                    remain == null ? instSinceRouted
                                   : HierName.append(instSinceRouted, remain));
        }
        
        protected HierName getCanonical(final CellInterface routed,
                                        final HierName local) {
            final CadenceInfo cinfo =
                routedCad.getExistingCadenceInfo(routed.getFullyQualifiedType());
            final Triplet<HierName,CadenceInfo,HierName> t =
                CellUtils.localize(local, cinfo, false);
            //Handle special case. t will be null when the local name is a staticizer node.
            if (t ==  null){
                return local;
            }
            return HierName.append(t.getFirst(), t.getThird());
        }

        /**
         * Descends the hierarchy to find all fanouts of a given fanin node.
         **/
        private void descend(
                final CellInterface cell,
                final CadenceInfo ci,
                final CellInterface topCell,
                final CadenceInfo topInfo,
                final HierName name,
                final HierName path,
                final Set<Result> result) {
            HierName canon =
                (HierName) ci.getLocalNodes().getCanonicalKey(name);
            //when node name is staticizer inverter, it does not exist in CandenceInfo localnode
            if (canon == null) canon = name;
            if (cell.containsNetlist()) {
                final Set<HierName> fanouts = getFan(cell).get(canon);
                if (fanouts != null) {
                    for (HierName fanout : fanouts) {
                        result.add(resolveNames(path, canon, fanout, cad,
                                                topCell, topInfo));
                    }
                }
            } else {
                final AliasedSet nodes = ci.getLocalNodes();
                final Set<HierName> seen = new HashSet<HierName>();
                for (Iterator i = nodes.getAliases(canon); i.hasNext(); ) {
                    final HierName node = (HierName) i.next();
                    final Pair<HierName,HierName> p =
                        CellUtils.getFirstInstance(cell, node, true);
                    if (p.getFirst() != null) {
                        final HierName inst = p.getFirst();
                        final CadenceInfo subci = ci.getSubcell(inst);
                        if (subci != null) {
                            final HierName subnode = p.getSecond();
                            final Boolean realPort =
                                ((Boolean) subci.getPortNodes()
                                                .getValue(subnode));
                            if (realPort.booleanValue()) {
                                final HierName canonPort =
                                    (HierName) subci.getPortNodes()
                                                    .getCanonicalKey(subnode);
                                if (seen.add(HierName.append(p.getFirst(),
                                                             canonPort))) {
                                    descend(cell.getSubcell(inst),
                                            ci.getSubcell(inst),
                                            topCell, topInfo,
                                            subnode,
                                            HierName.append(path, inst),
                                            result);
                                }
                            }
                        }
                    }
                }
            }
        }

        public void outputCell(final CellInterface cell,
                               final CellInterface envCell)
            throws CellFormatterException, IOException {

            if(envCell != null ) {
                return;
            }

            outputCell(cell, null, cell);

            // checkError also flushes
            if (pw.checkError())
                throw new IOException();
        }

        private void outputCell(final CellInterface cell,
                                final HierName prefix,
                                final CellInterface top) {
            final CadenceInfo info = cad.convert(cell);
            if (topInfo == null) topInfo = info;

            final AliasedSet locals = info.getLocalNodes();
            final AliasedMap ports = info.getPortNodes();
            final Iterator i;
            final Iterator inv_i;
            if (nodes == null) {
                i = new FilteringIterator(locals.getCanonicalKeys(),
                        new UnaryPredicate() {
                            public boolean evaluate(final Object o) {
                                return ports.getCanonicalKey(o) == null;
                            }
                        });
                //Add staticizer inverter nodes
                final Collection staticizerNodes = new NetGraph(cell, cad, cfp).getStaticizerNodes();
                inv_i=staticizerNodes.iterator();
            } else {
                final Set<HierName> hnodes = new HashSet<HierName>();
                for (String node : nodes) {
                    hnodes.add(makeHierName(node));
                }
                i = hnodes.iterator();
                inv_i = null;
            }
            //TODO: make inverter node as fanin node
            //while (i.hasNext() || inv_i.hasNext()) {
            while (i.hasNext()) {
                final HierName canon;
                boolean isStaticizerInv = false;
                if (i.hasNext()){
                    canon = (HierName) i.next();
                } else {
                    isStaticizerInv = true;
                    //After parse all local nodes, start to check staticizer inverter node
                    final NetGraph.NetNode node = (NetGraph.NetNode) inv_i.next();
                    canon = node.getName();
                }
                final Set<Result> result = new TreeSet<Result>();
                descend(cell, info, top, topInfo, canon, prefix, result);
                if (!result.isEmpty()) {
                    HierName canon_ = getCanonical(top, HierName.append(prefix, canon));
                    if (isStaticizerInv) {
                        //fix inverter digital name such as Lv[0]_inverse -> Lv_inverse[0]
                        pw.print(CellUtils.getCastNodeName(canon_));
                    } else {
                        pw.print(canon_);
                    }
                    
                    for (Result r : result) {
                        pw.print(" " + r);
                    }
                    pw.println();
                }
            }

            if (routed) {
                for (Iterator j = new SortingIterator(cell.getSubcellPairs());
                        j.hasNext(); ) {
                    final Pair<HierName,CellInterface> p =
                        (Pair<HierName,CellInterface>) j.next();
                    final CellInterface subcell = p.getSecond();
                    if (info.getSubcell(p.getFirst()) != null &&
                        !CellUtils.isRouted(subcell)) {
                        outputCell(subcell,
                                   HierName.append(prefix, p.getFirst()),
                                   top);
                    }
                }
            }
        }
    }

    private static class FaninFormatter extends FanFormatter {
        private static class FaninResult implements Result,
                                                    Comparable<FaninResult> {
            private final String fqcn;
            private final HierName relFanIn;
            private final HierName absFanIn;

            FaninResult(final String fqcn,final HierName relFanIn,
                        final HierName absFanIn) {
                this.fqcn = fqcn;
                this.relFanIn = relFanIn;
                this.absFanIn = absFanIn;
            }

            public String toString() {
                return fqcn + "/" + relFanIn + "/" + absFanIn;
            }

            public int compareTo(FaninResult o) {
                return ObjectUtils.compare(fqcn, o.fqcn,
                                           relFanIn, o.relFanIn,
                                           absFanIn, o.absFanIn);
            }
        }

        public FaninFormatter(final PrintWriter pw, 
                              final CastFileParser cfp,
                              final Cadencize cad,
                              final Cadencize routedCad,
                              final Set<String> nodes,
                              final boolean ignoreFeedback,
                              final boolean routed) {
            super(pw, cfp, cad, routedCad, nodes, ignoreFeedback, routed);
        }

        protected Result resolveNames(final HierName path,
                                      final HierName canon,
                                      final HierName related,
                                      final Cadencize cad,
                                      final CellInterface topCell,
                                      final CadenceInfo topInfo) {
            final Triplet<HierName,CadenceInfo,HierName> rel =
                CellUtils.localize(HierName.append(path, related),
                                   topInfo, false);
            final CellInterface routedCell;
            final HierName relInst;
            final HierName relFanIn;
            if (rel.getFirst() == null) {
                routedCell = topCell;
                relInst = null;
                relFanIn = rel.getThird();
            } else {
                final Triplet<HierName,CellInterface,HierName> t =
                    findRouted(topCell, rel.getFirst());
                routedCell = t.getSecond();
                relInst = t.getFirst();
                relFanIn = t.getThird() == null
                    ? rel.getThird()
                    : HierName.append(t.getThird(), rel.getThird());
            }

            final HierName canonFanin = getCanonical(routedCell, relFanIn);

            final HierName absFanIn = HierName.append(relInst, canonFanin);

            return new FaninResult(routedCell.getFullyQualifiedType(),
                                   canonFanin, absFanIn);
        }

        protected void getFan(final NetGraph ng,
                              final AliasedSet localNodes,
                              final Map<HierName,Set<HierName>> result,
                              final boolean ignoreFeedback) {
            for (Iterator n = ng.getNodes().iterator(); n.hasNext(); ) {
                final NetGraph.NetNode node = (NetGraph.NetNode) n.next();
                for (Iterator i = node.getPaths().iterator();
                        i.hasNext(); ) {
                    final NetGraph.NetPath p = (NetGraph.NetPath) i.next();
                    if (ignoreFeedback && p.isFeedBack()) continue;

                    for (NetGraph.NetNode out :
                            Arrays.asList(p.getStartNode(), p.getEndNode())) {
                        // only include names that are valid CAST names
                        if (!out.isRail() && localNodes.contains(out.name)) {
                            Set<HierName> fanins = result.get(out);
                            if (fanins == null) {
                                fanins = new HashSet<HierName>();
                                result.put(out.name, fanins);
                            }
                            for (Iterator j = p.getGateNodes().iterator();
                                 j.hasNext(); ) {
                                final NetGraph.NetNode g =
                                    (NetGraph.NetNode) j.next();
                                fanins.add(g.name);
                            }
                        }
                    }
                }
            }
        }

    }

    private static class FanoutFormatter extends FanFormatter {
        private static class FanoutResult implements Result,
                                                     Comparable<FanoutResult> {
            private final String fqcn;
            private final String path;
            private final HierName fanin;
            private final HierName fanout;

            FanoutResult(final String path, final String fqcn,
                         final HierName fanin, final HierName fanout) {
                this.path = path;
                this.fqcn = fqcn;
                this.fanin = fanin;
                this.fanout = fanout;
            }

            public String toString() {
                return path + "/" + fqcn + "/" + fanin + "/" + fanout;
            }

            public int compareTo(FanoutResult o) {
                return ObjectUtils.compare(fqcn, o.fqcn,
                                           path, o.path,
                                           fanin, o.fanin,
                                           fanout, o.fanout);
            }
        }

        public FanoutFormatter(final PrintWriter pw, 
                              final CastFileParser cfp,
                              final Cadencize cad,
                              final Cadencize routedCad,
                              final Set<String> nodes,
                              final boolean ignoreFeedback,
                              final boolean routed) {
            super(pw, cfp, cad, routedCad, nodes, ignoreFeedback, routed);
        }

        /**
         * Returns a map from fanin to set of fanouts in a cell containing a
         * netlist block.  Iterate over all NetPath in the NetGraph created
         * from the netlist block; nodes that gate NetEdges in a NetPath are
         * considered fanins of the ends of the NetPath (unless the end is a
         * power rail).  Results are cached per fqcn.
         **/
        protected void getFan(final NetGraph ng,
                              final AliasedSet localNodes,
                              final Map<HierName,Set<HierName>> result,
                              final boolean ignoreFeedback) {
            for (Iterator n = ng.getNodes().iterator(); n.hasNext(); ) {
                final NetGraph.NetNode node = (NetGraph.NetNode) n.next();
                for (Iterator i = node.getPaths().iterator();
                        i.hasNext(); ) {
                    final NetGraph.NetPath p = (NetGraph.NetPath) i.next();
                    for (Iterator j = p.getGateNodes().iterator();
                         j.hasNext(); ) {
                        final NetGraph.NetNode g =
                            (NetGraph.NetNode) j.next();
                        for (NetGraph.NetNode out :
                                Arrays.asList(p.getStartNode(),
                                              p.getEndNode())) {
                            if (ignoreFeedback && p.isFeedBack()) {
                                //Only include Staticizer in feedback path
                                if (!g.isStaticizerInverter() && !g.gatesSmallInverter()) continue;
                            }
                            if (!out.isRail()){
                                Set<HierName> fanouts = result.get(g.name);
                                if (fanouts == null) {
                                    fanouts = new HashSet<HierName>();
                                    result.put(g.name, fanouts);
                                }
                                fanouts.add(out.name);
                            }
                        }
                    }
                }
            }
        }

        protected Result resolveNames(final HierName path,
                                      final HierName canon,
                                      final HierName related,
                                      final Cadencize cad,
                                      final CellInterface topCell,
                                      final CadenceInfo topInfo) {
            Triplet<HierName,CadenceInfo,HierName> sink =
                CellUtils.localize(HierName.append(path, related),
                                   topInfo, false);
            final CellInterface routedCell;
            HierName relFanIn = HierName.append(path, canon);
            final HierName relFanOut;
            String instPath = "";
            boolean fanoutNotPrsLocalNode=false;
            if (sink == null){
               //This should be the stacizer inverter node case.
               routedCell = topCell;
               relFanOut = related;               
            }            
            else if (sink.getFirst() == null) {
                routedCell = topCell;                
                relFanOut = sink.getThird();                
            } else {
                final Triplet<HierName,CellInterface,HierName> t =
                    findRouted(topCell, sink.getFirst());
                routedCell = t.getSecond();
                relFanOut = t.getThird() == null
                    ? sink.getThird()
                    : HierName.append(t.getThird(), sink.getThird());
                if (t.getFirst() != null) {
                    relFanIn = relFanIn.tail(t.getFirst().getNumComponents());
                    instPath = t.getFirst().toString();
                }
            }
            return new FanoutResult(instPath,
                                    routedCell.getFullyQualifiedType(),
                                    CellUtils.getCastNodeName(getCanonical(routedCell, relFanIn)),
                                    CellUtils.getCastNodeName(getCanonical(routedCell, relFanOut)));
        }
    }

    /**
     * Make multiple formatters into one.  The formatters must return the same
     * cell from <code>prepCell</code>.
     **/
    private static class AggregateFormatter implements CellFormatterInterface {
        private final CellFormatterInterface[] formatters;
        public AggregateFormatter(final CellFormatterInterface a,
                                  final CellFormatterInterface b) {
            this(new CellFormatterInterface[] { a, b });
        }
        public AggregateFormatter(final CellFormatterInterface[] formatters) {
            assert formatters != null && formatters.length > 1 &&
                formatters[0] != null;
            this.formatters = formatters;
        }
        public void outputCell(CellInterface cell, CellInterface envCell)
            throws CellFormatterException, IOException {
            for (int i = 0; i < formatters.length; ++i) {
                formatters[i].outputCell(cell, envCell);
            }
        }
        public CellInterface prepCell(CellInterface cell,
                                      CellInterface routed) {
            for (int i = 1; i < formatters.length; ++i) {
                formatters[i].prepCell(cell, routed);
            }
            return formatters[0].prepCell(cell, routed);
        }
    }
}
