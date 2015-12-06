/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.prs2verilog;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;

import com.avlsi.cast.CastFileParser;
import com.avlsi.cast.CastSyntaxException;
import com.avlsi.cast.CastSemanticException;
import com.avlsi.cast.impl.CastParserEnvironment;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.directive.impl.DirectiveTable;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cast2.util.StandardParsingOption;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;
import com.avlsi.cell.NoSuchEnvironmentException;
import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.BlockIterator;
import com.avlsi.fast.CastDesign;
import com.avlsi.fast.CellType;
import com.avlsi.fast.CellTypeProcessor;
import com.avlsi.fast.ConnectionInfo;
import com.avlsi.fast.VerilogBlock;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.CDLRenameException;
import com.avlsi.file.cdl.util.rename.CadenceNameInterface;
import com.avlsi.file.cdl.util.rename.CadenceReverseNameInterface;
import com.avlsi.file.cdl.util.rename.GDS2NameInterface;
import com.avlsi.file.cdl.util.rename.IdentityNameInterface;
import com.avlsi.file.cdl.util.rename.Rename;
import com.avlsi.io.FileSearchPath;
import com.avlsi.io.SearchPath;
import com.avlsi.io.SearchPathFile;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cosim.CoSimParameters;
import com.avlsi.tools.cosim.CoSimHelper;
import com.avlsi.tools.cosim.spec.*;
import com.avlsi.tools.dsim.ExceptionPrettyPrinter;
import com.avlsi.tools.dsim.NoBehaviorFoundException;
import com.avlsi.tools.jauto.GlobalNet; 
import com.avlsi.tools.jauto.JautoMessageCenter;
import com.avlsi.tools.jauto.TechnologyData; 
import com.avlsi.tools.prs2verilog.ConverterInterface;
import com.avlsi.tools.prs2verilog.GateConverter;
import com.avlsi.tools.prs2verilog.NetgraphConverter;
import com.avlsi.tools.prs2verilog.NetlistConverter;
import com.avlsi.tools.prs2verilog.TriConverter;
import com.avlsi.tools.prs2verilog.verilog.SimpleRenamingVerilogFactory;
import com.avlsi.tools.prs2verilog.verilog.VerilogObject;
import com.avlsi.tools.prs2verilog.verilog.VerilogEmitter;
import com.avlsi.tools.prs2verilog.verilog.VerilogFactoryInterface;
import com.avlsi.tools.prs2verilog.verilog.VerilogFactoryImpl;
import com.avlsi.tools.prs2verilog.verilog.VerilogUtil;
import com.avlsi.tools.prs2verilog.verilog.VerilogVisitor;
import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgFormatException;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.CommandLineArgsUtil;
import com.avlsi.util.cmdlineargs.InvalidCommandLineArgException;
import com.avlsi.util.cmdlineargs.MissingCommandLineArgException;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.MultiMap;
import com.avlsi.util.container.Pair;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.functions.BinaryAction;
import com.avlsi.util.functions.UnaryPredicate;
import com.avlsi.util.text.StringUtil;

public class Prs2Verilog {
    /**
     * This class should not be instantiated.
     **/
    private Prs2Verilog() { }

    /**
     * Choose the named Verilog block to use.  Return <code>null</code> if the
     * verilog block should not be used.
     **/
    public interface VerilogChooser {
        VerilogBlock.NamedBlock choose(final CellInterface cell,
                                       final VerilogBlock vb);
    }

    public static class NullChooser implements VerilogChooser {
        public VerilogBlock.NamedBlock choose(final CellInterface cell,
                                              final VerilogBlock vb) {
            return null;
        }
    }

    public static class SimpleChooser implements VerilogChooser {
        private final String name;
        public SimpleChooser(final String name) {
            this.name = name;
        }
        public VerilogBlock.NamedBlock choose(final CellInterface cell,
                                              final VerilogBlock vb) {
            return vb.getNamedBlock(name);
        }
    }

    public static class DefaultChooser implements VerilogChooser {
        private final String def;
        private final Map map;

        public DefaultChooser(final String def, final Map map) {
            this.def = def;
            this.map = map;
        }

        public VerilogBlock.NamedBlock choose(final CellInterface cell,
                                              final VerilogBlock vb) {
            final String c = (String) map.get(cell.getFullyQualifiedType());
            VerilogBlock.NamedBlock result;
            if (c == null) {
                result = vb.getNamedBlock(def);
                if (result == null) {
                    final Iterator i = vb.getNamesIterator();
                    if (i.hasNext()) {
                        final String first = (String) i.next();
                        if (!i.hasNext()) {
                            result = vb.getNamedBlock(first);
                        } else {
                            throw new RuntimeException("Default named verilog block " + def + " not found in " + cell.getFullyQualifiedType() + ", which also has multiple named verilog blocks.");
                        }
                    }
                }
            } else {
                result = vb.getNamedBlock(c);
                if (result == null) {
                    throw new RuntimeException("Cannot find named verilog block " + c + " in cell " + cell.getFullyQualifiedType());
                }
            }
            return result;
        }
    }

    public static class CosimChooser implements VerilogChooser {
        private final MultiMap typeBehaviors;
        public CosimChooser(final MultiMap typeBehaviors) {
            this.typeBehaviors = typeBehaviors;
        }
        public VerilogBlock.NamedBlock choose(final CellInterface cell,
                                              final VerilogBlock vb) {
            final Collection behs =
                (Collection) typeBehaviors.get(cell.getFullyQualifiedType());
            assert behs.size() == 1;
            final Mode m = (Mode) behs.iterator().next();
            if (m instanceof Mode.VerilogMode) {
                final String level = ((Mode.VerilogMode) m).getLevel();
                return vb.getNamedBlock(level);
            }
            return null;
        }
    }

    public interface VisitorFactory {
        VerilogVisitor getVisitor(final String cellName);
        void doneVisitor(final VerilogVisitor visitor);
    }

    private static String getCopyright() {
        return "/* Copyright " + Calendar.getInstance().get(Calendar.YEAR) + " Intel Corporation.  All rights reserved.\n * Automatically generated.  Modify at your own risk.\n */\n";
    }

    private static void writeHeader(final Writer w, final String cell)
        throws IOException {
        w.write("`ifndef " + cell + "\n");
        w.write("`define " + cell + " 1\n");
    }

    private static void writeFooter(final Writer w)
        throws IOException {
        w.write("`endif\n");
    }

    public static class SingleWriterVisitor implements VisitorFactory {
        private final Writer w;
        private final VerilogEmitter emitter;
        private final boolean ifdef;
        public SingleWriterVisitor(final Writer writer) {
            this(writer, false);
        }
        public SingleWriterVisitor(final Writer writer, final boolean ifdef) {
            this.w = writer;
            emitter = new VerilogEmitter(writer);
            this.ifdef = ifdef;
        }
        public VerilogVisitor getVisitor( final String cellName ) {
            if (ifdef) {
                try {
                    final String cond = VerilogUtil.escapeIfNeeded(cellName);
                    writeHeader(w, cond);
                } catch (IOException e) {
                    throw new RuntimeException("Cannot write to file", e);
                }
            }
            return emitter;
        }
        public void doneVisitor(final VerilogVisitor visitor) {
            try {
                if (ifdef) writeFooter(w);
                emitter.flush();
                w.flush();
            } catch (IOException e) {
                throw new RuntimeException("Cannot flush output", e);
            }
        }
    }

    public static class MultipleFileVisitor implements VisitorFactory {
        private final File dir, sdir;
        private final Map visitors;
        private final Collection files;
        private final boolean relativePath;
        private final String copyright;
        private final CDLNameInterface namer;
        private final boolean ifdef;
        public MultipleFileVisitor(final String out, final String special,
                                   final Collection files,
                                   final boolean relativePath) {
            this(out, special, files, relativePath, false);
        }
        public MultipleFileVisitor(final String out, final String special,
                                   final Collection files,
                                   final boolean relativePath,
                                   final boolean ifdef) {
            this(out, special, files, relativePath,
                 new TruncatingRenamer(new IdentityNameInterface()), ifdef);
        }
        public MultipleFileVisitor(final String out, final String special,
                                   final Collection files,
                                   final boolean relativePath,
                                   final CDLNameInterface namer,
                                   final boolean ifdef) {
            this(new File(out), special, files, relativePath, namer, ifdef);
        }
        public MultipleFileVisitor(final File dir, final String special,
                                   final Collection files,
                                   final boolean relativePath,
                                   final CDLNameInterface namer,
                                   final boolean ifdef) {
            this.dir = dir;
            if (!dir.exists() && !dir.mkdir()) {
                throw new RuntimeException("Cannot create directory: " + dir);
            }
            sdir = special == null ? null : new File(special);
            visitors = new HashMap();
            this.files = files;
            this.relativePath = relativePath;
            this.copyright = getCopyright();
            this.namer = namer;
            this.ifdef = ifdef;
        }
        public VerilogVisitor getVisitor(final String cellName) {
            final String name;
            try {
                name = namer.renameCell(cellName) + ".v";
            } catch (CDLRenameException e) {
                throw new RuntimeException("Cannot rename " + cellName, e);
            }
            final File sfile = new File(sdir, name);
            if (sdir != null && sfile.exists()) {
                try {
                    files.add(sfile.getCanonicalPath());
                } catch (IOException e) {
                    throw new RuntimeException("Cannot get canonical path for " + sfile, e);
                }
                return null;
            }

            final File out = new File(dir, name);
            try {
                files.add(relativePath ? name : out.getCanonicalPath());
            } catch (IOException e) {
                throw new RuntimeException("Cannot get canonical path for " + out, e);
            }

            final Writer w;
            try {
                w = new BufferedWriter(new FileWriter(out));
                w.write(copyright);
                if (ifdef) {
                    final String cond = VerilogUtil.escapeIfNeeded(cellName);
                    writeHeader(w, cond);
                }
            } catch (IOException e) {
                throw new RuntimeException("Cannot write to file: " + out, e);
            }

            final VerilogVisitor visitor = new VerilogEmitter(w);
            visitors.put(visitor, new Pair(w, out));

            return visitor;
        }
        public void doneVisitor(final VerilogVisitor visitor) {
            final Pair p = (Pair) visitors.get(visitor);
            if (p != null) {
                final Writer w = (Writer) p.getFirst();
                try {
                    if (ifdef) writeFooter(w);
                    ((VerilogEmitter) visitor).flush();
                    w.close();
                } catch (IOException e) {
                    final File f = (File) p.getSecond();
                    throw new RuntimeException("Cannot close file: " + f, e);
                }
                visitors.remove(visitor);
            }
        }
    }

    private static class TruncatingRenamer implements CDLNameInterface {
        private final CDLNameInterface inner;
        public TruncatingRenamer(final CDLNameInterface inner) {
            this.inner = inner;
        }
        private String truncate(final String s) {
            return CellUtils.hashMetaParameters(s.replace('$', '_'));
        }
        public String renameCell(final String oldCellName)
            throws CDLRenameException {
            return truncate(inner.renameCell(oldCellName));
        }
        public String renameNode(final String oldNodeName)
            throws CDLRenameException {
            return inner.renameNode(oldNodeName);
        }
        public String renameDevice(final String oldDeviceName)
            throws CDLRenameException {
            return inner.renameDevice(oldDeviceName);
        }
        public String renameSubCellInstance(final String oldInstanceName)
            throws CDLRenameException {
            return inner.renameSubCellInstance(oldInstanceName);
        }
        public String renameTransistorModel(final String oldTransistorModel)
            throws CDLRenameException {
            return inner.renameTransistorModel(oldTransistorModel);
        }
    }

    private static class KeywordRenamer implements CDLNameInterface {
        private final CDLNameInterface inner;
        public KeywordRenamer(final CDLNameInterface inner) {
            this.inner = inner;
        }
        private String dekeyword(final String s) {
            return VerilogUtil.isKeyword(s) ? s + "$keyword" : s;
        }
        public String renameCell(final String oldCellName)
            throws CDLRenameException {
            return dekeyword(inner.renameCell(oldCellName));
        }
        public String renameNode(final String oldNodeName)
            throws CDLRenameException {
            return dekeyword(inner.renameNode(oldNodeName));
        }
        public String renameDevice(final String oldDeviceName)
            throws CDLRenameException {
            return dekeyword(inner.renameDevice(oldDeviceName));
        }
        public String renameSubCellInstance(final String oldInstanceName)
            throws CDLRenameException {
            return dekeyword(inner.renameSubCellInstance(oldInstanceName));
        }
        public String renameTransistorModel(final String oldTransistorModel)
            throws CDLRenameException {
            return dekeyword(inner.renameTransistorModel(oldTransistorModel));
        }
    }

    private static class OliverRenamer implements CDLNameInterface {
        private final CDLNameInterface inner;
        private String bracket(final String s) {
            return s.replaceAll("\\[", "_L_").replaceAll("\\]", "_R_");
        }
        public OliverRenamer() {
            this.inner = new CadenceNameInterface();
        }
        public String renameCell(final String oldCellName)
            throws CDLRenameException {
            return inner.renameCell(oldCellName);
        }
        public String renameNode(final String oldNodeName)
            throws CDLRenameException {
            return bracket(inner.renameNode(oldNodeName));
        }
        public String renameDevice(final String oldDeviceName)
            throws CDLRenameException {
            return inner.renameDevice(oldDeviceName);
        }
        public String renameSubCellInstance(final String oldInstanceName)
            throws CDLRenameException {
            return bracket(inner.renameSubCellInstance(oldInstanceName));
        }
        public String renameTransistorModel(final String oldTransistorModel)
            throws CDLRenameException {
            return inner.renameTransistorModel(oldTransistorModel);
        }
    }

    private static final Map portInfo = new HashMap();
    private static final HierName Vdd = HierName.makeHierName("Vdd");
    private static final HierName GND = HierName.makeHierName("GND");
    private static final HierName _RESET = HierName.makeHierName("_RESET");

    private static CastDesign loadDesign(final CellInterface cell,
                                        final Cadencize cad,
                                        final CommandLineArgs theArgs) {

        // tdata should be final, but jikes doesn't know (and isn't
        // required to know) that usage() never returns.
        // tdata only needed when trying to minimize-tri-regs
        if (theArgs.argExists("minimize-tri-regs")) {
            TechnologyData tdata = null;
            try {
                tdata = new TechnologyData(theArgs);
            } catch (CommandLineArgFormatException e) {
                System.err.println(e.toString());
                usage();
            } catch (InvalidCommandLineArgException e) {
                System.err.println(e.toString());
                usage();
            } catch (MissingCommandLineArgException e) {
                System.err.println(e.toString());
                usage();
            }
            /* 100 is any non-zero TAU */
            return new CastDesign(cell, Vdd, GND, _RESET, 1000, tdata, cad,
                                  null,
                                  !theArgs.argExists("ignore-asta-extra-delay"));
        } else {
            /* 100 is any non-zero TAU */ 
            return new CastDesign(cell, Vdd, GND, _RESET, 100, null, cad,
                                  null,
                                  !theArgs.argExists("ignore-asta-extra-delay"));
        }

    }

    private static CellType loadCell(final CastDesign design,
                                     final CellInterface ci,
                                     final CastFileParser cfp,
                                     final CommandLineArgs theArgs) {
        return loadCell(design, ci, cfp,
                        theArgs.argExists("minimize-tri-regs"),
                        theArgs.getArgValue("gates", null));
    }

    private static CellType loadCell(final CastDesign design,
                                     final CellInterface ci,
                                     final CastFileParser cfp,
                                     final boolean minimizeTriRegs,
                                     final String gates) {
        final CellType top = design.getTopLevelCell();
        fillPortInfo(top);

        final CellType cell = findCell(top, ci.getFullyQualifiedType());
        final CellType newCell;

        // Create half-operators and global nets so that can determine isShared in converter 
        if (minimizeTriRegs) {
            final String[] gateNames;
            if (gates == null) gateNames = new String[0];
            else gateNames = StringUtil.split(gates, ':');

            design.setMessageCenter(new JautoMessageCenter());
            newCell = cell.instanceMinimalSubTypes(CastDesign.TRANSISTORS, 
                        /*staticizer*/ null, 
                        /*weakInverter*/ null,
                        /*smallInveretr*/ null,
                        gateNames,     /* list of gates we will match to */ 
                        cfp); 
            GlobalNet.generateGlobalNets(newCell, new ArrayList());
        }  else {
            newCell = cell;
        }
        return newCell;
    }

    private static void fillPortInfo(final CellType top) {
        top.walkOnce(new CellTypeProcessor() {
            public void processCellType(CellType c) {
                for (Iterator i = c.getAllSubcellConnections().iterator();
                     i.hasNext(); ) {
                    ConnectionInfo ci = (ConnectionInfo) i.next();
                    final String childTypeName =
                        ci.child.cast_cell.getFullyQualifiedType();
                    if (!portInfo.containsKey(childTypeName)) {
                        portInfo.put(childTypeName, ci);
                    }
                }
            }
        });
    }

    private static CellType findCell(final CellType top, final String name) {
        final CellType[] result = new CellType[1];
        top.walkOnce(new CellTypeProcessor() {
            public void processCellType(CellType c) {
                if (c.cast_cell.getFullyQualifiedType().equals(name)) {
                    result[0] = c;
                }
            }
        });
        return result[0];
    }

    private static
    ConverterInterface getConverter(final String type, final CellType cell,
                                    final VerilogFactoryInterface factory,
                                    final ConnectionInfo ports,
                                    final HierName Vdd, final HierName GND,
                                    final boolean alwaysEscape,
                                    final VerilogChooser chooser,
                                    final Cadencize cad,
                                    final boolean minimizeTriRegs) {
        final ConverterInterface cv;
        if (type.equals("gate")) {
            cv = new GateConverter(cell, factory, ports, GND, alwaysEscape, chooser, cad);
        } else if (type.equals("tri")) {
            cv = new TriConverter(cell, factory, ports, alwaysEscape);
        } else if (type.equals("netgraph")) {
            cv = new NetgraphConverter(cell, factory, ports, Vdd, GND,
                                       alwaysEscape, chooser, cad, minimizeTriRegs);
        } else if (type.equals("netlist")) {
            cv = new NetlistConverter(cell, factory, ports, Vdd, GND, alwaysEscape, chooser);
        } else if (type.equals("lvs")) {
            cv = new LVSConverter(cell, factory, ports, alwaysEscape, chooser);
        } else {
            cv = null;
        }
        return cv;
    }

    public static Map writeVerilog(final CellInterface topCell,
                                   final VisitorFactory visitorFactory,
                                   final CastFileParser cfp,
                                   final CommandLineArgs theArgs,
                                   final VerilogFactoryInterface verilogFactory,
                                   final String converter,
                                   final boolean alwaysEscape) {
        return writeVerilog(topCell, visitorFactory, cfp, theArgs,
                            verilogFactory, converter, alwaysEscape, null);
    }

    public static Map writeVerilog(final CellInterface topCell,
                                   final VisitorFactory visitorFactory,
                                   final CastFileParser cfp,
                                   final CommandLineArgs theArgs,
                                   final VerilogFactoryInterface verilogFactory,
                                   final String converter,
                                   final boolean alwaysEscape,
                                   final CoSim cosim) {
        final Cadencize cad = new Cadencize(true, true);
        final CastDesign design = loadDesign( topCell, cad, theArgs );
        final CellType cell = loadCell(design, topCell, cfp, theArgs);

        if (cell == null) {
            throw new RuntimeException( "Unable to get CellType for \"" +
                                        topCell.getFullyQualifiedType() +
                                        "\"." );
        } else {
            try {
                return writeVerilog(cell, converter, theArgs, verilogFactory, visitorFactory, alwaysEscape, getChooser(cosim, topCell, theArgs), false, cad, theArgs.argExists("minimize-tri-regs"), false);
            } catch (Exception e) {
                throw new RuntimeException("Invalid cosim spec specified", e);
            }
        }
    }

    private static VerilogChooser getChooser(final CoSim cosim,
                                             final CellInterface castCell,
                                             final CommandLineArgs theArgs)
        throws DuplicateInstanceSpecException,
               ExtraInstanceSpecException,
               HierarchyDepthException,
               NoBehaviorFoundException,
               NoSuchInstanceException,
               NoSuchEnvironmentException {
        VerilogChooser chooser = parseVerilogBlock(theArgs);

        if (chooser == null && cosim != null) {
            final String envName = cosim.getEnvName();

            final CoSimParameters coSimParams = new CoSimParameters();
            final MultiMap behaviorType = new MultiMap();
            CoSimHelper.setCoSimParams("x", castCell, cosim.getCoSimSpecList(),
                                       coSimParams, null, false);
            CoSimHelper.getBehaviorByType(castCell, coSimParams,
                                          HierName.makeHierName("x"),
                                          behaviorType);
            if (envName != null) {
                final CellInterface envCell = castCell.getEnvironment(envName);
                CoSimHelper.setCoSimParams("_env", envCell,
                        new CoSimSpecList(
                            new CoSimSpec[] { cosim.getEnvSpec() }),
                        coSimParams, null, false);
                CoSimHelper.getBehaviorByType(envCell, coSimParams,
                                              HierName.makeHierName("_env"),
                                              behaviorType);
            }

            final List tooManyBehaviors = new ArrayList();
            for (Iterator i = behaviorType.keySet().iterator(); i.hasNext(); ) {
                final String key = (String) i.next();
                final Collection behs = (Collection) behaviorType.get(key);
                if (behs.size() > 1) tooManyBehaviors.add(key);
            }
            if (!tooManyBehaviors.isEmpty()) {
                System.err.println("Only one behavior per cell is supported:");
                for (Iterator i = tooManyBehaviors.iterator(); i.hasNext(); ) {
                    final String key = (String) i.next();
                    System.err.print(key + ":");
                    final Collection behs = (Collection) behaviorType.get(key);
                    for (Iterator j = behs.iterator(); j.hasNext(); ) {
                        System.err.print(" " + j.next());
                    }
                }
                System.err.println();
                System.exit(2);
            }
            chooser = new CosimChooser(behaviorType);
        }

        return chooser;
    }

    private static void walk(final CellType cell, final CellTypeProcessor p,
                             final VerilogChooser chooser, final boolean routed,
                             final UnaryPredicate stopRecurse,
                             final HashSet seen, final boolean inlineVerilog) {
        if (!seen.add(cell)) return;
        // Do not visit this cell or its subcells if a Verilog block for this
        // cell is to be used.
        boolean chooseVerilog =
            AbstractConverter.chooseVerilog(cell.cast_cell, chooser) != null;

        if (inlineVerilog && chooseVerilog) return;

        p.processCellType(cell);

        // Do not visit the subcells if we are considering the routed
        // directive, as the cell should be considered a black box.
        // But always visit the subcells of the top level cell, even if
        // routed=true; otherwise there is no way to route the top level
        if (routed && CellUtils.isRouted(cell.cast_cell) && seen.size() > 1 ||
            chooseVerilog)
            return;

        if (stopRecurse.evaluate(cell.cast_cell)) return;

        for (Iterator i = cell.getAllSubcellConnections().iterator();
             i.hasNext(); ) {
            final CellType subcell = ((ConnectionInfo) i.next()).child;
            walk(subcell, p, chooser, routed, stopRecurse, seen, inlineVerilog);
        }
    }

    private static Map writeVerilog(final CellType cell,
                                    final String converter,
                                    final CommandLineArgs theArgs,
                                    final VerilogFactoryInterface f,
                                    final VisitorFactory vfact,
                                    final boolean alwaysEscape,
                                    final VerilogChooser chooser,
                                    final boolean hasEnv,
                                    final Cadencize cad,
                                    final boolean minimizeTriRegs,
                                    final boolean routed) {
        return writeVerilog(cell, converter, theArgs, f, vfact, alwaysEscape,
                            chooser, hasEnv, cad, minimizeTriRegs,
                            new UnaryPredicate.Constant(false), routed);
    }

    private static Map writeVerilog(final CellType cell,
                                    final String converter,
                                    final CommandLineArgs theArgs,
                                    final VerilogFactoryInterface f,
                                    final VisitorFactory vfact,
                                    final boolean alwaysEscape,
                                    final VerilogChooser chooser,
                                    final boolean hasEnv,
                                    final Cadencize cad,
                                    final boolean minimizeTriRegs,
                                    final UnaryPredicate stopRecurse,
                                    final boolean routed) {
        final Map depends = new HashMap();
        final VerilogChooser realChooser =
            chooser == null ? new NullChooser() : chooser;
        walk(cell, new CellTypeProcessor() {
            boolean topEnv = hasEnv;
            String topName = theArgs.getArgValue("toplevel", null);
            boolean toplevel = true;
            public void processCellType(CellType c) {
                final VerilogObject top = f.ident(
                    topName == null ? CellUtils.hashMetaParameters(c.typeName)
                                    : topName, alwaysEscape);

                final VerilogVisitor visitor = vfact.getVisitor(c.typeName);
                if (visitor == null) return;

                final ConnectionInfo p = (ConnectionInfo)
                        portInfo.get(c.cast_cell.getFullyQualifiedType());
                assert p != null : "Cannot produce Verilog for top-level cell: " + c.cast_cell.getFullyQualifiedType();
                final ConverterInterface cv = getConverter(converter, c, f, p, Vdd, GND, alwaysEscape, realChooser, cad,
                        minimizeTriRegs);
                if (cv == null)
                    throw new RuntimeException("Unknown converter type: " + converter);
                final VerilogObject vo =
                    cv.convert(theArgs, top, toplevel, topEnv);
                topName = null;
                topEnv = false;
                toplevel = false;
                if (vo == null) return;

                final Map dependencies = cv.getDependencies();
                if (dependencies.size() > 0) {
                    depends.putAll(dependencies);
                }

                try {
                    vo.accept(visitor);
                } catch (Exception e) {
                    throw new RuntimeException("Cannot convert cell " + cell.typeName, e);
                }
                vfact.doneVisitor(visitor);
            }
        }, realChooser, routed, stopRecurse, new HashSet(),
        !theArgs.argExists("uninline-verilog"));
        return depends;
    }

    public static void verilogFiles(final Map dependencies,
                                    final Set goodfiles) {
        for (Iterator i = dependencies.entrySet().iterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            final CellInterface ci = (CellInterface) entry.getKey();

            final String[] f = (String[]) entry.getValue();
            for (int j = 0; j < f.length; ++j) {
                goodfiles.add(f[j]);
            }
        }
    }

    /**
     * Runs the netgraph converter on the specified cell.
     *
     * @param cell cell to convert
     * @param level named verilog block to choose, or <code>null</code> if
     * verilog blocks should not be considered
     * @param theArgs arguments to pass on to the netgraph converter
     * @param validVerilogFiles verilog files needed by verilog blocks that are
     * found
     * @param invalidVerilogFiles verilog files needed by verilog blocks that
     * are not found
     **/
    public static void emitVerilogBlock(final CellInterface cell,
                                        final Cadencize cad,
                                        final String level,
                                        final VisitorFactory vfact,
                                        final CommandLineArgs theArgs,
                                        final Set validVerilogFiles)
    throws IOException {
        final CastDesign design =
            new CastDesign(cell, Vdd, GND, _RESET, 100, null, cad, null,
                           !theArgs.argExists("ignore-asta-extra-delay"));
        final CellType top = design.getTopLevelCell();
        fillPortInfo(top);

        final CellType ct = findCell(top, cell.getFullyQualifiedType());
        // CastDesign always creats a dummy top-level cell with the same name;
        // CadenceInfo for this dummy cell cannot be shared, so we remove it
        // explicitly
        cad.removeCachedCadenceInfo(top.typeName);

        final SimpleChooser chooser =
            level == null ? null : new SimpleChooser(level);
        final Map dependencies =
            writeVerilog(ct, "netgraph", theArgs,
                         new VerilogFactoryImpl(),
                         vfact, true, chooser, false, cad, false, false);

        verilogFiles(dependencies, validVerilogFiles);

    }

    public static void emitVerilogNetlist(final CellInterface cell,
                                          final Cadencize cad,
                                          final VisitorFactory vfact,
                                          final UnaryPredicate stopRecurse,
                                          final CDLNameInterface renamer,
                                          final CommandLineArgs theArgs)
    throws IOException {
        final CastDesign design =
            new CastDesign(cell, Vdd, GND, _RESET, 100, null, cad, null,
                           1, Collections.<String>emptySet(),
                           Collections.emptySet(), true,
                           !theArgs.argExists("ignore-asta-extra-delay"));
        final CellType top = design.getTopLevelCell();
        fillPortInfo(top);

        final CellType ct = findCell(top, cell.getFullyQualifiedType());

        final VerilogFactoryInterface factory = renamer == null ?
            new VerilogFactoryImpl() :
            new SimpleRenamingVerilogFactory(renamer);

        final Map dependencies =
            writeVerilog(ct, "netlist", theArgs, factory,
                         vfact, true, null, false, cad, false, stopRecurse,
                         true);
    }

    private static void writeList(final Collection/*<String>*/ list,
                                  final Writer lister) throws IOException {
        for (Iterator i = list.iterator(); i.hasNext(); ) {
            lister.write((String) i.next() + "\n");
        }
        lister.flush();
    }

    private static void writeList(final Collection/*<String>*/ list,
                                  final String outfile) throws IOException {
        final Writer lister = new FileWriter(outfile);
        writeList(list, lister);
        lister.close();
    }

    private static VerilogChooser parseVerilogBlock(final CommandLineArgs args)
    {
        final String verilogBlock = args.getArgValue("verilog-block", null);
        final VerilogChooser chooser;
        if (verilogBlock == null) {
            chooser = null;
        } else {
            final String[] names = StringUtil.split(verilogBlock, ':');
            final Map m = new HashMap();
            for (int i = 1; i < names.length; ++i) {
                final String[] parts = StringUtil.split(names[i], '=');
                if (parts.length != 2) {
                    throw new RuntimeException("Invalid <cell type>=<verilog block> specification: " + names[i]);
                }
                m.put(parts[0], parts[1]);
            }
            chooser = new DefaultChooser(names[0], m);
        }
        return chooser;
    }

    private static void usage() {
        final String translateSchemes =
            Rename.getNamespaces().collect(Collectors.joining(" | "));
        System.err.print(
"Usage: java com.avlsi.tools.prs2verilog.Prs2Verilog\n" +
"   --cast-path=<path> (CAST path; defaults to .)\n" +
"   --verilog-block=<default>:<cell>=<block>:... (specify verilog block to use)\n" +
"   --cast-version=[ 1 | 2 ] (defaults to 2)\n" +
"     --outdir=<dir> (output directory, 1 module per file)\n" +
"   | --outfile=<file> (output file with all modules)\n" +
"   [ --file-list=<file> | <file>:<file> ] (file lists needed for simulation)\n" +
"     --cell=<cellName[:env]> (cell to translate)\n" +
"   [--library=<CDL library> (a gate library to match against), required if --minimize-tri-regs not set]\n" +
"   [--special=<path> (path to find cells that required manual translation)]\n" +
"   --converter=[ gate | wire | netgraph | netlist | lvs ] (defaults to netgraph)\n" +
"   [--number-gate=<n>] (name AND, OR gates with <= n inputs as AND<k>, OR<k>)\n" +
//"   [--tau=<number>] (digital delay is multiplied by specified number)\n" +
"   [--estimated-delay] (use estimated_delay directives for delay)\n" +
"   [--estimated-tau=<tau>] (tau to use if the tau directive does not exist)\n" +
"   [--clk] (adds a clk signal to all subcircuits)\n" +
"   [--toplevel=<top-level name>] (override the cell name)\n" +
"   [--internal-net-wire] (declare internal nets as wire instead of trireg)\n" +
"   [--relative-path]\n" + 
"   [--instance=<name>] (name of the cell for DSim integration)\n" +
"   [--env-instance=<name>] (name of the environment for DSim integration)\n" +
"   [--minimize-tri-regs] (places tri-regs only on shared busses, does not work yet with clk option)\n" +
"   [--gates=<list of gates seperated by :>] (list of gates to match with for minimize-tri-reg option)\n" +
"   [--config=<path to process.config that contains technology data>] (required for minimize-tri-reg option)\n" +
"   [--by-name] (connect ports by name; only for netlist converter)\n" +
"   [--translate=<" + translateSchemes + ">] (name translation)\n" +
"   [--skip-power-rail] (tell netlist, netgraph converter to skip power rails)\n" +
"   [--routed] (makes netlist converter consider routed directives)\n" +
"   [--cadence-name] (treat cell name as a Cadence name; no cosim spec allowed)\n" +
"   [--power-grid-template=<template>] (name of powergrid tieoff cell; $lib$\n" +
"                                       $type$, $subtype$ are substituted)\n" +
"   [--ifdef] (surround each module definition with `ifdef directives)\n" +
"   [--zoix] (enable some Zoix workarounds; only for netgraph converter)\n" +
"   [--ignore-inline[=<cell>:<cell>:...]] (do not process inline keyword)\n" +
"   [--macro-model] (put blackbox and related logic in seperate module; only\n" +
"                    for netgraph converter)\n" +
"   [--no-reset-fault] (generate Zoix parameters to try to avoid faults on\n" +
"                       reset; for netgraph converter)\n" +
"   [--make-gnd-0] (use 1'b0 in places of GND; for netgraph converter)\n" +
"   [--make-vdd-1] (use 1'b1 in places of Vdd; for netgraph converter)\n" +
"   [--extra-port-info] (put port names into blackbox names; for netgraph converter)\n" +
"   [--trireg-output] (declare non-combinational output ports of the top level\n" +
"                      cell as trireg)\n" +
"   [--uninline-verilog] (do not inline Verilog block)\n");
    }
    public static void realMain(String[] args) throws Exception {
        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
        final CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles( parsedArgs ); 

        final CommandLineArgs cachedArgs = 
            new CachingCommandLineArgs( argsWithConfigs );

        final CommandLineArgs theArgs = cachedArgs;

        if (theArgs.argExists("version")) {
            System.out.println(
                com.avlsi.util.debug.VersionInfo.getVersionString(
                    Prs2Verilog.class));
        }

        final String castRoot = theArgs.getArgValue("cast-path", ".");
        final String castVersion = theArgs.getArgValue("cast-version", "2");
        final String outdir = theArgs.getArgValue("outdir", null);
        final String outfile = theArgs.getArgValue("outfile", null);
        final String unprocessedSpec = theArgs.getArgValue("cell", null);
        final String converter = theArgs.getArgValue("converter", "netgraph");
        final String special = theArgs.getArgValue("special", null);
        final String filelist = theArgs.getArgValue("file-list", null);
        final boolean relativePath = theArgs.argExists("relative-path");
        final boolean minimizeTriRegs = theArgs.argExists("minimize-tri-regs");
        final String gates = theArgs.getArgValue("gates", null);
        final boolean ifdef = theArgs.argExists("ifdef");
        final boolean routed = theArgs.argExists("routed");

        if (unprocessedSpec == null) {
            System.err.println("--cell must be specified.");
            usage();
            System.exit(1);
        }

        if ((outfile == null) == (outdir == null)) {
            System.err.println("One and only one of --outdir and --outfile must be specified.");
            usage();
            System.exit(1);
        }

        if ((minimizeTriRegs && !converter.equals("netgraph"))) {
            System.err.println("Minimizing tri-state registers only works with netgraph converter."); 
            usage();
            System.exit(1);
        }

        final String translate = theArgs.getArgValue("translate", null);
        final CDLNameInterface renamer;
        if (translate == null) {
            renamer = null;
        } else {
            renamer = Rename.getInterface("cast", translate);
            if (renamer == null) {
                System.err.println("Unknown name translation method: " +
                                   translate);
                System.exit(1);
            }
        }

        /* This may now work 
        if (!minimizeTriRegs && (gates != null)) {
            System.err.println("Gate-list option can only be used with minimizing tri-state registers ."); 
            usage();
            System.exit(1);
        }
        */

        final String cosimSpec;
        if (theArgs.argExists("cadence-name")) {
            cosimSpec =
                new CadenceReverseNameInterface().renameCell(unprocessedSpec);
        } else {
            cosimSpec = unprocessedSpec;
        }

        final CoSimSpec defaultSpec =
            new CoSimSpec(
                new ModeListLevelSpec(
                    new ModeList(new Mode[] { Mode.SUBCELLS, Mode.PRS })),
                new InstSpecList(new InstSpec[0]));

        final CoSim cosim =
            CoSim.getCoSim(cosimSpec, true,
                           new CoSimSpecList(new CoSimSpec[] { defaultSpec }),
                           defaultSpec);
        final String cellName = cosim.getCellType();
        final String envName = cosim.getEnvName();

        assert cellName != null : "Invalid cosim spec: " + cosimSpec;

        DirectiveTable.registerDirective(BlockInterface.CELL, "infile", DirectiveConstants.STRING_TYPE, DirectiveConstants.STRING_TYPE, null);
        DirectiveTable.registerDirective(BlockInterface.CELL, "outfile", DirectiveConstants.STRING_TYPE, DirectiveConstants.STRING_TYPE, null);
        DirectiveTable.registerDirective(BlockInterface.CELL, "memsize", DirectiveConstants.STRING_TYPE, DirectiveConstants.INT_TYPE, null);
        DirectiveTable.registerDirective(BlockInterface.CELL, "data_set", DirectiveConstants.STRING_TYPE, DirectiveConstants.INT_TYPE, null);
        DirectiveTable.registerDirective(BlockInterface.CELL, "data_reset", DirectiveConstants.STRING_TYPE, DirectiveConstants.INT_TYPE, null);
        DirectiveTable.registerDirective(BlockInterface.CELL, "enable_set", DirectiveConstants.STRING_TYPE, DirectiveConstants.INT_TYPE, null);
        DirectiveTable.registerDirective(BlockInterface.CELL, "enable_reset", DirectiveConstants.STRING_TYPE, DirectiveConstants.INT_TYPE, null);

        final SearchPath castPath = new FileSearchPath(castRoot);

        // perhaps move ignore-inline processing to StandardParsingOption if it
        // proves useful for other tools as well
        final String ignoreInline = theArgs.getArgValue("ignore-inline", null);
        final String[] ignoreCells =
            ignoreInline == null ? new String[0]
                                 : StringUtil.split(ignoreInline, ':');
        final StandardParsingOption spo = new StandardParsingOption(theArgs) {
            private final UnaryPredicate inlineCheck =
                CellUtils.getTypeMatcher(Arrays.asList(ignoreCells));
            public boolean processInline(final CellInterface cell) {
                if (theArgs.argExists("ignore-inline")) {
                    if (ignoreInline == null) return false;
                    else return !inlineCheck.evaluate(cell);
                } else {
                    return true;
                }
            }
        };
        final CastFileParser cfp =
            new CastFileParser(castPath, castVersion, spo);

        final CastDesign design;
        final String targetName;

        final CellInterface plainCell = cfp.getFullyQualifiedCell(cellName);
        final CellInterface castCell =
            routed ? plainCell.routedSubcells(false) : plainCell;
        final CellInterface ci;
        if (envName == null) {
            ci = castCell;
        } else {
            ci = CellUtils.getEnvWithCell(castCell, envName,
                                          castCell.getType() + "$" + envName,
                                          "_env",
                                          "x");
        }

        // Ignore all verilog blocks when using the netlist converter.  The
        // generated files are used for routing, so the module ports must match
        // what is in CDL.  Considering verilog blocks may cause a port to be
        // marked as used when it isn't really used in CDL.
        final boolean ignoreVerilog = converter.equals("netlist") &&
                                      !theArgs.argExists("uninline-verilog");
        final VerilogChooser chooser =
            ignoreVerilog ? null : getChooser(cosim, castCell, theArgs);

        final Cadencize cad = new Cadencize(true, !ignoreVerilog);
        design = loadDesign(ci, cad, theArgs);
        final CellType newCell =
            loadCell(design, ci, cfp, minimizeTriRegs, gates);

        final String[] filesLists =
            filelist == null ? new String[0] : StringUtil.split(filelist, ':');
        if (newCell == null) {
            System.err.println("Cell " + ci.getFullyQualifiedType() + " not found!");
        } else {
            final Set files = new LinkedHashSet();
            final VerilogFactoryInterface factory = renamer == null ?
                new VerilogFactoryImpl() :
                new SimpleRenamingVerilogFactory(
                    renamer instanceof GDS2NameInterface ?
                        (CDLNameInterface) new KeywordRenamer(renamer)
                      : renamer);

            final VisitorFactory vf;
            if (outdir != null) {
                vf = new MultipleFileVisitor(outdir, special, files,
                                             relativePath, ifdef);
            } else {
                final Writer fileWriter;
                try {
                    fileWriter = new FileWriter(outfile);
                } catch (IOException e) {
                    throw new RuntimeException("Cannot write to " + outfile, e);
                }
                vf = new SingleWriterVisitor(fileWriter, ifdef);
            }
            final Map dependencies =
                writeVerilog(newCell, converter, theArgs, factory, vf,
                             true, chooser, envName != null, cad,
                             minimizeTriRegs, routed);
            if (filesLists.length == 0) return;
            final Set goodfiles = new LinkedHashSet();
            verilogFiles(dependencies, goodfiles);

            Writer lister = new FileWriter(filesLists[0]);
            for (Iterator i = goodfiles.iterator(); i.hasNext(); ) {
                lister.write((String) i.next() + "\n");
            }

            if (filesLists.length > 1) {
                lister.flush();
                lister.close();
                lister = new FileWriter(filesLists[1]);
            }
            for (Iterator i = files.iterator(); i.hasNext(); ) {
                lister.write((String) i.next() + "\n");
            }
            lister.flush();
            lister.close();
        }
    }
    public static void main(String[] args) throws Exception {
        try {
            realMain(args);
        } catch (NoSuchEnvironmentException e) {
            System.err.println("Cannot load environment " +
                               e.getEnvironmentName() +
                               " from cell " +
                               e.getCellName());
            ExceptionPrettyPrinter.printException(e, System.err);
            System.exit(2);
        } catch (CastSemanticException e) {
            ExceptionPrettyPrinter.printException(e, System.err);
            System.exit(2);
        }
    }
}
