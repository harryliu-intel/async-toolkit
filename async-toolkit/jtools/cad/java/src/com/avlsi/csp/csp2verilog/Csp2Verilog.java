/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.csp.csp2verilog;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.OutputStreamWriter;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

import com.avlsi.cast.CastFileParser;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cell.CellInterface;
import com.avlsi.csp.ast.*;
import com.avlsi.csp.csp2java.NoCSPBlockException;
import com.avlsi.csp.csp2java.SemanticException;
import com.avlsi.csp.csp2xml.XmlTourist;
import com.avlsi.csp.util.CSPCellInfo;
import com.avlsi.csp.util.ConstantEvaluator;
import com.avlsi.csp.util.DeclarationProcessor;
import com.avlsi.csp.util.FunctionPreprocessor;
import com.avlsi.csp.util.Problem;
import com.avlsi.csp.util.ProblemFilter;
import com.avlsi.csp.util.RefinementResolver;
import com.avlsi.io.FileSearchPath;
import com.avlsi.io.Printable;
import com.avlsi.io.WrapPrintWriter;
import com.avlsi.tools.cosim.CoSimInfo;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.functions.BinaryFunction;
import com.avlsi.util.functions.UnaryFunction;
import com.avlsi.util.functions.UnaryPredicate;

/**
 * Generates gma from a cell's CSP block.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class Csp2Verilog {
    private final PrintWriter warningWriter;
    private final PrintWriter errorWriter;
    private final PrintWriter debugWriter;
    private final String resetNodeName; 
    private final int registerBitWidth; 
    private final ProblemFilter probFilter;
    private final boolean debugTransformation;
    private final boolean enableSystemVerilog;
    private List<StructureDeclaration> structDecls = Collections.emptyList();

    public Csp2Verilog(PrintWriter warningWriter,
                       PrintWriter errorWriter,
                       PrintWriter debugWriter,
                       String resetNodeName,
                       int registerBitWidth,
                       ProblemFilter probFilter,
                       boolean enableSystemVerilog) {
        this(warningWriter, errorWriter, debugWriter, resetNodeName,
             registerBitWidth, probFilter, enableSystemVerilog, false);
    }

    public Csp2Verilog(PrintWriter warningWriter,
                       PrintWriter errorWriter,
                       PrintWriter debugWriter,
                       String resetNodeName,
                       int registerBitWidth,
                       ProblemFilter probFilter,
                       boolean enableSystemVerilog,
                       boolean debugTransformation) {
        this.warningWriter = warningWriter;
        this.errorWriter = errorWriter;
        this.debugWriter = debugWriter;
        this.resetNodeName = resetNodeName; 
        this.registerBitWidth = registerBitWidth;
        this.probFilter = probFilter;
        this.enableSystemVerilog = enableSystemVerilog;
        this.debugTransformation = debugTransformation;
    }

    /**
     * Generates Verilog from csp.
     *
     * @param p  parsed CSP program
     * @param cellInfo Contains information on the cell's ports and metaparameters.
     * @param pw  PrintWriter to which to write the generated class.
     **/
    private void genCode(final CSPProgram p,
                         final String moduleName,
                         final CSPCellInfo cellInfo,
                         final Collection/*<String>*/ inputPorts,
                         final boolean strictVars,
                         final boolean implicitInit,
                         final PrintWriter pw)
        throws SemanticException {

        try {
            final VisitorInterface visitor;
            VerilogEmitter emitter = null;
            if (enableSystemVerilog) {
                visitor = new SystemVerilogEmitter(cellInfo, inputPorts,
                    pw, warningWriter, errorWriter, debugWriter,
                    resetNodeName, registerBitWidth, strictVars,
                    implicitInit, probFilter);
            } else {
                emitter = new VerilogEmitter(
                    cellInfo, inputPorts,
                    pw, warningWriter, errorWriter, debugWriter,
                    moduleName,
                    resetNodeName, registerBitWidth, strictVars,
                    implicitInit, probFilter);
                visitor = emitter;
            }
            p.accept(visitor);
            if (emitter != null) structDecls = emitter.getStructureDeclaration();
        } catch (VisitorException e) {
            throw new SemanticException(e);
        }
    }

    private void dumpProgram(final CSPProgram prog, final String mesg) {
        if (debugTransformation) {
            try {
                debugWriter.println(mesg);
                prog.accept(new XmlTourist(debugWriter));
                debugWriter.flush();
            } catch (VisitorException e) {
                debugWriter.println("Exception while dumping program: " + e);
            }
        }
    }

    /**
     * Generates Verilog from csp.
     *
     * @param cell
     *        CAST cell from which channel declarations and CSP program
     *        are fetched
     * @param out
     *        PrintWriter to which the verilog should be written.
     **/
    public void convert(final CellInterface cell,
                        final String moduleName,
                        final Collection/*<String>*/ inputPorts,
                        final PrintWriter out)
        throws SemanticException {
        final CSPProgram p = cell.getCSPProgram();
        if (p == null) {
            throw new NoCSPBlockException();
        }
        final CSPProgram unrolled;
        try {
            dumpProgram(p, "Original program");

            final CSPProgram noConst = ConstantEvaluator.evaluate(p);

            dumpProgram(noConst, "After constant removal");

            final FunctionPreprocessor fp =
                new FunctionPreprocessor(cell.getCSPInfo(), null);
            fp.visitCSPProgram(noConst);
            probFilter.process(fp.getProblems());

            unrolled = (CSPProgram) fp.getResult();

            dumpProgram(unrolled, "After function preprocessing");
        } catch (VisitorException e) {
            throw new SemanticException(e);
        }
        final boolean strictVars =
            ((Boolean) DirectiveUtils.getCspDirective(
                           cell,
                           DirectiveConstants.STRICT_VARS)).booleanValue();
        final boolean implicitInit =
            ((Boolean) DirectiveUtils.getCspDirective(
                           cell,
                           DirectiveConstants.IMPLICIT_INIT)).booleanValue();
        genCode(unrolled, moduleName, cell.getCSPInfo(), inputPorts, strictVars,
                implicitInit, out);
    }
    
    public List<StructureDeclaration> getStructureDeclaration() {
        return structDecls;
    }

    /**
     * Prints usage message and exits.
     **/
    private static void usage() {
        System.out.println("Usage: " +
                "csp2verilog [--cast-path=castpath] --cell=f.q.c.n" +
                " [--output-file=file_name] [--debug-transformation]" +
                " [--enable-system-verilog]");
        System.exit(1);
    }

    /**
     * Converts CSP to Verilog.  By default, Verilog is written to cell_name.v.
     *
     * @param args  Arguments.  
     **/
    public static void main(String[] args) throws Exception {

        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl(args);
        final CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles(parsedArgs); 
        final CommandLineArgs theArgs = 
            new CachingCommandLineArgs(argsWithConfigs);
        
        if (theArgs.argExists("version")) {
            System.out.println(
                com.avlsi.util.debug.VersionInfo.getVersionString(
                    Csp2Verilog.class));
        }

        final String castCellName = theArgs.getArgValue("cell", null);
        if (castCellName == null)
            usage();

        final FileSearchPath castFSP =
            new FileSearchPath(theArgs.getArgValue("cast-path", "."));
        final CellInterface cell =
            new CastFileParser(castFSP, "2")
                .getFullyQualifiedCell(castCellName);
        final PrintWriter systemErrWriter =
            new PrintWriter(new OutputStreamWriter(System.err));

        final String outputFileName =
            theArgs.getArgValue("output-file", castCellName + ".v");
        final PrintWriter out =
            new PrintWriter(
                new BufferedWriter(
                    new FileWriter(new File(outputFileName))));

        final boolean debugTransformation =
            theArgs.argExists("debug-transformation");

        final boolean enableSystemVerilog =
            theArgs.argExists("enable-system-verilog");

        final String resetNodeName = "\\$Reset"; 

        final int registerBitWidth = 128; 

        new Csp2Verilog(systemErrWriter, systemErrWriter, systemErrWriter, 
              resetNodeName, registerBitWidth,
              getProblemFilter(systemErrWriter), enableSystemVerilog,
              debugTransformation)
        .convert(cell, "TOP", Collections.EMPTY_LIST, out);
        out.close();
    }
    public static ProblemFilter getProblemFilter(final PrintWriter pw) {
        final Pattern temp = Pattern.compile("^temp\\$[0-9]+$");
        final Printable prntble = new WrapPrintWriter(pw);
        final UnaryFunction classifier = ProblemFilter.getClassifier(
            new HashSet(Arrays.asList("type.checker.undeclared.set.unused",
                                      "preprocessor.assign.to.input",
                                      "type.checker.invalid.guard")));
        final BinaryFunction filter = new BinaryFunction() {
            public Object execute(final Object o1, final Object o2) {
                final String type = (String) o1;
                final Problem prob = (Problem) o2;
                if (prob.getCode()
                        .equals("type.checker.undeclared.set.unused")) {
                    final Object[] args = prob.getArguments();
                    final String var = (String) args[0];
                    if (temp.matcher(var).matches()) {
                        return null;
                    }
                }
                return prntble;
            }
        };
        return new ProblemFilter(classifier, filter);
    }
}
