/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.csp.csp2gma;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.OutputStreamWriter;

import com.avlsi.cast.CastFileParser;
import com.avlsi.cell.CellInterface;
import com.avlsi.csp.ast.VisitorException;
import com.avlsi.csp.ast.CSPProgram;
import com.avlsi.csp.csp2java.NoCSPBlockException;
import com.avlsi.csp.csp2java.SemanticException;
import com.avlsi.csp.util.CSPCellInfo;
import com.avlsi.csp.util.ConstantEvaluator;
import com.avlsi.io.FileSearchPath;
import com.avlsi.tools.cosim.CoSimInfo;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;


/**
 * Generates gma from a cell's CSP block.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class Csp2Gma {
    private final PrintWriter warningWriter;
    private final PrintWriter errorWriter;
    private final PrintWriter debugWriter;

    public Csp2Gma(PrintWriter warningWriter,
                   PrintWriter errorWriter,
                   PrintWriter debugWriter) {
        this.warningWriter = warningWriter;
        this.errorWriter = errorWriter;
        this.debugWriter = debugWriter;
    }
    
    /**
     * Generates GMAs from csp.
     *
     * @param p  parsed CSP program
     * @param cellInfo Contains information on the cell's ports and metaparameters.
     * @param pw  PrintWriter to which to write the generated class.
     **/
    private void genCode(final CSPProgram p,
                         final CSPCellInfo cellInfo,
                         final PrintWriter pw)
        throws SemanticException {
        try {
            p.accept(new GmaEmitter(cellInfo, pw, 
                                    warningWriter, errorWriter, debugWriter));
        } catch (VisitorException e) {
            throw new SemanticException(e);
        }
    }

    /**
     * Generates GMAs from csp.
     *
     * @param cell
     *        CAST cell from which channel declarations and CSP program
     *        are fetched
     * @param fileName
     *        name of generated GMA file
     **/
    public void convert(final CellInterface cell,
                        final File file)
        throws IOException, SemanticException {
        final PrintWriter out =
            new PrintWriter(
                new BufferedWriter(
                    new FileWriter(file)));
        final CSPProgram p = cell.getCSPProgram();
        if (p == null) {
            throw new NoCSPBlockException();
        }
        final CSPProgram unrolled;
        try {
            unrolled = ConstantEvaluator.evaluate(p);
        } catch (VisitorException e) {
            throw new SemanticException(e);
        }
        genCode(unrolled, cell.getCSPInfo(), out);
        out.close();
    }

    /**
     * Prints usage message and exits.
     **/
    private static void usage() {
        System.out.println("Usage: " +
                "csp2gma [--cast-path=castpath] --cell=f.q.c.n" +
                 "[--output-file=file_name]");
        System.exit(1);
    }

    /**
     * Converts CSP to GMAs.  By default, GMAs are written to cell_name.asim.
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
                    Csp2Gma.class));
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
            theArgs.getArgValue("output-file", castCellName + ".asim");
        
        new Csp2Gma(systemErrWriter, systemErrWriter, systemErrWriter)
            .convert(cell, new File(outputFileName));
    }
}
