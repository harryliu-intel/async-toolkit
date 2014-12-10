/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */



package com.avlsi.csp.csp2java;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Writer;

import java.util.ListIterator;

import com.avlsi.io.FileSearchPath;

import com.avlsi.cast.CastFile;
import com.avlsi.cast.CastFileParser;

import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.util.DirectiveUtils;

import com.avlsi.csp.ast.VisitorException;
import com.avlsi.csp.ast.CSPProgram;
import com.avlsi.csp.util.CSPCellInfo;
import com.avlsi.csp.util.ConstantEvaluator;
import com.avlsi.cell.CellInterface;
import com.avlsi.tools.cosim.CoSimInfo;

import com.avlsi.util.container.StringContainerIterator;

import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;

/**
 * Generates cosim Java code from a cell's CSP block.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class CSP2Java {
    PrintWriter warningWriter;
    PrintWriter errorWriter;
    PrintWriter debugWriter;
    boolean emitCoverageProbes;
    boolean emitSnoopStatements;

    public CSP2Java(PrintWriter warningWriter,
                    PrintWriter errorWriter,
                    PrintWriter debugWriter,
                    boolean emitCoverageProbes,
                    boolean emitSnoopStatements) {
        this.warningWriter = warningWriter;
        this.errorWriter = errorWriter;
        this.debugWriter = debugWriter;
        this.emitCoverageProbes = emitCoverageProbes;
        this.emitSnoopStatements = emitSnoopStatements;
    }


    /**
     * Generates java code from csp.
     *
     * @param p  parsed CSP program
     * @param packageName  package name for the generated Java class.  
     *     May be null for the empty package.
     * @param className  class name for the generated Java class.
     * @param cellInfo Contains information on the cell's ports and metaparameters.
     * @param pw  PrintWriter to which to write the generated class.
     * @param strictVars  error on undeclared variable (bug 6085)
     **/
    private void genCode(final CSPProgram p,
                         final String packageName,
                         final String className,
                         final CSPCellInfo cellInfo,
                         final PrintWriter pw,
                         final boolean strictVars,
                         final boolean disableDftHandler,
                         final boolean synthesizable)
        throws SemanticException {
        try {
            p.accept(new JavaEmitter(packageName, className, cellInfo, pw, 
                                     warningWriter, errorWriter, debugWriter,
                                     emitCoverageProbes,
                                     emitSnoopStatements, strictVars,
                                     disableDftHandler, synthesizable));
        } catch (VisitorException e) {
            throw new SemanticException(e);
        }
    }

    /**
     * Generates java code from csp.
     *
     * @param cell  CAST cell from which channel declarations are fetched
     * @param packageName  package name for the generated Java class.  
     *     May be null for the empty package.
     * @param writer  Writer to which java code should be written
     * @param className  class name for the generated Java class.
     **/
    public void convert(final CellInterface cell,
                        final String className,
                        final Writer w)
        throws IOException, SemanticException {
        // begin debugging
        int j = 0;
        for (final java.util.Iterator i = cell.getPortDefinitions();
                i.hasNext(); ) {
            final com.avlsi.fast.ports.PortDefinition portDef =
                (com.avlsi.fast.ports.PortDefinition) i.next();
            debugWriter.println("port " + j++ + " " + portDef);
        }
        // end debugging
        final PrintWriter out = new PrintWriter(new BufferedWriter(w));
        CSPProgram p = cell.getCSPProgram();
        if (p == null) {
            throw new NoCSPBlockException();
        }
        /*
        try {
            p = com.avlsi.csp.util.ConstantEvaluator.evaluate(p);
        } catch (VisitorException e) {
            throw new SemanticException(e);
        }
        */
        boolean strictVars =
            ((Boolean)DirectiveUtils.getCspDirective
             (cell, DirectiveConstants.STRICT_VARS)).booleanValue();
        boolean disableDftHandler =
            ((Boolean)DirectiveUtils.getCspDirective
             (cell, DirectiveConstants.DISABLE_CHANDFT_HANDLER)).booleanValue();
        boolean synthesizable =
            ((Boolean)DirectiveUtils.getCspDirective
             (cell, DirectiveConstants.SYNTHESIZABLE)).booleanValue();
        genCode(p, null, className, cell.getCSPInfo(), out, strictVars,
                disableDftHandler, synthesizable);
        out.close();
    }

    /**
     * Prints usage message and exits.
     **/
    private static void usage() {
        System.out.println("Usage: " +
                "csp2java [--cast-path=castpath] [--cast-file=file] CAST_CELL java_class\n" +
                "  If CAST_CELL is a fully qualified cell name, then the value of cast-file is ignored.\n" +
                "  If CAST_CELL is not a fully qualified cell name, then the specified cast file is searched\n" +
                "  for the specified cell." );
        System.exit(1);
    }

    /**
     * Converts CSP to java.  Java is written to Foo.java.
     *
     * @param args  Arguments.  
     *   <code>args[0]</code> is the cast file name.
     *   <code>args[1]</code> is the cast cell name.
     *      [Hacked to accept an instance name also.  --dhilvert]
     *   <code>args[2]</code> is the output java class name.
     **/
    public static void main(String[] args) throws Exception {

        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
        final CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles( parsedArgs ); 

        final CommandLineArgs cachedArgs = 
            new CachingCommandLineArgs( argsWithConfigs );

        final CommandLineArgs theArgs = cachedArgs;

        
        final boolean emitCoverageProbes = theArgs.argExists( "emit-coverage-probes" );
        final boolean emitSnoopStatements =
            theArgs.argExists( "emit-snoop-statements" );
        
        if (theArgs.argExists("version")) {
            System.out.println(
                com.avlsi.util.debug.VersionInfo.getVersionString(
                    CSP2Java.class));
        }

        final FileSearchPath castFSP = new FileSearchPath( theArgs.getArgValue( "cast-path", "." ) );

        final String castFileName = theArgs.getArgValue("cast-file" , null);
        String castCellName = null;
        String javaClassName = null;

        StringContainerIterator strIter = theArgs.nonParsedArgumentsIterator();

        if ( strIter.hasNext() ) {
            castCellName = strIter.next();
            if ( strIter.hasNext() ) {
                javaClassName = strIter.next();
            }
            else {
                usage();
            }
        }
        else {
            usage();
        }

        final CastFileParser cfp =
            new CastFileParser(castFSP, "2");
       
        CellInterface cell = null;

        if ( castCellName.lastIndexOf( '.' ) != -1 ) {
            cell = cfp.getFullyQualifiedCell( castCellName );
        }
        else {
            final CastFile cf = cfp.parse(castFileName);
            cell = cf.getCell(castCellName);
        }

        final PrintWriter systemOutWriter = new PrintWriter( new OutputStreamWriter( System.out ) );
        
        new CSP2Java(systemOutWriter, 
                     systemOutWriter, 
                     systemOutWriter,
                     emitCoverageProbes,
                     emitSnoopStatements).convert(cell, javaClassName, new FileWriter(javaClassName + ".java"));
    }
}
