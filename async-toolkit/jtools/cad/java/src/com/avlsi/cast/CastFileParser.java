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

package com.avlsi.cast;

import java.io.IOException;
import java.io.PrintStream;
import java.util.LinkedList;
import java.util.Map;

import antlr.RecognitionException;
import antlr.TokenStreamException;
import antlr.TokenStreamRecognitionException;

import com.avlsi.cast.impl.CastParserEnvironment;
import com.avlsi.cast.impl.CircularImportException;
import com.avlsi.cast.impl.SelfImportException;
import com.avlsi.cast.impl.SemanticWrapperException;
import com.avlsi.cast2.impl.CastParsingOption;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellImpl;
import com.avlsi.io.SearchPath;
import com.avlsi.io.SearchPathFile;
import com.avlsi.util.debug.Debug;
import com.avlsi.file.common.HierName;
import com.avlsi.tools.dsim.ExceptionPrettyPrinter;

/**
 * Parser for cast files.  Adds some convenience to CastParserEnvironment.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class CastFileParser {
    public static final String defaultCastVersion = "2";
    /**
     * The cast parser environment that stores the environment
     * CellInterface, cast search path, and other globally needed
     * information for the parsing.
     **/
    final CastParserEnvironment cpe;

    /**
     * Keeps track of which version of cast this parses.
     **/
    private final String castVersion;

    /**
     * Whether the cast 2 parser should warn of outdated syntax or
     * not.  Not used for the cast 1 parser.
     **/
    private final boolean verbose;

   /**
     * Returns a new CastSemanticException, with the specified
     * detail message, using the file / line / col information
     * from the specified RecognitionException.
     **/
    private CastSemanticException castSemanticException(
            final RecognitionException re) {
        return new CastSemanticException(re,
                re.getFilename(), re.getLine(), re.getColumn());
    }

    /**
     * Returns a new CastSyntaxException, with the specified
     * detail message, using the file / line / col information
     * from the specified RecognitionException.
     **/
    private CastSyntaxException castSyntaxException(
            final RecognitionException re) {
        return new CastSyntaxException(re,
                re.getFilename(), re.getLine(), re.getColumn());
    }

    public CastFileParser(final SearchPath castPath)
        throws CastSemanticException,
               CastSyntaxException,
               IOException {
        this(castPath, "1");
    }

    public CastFileParser(final SearchPath castPath,
                          String castVersion)
        throws CastSemanticException,
               CastSyntaxException,
               IOException {
        this(castPath, castVersion, true);
    }

    public CastFileParser(final SearchPath castPath,
                          String castVersion,
                          final CastParsingOption opt)
        throws CastSemanticException,
               CastSyntaxException,
               IOException {
        this(castPath, castVersion, true, null, opt);
    }

    public CastFileParser(final SearchPath castPath,
                          String castVersion,
                          boolean verbose)
        throws CastSemanticException,
               CastSyntaxException,
               IOException {
        this(castPath, castVersion, verbose, null);
    }

    public CastFileParser(final SearchPath castPath,
                          String castVersion,
                          boolean verbose,
                          Map importedEnvMap)
        throws CastSemanticException,
               CastSyntaxException,
               IOException {
        this(castPath, castVersion, verbose, importedEnvMap, null);
    }

    public CastFileParser(final SearchPath castPath,
                          String castVersion,
                          boolean verbose,
                          Map importedEnvMap,
                          final CastParsingOption opt)
        throws CastSemanticException,
               CastSyntaxException,
               IOException {
        if (castVersion == null) castVersion = defaultCastVersion;
        this.castVersion = castVersion;
        this.verbose = verbose;
        Debug.assertTrue(castVersion.equals("1") || castVersion.equals("2"),
                     "castversion "+castVersion+" not recognized");

        try {
            cpe = new CastParserEnvironment(castPath, "standard", 
                                            castVersion, verbose,
                                            importedEnvMap, opt);
        } catch (TokenStreamRecognitionException e) {
            throw castSemanticException(e.recog);
        } catch (RecognitionException e) {
            throw castSyntaxException(e);
        } catch (TokenStreamException e) {
            throw new CastSyntaxException(e, "<unknown>", 0, 0);
        } catch (CircularImportException e) {
            throw new CastSemanticException(e, "<file unknown>", 0, 0);
        }
    }

    /**
     * Parse the given file.  
     **/
    public CastFile parse(final String fileName)
        throws CastSemanticException,
               CastSyntaxException,
               IOException {
        try {
            final CellImpl envCell = new CellImpl("$env", null,
                                                  CellImpl.SYNTHETIC_CELL);
            return new CastFile(cpe,
                                cpe.parseFile(fileName, new LinkedList(), envCell ),
                                envCell );
        } catch (TokenStreamRecognitionException e) {
            throw castSemanticException(e.recog);
        } catch (RecognitionException e) {
            throw castSyntaxException(e);
        } catch (TokenStreamException e) {
            throw new CastSyntaxException(e, "<unknown>", 0, 0);
        } catch (CircularImportException e) {
            throw new CastSemanticException(e, "<file unknown>", 0, 0);
        } catch (SelfImportException e) {
            throw new CastSemanticException(e, "<file unknown>", 0, 0);
        }
    }

    /**
     * Parse the given file, bypassing the normal CAST searching mechanism.
     **/
    public CastFile parse(final SearchPathFile file)
        throws CastSemanticException,
               CastSyntaxException,
               IOException {
        try {
            final CellImpl envCell = new CellImpl("$env", null,
                                                  CellImpl.SYNTHETIC_CELL);
            return new CastFile(cpe,
                                cpe.parseFile(file, new LinkedList(), envCell),
                                envCell);
        } catch (TokenStreamRecognitionException e) {
            throw castSemanticException(e.recog);
        } catch (RecognitionException e) {
            throw castSyntaxException(e);
        } catch (TokenStreamException e) {
            throw new CastSyntaxException(e, "<unknown>", 0, 0);
        } catch (CircularImportException e) {
            throw new CastSemanticException(e, "<file unknown>", 0, 0);
        } catch (SelfImportException e) {
            throw new CastSemanticException(e, "<file unknown>", 0, 0);
        }
    }

    /**
     * Parses the file that <code>moduleName</code> is defined in, searching
     * all possible locations.
     **/
    public CastFile parseModule(final String moduleName)
        throws CastSemanticException,
               CastSyntaxException,
               IOException {
        try {
            final CellImpl envCell = new CellImpl("$env", null,
                                                  CellImpl.SYNTHETIC_CELL);
            return new CastFile(cpe,
                    cpe.parseModule(new LinkedList(), envCell, moduleName),
                    envCell);
        } catch (TokenStreamRecognitionException e) {
            throw castSemanticException(e.recog);
        } catch (RecognitionException e) {
            throw castSyntaxException(e);
        } catch (TokenStreamException e) {
            throw new CastSyntaxException(e,
                    "<unknown>", 0, 0);
        } catch (CircularImportException e) {
            throw new CastSemanticException(e,
                    "<file unknown>", 0, 0);
        } catch (SelfImportException e) {
            throw new CastSemanticException(e,
                    "<file unknown>", 0, 0);
        }
    }

    /**
     * Takes a module name and cell name, returns the relevant
     * CellInterface.  Doesn't give any metaparameters.
     **/
    public CellInterface getFullyQualifiedCell(String moduleName,
                                               String cellName) 
            throws CastSemanticException {
        return getFullyQualifiedCell(moduleName, cellName, null, null);
    }

    /**
     * Takes a module name and cell name, returns the relevant
     * CellInterface.  Doesn't give any metaparameters.
     **/
    public CellInterface getFullyQualifiedCell(String moduleName,
                                               String cellName, 
                                               CellImpl parent, 
                                               final HierName instance)
        throws CastSemanticException {

        try {
            return cpe.getFullyQualifiedCell( moduleName, cellName, parent, 
                                                instance );
        } catch (SemanticWrapperException e ) {
            throw new CastSemanticException(e.getCause(),
                    e.getFilename(), e.getLine(), e.getColumn());
        }
        
    }

    public CellInterface getFullyQualifiedCell(String fullyQualifiedCellName) 
           throws CastSemanticException {
        return getFullyQualifiedCell(fullyQualifiedCellName, null, null);
    }

    public CellInterface getFullyQualifiedCellPretty(
            String fullyQualifiedCellName) {
        return getFullyQualifiedCellPretty(fullyQualifiedCellName, System.out);
    }

    public CellInterface getFullyQualifiedCellPretty(
            String fullyQualifiedCellName, PrintStream out) {
        try {
            return getFullyQualifiedCell(fullyQualifiedCellName);
        } catch (CastSemanticException e) {
            ExceptionPrettyPrinter.printException(e, out);
            return null;
        }
    }

    public CellInterface getFullyQualifiedCellPretty(
            String fullyQualifiedCellName, int exitStatus) {
        return getFullyQualifiedCellPretty(fullyQualifiedCellName, System.out,
                                           exitStatus);
    }

    public CellInterface getFullyQualifiedCellPretty(
            String fullyQualifiedCellName, PrintStream out, int exitStatus) {
        final CellInterface result =
            getFullyQualifiedCellPretty(fullyQualifiedCellName, out);
        if (result == null) System.exit(exitStatus);
        return result;
    }


    public CellInterface getFullyQualifiedCell(String fullyQualifiedCellName, 
                                               CellImpl parent, 
                                               final HierName instance)
        throws CastSemanticException {

        try {
            return cpe.getFullyQualifiedCell( fullyQualifiedCellName, parent, 
                                               instance );
        } catch (SemanticWrapperException e) {
            throw new CastSemanticException(e.getCause(),
                    e.getFilename(), e.getLine(), e.getColumn());
        }
       
    }

    public CastParserEnvironment getParserEnvironment() {
        return cpe;
    }
}
