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

package com.avlsi.cast.impl;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import antlr.ASTFactory;
import antlr.CharScanner;
import antlr.RecognitionException;
import antlr.TokenStreamException;
import antlr.TokenStreamRecognitionException;
import antlr.TokenStreamSelector;
import antlr.collections.AST;

import com.avlsi.cast2.impl.CastTwoLexer;
import com.avlsi.cast2.impl.CastTwoParser;
import com.avlsi.cast2.impl.CastTwoTreeParser;
import com.avlsi.cast2.impl.CastParsingOption;
import com.avlsi.cast2.impl.DumbTwoLexer;
import com.avlsi.cell.CellImpl;


import com.avlsi.util.debug.Debug;

import com.avlsi.cell.CellInterface;

import com.avlsi.util.container.StringContainerIterator;
import com.avlsi.util.container.Pair;
import com.avlsi.util.text.StringUtil;
import com.avlsi.cast2.impl.CastTwoTreeParserTokenTypes;
import com.avlsi.cast2.directive.impl.DirectiveImpl;
import com.avlsi.cast.impl.AmbiguousLookupException;
import com.avlsi.cast.impl.NullEnvironment;
import com.avlsi.cast.impl.SemanticWrapperException;
import com.avlsi.file.common.HierName;


import com.avlsi.io.SearchPath;
import com.avlsi.io.SearchPathFile;
import com.avlsi.io.SearchPathFileFilter;
import com.avlsi.io.SearchPathDirectory;
import com.avlsi.io.FileSearchPath;
import com.avlsi.io.SearchPathFileIterator;

/**
 * This class parses a cast file and returns its exported environment.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class CastParserEnvironment {
    /**
     * Map from canonical file name to imported environment.
     **/
    private final Map importedEnvMap;
    
    /**
     * The search path for cast files.
     **/
    private SearchPath castPath;
    
    /**
     * The environment that is imported by default.  This is
     * passed to the constructor of every ImportEnvironment.
     **/
    private final ImportEnvironment defaultImportEnv;
    
    

    /**
     * The number of anonymous names that have been given out.
     * Used by nextAnonymousName to pick the next name.
     **/
    private int numAnonymousNames = 0;

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
     * Caches the file containing a module or a cell.  For a module, the key is
     * a <code>String</code>.  For a cell, the key is a <code>Pair<code> of
     * module name, and cell name.  The value is the corresponding
     * SearchPathFile.
     **/
    private final Map /*<Object,SearchPathFile>*/ fileForModuleCell;

    /**
     * CAST parsing options to be passed to CastTwoTree.
     **/
    private final CastParsingOption opt;

    /**
     * Number of directive warnings.
     **/
    private int directiveWarnings = 0;

    /**
     * Constructs a CastParserEnvironment with the given search
     * path for cast files.  If the defaultImportDir is specified,
     * that directory will be searched for in castPath.
     * All files in the first occurence of that directory will 
     * be parsed, and added to the default environment.
     *
     * @throws FileNotFoundException if the defaultImportDir
     *   could not be found.
     **/
    public CastParserEnvironment(final SearchPath castPath,
                                 final String defaultImportDir)
        throws IOException,
               RecognitionException,
               TokenStreamException,
               CircularImportException
    {
        this(castPath, defaultImportDir, "1");
    }
    
    /**
     * Constructs a CastParserEnvironment with the given search
     * path for cast files.  If the defaultImportDir is specified,
     * that directory will be searched for in castPath.
     * All files in the first occurence of that directory will 
     * be parsed, and added to the default environment.
     *
     * @throws FileNotFoundException if the defaultImportDir
     *   could not be found.
     **/
    public CastParserEnvironment(final SearchPath castPath,
                                 final String defaultImportDir,
                                 final String castVersion)
        throws IOException,
               RecognitionException,
               TokenStreamException,
               CircularImportException
    {
        this(castPath, defaultImportDir, castVersion, false);
    }
    
    /**
     * Constructs a CastParserEnvironment with the given search
     * path for cast files.  If the defaultImportDir is specified,
     * that directory will be searched for in castPath.
     * All files in the first occurence of that directory will 
     * be parsed, and added to the default environment.
     *
     * @throws FileNotFoundException if the defaultImportDir
     *   could not be found.
     **/
    public CastParserEnvironment(final SearchPath castPath,
                                 final String defaultImportDir,
                                 final String castVersion,
                                 final boolean verbose)
        throws IOException,
               RecognitionException,
               TokenStreamException,
               CircularImportException
    {
        this(castPath,defaultImportDir,castVersion,verbose,
             null);
    }

    public CastParserEnvironment(final SearchPath castPath,
                                 final String defaultImportDir,
                                 final String castVersion,
                                 final boolean verbose,
                                 final Map importedEnvMap)
        throws IOException,
               RecognitionException,
               TokenStreamException,
               CircularImportException
   {
        this(castPath, defaultImportDir, castVersion, verbose, importedEnvMap,
             null);
    }

    public CastParserEnvironment(final SearchPath castPath,
                                 final String defaultImportDir,
                                 final String castVersion,
                                 final boolean verbose,
                                 final Map importedEnvMap,
                                 final CastParsingOption opt)
        throws IOException,
               RecognitionException,
               TokenStreamException,
               CircularImportException
    {
        this.castPath = castPath;
        this.defaultImportEnv = new ImportEnvironment();
        this.castVersion = castVersion;
        this.verbose = verbose;
        this.importedEnvMap = importedEnvMap==null ? 
            new HashMap() : importedEnvMap;
        this.opt = opt;
        this.fileForModuleCell = new HashMap();
        Debug.assertTrue(castVersion.equals("1") || castVersion.equals("2"),
                     "castversion "+castVersion+" not recognized");

        if (defaultImportDir != null) {
            /** @bug jmr parseDirectory ignores SelfImportException **/
            final Environment[] envs
                = parseDirectory(defaultImportDir, new LinkedList());
            for (int i = 0; i < envs.length; ++i)
                defaultImportEnv.addEnvironment(envs[i]);
        }
    }
    
    public final Environment parseFile( final String fileName,
                                        final LinkedList fileList )
        throws IOException,
               RecognitionException,
               TokenStreamException,
               CircularImportException,
               SelfImportException {
        return parseFile( fileName, fileList,
                           new CellImpl( "$env", null,
                                          CellImpl.SYNTHETIC_CELL ) );
    }

    /**
     * If the file has not been parsed, parse it, and add it to 
     * the store of parsed environments.  If it has been
     * parsed, just return the environment.  This handles imports.
     * @review jmr we should differentiate between tree errors 
     *   and errors in parsing / lexing
     * @param fileList  list of files that are currently on the parse
     *   stack.  Used to detect circular and self-importation.
     * @param envCell A CellImpl into which instantiations and nodes found outside
     *                of cell definitions go.
     **/
    public final Environment parseFile(final String filename,
                                       final LinkedList fileList,
                                       final CellImpl envCell )
        throws IOException,
               RecognitionException,
               TokenStreamException,
               CircularImportException,
               SelfImportException
    {
        return parseFile(castPath.findFile(filename), fileList, envCell );
    }

    public final Environment parseFile( final SearchPathFile file,
                                        final LinkedList fileList )
        throws IOException,
               RecognitionException,
               TokenStreamException,
               CircularImportException,
               SelfImportException {
        return parseFile( file, fileList,
                           new CellImpl( "$env", null,
                                          CellImpl.SYNTHETIC_CELL ) );
    }

    public Environment parseFile(final SearchPathFile file,
                                 final LinkedList fileList,
                                 final CellImpl envCell)
        throws IOException,
               RecognitionException,
               TokenStreamException,
               CircularImportException,
               SelfImportException {
       return parseFile(file, fileList, envCell, null);
    }

    /**
     * If the file has not been parsed, parse it, and add it to 
     * the store of parsed environments.  If it has been
     * parsed, just return the environment.  This handles imports.
     * @review jmr we should differentiate between tree errors 
     *   and errors in parsing / lexing
     * @param fileList  list of files that are currently on the parse
     *   stack.  Used to detect circular and self-importation.
     * @param envCell A CellImpl into which instantiations and nodes found outside
     *                of cell definitions go.
     **/
    private Environment parseFile(final SearchPathFile file,
                                 final LinkedList fileList,
                                 final CellImpl envCell,
                                 final String moduleName)
        throws IOException,
               RecognitionException,
               TokenStreamException,
               CircularImportException,
               SelfImportException
    {
        final String canonicalFilename = file.getName();

        final int idx = fileList.indexOf(canonicalFilename);
        if (idx == 0)
            throw new SelfImportException();
        if (idx > 0)
            throw new CircularImportException(
                    canonicalFilename, new ArrayList(fileList));

        synchronized(importedEnvMap) {
            Environment env
                = (Environment) importedEnvMap.get(canonicalFilename);

            if (env == null) {
                final InputStream ins =
                    new BufferedInputStream(file.getStream());
                final AST t = getStreamAST( ins, canonicalFilename);
                ins.close();
                final CastTreeParserInterface ctp = getTreeParser();
                ctp.setCastParserEnvironment(this);

                fileList.addFirst(canonicalFilename);
                env = ctp.goal(t, envCell, moduleName, fileList);
                fileList.removeFirst();

                importedEnvMap.put(canonicalFilename, env);
            }

            return env;
        }
    }

    /**
     * Returns a list of the possible files that a cell could be defined in.
     **/
    public static String[] possibleFilenamesForCell(
            final String moduleName,
            final String cellName) { 

        final String[] components = StringUtil.split(moduleName, '.');

        if (components.length == 1) {
            return new String[] {
                components[0] + ".cast",
                components[0] + File.separatorChar + components[0] + ".cast",
                components[0] + File.separatorChar + cellName + ".cast",
            };
        } else {
            final String fullPath =
                StringUtil.join(components, File.separatorChar);
            final String[] partialPathComponents =
                new String[components.length - 1];
            System.arraycopy(components, 0,
                             partialPathComponents, 0,
                             partialPathComponents.length);
            final String partialPath =
                StringUtil.join(partialPathComponents, File.separatorChar) +
                File.separatorChar + components[components.length - 2] +
                File.separatorChar + components[components.length - 1];
            return new String[] {
                fullPath + ".cast",
                fullPath + File.separatorChar +
                    components[components.length - 1] + ".cast",
                fullPath + File.separatorChar + cellName + ".cast",
                partialPath + ".cast", 
                partialPath + File.separatorChar + cellName + ".cast",
            };
        }
    }

    /**
     * Returns a list of the possible files that a module could be defined in.
     **/
    public String[] possibleFilenamesForModule(final String moduleName) {
        // module t1.t2. ... . tN could be defined in the following files:
        // 1: t1/t2/.../tN-1/tN.cast
        // 2: t1/t2/.../tN-1/tN/tN.cast
        // 3: t1/t2/.../tN-1/tN-1/tN.cast (if N > 1)

        // TODO: factor common code with possibleFilenamesForCell into
        // a private possible prefixesForModule

        final String[] components = StringUtil.split(moduleName, '.');

        if (components.length == 1) {
            return new String[] {
                components[0] + ".cast",
                components[0] + File.separatorChar + components[0] + ".cast",
            };
        } else {
            final String fullPath =
                StringUtil.join(components, File.separatorChar);
            final String[] partialPathComponents =
                new String[components.length - 1];
            System.arraycopy(components, 0,
                             partialPathComponents, 0,
                             partialPathComponents.length);
            final String partialPath =
                StringUtil.join(partialPathComponents, File.separatorChar) +
                File.separatorChar + components[components.length - 2] +
                File.separatorChar + components[components.length - 1];
            return new String[] {
                fullPath + ".cast",
                fullPath + File.separatorChar +
                    components[components.length - 1] + ".cast",
                partialPath + ".cast", 
            };
        }
    }

    public Environment parseFirstFileOf (final String[] fileNames,
                                         final LinkedList fileList,
                                         final String moduleName)
        throws IOException,
               RecognitionException,
               TokenStreamException,
               CircularImportException,
               SelfImportException {
        return parseFirstFileOf(fileNames, fileList,
                new CellImpl("$env", null, CellImpl.SYNTHETIC_CELL),
                moduleName);
    }

    public Environment parseFirstFileOf (final String[] fileNames,
                                         final LinkedList fileList,
                                         final CellImpl envCell,
                                         final String moduleName)
        throws IOException,
               RecognitionException,
               TokenStreamException,
               CircularImportException,
               SelfImportException {
        return parseFile(castPath.findFirstFileOf(fileNames),
                         fileList, envCell, moduleName);
    }

    public Environment parseCell (final LinkedList fileList,
                                  final String moduleName,
                                  final String cellName)
        throws IOException,
               RecognitionException,
               TokenStreamException,
               CircularImportException,
               SelfImportException {
        final Pair p = new Pair(moduleName, cellName);
        SearchPathFile firstFile = (SearchPathFile) fileForModuleCell.get(p);
        if (firstFile == null) {
            final String[] fileNames =
                possibleFilenamesForCell(moduleName, cellName);
            firstFile = castPath.findFirstFileOf(fileNames);
            fileForModuleCell.put(p, firstFile);
        }
        return parseFile(firstFile, fileList,
                         new CellImpl("$env", null, CellImpl.SYNTHETIC_CELL),
                         moduleName);
    }

    public Environment parseModule (final LinkedList fileList,
                                    final String moduleName)
        throws IOException,
               RecognitionException,
               TokenStreamException,
               CircularImportException,
               SelfImportException {
        return parseModule(fileList,
                           new CellImpl("$env", null, CellImpl.SYNTHETIC_CELL),
                           moduleName);
    }

    public Environment parseModule (final LinkedList fileList,
                                    final CellImpl envCell,
                                    final String moduleName)
        throws IOException,
               RecognitionException,
               TokenStreamException,
               CircularImportException,
               SelfImportException {
        SearchPathFile firstFile =
            (SearchPathFile) fileForModuleCell.get(moduleName);
        if (firstFile == null) {
            final String[] fileNames = possibleFilenamesForModule(moduleName);
            firstFile = castPath.findFirstFileOf(fileNames);
            fileForModuleCell.put(moduleName, firstFile);
        }
        return parseFile(firstFile, fileList, envCell, moduleName);
    }

    /**
     * Returns the AST for the given stream.  Does not use the cast path.
     *
     * @param s  input stream to use
     * @param filename  filename to use for error messages.
     *   For informational purposes, not used to open the file.
     **/
    public AST getStreamAST(final InputStream s, final String filename) 
        throws RecognitionException,
               TokenStreamException
    {
        final CastParserInterface parser = getParser(s, filename);

        // start parsing at the program rule
        parser.goal();

        return parser.getAST();
    }

    public AST getInstantiationAST(
            final String cellName,
            final HierName instName) 
        throws RecognitionException,
               TokenStreamException
    {
        final String instantiation =
            cellName + ' ' + instName.getAsString('.');
        final CastParserInterface parser =
            getParser(new ByteArrayInputStream(instantiation.getBytes()),
                    "<auto-generated temporary file>");

        // start parsing at the instantiation rule
        parser.instantiation();

        return parser.getAST();
    }

    private CastParserInterface getParser(final InputStream input,
                                          final String filename) {
        final TokenStreamSelector selector = new TokenStreamSelector();

        // Create a scanner that reads from the input stream passed to us
        final CastLexerInterface castLexer;
        final DumbLexerInterface dumbLexer;
        if (castVersion.equals("1")) {
            castLexer = new CastLexer(input);
            dumbLexer = new DumbLexer(castLexer.getInputState());
        }
        else {
            castLexer = new CastTwoLexer(input);
            dumbLexer = new DumbTwoLexer(castLexer.getInputState());
        }

        castLexer.setFilename(filename);
        castLexer.setTokenObjectClass(TokenWithInfo.class.getName());

        // notify selector about various lexers;
        // name them for convenient reference later
        selector.addInputStream((CharScanner)castLexer, "castlexer");
        selector.addInputStream((CharScanner)dumbLexer, "dumblexer");
        selector.select("castlexer"); // start with main java lexer

        // Create a parser that reads from the scanner
        final CastParserInterface castParser;
        if (castVersion.equals("1")) {
            castParser = new CastParser(selector);
        }
        else {
            castParser = new CastTwoParser(selector);
            ((CastTwoParser) castParser).setVerbosity(verbose);
        }

        castParser.setFilename(filename);
        castParser.setASTNodeClass(ASTWithInfo.class.getName());
        castParser.setSelector(selector);

        return castParser;
    }

    public CastTreeParserInterface getTreeParser() {
        if (castVersion.equals("1")) {
            return new CastTreeParser();
        } else {
            CastTwoTreeParser ctp2 = new CastTwoTreeParser();
            ctp2.setVerbosity(verbose);
            if (opt != null) ctp2.setCastParsingOption(opt);
            ctp2.setDirectiveErrorHandler(
                new DirectiveImpl.ErrorHandler() {
                    public void error(String message) {
                        System.err.println("SYNTAX: " + message);
                        ++directiveWarnings;
                    }
                });
            return ctp2;
        }
    }


    public final Environment[] parseDirectory( final String dirName,
                                               final LinkedList fileList )
        throws IOException,
               RecognitionException,
               TokenStreamException,
               CircularImportException {
        return parseDirectory( dirName, fileList,
                               new CellImpl( "$env", null,
                                             CellImpl.SYNTHETIC_CELL ) );
    }

    /**
     * Looks for the first occurence of directory in the cast path,
     * throwing an exception if it is not found.  Returns an
     * array of Environments generated from
     * all the .cast files in that directory.  Self-imports are ignored.
     * @param fileList  list of files that are currently on the parse
     *   stack.  Used to detect circular and self-importation.
     *
     * @bug jmr parseDirectory ignores SelfImportException
     *   we want things in lib to be able to import lib/*.cast,
     *   without triggering self-import exception,
     *   but if lib/foo.cast imports lib/foo.cast explicitly, that is bad!
     **/
    public Environment[] parseDirectory(final String dirname,
                                        final LinkedList fileList,
                                        final CellImpl topLevelEnv )
        throws IOException,
               RecognitionException,
               TokenStreamException,
               CircularImportException
    {
        final SearchPathDirectory f = castPath.findDirectory(dirname);
        
        final SearchPathFileFilter castFilter = new SearchPathFileFilter() {
                public boolean accept(final SearchPathFile pathname) {
                    return pathname.getName().endsWith(".cast");
                }
            };

        final SearchPathFileIterator fileIter = f.getFiles( castFilter );
        

        final List envList = new ArrayList();
        
        while ( fileIter.hasNext() ) {
            SearchPathFile currFile = fileIter.next();
            try {
                envList.add( parseFile( currFile, fileList, topLevelEnv,
                                        castVersion.equals("1") ? null :
                                        dirname.replace( File.separatorChar,
                                                         '.' ) + '.' +
                                        StringUtil.chompEnd(
                                            StringUtil.chompStart(
                                                currFile.getName(),
                                                f.getName() +
                                                    File.separatorChar),
                                            ".cast" ) ) );
            } catch (SelfImportException e) {
                continue;
            }
        }
	
        return (Environment[]) envList.toArray(new Environment[0]);
    }
    
    /**
     * Returns a new ImportEnvironment, with the default imports
     * already added.
     **/
    public ImportEnvironment newImportEnvironment() {
	return new ImportEnvironment(defaultImportEnv);
    }
    
    
    
    /**
     * Returns a sequence of strings of the form "$0", "$1",
     * "$2", ... .  Increments numAnonymousNames as a side effect.
     **/
    public String nextAnonymousName() {
	return "$" + (numAnonymousNames++);
    }
    

    private static String getModuleNameFromFullyQualifiedName( final String fullyQualifiedCellName ) {

	final int dotIndex = fullyQualifiedCellName.lastIndexOf( '.' );

	if ( dotIndex >= 0 ) {
	    return fullyQualifiedCellName.substring( 0, dotIndex );
	}
	else {
	    return null;
	}
    }

    private static String getCellNameFromFullyQualifiedName( final String fullyQualifiedCellName ) {
	final int dotIndex = fullyQualifiedCellName.lastIndexOf( '.' );

	if ( dotIndex >= 0 ) {
	    return fullyQualifiedCellName.substring( dotIndex + 1 );
	}
	else {
	    return null;
	}
    }
    
    public CellInterface getFullyQualifiedCell( final String fullyQualifiedCellName)
	throws SemanticWrapperException {
	
	return getFullyQualifiedCell( fullyQualifiedCellName, null, null, new LinkedList() ); 
    }

    public CellInterface getFullyQualifiedCell( final String fullyQualifiedCellName, 
						CellImpl parent,
						final HierName instance) 
	    throws SemanticWrapperException {
	return getFullyQualifiedCell(fullyQualifiedCellName, parent, instance,
					new LinkedList());
    }

    public CellInterface getFullyQualifiedCell( final String moduleName,
						final String cellName) 
	throws SemanticWrapperException {
	
	return getFullyQualifiedCell( moduleName, cellName, new LinkedList());
    }

    public CellInterface getFullyQualifiedCell( final String moduleName,
						final String cellName,
						CellImpl parent, 
						final HierName instance)
	    throws SemanticWrapperException {
	return getFullyQualifiedCell( moduleName, cellName, parent,
					instance, new LinkedList());
    }

    public CellInterface getFullyQualifiedCell( final String moduleName,
                                                final String cellName,
						final LinkedList fileList ) 
	    throws SemanticWrapperException {
	return getFullyQualifiedCell( moduleName, cellName, null, null,
					fileList);
    }

    public CellInterface getFullyQualifiedCell( final String moduleName,
						final String cellName,
						CellImpl parent, 
						final HierName inst,
						final LinkedList fileList ) 
	throws SemanticWrapperException {
	
        Debug.assertTrue( ( moduleName.length() > 0 ), "Module name was empty." );

        return getCell(NullEnvironment.getInstance(),
                       moduleName + '.' + cellName,
                       parent, inst);
    }
                         
    public CellInterface getFullyQualifiedCell( final String fullyQualifiedCellName,
						CellImpl parent, 
						final HierName instance,  
						final LinkedList fileList ) 
	throws SemanticWrapperException {
	
	final String cellName = getCellNameFromFullyQualifiedName( fullyQualifiedCellName );
	final String moduleName = getModuleNameFromFullyQualifiedName( fullyQualifiedCellName );
	
        if ( moduleName == null ) {
            final String s = "fully qualified cell name required, found " +
                             fullyQualifiedCellName;
            throw new SemanticWrapperException(s,
                    new RecognitionException(s, "<unknown file>", -1, -1));
        }

	return getFullyQualifiedCell( moduleName, cellName, parent, instance, 
					fileList );
	
    }

    public CellInterface getCell(final Environment env, final String cellName) 
	    throws SemanticWrapperException {
	return getCell(env, cellName, null, null);
    }

    public CellInterface getCell(final Environment env, final String cellName, 
				 CellImpl parent, HierName instName)
        throws SemanticWrapperException {
        return getCell(env, cellName, parent, instName, null);
    }

    /**
     * Get the cell interface for a cell, defined in the specified
     environment.
     **/
    public CellInterface getCell(final Environment env, final String cellName, 
				 CellImpl parent, HierName instName, CellInterface envContainer)
        throws SemanticWrapperException {


        try {
            // parent and instName can be null.  I'm not sure if this
            // is the best way to solve it, but it seems to work --jmr
            if (parent == null)
                parent = new CellImpl("$fakeEnv", null,
                                      CellImpl.SYNTHETIC_CELL);
            if (instName == null)
                instName = HierName.makeHierName("__");

            final AST ast = getInstantiationAST(cellName, instName);

            final CastTreeParserInterface parser = getTreeParser();
            parser.setCastParserEnvironment(this);

            parser.doInstantiation(ast,
                    getModuleNameFromFullyQualifiedName(cellName),
                    env, parent, envContainer);

            return parent.getSubcell(instName);
        } catch (RecognitionException e) {
            throw new SemanticWrapperException("trouble getting a cell", e);
        } catch (TokenStreamException e) {
            throw new SemanticWrapperException("trouble getting a cell", e);
        } 
    }

    public boolean searchPathContains(final String directoryName) {
        return castPath.containsDirectory(directoryName);
    }

    public void clearDirectiveWarnings() {
        directiveWarnings = 0;
    }

    public int getDirectiveWarnings() {
        return directiveWarnings;
    }
}
