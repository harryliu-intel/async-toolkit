/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */
package com.avlsi.file.cdl.util.rename;


import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

import java.io.Writer;
import java.io.Reader;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.OutputStream;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.InputStreamReader;
import java.io.FileOutputStream;
import java.io.FileInputStream;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;


import com.avlsi.file.cdl.parser.ReadCDLIntoFactory;

import com.avlsi.file.cdl.util.rename.CDLRenamerFactory;
import com.avlsi.file.cdl.util.rename.GDS2NameInterface;
import com.avlsi.file.cdl.util.rename.CadenceNameInterface;
import com.avlsi.file.cdl.util.rename.ReloadableNameInterface;
import com.avlsi.file.cdl.util.rename.CDLRenameException;
import com.avlsi.file.cdl.util.rename.PMCHackNameInterface;

import com.avlsi.io.NullWriter;

import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.cmdlineargs.defimpl.PedanticCommandLineArgs;

/**
 * An CDLFactoryInterface that can be used to rename nodes and names of circuit
 * components to another format
 **/
public class CDLRenamer  {
    public interface StartObserver {
        void renameStart() throws CDLRenameException;
    }
    public interface EndObserver {
        void renameEnd() throws CDLRenameException;
    }
    private static class CompositeInterface extends CompositeCDLNameInterface
        implements StartObserver, EndObserver {
        public CompositeInterface(final CDLNameInterface f,
                                  final CDLNameInterface g) {
            super(f, g);
        }
        public void renameStart() throws CDLRenameException {
            if (f instanceof StartObserver) {
                ((StartObserver) f).renameStart();
            }
            if (g instanceof StartObserver) {
                ((StartObserver) g).renameStart();
            }
        }
        public void renameEnd() throws CDLRenameException {
            if (f instanceof EndObserver) {
                ((EndObserver) f).renameEnd();
            }
            if (g instanceof EndObserver) {
                ((EndObserver) g).renameEnd();
            }
        }
    }

    private static void usage( String m ) {

        System.err.println( "Usage: cdl_renamer\n" + 
            "   --source-cdl-file=file\n" +
            "   --name-in=cast|gds2|cadence|pmc_hack|rcx_hack\n" + 
            "   --name-out=cast|gds2|cadence|pmc_hack|rcx_hack\n" + 
            "   --translated-cdl=file\n" +
            "   [--rcx-cell-map=file]\n" +
            "   [--rcx-pipo-map=file]\n" +
            "   [--translated-nmap=file]\n" +
            "   (only understands conditionals and loops if nmap specified)\n" );
        if (m != null && m.length() > 0)
            System.err.print( m );
        System.exit(1);
    }

    private static void usage() {
        usage( null );
    }

    private static void usagej() {

        final String className = CDLRenamer.class.getName();
        
        System.out.println( "Usage: " + 
                            System.getProperty( "java.home" ) +
                            System.getProperty( "file.separator" ) +
                            "bin" +
                            System.getProperty( "file.separator" ) +
                            "java " +
                            " -classpath " +
                            System.getProperty( "java.class.path" ) + " " +
                            className + "\n" +
                            "   --source-cdl-file=file\n" +
                            "   --name-in=cast|gds2|cadence|pmc_hack|rcx_hack\n" + 
                            "   --name-out=cast|gds2|cadence|pmc_hack|rcx_hack\n" + 
                            "   --translated-cdl=file\n" +
                            "   [--rcx-cell-map=file]\n" +
                            "   [--rcx-pipo-map=file]\n" +
                            "   [--translated-nmap=file]\n" +
                            "   (only understands conditionals and loops if nmap specified)\n" );
    }

    public static Writer openOutputFile( final String fileName ) 
        throws IOException 
    {
        final Writer ret;
        
        final OutputStream outputStream =
            new FileOutputStream( fileName );
        
        ret  = new BufferedWriter( new OutputStreamWriter( outputStream ) );
        
        return ret;
    }

    public static void main(String[] args) 
        throws Exception 
    {
        final CommandLineArgs parsedArgs = 
            new CommandLineArgsDefImpl( args );
        final CommandLineArgs argsWithConfigs =
	    new CommandLineArgsWithConfigFiles( parsedArgs ); 
        final CommandLineArgs cachedArgs =
	    new CachingCommandLineArgs( argsWithConfigs );    
        final PedanticCommandLineArgs pedanticArgs =
	    new PedanticCommandLineArgs( cachedArgs );    
        final CommandLineArgs theArgs = pedanticArgs;

        final String nameIn = 
            theArgs.getArgValue( "name-in", null );

        final String nameOut = 
            theArgs.getArgValue( "name-out", null );

        final String translatedCDLFileName = 
            theArgs.getArgValue( "translated-cdl", null );

        final String nameMapFileName = 
            theArgs.getArgValue( "translated-nmap", null );

        final String sourceCDLFileName = 
            theArgs.getArgValue( "source-cdl-file", null );

        pedanticArgs.argTag("rcx-cell-map");
        pedanticArgs.argTag("rcx-pipo-map");

        if ( ! pedanticArgs.pedanticOK( false, true ) ) {
            usage( pedanticArgs.pedanticString() );
        }

        final List outputFileNames = new ArrayList( 2 );
        
        if ( ( sourceCDLFileName != null ) &&
             ( translatedCDLFileName != null ) &&
             ( nameIn != null ) && 
             ( nameOut != null ) ) {
            final File sourceCDLFile = new File( sourceCDLFileName );
            
            if ( ( sourceCDLFile.isFile() ) && ( sourceCDLFile.canRead() ) ) {
                
                final InputStream sourceCDLInputStream =
                    new FileInputStream( sourceCDLFile );

                final Reader sourceCDLReader =
                    new BufferedReader(
                        new InputStreamReader( sourceCDLInputStream ) );

                final CDLRenamerFactory cdlFactory =
                    new CDLRenamerFactory( );


                outputFileNames.add( translatedCDLFileName );

                final Writer cdlWriter = 
                    openOutputFile( translatedCDLFileName ); 

                CDLNameInterface ni;

                if( nameIn.equals(nameOut) ) {
                    ni = new IdentityNameInterface();
                }
                else {
                    final CDLNameInterface f,g;
                        
                    if( nameIn.equals( "cast" ) ||
                        nameOut.equals( "pmc_hack" ) ) {
                        f = new IdentityNameInterface();
                    }
                    else if( nameIn.equals( "cadence" ) ) {
                        f = new CadenceReverseNameInterface();
                    }
                    else if ( nameIn.equals( "gds2" ) ) {
                        f = new GDS2ReverseNameInterface();
                    }
                    else if ( nameIn.equals( "pmc_hack" ) ) {
                        f = PMCHackNameInterface.getReverseNamer();
                    }
                    else if ( nameIn.equals( "rcx_hack" ) ) {
                        final String cellMapFile =
                            theArgs.getArgValue("rcx-cell-map", null);
                        if (cellMapFile == null) {
                            throw new CDLRenameException(
                                "rcx_hack naming requires valid " +
                                "--rcx-cell-map cell name mapping file" );
                        }
                        final Reader r = new FileReader(cellMapFile);
                        f = RCXHackNameInterface.getReverseNamer(r);
                    }
                    else 
                        throw new CDLRenameException("Invalid translation: " +
                                                     nameIn + " -> " + nameOut );
                        
                    if( nameOut.equals( "cast" ) ||
                        nameIn.equals( "pmc_hack" ) ) {
                        g = new IdentityNameInterface();
                    }
                    else if( nameOut.equals( "cadence" ) ) {
                        g = new CadenceNameInterface();
                    }
                    else if( nameOut.equals( "gds2" ) ) {
                        g = new GDS2NameInterface();
                    }
                    else if( nameOut.equals( "pmc_hack" ) ) {
                        g = PMCHackNameInterface.getForwardNamer();
                    }
                    else if ( nameOut.equals( "rcx_hack" ) ) {
                        final String cellMapFile =
                            theArgs.getArgValue("rcx-cell-map", null);
                        final String pipoMapFile =
                            theArgs.getArgValue("rcx-pipo-map", null);
                        if (cellMapFile == null) {
                            throw new CDLRenameException(
                                "rcx_hack naming requires valid " +
                                "--rcx-cell-map cell name mapping file" );
                        }
                        final Writer cellWriter = new FileWriter(cellMapFile);
                        final Writer pipoWriter = pipoMapFile == null ?
                            (Writer) NullWriter.getInstance() :
                            (Writer) new FileWriter(pipoMapFile);
                        g = RCXHackNameInterface.getForwardNamer(
                                60, cellWriter, pipoWriter);
                    }
                    else 
                        throw new CDLRenameException("Invalid translation: " +
                                                     nameIn + " -> " + nameOut );
                    ni = new CompositeInterface(f,g);
                }
                final CDLNameInterface nameInterface = ni;             
                try {                
                    if (nameInterface instanceof StartObserver) {
                        ((StartObserver) nameInterface).renameStart();
                    }
                    if( nameMapFileName != null ) {
                        final ReloadableNameInterface reloadableNameInterface =
                            new ReloadableNameInterface( nameInterface );
                            
                        cdlFactory.addNameInterface( cdlWriter,
                                                     reloadableNameInterface,
                                                     76 );

                        ReadCDLIntoFactory.readCDL( sourceCDLReader,
                                                    cdlFactory );
                        
                        outputFileNames.add(nameMapFileName);

                        final Writer nameMapWriter =
                            openOutputFile( nameMapFileName );

                        reloadableNameInterface.save( nameMapWriter );
                            
                        nameMapWriter.close();
                    }
                    else {                            
                        cdlFactory.addNameInterface( cdlWriter,
                                                     nameInterface,
                                                     76 );                            
                        ReadCDLIntoFactory.readCDLSimple( sourceCDLReader,
                                                          cdlFactory );
                    }
                    if (nameInterface instanceof EndObserver) {
                        ((EndObserver) nameInterface).renameEnd();
                    }
                    cdlFactory.closeOutputs();
                    if ( cdlFactory.haveError() ){
                        throw cdlFactory.getError();
                    }
                }
                catch ( Exception e ) {
                    final Iterator outputFileNameIter = outputFileNames.iterator();
                        
                    cdlFactory.closeOutputs();
                        
                    while ( outputFileNameIter.hasNext() ) { 
                            
                        final String currOutputFileName = ( String ) outputFileNameIter.next();
                            
                        final File currOutputFile = new File( currOutputFileName );
                            
                        System.out.println( "deleting " + currOutputFileName + "." );
                            
                        if ( currOutputFile.isFile() ) {
                            currOutputFile.delete();
                        }
                    }
                    throw e;
                }
            }
            else {
                System.out.println( "\"" +
                                    sourceCDLFileName +
                                    "\" is not a readable file." );
            }
        }
        else {
            if ( sourceCDLFileName == null ) {
                System.out.println( "You must specify a source cdl file." );
            }
            usage();
        }
    }
}
