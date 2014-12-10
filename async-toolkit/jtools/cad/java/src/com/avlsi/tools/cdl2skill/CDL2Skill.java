/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cdl2skill;


import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.Writer;
import java.io.OutputStream;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.BufferedReader;
import java.io.BufferedWriter;



import java.util.Set;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.LinkedList;
import java.util.Map;
import java.util.HashMap;

import com.avlsi.file.common.HierName;
import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;

import com.avlsi.file.cdl.parser.ReadCDLIntoFactory;
import com.avlsi.file.cdl.parser.CDLFactoryInterface;
import com.avlsi.file.cdl.parser.Template;
import com.avlsi.file.cdl.parser.SplittingFactory;

import com.avlsi.tools.cdl2skill.CDL2SkillFactory;
import com.avlsi.tools.jauto.TechnologyData;

public class CDL2Skill {

    private static void usage( ) {

        final String className = CDL2Skill.class.getName();
        
        System.out.println( "Usage: " + 
                            System.getProperty( "java.home" ) +
                            System.getProperty( "file.separator" ) +
                            "bin" +
                            System.getProperty( "file.separator" ) +
                            "java " +
                            " -classpath " +
                            System.getProperty( "java.class.path" ) + " " +
                            className + "\n" +
                            "    --cdl-file=file\n" +
                            "    --output-dir=dir\n" +
                            "    [ --library-list=file ]\n" +
                            "    [ --cell-list=file ]\n" );
    }
   
    public static void main( String[] args ) throws Exception {
    

        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
        final CommandLineArgs argsWithConfigs =
	    new CommandLineArgsWithConfigFiles( parsedArgs ); 

        final CommandLineArgs cachedArgs = 
	    new CachingCommandLineArgs( argsWithConfigs );

        final CommandLineArgs theArgs = cachedArgs;

        final String cdlFileName = theArgs.getArgValue( "cdl-file", null );
        final String outputDirName = theArgs.getArgValue( "output-dir", null );
        final String libListFileName = theArgs.getArgValue( "library-list", null );
        final String cellListFileName = theArgs.getArgValue( "cell-list", null );

        if (theArgs.argExists("version")) {
            System.out.println(
                com.avlsi.util.debug.VersionInfo.getVersionString(
                    CDL2Skill.class));
        }

        final TechnologyData techData;
        try {
            techData = TechnologyData.getTechnologyData(theArgs);
        } catch (Exception e) {
            System.err.println("Can't load technology data: " + e.getMessage());
            System.exit(1);
        }


        if ( ( cdlFileName != null ) &&
             ( outputDirName != null )
             ) {
            
            final File cdlFile = new File( cdlFileName );
            final File outputDir = new File( outputDirName );

            if ( ! ( outputDir.exists() ) ) {
                outputDir.mkdirs();
            }

            if ( ( cdlFile.isFile() ) &&
                 ( cdlFile.canRead() ) &&
                 ( outputDir.isDirectory() ) &&
                 ( outputDir.canWrite() ) ) {
                
                final InputStream cdlInputStream = 
                    new FileInputStream( cdlFile );
                final Reader cdlReader = 
                    new BufferedReader( new InputStreamReader( cdlInputStream, "UTF-8" ) );

                final Map cellTemplateMap = new HashMap();
                final Template templateFactory = new Template( cellTemplateMap, cdlFileName );


                final Map cellStatMap = new HashMap();
                final Set libSet = new HashSet();
                final List cellList = new LinkedList();
                final CDL2SkillFactory skillFactory = 
                    new CDL2SkillFactory( outputDir,
                                          "NetListTable",
                                          libSet,
                                          cellList,
                                          cellTemplateMap,
                                          cellStatMap,
                                          techData );
                
                final CDLFactoryInterface factory = new SplittingFactory( templateFactory,
                                                                          skillFactory );

                ReadCDLIntoFactory.readCDL( cdlReader, factory );

                if ( skillFactory.getError() != null ) {
                    throw skillFactory.getError();
                }

                if ( libListFileName != null ) {
                    final OutputStream outStream = new FileOutputStream( libListFileName );
                    final Writer outWriter = 
                        new BufferedWriter( new OutputStreamWriter( outStream, "UTF-8" ) );
                    
                    final Iterator i = libSet.iterator();
                    while ( i.hasNext() ) {
                        final String currLibName = ( String ) i.next();
                        outWriter.write( currLibName + "\n" );
                    }
                    outWriter.close();
                }

                if ( cellListFileName != null ) {
                    final OutputStream outStream = new FileOutputStream( cellListFileName );
                    final Writer outWriter = 
                        new BufferedWriter( new OutputStreamWriter( outStream, "UTF-8" ) );
                    
                    final Iterator i = cellList.iterator();
                    while ( i.hasNext() ) {
                        final String currCellName = ( String ) i.next();
                        outWriter.write( currCellName + "\n" );
                    }
                    outWriter.close();
                }
                
            }

        }
        else {
            if ( cdlFileName == null ) {
                System.out.println( "You must specify a cdl file to convert to skill." );
            }
            if ( outputDirName == null ) {
                System.out.println( "You must specify an output directory." );
            }
            usage();
        }

    }
    

}
