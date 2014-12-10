/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.file.cdl.util.rename;


import java.util.Set;
import java.util.Iterator;
import java.util.Map;

import java.io.Writer;
import java.io.Reader;
import java.io.LineNumberReader;
import java.io.IOException;

import com.avlsi.util.text.StringUtil;

import com.avlsi.file.cdl.util.rename.CachingNameInterface;

public class ReloadableNameInterface extends CachingNameInterface {

    public class FileFormatException extends Exception {
        FileFormatException( final String errorStr ) {
            super( errorStr );
        }
    }

    public ReloadableNameInterface( final CDLNameInterface renameInterface ) {
        super( renameInterface );
    }

    private void throwFileFormatException( final String line,
                                           final LineNumberReader r ) 
    throws FileFormatException 
    {
        final String errorStr =
            "Invalid mapping \"" +
            line +
            "\" on line " +
            Integer.toString( r.getLineNumber() ) +
            ".";
        throw new FileFormatException( errorStr );
    }

    public void load( Reader r ) 
        throws IOException, FileFormatException
    {    
        final LineNumberReader myReader = new LineNumberReader( r );
        
        String currLine;
        
        do {
            currLine = myReader.readLine();
            
            if ( currLine != null ) {
                
                final String[] lineComponents = StringUtil.tokenize( currLine );
                
                if ( lineComponents.length > 0 ) {
                    if ( lineComponents[0].charAt( 0 ) != '#' ) {
                        if ( lineComponents.length == 3 ) {
                            final String mappingType =
                                lineComponents[0];
                            final String fromName = lineComponents[1];
                            final String toName = lineComponents[2];
                            
                            if ( mappingType.equals( "cell" ) ) {
                                addCellNameMapping( fromName,
                                                    toName );
                            }
                            else{
                                if ( mappingType.equals( "node" ) ) {
                               
                                    addNodeNameMapping( fromName,
                                                        toName );
                                }
                                else if ( mappingType.equals( "device" ) ) {
                                    addDeviceNameMapping( fromName,
                                                          toName );
                                }
                                else if ( mappingType.equals( "instance" ) ) {
                                    addSubCellInstanceNameMapping( fromName,
                                                                    toName );
                                }
                                else if ( mappingType.equals( "transistor" ) ) {
                                    addTransistorModelNameMapping( fromName,
                                                                    toName );
                                }
                                else {
                                    throwFileFormatException( currLine, myReader );
                                }
                            }
                        }
                        else {
                            throwFileFormatException( currLine, myReader );
                        }
                    }
                }
            }
        } while ( currLine != null );
    }

    public void save( Writer w )
        throws IOException
    {
    
        final Set cellNameMappings = getCellNameMappings();
        final Iterator cellNameIter = cellNameMappings.iterator();

        while ( cellNameIter.hasNext() ) {
            final Map.Entry currMapping = ( Map.Entry ) cellNameIter.next();
            final String fromName = ( String ) currMapping.getKey();
            final String toName = ( String ) currMapping.getValue();
            if ( ! ( fromName.equals( toName ) ) ) {
                final String outputStr = 
                    "cell " +
                    fromName +
                    ' ' +
                    toName +
                    '\n';
                w.write( outputStr );
            }
        }

        final Set nodeNameMappings = getNodeNameMappings();
        final Iterator nodeNameIter = nodeNameMappings.iterator();
        
        while ( nodeNameIter.hasNext() ) {
            final Map.Entry currMapping = ( Map.Entry ) nodeNameIter.next();
            final String fromName = ( String ) currMapping.getKey();
            final String toName = ( String ) currMapping.getValue();
            if ( ! ( fromName.toString().equals( toName ) ) ) {
                final String outputStr = 
                    "node " +
                    fromName.toString() +
                    ' ' +
                    toName.toString() +
                    '\n';
                w.write( outputStr );
            }
        }

        final Set deviceNameMappings = getDeviceNameMappings();
        final Iterator deviceNameIter = deviceNameMappings.iterator();

        while ( deviceNameIter.hasNext() ) {
            final Map.Entry currMapping = ( Map.Entry ) deviceNameIter.next();
            final String fromName = ( String ) currMapping.getKey();
            final String toName = ( String ) currMapping.getValue();
            if ( ! ( fromName.toString().equals( toName ) ) ) {
                final String outputStr = 
                    "device " +
                    fromName.toString() +
                    ' ' +
                    toName +
                    '\n';
                w.write( outputStr );
            }
        }

        final Set subCellInstanceNameMappings = getSubCellInstanceNameMappings();
        final Iterator subCellInstanceNameIter = subCellInstanceNameMappings.iterator();
        
        while ( subCellInstanceNameIter.hasNext() ) {
            final Map.Entry currMapping = ( Map.Entry ) subCellInstanceNameIter.next();
            final String fromName = ( String ) currMapping.getKey();
            final String toName = ( String ) currMapping.getValue();
            if ( ! ( fromName.toString().equals( toName ) ) ) {
                final String outputStr = 
                    "instance " +
                    fromName.toString() +
                    ' ' +
                    toName +
                    '\n';
                w.write( outputStr );
            }
        }
        w.flush();
    }
}
