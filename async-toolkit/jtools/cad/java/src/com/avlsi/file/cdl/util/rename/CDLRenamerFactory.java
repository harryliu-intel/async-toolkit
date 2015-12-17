/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.cdl.util.rename;


import java.io.Writer;
import java.io.IOException;

import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;

import com.avlsi.cast.impl.Environment;

import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;

import com.avlsi.file.cdl.parser.CDLLexer;
import static com.avlsi.file.cdl.parser.CDLLexer.MODEL_PARAMETER;
import com.avlsi.file.cdl.parser.CDLFactoryInterface;

import com.avlsi.file.cdl.util.rename.CDLRenameException;
import com.avlsi.file.cdl.util.rename.CDLNameInterface;

/**
 * An CDLFactoryInterface that can be used to rename nodes and names of circuit
 * components to another format
 *
 * TODO: reimplement as CDLRenameFactory + CDLFactoryEmitter
 **/
public class CDLRenamerFactory implements CDLFactoryInterface {
   

    private static final class OutputInfo {
        private final Writer m_CDLOutput;

        private final int m_MaxLineSize;
        private int m_CurrLineSize;
        private final String m_callDelimiter;

        private final CDLNameInterface m_RenameInterface;

        public OutputInfo( final Writer cdlOutput,
                           final CDLNameInterface renameInterface,
                           final int maxLineSize,
                           final String callDelimiter ) {
            m_CDLOutput = cdlOutput;
            m_RenameInterface = renameInterface;
            m_CurrLineSize = 0;
            m_MaxLineSize = maxLineSize;
            m_callDelimiter = callDelimiter;
        }

        public void print( final StringBuffer s ) throws IOException {
            print( s.toString() );
        }

        public void print( final String s ) throws IOException {
            m_CDLOutput.write( s );
            m_CurrLineSize += s.length();
        }

        public void printws( final StringBuffer s ) throws IOException {
            printws( s.toString() );
        }

        public void printws( final String s ) throws IOException {
            if ( ( m_CurrLineSize + 1 + s.length() ) > m_MaxLineSize ) {
                println();
                print( "+" );
            }
            else {
                m_CDLOutput.write( ' ' );
            }
            m_CDLOutput.write( s );
            m_CurrLineSize += s.length() + 1;
        }

        public void printCallDelimiter( ) throws IOException {
            if ( !m_callDelimiter.equals("") ) {
                printws( m_callDelimiter );
            }
        }

        public void println( ) throws IOException {
            m_CurrLineSize = 0;
            m_CDLOutput.write( '\n' );
        }

        public void flush() throws IOException {
            m_CDLOutput.flush();
        }

        public Writer getWriter( ) {
            return m_CDLOutput;
        }

        public CDLNameInterface getNameInterface() {
            return m_RenameInterface;
        }

        public String renameCell( final String oldCellName ) 
            throws CDLRenameException
        {
            return getNameInterface().renameCell( oldCellName );
        }

        public String renameNode( final HierName oldNodeName ) 
            throws CDLRenameException
        {
            return getNameInterface().renameNode(oldNodeName.getAsString('.'));
        }

        public String renameDevice( final HierName oldDeviceName ) 
        throws CDLRenameException
        {
            return getNameInterface().renameDevice( oldDeviceName.getAsString('.') );
        }

        public String renameSubCellInstance( final HierName oldInstanceName ) 
            throws CDLRenameException
        {
            return getNameInterface().renameSubCellInstance( oldInstanceName.getAsString('.') );
        }

        public String renameTransistorModel( final String oldModelName )  
            throws CDLRenameException
        {
            return getNameInterface().renameTransistorModel( oldModelName );
        }

        public void close() throws IOException {
            m_CDLOutput.close();
        }
    }

    final Collection m_OutputInfos;
    Exception m_Error;

    
    public CDLRenamerFactory( ) {
        m_OutputInfos = new LinkedList();
        m_Error = null;
        
    }

    

    public void addNameInterface( final Writer cdlOutput,
                                  final CDLNameInterface nameInterface,
                                  final int maxLineSize,
                                  final String callDelimiter ) {
        final OutputInfo newInfo = new OutputInfo( cdlOutput,
                                                   nameInterface,
                                                   maxLineSize,
                                                   callDelimiter );
        m_OutputInfos.add( newInfo );
    }

    

    private void writeParameters( final Map parameters,
                                  final Set exclude,
                                  final Environment env,
                                  final OutputInfo output ) throws IOException {
        final StringBuffer accumulator = new StringBuffer();
        final Iterator i = parameters.entrySet().iterator();
        while ( i.hasNext() ) {
            
            final Map.Entry entry = ( Map.Entry ) i.next();
            final String key = ( String ) entry.getKey();
            if ( exclude.contains( key ) ) continue;

            final CDLLexer.InfoToken val = ( CDLLexer.InfoToken ) entry.getValue();
            final String valStr = val.getSpiceString( env );
            
            output.printws( key + "=" + valStr );
            
        }
    }

    private void writeParameters( final Map parameters,
                                  final Environment env,
                                  final OutputInfo output ) throws IOException {
        writeParameters( parameters, Collections.emptySet(), env, output );
    }

    public void makeResistor( HierName name,
                              HierName n1,
                              HierName n2,
                              CDLLexer.InfoToken val,
                              Map parameters,
                              Environment env ) {

        if ( m_Error == null ) {
            try {
                final Iterator outputInfoIter =
                    m_OutputInfos.iterator();
            
                while ( outputInfoIter.hasNext() ) {
                    final OutputInfo currInfo = ( OutputInfo ) outputInfoIter.next();
                    
                    currInfo.print( "R" + currInfo.renameDevice( name ) );
                    currInfo.printws( currInfo.renameNode( n1 ) );
                    currInfo.printws( currInfo.renameNode( n2 ) );
                    final CDLLexer.InfoToken mname = ( CDLLexer.InfoToken )
                        parameters.get( MODEL_PARAMETER );
                    if (mname != null) currInfo.printws( mname.getText() );
                    if (val != null) currInfo.printws( val.getText( env ) );
                    writeParameters( parameters,
                                     Collections.singleton(MODEL_PARAMETER),
                                     env, currInfo );
                    currInfo.println();
                }
            }
            catch( IOException e ) {
                m_Error = e;
            }
            catch( CDLRenameException e ) {
                m_Error = e;
            }
        }
    }

    public void makeCapacitor(HierName name,
                              HierName npos,
                              HierName nneg,
                              CDLLexer.InfoToken val,
                              Map parameters,
                              Environment env) {
        if ( m_Error == null ) {
            try {
                final Iterator outputInfoIter =
                    m_OutputInfos.iterator();
                while ( outputInfoIter.hasNext() ) {
                    final OutputInfo currInfo = ( OutputInfo ) outputInfoIter.next();
                    

                    currInfo.print( "C" + currInfo.renameDevice( name ) );
                    currInfo.printws( currInfo.renameNode( npos ) );
                    currInfo.printws( currInfo.renameNode( nneg ) );
                    final CDLLexer.InfoToken mname = ( CDLLexer.InfoToken )
                        parameters.get( MODEL_PARAMETER );
                    if (mname != null) currInfo.printws( mname.getText() );
                    currInfo.printws( val.getText( env ) );
                    writeParameters( parameters,
                                     Collections.singleton(MODEL_PARAMETER),
                                     env, currInfo );
                    currInfo.println();

                }
            }
            catch( IOException e ) {
                m_Error = e;
            }
            catch( CDLRenameException e ) {
                m_Error = e;
            }
        }
    }

    public void makeTransistor(HierName name,
                               String type,
                               HierName ns,
                               HierName nd,
                               HierName ng,
                               HierName nb,
                               CDLLexer.InfoToken width,
                               CDLLexer.InfoToken length,
                               Map parameters,
                               Environment env) {
        if ( m_Error == null ) {
            try {
                final Iterator outputInfoIter =
                    m_OutputInfos.iterator();
                while ( outputInfoIter.hasNext() ) {
                    final OutputInfo currInfo = ( OutputInfo ) outputInfoIter.next();
                    

                    currInfo.print( "M" + currInfo.renameDevice( name ) );
                    currInfo.printws( currInfo.renameNode( nd ) );
                    currInfo.printws( currInfo.renameNode( ng ) );
                    currInfo.printws( currInfo.renameNode( ns ) );
                    currInfo.printws( currInfo.renameNode( nb ) );
                    currInfo.printws( currInfo.renameTransistorModel( type ) );
                    currInfo.printws( "w=" + width.getText( env ) );
                    currInfo.printws( "l=" + length.getText( env ) );
                    writeParameters( parameters, env, currInfo );
                    currInfo.println();

                }
            }
            catch( IOException e ) {
                m_Error = e;
            }
            catch( CDLRenameException e ) {
                m_Error = e;
            }
        }
    }

    public void makeDiode(HierName name,
                          String type,
                          HierName npos,
                          HierName nneg,
                          CDLLexer.InfoToken val,
                          Map parameters,
                          Environment env) {
        if ( m_Error == null ) {
            try {
                final Iterator outputInfoIter =
                    m_OutputInfos.iterator();
                while ( outputInfoIter.hasNext() ) {
                    final OutputInfo currInfo = ( OutputInfo ) outputInfoIter.next();
                    
                    currInfo.print( "D" + currInfo.renameDevice( name ) );
                    currInfo.printws( currInfo.renameNode( npos ) );
                    currInfo.printws( currInfo.renameNode( nneg ) );
                    currInfo.printws( type );
                    currInfo.printws( val.getText( env ) );
                    writeParameters( parameters, env, currInfo );
                    currInfo.println();
                }
            }
            catch( IOException e ) {
                m_Error = e;
            }
            catch( CDLRenameException e ) {
                m_Error = e;
            } 
        }
    }

    public void makeInductor(HierName name,
                             HierName npos,
                             HierName nneg,
                             CDLLexer.InfoToken val,
                             Map parameters,
                             Environment env) {
        if ( m_Error == null ) {
            try {
                final Iterator outputInfoIter =
                    m_OutputInfos.iterator();
                while ( outputInfoIter.hasNext() ) {
                    final OutputInfo currInfo = ( OutputInfo ) outputInfoIter.next();
                    
                    currInfo.print( "L" + currInfo.renameDevice( name ) );
                    currInfo.printws( currInfo.renameNode( npos ) );
                    currInfo.printws( currInfo.renameNode( nneg ) );
                    currInfo.printws( val.getText( env ) );
                    writeParameters( parameters, env, currInfo );
                    currInfo.println();

                }
            }
            catch( IOException e ) {
                m_Error = e;
            }
            catch( CDLRenameException e ) {
                m_Error = e;
            } 
        }
    }

    public void makeBipolar(HierName name,
                            String type,
                            HierName nc,
                            HierName nb,
                            HierName ne,
                            CDLLexer.InfoToken val,
                            Map parameters,
                            Environment env) {
        if ( m_Error == null ) {
            try {
                final Iterator outputInfoIter =
                    m_OutputInfos.iterator();
                while ( outputInfoIter.hasNext() ) {
                    final OutputInfo currInfo = ( OutputInfo ) outputInfoIter.next();
                    
                    currInfo.print( "Q" + currInfo.renameDevice( name ) );
                    currInfo.printws( currInfo.renameNode( nc ) );
                    currInfo.printws( currInfo.renameNode( nb ) );
                    currInfo.printws( currInfo.renameNode( ne ) );
                    currInfo.printws( type );
                    currInfo.printws( "area=" + val.getText( env ) );
                    writeParameters( parameters, env, currInfo );
                    currInfo.println();
                }
            }
            catch( IOException e ) {
                m_Error = e;
            }
            catch( CDLRenameException e ) {
                m_Error = e;
            } 
        }
    }

    public void makeCall(HierName name, String subName, HierName[] args,
                         Map parameters, Environment env) {
        if ( m_Error == null ) {
            try {
                final Iterator outputInfoIter =
                    m_OutputInfos.iterator();
                while ( outputInfoIter.hasNext() ) {
                    final OutputInfo currInfo = ( OutputInfo ) outputInfoIter.next();
                    
                    currInfo.print( "X" + currInfo.renameSubCellInstance( name ) );
                    
                    for ( int i = 0 ; i < args.length ; ++i ) {
                        currInfo.printws( currInfo.renameNode( args[i] ) );
                    }
                    currInfo.printCallDelimiter();
                    currInfo.printws( currInfo.renameCell( subName ) );
                    writeParameters( parameters, env, currInfo );
                    currInfo.println();
                    
                }
            }
            catch( IOException e ) {
                m_Error = e;
            }
            catch( CDLRenameException e ) {
                m_Error = e;
            }     
        }
    }

    public void beginSubcircuit(String subName, String[] in, String[] out,
                                Map parameters, Environment env) {
        if ( m_Error == null ) {
            try {
                final Iterator outputInfoIter =
                    m_OutputInfos.iterator();
                while ( outputInfoIter.hasNext() ) {
                    final OutputInfo currInfo = ( OutputInfo ) outputInfoIter.next();
                    
                    currInfo.print( ".SUBCKT " + currInfo.renameCell( subName ) );
                    
                    for ( int i = 0; i < in.length; ++i ) {
                        final HierName currHierName =
                            HierName.makeHierName( in[i], '.' );
                        currInfo.printws( currInfo.renameNode( currHierName ) );
                    }
                    
                    for ( int i = 0; i < out.length; ++i ) {
                        final HierName currHierName =
                            HierName.makeHierName( out[i], '.' );
                        currInfo.printws( currInfo.renameNode( currHierName ) );
                        
                    }
                    writeParameters( parameters, env, currInfo );
                    currInfo.println();
                }
            }
            catch( InvalidHierNameException e ) {
                m_Error = e;
            }
            catch( IOException e ) {
                m_Error = e;
            }
            catch( CDLRenameException e ) {
                m_Error = e;
            }
        }
    }

    /**
     * Called by the parser after processing a subcircuit.
     * @param subName Name of the subcircuit
     **/
    public void endSubcircuit(String subName, Environment env) {
        if ( m_Error == null ) {
            try {
                final Iterator outputInfoIter =
                    m_OutputInfos.iterator();
                while ( outputInfoIter.hasNext() ) {
                    final OutputInfo currInfo = ( OutputInfo ) outputInfoIter.next();
                    
                    currInfo.print( ".ENDS" );
                    currInfo.println();
                    currInfo.flush();
                }
            }
            catch( IOException e ) {
                m_Error = e;
            }
        }
    }

    public void closeOutputs( ) {
        if ( m_Error == null ) {
            try {
                final Iterator outputInfoIter =
                    m_OutputInfos.iterator();
                while ( outputInfoIter.hasNext() ) {
                    final OutputInfo currInfo = ( OutputInfo ) outputInfoIter.next();
                    
                    currInfo.close();
                }
            }
            catch( IOException e ) {
                m_Error = e;
            }
        }
        
    }

    public boolean haveError() {
        return m_Error != null;
    }

    public Exception getError() {
        return m_Error;
    }
}
