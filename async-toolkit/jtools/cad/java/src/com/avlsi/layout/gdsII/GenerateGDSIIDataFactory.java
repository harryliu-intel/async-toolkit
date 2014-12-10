/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.layout.gdsII;



import java.util.Map;
import java.util.HashSet;

import com.avlsi.file.common.HierName;

import com.avlsi.cast.impl.Environment;

import com.avlsi.file.cdl.parser.CDLFactoryInterface;
import com.avlsi.file.cdl.parser.CDLLexer;

import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.CDLNameInterfaceFactory;
import com.avlsi.file.cdl.util.rename.CDLRenameException;

import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cadencize.CadenceInfo;

import com.avlsi.layout.gdsII.TableEmitterInterface;
import com.avlsi.layout.gdsII.TableEmitterException;
import com.avlsi.layout.gdsII.TableEmitterFactoryInterface;

public class GenerateGDSIIDataFactory implements CDLFactoryInterface {
    
    private final CDLNameInterfaceFactory m_fromFactory;
    private final CDLNameInterfaceFactory m_toFactory;

    private Exception m_Error;

    private final TableEmitterFactoryInterface m_EmitterFactory;

    private String m_CurrCellName;
    private TableEmitterInterface m_CurrEmitter;

    public GenerateGDSIIDataFactory( TableEmitterFactoryInterface emitterFactory,
                                     CDLNameInterfaceFactory from,
                                     CDLNameInterfaceFactory to
                                     ) {
        m_EmitterFactory = emitterFactory;
        m_CurrCellName = null;
        m_CurrEmitter = null ;
        m_fromFactory = from;
        m_toFactory = to;

        m_Error = null;
    }

        

    public void emitNodeName(final HierName name ) {
        emitNodeName( name.toString() );
    }

    public void emitNodeName( final String nameString ) {
        if ( m_Error == null ) {
            try {
                if ( ! m_CurrEmitter.haveNodeName( nameString ) ||
                     ! m_CurrEmitter.haveCellName( m_CurrCellName ) ) {

                    CDLNameInterface from = m_fromFactory.getNameInterface(m_CurrCellName);
                    CDLNameInterface to = m_toFactory.getNameInterface(m_CurrCellName);

                    final String cadenceName = from.renameNode( nameString );
                    final String gdsIIName = to.renameNode( nameString );
                    m_CurrEmitter.emitNodeName( nameString, cadenceName, gdsIIName );
                }
            }
            catch ( CDLRenameException e ) {
                m_Error = e;
            }
            catch ( TableEmitterException e ) {
                m_Error = e ;
            }
        }
    }

    public void emitCellName( final HierName name ) {
        emitCellName( name.toString() );
    }

    public void emitCellName( final String nameString ) {
        if ( m_Error == null ) {
            try {
                if ( ! m_CurrEmitter.haveCellName( nameString ) ) {

                    CDLNameInterface from = m_fromFactory.getNameInterface(nameString);
                    CDLNameInterface to = m_toFactory.getNameInterface(nameString);

                    final String cadenceName = from.renameCell( nameString );
                    final String gdsIIName = to.renameCell( nameString );
                    m_CurrEmitter.emitCellName( nameString, cadenceName, gdsIIName );
                }
            }
            catch ( CDLRenameException e ) {
                m_Error = e;
            }
            catch ( TableEmitterException e ) {
                m_Error = e ;
            }
        }
    }

        

    public void emitInstanceName( final HierName name ) {
        if ( m_Error == null ) {
            try {
                final String nameString = name.toString();
                if ( ! m_CurrEmitter.haveInstanceName( nameString ) ||
                     ! m_CurrEmitter.haveCellName( m_CurrCellName ) )      { 
                    
                    CDLNameInterface from = m_fromFactory.getNameInterface(m_CurrCellName);
                    CDLNameInterface to = m_toFactory.getNameInterface(m_CurrCellName);
                    
                    final String cadenceName = from.renameSubCellInstance( nameString );
                    final String gdsIIName = to.renameSubCellInstance( nameString );
                    m_CurrEmitter.emitInstanceName( nameString, cadenceName, gdsIIName );
                }
            }
            catch ( CDLRenameException e ) {
                m_Error = e;
            }
            catch ( TableEmitterException e ) {
                m_Error = e ;
            }
        }
    }

    public void makeTransistor( HierName name, 
                                String type,
                                HierName ns,
                                HierName nd,
                                HierName ng,
                                HierName nb,
                                CDLLexer.InfoToken w,
                                CDLLexer.InfoToken l,
                                Map parameters, 
                                Environment env) {
            
        if ( m_Error == null ) {
            emitNodeName( ns );
            emitNodeName( nd );
            emitNodeName( ng );
            emitNodeName( nb );
        }
            
    }

    public void makeDiode(HierName name, String type, HierName npos, HierName nneg,
                          CDLLexer.InfoToken val, 
                          Map parameters, Environment env) { }
    public void makeResistor(HierName name, HierName n1, HierName n2,
                             CDLLexer.InfoToken val, Map parameters,
                             Environment env) { }
    public void makeCapacitor(HierName name, HierName npos, HierName nneg,
                              CDLLexer.InfoToken val, Map parameters,
                              Environment env) { }
    public void makeInductor(HierName name, HierName npos, HierName nneg,
                             CDLLexer.InfoToken val, Map parameters,
                             Environment env) { }
    public void makeBipolar(HierName name, String type, HierName nc,
                            HierName nb, HierName ne,
                            CDLLexer.InfoToken val, 
                            Map parameters, Environment env) { }
        
    public void makeCall( HierName name, 
                          String subName,
                          HierName[] args,
                          Map parameters, 
                          Environment env ) {
        if ( m_Error == null ) {
            for ( int i = 0 ; i < args.length ; ++i ) {
                emitNodeName( args[i] );
            }
            emitCellName( subName );
            emitInstanceName( name );
        }
    }
        
    public void beginSubcircuit( String subName,
                                 String[] in, 
                                 String[] out,
                                 Map parameters,
                                 Environment env ) {
        if ( m_Error == null ) {
            try {
                m_CurrCellName = subName;

                CDLNameInterface from = m_fromFactory.getNameInterface(m_CurrCellName);
                CDLNameInterface to = m_toFactory.getNameInterface(m_CurrCellName);

                final String fromName = from.renameCell( subName );
                final String toName = to.renameCell( subName );
                    
                m_CurrEmitter = m_EmitterFactory.getTableEmitter( subName,
                                                                  fromName, 
                                                                  toName ); 
                emitCellName( subName );

                int i;
                for ( i = 0 ; i < in.length ; ++i ) {
                    emitNodeName( in[i] );
                }                    
                for ( i = 0 ; i < out.length ; ++i ) {
                    emitNodeName( out[i] );
                }       
            }
                
            catch ( CDLRenameException e ) {
                m_Error = e;
            }
            catch ( TableEmitterException e ) {
                m_Error = e;
            }
        }
    }

    public void endSubcircuit( String subName, Environment env ) {
        if ( m_Error == null ) {
            try {
                m_CurrCellName = null;
                m_CurrEmitter.close();
            }
            catch ( TableEmitterException e ) {
                m_Error = e ;
            }
        }
    }

}
