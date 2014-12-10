/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */
package com.avlsi.netlist.impl.simple;

import java.util.Iterator;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.Collection;

import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.file.common.HierName;

import com.avlsi.cast.impl.Environment;

import com.avlsi.file.cdl.parser.CDLLexer;

import com.avlsi.file.cdl.parser.CDLFactoryInterface;

import com.avlsi.netlist.AbstractNetlist;
import com.avlsi.netlist.AbstractNetlistIterator;

import com.avlsi.netlist.impl.SimpleAbstractNetlistIterator;

import com.avlsi.netlist.impl.simple.FET;
import com.avlsi.netlist.impl.simple.SubCircuit;

public class SimpleNetlistFactory implements CDLFactoryInterface {

    private Map m_CircuitMap;

    public SimpleNetlist m_CurrNetlist;

    public SimpleNetlistFactory() {
        m_CircuitMap = new HashMap();
        m_CurrNetlist = null;
    }


    private Map convertParameterMap( final Map parameters,
                                     final Environment env ) {
        final Map ret = new HashMap();
        final Iterator i = parameters.entrySet().iterator();
        while ( i.hasNext() ) {
            final Map.Entry currEntry = ( Map.Entry ) i.next();
            final String key = ( String ) currEntry.getKey();
            final Double val = 
                ( ( CDLLexer.InfoToken ) currEntry.getValue() ).getValue( env );
            if ( val != null ) {
                ret.put( key, val );
            }
        }
        return ret;
    }

    public AbstractNetlist getNetlist( final String rootCellName ) {
        return ( AbstractNetlist ) 
            m_CircuitMap.get( rootCellName );
        
    }

    public AbstractNetlistIterator getAllNetlists( ) {
        final Collection circuits = m_CircuitMap.values();
        return new SimpleAbstractNetlistIterator( circuits.iterator());
    }
     
    public void makeResistor( final HierName name,
                              final HierName n1,
                              final HierName n2,
                              final CDLLexer.InfoToken val,
                              final Map parameters,
                              final Environment env ) {
        throw new RuntimeException();
    }

    
    public void makeCapacitor( final HierName name,
                               final HierName npos,
                               final HierName nneg,
                               final CDLLexer.InfoToken val, 
                               final Map parameters, 
                               final Environment env ) {
        throw new RuntimeException();
    }
    
    
    public void makeTransistor( final HierName name,
                                final String type,
                                final HierName ns,
                                final HierName nd,
                                final HierName ng, 
                                final HierName nb,
                                final CDLLexer.InfoToken w,
                                final CDLLexer.InfoToken l,
                                final Map parameters,
                                final Environment env ) {
        if ( m_CurrNetlist != null ) {
            final Double wVal = w.getValue( env );
            final Double lVal = l.getValue( env );
            if ( ( wVal != null ) && ( lVal != null ) ) {
                final FET f = new FET( name,
                                       m_CurrNetlist.makeNode( nd ),
                                       m_CurrNetlist.makeNode( ng ),
                                       m_CurrNetlist.makeNode( ns ),
                                       m_CurrNetlist.makeNode( nb ),
                                       lVal.doubleValue(),
                                       wVal.doubleValue(),
                                       type,
                                       convertParameterMap( parameters, env ) );
                m_CurrNetlist.addDevice( f );
            }                          
        }
    }
    
    
    public void makeDiode( final HierName name, 
                           final String type,
                           final HierName npos,
                           final HierName nneg,
                           final CDLLexer.InfoToken val,
                           final Map parameters,
                           final Environment env ) {
        throw new RuntimeException();
    }
    
    
    public void makeInductor( final HierName name,
                              final HierName npos, 
                              final HierName nneg,
                              final CDLLexer.InfoToken val,
                              final Map parameters,
                              final Environment env ) {
        throw new RuntimeException();
    }
    
    
    public void makeBipolar( final HierName name, 
                             final String type,
                             final HierName nc,
                             final HierName nb,
                             final HierName ne,
                             final CDLLexer.InfoToken val,
                             final Map parameters,
                             final Environment env ) {
        throw new RuntimeException();
    }
    
    
    public void makeCall( final HierName name,
                          final String subName,
                          final HierName[] args,
                          final Map parameters, 
                          final Environment env ) {
        if ( m_CurrNetlist != null ) {
            final SimpleNetlist subCircuit = 
                ( SimpleNetlist ) m_CircuitMap.get( subName );
            if ( subCircuit != null ) {
                final List nodes = new ArrayList( args.length );
                int i = 0;
                for ( i = 0 ; i < args.length ; ++i ) {
                    nodes.add( m_CurrNetlist.makeNode( args[i] ) );
                }
                final SubCircuit s = 
                    new SubCircuit( name,
                                    subCircuit,
                                    nodes,
                                    convertParameterMap( parameters, env ) );
                m_CurrNetlist.addDevice( s );
            }                          
        }
    }
    
    
    public void beginSubcircuit( final String subName, 
                                 final String[] in,
                                 final String[] out,
                                 final Map parameters, 
                                 final Environment env ) {
        try {
            m_CurrNetlist = 
                new SimpleNetlist( HierName.makeHierName( subName, '.' ),
                                   in,
                                   out );
        }
        catch( InvalidHierNameException e ) {
            throw (IllegalArgumentException)
                new IllegalArgumentException().initCause(e);
        }
       
                                           
    }
    
    
    public void endSubcircuit( final String subName, 
                               final Environment env ) {
        m_CircuitMap.put( m_CurrNetlist.getName().toString(), m_CurrNetlist );
        m_CurrNetlist = null;
    }
    
}
