/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.layout.gdsII;


import java.util.Set;
import java.util.HashSet;

import com.avlsi.layout.gdsII.TableEmitterInterface ;



public class CachingTableEmitter implements TableEmitterInterface {

    private final Set m_EmittedNodeNames;
    private final Set m_EmittedCellNames;
    private final Set m_EmittedInstanceNames;

    private final TableEmitterInterface m_Chained;

    public CachingTableEmitter( TableEmitterInterface chained ) {
        m_Chained = chained;
        m_EmittedNodeNames = new HashSet();
        m_EmittedCellNames = new HashSet();
        m_EmittedInstanceNames = new HashSet();
    }

    public boolean haveCellName( final String castName ) {
        return m_EmittedCellNames.contains( castName );
    }
    
    public void emitCellName( final String castName, 
                              final String cadenceName, 
                              final String gdsIIName ) throws TableEmitterException {
        if ( m_EmittedCellNames.add( castName ) ) {
            m_Chained.emitCellName( castName, cadenceName, gdsIIName );
        }
    }
    
    public boolean haveNodeName( final String castName ) {
        return m_EmittedNodeNames.contains( castName );
    }
    
    public void emitNodeName( final String castName, 
                              final String cadenceName,
                              final String gdsIIName ) throws TableEmitterException {
        if ( m_EmittedNodeNames.add( castName ) ) {
            m_Chained.emitNodeName( castName, cadenceName, gdsIIName );
        }
    }
    
    public boolean haveInstanceName( final String castName ) {
        return m_EmittedInstanceNames.contains( castName );
    }
    
    public void emitInstanceName( final String castName,
                                  final String cadenceName,
                                  final String gdsIIName ) throws TableEmitterException {
        if ( m_EmittedInstanceNames.add( castName ) ) {
            m_Chained.emitInstanceName( castName, cadenceName, gdsIIName );
        }
    }
    
    public void close() throws TableEmitterException {
        m_Chained.close();
    }
    
}
