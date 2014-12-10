/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.layout.gdsII;


import com.avlsi.layout.gdsII.TableEmitterFactoryInterface;
import com.avlsi.layout.gdsII.TableEmitterInterface;
import com.avlsi.layout.gdsII.TableEmitterException;

import com.avlsi.layout.gdsII.CachingTableEmitter;

public class SplittingTableEmitterFactory implements TableEmitterFactoryInterface {

    private final TableEmitterFactoryInterface m_Left;
    private final TableEmitterFactoryInterface m_Right;

    private static final class SplittingTableEmitter implements TableEmitterInterface {
        private final TableEmitterInterface m_Left;
        private final TableEmitterInterface m_Right;

        public boolean haveCellName( final String castName ) {
            return ( ( m_Left.haveCellName( castName ) ) && 
                     ( m_Right.haveCellName( castName ) ) ); 
        }

        public void emitCellName( final String castName, 
                                  final String cadenceName, 
                                  final String gdsIIName ) throws TableEmitterException {
            m_Left.emitCellName( castName, cadenceName, gdsIIName );
            m_Right.emitCellName( castName, cadenceName, gdsIIName );
        }

        public boolean haveNodeName( final String castName ) {
            return ( ( m_Left.haveNodeName( castName ) ) && 
                     ( m_Right.haveNodeName( castName ) ) ); 
        }

        public void emitNodeName( final String castName, 
                                  final String cadenceName, 
                                  final String gdsIIName ) throws TableEmitterException {
            m_Left.emitNodeName( castName, cadenceName, gdsIIName );
            m_Right.emitNodeName( castName, cadenceName, gdsIIName );
        }

        public boolean haveInstanceName( final String castName ) {
            return ( ( m_Left.haveInstanceName( castName ) ) && 
                     ( m_Right.haveInstanceName( castName ) ) ); 
        }

        public void emitInstanceName( final String castName, 
                                      final String cadenceName, 
                                      final String gdsIIName ) throws TableEmitterException {
            m_Left.emitInstanceName( castName, cadenceName, gdsIIName );
            m_Right.emitInstanceName( castName, cadenceName, gdsIIName );
        }

        public void close() throws TableEmitterException {
            m_Left.close();
            m_Right.close();
        }

        public SplittingTableEmitter( final TableEmitterInterface left,
                                      final TableEmitterInterface right ) {
            m_Left = left;
            m_Right = right;
        }
    }

    public SplittingTableEmitterFactory( final TableEmitterFactoryInterface left,
                                         final TableEmitterFactoryInterface right ) {
        m_Left = left;
        m_Right = right;
    }
    
    public TableEmitterInterface getTableEmitter( final String castCellName,
                                                  final String cadenceCellName,
                                                  final String gdsIICellName )
        throws TableEmitterException {
        
        TableEmitterInterface left = m_Left.getTableEmitter( castCellName,
                                                             cadenceCellName,
                                                             gdsIICellName );

        TableEmitterInterface right = m_Right.getTableEmitter( castCellName,
                                                               cadenceCellName,
                                                               gdsIICellName );
        
        return new CachingTableEmitter( new SplittingTableEmitter( left, right ) );
    }
}
