/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */
/**
   An iterator to iterate over all subcell instances
   in an iteration of AbstractDevices.
 */

package com.avlsi.netlist.util;

import java.util.Collections;
import java.util.Map;
import java.util.NoSuchElementException;

import com.avlsi.file.common.HierName;

import com.avlsi.netlist.AbstractDevice;
import com.avlsi.netlist.AbstractDeviceIterator;
import com.avlsi.netlist.AbstractNode;
import com.avlsi.netlist.Visitor;

import com.avlsi.netlist.impl.VisitorImpl;

public class FETIterator {
    /**
       A nice interface to an instantiation of a subcell.
     */
    public static final class FET implements AbstractDevice {
        private final HierName m_Name;
        private final AbstractNode m_Drain;
        private final AbstractNode m_Gate;
        private final AbstractNode m_Source;
        private final AbstractNode m_Bulk;
        private final double m_Length;
        private final double m_Width;
        private final String m_Type;
        private final Map m_Params;

        FET( final HierName name,
             final AbstractNode drain,
             final AbstractNode gate,
             final AbstractNode source,
             final AbstractNode bulk,
             final double length,
             final double width,
             final String type,
             final Map parameters ) {
            m_Name = name;
            
            m_Drain = drain;
            m_Gate = gate;
            m_Source = source;
            m_Bulk = bulk;
            
            m_Length = length;
            m_Width = width;
            m_Type = type;

            m_Params = parameters;
            
        }
        
        /**
           Retrieves the name of the instance.
           @return The name of the instance.
         */
        public HierName getName() {
            return m_Name;
        }

        public AbstractNode getDrain() {
            return m_Drain;
        }

        public AbstractNode getGate() {
            return m_Gate;
        }

        public AbstractNode getSource() {
            return m_Source;
        }

        public AbstractNode getBulk() {
            return m_Bulk;
        }

        public double getLength() {
            return m_Length;
        }

        public double getWidth() {
            return m_Width;
        }

        public String getType() {
            return m_Type;
        }

        /**
           @return A map from String to Double that captures additional
           parameters of the instantiation.
         */
        public Map getParams() {
            return Collections.unmodifiableMap( m_Params );
        }

        public void accept( final Visitor v ) {
            
            v.genericTransistor( m_Name,
                                 m_Drain,
                                 m_Gate,
                                 m_Source,
                                 m_Bulk,
                                 m_Length,
                                 m_Width,
                                 m_Type,
                                 m_Params );

        }
    }

    private FET m_CurrFET;

    private final AbstractDeviceIterator m_Iter;

    /**
       Construcst a SubcellIterator that will iterate over
       all the subcell instances that the specified
       AbstractDeviceIterator returns.
       @param iter Iterator to get AbstractDevices from.
     */
    public FETIterator( final AbstractDeviceIterator iter ) {
        m_Iter = iter;
        m_CurrFET = null;
    }

    public boolean hasNext() {
        while ( ( m_CurrFET == null ) && ( m_Iter.hasNext() ) ) {
            final AbstractDevice currDevice = m_Iter.next();

            currDevice.accept( new VisitorImpl() {
                    public void genericTransistor(final HierName name,
                                                  final AbstractNode drain,
                                                  final AbstractNode gate,
                                                  final AbstractNode source,
                                                  final AbstractNode bulk,
                                                  final double length,
                                                  final double width,
                                                  final String type,
                                                  final Map parameters) {
                        m_CurrFET = new FET( name,
                                             drain,
                                             gate,
                                             source,
                                             bulk,
                                             length,
                                             width,
                                             type,
                                             parameters );
                    }
                } );

        }
        return m_CurrFET != null;
    }

    public FET next() {
        if ( hasNext() ) {
            final FET ret = m_CurrFET;
            m_CurrFET = null;
            return ret;
        }
        else {
            throw new NoSuchElementException();
        }
    }

}
