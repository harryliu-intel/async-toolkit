/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.netlist.util;

import java.util.Collections;
import java.util.Map;
import java.util.List;
import java.util.LinkedList;
import java.util.NoSuchElementException;

import com.avlsi.file.common.HierName;

import com.avlsi.netlist.AbstractNetlist;
import com.avlsi.netlist.AbstractDevice;
import com.avlsi.netlist.AbstractDeviceIterator;
import com.avlsi.netlist.AbstractNode;
import com.avlsi.netlist.AbstractNodeIterator;
import com.avlsi.netlist.Visitor;

import com.avlsi.netlist.impl.SimpleAbstractNodeIterator;
import com.avlsi.netlist.impl.VisitorImpl;


/**
   An iterator to iterate over all subcell instances
   in an iteration of AbstractDevices.
 */
public class SubcellIterator {
    /**
       A nice interface to an instantiation of a subcell.
     */
    public static final class SubcellInstance implements AbstractDevice {
        private final HierName m_Name;
        private final AbstractNetlist m_Netlist;
        private final List m_ConnectedNets;
        private final Map m_Params;

        SubcellInstance( final HierName name, 
                         final AbstractNetlist netlist,
                         final AbstractNodeIterator connectedNodes,
                         final Map params ) {
            m_Name = name;
            m_Netlist = netlist;
            m_Params = params;
            m_ConnectedNets = new LinkedList();
            
            while( connectedNodes.hasNext() ) {
                final AbstractNode currNode = connectedNodes.next();
                m_ConnectedNets.add( currNode );
            }
        }
        
        /**
           Retrieves the name of the instance.
           @return The name of the instance.
         */
        public HierName getName() {
            return m_Name;
        }

        /**
           Retrieves the nodes connected to the instantiated netlist.
           @return An iterator that will iterate over all the nodes connected
           to input nodes of the instantiated netlist in the same order as 
           <code>getInstantiatedNetlist().getInputNodes()</code> would 
           return, followed by all the nodes connected to output 
           nodes of the instantiated netlist in the same order as
           <code>getInstantiatedNetlist().getOutputNodes()</code>
           would return.
         */
        public AbstractNodeIterator getConnectedNodes() {
            return new SimpleAbstractNodeIterator( m_ConnectedNets.iterator() );
        }

        /**
           Retrieves the netlist of the instantiated cell.
           @return The netlist of the instantiated cell.
         */
        public AbstractNetlist getInstantiatedNetlist() {
            return m_Netlist;
        }

        /**
           @return A map from String to Double that captures additional
           parameters of the instantiation.
         */
        public Map getParams() {
            return Collections.unmodifiableMap( m_Params );
        }

        public void accept( final Visitor v ) {
            v.subcircuitCall( m_Name,
                              m_Netlist,
                              getConnectedNodes(),
                              m_Params );
        }
    }

    private SubcellInstance m_CurrInstance;

    private final AbstractDeviceIterator m_Iter;

    /**
       Construcst a SubcellIterator that will iterate over
       all the subcell instances that the specified
       AbstractDeviceIterator returns.
       @param iter Iterator to get AbstractDevices from.
     */
    public SubcellIterator( final AbstractDeviceIterator iter ) {
        m_Iter = iter;
        m_CurrInstance = null;
    }

    public boolean hasNext() {
        while ( ( m_CurrInstance == null ) && ( m_Iter.hasNext() ) ) {
            final AbstractDevice currDevice = m_Iter.next();

            currDevice.accept( new VisitorImpl() {
                    public void subcircuitCall(final HierName name,
                                               final AbstractNetlist circuit,
                                               final AbstractNodeIterator nodes,
                                               final Map parameters) {
                        m_CurrInstance = new SubcellInstance( name,
                                                              circuit,
                                                              nodes,
                                                              parameters );
                    }
                } );

        }
        return m_CurrInstance != null;
    }

    public SubcellInstance next() {
        if ( hasNext() ) {
            final SubcellInstance ret = m_CurrInstance;
            m_CurrInstance = null;
            return ret;
        }
        else {
            throw new NoSuchElementException();
        }
    }

}
