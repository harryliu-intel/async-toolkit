/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.netlist.util;

import java.util.Stack;
import java.util.Set;
import java.util.HashSet;
import java.util.NoSuchElementException;

import com.avlsi.netlist.AbstractNetlist;
import com.avlsi.netlist.AbstractNetlistIterator;
import com.avlsi.netlist.AbstractDeviceIterator;

import com.avlsi.netlist.util.SubcellIterator;

/**
   Iterator to iterate over all the abstract netlists
   in a circuit hierarchy.  Starting from a root circuit
   all the instantiated sub circuits are enumerated such that
   a sub circuit is enumerated after all the sub circuits it
   instantiates are enumerated.
 */
public final class ChildrenFirstNetlistIterator 
    implements AbstractNetlistIterator {
    
    private static class StackFrame {
        
        private final AbstractNetlist m_Netlist;
        private final SubcellIterator m_Iter;
        
        public StackFrame( final AbstractNetlist netlist ) {
            m_Netlist = netlist;
            m_Iter = new SubcellIterator( netlist.getDevices() );
        }
        
        public SubcellIterator getIter() {
            return m_Iter;
        }

        public AbstractNetlist getNetlist() {
            return m_Netlist;
        }
    }
    
    private final Set m_VisitedSet;
    private final Stack m_Stack;

    private AbstractNetlist m_NextNetlist;

    private void push( final AbstractNetlist netlist ) {
        final StackFrame newFrame = new StackFrame( netlist );
        m_Stack.push( newFrame );
    } 

    private SubcellIterator getTopIterator() {
        final StackFrame topFrame = ( StackFrame ) m_Stack.peek();
        return topFrame.getIter();
    }

    private AbstractNetlist getTopNetlist() {
        final StackFrame topFrame = ( StackFrame ) m_Stack.peek();
        return topFrame.getNetlist();
    }

    private boolean wasVisited( final AbstractNetlist netlist ) {
        return m_VisitedSet.contains( netlist.getName() );
    }

    private void markAsVisited( final AbstractNetlist netlist ) {
        m_VisitedSet.add( netlist.getName() );
    }

    /**
       Constructs the iterator such that it will enumerate all the
       circuits instantiated in the circuit hierarchy defined in
       the specified netlist.
       @param top The root netlist under which to enumerate all netlists.
     */
    public ChildrenFirstNetlistIterator( final AbstractNetlist top ) {
        m_VisitedSet = new HashSet();
        m_Stack = new Stack();
        push( top );
        m_NextNetlist = null;
    }

    /**
       Determines if there is another circuit in the enumeration.
       @return true if there is another circuit in the enumeration.
               false otherwise.
     */
    public boolean hasNext() {   
        while ( ( m_NextNetlist == null ) && ( ! ( m_Stack.empty() ) ) ) {
            final SubcellIterator currIter = getTopIterator();

            AbstractNetlist instanceMaster = null;
            
            if ( currIter.hasNext() ) {
                do {
                    final SubcellIterator.SubcellInstance instance = 
                        currIter.next();
                    instanceMaster = 
                        instance.getInstantiatedNetlist();
                    
                } while ( ( currIter.hasNext() ) && 
                          ( wasVisited( instanceMaster ) ) );
            }
            if ( ( instanceMaster != null ) && 
                 ( ! wasVisited( instanceMaster ) ) ) {
                push( instanceMaster );
            }
            else {
                m_NextNetlist = getTopNetlist();
                m_Stack.pop();
                markAsVisited( m_NextNetlist );
            }
        }
        return m_NextNetlist != null;
    }

    /**
       Retrieves the next circuit from the enumeration.
       @return The next circuit in the enumeration.
     */
    public AbstractNetlist next() {   
        if ( hasNext() ) {
            final AbstractNetlist ret = m_NextNetlist;
            m_NextNetlist = null;
            return ret;
        }
        else {
            throw new NoSuchElementException();
        }
    }
    
    

}
