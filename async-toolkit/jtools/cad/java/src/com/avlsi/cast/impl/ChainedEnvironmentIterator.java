/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */
package com.avlsi.cast.impl;

import java.util.Map.Entry;
import java.util.NoSuchElementException;

import com.avlsi.cast.impl.Value;
import com.avlsi.cast.impl.Environment;
import com.avlsi.cast.impl.EnvironmentIterator;


/**
   An iterator that will iterate over one environment and then another.
 */
public class ChainedEnvironmentIterator implements EnvironmentIterator {
    
    private final EnvironmentIterator m_FirstIter;
    private final EnvironmentIterator m_SecondIter;

    /**
       @param first The first environment to iterate over.
       @param second The second environment to iterate over.
     */
    public ChainedEnvironmentIterator( Environment first, Environment second ){
        this( first.iterator(), second.iterator() );
    }

    /**
       @param first An iterator into the first environment to iterate over.
       @param second An iterator into the second environment to iterate over.
     */
    public ChainedEnvironmentIterator( EnvironmentIterator first, EnvironmentIterator second ) {
        m_FirstIter = first;
        m_SecondIter = second;
    }

    public final boolean hasNext() {
        return m_SecondIter.hasNext() || m_FirstIter.hasNext();
    }

    public final EnvironmentEntry next() {
        if ( m_FirstIter.hasNext() ) {
            return m_FirstIter.next();
        }
        else {
            return m_SecondIter.next();
        }
    }
}
