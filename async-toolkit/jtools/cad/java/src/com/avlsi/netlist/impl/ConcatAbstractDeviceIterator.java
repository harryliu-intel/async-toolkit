/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.netlist.impl;

import java.util.Collection;
import java.util.Iterator;
import java.util.NoSuchElementException;

import com.avlsi.netlist.AbstractDevice;
import com.avlsi.netlist.AbstractDeviceIterator;


/**
   Utility class to concantenate any number of AbstractDeviceIterators into
   a single AbstractDeviceIterator.
 */
public class ConcatAbstractDeviceIterator implements AbstractDeviceIterator {
    protected final AbstractDeviceIterator[] iterators;
    protected int current;
    
    /**
       Construct a AbstractDeviceIterator that is the concatentation of a
       collection of AbstractDeviceIterators.
       @param iterators Collection of AbstractDeviceIterator instances to
       concatenate into a single AbstractDeviceIterator.  The iterators in the
       collection should not be modified after this constructor is called.
     */
    public ConcatAbstractDeviceIterator( final Collection iterators) {
        
        this( ( AbstractDeviceIterator[] ) iterators.toArray(new AbstractDeviceIterator[0]) );
    }
    /**
      Construct a AbstractDeviceIterator that is the concatentation of an array 
      AbstractDeviceIterators.
      @param iterators Array of AbstractDeviceIterator instances to concatenate
      into a single AbstractDeviceIterator.  The iterators in the array should
      not be modified after this constructor is called.
    */
    public ConcatAbstractDeviceIterator( final AbstractDeviceIterator[] iterators ) {
        this.iterators = iterators;
        current = 0;
    }
    
    /**
       Utility constructor to concatenate 2 AbstractDeviceIterators.
       The specified iterators should not be modified after this constructor is
       called.
       @param i1 The first iterator.
       @param i2 The second iterator.
    */
    public ConcatAbstractDeviceIterator( final AbstractDeviceIterator i1, 
                                         final AbstractDeviceIterator i2 ) {
        this(new AbstractDeviceIterator[] { i1, i2 });
        
    }

    /**
       Utility constructor to concatenate 3 AbstractDeviceIterators.
       The specified iterators should not be modified after this constructor is
       called.
       @param i1 The first iterator.
       @param i2 The second iterator.
       @param i3 The third iterator.
     */
    public ConcatAbstractDeviceIterator( final AbstractDeviceIterator i1, 
                                         final AbstractDeviceIterator i2,
                                         final AbstractDeviceIterator i3 ) {
        this(new AbstractDeviceIterator[] { i1, i2, i3 });
    }

    public boolean hasNext() {
        while (current < iterators.length) {
            if (iterators[current].hasNext()) {
                return true;
            } else {
                current++;
            }
        }
        return false;
    }

    public AbstractDevice next() {
        if (hasNext()) {
            return (AbstractDevice) iterators[current].next();
        } else {
            throw new NoSuchElementException();
        }
    }
}
