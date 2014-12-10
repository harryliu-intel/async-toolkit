/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.netlist.util;

import java.util.NoSuchElementException;

import com.avlsi.netlist.AbstractNodeIterator;
import com.avlsi.netlist.AbstractNode;

public class CombiningNodeIterator implements AbstractNodeIterator {


    private final AbstractNodeIterator mFirst;
    private final AbstractNodeIterator mSecond;

    public CombiningNodeIterator( AbstractNodeIterator first,
                                  AbstractNodeIterator second ) {
        mFirst = first;
        mSecond = second;
    }

    public boolean hasNext() {
        return ( ( mSecond.hasNext() ) || ( mFirst.hasNext() ) );
    }
    /**
       @throws NoSuchElementException
     */
    public AbstractNode next() {
        if ( mFirst.hasNext() ) {
            return mFirst.next();
        }
        else {
            return mSecond.next();
        }
    }
}
