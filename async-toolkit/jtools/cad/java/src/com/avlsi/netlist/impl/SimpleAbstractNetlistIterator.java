/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.netlist.impl;

import java.util.Iterator;

import com.avlsi.netlist.AbstractNetlist;
import com.avlsi.netlist.AbstractNetlistIterator;

public class SimpleAbstractNetlistIterator implements AbstractNetlistIterator {
    protected final Iterator iterator;

    public SimpleAbstractNetlistIterator(Iterator iterator) {
        this.iterator = iterator;
    }

    public boolean hasNext() {
        return iterator.hasNext();
    }

    public AbstractNetlist next() {
        return (AbstractNetlist) iterator.next();
    }
}
