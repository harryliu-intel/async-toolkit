/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.netlist.impl;

import java.util.Iterator;

import com.avlsi.netlist.AbstractNode;
import com.avlsi.netlist.AbstractNodeIterator;

public class SimpleAbstractNodeIterator implements AbstractNodeIterator {
    protected final Iterator iterator;

    public SimpleAbstractNodeIterator(Iterator iterator) {
        this.iterator = iterator;
    }

    public boolean hasNext() {
        return iterator.hasNext();
    }

    public AbstractNode next() {
        return (AbstractNode) iterator.next();
    }
}
