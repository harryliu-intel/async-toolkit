/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.netlist.impl;

import java.util.Iterator;

import com.avlsi.netlist.AbstractDevice;
import com.avlsi.netlist.AbstractDeviceIterator;

public class SimpleAbstractDeviceIterator implements AbstractDeviceIterator {
    protected final Iterator iterator;

    public SimpleAbstractDeviceIterator(Iterator iterator) {
        this.iterator = iterator;
    }

    public boolean hasNext() {
        return iterator.hasNext();
    }

    public AbstractDevice next() {
        return (AbstractDevice) iterator.next();
    }
}
