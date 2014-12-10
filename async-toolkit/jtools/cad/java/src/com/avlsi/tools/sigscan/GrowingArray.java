/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.sigscan;

/**
 * Class for a growable array if ints
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class GrowingArray {

    private static int stepsize = 100;

    private long[] ints;

    private int size;
    /**
     * Constructor.
     **/
    GrowingArray() {
        this.ints = new long[stepsize];
        this.size = 0;
    }

    public void grow() {
        long[] tmp = ints;
        ints = new long[tmp.length+stepsize];
        System.arraycopy(tmp,0,ints,0,ints.length);
    }
    
    public void add(long i) {
        if (size == ints.length) grow();
        ints[size++] = i;
    }

    public int size() { return size; }

    public long[] getInts() { return ints; }
    
}

