/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id
 */

package com.fulcrummicro.hw.verification.util.gen;


public class ZeroGenerator extends NumberGenerator {

    /* standard NumberGenerator constructors */
    public ZeroGenerator() {

    }

    protected long nextSeed() {
        return 0;
    }

    public long next(int bits) {
        return 0;
    }

    /** Get next number, guaranteed to be between min and max. 
     *
     * @todo Fix algorithm to allow min, max to be farther apart than
     * MAX_LONG
     **/
    public long next(long min, long max) {
        return 0;
    }

    public boolean nextBit() {
        return false;
    }

    /** These methods return a new value which has been cast to the
     * specified Java type. **/
    public byte nextByte() {
        return 0;
    }

    public short nextShort() {
        return 0;
    }
    
    public int nextInt() {
        return 0;
    }

    public long nextLong() {
        return 0;
    }

    /** Return the number generator to its original state **/
    public void reset() {
    }
}
