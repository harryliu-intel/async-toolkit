/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

import com.avlsi.util.debug.Debug;
import com.avlsi.util.exception.AssertionFailure;

import java.math.BigInteger;

/**
 * <p> Class containing bit-manipulation tools for
 * <code>ChannelOutputBundle</code> and <code>ChannelInputBundle</code>.  It's a
 * static toolkit because inheritance isn't needed. </p>
 *
 * @author Kim Wallmar
 *
 * @review kiniry - Under review beginning 29 July 2002.
 **/

final class ChannelBundle {

    /** Should never be used; this is a static toolkit class. **/
    private ChannelBundle() {
        throw new AssertionFailure("ChannelBundle instantiation");
    }

    /**
     * Deconstructs the token given it into several tokens.  Since
     * Java is big-endian, the most significant "bits" are in the 0th
     * slot of the array.  The deconstruction method is like
     * bitmasking, but more flexible, because hardware channels don't
     * always carry an integral number of bits.
     *
     * FIXME: doesn't do anything about overflow.
     *
     * @param token The number which is being deconstructed.
     * @param Ns An array of the number of possible values for each slice.
     * @return Slices.
     **/
    public static BigInteger[] deconstructToken(BigInteger token,
                                                BigInteger[] Ns) {
        Debug.assertTrue(Ns.length > 1,
                     "must slice token into at least two tokens");

        final int numSlices = Ns.length;
        final BigInteger[] slices = new BigInteger[numSlices];
        BigInteger tokenLeft = token;

        // Counting down because the divide/remainder cascade has to
        // start from the least significant bits
        for (int i=numSlices-1; i>=0; i--) {
            final BigInteger[] quotientAndRemainder =
                tokenLeft.divideAndRemainder(Ns[i]);
            tokenLeft = quotientAndRemainder[0];
            slices[i] = quotientAndRemainder[1];
        }

        return slices;
    }

    /**
     * Assumes that 0 <= slice[i] < Ns[i] for all i.
     **/
    public static BigInteger constructToken(BigInteger[] slices,
                                            BigInteger[] Ns) {
        final int numSlices = slices.length;
        Debug.assertTrue(numSlices > 1,
                     "can't construct a token from fewer than two tokens");
        Debug.assertTrue(numSlices == Ns.length,
                     "array length mismatch: " + numSlices + "!=" + Ns.length);
        BigInteger accum = slices[0];
        for (int i=1; i<numSlices; i++) {
            accum = (accum.multiply(Ns[i])).add(slices[i]);
        }

        return accum;
    }

} // end of final class ChannelBundle

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
