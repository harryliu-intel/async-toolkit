/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.fulcrummicro.hw.verification.util.gen;

import java.util.Random;

public class RandomNumberGenerator extends NumberGenerator {
    Random random;
    long origSeed;

    /* standard NumberGenerator constructors */
    public RandomNumberGenerator(String name, Granary g, long seed) {
        this(g.getSeed(name, seed));
    }

    public RandomNumberGenerator(String name, Granary g) {
        this(g.getSeed(name));
    }

    public RandomNumberGenerator(String name) {
        this(Granary.getDefault().getSeed(name));
    }

    public long next(int bits) {
        return random.nextLong() & ((1L<<bits)-1);
    }

    /** @return A random number in the range [min,max), not including max.
     * If min=max=N, will always return N.
     **/
    public long next(long min, long max) {
        if (min == max) max++; //DD - asking for next(N,N) returns N
        assert max-1>=min :
              "RandomNumberGenerator min ("+min+") > max ("+max+")";
        if(max-min<=0) {
            // range too big to use below method; max-min >
            // Long.MAX_VALUE
            long r;
            do {
                r = random.nextLong();
            } while(r<min || r>=(max-1));
            return r;
        } else {
            // get a value which is evenly distributed over the
            // interval (use mod, but reject numbers which come from
            // the last group. based on implementation of
            // java.util.Random.nextInt(int))
            long r, val, n=max-min;
            do {
                r = random.nextLong()>>>1;
                val = r % n;
            } while(r - val + (n-1) < 0);
            return val+min;
        }
    }

    public boolean nextBit() {
        return (nextByte()&1)==0;
    }

    /** These methods return a new value which has been cast to the
     * specified Java type. **/
    public byte nextByte() {
        return (byte)(random.nextInt() & 255);
    }

    public void nextBytes(byte[] bytes) {
        random.nextBytes(bytes);
    }

    public short nextShort() {
        return (short)(random.nextInt() & ((1<<16)-1));
    }

    public int nextInt() {
        return random.nextInt();
    }

    /**
     * Returns a pseudorandom, uniformly distributed integer value between 0
     * (inclusive) and the specified value (exclusive).
     *
     * @param max
     *            is the upper bound of the range to draw from. Must be
     *            positive.
     * @return a pseudorandom, uniformly distributed integer value between 0
     *         (inclusive) and the specified value (exclusive).
     */
    public int nextInt(int max) {
        return random.nextInt(max);
    }

    public long nextLong() {
        return random.nextLong();
    }

    public double nextDouble() {
        return random.nextDouble();
    }

    /**
     *
     * @param p probability of returning true
     * @return
     * true with probability p
     * false with probability 1-p
     */
    public boolean nextBoolean(double p) {
        if (p==0.0) return false;
        if (p==1.0) return true;
        assert p >= 0.0 && p <= 1.0 :
            String.format("input %f out of range (0.0,1.0)", p);
        return random.nextDouble() < p;
    }

    /** Return the number generator to its original state **/
    public void reset() {
        random.setSeed(origSeed);
    }

    // FIXME The visibility of this constructor should be changed to protected
    // once Bugzilla #17834 has been fixed -- mhesseli.
    public RandomNumberGenerator(long seed) {
        this.origSeed = seed;
        random = new Random(seed);
    }

}
