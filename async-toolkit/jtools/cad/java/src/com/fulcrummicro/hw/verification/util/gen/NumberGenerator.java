/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.fulcrummicro.hw.verification.util.gen;

public abstract class NumberGenerator {
    /** Instantiate a new NumberGenerator with the given name and seed
     * store. The name will be used to identify a seed in the store,
     * or to create a new one, and should be unique to your program
     * unless you want to instantiate multiple generators with the
     * same seed. **/
    //public NumberGenerator(String name, Granary g);

    /** Use default Granary. See above. **/
    //public NumberGenerator(String name);

    /** Instantiate with a default seed (which is overridden by that
     * in the Granary if one is present). See above. **/ 
    //public NumberGenerator(String name, Granary g, long seed);

    /** Get next number, with all bits other than the first "bits"
     * bits set to 0. **/
    abstract public long next(int bits);

    /** Returns a random number in [min, max]. */
    public long nextInclusive(long min, long max) {
        return next(min, max+1);
    }

    /** Get next number, guaranteed to be between min and max --
     * inclusive of min, exclusive of max, as it should be. In other
     * words, next(0,1) will always return 0. If max is the smallest
     * long, then it will be interpreted as one more than the largest
     * long, so next(1L<<63,1L<<63) will return an arbitrary long, and
     * next(0,1L<<63) will return any positive long. **/
    abstract public long next(long min, long max);

    /** These methods return a new value which has been cast to the
     * specified Java type. **/
    abstract public boolean nextBit();
    abstract public byte nextByte();
    abstract public short nextShort();
    abstract public int nextInt();
    abstract public long nextLong();

    /** Return the number generator to its original state **/
    abstract public void reset();

    public boolean nextWeightedBoolean(double p) {
        return Math.abs(nextInt()) < p*Integer.MAX_VALUE;
    }

    /** Utility functions */

    public int next(int[] array) {
        return array[(int)next(0,array.length)];
    }

    /** Return a surjection from 'domain' elements to 'range'
     * elements, stored as an array.
     **/
    public int[] nextSurjection(int domain, int range) {
        // todo: might be nice to find an algorithm which gets a
        // random n and then finds the nth surjection in some natural
        // ordering. this would preserve the LFSR period, and would
        // make the behavior of CountingGenerator useful for full
        // enumeration. the current implementation of this and
        // nextInjection lacks these features. (i think) --fred
        assert domain >= range;
        int inv[] = nextInjection(range,domain);
        int map[] = new int[domain];
        for(int i=0; i<domain; i++) map[i]=-1;
        for(int i=0; i<range; i++) map[inv[i]]=i;
        for(int i=0; i<domain; i++) {
            if(map[i] != -1) continue;
            int c=(int)next(0,range);
            map[i] = c;
        }
        return map;
    }
    
    /** Return an injection from 'domain' elements to 'range'
     * elements, stored as an array. This is faster than
     * nextSurjection, so it should be used preferentially when a
     * bijection is needed (domain==range).
     **/
    public int[] nextInjection(int domain, int range) {
        assert domain <= range;
        // m is the map
        int[] m = new int[domain];
        // p is a structure we use to keep track of the unused numbers
        // in the range. if x is in [i,range) then p[x]+x is unused.
        // the index-relative encoding saves on initialization.
        int[] p = new int[range];
        for(int i=0; i<domain; i++) {
            int j=(int)next(i,range);
            int t=p[i]+i; p[i]=p[j]+j-i; p[j]=t-j;
            m[i] = p[i]+i;
        }
        return m;
    }

    public Object[] permuteArray(Object[] array) {
        Object[] t = array.clone();
        int[] ind = nextInjection(array.length, array.length);
        for(int i=0; i<array.length; i++)
            t[i]=array[ind[i]];
        return t;
    }

    public int[] permuteArray(int[] array) {
        int[] t = array.clone();
        int[] ind = nextInjection(array.length, array.length);
        for(int i=0; i<array.length; i++)
            t[i]=array[ind[i]];
        return t;
    }

    /** Returns a random point with integer coordinates from a regular
     * n-1 dimensional simplex. The simplex is the set of points
     * (x_0,...,x_{n-1}) satisfying x_0+...+x_{n-1} <= k for
     * nonnegative x_i.
     */
    public int[] nextPointOnSimplex(int k, int n) {
        assert n>=1 && k>=0;
        int[] y = new int[n+1], x = new int[n];
        y[0]=0;
        y[n]=k;
        for(int i=1; i<n; i++) y[i] = (int)next(0,k+1);
        java.util.Arrays.sort(y);
        for(int i=0; i<n; i++) x[i] = y[i+1]-y[i];
        return x;
    }
    
    /** Like nextPointOnSimplex(int,int), but allows specification of
     * a set of limits such that the returned point (x_0,...,x_{n-1})
     * also satisfies 0<=x_i<=limit[i] for each x_i. 
     */
    public int[] nextPointOnSimplex(int k, int n, int[] limit) {
        assert n == limit.length;
        int slimit=0;
        for(int i=0; i<n; i++) slimit += limit[i];
        System.err.println("k="+k+", slimit-k="+(slimit-k)+", slimit="+slimit);
        assert k <= slimit;
        if(k > slimit-k) {
            int[] u = nextPointOnSimplex(slimit-k, n, limit);
            for(int i=0; i<n; i++) u[i] = limit[i]-u[i];
            return u;
        } else {
            int[] x;
            do {
                x = nextPointOnSimplex(k,n);
                int i;
                for(i=0; i<n; i++)
                    if(x[i]>limit[i]) break;
                if(i==n) break;
            } while(true);
            return x;
        }
    }

    public boolean[] choose(int k, int n) {
        boolean[] chosen = new boolean[n];
        int[] which = nextInjection(k, n);
        for(int i=0; i<k; i++) chosen[which[i]] = true;
        return chosen;
    }
}
