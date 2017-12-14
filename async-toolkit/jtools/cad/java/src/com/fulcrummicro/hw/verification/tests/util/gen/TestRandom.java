/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.fulcrummicro.hw.verification.tests.util.gen;

import com.fulcrummicro.hw.verification.util.gen.NumberGenerator;
import com.fulcrummicro.hw.verification.util.gen.RandomNumberGenerator;

public class TestRandom {
    static public void main(String args[]) throws Exception {
        TestRandom t=new TestRandom();
        t.testI();
    }

    public void testI() throws Exception {
        RandomNumberGenerator g = new RandomNumberGenerator("testI");
        int sum=0, n=1000000;
        for(int i=0; i<n; i++) {
            if(g.nextWeightedBoolean(0.2))
                sum++;
        }
        System.out.println(((double)sum)/n);
    }

    public void testH() throws Exception {
        RandomNumberGenerator g = new RandomNumberGenerator("testH");
        for(int i=0; i<100; i++) {
            System.out.println(Long.toHexString(g.next(32)));
        }
    }

    public void testInjection(NumberGenerator g, 
                              int domain, int range, int n) {
        for(int j=0; j<n; j++) {
            int[] m = g.nextInjection(domain,range);

            for(int i=0; i<m.length; i++)
                System.out.print(m[i]+" ");
            System.out.println();

            boolean[] used=new boolean[range];
            for(int i=0; i<domain; i++) {
                assert !used[m[i]];
                assert 0<=m[i] && m[i]<range;
                used[m[i]]=true;
            }
        }
    }
    
    public void testG() throws Exception {
        testInjection(new RandomNumberGenerator("testG"), 
                      30, 100, 10000);
    }
    
    public void testF() throws Exception {
        RandomNumberGenerator g = new RandomNumberGenerator("testF");
        int n=30;
        int[] lim=new int[n];
        for(int i=0; i<n; i++) lim[i]=340;
        for(int j=0; j<100; j++) {
            int[] x = g.nextPointOnSimplex(1000, n, lim);
            for(int i=0; i<x.length; i++)
                System.out.print(x[i]+" ");
            System.out.println();
        }
    }
    
    public void testE() throws Exception {
        RandomNumberGenerator g = new RandomNumberGenerator("testE");
        for(int j=0; j<100; j++) {
            int[] x = g.nextPointOnSimplex(100, 10);
            for(int i=0; i<x.length; i++)
                System.out.print(x[i]+" ");
            System.out.println();
        }
    }
    
    public void testD() throws Exception {
        RandomNumberGenerator g = new RandomNumberGenerator("testD");
        String[] s = new String[] {"a", "b", "c", "d"};
        String[] t;
        for(int i=0; i<100; i++) {
            t=(String[])g.permuteArray(s);
            for(int j=0; j<t.length; j++) {
                System.out.print(t[j]+" ");
            }
            System.out.println();
        }
    }
    
    // test small intervals
    public void testC() throws Exception {
        RandomNumberGenerator g = new RandomNumberGenerator("testC");
        for(int i=0; i<10000; i++) {
            System.out.println(g.next(-500, 500));
        }
    }

    // test large intervals
    public void testB() throws Exception {
        RandomNumberGenerator g = new RandomNumberGenerator("testB");
        for(int i=0; i<1000000; i++) {
            System.out.println(g.next(0, 1L<<62)>>59);
        }
    }

    // test really large intervals
    public void testA() throws Exception {
        RandomNumberGenerator g = new RandomNumberGenerator("testA");
        for(int i=0; i<10000; i++) {
            System.out.println
                (Long.toHexString
                 (g.next(1L<<63, 1L<<63)));
        }
    }
}
