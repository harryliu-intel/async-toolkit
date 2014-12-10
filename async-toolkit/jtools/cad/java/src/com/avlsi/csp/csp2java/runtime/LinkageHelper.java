package com.avlsi.csp.csp2java.runtime;

import java.util.Arrays;
import java.util.ArrayList;
import java.util.Collection;

import com.avlsi.tools.tsim.Arbiter.Term;

public class LinkageHelper {
    public static abstract class NeutralityUnroller {
        public abstract Term[] evaluate(final CspInteger i)
            throws InterruptedException;
        public Term[] unroll(final CspInteger min, final CspInteger max)
            throws InterruptedException {
            final Collection result = new ArrayList();
            for (CspInteger i = min; i.compareTo(max) <= 0;
                 i = i.add(CspInteger.ONE)) {
                result.addAll(Arrays.asList(evaluate(i)));
            }
            return (Term[]) result.toArray(new Term[0]);
        }
    }
    public static abstract class GuardUnroller {
        public abstract Term[][] evaluate(final CspInteger i)
            throws InterruptedException;
        public Term[][] unroll(final CspInteger min, final CspInteger max)
            throws InterruptedException {
            final Collection result = new ArrayList();
            for (CspInteger i = min; i.compareTo(max) <= 0;
                 i = i.add(CspInteger.ONE)) {
                result.addAll(Arrays.asList(evaluate(i)));
            }
            return (Term[][]) result.toArray(new Term[0][0]);
        }
    }
}
