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

package com.avlsi.util.functions;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

/**
 * Class holding "functional" style/lisp-like methods.
 *
 * @author Aaron Denney
 * @version $Name:  $ $Date$
 **/

public class Functional {
    // We don't want to be instantiated.
    private Functional() {
    }

    /**
     * ML's foldl, or Common Lisp's reduce.
     *
     * Returns the repeated application of f to the two frontmost
     * elements of <code>c</code>, with the result being stuck on the
     * front.  f(f(f(a0, a1), a2) ... )
     *
     * If the elements of <code>c</code> and the function <code>f</code>
     * form a group, then id often corresponds to the identity of the
     * group.
     *
     * Unlike Haskell's foldl (or ocaml's fold_left), will not use id
     * if the collection is not empty.
     *
     * @param id The fallback object, if the collection is empty.
     * @param f  The function to apply.
     * @param c  The collection 
     * 
     * @return id for an empty collection, or the result described above.
     **/
    public static Object foldl(Collection c, BinaryFunction f, Object id) {
        if (c.isEmpty()) {
            return id;
        }

        Iterator i = c.iterator();
        Object rv = i.next();
        while (i.hasNext()) {
            rv = f.execute(rv, i.next());
        }

        return rv;
    }

    /**
     * ML's foldr.  Somewhat slow and inefficient.  
     * Use <code>foldl</code> unless you need these precise semantics.
     *
     * Returns the repeated application of f to the two last
     * elements of <code>c</code>, with the result being stuck on the
     * end.  f(a0, f(a1, f( ..., f(a_n-1, a_n))))
     *
     * If the elements of <code>c</code> and the function <code>f</code>
     * form a group, then id often corresponds to the identity of the
     * group.
     *
     * Unlike Haskell's foldr, (or ocaml's fold_right) will not use id
     * if the collection is not empty.
     *
     * @param id The fallback object, if the collection is empty.
     * @param f  The function to apply.
     * @param c  The collection 
     * 
     * @return id for an empty collection, or the result described above.
     **/
    public static Object foldr(Collection c, BinaryFunction f, Object id) {
        if (c.isEmpty()) {
            return id;
        }

        Iterator i = c.iterator();
        return foldrIterator(i, f);
    }

    private static Object foldrIterator(Iterator i, BinaryFunction f) {
        Object rv = i.next();
        if (i.hasNext()) {
            rv = f.execute(rv, foldrIterator(i, f));
        }
        return rv;
    }

    /**
     * filter.
     * Returns a collection only having the elements of <code>c</code>
     * that satisfy <code>p</code>.
     **/
    public static Collection filter(Collection c, UnaryPredicate p) {
        Collection rv;
        // if (c instanceof Cloneable) {
        //     rv = (Collection)((Cloneable)c).clone();
        // } else {
            rv = new ArrayList(c);
        // }
        Iterator i = rv.iterator();
        while (i.hasNext()) {
            if (!p.evaluate(i.next())) {
                i.remove();
            }
        }
        return rv;
    }

    /**
     * filter.
     * Returns a collection only having the elements iterated over by
     * <code>i</code> that satisfy <code>p</code>.  Semantics might be slightly
     * different from filter(Collection, UnaryPredicate), otherwise, that
     * function should be rewritten in terms of this function.
     **/
    public static Collection filter(final Iterator i, final UnaryPredicate p) {
        final Collection rv = new ArrayList();
        while (i.hasNext()) {
            Object o = i.next();
            if (p.evaluate(o)) rv.add(o);
        }
        return rv;
    }

    /**
     * map -- Common Lisp's mapcar.  
     * 
     * if c has an order, it will be preserved.
     * 
     * @return { forall i in c : f(i) }
     **/
    public static Collection map(Collection c, UnaryFunction f) {
        ArrayList rv = new ArrayList(c.size());
        Iterator i = c.iterator();
        while (i.hasNext()) {
            rv.add(f.execute(i.next()));
        }
        return rv;
    }
}
