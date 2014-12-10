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

/**
 * Represents a function mapping from (Object, Object) to boolean.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public interface BinaryPredicate<A,B> {
    boolean evaluate(A a, B b);

    class Not<A,B> implements BinaryPredicate<A,B> {
        private final BinaryPredicate predicate;
        public Not(final BinaryPredicate predicate) {
            this.predicate = predicate;
        }
        public boolean evaluate(final A a, final B b) {
            return !predicate.evaluate(a, b);
        }
    }
}
