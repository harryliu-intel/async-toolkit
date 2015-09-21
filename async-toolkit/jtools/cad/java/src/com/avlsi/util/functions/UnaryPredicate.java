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
 * Represents a function mapping from Object to boolean.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
@FunctionalInterface
public interface UnaryPredicate<A> {
    boolean evaluate(A a);

    class Not<A> implements UnaryPredicate<A> {
        private final UnaryPredicate predicate;
        public Not(final UnaryPredicate predicate) {
            this.predicate = predicate;
        }
        public boolean evaluate(final A a) {
            return !predicate.evaluate(a);
        }
    }

    class Constant<A> implements UnaryPredicate<A> {
        private final boolean value;
        public Constant(final boolean value) {
            this.value = value;
        }
        public boolean evaluate(final A a) {
            return value;
        }
    }
}
