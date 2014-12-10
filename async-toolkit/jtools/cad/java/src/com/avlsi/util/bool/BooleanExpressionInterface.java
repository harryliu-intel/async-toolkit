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

package com.avlsi.util.bool;

/**
 * Base interface for boolean expressions.
 * Throughout this interface, I use the following terms:
 * <dl>
 * <dt>Conjunction
 * <dd>An expression that is an ''and'' of other terms.
 * <dt>Disjunction
 * <dd>An expression that is an ''or'' of other terms.
 * <dt>Conjunct
 * <dd>An individual term of a conjunction.  Will be a disjunction
 *     in a CNF, but need not be generally.
 * <dt>Disjunct
 * <dd>An individual term of a disjunction.  Will be a conjunction
 *     in a DNF, but need not be generally.
 **/
public interface BooleanExpressionInterface {
    /**
     * Return a boolean expression that is the negation of this expression.
     **/
    BooleanExpressionInterface negated();
    /**
     * Return a boolean expression that is the this one in DNF.
     * DNF -- Disjunctive Normal Form
     * An OrBooleanExpressionInterface whose terms are all of type
     * AndBooleanExpressionInterface whose terms are all atomic.
     **/
    OrBooleanExpressionInterface DNFForm();
    /**
     * Return a boolean expression that is the this one in CNF.
     * CNF -- Conjunctive Normal Form
     * An AndBooleanExpressionInterface whose terms are all of type
     * OrBooleanExpressionInterface whose terms are all atomic.
     **/
    AndBooleanExpressionInterface CNFForm();
    /**
     * Returns a string suitable for inclusion in output files.
     * The string will look something like this:
     * <pre> ~(~_PReset|~a.0&amp;~be) </pre>
     **/
    String toUserVisibleString();

    /**
     * Let the visitor process <code>this</code>.  The implementation
     * should always be <code>visitor.visit(this)</code>.
     **/
    void visitWith(BooleanExpressionVisitorInterface visitor);
}
