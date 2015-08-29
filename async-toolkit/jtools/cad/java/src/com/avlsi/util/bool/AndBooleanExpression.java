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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Boolean expression that is a conjunction of other Boolean expressions.
 *
 * @author Aaron Denney
 * @version $Name:  $ $Date$
 * @see OrBooleanExpression
 **/
public final class AndBooleanExpression extends AbstractBooleanExpression
            implements AndBooleanExpressionInterface {
    private static final boolean debug = true;
    private final ArrayList<BooleanExpressionInterface> terms;
    
    /**
     * Constructor.
     * Could add checks so that same type adds components rather then
     * self?
     **/
    public AndBooleanExpression(boolean nonNegated,
                                Collection<BooleanExpressionInterface> terms) {
        super(nonNegated);
        if (debug) {
            terms.forEach(t -> {
                    assert t instanceof AbstractBooleanExpression; });
        }
        this.terms = new ArrayList<>(terms);
    }

    /**
     * Apply DeMorgan's laws to get equivalent expression.
     **/
    public OrBooleanExpression OrBooleanExpression() {
        return new OrBooleanExpression(!sense, negateTerms(terms));
    }

    /**
     * Negate Me.
     **/
    public BooleanExpressionInterface negated() {
        return new AndBooleanExpression(!sense, terms);
    }
        
    public AndBooleanExpressionInterface CNFForm() {
        if (!sense) {
            return OrBooleanExpression().CNFForm();
        }

        /* Should go away, and be checked later. */
        if (inCNFForm()) {
            return this;
        }

        /* at least one sub-term is not a one-level-disjunction */
        ArrayList<BooleanExpressionInterface> newTerms = new ArrayList<>();
        terms.forEach(e -> {
            if (e instanceof OrBooleanExpressionInterface &&
                ((OrBooleanExpressionInterface)e).oneLevelDisjunction()) {
                newTerms.add(e);
            } else {
                newTerms.addAll(e.CNFForm().getConjuncts());
            }
        });

        return new AndBooleanExpression(true, newTerms);
    }

    public OrBooleanExpressionInterface DNFForm() {
        if (!sense) {
            return OrBooleanExpression().DNFForm();
        }

        // Convert all sub-expressions to DNFs
        final List<OrBooleanExpressionInterface> DNFs =
            terms.stream()
                 .map(e -> e.DNFForm())
                 .collect(Collectors.toList());

        // Breed them together:
        // To be satisfied, all disjuncts must be satisfied, which
        // means at least one conjunct from each disjunct must be satisfied
        // so we loop through all possible combinations.
        if (DNFs.isEmpty()) {
            // We have no DNFs.  We are an empty and expression, so trivially
            // true.  This means we need to return an or of and empty and.
            // Since we are empty, we can use ourselves.
            return new OrBooleanExpression(true, Collections.singleton(this));
        }

        Iterator i=DNFs.iterator();
        Collection disjunctAccumulator = ((OrBooleanExpressionInterface)i.next()).getDisjuncts();
        while (i.hasNext()) {
            Collection newDisjuncts = ((OrBooleanExpressionInterface)i.next()).getDisjuncts();
            ArrayList result = new ArrayList();
            // fold dnf stored in disjunctAccumulator and newDisjuncts
            // intor result.
            for (Iterator j = newDisjuncts.iterator(); j.hasNext();) {

                Collection conjunctsR =
                    ((AndBooleanExpressionInterface)j.next()).getConjuncts();
                for (Iterator k = disjunctAccumulator.iterator(); k.hasNext();) {
                Collection conjunctsL = 
                    ((AndBooleanExpressionInterface)k.next()).getConjuncts();
                    ArrayList tempDisjunct = new ArrayList();
                    // Each conjunct satisfies all of js 
                    tempDisjunct.addAll(conjunctsL);
                    tempDisjunct.addAll(conjunctsR);
                    result.add(new AndBooleanExpression(true, tempDisjunct));
                }
            }
            disjunctAccumulator = result;
        }
        
        return new OrBooleanExpression(true, disjunctAccumulator);
    }

    public Collection<BooleanExpressionInterface> getConjuncts() {
        return terms;
    }

    /**
     * Do not use unless you really know what you are doing.
     * Not entirely accurate.  This is a design decision; the only
     * real uses of this is aggregation of {conjuncts, disjuncts}
     * and equality tests for the testsuite.  The testsuite will make
     * do, and the simplification is ... not very good anyway.
     * We'll burn that bridge when we get any cast files that
     * have horribly complex production rules.
     **/
    public boolean equals(Object obj) {
        if (obj instanceof AndBooleanExpressionInterface) {
            return equals((AndBooleanExpressionInterface)obj);
        }
        /* should technically check for or statements, but that would be
         * expensive, and is unneeded for our uses. */
        return false;
    }

    public boolean equals(AndBooleanExpressionInterface obj) {
        return (sense == obj.getSense()) &&
            (terms.containsAll(obj.getConjuncts())) &&
            (obj.getConjuncts().containsAll(terms));
    }

    public int hashCode() {
        return terms.hashCode();
    }

    /* Will go away in re-factoring */
    private boolean inCNFForm() {
        return terms.stream()
                    .allMatch(e -> e instanceof OrBooleanExpressionInterface &&
                                   ((OrBooleanExpressionInterface) e).
                                        oneLevelDisjunction());
    }

    public boolean oneLevelConjunction () {
        return getSense() && AbstractBooleanExpression.oneLevel(terms);
    }

    public String toString() {
        return toUserVisibleString();
    }

    //
    // implementing BooleanExpressionInterface
    //
    public String toUserVisibleString() {
        // in AbstractBooleanExpression
        return toUserVisibleString(terms, "true", '&');
    }

    public void visitWith(BooleanExpressionVisitorInterface visitor) {
        visitor.visit(this);
    }
}
