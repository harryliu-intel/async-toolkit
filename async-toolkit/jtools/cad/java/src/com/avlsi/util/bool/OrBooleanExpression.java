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
 **/
public class OrBooleanExpression extends AbstractBooleanExpression
            implements OrBooleanExpressionInterface {
    private static final boolean debug = true;
    private final ArrayList<BooleanExpressionInterface> terms;
    
    /**
     * Constructor.
     **/
    public OrBooleanExpression(boolean nonNegated,
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
    public AndBooleanExpression AndBooleanExpression() {
        return new AndBooleanExpression(!sense, negateTerms(terms));
    }

    /**
     * Negate Me.
     **/
    public BooleanExpressionInterface negated() {
        return new OrBooleanExpression(!sense, terms);
    }
        
    public OrBooleanExpressionInterface DNFForm() {
        if (!sense) {
            return AndBooleanExpression().DNFForm();
        }

        /* Should go away, and be checked later. */
        if (inDNFForm()) {
            return this;
        }

        /* at least one sub-term is not a one-level-conjunction */
        ArrayList<BooleanExpressionInterface> newTerms = new ArrayList<>();
        terms.forEach(e -> {
            if (e instanceof AndBooleanExpressionInterface &&
                    ((AndBooleanExpressionInterface) e).oneLevelConjunction()) {
                newTerms.add(e);
            } else {
                newTerms.addAll(e.DNFForm().getDisjuncts());
            }
        });

        return new OrBooleanExpression(true, newTerms);
    }

    public AndBooleanExpressionInterface CNFForm() {
        if (!sense) {
            return AndBooleanExpression().CNFForm();
        }

        // Convert all sub-expressions to CNFs
        final List<AndBooleanExpressionInterface> CNFs =
            terms.stream()
                 .map(e -> e.CNFForm())
                 .collect(Collectors.toList());

        // Breed them together:
        // To be satisfied, all disjuncts must be satisfied, which
        // means at least one conjunct from each disjunct must be satisfied
        // so we loop through all possible combinations.
        if (CNFs.isEmpty()) {
            // We have no CNFs.  We are an empty or expression, so trivially
            // false.  This means we need to return an and of an empty or.
            // Since we are empty, we can use ourselves.
            return new AndBooleanExpression(true, Collections.singleton(this));
        }

        Iterator i=CNFs.iterator();
        Collection conjunctAccumulator = ((AndBooleanExpressionInterface)i.next()).getConjuncts();
        while (i.hasNext()) {
            Collection newConjuncts = ((AndBooleanExpressionInterface)i.next()).getConjuncts();
            ArrayList result = new ArrayList();
            for (Iterator j = newConjuncts.iterator(); j.hasNext();) {
                Collection disjunctsR =
                    ((OrBooleanExpressionInterface)j.next()).getDisjuncts();
                for (Iterator k = conjunctAccumulator.iterator(); k.hasNext();) {
                Collection disjunctsL = 
                    ((OrBooleanExpressionInterface)k.next()).getDisjuncts();
                    ArrayList tempConjunct = new ArrayList();
                    tempConjunct.addAll(disjunctsL);
                    tempConjunct.addAll(disjunctsR);
                    result.add(new OrBooleanExpression(true, tempConjunct));
                }
            }
            conjunctAccumulator = result;
        }
        
        return new AndBooleanExpression(true, conjunctAccumulator);
    }

    public Collection<BooleanExpressionInterface> getDisjuncts() {
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
        if (obj instanceof OrBooleanExpressionInterface) {
            return equals((OrBooleanExpressionInterface)obj);
        }
        /* should technically check for negated and statements, but
         * that would be expensive, and is unneeded for our uses. */
        return false;
    }

    public boolean equals(OrBooleanExpressionInterface obj) {
        return (sense == obj.getSense()) &&
            (terms.containsAll(obj.getDisjuncts())) &&
            (obj.getDisjuncts().containsAll(terms));
    }

    public int hashCode() {
        return terms.hashCode();
    }

    /* Will go away in re-factoring */
    private boolean inDNFForm() {
        return terms.stream()
                    .allMatch(e -> e instanceof AndBooleanExpressionInterface &&
                                   ((AndBooleanExpressionInterface) e).
                                        oneLevelConjunction());
    }


    public boolean oneLevelDisjunction() {
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
        return toUserVisibleString(terms, "false", '|');
    }

    public void visitWith(BooleanExpressionVisitorInterface visitor) {
        visitor.visit(this);
    }

}
