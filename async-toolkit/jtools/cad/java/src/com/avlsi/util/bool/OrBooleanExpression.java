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
import java.util.Iterator;

/**
 * Boolean expression that is a conjunction of other Boolean expressions.
 *
 * @author Aaron Denney
 * @version $Name:  $ $Date$
 **/
public class OrBooleanExpression extends AbstractBooleanExpression
            implements OrBooleanExpressionInterface {
    private static final boolean debug = true;
    private final ArrayList terms;
    
    /**
     * Constructor.
     **/
    public OrBooleanExpression(boolean nonNegated, Collection terms) {
        super(nonNegated);
        if (debug) {
            this.terms = new ArrayList();
            Iterator i=terms.iterator();
            while (i.hasNext()) {
                AbstractBooleanExpression e =
                    (AbstractBooleanExpression) i.next();
                this.terms.add(e);
            }
        } else {
            this.terms = new ArrayList(terms);
        }
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
        Iterator i=terms.iterator();
        ArrayList newTerms = new ArrayList();
        while (i.hasNext()) {
            AbstractBooleanExpression e =
                (AbstractBooleanExpression) i.next();
            if (e instanceof AndBooleanExpressionInterface &&
                    ((AndBooleanExpressionInterface)e).oneLevelConjunction()) {
                newTerms.add(e);
            } else {
                newTerms.addAll(e.DNFForm().getDisjuncts());
            }
        }

        return new OrBooleanExpression(true, newTerms);
    }

    public AndBooleanExpressionInterface CNFForm() {
        if (!sense) {
            return AndBooleanExpression().CNFForm();
        }

        // Convert all sub-expressions to CNFs
        Iterator i=terms.iterator();
        ArrayList CNFs = new ArrayList();
        while (i.hasNext()) {
            AbstractBooleanExpression e =
                (AbstractBooleanExpression) i.next();
            CNFs.add(e.CNFForm());
        }

        // Breed them together:
        // To be satisfied, all disjuncts must be satisfied, which
        // means at least one conjunct from each disjunct must be satisfied
        // so we loop through all possible combinations.
        i=CNFs.iterator();
        if (!i.hasNext()) {
            // We have no CNFs.  We are an empty or expression, so trivially
            // false.  This means we need to return an and of an empty or.
            // Since we are empty, we can use ourselves.
            Collection us = new ArrayList();
            us.add(this);
            return new AndBooleanExpression(true, us);
        }
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

    public Collection getDisjuncts() {
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
        int result = 0;
        for (Iterator i = terms.iterator(); i.hasNext(); ) {
            result += i.next().hashCode();
        }   
        return result;
    }

    /* Will go away in re-factoring */
    private boolean inDNFForm() {
        for (Iterator i = terms.iterator(); i.hasNext();) {
            BooleanExpressionInterface e = (BooleanExpressionInterface)i.next();
            if (!(e instanceof AndBooleanExpressionInterface) ||
                    !((AndBooleanExpressionInterface)e).oneLevelConjunction()) {
                return false;
            }
        }
        return true;
    }


    public boolean oneLevelDisjunction() {
        if (!sense) {
            return false;
        }
        for (Iterator i = terms.iterator();i.hasNext();) {
            BooleanExpressionInterface e =
               (BooleanExpressionInterface) i.next();
            if (!(e instanceof AbstractAtomicBooleanExpression)) {
                return false;
            }
        }
        return true;
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
