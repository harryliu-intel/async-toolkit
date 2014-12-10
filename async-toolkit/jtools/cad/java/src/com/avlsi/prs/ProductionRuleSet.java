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

package com.avlsi.prs;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;

import com.avlsi.util.container.AliasedSet;

/**
 * Class to represent the set of production rules for a Cell.
 *
 * @see com.avlsi.cell.CellInterface
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class ProductionRuleSet {
    /**
     * List of the production rules.
     **/
    private ArrayList productionRuleList = new ArrayList();

    /**
     * Returns an unmodifiable iterator through the production rules, 
     * in the order they were added.
     **/
    public Iterator getProductionRules() {
        return Collections.unmodifiableList(productionRuleList).iterator();
    }

    /**
     * Adds a reference to the production rule to the end of the list.
     * Any changes to the production rule passed in will be change
     * the stored production rule.
     **/
    public void addProductionRule(final ProductionRule pr) {
        productionRuleList.add(pr);
    }

    /**
     * Ensures that all nodes use only the canonical node name, as defined
     * in <code>aliases</code>.
     **/
    public void canonicalizeNames(final AliasedSet aliases) {
        for (int i = 0; i < productionRuleList.size(); ++i) {
            final ProductionRule pr =
                (ProductionRule) productionRuleList.get(i);

            productionRuleList.set(i, pr.canonicalizeNames(aliases));
        }
    }

    /**
     * Absorbs production rules from the refinement parent.
     **/
    public void refineFrom(final ProductionRuleSet parent) {
        productionRuleList.addAll(parent.productionRuleList);
    }

    /**
     * String representation for debugging only.
     **/
    public String toString() {
        final StringBuffer sb = new StringBuffer();
        for (final Iterator iPr = getProductionRules(); iPr.hasNext(); ) {
            final ProductionRule pr = (ProductionRule) iPr.next();
            sb.append(pr.toString()).append('\n');
        }
        return sb.toString();
    }

    public int size() {
        return productionRuleList.size();
    }
}
