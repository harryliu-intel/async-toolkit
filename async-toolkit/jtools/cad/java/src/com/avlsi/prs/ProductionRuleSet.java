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
import java.util.stream.Collectors;
import java.util.stream.Stream;

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
    private ArrayList<ProductionRule> productionRuleList = new ArrayList<>();

    /**
     * Returns an unmodifiable iterator through the production rules, 
     * in the order they were added.
     **/
    public Iterator<ProductionRule> getProductionRules() {
        return Collections.unmodifiableList(productionRuleList).iterator();
    }

    public Stream<ProductionRule> stream() {
        return productionRuleList.stream();
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
     * Adds all production rules from the given set to the end of the list.
     **/
    public void addProductionRule(final ProductionRuleSet prs) {
        productionRuleList.addAll(prs.productionRuleList);
    }

    /**
     * Ensures that all nodes use only the canonical node name, as defined
     * in <code>aliases</code>.
     **/
    public void canonicalizeNames(final AliasedSet aliases) {
        final int elems = productionRuleList.size();
        for (int i = 0; i < elems; ++i) {
            final ProductionRule pr = productionRuleList.get(i);
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
        return stream().map(pr -> pr + "\n").collect(Collectors.joining());
    }

    public int size() {
        return productionRuleList.size();
    }
}
