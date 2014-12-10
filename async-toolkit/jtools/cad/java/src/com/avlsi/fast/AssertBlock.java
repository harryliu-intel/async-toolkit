/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.fast;

import com.avlsi.cell.ExclusiveNodeSets;
import com.avlsi.prs.ProductionRuleSet;
import com.avlsi.util.debug.Debug;

/**
 * Represents all the data stored in the assert block of castv2.  Note
 * that to get _all_ the production rules of a cell you need to query
 * this block as well as the prs block.  This only stores the
 * simulator-only production rules; the prs block only stores the
 * general-purpose production rules.
 **/

public class AssertBlock extends BlockCommon {
    /**
     * The production rules created by the prs assertions.  All of
     * them should be "environmental" (simulator only).
     **/
    private final ProductionRuleSet productionRuleSet;

    private final ExclusiveNodeSets exclNodeSets;

    public AssertBlock() {
        this.productionRuleSet = new ProductionRuleSet();
        this.exclNodeSets = new ExclusiveNodeSets();
    }

    public String getType() {
        return BlockInterface.ASSERT;
    }

    /**
     * Absorbs the production rules and exclusive node sets from its
     * refinement parent into itself.
     **/
    public void refineFrom(final BlockInterface o) {
        super.refineFrom(o);

        final AssertBlock parent = (AssertBlock) o;
        productionRuleSet.refineFrom(parent.productionRuleSet);
        exclNodeSets.refineFrom(parent.exclNodeSets);
    }

    public BlockInterface merge(BlockInterface o) {
        Debug.assertTrue(false, "AssertBlock doesn't support all BlockInterface functionality");
        return null;
    }
    public BlockInterface replace(BlockInterface o) {
        Debug.assertTrue(false, "AssertBlock doesn't support all BlockInterface functionality");
        return null;
    }

    public ProductionRuleSet getProductionRuleSet() {
        return productionRuleSet;
    }

    public ExclusiveNodeSets getExclusiveNodeSets() {
        return exclNodeSets;
    }

    public String toString() {
        return "AssertBlock(" + productionRuleSet + exclNodeSets + ")";
    }
}
