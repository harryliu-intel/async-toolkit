/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast;

import com.avlsi.fast.BlockCommon;
import com.avlsi.prs.ProductionRuleSet;
import com.avlsi.util.debug.Debug;

/**
 * Interface to a PRS block.
 **/
public class PrsBlock extends BlockCommon {
    private ProductionRuleSet productionRuleSet;

    public PrsBlock() {
        this.productionRuleSet = new ProductionRuleSet();
    }

    public String getType() {
        return BlockInterface.PRS;
    }

    /**
     * Absorbs the production rules from its refinement parent into
     * itself.
     **/
    public void refineFrom(final BlockInterface o) {
        super.refineFrom(o);

        final PrsBlock parent = (PrsBlock) o;
        productionRuleSet.refineFrom(parent.productionRuleSet);
    }

    public BlockInterface merge(BlockInterface o) {
        Debug.assertTrue(false, "PrsBlock doesn't support all BlockInterface functionality");
        return null;
    }
    public BlockInterface replace(BlockInterface o) {
        Debug.assertTrue(false, "PrsBlock doesn't support all BlockInterface functionality");
        return null;
    }

    public ProductionRuleSet getProductionRuleSet() {
        return productionRuleSet;
    }

    public String toString() {
        return "PrsBlock " + productionRuleSet;
    }
}
