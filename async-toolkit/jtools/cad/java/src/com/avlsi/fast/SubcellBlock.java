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
 * Represents all the data stored in the subcell block of castv2.  Right now,
 * this is just a place holder to attach a directives block.
 **/

public class SubcellBlock extends BlockCommon {
    public SubcellBlock() {
    }

    public String getType() {
        return BlockInterface.SUBCELL;
    }

    public void refineFrom(final BlockInterface o) {
        super.refineFrom(o);
    }

    public BlockInterface merge(BlockInterface o) {
        Debug.assertTrue(false, "SubcellBlock doesn't support all BlockInterface functionality");
        return null;
    }
    public BlockInterface replace(BlockInterface o) {
        Debug.assertTrue(false, "SubcellBlock doesn't support all BlockInterface functionality");
        return null;
    }

    public String toString() {
        return "SubcellBlock()";
    }
}
