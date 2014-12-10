/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast;

import com.avlsi.cell.CellImpl;
import com.avlsi.fast.BlockInterface;
import com.avlsi.util.debug.Debug;

public class CellBlock extends BlockCommon {
    public String getType() {
        return BlockInterface.CELL;
    }

    public BlockInterface merge(BlockInterface o) {
        Debug.assertTrue(false, "CellBlock does not support merge");
        return null;
    }

    // The inherited refineFrom() is fine

    public BlockInterface replace(BlockInterface o) {
        Debug.assertTrue(false, "CellBlock does not support replace");
        return null;
    }
}
