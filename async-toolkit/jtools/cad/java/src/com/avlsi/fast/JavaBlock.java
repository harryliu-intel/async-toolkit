// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast;

import com.avlsi.util.debug.Debug;

public class JavaBlock extends BlockCommon {
    public JavaBlock() { } 

    public String getType() {
        return BlockInterface.JAVA;
    }

    public BlockInterface merge(BlockInterface o) {
        Debug.assertTrue(false, "JavaBlock does not support merge");
        return null;
    }

    public BlockInterface replace(BlockInterface o) {
        Debug.assertTrue(false, "JavaBlock does not support replace");
        return null;
    }
}
