// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast;

import java.util.NoSuchElementException;

/**
 * Interface representing an iterator over BlockInterface.
 **/

public interface BlockIterator {
    void add(BlockInterface block);
    boolean hasNext();
    boolean hasPrevious();

    /**
     * @throws NoSuchElementException
     **/
    BlockInterface next();

    /**
     * @throws NoSuchElementException
     **/
    BlockInterface previous();

    void remove();
    void set(BlockInterface block);
    void merge(BlockInterface block);
}
