// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.netlist;

import java.util.NoSuchElementException;

import com.avlsi.netlist.AbstractNetlist;

/**
   Interface to a netlist iterator, for type safety.
 */

public interface AbstractNetlistIterator {
    /**
       Returns true if the iteration has more elements.
       @return true if iterator has more elements
     */
    boolean hasNext();

    /**
       Returns the next AbstractNetlist in the iteration.
       @return the next AbstractNetlist
       @throws NoSuchElementException iteration has no more elements
     */
    AbstractNetlist next();
}
