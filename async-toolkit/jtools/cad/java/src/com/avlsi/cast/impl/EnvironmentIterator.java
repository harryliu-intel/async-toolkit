// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast.impl;

import java.util.NoSuchElementException;


import com.avlsi.cast.impl.Value;

/**
   Interface to an iterator that iterates over all the values in an environment.
 */
public interface EnvironmentIterator {
    /**
       Determines if the iteration has another element.
       @return true if there is another element in the iteration, false otherwise.
     */
    boolean hasNext();

    /**
       Retrieves the next element in the iteration.
       @return The next element in the iteration.
       @throws NoSuchElementException
     */
    EnvironmentEntry next();
//    Value next();
}
