// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.io;

import java.util.NoSuchElementException;

import com.avlsi.io.SearchPathDirectory;

public interface SearchPathDirectoryIterator {

    /**
       @return true if there is another subdirectory of the directory being iterated, false otherwise.
     */
    boolean hasNext();

    /**
       @return The next directory in the directory being iterated.
       @throws NoSuchElementException
    */
    SearchPathDirectory next();
    
}
