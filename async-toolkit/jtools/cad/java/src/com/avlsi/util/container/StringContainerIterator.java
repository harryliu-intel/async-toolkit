// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */



package com.avlsi.util.container;


import java.util.NoSuchElementException;

/**
   Interface of an iterator that iterates over a container that
   contains strings.
 */
public interface StringContainerIterator {

    /**
       Determines if there is another string in the container
       this iterator is iterating.
       @return true if there is another string in the collection, false otherwise.
     */
    boolean hasNext();
    
    /**
       Retrieves the next string in the container this iterator is iterating.
       Throws if there is not another string in the container.
       @return The next String.
       @throws NoSuchElementException
     */
    String next();

}
