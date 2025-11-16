// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.container;

import java.util.Iterator;
import java.util.NoSuchElementException;


public class Iterator2StringContainerIteratorAdapter 
    implements StringContainerIterator {


    private Iterator m_InnerIterator;

    public Iterator2StringContainerIteratorAdapter( Iterator innerIter ) {
	m_InnerIterator = innerIter;
    }
    

    public boolean hasNext() {
	return m_InnerIterator.hasNext();
    }

    /** @throws NoSuchElementException **/
    public String next() {
	return ( String ) m_InnerIterator.next();
    }
}
