/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */
package com.avlsi.io;
import java.util.NoSuchElementException;

import com.avlsi.io.SearchPathFile;


public interface SearchPathFileIterator {

    /**
       @return true if there is another file in the directory being iterated.  false otherwise.
     */
    boolean hasNext();

    /**
     * @return The next file in the directory being iterated.
     * @throws NoSuchElementException
     */
    SearchPathFile next();

}
