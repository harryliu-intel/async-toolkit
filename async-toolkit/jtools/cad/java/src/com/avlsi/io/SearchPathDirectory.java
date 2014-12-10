/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */
package com.avlsi.io;
import com.avlsi.io.SearchPathDirectoryIterator;

import java.io.FileFilter;

/**
   An iterface that encapsulates a directory in a SearchPath.
   @see com.avlsi.io.SearchPath
 */
public interface SearchPathDirectory {

    /**
       Gets the absolute name of the directory in the search path.
       If the directory was found in some sort of archive in the SearchPath the implementation should
       try to do something reasonable.  The returned string must uniquely identify the directory
       referenced by this interface.  If two instances of an implementation of this interface
       reference the same directory then the strings returned by this method MUST be the same.
       @return The absolute name of the directory in the SearchPath.
     */
    String getName();

   
    /**
       @return An iterator that iterates over all the sub directories in the encapsulated directory.
     */
    SearchPathDirectoryIterator getSubDirectories();

    /**
       @return An iterator that iterates over all the files in the encapsulated directory.
     */
    SearchPathFileIterator getFiles();

    /**
       Gets an iterator that iterates over all the files in the encapsulated directory that satisfy the filter.
       @param filter The filter through which file must return before it is returned by the iterator.
       @return An iterator that iterates over all the files in the encapsulated directory that satisfy the filter.
    */
    SearchPathFileIterator getFiles( SearchPathFileFilter filter );
}
