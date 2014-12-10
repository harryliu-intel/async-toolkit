/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */
package com.avlsi.io;

import java.io.InputStream;

import java.io.FileNotFoundException;

import com.avlsi.io.SearchPathFile;
import com.avlsi.io.SearchPathDirectory;

import com.avlsi.util.container.StringContainerIterator;


/**
   Interface to a search path.
 */
public interface SearchPath {

    /**
       @param fileName The name of the file to open relative to the entries in the SearchPath. Can also
       be an absolute file name.
       @return The first InputStream that could be opened for a file whose name was a search path
       entry with the specified file name appended.
     */
    InputStream openFileAsStream(final String fileName) throws FileNotFoundException;

    /**
       @param fileName The name of the file to find relative to the entries in the SearchPath.  Can also
       be an absolute file name.
       @return A SearchPathFile that encapsulates a reference to the file within the search path.
       If there are multiple files in the search path that would match the file name, a reference to
       to first one that can be read will be returned.
     */
    SearchPathFile findFile( final String fileName ) throws FileNotFoundException;

    /**
     * Searches for each element of <code>fileNames</code> in successive
     * directories of the search path.  All of the <code>fileNames</code>
     * are attempted in one search path entry before moving to the next 
     * search path entry.  The first matching file is returned.
     *
     * @param fileNames The file names to find relative to the
     *   entries in the SearchPath.  Can also contain absolute file names.
     *
     * @return A SearchPathFile that encapsulates a reference to the file
     *   within the search path.  If there are multiple files in the search
     *   path that would match the file name, a reference to to first one
     *   that can be read will be returned.
     *
     * @throws FileNotFoundException  If no matching file could be found.
     * @throws IllegalArgumentException  If <code>fileNames</code> does
     *   not contain at least entry.
     */
    SearchPathFile findFirstFileOf(String[] fileNames)
        throws FileNotFoundException;

    /**
       @param dirName The name of the directory to find relative to the entries in the SearchPath.  Can also
       be an absolute directory name.
       @return A SearchPathFile that encapsulates a reference to the directory within the search path.
       If there are multiple directories in the search path that would match the directory name, a reference to
       to first one that can be read will be returned.
     */
    SearchPathDirectory findDirectory( final String dirName ) throws FileNotFoundException;

    /**
       Returns the search path as a string.  The components of the path are seperated by File.pathSeparatorChar
       @return The search path as a string.
     **/
    String getSearchPathString();

    /**
       @return An iterator that iterates over the entries in the SearchPath
     */
    StringContainerIterator getSearchPath();

    /**
     * Returns true if the specified directory is part of this path.
     * (Note that being a subdirectory of part of this path isn't good
     * enough.)  This is used for verifying module names.
     **/
    boolean containsDirectory(String dirName);
}
