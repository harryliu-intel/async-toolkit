/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.io;

import java.util.LinkedList;
import java.util.List;

import java.io.IOException;
import java.io.File;
import java.io.InputStream;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.BufferedReader;

/**
 * Utility functions for dealing with files.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class FileUtil {
    private FileUtil() {
        throw new AssertionError();
    }

    /**
     * Recursively deletes the directory hierarchy starting at
     * <code>file</code>.  If <code>file</code> is a directory, delete
     * the hierarchy under it.  If <code>file</code> is not, just delete
     * it.
     *
     * @return <code>true</code> if and only if the entire hierarchy was
     *     successfully deleted; <code>false</code> otherwise.
     *
     * @param file
     *        The file or directory to be deleted.
     **/
    public static boolean recursiveDelete(final /*@ non_null @*/ File file) {
        boolean allRemoved = true;
        final File[] files = file.listFiles();
        if (files != null)
            for (int i = 0; i < files.length; ++i)
                allRemoved &= recursiveDelete(files[i]);

        allRemoved &= file.delete();

        return allRemoved;
    }
    
    public static List getLines(final File file) throws IOException {
        final List lines = new LinkedList();
        final InputStream is = new FileInputStream( file );
        final Reader r = new InputStreamReader( is, "UTF-8" );
        final BufferedReader br =  new BufferedReader( r );
        String line;
        while((line = br.readLine()) != null) {
            lines.add(line);
        }
        return lines;
    }
}
