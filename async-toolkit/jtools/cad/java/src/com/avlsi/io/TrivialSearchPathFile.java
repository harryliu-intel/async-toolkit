/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */
package com.avlsi.io;

import java.io.FileNotFoundException;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.FileInputStream;
import com.avlsi.io.SearchPathFile;

import com.avlsi.util.debug.Debug;

public class TrivialSearchPathFile implements SearchPathFile {

    private File m_File;

    /**
     * Return the canonical file of <code>theFile</code> if possible, or
     * <code>theFile</code> if not (due to an <code>IOException</code> being
     * thrown).
     **/
    private static File canonizeIfPossible(final File theFile) {
        try {
            return theFile.getCanonicalFile();
        } catch (IOException e) {
            return theFile;
        }
    }

    /**
     * Returns true if <code>theFile</code> is canonical, i.e., its name equals
     * to the name of the <code>File</code> returned by
     * <code>theFile.getCanonicalPath()</code>.  If an <code>IOException</code>
     * is thrown, return false.
     **/
    private static boolean isCanonical(final File theFile) {
        final String canon;
        try {
            canon = theFile.getCanonicalPath();
        } catch (IOException e) {
            return false;
        }
        return canon.equals(theFile.getPath());
    }

    public TrivialSearchPathFile( final File theFile ) {
        this(theFile, false);
    }

    /**
     * Construct a new <code>TrivialSearchPathFile</code>.
     *
     * @param isCanon If true, then <code>theFile</code> is assumed to be
     * canonical (in the sense it's an object returned by <code>File</code>'s
     * <code>getCanonicalFile</code> method).
     **/
    public TrivialSearchPathFile( final File theFile, boolean isCanon ) {
        assert theFile.isFile();

        // XXX: We would like to check that if the caller claims theFile is
        // canonical, that it actually is.  But, there is no good way to check
        // this, since the filesystem might have changed between when this
        // constructor is called and now, so that theFile might have been
        // canonical in the caller, but is no longer.  On the other hand,
        // applications that is expected to be robust to such changes should
        // not be using this class anyway.
        assert !isCanon || isCanonical(theFile);

        m_File = isCanon ? theFile : canonizeIfPossible(theFile);
    }

    public TrivialSearchPathFile( final String fileName ) {
        
        this( new File( fileName ) );
    }


    public String getName() {
        return m_File.getAbsolutePath();
    }
        
    public InputStream getStream() throws FileNotFoundException {
        if ( m_File.isFile() ) {
            return new FileInputStream( m_File );
        }
        else {
            throw new FileNotFoundException();
        }
    }

    public File getFile() {
        return m_File;
    }

    public boolean canRead() {
        return m_File.canRead();
    }
    
    public boolean exists() {
        return m_File.exists();
    }
    
    public long lastModified() {
        final long ret = m_File.lastModified();
        if ( ret == 0 ) {
            return -1L;
        }
        else {
            return ret;
        }
    }

    public long length() {
        return m_File.length();
    }
}
