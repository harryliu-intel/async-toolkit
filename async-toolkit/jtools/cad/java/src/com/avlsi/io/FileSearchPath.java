/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.io;

import java.io.File;

import java.io.InputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;
import java.util.NoSuchElementException;


import com.avlsi.util.text.StringUtil;

import com.avlsi.io.SearchPath;
import com.avlsi.io.SearchPathFile;
import com.avlsi.io.SearchPathFileFilter;
import com.avlsi.io.TrivialSearchPathFile;


import com.avlsi.util.container.StringContainerIterator;


/**
 * This class represents a search path for files.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class FileSearchPath implements SearchPath {

    public interface SearchPathFileFactory {
        SearchPathFile getSearchPathFile(File file);
        SearchPathFile getSearchPathFile(File file, boolean isCanon);
    }

    public static final class DefaultSearchPathFileFactory 
        implements SearchPathFileFactory {
        public SearchPathFile getSearchPathFile(File file) {
            return new TrivialSearchPathFile(file);
        }
        public SearchPathFile getSearchPathFile(File file, boolean isCanon) {
            return new TrivialSearchPathFile(file, isCanon);
        }
    }
    
    public static final class TrackingSearchPathFileFactory
        implements SearchPathFileFactory {

        private final Collection history;
        private static final class TrackingSearchPathFile
            extends TrivialSearchPathFile {
            
            private final Collection history;
            public TrackingSearchPathFile(File file, Collection history) {
                super(file);
                this.history = history;
            }
            public TrackingSearchPathFile(File file, boolean isCanon, Collection history) {
                super(file,isCanon);
                this.history = history;
            }
            public InputStream getStream()
                throws java.io.FileNotFoundException {
                history.add(getName());
                return super.getStream();
            }
        }

        public SearchPathFile getSearchPathFile(File file) {
            return new TrackingSearchPathFile(file,history);
        }
        public SearchPathFile getSearchPathFile(File file, boolean isCanon) {
            return new TrackingSearchPathFile(file,isCanon,history);
        }
        public TrackingSearchPathFileFactory(final Collection history) {
            this.history = history;
        }
    }

    private static final class FileSearchPathDirectory implements SearchPathDirectory { 
        private final File m_File ;
        private final File[] m_SubDirs;
        private final File[] m_Files;
        private final SearchPathFileFactory m_Factory;

        public FileSearchPathDirectory( final File theFile ,
                                        final SearchPathFileFactory factory) {
            m_File = theFile;
            m_Factory = factory;

            final File[] entries = theFile.listFiles();

            if ( entries != null ) {
            
                int readPos;
                int numDirs = 0;
                int numFiles = 0;
                
                for ( readPos = 0 ; readPos < entries.length ; ++readPos ) {
                    if ( entries[readPos].canRead() ) {
                        if ( entries[readPos].isDirectory() ) {
                            ++numDirs;
                        }
                        else {
                            ++numFiles;
                        }
                    }
                }

                m_SubDirs = new File[numDirs];
                m_Files = new File[numFiles];
                
                int dirWritePos = 0;
                int fileWritePos = 0;

                for ( readPos = 0 ; readPos < entries.length ; ++readPos ) {
                
                    if ( entries[readPos].canRead() ) {
                        if ( entries[readPos].isDirectory() ) {
                        
                            m_SubDirs[dirWritePos] = entries[readPos];
                            ++dirWritePos;
                        }
                        else {
                            m_Files[ fileWritePos] = entries[readPos];
                            ++fileWritePos;
                        }
                    }
                }
            }
            else {
                m_SubDirs = null;
                m_Files = null;
            }
        }

        public String getName() {
            return m_File.getAbsolutePath();
        }
        
        public SearchPathDirectoryIterator getSubDirectories() {
            return new SearchPathDirectoryIterator() {

                    private final File[] dirs = m_SubDirs;
                    
                    private int currIndex = 0;

                    public final boolean hasNext() {
                        return ( dirs != null ) && ( currIndex < dirs.length );
                    }

                    public final SearchPathDirectory next() {
                    
                        if ( hasNext() ) {
                            final SearchPathDirectory ret = new FileSearchPathDirectory( dirs[ currIndex ], m_Factory );
                            ++currIndex;
                            return ret;
                        }
                        else {
                            throw new NoSuchElementException();
                        }
                        
                    }


                };
            
            

        }
        
        public SearchPathFileIterator getFiles() {
            return new SearchPathFileIterator() {
                    private final File[] files = m_Files;
                    private int currIndex;

                    public final boolean hasNext() {
                        return ( files != null ) && ( currIndex < files.length );
                    }

                    public final SearchPathFile next() {
                    
                        if ( hasNext() ) {
                            final SearchPathFile ret = m_Factory.getSearchPathFile( files[ currIndex ] );
                            ++currIndex;
                            return ret;
                        }
                        else {
                            throw new NoSuchElementException();
                        }
                        
                    }
                };
        }

        public SearchPathFileIterator getFiles( final SearchPathFileFilter f ) {
            return new SearchPathFileIterator() {
                    private final SearchPathFileIterator m_InnerIter = getFiles();
                    
                    private final SearchPathFileFilter m_Filter = f;

                    SearchPathFile m_NextFile = null;
                    
                    private void updateNextFile( ) {
                        while ( ( m_NextFile == null ) && ( m_InnerIter.hasNext() ) ){
                            final SearchPathFile currFile = ( SearchPathFile ) m_InnerIter.next();
                            
                            if ( f.accept( currFile  ) ) {
                                m_NextFile = currFile;
                            }
                        }
                    }

                    public final boolean hasNext() {
                        updateNextFile();
                        return m_NextFile != null;
                    }

                    public final SearchPathFile next() {
                        
                        if ( hasNext() ) {
                            final SearchPathFile ret = m_NextFile;
                            m_NextFile = null;
                            return ret;
                        }
                        else {
                            throw new NoSuchElementException();
                        }
                    }
                    
                };
        }

    }

    /**
     * Array of directory names to be searched.
     *
     * TODO: convert to a Vector.
     **/
    private String[] dirs;
    private final SearchPathFileFactory spfFactory;

    /**
     * Used to create <code>SearchPathFile</code>s.
     **/

    public FileSearchPath(final String path) {
        this(path, new DefaultSearchPathFileFactory());
    }
    
    /**
     * Constructs a new search path from the given colon separated path.
     **/
    public FileSearchPath(final String path,
                          final SearchPathFileFactory spfFactory) {
        this(path, System.getProperty( "user.home" ), spfFactory );
    }

    public FileSearchPath(final String path, final String homeDir) {
        this(path, homeDir, new DefaultSearchPathFileFactory());
    }

    public FileSearchPath(final String path, final String homeDir,
                          final SearchPathFileFactory spfFactory) {
        this(path, homeDir, System.getProperty( "user.name" ), spfFactory);
    }

    /**
     * Constructs a new search path from the given colon separated path.
     * Occurences of ~ are replaced with <code>homeDir</code>.
     **/
    public FileSearchPath(final String path, final String homeDir,
                          final String userName,
                          final SearchPathFileFactory spfFactory) {
        this.spfFactory = spfFactory;
	
        final String seperatorChar = System.getProperty("path.separator", ":");

        final StringTokenizer st = new StringTokenizer(path, seperatorChar);
        final List dirList = new ArrayList();

        while (st.hasMoreTokens()) {
            String dir = (String) st.nextToken();

            if (homeDir != null) {
                if (dir.equals("~"))
                    dir = homeDir;
                else if (dir.startsWith("~/"))
                    dir = homeDir + '/' + StringUtil.chompStart(dir, "~/");
                else {
                    final String start = '~' + userName + '/';
                    if (dir.startsWith(start))
                        dir = homeDir + '/' + StringUtil.chompStart(dir, start);
                }
            }

            if (dir.length() > 0)
                dirList.add(dir);
        }

        dirs = (String[]) dirList.toArray(new String[0]);
    }

    public FileSearchPath(final String[] dirs) {
        this(dirs, new DefaultSearchPathFileFactory());
    }

    /**
     * Constructs a search path from the specified array of paths
     **/
    public FileSearchPath(final String[] dirs,
                          final SearchPathFileFactory spfFactory) {
        this.spfFactory = spfFactory;
        this.dirs = new String[dirs.length];
        System.arraycopy(dirs, 0, this.dirs, 0, dirs.length);
    }

    /**
     * Returns the first file of name fileName found in the search path.
     * If the fileName is an an absolute path, return it.
     *
     * @exception FileNotFoundException  if the file could not be found
     *   in the search path
     **/
    public InputStream openFileAsStream(final String fileName)
        throws FileNotFoundException
    {
        final SearchPathFile theFile = findFile( fileName );
        return theFile.getStream();
    }

    private String generateErrorMessage( final String fileName ) {
        final StringBuffer errorMessage = new StringBuffer( fileName );
            
        final StringContainerIterator iter = getSearchPath();
        
        errorMessage.append( " not found in:" );
        
        boolean hasTilde = false;

        while ( iter.hasNext() ) {
            errorMessage.append( "\n    " );
            final String next = iter.next();
            if ( next.startsWith( "~" ) ) hasTilde = true;
            errorMessage.append( next );
        }
        errorMessage.append( '\n' );

        // an additional hint on ~ expansion limitations
        if ( hasTilde ) {
            errorMessage.append(
                "note: expansion of ~anotherUser in path is not supported\n" );
        }

        return errorMessage.toString();
    }

    private String generateErrorMessage(final String[] fileNames) {
        final StringBuffer files = new StringBuffer();

        for (int i = 0; i < fileNames.length; ++i)
            files.append(fileNames[i]).append(' ');

        return generateErrorMessage(files.toString());
    }

    public SearchPathFile findFile( final String fileName ) throws FileNotFoundException {
        final File absFileObj = new File( fileName );

        File currFileObj = null;

        if ( absFileObj.isAbsolute() ) {
            try {
                currFileObj = absFileObj.getCanonicalFile();  
            }
            catch ( IOException e ) {
              throw (FileNotFoundException)
                  new FileNotFoundException( generateErrorMessage( fileName ) )
                      .initCause(e);
            }
        }
        else {
            for (int i = 0; i < dirs.length && currFileObj == null; ++i) {
                final File candidate = getFile(i, fileName);
                if (candidate.canRead() && candidate.isFile()) {
                    try {
                        currFileObj = candidate.getCanonicalFile();
                    } catch (IOException e) {
                        currFileObj = null;
                    }
                }
            }
        }
        
        if ( ( currFileObj != null ) && ( currFileObj.canRead() ) ) {
            return spfFactory.getSearchPathFile( currFileObj, true );
        }
        else {
            
            throw new FileNotFoundException( generateErrorMessage( fileName ) );
        }
    }

    public SearchPathFile findFirstFileOf(final String[] fileNames)
        throws FileNotFoundException {

        if (fileNames.length == 0)
            throw new IllegalArgumentException("No file names specified.");


        for (int i = 0; i < dirs.length; ++i) {
            for (int j = 0; j < fileNames.length; ++j) {
                final File candidate = getFile(i, fileNames[j]);
                if (candidate.canRead()) {
                    try {
                        return spfFactory.getSearchPathFile(candidate.getCanonicalFile(), true);
                    } catch (IOException e) {
                        // do nothing
                    }
                }
            }
        }

        throw new FileNotFoundException(generateErrorMessage(fileNames));
    }
    
    public SearchPathDirectory findDirectory( final String dirName ) throws FileNotFoundException {
        final File absFileObj = new File( dirName );
        File currFileObj = null;
        
        if ( absFileObj.isAbsolute() ) {
            try {
                currFileObj = absFileObj.getCanonicalFile();
            }
            catch ( IOException e ) {
              throw (FileNotFoundException)
                  new FileNotFoundException( generateErrorMessage( dirName ) )
                    .initCause(e);
            }
        }
        else {
            for (int i = 0; i < dirs.length && currFileObj == null; ++i) {
                final File candidate = getFile(i, dirName);
                if (candidate.canRead() && candidate.isDirectory()) {
                    try {
                        currFileObj = candidate.getCanonicalFile();
                    } catch (IOException e) {
                        currFileObj = null;
                    }
                }
            }
        }
        
        if ( ( currFileObj != null ) && ( currFileObj.canRead() ) ) {

            return new FileSearchPathDirectory( currFileObj, spfFactory );

        }
        else {
            throw new FileNotFoundException( generateErrorMessage( dirName ) );
        }
    }
    
    
    /**
     * Returns the components of the search path as an unmodifiable
     * Iterator.
     **/
    public StringContainerIterator getSearchPath() {
        return new StringContainerIterator() {
                private final String[] myEntries = dirs;
                private int currIndex = 0;
                
                public final boolean hasNext() {
                    return currIndex < myEntries.length;
                }

                public final String next() {
                    if ( hasNext() ) {
                        final String ret = myEntries[currIndex];
                        ++currIndex;
                        return ret;
                    }
                    else {
                        throw new NoSuchElementException();
                    }
                }

            };
    }

    /**
     * Returns true if the specified directory is part of this path.
     * (Note that being a subdirectory of part of this path isn't good
     * enough.)  This is used for verifying module names.  dirName is
     * assumed to end with a '/'.
     **/
    public boolean containsDirectory(String dirName) {
        for (int i=0; i<dirs.length; i++) {
            if (dirs[i].equals(dirName) ||
                dirs[i].equals(dirName.substring(0, dirName.length()-1)))
                return true;
        }
        return false;
    }
    
    public String getSearchPathString() {
        final StringBuffer sb = new StringBuffer();
        final StringContainerIterator strIter = getSearchPath();
        boolean first = true;
        while ( strIter.hasNext() ) {
            final String currEntry = strIter.next();
            if ( first ) {
                first = false ;
            } else {
                sb.append(File.pathSeparator);
            }
            sb.append( currEntry );
        }

        return sb.toString();
    }

    /**
     * Appends the filename to the name of the iDir'th directory.
     **/
    private File getFile(final int iDir, final String fileName) {
        return new File( dirs[iDir], fileName );
    }

    public String toString() {
        return getSearchPathString();
    }
}
