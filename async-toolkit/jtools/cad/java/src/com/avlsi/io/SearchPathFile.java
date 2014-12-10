/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */
package com.avlsi.io;
import java.io.InputStream;
import java.io.FileNotFoundException;


/**
   Interface for objects that reference files found in a SearchPath.
 */
public interface SearchPathFile {
    
    /**
       Gets the absolute name of the file in the search path.
       If the file was found in some sort of archive in the SearchPath the implementation should
       try to do something reasonable.  The returned string must uniquely identify the file
       referenced by this interface.  If two instances of an implementation of this interface
       reference the same file then the strings returned by this method MUST be the same.
       @return The absolute name of the file in the SearchPath.
     */
    String getName( );

    /**
       Opens an InputStream that reads the contents of the file.
       @return An InputStream that reads the contents of the file referenced by an instance of the implementation of
       this interface.
     */
    InputStream getStream( ) throws FileNotFoundException;

    /**
       Tests whether the encapsulated file can be read.
       @return true if the encapsulated file can be read.
     */
    boolean canRead( );
   
    /**
       Tests whether the encapsulated file exists.
       @return true if the encapsulated file can be written.
     */
    boolean exists( );

    /**
       @return The time encapsulated file was last modified in milliseconds since the epoch ( 00:00:00 GMT, 1/1/1970 ) or
       -1 if the last modification time is not available.
    */
    long lastModified( );

    /**
       @return The number of bytes that could be read for the InputStream returned by getStream, or
       -1 if that number cannot be computed.
     */
    long length( );

}
