/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.cdl.util.rename;


import com.avlsi.file.common.HierName;

public class CDLRenameException extends Exception {

    public CDLRenameException( final HierName from, final HierName to ) {
        this( from.toString(), to.toString(), null );
    }

    public CDLRenameException( final String from, final String to,
                               final Throwable cause ) {
        this( "Could not rename \"" + 
              from + "\" to \"" + to + "\".", cause );
    }

    public CDLRenameException( final String errorMessage ) {
        super( errorMessage );
    }

    public CDLRenameException( final String errorMessage,
                               final Throwable cause ) {
        super( errorMessage, cause );
    }
    
    public CDLRenameException( final Throwable cause ) {
        super( cause );
    }

}
