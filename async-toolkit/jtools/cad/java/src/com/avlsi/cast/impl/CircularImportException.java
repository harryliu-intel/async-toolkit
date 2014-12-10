/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.cast.impl;

import java.util.Collection;

/**
 * Exception thrown when a cast file tries to import a file that
 * has imported it.
 *
 * @see CastParserEnvironment
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class CircularImportException extends Exception {
    private final String currentImport;
    private final Collection importedFiles;
    public CircularImportException(final String currentImport,
                                   final Collection importedFiles) {
        this.currentImport = currentImport;
        this.importedFiles = importedFiles;
    }
    public Collection getImportedFiles() {
        return importedFiles;
    }
    public String getCurrentImport() {
        return currentImport;
    }
}
