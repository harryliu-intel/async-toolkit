/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */
package com.avlsi.file.cdl.util.rename;



import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.CDLNameInterfaceFactory;
import com.avlsi.file.cdl.util.rename.CDLRenameException;

import com.avlsi.file.cdl.util.rename.CompositeCDLNameInterface;

public class CompositeCDLNameInterfaceFactory implements CDLNameInterfaceFactory {

    private final CDLNameInterfaceFactory mF;
    private final CDLNameInterfaceFactory mG;

    public CompositeCDLNameInterfaceFactory( final CDLNameInterfaceFactory f,
                                             final CDLNameInterfaceFactory g ) {
        mF = f;
        mG = g;
    }

    public CDLNameInterface getNameInterface( final String cellName ) throws CDLRenameException {
        final CDLNameInterface niF = mF.getNameInterface( cellName );
        final CDLNameInterface niG = mG.getNameInterface( cellName );

        return new CompositeCDLNameInterface( niF, niG );
    }

}
