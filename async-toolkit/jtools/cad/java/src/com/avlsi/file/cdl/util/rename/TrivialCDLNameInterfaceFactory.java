/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.file.cdl.util.rename;


import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.CDLNameInterfaceFactory;
import com.avlsi.file.cdl.util.rename.CDLRenameException;

public class TrivialCDLNameInterfaceFactory implements CDLNameInterfaceFactory {

    private final CDLNameInterface mTheInterface;

    public TrivialCDLNameInterfaceFactory( final CDLNameInterface theInterface ) {
        mTheInterface = theInterface;
    }

    public CDLNameInterface getNameInterface( final String cellName ) throws CDLRenameException {
        return mTheInterface;
    }

}
