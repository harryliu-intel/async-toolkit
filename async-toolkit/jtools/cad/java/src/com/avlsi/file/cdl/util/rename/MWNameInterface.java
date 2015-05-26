/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id: //mrl/sw/intel/cad/java/src/com/avlsi/file/cdl/util/rename/GDS2NameInterface.java#1 $
 * $DateTime: 2014/12/10 04:47:55 $
 * $Author: rliu68 $
 */


package com.avlsi.file.cdl.util.rename;


import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.CDLRenameException;

public class MWNameInterface extends MixNameInterface {
    private static GDS2NameInterface gds2 = new GDS2NameInterface();
    private static CadenceNameInterface cadence = new CadenceNameInterface();

    public MWNameInterface() {
        super(gds2, cadence, cadence, cadence, cadence);
    }
}
