// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id: //mrl/sw/intel/cad/java/src/com/avlsi/file/cdl/util/rename/GDS2NameInterface.java#1 $
 * $DateTime: 2014/12/10 04:47:55 $
 * $Author: rliu68 $
 */


package com.avlsi.file.cdl.util.rename;


import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.CDLRenameException;

public class MWReverseNameInterface extends MixNameInterface {
    private static GDS2ReverseNameInterface gds2 =
        new GDS2ReverseNameInterface();
    private static CadenceReverseNameInterface cadence =
        new CadenceReverseNameInterface();

    public MWReverseNameInterface() {
        super(gds2, cadence, cadence, cadence, cadence);
    }
}
