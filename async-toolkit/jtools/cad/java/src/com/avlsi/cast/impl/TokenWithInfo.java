// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

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

import antlr.CommonToken;

/**
 * This class adds filename information to CommonToken.
 *
 * @see antlr.CommonToken
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class TokenWithInfo extends CommonToken {
    private String filename = null;

    public TokenWithInfo() {
        super();
    }

    public TokenWithInfo(final int t, final String text) {
        super(t, text);
    }

    public TokenWithInfo(final String text) {
        super(text);
    }

    public String getFilename() {
        return filename;
    }

    public void setFilename(final String filename) {
        this.filename = filename;
    }

    public String toString() {
        return "[\""+getText()+"\",<"+getType()+">,line="+getLine()
            +",column="+getColumn()+",filename="+getFilename()+"]";
    }
}
