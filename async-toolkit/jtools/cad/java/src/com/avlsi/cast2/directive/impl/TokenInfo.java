/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast2.directive.impl;

class TokenInfo extends Object {
    private final int line;
    private final String file;
    public TokenInfo(int line, String file) {
        this.line = line;
        this.file = file;
    }
    public int getLine() {
        return line;
    }
    public String getFile() {
        return file;
    }
    public String toString() {
        return file + ":" + line;
    }
}
