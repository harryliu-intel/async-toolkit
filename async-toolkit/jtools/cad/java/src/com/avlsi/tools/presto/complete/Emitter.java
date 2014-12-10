/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.complete;

public interface Emitter {
    // void declare (Name name);
    void emit(String str, Object cookie);
}
