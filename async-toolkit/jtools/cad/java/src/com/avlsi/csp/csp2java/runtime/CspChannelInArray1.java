/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.csp.csp2java.runtime;

import java.math.BigInteger;
import com.avlsi.tools.tsim.ChannelInput;

/* A class for runtime array support. */

public class CspChannelInArray1 {
    final ChannelInput[] cia;
    final int min, max;
    final String elementTypeName;
    public CspChannelInArray1 (int min, int max, 
            String elementTypeName, ChannelInput[] cia, int offset) {

        this.min = min;
        this.max = max;
        this.elementTypeName = elementTypeName;

        this.cia = new ChannelInput[this.max - this.min + 1];

        System.arraycopy(cia, offset, this.cia, 0, this.cia.length);
    }

    public CspChannelInArray1 (CspInteger min, CspInteger max, 
            String elementTypeName, ChannelInput[] cia, int offset) {

        // XXX: this should throw an exception if the narrowing is invalid.

        this.min = min.intValue();
        this.max = max.intValue();
        this.elementTypeName = elementTypeName;

        this.cia = new ChannelInput[this.max - this.min + 1];

        System.arraycopy(cia, offset, this.cia, 0, this.cia.length);
    }

    public ChannelInput get (final CspInteger index) {
        return cia[index.intValue() - min];
    }

    public ChannelInput get (final CspInteger index, String filename, int line,
                             int column) {

        // XXX: this should throw an exception if the narrowing is invalid.
        try {
            return cia[index.intValue() - min];
        } catch (ArrayIndexOutOfBoundsException e) {
            throw (CspArrayBoundsException)
                  new CspArrayBoundsException(index, min, max, filename, line,
                                              column).initCause(e);
        }
    }

    public int getMinIndex() {
        return min;
    }

    public int getMaxIndex() {
        return max;
    }

    public String getElementTypeName() {
        return elementTypeName;
    }
}

