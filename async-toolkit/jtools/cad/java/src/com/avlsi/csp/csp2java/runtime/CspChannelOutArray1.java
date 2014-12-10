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

import com.avlsi.tools.tsim.ChannelOutput;

/* A class for runtime array support. */

public class CspChannelOutArray1 {
    final ChannelOutput[] cia;
    final int min, max;
    final String elementTypeName;
    public CspChannelOutArray1 (int min, int max, String elementTypeName,
            ChannelOutput[] cia, int offset) {

        this.min = min;
        this.max = max;
        this.elementTypeName = elementTypeName;

        this.cia = new ChannelOutput[this.max - this.min + 1];

        System.arraycopy(cia, offset, this.cia, 0, this.cia.length);
    }

    public CspChannelOutArray1 (final CspInteger min, final CspInteger max, 
            String elementTypeName,
            ChannelOutput[] cia, int offset) {

        // XXX: this should throw an exception if the narrowing is invalid.

        this.min = min.intValue();
        this.max = max.intValue();
        this.elementTypeName = elementTypeName;

        this.cia = new ChannelOutput[this.max - this.min + 1];

        System.arraycopy(cia, offset, this.cia, 0, this.cia.length);
    }

    public ChannelOutput get (final CspInteger index) {
        return cia[index.intValue() - min];
    }

    public ChannelOutput get (final CspInteger index, String filename,
                              int line, int column) {

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

