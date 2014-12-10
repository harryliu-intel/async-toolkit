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

import com.avlsi.tools.tsim.ChannelInput;

/* A class for runtime array support. */

public class CspChannelInArray2 {
    final CspChannelInArray1[] cia;
    final int min, max;
    public CspChannelInArray2 (int min1, int max1, int min2, int max2, 
                      String elementTypeName, ChannelInput[] cia, int offset) {

        // XXX: this should throw an exception if the narrowing is invalid.

        int subarray_min = min2;
        int subarray_max = max2;

        this.min = min1;
        this.max = max1;

        this.cia = new CspChannelInArray1[this.max - this.min + 1];

        for (int i = 0; i < this.cia.length; i++) {
            this.cia[i] = new CspChannelInArray1(min2, max2, elementTypeName,
                    cia, offset + i * (subarray_max - subarray_min + 1));
        }
    }

    public CspChannelInArray2 (final CspInteger min1, final CspInteger max1, 
                      final CspInteger min2, final CspInteger max2,
                      String elementTypeName,
                      ChannelInput[] cia, int offset) {

        // XXX: this should throw an exception if the narrowing is invalid.

        int subarray_min = min2.intValue();
        int subarray_max = max2.intValue();

        this.min = min1.intValue();
        this.max = max1.intValue();

        this.cia = new CspChannelInArray1[this.max - this.min + 1];

        for (int i = 0; i < this.cia.length; i++) {
            this.cia[i] = new CspChannelInArray1(min2, max2, elementTypeName, 
                    cia, offset + i * (subarray_max - subarray_min + 1));
        }
    }

    public CspChannelInArray1 get (final CspInteger index) {

        // XXX: this should throw an exception if the narrowing is invalid.

        return cia[index.intValue() - min];
    }
}

