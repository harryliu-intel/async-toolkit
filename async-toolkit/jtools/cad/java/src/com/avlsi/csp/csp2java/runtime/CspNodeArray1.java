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
import com.avlsi.csp.csp2java.runtime.Fold.BinaryFunction; 
import com.avlsi.tools.tsim.WideNode;

/* A class for runtime array support. */
public class CspNodeArray1 implements CspValue {
    private final CspArray array;
    public CspNodeArray1 (CspInteger min, CspInteger max, WideNode[] nodes,
                          int offset) {
        this(min.intValue(), max.intValue(), nodes, offset);
    }
    public CspNodeArray1 (int min, int max, WideNode[] nodes, int offset) {
        final CspValue[] cspNodes = new CspValue[max - min + 1];
        for (int i = 0; i < cspNodes.length; ++i) {
            cspNodes[i] = new CspNode(nodes[offset + i]);
        }
        this.array = new CspArray(min, max, cspNodes);
    }
    public CspNode get (final CspInteger index) {
        return (CspNode) array.get(index);
    }
    public CspNode get (final CspInteger index, String filename, int line,
                        int column) {
        return (CspNode) array.get(index, filename, line, column);
    }
    public void setValue(final CspValue v) {
        array.setValue(v);
    }
    public void setValue(CspValue v, BinaryFunction modifier) {
        throw new UnsupportedOperationException();
    }
}
