package com.avlsi.csp.csp2java.runtime;

import java.math.BigInteger;
import com.avlsi.tools.tsim.WideNode;

public class CspNode extends CspInteger {
    public static class UnstableException extends RuntimeException { }

    private final WideNode node;

    private CspNode(BigInteger value) {
        super(value);
        this.node = null;
    }

    public CspNode(WideNode node) {
        this.node = node;
    }

    protected void setValue(final BigInteger v) {
        if (node == null) super.setValue(v);
        else node.setValue(v);
    }

    protected BigInteger getValue() {
        if (node == null) {
            return super.getValue();
        } else {
            final BigInteger result = node.getValue();
            if (result == null) throw new UnstableException();
            return node.isArrayed() ? result : result.negate();
        }
    }

    public boolean stable() {
        return node == null ? true : node.stable();
    }

    public CspCloneableValue duplicate() {
        return new CspNode(getValue());
    }
}
