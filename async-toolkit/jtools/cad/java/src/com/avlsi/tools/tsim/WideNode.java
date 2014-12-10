package com.avlsi.tools.tsim;

import java.math.BigInteger;

import com.avlsi.tools.dsim.Node;

/**
 * A collection of nodes to represents a "wide" node.
 **/
public interface WideNode {
    /**
     * Return the value of the node as a BigInteger.
     **/
    BigInteger getValue();

    /**
     * Set the value of each node identified by each element of
     * <code>indices</code> to the corresponding value from
     * <code>values</code>.
     **/
    void setValue(final int[] indices, final byte[] values);

    /**
     * Set the value of bits from <code>begin</code> to <code>end</code> using
     * the values of the corresponding bits from <code>val</code>.
     **/
    void setValue(final int begin, final int end, final BigInteger val);

    /**
     * Set the value to <code>val</code>.
     **/
    void setValue(final BigInteger val);

    /**
     * Returns the constituent nodes.
     **/
    Node[] getNodes();

    /**
     * Returns whether this is a "wide" node.
     **/
    boolean isArrayed();

    /**
     * Returns true if all constituent nodes is either 1 or 0.
     **/
    boolean stable();
}
