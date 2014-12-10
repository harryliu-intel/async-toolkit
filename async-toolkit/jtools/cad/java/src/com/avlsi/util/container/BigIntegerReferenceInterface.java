/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.container;

import java.math.BigInteger;

/**
 * Interface to encapulate a reference to a <code>BigInteger</code>.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public interface BigIntegerReferenceInterface {

    /**
     * Set the value of the reference.
     *
     * @param val
     *        The value to set.
     **/
    void setBigInteger(BigInteger val);

    /**
     * Returns the last value set.
     **/
    BigInteger getBigInteger();
}
