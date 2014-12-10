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

package com.avlsi.util.text;

/**
 * Exception thrown by {@link ByteArrayString#fromString} when a character
 * cannot be represented as a byte.
 *
 * @see ByteArrayString
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class UnrepresentableCharException extends Exception {
    /**
     * the unrepresentable character.
     **/
    private final char ch;

    /**
     * Class constructor
     **/
    public UnrepresentableCharException(final char ch) {
        this.ch = ch;
    }

    /**
     * Returns the character that could not be represented.
     **/
    public char getUnrepresentableChar() {
        return ch;
    }
}
