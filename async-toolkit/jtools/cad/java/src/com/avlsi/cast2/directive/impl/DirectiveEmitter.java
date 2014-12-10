/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast2.directive.impl;

/**
 * API used to convert a value type to a string representation.
 **/
public interface DirectiveEmitter {
    /**
     * Converts a block specific parameter to a string.
     * @param block block where the parameter is located
     * @param type type of the parameter
     * @param value value of the parameter
     * @return The string representation of value.
     **/
    String emit(String block, String type, Object value);
}
