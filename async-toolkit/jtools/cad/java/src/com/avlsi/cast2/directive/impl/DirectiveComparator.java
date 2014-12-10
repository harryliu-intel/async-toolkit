/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast2.directive.impl;

/**
 * API used to compare 2 values.
 **/
public interface DirectiveComparator {
    /**
     * Compare 2 block specific parameters.
     * @param block block where the parameter is located
     * @param type type of the parameter
     * @param value1 first value of the parameter
     * @param value2 second value of the parameter
     * @return <code>true</code> if the values are equivalent; false otherwise.
     **/
    boolean compare(String block, String type, Object value1, Object value2);
}
