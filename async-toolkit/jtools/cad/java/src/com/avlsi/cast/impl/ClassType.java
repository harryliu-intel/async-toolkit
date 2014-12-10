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

package com.avlsi.cast.impl;

/**
 * An implementation of Type that compares only based on the type name.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class ClassType extends Type {
    private final String typeName;

    public ClassType(final String typeName) {
        this.typeName = typeName;
    }

    public boolean equals(final Object o) {
        return o instanceof ClassType && equals((ClassType) o);
    }

    public boolean equals(final ClassType ct) {
        // == is ok because the strings are all hardcoded at
        // compile time
        return typeName == ct.typeName;
    }

    public int hashCode() {
        return typeName.hashCode();
    }

    public String toString() {
        return "ClassType(" + typeName + ")";
    }
    
    public String getString() {
        return typeName;
    }
}
