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

package com.avlsi.csp.ast;

/**
 * Class for CSP structure types
 *
 * @author Harry Liu
 * @version $Revision$ $Date$
 **/
public class StructureType extends Type {
    /**
     * Name of the structure.
     **/
    private String name;

    /**
     * Is the structure a constant?
     **/
    private final boolean is_const;

    public StructureType (final boolean is_const, final String name) {
        this.is_const = is_const;
        this.name = name;
    }

    public int dimension() {
        return 0;
    }

    /**
     * Get the name of the structure.
     *
     * @return name of the structure
     **/
    public String getName() {
        return name;
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitStructureType(this);
    }

    /**
     * Indicates whether this structure type is a constant structure type.
     **/
    public boolean isConst() {
        return is_const;
    }

    public String toString() {
        return "structure " + name;
    }
}
