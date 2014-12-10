/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.fast.metaparameters;

/**
 * A metaparameter variable definition, consisting of its type and name.
 * (Modified from com.avlsi.fast.ports.PortDefinition.)
 *
 * @author David Hilvert
 * @version $Revision$ $Date$
 **/
public final class MetaParamDefinition {

    /** Meta-parameter name, not null. **/
    private final String name;

    /** Meta-parameter type, not null. **/
    private final MetaParamTypeInterface type;

    /**
     * Class constructor
     *
     * @param name  metaparameter name, not null
     * @param type  metaparameter type, not null
     **/
    public MetaParamDefinition(final String name,
            final MetaParamTypeInterface type) {
        this.name = name;
        this.type = type;
    }

    /**
     * Returns the metaparameter variable name.
     *
     * @return param name, not null
     **/
    public String getName() {
        return name;
    }

    /**
     * Returns the metaparameter type.
     *
     * @return param type, not null
     **/
    public MetaParamTypeInterface getType() {
        return type;
    }

    public String toString() {
        return name + ": " + ' ' + type.toString();
    }
}
