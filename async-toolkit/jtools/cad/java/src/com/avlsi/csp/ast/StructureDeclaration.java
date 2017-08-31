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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.HashMap;

/**
 * Class for CSP structure definitions
 *
 * @author Harry Liu
 * @version $Revision$ $Date$
 **/
public class StructureDeclaration extends AbstractASTNode {
    /**
     * Name of the structure
     **/
    private final String name;

    /**
     * Members declared in the structure
     **/
    private final DeclarationList declarations;

    /**
     * Class constructor.
     *
     * @param name name of the structure
     * @param declarations members declared in the structure
     **/
    public StructureDeclaration(final String name,
                                final DeclarationList declarations) {
        this.name = name;
        this.declarations = declarations;
    }

    /**
     * Get name of the structure
     *
     * @return name of the structure
     **/
    public String getName() {
        return name;
    }

    /**
     * Get members declared in the structure
     *
     * @return members in the structure
     **/
    public DeclarationList getDeclarations() {
        return declarations;
    }

    /**
     * Get a map from members to their types.  Note that if a member is
     * declared multiple times in the structure, there is no guarantee which
     * type from the declarations is returned.
     *
     * @return map from <code>String</code> to <code>Type</code>
     **/
    public Map<String,Type> getMap() {
        final Map<String,Type> result = new HashMap<>();
        declarations.getDeclarations().forEachRemaining(
            decl -> result.putAll(decl.getMap()));
        return result;
    }
}
