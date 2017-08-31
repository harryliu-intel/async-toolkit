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

/**
 * Class to store lists of declarations.
 *
 * @author David Hilvert
 * @version $Revision$ $Date$
 **/
public class DeclarationList extends AbstractASTNode {
    private final List<Declaration> declarations;

    public DeclarationList() {
        declarations = new ArrayList<>();
    }

    public void addDeclaration(final Declaration d) {
        declarations.add (d);
    }

    public Iterator<Declaration> getDeclarations() {
        return declarations.iterator();
    }
}
