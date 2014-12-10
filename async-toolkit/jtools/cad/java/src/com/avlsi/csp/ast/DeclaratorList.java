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
 * Class to store lists of declarators.
 *
 * @author David Hilvert
 * @version $Revision$ $Date$
 **/
public class DeclaratorList extends AbstractASTNode {
    private final List declarators;

    public DeclaratorList() {
        declarators = new ArrayList();
    }

    public void addDeclarator(final Declarator i) {
        declarators.add (i);
    }

    public Iterator getDeclarators() {
        return declarators.iterator();
    }
}
