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
 * Class to store lists of identifiers.
 *
 * @author David Hilvert
 * @version $Revision$ $Date$
 **/
public class IdentifierList extends AbstractASTNode {
    private final List identifiers;

    public IdentifierList() {
        identifiers = new ArrayList();
    }

    public void addIdentifier(final IdentifierExpression i) {
        identifiers.add (i);
    }

    public Iterator getIdentifiers() {
        return identifiers.iterator();
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitIdentifierList(this);
    }
}
