/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.csp.ast;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

/**
 *
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public final class LinkageTerms extends AbstractASTNode {
    private final List terms;

    public LinkageTerms() {
        terms = new ArrayList();
    }

    public void addTerm(final LinkageTermInterface term) {
        terms.add(term);
    }

    public Iterator getTerms() {
        return Collections.unmodifiableList(terms).iterator();
    }
}
