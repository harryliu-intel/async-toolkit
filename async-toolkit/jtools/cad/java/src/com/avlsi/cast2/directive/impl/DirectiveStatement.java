/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast2.directive.impl;

import com.avlsi.cast2.directive.impl.DirectiveVisitor;

public interface DirectiveStatement {
    void visit(final DirectiveVisitor v);
}
