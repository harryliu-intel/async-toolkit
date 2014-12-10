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
 * Interface that CSP statements implement.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public interface StatementInterface extends AbstractASTNodeInterface {
    void accept(VisitorInterface v) throws VisitorException;
}
