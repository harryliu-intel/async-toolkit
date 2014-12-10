/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.csp.ast;

/**
 *
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public interface LinkageTermInterface
    extends AbstractASTNodeInterface {
    void accept(VisitorInterface v) throws VisitorException;
}
