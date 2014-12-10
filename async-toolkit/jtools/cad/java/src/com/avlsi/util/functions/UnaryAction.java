/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.util.functions;

/**
 * Represents a (presumably side-effecting) action on an Object.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public interface UnaryAction<A> {
    void execute(A a);
}
