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
 * Abstract base class for deterministic and non-deterministic selection
 * statements. Ie <code>[e1 -&gt; s1 [] e2 -&gt; s2 [] ...] </code>
 * and <code>[e1 -&gt; s1 : e2 -&gt; s2 : ...]</code>.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public abstract class AbstractSelectionStatement
    extends AbstractGuardedStatement {
}
