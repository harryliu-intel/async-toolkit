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

package com.avlsi.util.bool;

import java.util.Collection;

/**
 * An interface that all boolean expressions easily representable as
 * a disjunction should implement.
 **/
public interface OrBooleanExpressionInterface extends BooleanExpressionInterface {
    /**
     * @todo denney what does this do?
     **/
    Collection getDisjuncts();

    /**
     * @todo denney what does this do?
     **/
    boolean getSense();

    /**
     * @todo denney what does this do?
     **/
    boolean oneLevelDisjunction();
}
