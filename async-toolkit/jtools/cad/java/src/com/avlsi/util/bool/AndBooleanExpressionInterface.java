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
 * Boolean expression that is a conjunct of other Boolean expressions.
 *
 * Must be able to return these expressions, hence getConjuncts().
 *
 * @author Aaron Denney
 * @version $Name:  $ $Date$
 **/
public interface AndBooleanExpressionInterface extends BooleanExpressionInterface {
    /**
     * returns a Collection containing all of the sub expressions that
     * this is a conjunct of.
     **/
    Collection<BooleanExpressionInterface> getConjuncts();

    /**
     * @todo denney what does this do?
     **/
    boolean getSense();

    /**
     * @todo denney what does this do?
     **/
    boolean oneLevelConjunction();
}
