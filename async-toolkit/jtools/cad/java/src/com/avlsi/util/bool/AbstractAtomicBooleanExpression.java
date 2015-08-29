/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.util.bool;

import java.util.Collection;
import java.util.Collections;
import java.util.ArrayList;

/**
 * Class for ...
 *
 * @author Aaron Denney
 * @version $Revision$ $Date$
 **/

public abstract class AbstractAtomicBooleanExpression
        extends AbstractBooleanExpression
        implements OrBooleanExpressionInterface, AndBooleanExpressionInterface
{
    protected AbstractAtomicBooleanExpression(final boolean sense) {
        super(sense);
    }

    /**
     * We are in DNF.
     * ...More or less.
     **/
    public OrBooleanExpressionInterface DNFForm() {
        return this;
    }

    /**
     * We are in CNF.
     * ...More or less.
     **/
    public AndBooleanExpressionInterface CNFForm() {
        return this;
    }

    /**
     * Our disjucts are a list of one: ourself.
     **/
    public Collection<BooleanExpressionInterface> getDisjuncts() {
        return Collections.singleton(this);
    }

    /**
     * Our conjuncts are a list of one: ourself.
     **/
    public Collection<BooleanExpressionInterface> getConjuncts() {
        return Collections.singleton(this);
    }

    public boolean oneLevelDisjunction() {
        return true;
    }

    public boolean oneLevelConjunction() {
        return true;
    }
}

