/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cosim.spec;

/**
 * Represents a <code>levelspec</code> and list of instance-by-instance
 * exceptions to that <code>levelspec</code>.
 * <p>
 * Represents the rule <code>cosimspec</code>
 * in the
 * <a href="http://internal.avlsi.com/tree/sw/cad/doc/specs/cast/cosim.html">Cosimulation UI Specification</a>.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class CoSimSpec implements AcceptorInterface {
    /**
     * Cosimulation level and method.  May not be null.
     **/
    private final LevelSpecInterface levelSpec;

    /**
     * Instance-by-instance exceptions to the <code>levelSpec</code>
     **/
    private final InstSpecList instSpecList;

    /**
     * Class constructor.
     *
     * @param levelSpec  Cosimulation level and method.  May not be null.
     * @param instSpecList  Instance-by-instance exceptions to the
     *   <code>levelSpec</code>.  May not be null.
     **/
    public CoSimSpec(
            final LevelSpecInterface levelSpec,
            final InstSpecList instSpecList) {
        this.levelSpec = levelSpec;
        this.instSpecList = instSpecList;
    }

    /**
     * Returns the cosimulation level and method
     * @return cosimulation level and method, not null
     **/
    public LevelSpecInterface getLevelSpec() {
        return levelSpec;
    }

    /**
     * Returns instance-by-instance exceptions to the
     *   <code>levelSpec</code>.
     * @return instance-by-instance exceptions to the
     *   <code>levelSpec</code>, not null
     **/
    public InstSpecList getInstSpecList() {
        return instSpecList;
    }

    public String toString() {
        return levelSpec.toString() + instSpecList.toString();
    }

    public void accept(VisitorInterface v) {
        v.visitCoSimSpec(this);
    }
}
