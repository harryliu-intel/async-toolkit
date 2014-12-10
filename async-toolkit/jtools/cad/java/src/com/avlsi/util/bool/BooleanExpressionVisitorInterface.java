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

package com.avlsi.util.bool;

/**
 * The visitor pattern for BooleanExpressionInterfaces.  A type-safe way
 * to provide operations that differ depending on the implementation
 * of the interface (no need to use instanceof).  When a new implementation
 * of BooleanExpressionInterface is added, a visit method will need
 * to be added here.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public interface BooleanExpressionVisitorInterface {
    void visit(AndBooleanExpressionInterface andExpr);
    void visit(OrBooleanExpressionInterface orExpr);
    void visit(HierNameAtomicBooleanExpression atomicExpr);
}
