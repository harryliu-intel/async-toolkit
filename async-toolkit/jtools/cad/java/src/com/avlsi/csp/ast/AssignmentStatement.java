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
 * Assignment statement, represents <code> lhs := rhs </code>.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class AssignmentStatement
    extends AbstractASTNode
    implements StatementInterface {

    public static final char EQUAL      = '=';
    public static final char ADD        = '+';
    public static final char SUBTRACT   = '-';
    public static final char MULTIPLY   = '*';
    public static final char DIVIDE     = '/';
    public static final char REMAINDER  = '%';
    public static final char AND        = '&';
    public static final char OR         = '|';
    public static final char XOR        = '^';
    public static final char LEFTSHIFT  = '<';
    public static final char RIGHTSHIFT = '>';

    /**
     * Left hand side of assignment.  Expression to which to assign.
     * May not be null.
     **/
    private final ExpressionInterface lhs;

    /**
     * Right hand side of assignment.  Expression from which to assign.
     * May not be null.
     **/
    private final ExpressionInterface rhs;

    /**
     * Kind of assignment statement (i.e., plain assignment statement, or +=
     * assignment operator).
     **/
    private final char kind;

    /**
     * Class constructor.
     *
     * @param lhs left hand side expression, to be assigned to.  Not null.
     * @param lhs right hand side expression, to be assigned from.  Not null.
     **/
    public AssignmentStatement(final ExpressionInterface lhs,
            final ExpressionInterface rhs) {
        this(lhs, rhs, EQUAL);
    }

    /**
     * Class constructor.
     *
     * @param lhs left hand side expression, to be assigned to.  Not null.
     * @param lhs right hand side expression, to be assigned from.  Not null.
     * @param kind the kind of assignment statement.
     **/
    public AssignmentStatement(final ExpressionInterface lhs,
            final ExpressionInterface rhs, final char kind) {
        this.lhs = lhs;
        this.rhs = rhs;
        this.kind = kind;
    }

    /**
     * Returns left hand side expression, to be assigned to.
     *
     * @return left hand side expression, not null
     **/
    public ExpressionInterface getLeftHandSide() {
        return lhs;
    }

    /**
     * Returns right hand side expression, to be assigned from.
     *
     * @return right hand side expression, not null
     **/
    public ExpressionInterface getRightHandSide() {
        return rhs;
    }

    /**
     * Return the kind of the assignment statement.
     **/
    public char getKind() {
        return kind;
    }

    /**
     * Return the kind of the assignment statement, as a string.
     **/
    public String getKindString() {
        switch (getKind()) {
          case ADD:        return "add";
          case SUBTRACT:   return "subtract";
          case MULTIPLY:   return "multiply";
          case DIVIDE:     return "divide";
          case REMAINDER:  return "remainder";
          case AND:        return "and";
          case OR:         return "or";
          case XOR:        return "xor";
          case LEFTSHIFT:  return "leftshift";
          case RIGHTSHIFT: return "rightshift";
          default: throw new AssertionError();
        }
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitAssignmentStatement(this);
    }
}
