/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.csp.ast;

import antlr.Token;
import com.avlsi.csp.grammar.ParsePosition;
import com.avlsi.csp.grammar.ParseRange;

/**
 * This is an interface capturing the API of AbstractASTNode, which is
 * meant to be inherited by other AST node interfaces such as
 * ExpressionInterface. Thus a variable of type ExpressionInterface
 * need not be cast to AbstractASTNode in order to access or modify
 * its parse range information. Note that in many places we assume (by
 * casting to AbstractASTNode) that the only implementor of this is
 * interface is AbstractASTNode, which should be a valid assumption
 * since all AST types inherit from that class.
 *
 * @author Frederik Eaton
 * @version $Revision$ $Date$
 **/
public interface AbstractASTNodeInterface {
    /**
     * Prints an informative error message and throws an exception if
     * parseRange is not set
     **/
    void checkParseRange();


    /** 
     * Return parseRange
     * @return parseRange
     **/
    ParseRange getParseRange();

    /**
     * Sets parseRange to the convex closure parseRange and p.
     *
     * This and the following methods are called by the parser in the
     * process of constructing csp ast objects. The effect is to
     * extend the parse range to the smallest interval containing both
     * the old parse range and the parse range corresponding to the
     * passed object. The reason for the short name and many variants
     * is to preserve parser legibility.
     *
     * "epr" stands for "extend parse range"
     * 
     * @param p
     * @return (AbstractASTNode)this
     **/
    AbstractASTNode epr(ParseRange p);

    /**
     * Extends parseRange to include the characters of t
     *
     * @param t A token
     * @return (AbstractASTNode)this
     **/
    AbstractASTNode epr(Token t);

    /**
     * Extends parseRange to include the parseRange of x
     *
     * @param x Any object representing an AST node
     * @return this
     **/
    AbstractASTNode epr(AbstractASTNodeInterface x);

    /** same as epr(x).epr(y)
        @see #epr(AbstractASTNodeInterface) **/
    AbstractASTNode epr(AbstractASTNodeInterface x, 
                        AbstractASTNodeInterface y);

    /** same as epr(t).epr(s)
        @see #epr(Token) **/
    AbstractASTNode epr(Token t, Token s);

    /** same as epr(t).epr(x)
        @see #epr(Token) @see #epr(AbstractASTNodeInterface) **/
    AbstractASTNode epr(Token t, AbstractASTNodeInterface x);

    /** same as epr(x).epr(t)
        @see #epr(Token) @see #epr(AbstractASTNodeInterface) **/
    AbstractASTNode epr(AbstractASTNodeInterface x, Token t);
}
