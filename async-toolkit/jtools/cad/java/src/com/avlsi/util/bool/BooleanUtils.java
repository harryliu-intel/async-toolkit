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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.Optional;
import java.util.function.Predicate;

import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.exception.AssertionFailure;
import com.avlsi.util.functions.UnaryAction;
import com.avlsi.util.functions.UnaryFunction;

/**
 * Utility functions for creating <code>BooleanExpressionInterface</code>s.
 * <p>
 * The class's methods are not static so you can do
 * <code><pre>
 *   final BooleanUtils b = new BooleanUtils();
 *   b.and(b.or(b.literal("a"), b.literal("b")), b.f());
 * </pre></code>
 * And save yourself from typing a bunch of <code>BooleanUtils</code>.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class BooleanUtils {
    /**
     * Returns the logical not of the expression.
     **/
    public BooleanExpressionInterface not(
            final BooleanExpressionInterface be) {
        return be.negated();
    }

    /**
     * Returns the logical and of the two expressions.
     **/
    public BooleanExpressionInterface and(
            final BooleanExpressionInterface... be) {
        return new AndBooleanExpression(true, Arrays.asList(be));
    }

    /**
     * Returns or of the two expressions.
     **/
    public BooleanExpressionInterface or(
            final BooleanExpressionInterface... be) {
        return new OrBooleanExpression(true, Arrays.asList(be));
    }

    /**
     * Returns xor of the two expressions.
     **/
    public BooleanExpressionInterface xor(
            final BooleanExpressionInterface be1,
            final BooleanExpressionInterface be2) {
        return this.or(this.and(be1, this.not(be2)),
                       this.and(this.not(be1), be2));
    }

    /**
     * Returns literal.
     **/
    public BooleanExpressionInterface literal(final String s) {
        try {
            return literal(HierName.makeHierName(s, '.'));
        } catch (InvalidHierNameException e) {
            throw new AssertionFailure(e);
        }
    }

    /**
     * Returns literal.
     **/
    public BooleanExpressionInterface literal(final HierName n) {
        return new HierNameAtomicBooleanExpression(true, n);
    }

    /**
     * Returns <code>BooleanExpressionInterface</code> representing true.
     **/
    public BooleanExpressionInterface t() {
        return new AndBooleanExpression(true, new ArrayList());
    }

    /**
     * Returns <code>BooleanExpressionInterface</code> representing false.
     **/
    public BooleanExpressionInterface f() {
        return new OrBooleanExpression(true, new ArrayList());
    }

    /**
     * Recurses through the BooleanExpressionInterface, returning
     * a new BooleanExpressionInterface with all HierNames mapped by
     * the specified mapping.
     **/
    public static BooleanExpressionInterface
        mapBooleanExpressionHierNames(
                final BooleanExpressionInterface be,
                final UnaryFunction<HierName,HierName> hierNameMapper) {
        return HierNameMappingVisitor.map(be, hierNameMapper);
    }

    /**
     * Recurses through the BooleanExpressionInterface, executing
     * <code>hierNameAction</code> on each HierName found.
     * 
     * @todo jmr This is a very inefficient implementation.
     *   A new BooleanExpressionInterface is constructed
     *   and thrown away.  Reimplement if it is a performance
     *   problem.
     **/
    public static void foreachHierName(
            final BooleanExpressionInterface be,
            final UnaryAction<HierName> hierNameAction) {
        HierNameMappingVisitor.map(be,
                x -> { hierNameAction.execute(x); return x; });
    }

    private static class HierNameMappingVisitor 
        implements BooleanExpressionVisitorInterface {

        private final UnaryFunction<HierName,HierName> hierNameMapper;
        private BooleanExpressionInterface result;

        private HierNameMappingVisitor(
                final UnaryFunction<HierName,HierName> hierNameMapper) {
            this.hierNameMapper = hierNameMapper;
            this.result = null;
        }

        private BooleanExpressionInterface getResult() {
            return result;
        }

        public void visit(AndBooleanExpressionInterface andExpr) {
            final Collection oldConjs = andExpr.getConjuncts();
            final Collection newConjs = new ArrayList(oldConjs.size());

            for (final Iterator iConj = oldConjs.iterator();
                    iConj.hasNext(); ) {
                final BooleanExpressionInterface subBe
                    = (BooleanExpressionInterface) iConj.next();

                newConjs.add(map(subBe, hierNameMapper));
            }

            result = new AndBooleanExpression(andExpr.getSense(), newConjs);
        }

        public void visit(OrBooleanExpressionInterface orExpr) {
            final Collection oldDisjs = orExpr.getDisjuncts();
            final Collection newDisjs = new ArrayList(oldDisjs.size());

            for (final Iterator iDisj = oldDisjs.iterator();
                    iDisj.hasNext(); ) {
                final BooleanExpressionInterface subBe
                    = (BooleanExpressionInterface) iDisj.next();

                newDisjs.add(map(subBe, hierNameMapper));
            }

            result = new OrBooleanExpression(orExpr.getSense(), newDisjs);
        }

        public void visit(HierNameAtomicBooleanExpression atomicExpr) {
            result =
                new HierNameAtomicBooleanExpression(atomicExpr.getSense(),
                        hierNameMapper.execute(atomicExpr.getName()));
        }

        public static BooleanExpressionInterface map(
                final BooleanExpressionInterface expr,
                final UnaryFunction<HierName,HierName> hierNameMapper) {
            final HierNameMappingVisitor v =
                new HierNameMappingVisitor(hierNameMapper);
            expr.visitWith(v);
            return v.getResult();
        }
    }

    public static boolean evaluate(final BooleanExpressionInterface e,
                                   final Predicate<HierName> env) {
        return BooleanExpressionEvaluator.evaluate(e, env);
    }
}
