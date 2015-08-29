package com.avlsi.util.bool;

import java.util.function.Predicate;

import com.avlsi.file.common.HierName;

public class BooleanExpressionEvaluator implements BooleanExpressionVisitorInterface {
    private final Predicate<HierName> env;
    private boolean val;

    public BooleanExpressionEvaluator(final Predicate<HierName> env) {
        this.env = env;
    }

    public void visit(AndBooleanExpressionInterface andExpr) {
        val = andExpr.getConjuncts()
                     .stream()
                     .anyMatch(e -> !evaluate(e, env));
        if (andExpr.getSense()) val = !val;
    }

    public void visit(OrBooleanExpressionInterface orExpr) {
        val = orExpr.getDisjuncts()
                    .stream()
                    .anyMatch(e -> evaluate(e, env));
        if (!orExpr.getSense()) val = !val;
    }

    public void visit(HierNameAtomicBooleanExpression atomicExpr) {
        val = env.test(atomicExpr.getName());
        if (!atomicExpr.getSense()) val = !val;
    }

    public boolean getResult() {
        return val;
    }

    static boolean evaluate(final BooleanExpressionInterface e,
                            final Predicate<HierName> env) {
        final BooleanExpressionEvaluator visitor =
            new BooleanExpressionEvaluator(env);
        e.visitWith(visitor);
        return visitor.getResult();
    }
}
