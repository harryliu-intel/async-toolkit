/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.mathexpression.impl;

import java.io.Writer;
import java.io.IOException;
import java.io.PrintWriter;

import java.util.Stack;

import java.math.BigDecimal;

import com.avlsi.util.mathexpression.ExpressionCollection;
import com.avlsi.util.mathexpression.ExpressionCollectionIterator;
import com.avlsi.util.mathexpression.MathExpression;
import com.avlsi.util.mathexpression.Visitor;
import com.avlsi.util.mathexpression.variable.VariableDictionary;

public class SkillVisitor implements Visitor {
    protected PrintWriter out;

    public SkillVisitor(final Writer out) {
        this.out = new PrintWriter(out);
    }

    protected void write(final String s) {
        out.print(" " + s);
    }

    public void constant(final BigDecimal constValue) {
        write(constValue.toString());
    }

    public void constant(final double constValue) {
        write(Double.toString(constValue));
    }

    public void variable(final String varName) {
        write(varName);
    }

    private void oneArg(final String name, final MathExpression x) {
        write("(" + name);
        x.accept(this);
        write(")");
    }

    private void twoArg(final String name,
                        final MathExpression x,
                        final MathExpression y) {
        write("(" + name);
        x.accept(this);
        y.accept(this);
        write(")");
    }

    private void manyArg(final String name, final ExpressionCollection terms) {
        final ExpressionCollectionIterator i = terms.getIterator();
        if (terms.size() == 1) {
            i.next().accept(this);
        } else {
            write("(" + name);
            while (i.hasNext()) {
                i.next().accept(this);
            }
            write(")");
        }
    }

    public void negationOperator(final MathExpression x) {
        oneArg("minus", x);
    }

    public void plusOperator(final ExpressionCollection terms) {
        manyArg("plus", terms);
    }

    public void minusOperator(final MathExpression a,
                              final MathExpression b) {
        twoArg("minus", a, b);
    }

    public void divideOperator(final MathExpression dividend,
                               final MathExpression divisor) {
        twoArg("quotient", dividend, divisor);
    }

    public void timesOperator(final ExpressionCollection terms) {
        manyArg("times", terms);
    }

    public void modOperator(final MathExpression dividend,
                            final MathExpression divisor) {
        twoArg("mod", dividend, divisor);
    }

    public void expFunction(final MathExpression x) {
        oneArg("exp", x);
    }

    public void logFunction(final MathExpression x) {
        oneArg("log", x);
    }

    public void sinFunction(final MathExpression x) {
        oneArg("sin", x);
    }

    public void cosFunction(final MathExpression x) {
        oneArg("cos", x);
    }

    public void tanFunction(final MathExpression x) {
        oneArg("tan", x);
    }

    public void arcsinFunction(final MathExpression x) {
        oneArg("asin", x);
    }

    public void arccosFunction(final MathExpression x) {
        oneArg("acos", x);
    }

    public void arctanFunction(final MathExpression x) {
        oneArg("atan", x);
    }

    public void ceilFunction(final MathExpression x) {
        oneArg("ceiling", x);
    }

    public void roundFunction(final MathExpression x) {
        oneArg("round", x);
    }

    public void floorFunction(final MathExpression x) {
        oneArg("floor", x);
    }

    public void lessThanOperator(final MathExpression a,
                                 final MathExpression b,
                                 final MathExpression trueResult,
                                 final MathExpression falseResult) {
        write("(if");
            write("(lessp");
            a.accept(this);
            b.accept(this);
            write(")");
        trueResult.accept(this);
        falseResult.accept(this);
        write(")");
    }

    public void sumOperator(final String indexName,
                            final MathExpression indexFirstValue,
                            final MathExpression indexLastValue,
                            final MathExpression termExpression) {
        final String sum = "sum_" + indexName;
        write("(let");
            write("(");
                write("(" + indexName);
                indexFirstValue.accept(this);
                write(")");
                write("( " + sum + " 0)");
            write(")");
            write("(while (lessp " + indexName);
            indexLastValue.accept(this);
            write(")");
                write("(setq " + sum + " (plus " + sum);
                termExpression.accept(this);
                write("))");
                write("(inc1 " + indexName + ")");
            write(")");
            write(sum);
        write(")");
    }

    public void subExpressionReference(final VariableDictionary subBindings,
                                       final MathExpression subExpression) {
        subExpression.accept(this);
    }
}
