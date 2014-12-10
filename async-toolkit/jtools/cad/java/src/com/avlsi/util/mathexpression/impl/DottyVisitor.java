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

public class DottyVisitor implements Visitor {
    protected Stack parent;
    protected String name;
    protected PrintWriter out;
    private int counter;

    public DottyVisitor(final String name, final Writer out) {
        parent = new Stack();
        this.name = name;
        this.out = new PrintWriter(out);
        counter = 0;
        printHeader();
    }

    private void printHeader() {
        out.println("digraph " + name + " {");
    }
    
    private String uniqId() {
        counter++;
        return "node" + counter;
    }

    private String makeNode(final String node, final String name) {
        return node + " [label=\"" + name + "\"];";
    }

    private String makeEdge(final String parent, final String child) {
        return parent + " -> " + child;
    }

    private void edge(final String me) {
        if (!parent.empty()) {
            out.println(makeEdge((String) parent.peek(), me));
        }
    }

    private void node(final String node, final String name) {
        out.println(makeNode(node, name));
    }

    public void constant(final BigDecimal constValue) {
        final String me = uniqId();
        node(me, constValue.toString());
        edge(me);
    }

    public void constant(final double constValue) {
        final String me = uniqId();
        node(me, Double.toString(constValue));
        edge(me);
    }

    public void variable(final String varName) {
        final String me = "var" + varName;
        node(me, varName);
        edge(me);
    }

    private void oneArg(final String name, final MathExpression x) {
        final String me = uniqId();
        node(me, name);
        edge(me);
        parent.push(me);
        x.accept(this);
        parent.pop();
    }

    private void twoArg(final String name, final MathExpression x, final MathExpression y) {
        final String me = uniqId();
        node(me, name);
        edge(me);
        parent.push(me);
        x.accept(this);
        y.accept(this);
        parent.pop();
    }

    private void manyArg(final String name, final ExpressionCollection terms) {
        final String me = uniqId();
        node(me, name);
        edge(me);
        parent.push(me);
        final ExpressionCollectionIterator i = terms.getIterator();
        while (i.hasNext()) {
            i.next().accept(this);
        }
        parent.pop();
    }

    public void negationOperator(final MathExpression x) {
        oneArg("-", x);
    }

    public void plusOperator(final ExpressionCollection terms) {
        manyArg("+", terms);
    }

    public void minusOperator(final MathExpression a,
                              final MathExpression b) {
        twoArg("-", a, b);
    }

    public void divideOperator(final MathExpression dividend,
                               final MathExpression divisor) {
        twoArg("/", dividend, divisor);
    }

    public void timesOperator(final ExpressionCollection terms) {
        manyArg("*", terms);
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
        oneArg("arcsin", x);
    }

    public void arccosFunction(final MathExpression x) {
        oneArg("arccos", x);
    }

    public void arctanFunction(final MathExpression x) {
        oneArg("arctan", x);
    }

    public void ceilFunction(final MathExpression x) {
        oneArg("ceil", x);
    }

    public void roundFunction(final MathExpression x) {
        oneArg("round", x);
    }

    public void floorFunction(final MathExpression x) {
        oneArg("floor", x);
    }

    public void lessThanOperator(final MathExpression a, final MathExpression b,
                                 final MathExpression trueResult,
                                 final MathExpression falseResult) {
        final String me = uniqId();
        node(me, "<");
        edge(me);
        parent.push(me);
        a.accept(this);
        b.accept(this);
        trueResult.accept(this);
        falseResult.accept(this);
        parent.pop();
    }

    public void sumOperator(final String indexName,
                            final MathExpression indexFirstValue,
                            final MathExpression indexLastValue,
                            final MathExpression termExpression) {
        final String me = uniqId();
        node(me, "sum " + indexName);
        edge(me);
        parent.push(me);
        indexFirstValue.accept(this);
        indexLastValue.accept(this);
        termExpression.accept(this);
        parent.pop();
    }

    public void subExpressionReference(final VariableDictionary subBindings,
                                       final MathExpression subExpression) {
        final String me = uniqId();
        node(me, "subexpression");
        edge(me);
        parent.push(me);
        subExpression.accept(this);
        parent.pop();
    }

    public void finish() {
        out.println("}");
    }
}
