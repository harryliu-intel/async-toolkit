/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes.stringexpression.impl;

import java.io.Writer;
import java.io.IOException;
import java.io.PrintWriter;

import com.avlsi.fast.shapes.stringexpression.StringVisitor;
import com.avlsi.fast.shapes.stringexpression.StringExpression;
import com.avlsi.fast.shapes.stringexpression.StringExpressionCollection;
import com.avlsi.fast.shapes.stringexpression.StringExpressionCollectionIterator;
import com.avlsi.fast.shapes.stringexpression.variable.StringVariableDictionary;

public class SkillStringVisitor implements StringVisitor {
    protected PrintWriter out;

    public SkillStringVisitor(final Writer out) {
        this.out = new PrintWriter(out);
    }

    protected void write(final String s) {
        out.print(" " + s);
    }

    public void constant(final String constValue) {
        write("\"" + constValue + "\"");
    }

    public void variable(final String varName) {
        write(varName);
    }

    private void oneArg(final String name, final StringExpression x) {
        write("(" + name);
        x.accept(this);
        write(")");
    }

    private void twoArg(final String name,
                        final StringExpression x,
                        final StringExpression y) {
        write("(" + name);
        x.accept(this);
        y.accept(this);
        write(")");
    }

    private void manyArg(final String name, final StringExpressionCollection terms) {
        final StringExpressionCollectionIterator i = terms.getIterator();
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

    public void concatOperator(final StringExpressionCollection terms) {
        manyArg("strcat", terms);
    }

    public void subExpressionReference(final StringVariableDictionary subBindings,
                                       final StringExpression subExpression) {
        subExpression.accept(this);
    }
}
