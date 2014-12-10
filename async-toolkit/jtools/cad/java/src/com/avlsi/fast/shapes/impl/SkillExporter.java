/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes.impl;

import java.io.Writer;
import java.io.PrintWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;

import com.avlsi.fast.shapes.Shape;
import com.avlsi.fast.shapes.ShapeIterator;
import com.avlsi.fast.shapes.Point;
import com.avlsi.fast.shapes.Polygon;
import com.avlsi.fast.shapes.PolygonIterator;
import com.avlsi.fast.shapes.ShapeCollection;
import com.avlsi.fast.shapes.ShapeCollectionIterator;
import com.avlsi.fast.shapes.stringexpression.StringExpression;
import com.avlsi.fast.shapes.stringexpression.StringVisitor;
import com.avlsi.fast.shapes.stringexpression.impl.SkillStringVisitor;

import com.avlsi.util.mathexpression.MathExpression;
import com.avlsi.util.mathexpression.Visitor;
import com.avlsi.util.mathexpression.impl.SkillVisitor;

public class SkillExporter {
    protected PrintWriter out;
    protected StringVisitor sv;
    protected Visitor v;
    protected int count;

    protected String gensym() {
        return "poly" + Integer.toString(count++);
    }

    public SkillExporter(final Writer out) {
        this.out = new PrintWriter(out);
        sv = new SkillStringVisitor(out) {
            public void variable(final String varName) {
                write("stringEnv[\"" + varName + "\"]");
            }
        };
        v = new SkillVisitor(out) {
            public void variable(final String varName) {
                write("mathEnv[\"" + varName + "\"]");
            }
        };
        count = 0;
    }

    protected void point(Point p) {
        out.print("(list (quote ");
        p.getX().accept(v);
        out.print(") (quote ");
        p.getY().accept(v);
        out.print("))");
    }

    protected void polygon(Polygon p) {
        PolygonIterator iter = p.iterator();
        out.print("(list ");
        while (iter.hasNext()) {
            point(iter.next());
            out.print(" ");
        }
        out.print(")");
    }

    protected void shape(Shape s) {
        out.println("(lambda (doShape) ");
        out.println("(doShape");

        out.print("?layerName (quote ");
        s.getLayerName().accept(sv);
        out.println(")");

        out.print("?netName ");
        if (s.getNetName() == null) {
            out.print("nil");
        } else {
            out.print("(quote ");
            s.getNetName().accept(sv);
            out.print(")");
        }
        out.println();

        Point o = s.getOrigin();
        out.print("?xOrigin (quote");
        o.getX().accept(v);
        out.println(")");

        out.print("?yOrigin (quote");
        o.getY().accept(v);
        out.println(")");

        final MathExpression xrep = s.getXRepeat();
        out.print("?xRepeat ");
        if (xrep == null) out.print("nil");
        else {
            out.print("(quote ");
            xrep.accept(v);
            out.print(")");
        }
        out.println();

        final MathExpression yrep = s.getYRepeat();
        out.print("?yRepeat ");
        if (yrep == null) out.print("nil");
        else {
            out.print("(quote ");
            yrep.accept(v);
            out.print(")");
        }
        out.println();

        final StringExpression bound = s.getRepeatBoundaryLayer();
        out.print("?boundaryLayer ");
        if (bound == null) out.print("nil");
        else {
            out.print("(quote ");
            bound.accept(sv);
            out.print(")");
        }
        out.println();

        ShapeIterator iter = s.iterator();
        out.println("?polygon (list ");
        while (iter.hasNext()) {
            polygon(iter.next());
            out.println();
        }
        out.print(")");
        out.println(")"); // doShape

        out.println(")"); // lambda
    }

    public void export(ShapeCollection coll, final String funName) {
        ShapeCollectionIterator iter = coll.iterator();
        while (iter.hasNext()) {
            out.println("(setq poly (cons");
            shape(iter.next());
            out.println("poly))");
        }
    }
}
