/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast.shapes.impl;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.HashSet;

import com.avlsi.fast.shapes.ArgumentMismatchException;
import com.avlsi.fast.shapes.ShapeFunctionUndefinedException;
import com.avlsi.fast.shapes.Shape;
import com.avlsi.fast.shapes.ShapeCollection;
import com.avlsi.fast.shapes.ShapeFunction;
import com.avlsi.fast.shapes.ShapeFunctionTable;
import com.avlsi.fast.shapes.Polygon;
import com.avlsi.fast.shapes.PolygonIterator;
import com.avlsi.fast.shapes.Point;
import com.avlsi.fast.shapes.WriteableShape;
import com.avlsi.fast.shapes.WriteableShapesBlock;
import com.avlsi.fast.shapes.impl.PolygonImpl;
import com.avlsi.fast.shapes.impl.WriteablePointImpl;
import com.avlsi.fast.shapes.impl.WriteableShapeImpl;
import com.avlsi.fast.shapes.impl.WriteableShapeCollectionImpl;
import com.avlsi.fast.shapes.impl.WriteableShapesBlockImpl;

import com.avlsi.util.mathexpression.MathExpression;
import com.avlsi.util.mathexpression.ExpressionCollection;
import com.avlsi.util.mathexpression.ExpressionCollectionIterator;
import com.avlsi.util.mathexpression.impl.WriteableExpressionCollectionImpl;
import com.avlsi.util.mathexpression.variable.VariableDictionary;
import com.avlsi.util.mathexpression.variable.impl.WriteableVariableDictionaryImpl;

import com.avlsi.fast.shapes.stringexpression.StringExpression;
import com.avlsi.fast.shapes.stringexpression.StringExpressionCollection;
import com.avlsi.fast.shapes.stringexpression.impl.WriteableStringExpressionCollectionImpl;
import com.avlsi.fast.shapes.stringexpression.StringExpressionCollectionIterator;
import com.avlsi.fast.shapes.stringexpression.variable.StringVariableDictionary;
import com.avlsi.fast.shapes.stringexpression.variable.impl.WriteableStringVariableDictionaryImpl;

public class ShapeFunctionImpl implements ShapeFunction {
    private String name;
    private StringExpression layer, net, boundary;
    private Point origin;
    private boolean repeatx, repeaty;
    private List polygons, calls;
    private String[] mathParams;
    private String[] stringParams;

    protected class FunctionCall {
        private String name;
        private ExpressionCollection mathArgs;
        private StringExpressionCollection stringArgs;

        /**
           Construct a representation of a function call given the name of the
           function, the math arguments, and the string arguments.
           @param name Name of the function
           @param mathArgs Mathematical arguments
           @param stringArgs String arguments
         */
        public FunctionCall(final String name,
                            final ExpressionCollection mathArgs,
                            final StringExpressionCollection stringArgs) {
            this.name = name;
            this.mathArgs = mathArgs;
            this.stringArgs = stringArgs;
        }

        /**
           Gets the name of the function.
           @return Name of the function
         */
        public String getName() {
            return name;
        }

        /**
           Evaluate function call arguments given a dictionary.
           @param dict Dictionary to lookup variables in
           @return Evaluated arguments in a ExpressionCollection
         */
        public ExpressionCollection resolve(VariableDictionary dict) {
            WriteableExpressionCollectionImpl coll =
                new WriteableExpressionCollectionImpl(mathArgs.size());
            ExpressionCollectionIterator iter = mathArgs.getIterator();
            while (iter.hasNext()) {
                coll.addExpression(iter.next().evaluate(dict));
            }
            return coll;
        }

        /**
           Evaluate function call arguments given a dictionary.
           @param dict Dictionary to lookup variables in
           @return Evaluated arguments in a StringExpressionCollection
         */
        public StringExpressionCollection resolve(StringVariableDictionary dict)
        {
            WriteableStringExpressionCollectionImpl coll =
                new WriteableStringExpressionCollectionImpl(stringArgs.size());
            StringExpressionCollectionIterator iter = stringArgs.getIterator();
            while (iter.hasNext()) {
                coll.addExpression(iter.next().evaluate(dict));
            }
            return coll;
        }
    }

    public ShapeFunctionImpl(final String name, final StringExpression layer,
                             final StringExpression net,
                             boolean repeatx, boolean repeaty,
                             final StringExpression boundary,
                             final Point origin, String[] mathParams,
                             String[] stringParams) {
        polygons = new ArrayList();
        calls = new ArrayList();
        this.name = name;
        this.layer = layer;
        this.net = net;
        this.repeatx = repeatx;
        this.repeaty = repeaty;
        this.boundary = boundary;
        this.origin = origin;
        this.mathParams = mathParams;
        this.stringParams = stringParams;
    }

    /**
       Bind arguments to parameters and return a dictionary with the bindings.
       @param args Arguments.
       @return VariableDictionary with the bindings.
       @throws ArgumentMismatchException if the number of arguments and number
       of parameters do not match.
     */
    protected VariableDictionary bind(ExpressionCollection args)
        throws ArgumentMismatchException {
        if (mathParams.length != args.size()) {
            throw new ArgumentMismatchException(name, mathParams.length,
                                                args.size());
        }
        WriteableVariableDictionaryImpl dict =
            new WriteableVariableDictionaryImpl();
        ExpressionCollectionIterator iter = args.getIterator();
        for (int i = 0; i < mathParams.length; i++) {
            dict.bindVariable(mathParams[i], iter.next());
        }
        return dict;
    }

    /**
       Bind arguments to parameters and return a dictionary with the bindings.
       @param args Arguments.
       @return StringVariableDictionary with the bindings.
       @throws ArgumentMismatchException if the number of arguments and number
       of parameters do not match.
     */
    protected StringVariableDictionary bind(StringExpressionCollection args)
        throws ArgumentMismatchException {
        if (stringParams.length != args.size()) {
            throw new ArgumentMismatchException(name, stringParams.length,
                                                args.size());
        }
        WriteableStringVariableDictionaryImpl dict =
            new WriteableStringVariableDictionaryImpl();
        StringExpressionCollectionIterator iter = args.getIterator();
        for (int i = 0; i < stringParams.length; i++) {
            dict.bindVariable(stringParams[i], iter.next());
        }
        return dict;
    }

    /**
       Evaluate a Point with a specified VariableDictionary.
       @param point Point to evaluate
       @param dict Dictionary to lookup bindings
       @return A new Point with its coordinates evaluated.
     */
    protected Point resolve(final Point point, final VariableDictionary dict) {
        return new WriteablePointImpl(point.getX().evaluate(dict),
                                      point.getY().evaluate(dict));
    }

    /**
       Evaluate a Polygon with a specified VariableDictionary.
       @param poly Polygon to evaluate
       @param dict Dictionary to lookup bindings
       @return A new Polygon with its coordinates evaluated.
     */
    protected Polygon resolve(final Polygon poly,
                              final VariableDictionary dict) {
        int size = poly.size();
        Point[] points = new Point[size];
        PolygonIterator iter = poly.iterator();
        for (int i = 0; i < size; i++) {
            points[i] = resolve(iter.next(), dict);
        }
        return new PolygonImpl(points);
    }

    public ShapeCollection resolve(final ShapeFunctionTable table,
                                   final Set seen,
                                   final ExpressionCollection mathArgs,
                                   final StringExpressionCollection stringArgs)
    {
        VariableDictionary mathDict;
        StringVariableDictionary stringDict;
        try {
            mathDict = bind(mathArgs);
            stringDict = bind(stringArgs);
        } catch (ArgumentMismatchException e) {
            System.err.println(e.getMessage());
            return new WriteableShapeCollectionImpl(1);
        }
        WriteableShapesBlockImpl block =
            new WriteableShapesBlockImpl();
        WriteableShapeCollectionImpl coll =
            new WriteableShapeCollectionImpl();

        if (!polygons.isEmpty()) {
            Point eo = resolve(origin, mathDict);
            MathExpression xrep = null;
            MathExpression yrep = null;
            if (repeatx) xrep = eo.getX();
            if (repeaty) yrep = eo.getY();
            WriteableShape shape =
                new WriteableShapeImpl(layer.evaluate(stringDict),
                                       net == null ?
                                           null : net.evaluate(stringDict),
                                       eo,
                                       xrep, yrep,
                                       boundary);
            Iterator iter = polygons.iterator();
            while (iter.hasNext()) {
                shape.addPolygon(resolve((Polygon) iter.next(), mathDict));
            }
            coll.addShape(shape);
            block.addShapes(coll);
        }

        resolve(table, seen, mathDict, stringDict, block);
        return block.getShapes();
    }

    public ShapeCollection resolve(final ShapeFunctionTable table) {
        VariableDictionary mathDict = new WriteableVariableDictionaryImpl();
        StringVariableDictionary stringDict =
            new WriteableStringVariableDictionaryImpl();
        WriteableShapesBlockImpl block =
            new WriteableShapesBlockImpl();

        resolve(table, new HashSet(), mathDict, stringDict, block);
        return block.getShapes();
    }

    /**
       Evaluate function calls.
     */
    protected void resolve(final ShapeFunctionTable table,
                        final Set seen,
                        final VariableDictionary mathDict,
                        final StringVariableDictionary stringDict,
                        final WriteableShapesBlock block)
    {
        Iterator iter = calls.iterator();
        while (iter.hasNext()) {
            FunctionCall call = (FunctionCall) iter.next();
            String funName = call.getName();
            if (seen.contains(funName)) {
                System.err.println("Recursion of ShapeFun " + funName + " detected!");
                continue;
            } else {
                seen.add(funName);
            }
            try {
                ShapeFunction func = table.lookup(funName);
                block.addShapes(func.resolve(table,
                                             seen,
                                             call.resolve(mathDict),
                                             call.resolve(stringDict)));
            } catch (ShapeFunctionUndefinedException e) {
                System.err.println(e.getMessage());
            } finally {
                seen.remove(funName);
            }
        }
    }

    public void addPolygon(final Polygon poly) {
        polygons.add(poly);
    }

    public void addFunctionCall(final String name,
                                final ExpressionCollection mathArgs,
                                final StringExpressionCollection stringArgs) {
        calls.add(new FunctionCall(name, mathArgs, stringArgs));
    }

    public String getName() {
        return name;
    }
}


