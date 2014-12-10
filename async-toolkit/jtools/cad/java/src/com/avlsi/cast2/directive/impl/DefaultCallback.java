/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast2.directive.impl;

import java.math.BigDecimal;
import java.io.StringReader;
import java.util.Iterator;

import antlr.RecognitionException;
import antlr.TokenStreamException;

import com.avlsi.cast.impl.AmbiguousLookupException;
import com.avlsi.cast.impl.Environment;
import com.avlsi.cast.impl.EnvironmentEntry;
import com.avlsi.cast.impl.EnvironmentEntryIterator;
import com.avlsi.cast.impl.BoolValue;
import com.avlsi.cast.impl.IntValue;
import com.avlsi.cast.impl.InvalidOperationException;
import com.avlsi.cast.impl.FloatValue;
import com.avlsi.cast.impl.NodeValue;
import com.avlsi.cast.impl.Symbol;
import com.avlsi.cast.impl.Value;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.directive.impl.DirectiveCallback;
import com.avlsi.cast2.impl.CastTwoParser;
import com.avlsi.cast2.impl.CastTwoTreeParser;
import com.avlsi.fast.shapes.stringexpression.StringExpression;
import com.avlsi.fast.shapes.stringexpression.StringExpressionFactory;
import com.avlsi.fast.shapes.stringexpression.impl.StringExpressionFactoryImpl;
import com.avlsi.fast.shapes.stringexpression.impl.parser.StringExpressionLexer;
import com.avlsi.fast.shapes.stringexpression.impl.parser.StringExpressionParser;
import com.avlsi.fast.shapes.stringexpression.variable.StringVariable;
import com.avlsi.fast.shapes.stringexpression.variable.StringVariableDictionary;
import com.avlsi.fast.shapes.stringexpression.variable.StringVariableDictionaryIterator;
import com.avlsi.util.container.FilteringIterator;
import com.avlsi.util.functions.UnaryPredicate;
import com.avlsi.util.mathexpression.NotAConstantValueException;

class DefaultCallback implements DirectiveCallback {
    private Object resolveCastExpr(String type, String value, Environment env) {
        try {
            final CastTwoParser castParser =
                CastTwoParser.getParser(value, 0, 0, "<directive block>");
            castParser.startExpression();
            final CastTwoTreeParser treeParser = new CastTwoTreeParser();
            final Value v = treeParser.expression(castParser.getAST(), env, false);
            if (type.equals(DirectiveConstants.INT_TYPE)) {
                return new Integer(IntValue.valueOf(v).getValue().intValue());
            } else if (type.equals(DirectiveConstants.FLOAT_TYPE)) {
                return new Float(FloatValue.valueOf(v).getValue());
            } else if (type.equals(DirectiveConstants.DOUBLE_TYPE)) {
                return new Double(FloatValue.valueOf(v).getValue());
            } else if (type.equals(DirectiveConstants.BOOLEAN_TYPE)) {
                if (BoolValue.valueOf(v).getValue()) return Boolean.TRUE;
                else return Boolean.FALSE;
            } else {
                return null;
            }
        } catch (Exception e) {
            return null;
        }
    }

    private static class EnvironmentDictionary
    implements StringVariableDictionary {
        private final Environment env;
        private final StringExpressionFactory factory;
        public EnvironmentDictionary(final Environment env,
                                     final StringExpressionFactory factory) {
            this.env = env;
            this.factory = factory;
        }

        private String getString(final Value v) {
            String s = null;
            try {
                if (v instanceof IntValue) {
                    s = ((IntValue) v).getValue().toString();
                } else if (v instanceof BoolValue) {
                    s = Boolean.toString(((BoolValue) v).getValue());
                } else if (v instanceof FloatValue) {
                    s = Double.toString(((FloatValue) v).getValue());
                } else if (v instanceof NodeValue) {
                    s = ((NodeValue) v).getInstanceName().toString();
                }
            } catch (InvalidOperationException e) { }
            return s;
        }

        public StringExpression getVariableValue(final String VariableName) {
            String s = null;
            try {
                final Value v = env.lookup(Symbol.create(VariableName));
                s = getString(v);
            } catch (AmbiguousLookupException e) { }

            return s == null ? null : factory.makeConstant(s);
        }
        
        public StringVariableDictionaryIterator getIterator() {
            final EnvironmentEntryIterator envIter = env.entryIterator();
            final Iterator entries = new Iterator() {
                public boolean hasNext() {
                    return envIter.hasNext();
                }
                public Object next() {
                    return envIter.next();
                }
                public void remove() {
                    throw new UnsupportedOperationException();
                }
            };

            final UnaryPredicate pred = new UnaryPredicate() {
                public boolean evaluate(Object o) {
                    final EnvironmentEntry entry = (EnvironmentEntry) o;
                    return getString(entry.getValue()) != null;
                }
            };

            final Iterator filtered = new FilteringIterator(entries, pred);

            return new StringVariableDictionaryIterator() {
                public boolean hasNext() {
                    return filtered.hasNext();
                }
                public StringVariable next() {
                    final EnvironmentEntry entry =
                        (EnvironmentEntry) filtered.next();
                    return new StringVariable() {
                        public String getName() {
                            return entry.getName().getString();
                        }
                        public StringExpression getValue() {
                            final String s = getString(entry.getValue());
                            return factory.makeConstant(s);
                        }
                    };
                }
            };
        }
    }

    private Object resolveStringExpression(String value, Environment env) {
        /* If the value does not contain "'", then trim it, and return the
         * result. */
        if (value.indexOf('\'') == -1) {
            return value.trim();
        }

        StringExpressionLexer lexer =
            new StringExpressionLexer(new StringReader(value), true);
        StringExpressionFactory factory = new StringExpressionFactoryImpl();
        StringExpressionParser parser = new StringExpressionParser(lexer, factory);
        StringExpression result;

        try {
            result = parser.goal();
        } catch (RecognitionException e) {
            return null;
        } catch (TokenStreamException e) {
            return null;
        }

        result = result.evaluate(new EnvironmentDictionary(env, factory));

        String str;
        try {
            str = result.getConstantValue();
        } catch (NotAConstantValueException e) {
            return null;
        }

        return str;
    }

    public Object resolve(String type, String value, Environment env) {
        Object ret = null;
        value = value.trim();
        if (type.equals(DirectiveConstants.INT_TYPE) ||
            type.equals(DirectiveConstants.FLOAT_TYPE) ||
            type.equals(DirectiveConstants.DOUBLE_TYPE) ||
            type.equals(DirectiveConstants.BOOLEAN_TYPE)) {
            ret = resolveCastExpr(type, value, env);
        } else if (type.equals(DirectiveConstants.STRING_TYPE)) {
            ret = resolveStringExpression(value, env);
        }
        return ret;
    }
}
