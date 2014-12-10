/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.mathexpression.variable.impl;

import java.math.BigDecimal;

import com.avlsi.cast.impl.AmbiguousLookupException;
import com.avlsi.cast.impl.Environment;
import com.avlsi.cast.impl.FloatValue;
import com.avlsi.cast.impl.IntValue;
import com.avlsi.cast.impl.InvalidOperationException;
import com.avlsi.cast.impl.Symbol;
import com.avlsi.cast.impl.Value;
import com.avlsi.util.mathexpression.MathExpression;
import com.avlsi.util.mathexpression.MathExpressionFactory;
import com.avlsi.util.mathexpression.variable.VariableDictionary;
import com.avlsi.util.mathexpression.variable.VariableDictionaryIterator;
import com.avlsi.util.mathexpression.variable.WriteableVariableDictionary;
import com.avlsi.util.mathexpression.variable.impl.WriteableVariableDictionaryImpl;
import com.avlsi.util.debug.Debug;

public class EnvironmentAdapter implements VariableDictionary {
    private final Environment env;
    private final MathExpressionFactory factory;
    private final WriteableVariableDictionary dict;

    public EnvironmentAdapter(final Environment env,
                              final MathExpressionFactory factory) {
        this.env = env;
        this.factory = factory;
        this.dict = new WriteableVariableDictionaryImpl();
    }

    public MathExpression getVariableValue(final String VariableName) {
        MathExpression ret = null;
        if ((ret = dict.getVariableValue(VariableName)) != null) return ret;

        Value val = null;
        try {
            val = env.lookup(Symbol.create(VariableName));
            if (val == null)
                return null;
        } catch (AmbiguousLookupException e) {
            return null;
        }

        try {
            if (val instanceof FloatValue) {
                ret = factory.makeConstant(((FloatValue) val).getValue());
            } else if (val instanceof IntValue) {
                ret = factory.makeConstant(new BigDecimal(((IntValue) val).getValue()));
            } else {
                return null;
            }
        } catch (InvalidOperationException e) {
            return null;
        }

        dict.bindVariable(VariableName, ret);
        return ret;
    }

    // XXX: Terribly broken, but there is no way to enumerate all key value
    // pairs in an environment.
    public VariableDictionaryIterator getIterator() {
        return dict.getIterator();
    }
}
