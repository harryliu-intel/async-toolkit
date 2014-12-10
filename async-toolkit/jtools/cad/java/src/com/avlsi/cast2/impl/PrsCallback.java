/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast2.impl;

import java.io.StringReader;

import antlr.RecognitionException;
import antlr.TokenStreamSelector;
import antlr.TokenStreamException;
import antlr.collections.AST;

import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.cast.impl.ASTWithInfo;
import com.avlsi.cast.impl.ChainEnvironment;
import com.avlsi.cast.impl.Environment;
import com.avlsi.cast.impl.FieldedValueInterface;
import com.avlsi.cast.impl.NodeValue;
import com.avlsi.cast.impl.TokenWithInfo;
import com.avlsi.cast.impl.TupleValue;
import com.avlsi.cast.impl.Value;
import com.avlsi.cast2.impl.CastTwoLexer;
import com.avlsi.cast2.impl.CastTwoParser;
import com.avlsi.cast2.impl.CastTwoTreeParser;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.directive.impl.DirectiveCallback;
import com.avlsi.prs.ProductionRuleSet;
import com.avlsi.util.container.Pair;

public class PrsCallback implements DirectiveCallback {
    private static PrsCallback singleton = null;

    private HierName resolveNode(String value, Environment env, boolean check,
                                 boolean deep) {
        final CastTwoParser castParser =
            CastTwoParser.getParser(value, 0, 0, "<no name>");
        try {
            castParser.startPrsNodeExpression();
        } catch (RecognitionException e) {
            return null;
        } catch (TokenStreamException e) {
            return null;
        }

        final AST node = castParser.getAST();

        final CastTwoTreeParser treeParser = new CastTwoTreeParser();

        Value v;

        try {
            v = treeParser.expr(node, env, false, deep);
        } catch (RecognitionException e) {
            if (check || value.indexOf('.') == -1) return null;
            else {
                /* XXX: We might like to support referencing a node that is in
                 * a non-inlined subcell, but that's difficult.  Instead, we
                 * give up (but trim any spaces), and assume what is specified
                 * is valid.  It must already be syntactically correct.
                 **/
                try {
                    return HierName.makeHierName(value.trim(), '.');
                } catch (InvalidHierNameException ee) {
                    return null;
                }
            }
        }

        if (v instanceof NodeValue) return ((NodeValue) v).getInstanceName();
        else return null;
    }

    private Pair resolveHalfOp(String value, Environment env, boolean check,
                               boolean deep) {
        value = value.trim();
        final boolean plus = value.endsWith("+");
        final boolean minus = value.endsWith("-");
        final Boolean up;
        final String nodePart;
        if (plus || minus) {
            up = plus ? Boolean.TRUE : Boolean.FALSE;
            nodePart = value.substring(0, value.length() - 1);
        } else {
            up = null;
            nodePart = value;
        }

        final HierName node = resolveNode(nodePart, env, check, deep);
        if (node == null) return null;
        else return new Pair(node, up);
    }

    // Return a rule set because one textual production rule may be represented
    // by 2 rules
    private ProductionRuleSet resolveRule(String value, Environment env,
                                          boolean deep) {
        final CastPrsParser prsParser = (CastPrsParser)
            CastTwoParser.getParser(CastPrsParser.class, value, 0, 0,
                                    "<no name>");
        try {
            prsParser.startPrsAction();
        } catch (RecognitionException e) {
            return null;
        } catch (TokenStreamException e) {
            return null;
        }

        final AST prs = prsParser.getAST();

        final CastTwoTreeParser treeParser = new CastTwoTreeParser();

        final ProductionRuleSet result = new ProductionRuleSet();

        try {
            treeParser.prsAction(prs, env, null, result, deep);
        } catch (RecognitionException e) {
            return null;
        }

        return result;
    }

    private Value resolveAlintScenario(String value, Environment env) {
        final CastTwoParser castParser =
            CastTwoParser.getParser(value, 0, 0, "<no name>");
        try {
            castParser.startAlintScenario();
        } catch (RecognitionException e) {
            return null;
        } catch (TokenStreamException e) {
            return null;
        }

        final AST node = castParser.getAST();

        // parsed an empty scenario
        if (node == null) return new TupleValue(new Value[0]);

        final CastTwoTreeParser treeParser = new CastTwoTreeParser();

        Value v;

        try {
            v = treeParser.expressionList(node, env, false, true);
        } catch (RecognitionException e) {
            return null;
        }

        return v;
    }

    private PrsCallback() {
    }

    public Object resolve(String type, String value, Environment env) {
        Object ret = null;
        if (type.equals(DirectiveConstants.NODE_TYPE)) {
            ret = resolveNode(value, env, true, false);
        } else if (type.equals(DirectiveConstants.HALFOP_TYPE)) {
            ret = resolveHalfOp(value, env, true, false);
        } else if (type.equals(DirectiveConstants.UNCHECKED_NODE_TYPE)) {
            ret = resolveNode(value, env, false, false);
        } else if (type.equals(DirectiveConstants.UNCHECKED_HALFOP_TYPE)) {
            ret = resolveHalfOp(value, env, false, false);
        } else if (type.equals(DirectiveConstants.DEEP_NODE_TYPE)) {
            ret = resolveNode(value, env, true, true);
        } else if (type.equals(DirectiveConstants.DEEP_HALFOP_TYPE)) {
            ret = resolveHalfOp(value, env, true, true);
        } else if (type.equals(DirectiveConstants.RULE_TYPE)) {
            ret = resolveRule(value, env, false);
        } else if (type.equals(DirectiveConstants.DEEP_RULE_TYPE)) {
            ret = resolveRule(value, env, true);
        } else if (type.equals(DirectiveConstants.ALINT_SCENARIO_TYPE)) {
            ret = resolveAlintScenario(value, env);
        }
        return ret;
    }

    public static PrsCallback getInstance() {
        if (singleton == null) singleton = new PrsCallback();
        return singleton;
    }
}
