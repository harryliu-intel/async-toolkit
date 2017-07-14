package com.avlsi.csp.util;

import java.io.StringReader;

import antlr.RecognitionException;
import antlr.TokenStreamSelector;
import antlr.TokenStreamException;
import antlr.collections.AST;

import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;
import com.avlsi.cast.impl.ArrayValue;
import com.avlsi.cast.impl.DenseSubscriptSpec;
import com.avlsi.cast.impl.Environment;
import com.avlsi.cast.impl.InstanceValue;
import com.avlsi.cast.impl.InvalidOperationException;
import com.avlsi.cast.impl.Range;
import com.avlsi.cast.impl.SubscriptSpecInterface;
import com.avlsi.cast.impl.TokenWithInfo;
import com.avlsi.cast.impl.Value;
import com.avlsi.cast2.impl.CastTwoParser;
import com.avlsi.cast2.impl.CastTwoTreeParser;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.directive.impl.DirectiveCallback;
import com.avlsi.csp.ast.*;
import com.avlsi.csp.grammar.CspLexer;
import com.avlsi.csp.grammar.CspParser;
import com.avlsi.tools.cosim.CoSimChannelNames;
import com.avlsi.util.container.Pair;
import com.avlsi.csp.ast.ExpressionInterface;

public class CspCallback implements DirectiveCallback {
    private static CspCallback singleton = null;

    private boolean isChannel(final Value v) {
        if (v instanceof InstanceValue) {
            final CellInterface c = ((InstanceValue) v).getCell();
            return CellUtils.isAsyncChannel(c);
        }
        return false;
    }

    private AST getAST(final String value) {
        final CastTwoParser castParser =
            CastTwoParser.getParser(value, 0, 0, "<no name>");

        try {
            castParser.startPrsNodeExpression();
        } catch (RecognitionException e) {
            return null;
        } catch (TokenStreamException e) {
            return null;
        }

        return castParser.getAST();
    }

    private String accessArray(final ArrayValue av,
                               final SubscriptSpecInterface spec) {
        return accessArray(av, spec, av.isWideChannel());
    }

    private String accessArray(final ArrayValue av,
                               final SubscriptSpecInterface spec,
                               final boolean ignoreLastDimension) {
        try {
            final Value v = av.accessArray(spec);
            if (isChannel(v)) {
                final CoSimChannelNames names =
                    spec.getCoSimChannelNames(av.getInstanceName()
                                                .getAsString('.'),
                                              ignoreLastDimension);
                return names.getFirstElement();
            }
        } catch (InvalidOperationException e) { }
        return null;
    }

    /**
     * Return the name of the channel as a string.  This is complicated by the
     * fact that wide channels are processed as having an extra dimension.
     *
     * @param value a string that represents a channel
     * @param env environment in which to interpret <code>value</code>
     * @param partialOkay whether it is okay to select a particular narrow
     * channel from a wide channel
     * @return name of the channel, or <code>null</code> if <code>value</code>
     * does not specify a valid channel.
     **/
    private String getNames(final String value, final Environment env,
                            final boolean partialOkay) {
        final AST ast = getAST(value);
        final CastTwoTreeParser treeParser = new CastTwoTreeParser();

        // Parse as an array expression first
        Pair p;
        try {
            p = treeParser.arrayAccessExpr(ast, env);
        } catch (RecognitionException e) {
            p = null;
        }
        
        if (p == null) {
            // value is not an array expression; it is either a plain channel
            // (e.g., e1of4 x) or a wide channel that is not arrayed (e.g.,
            // e1of4[2] x).
            Value v;
            try {
                v = treeParser.expr(ast, env, false);
            } catch (RecognitionException e) {
                v = null;
            }
            if (isChannel(v)) {
                return v.getInstanceName().getAsString('.');
            } else if (v instanceof ArrayValue) {
                final ArrayValue av = (ArrayValue) v;
                if (av.isWideChannel()) {
                    final Range[] r = new Range[] { new Range(0, 0) };
                    return accessArray(av, new DenseSubscriptSpec(r));
                }
            }
        } else if (p.getFirst() instanceof ArrayValue) {
            final ArrayValue av = (ArrayValue) p.getFirst();
            SubscriptSpecInterface spec =
                (SubscriptSpecInterface) p.getSecond();
            if (spec.getNumElements() != 1) return null;
            String result = accessArray(av, spec, false);
            if (av.isWideChannel()) {
                if (result == null) {
                    // if an array is a wide channel (e.g., e1of4[2] x[3]),
                    // then the last dimension, i.e., the channel width
                    // dimension, must not be specified.  Since channel width
                    // must be greater than 0, there is always a 0th element in
                    // the channel width dimension.  So add [0] to the end of
                    // subscript spec, and test if the expression is a valid
                    // channel.
                    final int[] idx = spec.indexOf(0);
                    assert idx.length == spec.getNumDimensions();
                    final Range[] r = new Range[idx.length + 1];
                    for (int i = 0; i < idx.length; ++i) {
                        r[i] = new Range(idx[i], idx[i]);
                    }
                    r[idx.length] = new Range(0, 0);
                    spec = new DenseSubscriptSpec(r);
                    result = accessArray(av, spec);
                } else if (!partialOkay) {
                    result = null;
                }
            }
            return result;
        }
        return null;
    }

    static class FullySpecifiedValue extends VisitorByCategory {
        boolean valid = true;
        public void visitStructureAccessExpression(StructureAccessExpression e)
            throws VisitorException {
            valid = false;
        }
        public void visitFunctionCallExpression(FunctionCallExpression e)
            throws VisitorException {
            valid = false;
        }
        public void visitArrayAccessExpression(ArrayAccessExpression e)
            throws VisitorException {
            super.visitArrayAccessExpression(e);
            valid &= e.getIndexExpression() instanceof IntegerExpression;
        }
        public void visitBitRangeExpression(BitRangeExpression e)
            throws VisitorException {
            super.visitBitRangeExpression(e);
            valid &= (e.getMinExpression() == null ||
                      e.getMinExpression() instanceof IntegerExpression) &&
                     e.getMaxExpression() instanceof IntegerExpression;
        }
    }

    private ExpressionInterface getLvalue(final String value,
                                          final Environment env) {
        final CspLexer cspLexer = new CspLexer(new StringReader(value));
        cspLexer.setLine(0);
        cspLexer.setColumn(0);
        cspLexer.setFilename("<no file>");
        cspLexer.setTokenObjectClass(TokenWithInfo.class.getName());
        final CspParser cspParser = new CspParser(cspLexer);
        cspParser.setFilename("<no file>");
        try {
            final ExpressionInterface lvalue = cspParser.startLvalue();
            final CSPProgram prog = new CSPProgram();
            prog.setStatement(new ExpressionStatement(lvalue));
            prog.setInitializerStatement(CastTwoTreeParser.makeCSPConstantInitializers(env));
            // use constant evaluator to remove references to loop variables in
            // the directives block
            final CSPProgram progConst = ConstantEvaluator.evaluate(prog);
            final ExpressionInterface lvalueConst =
                ((ExpressionStatement) progConst.getStatement()).getExpression();
            final FullySpecifiedValue fsv = new FullySpecifiedValue();
            lvalueConst.accept(fsv);
            return fsv.valid ? lvalueConst : null;
        } catch (RecognitionException e) {
            return null;
        } catch (TokenStreamException e) {
            return null;
        } catch (InvalidOperationException e) {
            return null;
        } catch (VisitorException e) {
            return null;
        }
    }

    private CspCallback() { }

    /**
     * A channel for the purpose of a csp block is either a channel, or a wide
     * channel (i.e., an array of channels).  Suppose we have:
     * <pre>e1of4 x; e1of4 y[0..2]; e1of4[2] z[0..2];</pre>
     * then <code>x</code>, <code>y[1]</code>, <code>z[1]</code> are considered
     * channels, but <code>y</code> and <code>z[1,1]</code> are not.
     **/
    public Object resolve(String type, String value, Environment env) {
        if (type.equals(DirectiveConstants.WIDE_CHANNEL_TYPE)) {
            return getNames(value, env, false);
        } else if (type.equals(DirectiveConstants.POSSIBLY_WIDE_CHANNEL_TYPE)) {
            return getNames(value, env, true);
        } else if (type.equals(DirectiveConstants.CSP_LVALUE)) {
            return getLvalue(value, env);
        } else {
            return null;
        }
    }

    public static CspCallback getInstance() {
        if (singleton == null) singleton = new CspCallback();
        return singleton;
    }
}
