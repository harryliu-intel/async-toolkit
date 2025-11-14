// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package com.avlsi.csp.csp2verilog;

import java.math.BigInteger;
import java.io.PrintWriter;
import java.io.Writer;
import java.util.Map;
import java.util.Set;

import com.avlsi.csp.ast.*;
import com.avlsi.csp.grammar.ParseRange;
import com.avlsi.csp.util.CspUtils;
import com.avlsi.csp.util.DeclarationProcessor;
import com.avlsi.csp.util.RefinementResolver;
import com.avlsi.csp.util.VariableAnalyzer;
import com.avlsi.io.IndentPrintWriter;
import com.avlsi.util.container.IterableIterator;
import com.avlsi.util.container.Pair;

abstract class CommonEmitter implements VisitorInterface {
    LengthAwareWriter out;

    /**
     * Number of bits to use for variables with unspecified length
     **/
    final int registerBitWidth;

    /**
     * The type of register to use for variables.
     **/
    final String registerType;

    CommonEmitter(final PrintWriter out, final int registerBitWidth,
                  final boolean enableCoveragePragma) {
        this.out = new LengthAwareWriter(1000, enableCoveragePragma, out);
        this.registerBitWidth = registerBitWidth;
        this.registerType = "bit signed [" + (registerBitWidth - 1) + ":0]";
    }

    CommonEmitter(final PrintWriter out, final int registerBitWidth) {
        this(out, registerBitWidth, false);
    }

    protected int getRegisterWidth(final IntegerType t)
        throws VisitorException {
        final int width;
        if (t.getDeclaredWidth() == null) {
            if (t instanceof TemporaryIntegerType && t.getInterval() != null) {
                width = CspUtils.getWidth(t.getInterval(), registerBitWidth);
            } else {
                width = registerBitWidth;
            }
        } else {
            width = getIntegerConstant(t.getDeclaredWidth());
        }
        return width;
    }

    protected boolean isRegisterSigned(final IntegerType t) {
        return t.getDeclaredWidth() == null || t.isSigned();
    }

    protected String getNodeType(final NodeType t) {
        final int width = t.getWidth();
        return "bit [" + (width - 1) + ":0]";
    }

    protected String getRegisterType(final IntegerType t)
        throws VisitorException {
        final boolean signed = isRegisterSigned(t);
        final int width = Math.max(1, getRegisterWidth(t));
        return "bit " + (signed ? "signed " : "") + "[" + (width - 1) + ":0]";
    }

    public void visitIntegerExpression(IntegerExpression e)
        throws VisitorException {
        if (e instanceof BooleanExpression) {
            if (((BooleanExpression) e).booleanValue()) {
                out.print("1'b1");
            } else {
                out.print("1'b0");
            }
        } else {
            processBigInteger(new BigInteger(e.getValue(), e.getRadix()),
                              e.getRadix());
        }
    }

    /**
     * Output an integer constant as a signed expression.  If radix is 2, 8, or
     * 10, use the "b", "o" and "d" base specifiers respectively.  Otherwise,
     * output in hexdecimal.
     *
     * @param bi integer to output
     * @param radix radix to use
     **/
    void processBigInteger(BigInteger bi, final int radix) {
        // all constants must be signed, because "if any operand is unsigned,
        // the result is unsigned, regardless of the operator" (4.5.1)
        if (bi.signum() == -1) {
            // if the number is negative, prepend a negative sign, and process
            // it's absolute value
            bi = bi.negate();
            out.print('-');
        }

        final int width = bi.bitLength() + 1; // plus 1 for the sign bit

        out.print(width);
        out.print("'s");
        if (radix == 2)
            out.print("b" + bi.toString(2));
        else if (radix == 8)
            out.print("o" + bi.toString(8));
        else if (radix == 10)
            out.print("d" + bi.toString(10));
        else {
            // write other bases in hex for no particular reason
            out.print("h" + bi.toString(16));
        }
    }

    // Determine if this is a simple bit range, which can be translated to
    // Verilog directly
    boolean needBitRangeFunction(final BitRangeExpression e) {
        // If both min and max are constants, and if we are not part-selecting
        // on an array element[1], then we can use Verilog's constant
        // part-select.
        // [1] Seems to be a ncverilog restriction, as part-selecting an array
        // element is legal per the Verilog 2001 spec 4.2.2 (p. 55).
        final ExpressionInterface be = e.getBitsExpression();
        if (be instanceof IdentifierExpression ||
            be instanceof ArrayAccessExpression ||
            be instanceof MemberAccessExpression) {
            if (e.getMinExpression() == null) {
                // bit select
                return false;
            } else if (e.getMaxExpression() instanceof IntegerExpression &&
                       e.getMinExpression() instanceof IntegerExpression) {
                // part select when bounds are constants
                return false;
            }
        }
        return true;
    }

    private BigInteger getBits(final RefinementResolver resolver, final Type t)
        throws VisitorException {
        if (t instanceof ArrayType) {
            final ArrayType at = (ArrayType) t;
            final Range r = at.getRange();
            final BigInteger min =
                CspUtils.getIntegerConstant(r.getMinExpression());
            final BigInteger max =
                CspUtils.getIntegerConstant(r.getMaxExpression());
            final BigInteger elem = getBits(resolver, at.getElementType());
            if (min == null || max == null || elem == null) return null;
            else return max.subtract(min).add(BigInteger.ONE).multiply(elem);
        } else if (t instanceof IntegerType) {
            final IntegerType it = (IntegerType) t;
            if (it.getDeclaredWidth() == null) {
                return BigInteger.valueOf(registerBitWidth);
            } else {
                return CspUtils.getIntegerConstant(it.getDeclaredWidth());
            }
        } else if (t instanceof BooleanType) {
            return BigInteger.ONE;
        } else if (t instanceof StructureType) {
            final BigInteger[] result = new BigInteger[] { BigInteger.ZERO };
            final Pair p = (Pair) resolver.getResolvedStructures().get(t);
            // VariableAnalyzer should have made sure all structures are valid
            final StructureDeclaration sdecl =
                (StructureDeclaration) p.getSecond();
            final DeclarationProcessor proc = new DeclarationProcessor() {
                public void process(final Declarator d)
                    throws VisitorException {
                    final BigInteger field = getBits(resolver, d.getTypeFragment());
                    if (field == null || result[0] == null) {
                        result[0] = null;
                    } else {
                        result[0] = result[0].add(field);
                    }
                }
            };
            proc.process(sdecl.getDeclarations());
            return result[0];
        } else if (t instanceof NodeType) {
            return BigInteger.valueOf(((NodeType) t).getWidth());
        } else {
            return BigInteger.ZERO;
        }
    }

    // Return top-level variables
    void getTopLevelVariables(
            final StatementInterface stmt,
            final RefinementResolver resolver,
            final VariableAnalyzer.Results analysisResults,
            final Map<String,Pair<ParseRange,BigInteger>> stateVars,
            final Map<IdentifierExpression,Declarator> topVars,
            final Set<Type> topTokens)
        throws VisitorException {
        for (Map.Entry<String,Type> entry :
                analysisResults.getUndeclaredTypes().entrySet()) {
            final IdentifierExpression id =
                new IdentifierExpression(entry.getKey());
            final Type t = entry.getValue();
            topTokens.add(t);
            topVars.put(id, new Declarator(id, t, null));
            if (stateVars != null) {
                stateVars.put(entry.getKey(),
                              new Pair<ParseRange,BigInteger>(ParseRange.EMPTY,
                                                              getBits(resolver, t)));
            }
        }
        for (Map.Entry<String,Type> entry :
                analysisResults.getPortTypes().entrySet()) {
            final IdentifierExpression id =
                new IdentifierExpression(entry.getKey());
            final Type t = entry.getValue();
            topTokens.add(t);
            topVars.put(id, new Declarator(id, t, null));
            if (stateVars != null) {
                if (t instanceof NodeType) {
                    final NodeType nt = (NodeType) t;
                    if (nt.getDirection() == PortDirection.OUT) {
                        stateVars.put(entry.getKey(),
                                new Pair<ParseRange,BigInteger>(null,
                                    getBits(resolver, t)));
                    }
                }
            }
        }

        if (stmt instanceof AbstractCompositeStatement) {
            final AbstractCompositeStatement seq =
                (AbstractCompositeStatement) stmt;
            final DeclarationProcessor proc = new DeclarationProcessor() {
                public void process(final Declarator d)
                    throws VisitorException {
                    final IdentifierExpression id = d.getIdentifier();
                    topTokens.add(d.getTypeFragment());
                    topVars.put(id, d);
                    if (stateVars != null) {
                        stateVars.put(
                            id.getIdentifier(),
                            new Pair<ParseRange,BigInteger>(
                                id.getParseRange(),
                                getBits(resolver, d.getTypeFragment())));
                    }
                }
            };
            for (StatementInterface i :
                    new IterableIterator<StatementInterface>(
                        seq.getStatements())) {
                if (i instanceof VarStatement) {
                    proc.process(((VarStatement) i).getDeclarationList());
                }
            }
        }
    }

    // A simple bit range is one that can be translated to Verilog directly
    void processSimpleBitRange(BitRangeExpression e, boolean lhs)
        throws VisitorException {
        if (!lhs) out.print("($signed({1'd0, ");
        e.getBitsExpression().accept(this);
        processSimpleBitRangeIndex(e);
        if (!lhs) out.print("}))");
    }

    void processSimpleBitRangeIndex(final BitRangeExpression e)
        throws VisitorException {
        final ExpressionInterface maxExpr = e.getMaxExpression();
        final ExpressionInterface minExpr = e.getMinExpression();
        out.print('['); maxExpr.accept(this);
        if (minExpr != null) {
            out.print(':'); minExpr.accept(this);
        }
        out.print(']');
    }


    // Detect when hi<lo in x{hi:lo}
    boolean isBitRangeInvalid(final BitRangeExpression e)
        throws VisitorException {
        final ExpressionInterface minExpr = e.getMinExpression();
        final BigInteger max =
            CspUtils.getIntegerConstant(e.getMaxExpression());
        if (minExpr == null) {
            return max != null && max.compareTo(BigInteger.ZERO) < 0;
        } else {
            final BigInteger min = CspUtils.getIntegerConstant(minExpr);
            return max != null && min != null && max.compareTo(min) < 0;
        }
    }

    int getIntegerConstant(final ExpressionInterface e)
        throws VisitorException {
        if (e instanceof IntegerExpression) {
            final IntegerExpression ie = (IntegerExpression) e;
            return Integer.parseInt(ie.getValue(), ie.getRadix());
        } else {
            throw new VisitorException(
                    "Compile time constant integer expression expected: " +
                    e.getParseRange().fullString());
        }
    }

    static class LengthAwareWriter extends IndentPrintWriter {
        private final int limit;
        private final boolean enableCoveragePragma;
        private int length = 0;
        private boolean coverage = true;
        private boolean nextCoverage = true;
        private boolean writingCoverage = false;
        public LengthAwareWriter(final int limit,
                                 final boolean enableCoveragePragma,
                                 final Writer out) {
            super(out);
            this.limit = limit;
            this.enableCoveragePragma = enableCoveragePragma;
        }
        public LengthAwareWriter(final int limit, final Writer out) {
            this(limit, false, out);
        }
        public void println() {
            super.println();
            length = 0;
        }
        public void ws() {
            if (length > limit) println();
            else print(' ');
        }
        public void write(int c) {
            writeCoverage();
            super.write(c);
            ++length;
        }
        public void write(char buf[], int off, int len) {
            writeCoverage();
            super.write(buf, off, len);
            length += len;
        }
        public void write(String s, int off, int len) {
            writeCoverage();
            super.write(s, off, len);
            length += len;
        }
        public void nextLevel(int x) {
            for (int i = 0; i < x; ++i) nextLevel();
        }
        public void prevLevel(int x) {
            for (int i = 0; i < x; ++i) prevLevel();
        }
        public void enableCoverage() {
            nextCoverage = true;
        }
        public void disableCoverage() {
            nextCoverage = false;
        }
        private void writeCoverage() {
            if (enableCoveragePragma && !writingCoverage &&
                nextCoverage != coverage) {
                writingCoverage = true;
                super.println("// VCS coverage " + (nextCoverage ? "on" : "off"));
                writingCoverage = false;
                coverage = nextCoverage;
            }
        }
    }

    void processBinary(final AbstractBinaryExpression expr, final String op)
        throws VisitorException {
        out.print('(');
        expr.getLeft().accept(this);
        out.ws(); out.print(op); out.ws();
        expr.getRight().accept(this);
        out.print(')');
    }

    void processCompare(final AbstractBinaryExpression expr, final String op)
        throws VisitorException {
        processBinary(expr, op);
    }

    public void visitConditionalAndExpression(ConditionalAndExpression e)
        throws VisitorException {
        processCompare(e, "&&");
    }

    public void visitConditionalOrExpression(ConditionalOrExpression e)
        throws VisitorException {
        processCompare(e, "||");
    }

    public void visitEqualityExpression(EqualityExpression e)
        throws VisitorException {
        processCompare(e, "==");
    }

    public void visitGreaterEqualExpression(GreaterEqualExpression e)
        throws VisitorException {
        processCompare(e, ">=");
    }

    public void visitGreaterThanExpression(GreaterThanExpression e)
        throws VisitorException {
        processCompare(e, ">");
    }

    public void visitInequalityExpression(InequalityExpression e)
        throws VisitorException {
        processCompare(e, "!=");
    }

    public void visitLessEqualExpression(LessEqualExpression e)
        throws VisitorException {
        processCompare(e, "<=");
    }

    public void visitLessThanExpression(LessThanExpression e)
        throws VisitorException {
        processCompare(e, "<");
    }
}
