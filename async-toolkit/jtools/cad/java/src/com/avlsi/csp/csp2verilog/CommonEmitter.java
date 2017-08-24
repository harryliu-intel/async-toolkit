package com.avlsi.csp.csp2verilog;

import java.math.BigInteger;
import java.io.PrintWriter;
import java.io.Writer;

import com.avlsi.csp.ast.*;
import com.avlsi.csp.util.CspUtils;
import com.avlsi.io.IndentPrintWriter;

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

    CommonEmitter(final PrintWriter out, final int registerBitWidth) {
        this.out = new LengthAwareWriter(1000, out);
        this.registerBitWidth = registerBitWidth;
        this.registerType = "bit signed [" + (registerBitWidth - 1) + ":0]";
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

    protected String getRegisterType(final IntegerType t)
        throws VisitorException {
        final boolean signed;
        final int width = getRegisterWidth(t);
        if (t.getDeclaredWidth() == null) {
            if (t instanceof TemporaryIntegerType && t.getInterval() != null) {
                signed = true;
            } else {
                return registerType;
            }
        } else {
            signed = t.isSigned();
        }
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

    // A simple bit range is one that can be translated to Verilog directly
    void processSimpleBitRange(BitRangeExpression e, boolean rhs)
        throws VisitorException {
        if (!rhs) out.print("($signed({1'd0, ");
        e.getBitsExpression().accept(this);
        processSimpleBitRangeIndex(e);
        if (!rhs) out.print("}))");
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
        private int length = 0;
        public LengthAwareWriter(final int limit, Writer out) {
            super(out);
            this.limit = limit;
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
            super.write(c);
            ++length;
        }
        public void write(char buf[], int off, int len) {
            super.write(buf, off, len);
            length += len;
        }
        public void write(String s, int off, int len) {
            super.write(s, off, len);
            length += len;
        }
        public void nextLevel(int x) {
            for (int i = 0; i < x; ++i) nextLevel();
        }
        public void prevLevel(int x) {
            for (int i = 0; i < x; ++i) prevLevel();
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
