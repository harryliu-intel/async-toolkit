/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.csp.util;

import java.lang.reflect.Constructor;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.BiConsumer;

import com.avlsi.csp.ast.*;
import com.avlsi.csp.csp2java.runtime.CspInteger;
import com.avlsi.csp.csp2java.runtime.CspValue;
import com.avlsi.csp.csp2java.runtime.Fold;
import com.avlsi.util.container.IterableIterator;

/**
 * A class to eliminate constant variables from a CSP program, and unrolls
 * iterators.
 **/
public class ConstantEvaluator implements VisitorInterface {
    private AbstractASTNodeInterface result;
    private final SymbolTable<Value> table;
    private boolean inLoop = false;
    private boolean inInitializers = false;
    private final Optional<ExpressionObserver> guardObserver;
    private final Optional<ExpressionObserver> actualObserver;

    public interface Value {
        Type getType();
        Object getValue();
    }

    public interface ExpressionObserver
        extends BiConsumer<ExpressionInterface,ExpressionInterface> {
    }

    /**
     * Returns a new {@link CSPProgram} that is the same as the given program,
     * except with all constant variables removed, and iterators with constant
     * bounds unrolled.
     *
     * @param p program to process
     * @return unrolled program with no constant variables
     **/
    public static CSPProgram evaluate(final CSPProgram p)
        throws VisitorException {
        return evaluate(p, Optional.empty(), Optional.empty());
    }

    /**
     * Returns a new {@link CSPProgram} that is the same as the given program,
     * except with all constant variables removed, and iterators with constant
     * bounds unrolled.
     *
     * @param p program to process
     * @param guardObserver observer of guard expression evaluation
     * @param actualObserver observer of actual parameter evaluation
     * @return unrolled program with no constant variables
     **/
    public static CSPProgram evaluate(
            final CSPProgram p,
            final ExpressionObserver guardObserver,
            final ExpressionObserver actualObserver)
        throws VisitorException {
        return evaluate(p,
                        Optional.of(guardObserver),
                        Optional.of(actualObserver));
    }

    private static CSPProgram evaluate(
            final CSPProgram p,
            final Optional<ExpressionObserver> guardObserver,
            final Optional<ExpressionObserver> actualObserver)
        throws VisitorException {
        final ConstantEvaluator e =
            new ConstantEvaluator(guardObserver, actualObserver);
        e.visitCSPProgram(p);
        return (CSPProgram) e.result;
    }

    private ConstantEvaluator(Optional<ExpressionObserver> guardObserver,
                              Optional<ExpressionObserver> actualObserver) {
        this.guardObserver = guardObserver;
        this.actualObserver = actualObserver;
        this.table = new SymbolTable<Value>();
    }

    private static CspInteger toCspInteger(final IntegerExpression e) {
        return new CspInteger(new BigInteger(e.getValue(), e.getRadix()));
    }

    private static IntegerExpression fromCspInteger(final CspInteger i) {
        return new IntegerExpression(i.toString(10), 10);
    }

    private static IntegerExpression fromValue(final Value v) {
        final CspInteger i = (CspInteger) v.getValue();
        if (v.getType() instanceof BooleanType) {
            return new BooleanExpression(i.booleanValue());
        } else {
            return fromCspInteger(i);
        }
    }

    private static String constArrayExpr(final IdentifierExpression id,
                                         final IntegerExpression e) {
        return id.getIdentifier() + "[" + e.getValue() + "]";
    }
    
    private static final Fold.BinaryFunction ADD =
        new Fold.BinaryFunction() {
            public CspValue evaluate(final CspValue a, final CspValue b) {
                return ((CspInteger) a).add((CspInteger) b);
            }
        };

    private static final Fold.BinaryFunction AND =
        new Fold.BinaryFunction() {
            public CspValue evaluate(final CspValue a, final CspValue b) {
                return ((CspInteger) a).and((CspInteger) b);
            }
        };

    private static final Fold.BinaryFunction DIVIDE =
        new Fold.BinaryFunction() {
            public CspValue evaluate(final CspValue a, final CspValue b) {
                return ((CspInteger) a).divide((CspInteger) b);
            }
        };

    private static final Fold.BinaryFunction POW =
        new Fold.BinaryFunction() {
            public CspValue evaluate(final CspValue a, final CspValue b) {
                return ((CspInteger) a).pow(((CspInteger) b).intValue());
            }
        };

    private static final Fold.BinaryFunction MULTIPLY =
        new Fold.BinaryFunction() {
            public CspValue evaluate(final CspValue a, final CspValue b) {
                return ((CspInteger) a).multiply((CspInteger) b);
            }
        };

    private static final Fold.BinaryFunction OR =
        new Fold.BinaryFunction() {
            public CspValue evaluate(final CspValue a, final CspValue b) {
                return ((CspInteger) a).or((CspInteger) b);
            }
        };

    private static final Fold.BinaryFunction XOR =
        new Fold.BinaryFunction() {
            public CspValue evaluate(final CspValue a, final CspValue b) {
                return ((CspInteger) a).xor((CspInteger) b);
            }
        };

    private static final Fold.BinaryFunction REMAINDER =
        new Fold.BinaryFunction() {
            public CspValue evaluate(final CspValue a, final CspValue b) {
                return ((CspInteger) a).remainder((CspInteger) b);
            }
        };

    private static final Fold.BinaryFunction SUBTRACT =
        new Fold.BinaryFunction() {
            public CspValue evaluate(final CspValue a, final CspValue b) {
                return ((CspInteger) a).subtract((CspInteger) b);
            }
        };

    private static final Fold.BinaryFunction LEFTSHIFT =
        new Fold.BinaryFunction() {
            public CspValue evaluate(final CspValue a, final CspValue b) {
                return ((CspInteger) a).shiftLeft(((CspInteger) b).intValue());
            }
        };

    private static final Fold.BinaryFunction RIGHTSHIFT =
        new Fold.BinaryFunction() {
            public CspValue evaluate(final CspValue a, final CspValue b) {
                return ((CspInteger) a).shiftRight(((CspInteger) b).intValue());
            }
        };

    private static final Fold.BinaryFunction AND2 =
        new Fold.BinaryFunction() {
            public CspValue evaluate(final CspValue a, final CspValue b) {
                return CspInteger.valueOf(((CspInteger) a).booleanValue() &&
                                          ((CspInteger) b).booleanValue());
            }
        };

    private static final Fold.BinaryFunction OR2 =
        new Fold.BinaryFunction() {
            public CspValue evaluate(final CspValue a, final CspValue b) {
                return CspInteger.valueOf(((CspInteger) a).booleanValue() ||
                                          ((CspInteger) b).booleanValue());
            }
        };

    private AbstractASTNodeInterface newBinaryExpression(
            final Class c,
            final ExpressionInterface left,
            final ExpressionInterface right) {
        try {
            final Constructor ctor =
                c.getConstructor(new Class[] { ExpressionInterface.class,
                                               ExpressionInterface.class });
            return (AbstractASTNodeInterface)
                ctor.newInstance(new Object[] { left, right });
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
                                                        
    private boolean isRealInteger(final AbstractASTNodeInterface n) {
        return   n instanceof IntegerExpression &&
               !(n instanceof BooleanExpression);
    }

    private void processBinaryExpression(final AbstractBinaryExpression e,
                                         final Fold.BinaryFunction op)
    throws VisitorException {
        e.getLeft().accept(this);
        final AbstractASTNodeInterface left = result;
        e.getRight().accept(this);

        if (left instanceof BooleanExpression &&
            result instanceof BooleanExpression) {
            result =
                new BooleanExpression(
                    ((CspInteger) 
                        op.evaluate(toCspInteger((IntegerExpression) left),
                                    toCspInteger((IntegerExpression) result)))
                    .booleanValue());
        } else if (isRealInteger(left) && isRealInteger(result)) {
            result =
                fromCspInteger(
                        (CspInteger)
                        op.evaluate(
                            toCspInteger((IntegerExpression) left),
                            toCspInteger((IntegerExpression) result)));
        } else if (e.getLeft() == left && e.getRight() == result) {
            result = e;
        } else  {
            result = newBinaryExpression(e.getClass(),
                                         (ExpressionInterface) left,
                                         (ExpressionInterface) result);
        }
        result.epr(e);
    }

    /* Arithmetic binary operators -------------------------------------- */
    public void visitAddExpression(AddExpression e) throws VisitorException
    {
        processBinaryExpression(e, ADD);
    }
    public void visitAndExpression(AndExpression e) throws VisitorException
    {
        processBinaryExpression(e, AND);
    }
    public void visitConditionalAndExpression(ConditionalAndExpression e)
        throws VisitorException {
        processBinaryExpression(e, AND2);
    }
    public void visitConditionalOrExpression(ConditionalOrExpression e)
        throws VisitorException {
        processBinaryExpression(e, OR2);
    }
    public void visitDivideExpression(DivideExpression e)
        throws VisitorException
    {
        processBinaryExpression(e, DIVIDE);
    }
    public void visitExponentialExpression(ExponentialExpression e)
        throws VisitorException
    {
        processBinaryExpression(e, POW);
    }
    public void visitMultiplyExpression(MultiplyExpression e)
        throws VisitorException
    {
        processBinaryExpression(e, MULTIPLY);
    }
    public void visitOrExpression(OrExpression e) throws VisitorException
    {
        processBinaryExpression(e, OR);
    }
    public void visitXorExpression(XorExpression e) throws VisitorException
    {
        processBinaryExpression(e, XOR);
    }    
    public void visitRemainderExpression(RemainderExpression e)
        throws VisitorException
    {
        processBinaryExpression(e, REMAINDER);
    }
    public void visitSubtractExpression(SubtractExpression e)
        throws VisitorException
    {
        processBinaryExpression(e, SUBTRACT);
    }
    public void visitLeftShiftExpression(LeftShiftExpression e)
        throws VisitorException
    {
        processBinaryExpression(e, LEFTSHIFT);
    }
    public void visitRightShiftExpression(RightShiftExpression e)
        throws VisitorException
    {
        processBinaryExpression(e, RIGHTSHIFT);
    }

    /* Arithmetic unary operators --------------------------------------- */
    private interface UnaryFunction {
        CspInteger evaluate(CspInteger a);
    }
    private static final UnaryFunction NEGATE =
        new UnaryFunction() {
            public CspInteger evaluate(final CspInteger a) {
                return a.negate();
            }
        };
    private static final UnaryFunction NOT =
        new UnaryFunction() {
            public CspInteger evaluate(final CspInteger a) {
                return a.not();
            }
        };

    private AbstractASTNodeInterface newUnaryExpression(
            final Class c,
            final ExpressionInterface operand) {
        try {
            final Constructor ctor =
                c.getConstructor(new Class[] { ExpressionInterface.class });
            return (AbstractASTNodeInterface)
                ctor.newInstance(new Object[] { operand });
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private void processUnaryExpression(final AbstractUnaryExpression e,
                                        final UnaryFunction op)
    throws VisitorException {
        e.getExpression().accept(this);

        if (result instanceof BooleanExpression) {
            result = 
                new BooleanExpression(
                    ((CspInteger)
                        op.evaluate(toCspInteger((IntegerExpression) result)))
                    .booleanValue());
        } else if (isRealInteger(result)) {
            result =
                fromCspInteger(
                        op.evaluate(
                            toCspInteger((IntegerExpression) result)));
        } else if (e.getExpression() == result) {
            result = e;
        } else {
            result = newUnaryExpression(e.getClass(),
                                        (ExpressionInterface) result);
        }
        result.epr(e);
    }
    public void visitNegateExpression(NegateExpression e)
        throws VisitorException
    {
        processUnaryExpression(e, NEGATE);
    }
    public void visitNotExpression(NotExpression e) throws VisitorException
    {
        processUnaryExpression(e, NOT);
    }

    /* Relational operators --------------------------------------------- */
    private static int intCompare(final CspValue a, final CspValue b) {
        return ((CspInteger) a).compareTo((CspInteger) b);
    }

    private static final Fold.BinaryFunction EQ =
        new Fold.BinaryFunction() {
            public CspValue evaluate(final CspValue a, final CspValue b) {
                return intCompare(a, b) == 0 ? CspInteger.ZERO.not()
                                             : CspInteger.ZERO;
            }
        };

    private static final Fold.BinaryFunction NE =
        new Fold.BinaryFunction() {
            public CspValue evaluate(final CspValue a, final CspValue b) {
                return intCompare(a, b) != 0 ? CspInteger.ZERO.not()
                                             : CspInteger.ZERO;
            }
        };

    private static final Fold.BinaryFunction LE =
        new Fold.BinaryFunction() {
            public CspValue evaluate(final CspValue a, final CspValue b) {
                return intCompare(a, b) <= 0 ? CspInteger.ZERO.not()
                                             : CspInteger.ZERO;
            }
        };

    private static final Fold.BinaryFunction LT =
        new Fold.BinaryFunction() {
            public CspValue evaluate(final CspValue a, final CspValue b) {
                return intCompare(a, b) < 0 ? CspInteger.ZERO.not()
                                            : CspInteger.ZERO;
            }
        };

    private static final Fold.BinaryFunction GT =
        new Fold.BinaryFunction() {
            public CspValue evaluate(final CspValue a, final CspValue b) {
                return intCompare(a, b) > 0 ? CspInteger.ZERO.not()
                                            : CspInteger.ZERO;
            }
        };

    private static final Fold.BinaryFunction GE =
        new Fold.BinaryFunction() {
            public CspValue evaluate(final CspValue a, final CspValue b) {
                return intCompare(a, b) >= 0 ? CspInteger.ZERO.not()
                                             : CspInteger.ZERO;
            }
        };

    private void processBinaryComparison(final AbstractBinaryExpression e,
                                         final Fold.BinaryFunction op)
    throws VisitorException {
        processBinaryExpression(e, op);
        if (isRealInteger(result)) {
            result =
                new BooleanExpression(toCspInteger((IntegerExpression) result)
                                      .booleanValue());
        }
    }

    public void visitEqualityExpression(EqualityExpression e)
        throws VisitorException
    {
        processBinaryComparison(e, EQ);
    }
    public void visitInequalityExpression(InequalityExpression e)
        throws VisitorException
    {
        processBinaryComparison(e, NE);
    }
    public void visitGreaterEqualExpression(GreaterEqualExpression e)
        throws VisitorException
    {
        processBinaryComparison(e, GE);
    }
    public void visitGreaterThanExpression(GreaterThanExpression e)
        throws VisitorException
    {
        processBinaryComparison(e, GT);
    }
    public void visitLessEqualExpression(LessEqualExpression e)
        throws VisitorException
    {
        processBinaryComparison(e, LE);
    }
    public void visitLessThanExpression(LessThanExpression e)
        throws VisitorException
    {
        processBinaryComparison(e, LT);
    }

    /* Primary expressions ---------------------------------------------- */
    public void visitArrayAccessExpression(ArrayAccessExpression e)
        throws VisitorException
    {
        e.getArrayExpression().accept(this);
        final ExpressionInterface arrayExpr = (ExpressionInterface) result;
        e.getIndexExpression().accept(this);

        // might be an access to a const array
        if (arrayExpr instanceof IdentifierExpression &&
            result instanceof IntegerExpression) {
            final IdentifierExpression id = (IdentifierExpression) arrayExpr;
            final Value v =
                table.lookup(constArrayExpr((IdentifierExpression) arrayExpr,
                                            (IntegerExpression) result));
            if (v != null && v.getValue() != null) {
                result = fromValue(v);
                result.epr(e);
                return;
            }
        }

        if (e.getArrayExpression() == arrayExpr &&
            e.getIndexExpression() == result) {
            result = e;
        } else {
            result = new ArrayAccessExpression(arrayExpr,
                                               (ExpressionInterface) result);
            result.epr(e);
        }
    }
    public void visitStructureAccessExpression(StructureAccessExpression e)
        throws VisitorException
    {
        e.getStructureExpression().accept(this);
        if (e.getStructureExpression() == result) {
            result = e;
        } else {
            result = new StructureAccessExpression((ExpressionInterface) result,
                                                   e.getFieldName());
            result.epr(e);
        }
    }
    public void visitMemberAccessExpression(MemberAccessExpression e)
        throws VisitorException
    {
        e.getStructureExpression().accept(this);
        if (e.getStructureExpression() == result) {
            result = e;
        } else {
            result = new MemberAccessExpression((ExpressionInterface) result,
                                                 e.getMemberName());
            result.epr(e);
        }
    }
    public void visitBitRangeExpression(BitRangeExpression e)
        throws VisitorException
    {
        e.getBitsExpression().accept(this);
        final ExpressionInterface bitsExpr = (ExpressionInterface) result;
        if (e.getMinExpression() == null) result = null;
        else e.getMinExpression().accept(this);
        final ExpressionInterface minExpr = (ExpressionInterface) result;
        e.getMaxExpression().accept(this);

        if (e.getBitsExpression() == bitsExpr &&
            e.getMinExpression() == minExpr &&
            e.getMaxExpression() == result) {
            result = e;
        } else {
            final ExpressionInterface maxExpr = (ExpressionInterface) result;
            result = new BitRangeExpression(bitsExpr, minExpr, maxExpr);
            if (bitsExpr instanceof IntegerExpression &&
                minExpr instanceof IntegerExpression && 
                maxExpr instanceof IntegerExpression) {
                ((BitRangeExpression) result).getValueExpression().accept(this);
                assert result instanceof IntegerExpression;
            }
            result.epr(e);
        }
    }
    public void visitFunctionCallExpression(FunctionCallExpression e)
    throws VisitorException {
        e.getFunctionExpression().accept(this);
        final ExpressionInterface functionExpr = (ExpressionInterface) result;
        
        boolean same = e.getFunctionExpression() == functionExpr;

        final FunctionCallExpression ne =
            new FunctionCallExpression(functionExpr);
        int count = 0;
        for (Iterator i = e.getActuals(); i.hasNext(); count++) {
            final ExpressionInterface arg = (ExpressionInterface) i.next();
            arg.accept(this);
            ne.addActual((ExpressionInterface) result);
            actualObserver.ifPresent(
                    obs -> obs.accept(arg, (ExpressionInterface) result));
            same &= arg == result;
        }

        result = same ? e : ne;

        // XXX: Hardcode support for log2, log4 and choose.  This should call
        // RefinementResolver to determine if a function is built-in or not,
        // and then use a more general mechanism to evaluate the result.
        if (functionExpr instanceof IdentifierExpression && count >= 1 &&
            ne.getActuals().next() instanceof IntegerExpression) {
            final String id =
                ((IdentifierExpression) functionExpr).getIdentifier();
            final Iterator args = ne.getActuals();
            final CspInteger val =
                toCspInteger((IntegerExpression) args.next());
            if ("log2".equals(id) && count == 1) {
                result = fromCspInteger(val.log2());
            } else if ("log4".equals(id) && count == 1) {
                result = fromCspInteger(val.log4());
            } else if ("choose".equals(id) && count == 3) {
                final ExpressionInterface trueExpr =
                    (ExpressionInterface) args.next();
                final ExpressionInterface falseExpr =
                    (ExpressionInterface) args.next();
                result = val.booleanValue() ? trueExpr : falseExpr;
            }
        }

        result.epr(e);
    }

    private void processIdentifierExpression(IdentifierExpression e) {
        result = inLoop ? new IdentifierExpression(e.getIdentifier()).epr(e)
                        : e;
    }

    public void visitIdentifierExpression(IdentifierExpression e)
        throws VisitorException
    {
        final Value v = table.lookup(e.getIdentifier());
        if (v == null || v.getValue() == null) {
            processIdentifierExpression(e);
        } else {
            result = fromValue(v);
            result.epr(e);
        }
    }
    public void visitIntegerExpression(IntegerExpression e)
        throws VisitorException
    {
        result = e;
    }

    public void visitStringExpression(StringExpression e)
        throws VisitorException {
        result = e;
    }

    private interface BinaryExpressionFactory {
        AbstractBinaryExpression makeExpression(ExpressionInterface left,
                                                ExpressionInterface right);
    }
    private static final BinaryExpressionFactory AND_MAKER =
        new BinaryExpressionFactory() {
            public AbstractBinaryExpression makeExpression(
                    ExpressionInterface left,
                    ExpressionInterface right) {
                return new AndExpression(left, right);
            }
        };
    private static final BinaryExpressionFactory OR_MAKER =
        new BinaryExpressionFactory() {
            public AbstractBinaryExpression makeExpression(
                    ExpressionInterface left,
                    ExpressionInterface right) {
                return new OrExpression(left, right);
            }
        };
    private static final BinaryExpressionFactory XOR_MAKER =
        new BinaryExpressionFactory() {
            public AbstractBinaryExpression makeExpression(
                    ExpressionInterface left,
                    ExpressionInterface right) {
                return new XorExpression(left, right);
            }
        };
    private static final BinaryExpressionFactory ADD_MAKER =
        new BinaryExpressionFactory() {
            public AbstractBinaryExpression makeExpression(
                    ExpressionInterface left,
                    ExpressionInterface right) {
                return new AddExpression(left, right);
            }
        };
    private static final BinaryExpressionFactory MULTIPLY_MAKER =
        new BinaryExpressionFactory() {
            public AbstractBinaryExpression makeExpression(
                    ExpressionInterface left,
                    ExpressionInterface right) {
                return new MultiplyExpression(left, right);
            }
        };

    private Range processRange(final Range range) throws VisitorException {
        range.getMinExpression().accept(this);
        final ExpressionInterface min = (ExpressionInterface) result;
        range.getMaxExpression().accept(this);

        if (range.getMinExpression() == min &&
            range.getMaxExpression() == result) {
            return range;
        } else {
            return (Range)
                new Range(min, (ExpressionInterface) result).epr(range);
        }
    }

    private interface Looper {
        void begin() throws VisitorException;
        void loop() throws VisitorException;
        void end() throws VisitorException;
    }

    private void processAbstractLoop(final IntegerExpression min,
                                     final IntegerExpression max,
                                     final String indexVar,
                                     final Looper looper)
    throws VisitorException {
        final CspInteger minVal = toCspInteger((IntegerExpression) min);
        final CspInteger maxVal = toCspInteger((IntegerExpression) max);
        final boolean oldInLoop = inLoop;
        inLoop = true;
        looper.begin();
        for (CspInteger i = minVal; i.compareTo(maxVal) <= 0;
             i = i.add(CspInteger.ONE)) {
            final CspInteger loopVar = i;
            table.enterScope();
            table.bind(indexVar,
                       new Value() {
                           public Type getType() {
                               return new IntegerType();
                           }
                           public Object getValue() {
                               return loopVar;
                           }
                       });
            looper.loop();
            table.leaveScope();
        }
        looper.end();
        inLoop = oldInLoop;
    }

    public void visitLoopExpression(final LoopExpression e)
        throws VisitorException
    {
        final BinaryExpressionFactory factory;
        switch (e.getSeparator()) {
          case LoopExpression.AND:   factory = AND_MAKER;
                                     break;
          case LoopExpression.OR:    factory = OR_MAKER;
                                     break;
          case LoopExpression.XOR:   factory = XOR_MAKER;
                                     break;
          case LoopExpression.TIMES: factory = MULTIPLY_MAKER;
                                     break;
          case LoopExpression.PLUS:  factory = ADD_MAKER;
                                     break;
          default:
                                     throw new AssertionError(e);
        }

        // resolve constants in the loop expression
        e.getExpression().accept(this);
        final ExpressionInterface expr = (ExpressionInterface) result;

        final Range range = processRange(e.getRange());
        final ExpressionInterface min = range.getMinExpression();
        final ExpressionInterface max = range.getMaxExpression();

        if (min instanceof IntegerExpression &&
            max instanceof IntegerExpression) {
            processAbstractLoop((IntegerExpression) min,
                                (IntegerExpression) max, e.getIndexVar(),
                                new Looper() {
                ExpressionInterface unit = null;
                public void begin() throws VisitorException { }
                public void loop() throws VisitorException {
                    expr.accept(ConstantEvaluator.this);
                    if (unit == null) {
                        unit = (ExpressionInterface) result;
                    } else {
                        unit = factory.makeExpression(
                            unit, (ExpressionInterface) result);
                    }
                }
                public void end() throws VisitorException {
                    unit.accept(ConstantEvaluator.this);
                }
            });
        } else if (e.getRange() == range && e.getExpression() == expr) {
            result = e;
        } else {
            result = new LoopExpression(e.getIndexVar(), range,
                                        e.getSeparator(), expr);
        }
        result.epr(e);
    }

    /* Channel expressions ---------------------------------------------- */
    private void processChannelExpression(final AbstractChannelExpression e)
    throws VisitorException {
        e.getChannelExpression().accept(this);
        
        if (e.getChannelExpression() == result) {
            result = e;
        } else {
            result = newUnaryExpression(e.getClass(),
                                        (ExpressionInterface) result);
            result.epr(e);
        }
    }
    public void visitPeekExpression(PeekExpression e) throws VisitorException
    {
        processChannelExpression(e);
    }
    public void visitProbeExpression(ProbeExpression e)
        throws VisitorException
    {
        processChannelExpression(e);
    }
    public void visitReceiveExpression(ReceiveExpression e)
        throws VisitorException
    {
        processChannelExpression(e);
    }

    /* Simple statements ------------------------------------------------ */
    public void visitErrorStatement(ErrorStatement s) throws VisitorException
    {
        result = s;
    }
    public void visitSkipStatement(SkipStatement s) throws VisitorException
    {
        result = s;
    }
    public void visitExpressionStatement(ExpressionStatement s)
        throws VisitorException
    {
        s.getExpression().accept(this);

        if (s.getExpression() == result) {
            result = s;
        } else {
            result = new ExpressionStatement((ExpressionInterface) result);
            result.epr(s);
        }
    }

    /* Channel statements ----------------------------------------------- */
    private void processChannelStatement(final AbstractChannelStatement s)
    throws VisitorException {
        s.getChannelExpression().accept(this);
        final ExpressionInterface chanExpr = (ExpressionInterface) result;
        if (s.getRightHandSide() == null) {
            result = null;
        } else {
            s.getRightHandSide().accept(this);
        }
        result = newBinaryExpression(s.getClass(), chanExpr,
                                     (ExpressionInterface) result);
        result.epr(s);
    }
    public void visitReceiveStatement(ReceiveStatement s)
        throws VisitorException
    {
        processChannelStatement(s);
    }
    public void visitSendStatement(SendStatement s) throws VisitorException
    {
        processChannelStatement(s);
    }

    /* Compound statements ---------------------------------------------- */
    private void processCompositeStatement(final AbstractCompositeStatement s,
                                           final AbstractCompositeStatement x)
    throws VisitorException {
        for (Iterator i = s.getStatements(); i.hasNext(); ) {
            final StatementInterface si = (StatementInterface) i.next();
            si.accept(this);
            x.addStatement((StatementInterface) result);
        }
        result = x;
        result.epr(s);
    }
    public void visitParallelStatement(ParallelStatement s)
        throws VisitorException
    {
        processCompositeStatement(s, new ParallelStatement());
    }
    public void visitSequentialStatement(SequentialStatement s)
        throws VisitorException
    {
        table.enterScope();
        processCompositeStatement(s, new SequentialStatement());
        table.leaveScope();
    }

    /* Guarded statements ----------------------------------------------- */
    private Object processGuardedCommand(final GuardedCommandInterface g)
    throws VisitorException {
        if (g instanceof GuardedCommand) {
            final GuardedCommand gc = (GuardedCommand) g;
            gc.getGuard().accept(this);
            final ExpressionInterface guard = (ExpressionInterface) result;
            gc.getCommand().accept(this);
            final StatementInterface command = (StatementInterface) result;
            processLinkageTerms(gc.getLinkageTerms());
            
            guardObserver.ifPresent(obs -> obs.accept(gc.getGuard(), guard));

            if (gc.getGuard() == guard && gc.getCommand() == command &&
                gc.getLinkageTerms() == result) {
                result = g;
            } else {
                result = new GuardedCommand(guard, (LinkageTerms) result,
                                            command);
                result.epr(g);
            }
            return result;
        } else if (g instanceof LoopGuard) {
            return processLoopGuard((LoopGuard) g);
        } else {
            throw new AssertionError();
        }
    }
    private void processAbstractGuardedStatement(
            final AbstractGuardedStatement s,
            final AbstractGuardedStatement x) throws VisitorException {
        for (Iterator i = s.getGuardedCommands(); i.hasNext(); ) {
            final GuardedCommandInterface g =
                (GuardedCommandInterface) i.next();
            final Object o = processGuardedCommand(g);
            if (o instanceof Collection) {
                for (Iterator j = ((Collection) o).iterator(); j.hasNext(); ) {
                    x.addGuardedCommand((GuardedCommandInterface) j.next());
                }
            } else {
                x.addGuardedCommand((GuardedCommandInterface) o);
            }
        }
        final StatementInterface els = s.getElseStatement();
        if (els != null) {
            els.accept(this);
            x.addElseStatement((StatementInterface) result);
        }
        result = x;
        result.epr(s);
    }
    public void
        visitDeterministicRepetitionStatement
        (DeterministicRepetitionStatement s) throws VisitorException
    {
        processAbstractGuardedStatement(
                s, new DeterministicRepetitionStatement());
    }
    /**
     * Returns true if all guard commands are simple guarded commands, with the
     * guards are constant false; false otherwise.
     **/
    private boolean allGuardsFalse(final AbstractGuardedStatement s) {
        for (GuardedCommandInterface g :
                new IterableIterator<GuardedCommandInterface>(
                    s.getGuardedCommands())) {
            if (g instanceof GuardedCommand) {
                final GuardedCommand gc = (GuardedCommand) g;
                if (gc.getGuard() instanceof IntegerExpression) {
                    final CspInteger expr =
                        toCspInteger((IntegerExpression) gc.getGuard());
                    if (!expr.booleanValue()) {
                        continue;
                    }
                }
            }
            return false;
        }
        return true;
    }
    public void
        visitDeterministicSelectionStatement
        (DeterministicSelectionStatement s) throws VisitorException
    {
        final DeterministicSelectionStatement x =
            new DeterministicSelectionStatement();
        processAbstractGuardedStatement(s, x);

        // match: [ false -> .. [] false -> .. [] ... [] else -> skip ]
        // replace with: skip
        if (x.getElseStatement() instanceof SkipStatement &&
            allGuardsFalse(x)) {
            result = new SkipStatement();
            result.epr(s);
        }
    }
    public void
        visitNonDeterministicRepetitionStatement
        (NonDeterministicRepetitionStatement s) throws VisitorException
    {
        processAbstractGuardedStatement(
                s, new NonDeterministicRepetitionStatement());
    }
    public void
        visitNonDeterministicSelectionStatement
        (NonDeterministicSelectionStatement s) throws VisitorException
    {
        processAbstractGuardedStatement(
                s, new NonDeterministicSelectionStatement());
    }
    public void visitLoopGuard(LoopGuard s) throws VisitorException
    {
        throw new AssertionError();
    }
    private List processLoopGuard(final LoopGuard s) throws VisitorException
    {
        final Range range = processRange(s.getRange());
        final ExpressionInterface min = range.getMinExpression();
        final ExpressionInterface max = range.getMaxExpression();

        final List l = new ArrayList();
        if (min instanceof IntegerExpression &&
            max instanceof IntegerExpression) {
            processAbstractLoop((IntegerExpression) min,
                                (IntegerExpression) max, s.getIndexVar(),
                                new Looper() {
                ExpressionInterface unit = null;
                public void begin() throws VisitorException { }
                public void loop() throws VisitorException {
                    for (Iterator i = s.getGuards().iterator(); i.hasNext(); ) {
                        final GuardedCommandInterface gci =
                            (GuardedCommandInterface) i.next();
                        final Object o = processGuardedCommand(gci);
                        if (o instanceof Collection) {
                            for (Iterator j = ((Collection) o).iterator();
                                 j.hasNext(); ) {
                                l.add(j.next());
                            }
                        } else {
                            l.add(o);
                        }
                    }
                }
                public void end() throws VisitorException { }
            });
        } else {
            final LoopGuard ret =
                new LoopGuard(s.getIndexVar(), range, s.getSeparator());
            for (Iterator i = s.getGuards().iterator(); i.hasNext(); ) {
                final GuardedCommandInterface gci =
                    (GuardedCommandInterface) i.next();
                final Object o = processGuardedCommand(gci);
                if (o instanceof Collection) {
                    for (Iterator j = ((Collection) o).iterator();
                         j.hasNext(); ) {
                        ret.addGuard((GuardedCommandInterface) j.next());
                    }
                } else {
                    ret.addGuard((GuardedCommandInterface) o);
                }
            }
            l.add(ret);
        }
        return l;
    }
    private interface DeclaratorProcessor {
        Declarator execute(final Declarator d) throws VisitorException;
    }
    private void processDeclarationList(final DeclarationList dlst,
                                        final DeclaratorProcessor doer)
    throws VisitorException {
        final DeclarationList ndlst = new DeclarationList();
        boolean sameDlst = true;
        for (Iterator i = dlst.getDeclarations(); i.hasNext(); ) {
            final Declaration decl = (Declaration) i.next();
            final DeclaratorList dclr = decl.getDeclaratorList();
            final DeclaratorList ndclr = new DeclaratorList();
            boolean sameDclr = true; // can we reuse the original declaration?
            boolean oneDclr = false; // at least one declarator left?
            for (Iterator j = dclr.getDeclarators(); j.hasNext(); ) {
                final Declarator d = (Declarator) j.next();
                final Declarator newd = doer.execute(d);
                // newd is null if the declarator should be removed

                if (newd == null) {
                    sameDclr = false;
                } else {
                    ndclr.addDeclarator(newd);
                    oneDclr = true;
                    if (newd != d) {
                        newd.epr(d);
                        sameDclr = false;
                    }
                }
            }
            sameDlst &= sameDclr;
            if (oneDclr) {
                if (sameDclr) {
                    ndlst.addDeclaration(decl);
                } else {
                    ndlst.addDeclaration(
                            (Declaration) new Declaration(ndclr).epr(decl));
                }
            }
        }
        result = sameDlst ? dlst : (DeclarationList) ndlst.epr(dlst);
    }
    private final DeclaratorProcessor PROCESS_DECLARATOR =
        new DeclaratorProcessor() {
            public Declarator execute(final Declarator d)
            throws VisitorException {
                d.getTypeFragment().accept(ConstantEvaluator.this);
                final Type frag = (Type) result;
                if (d.getInitializer() == null) result = null;
                else d.getInitializer().accept(ConstantEvaluator.this);
                if (d.getTypeFragment() == frag &&
                    d.getInitializer() == result) {
                    return d;
                } else {
                    return new Declarator(d.getIdentifier(), frag,
                                          (ExpressionInterface) result,
                                          d.getDirection());
                }
            }
        };
    private void processFunctionDeclaration(final FunctionDeclaration decl)
    throws VisitorException {
        processDeclarationList(decl.getFormals(), PROCESS_DECLARATOR);
        final DeclarationList formals = (DeclarationList) result;
        final Type retType;
        if (decl.getReturnType() == null) {
            retType = null;
        } else {
            decl.getReturnType().accept(this);
            retType = (Type) result;
        }
        decl.getBodyStatement().accept(this);
        if (formals == decl.getFormals() && retType == decl.getReturnType() &&
            result == decl.getBodyStatement()) {
            result = decl;
        } else {
            result = new FunctionDeclaration(decl.getName(), formals, retType,
                                             (StatementInterface) result);
            result.epr(decl);
        }
    }

    private void processStructureDeclaration(final StructureDeclaration decl)
    throws VisitorException {
        processDeclarationList(decl.getDeclarations(), PROCESS_DECLARATOR);
        final DeclarationList members = (DeclarationList) result;
        if (members == decl.getDeclarations()) {
            result = decl;
        } else {
            result = new StructureDeclaration(decl.getName(), members);
            result.epr(decl);
        }
    }

    private static SequentialStatement resolveInitializerStatement(
            final CSPProgram p) {
        SequentialStatement s = p.getInitializerStatement();
        for (Iterator i = p.getRefinementParents().iterator();
             s == null && i.hasNext(); ) {
            final CSPProgram parent = (CSPProgram) i.next();
            if (!p.inheritDeclarationOnly(parent))
                s = resolveInitializerStatement(parent);
        }
        return s;
    }

    public void visitCSPProgram(CSPProgram p) throws VisitorException {
        final CSPProgram ncsp = new CSPProgram();

        final SequentialStatement initializers = resolveInitializerStatement(p);

        table.enterScope();
        if (initializers != null) {
            inInitializers = true;
            for (Iterator i = initializers.getStatements(); i.hasNext(); ) {
                ((StatementInterface) i.next()).accept(this);
            }
            inInitializers = false;
        }
        ncsp.setInitializerStatement(p.getInitializerStatement());
        if (p.getStatement() != null) {
            p.getStatement().accept(this);
            ncsp.setStatement((StatementInterface) result);
        }
        for (Iterator i = p.getFunctionDeclarations(); i.hasNext(); ) {
            final FunctionDeclaration funDecl = (FunctionDeclaration) i.next();
            processFunctionDeclaration(funDecl);
            ncsp.addFunctionDeclaration((FunctionDeclaration) result);
        }
        for (Iterator i = p.getStructureIterator(); i.hasNext(); ) {
            processStructureDeclaration((StructureDeclaration) i.next());
            ncsp.addStructureDeclaration((StructureDeclaration) result);
        }
        for (Iterator i = p.getRefinementParents().iterator(); i.hasNext(); ) {
            final CSPProgram parent = (CSPProgram) i.next();
            ncsp.refineFrom(ConstantEvaluator.evaluate(parent));
        }
        table.leaveScope();
        ncsp.epr(p);
        result = ncsp;
    }

    public void visitAssignmentStatement(AssignmentStatement s)
        throws VisitorException
    {
        s.getLeftHandSide().accept(this);
        final ExpressionInterface lhs = (ExpressionInterface) result;
        s.getRightHandSide().accept(this);

        if (inInitializers && lhs instanceof ArrayAccessExpression &&
            result instanceof IntegerExpression) {
            final ArrayAccessExpression access = (ArrayAccessExpression) lhs;
            final ExpressionInterface arrayExpr = access.getArrayExpression();
            final ExpressionInterface indexExpr = access.getIndexExpression();
            if (arrayExpr instanceof IdentifierExpression &&
                indexExpr instanceof IntegerExpression) {
                final CspInteger var = toCspInteger((IntegerExpression) result);
                final IdentifierExpression id = (IdentifierExpression) arrayExpr;
                final String varName =
                    constArrayExpr(id, (IntegerExpression) indexExpr);
                final Value v = table.lookup(id.getIdentifier());
                final boolean boolArray;
                if (v != null && v.getValue() == null) {
                    final Type et = ((ArrayType) v.getType()).getElementType();
                    boolArray = et instanceof BooleanType;
                } else {
                    boolArray = false;
                }

                final boolean firstDecl =
                    table.bind(varName,
                               new Value() {
                                   public Type getType() {
                                       return boolArray ? new BooleanType()
                                                        : new IntegerType();
                                   }
                                   public Object getValue() {
                                       return var;
                                   }
                               });
                if (!firstDecl) {
                    System.err.println("Warning: element in const array " + varName + " redefined at " + s.getParseRange().fullString());
                }
            }
        } else if (s.getLeftHandSide() == lhs &&
                   s.getRightHandSide() == result) {
            result = s;
        } else if (lhs instanceof IntegerExpression) {
            throw new VisitorException("Left hand side of the assignment statement is a constant expression at " + s.getParseRange().fullString());
        } else {
            result = new AssignmentStatement(lhs, (ExpressionInterface) result,
                                             s.getKind());
            result.epr(s);
        }
    }
    public void visitIncDecStatement(final IncDecStatement s)
        throws VisitorException
    {
        s.getExpression().accept(this);
        if (s.getExpression() == result) {
            result = s;
        } else {
            result = new IncDecStatement((ExpressionInterface) result,
                                         s.isIncrement()).epr(s);
        }
    }
    public void visitLoopStatement(final LoopStatement s)
        throws VisitorException
    {
        s.getStatement().accept(this);
        final StatementInterface stmt = (StatementInterface) result;
        final Range range = processRange(s.getRange());

        final int sep = s.getSeparator();
        if (sep == LoopStatement.PARALLEL) {
            final AbstractCompositeStatement acs =
                sep == LoopStatement.SEQUENTIAL ?
                    (AbstractCompositeStatement) new SequentialStatement() :
                    (AbstractCompositeStatement) new ParallelStatement();
            final ExpressionInterface min = range.getMinExpression();
            final ExpressionInterface max = range.getMaxExpression();

            if (min instanceof IntegerExpression &&
                max instanceof IntegerExpression) {
                processAbstractLoop((IntegerExpression) min,
                                    (IntegerExpression) max,
                                    s.getIndexVar(), new Looper() {
                    public void begin() throws VisitorException { acs.epr(s); }
                    public void loop() throws VisitorException {
                        stmt.accept(ConstantEvaluator.this);
                        acs.addStatement((StatementInterface) result);
                    }
                    public void end() throws VisitorException {
                        result = acs;
                    }
                });
                return;
            }
        }

        if (s.getRange() == range && s.getStatement() == stmt) {
            result = s;
        } else {
            result = new LoopStatement(s.getIndexVar(), range, sep, stmt);
            result.epr(s);
        }
    }
    public void visitVarStatement(VarStatement s) throws VisitorException
    {
        StatementInterface si = s.getStatement();
        if (si != null) {
            si.accept(this);
            si = (StatementInterface) result;
        }
        processDeclarationList(
            s.getDeclarationList(),
            new DeclaratorProcessor() {
                public Declarator execute(final Declarator d)
                throws VisitorException {
                    d.getTypeFragment().accept(ConstantEvaluator.this);
                    final Type ty = (Type) result;
                    // Do not visit id because if it was defined as a constant,
                    // it would be turned into an IntegerExpression; but we
                    // must still take care to return a new object if we are
                    // inside a loop
                    processIdentifierExpression(d.getIdentifier());
                    final IdentifierExpression id =
                        (IdentifierExpression) result;
                    if (inInitializers && ty instanceof ArrayType) {
                        // Store types of arrays, so that when we bind array
                        // elements, we correctly choose BooleanExpression or
                        // IntegerExpression
                        table.bind(id.getIdentifier(),
                                   new Value() {
                                       public Type getType() {
                                           return Type.clone(ty);
                                       }
                                       public Object getValue() {
                                           return null;
                                       }
                                   });
                    }
                    final ExpressionInterface init = d.getInitializer();
                    if (init == null) result = null;
                    else init.accept(ConstantEvaluator.this);
                    final boolean constInt = (ty instanceof IntegerType) &&
                                             ((IntegerType) ty).isConst();
                    final boolean constBool = (ty instanceof BooleanType) &&
                                             ((BooleanType) ty).isConst();
                    if ((constInt || constBool) &&
                        result instanceof IntegerExpression) {
                        final CspInteger var =
                            toCspInteger((IntegerExpression) result);
                        final boolean firstDecl =
                            table.bind(id.getIdentifier(),
                                       new Value() {
                                           public Type getType() {
                                               return constInt ?
                                                   new IntegerType()
                                                 : new BooleanType();
                                           }
                                           public Object getValue() {
                                               return var;
                                           }
                                       });
                        if (!firstDecl) {
                            System.err.println("Warning: const int/bool " +
                                    id.getIdentifier() + " redeclared at " +
                                    id.getParseRange().fullString());
                        }
                        return null;
                    } else if (d.getInitializer() != result ||
                               d.getIdentifier() != id ||
                               d.getTypeFragment() != ty) {
                        return new Declarator(id, ty,
                                              (ExpressionInterface) result,
                                              d.getDirection());
                    } else {
                        return d;
                    }
                }
            });
        if (result == s.getDeclarationList() && si == s.getStatement()) {
            result = s;
        } else {
            result =
                new VarStatement((DeclarationList) result, s.getStatement());
            result.epr(s);
        }
    }
    private void processType(Type t) throws VisitorException {
        result = inLoop ? Type.clone(t) : t;
    }
    public void visitArrayType(ArrayType t) throws VisitorException
    {
        final Range r = processRange(t.getRange());
        t.getElementType().accept(this);
        if (r == t.getRange() && result == t.getElementType()) {
            result = t;
        } else {
            result = new ArrayType(r, (Type) result);
            result.epr(t);
        }
    }
    public void visitIntegerType(IntegerType t) throws VisitorException {
        final ExpressionInterface width = t.getDeclaredWidth();
        if (width == null) result = null;
        else width.accept(this);
        if (result != width) {
            result = new IntegerType(t.isConst(), t.isSigned(),
                                     (ExpressionInterface) result);
            result.epr(t);
            processType((IntegerType) result);
        } else {
            processType(t);
        }
    }
    public void visitBooleanType(BooleanType t) throws VisitorException {
        processType(t);
    }
    public void visitStringType(StringType t) throws VisitorException {
        processType(t);
    }
    public void visitStructureType(StructureType t) throws VisitorException {
        processType(t);
    }
    public void visitChannelType(ChannelType t) throws VisitorException {
        processType(t);
    }
    public void visitNodeType(NodeType t) throws VisitorException {
        processType(t);
    }
    public void visitChannelStructureType(ChannelStructureType t)
    throws VisitorException {
        final ChannelStructureType newt = new ChannelStructureType(t.getName());
        boolean same = true;
        for (Iterator i = t.getMembers().entrySet().iterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            ((Type) entry.getValue()).accept(this);
            newt.addMember((String) entry.getKey(), (Type) result);
            same &= result == entry.getValue();
        }
        result = same ? t : newt;
    }
    public void visitIdentifierList(IdentifierList il) throws VisitorException
    {
        throw new AssertionError();
    }

    private void processLinkageTerms(final LinkageTerms terms)
        throws VisitorException {
        boolean same = true;
        final LinkageTerms newTerms = new LinkageTerms();
        if (terms != null) {
            for (Iterator i = terms.getTerms(); i.hasNext(); ) {
                final LinkageTermInterface lti =
                    (LinkageTermInterface) i.next();
                lti.accept(this);
                // unrolling a loop may cause a LinkageTermInterface to become
                // a LinkageTerms
                if (result instanceof LinkageTerms) {
                    processLinkageTerms((LinkageTerms) result);
                    for (Iterator j = ((LinkageTerms) result).getTerms();
                         j.hasNext(); ) {
                        newTerms.addTerm((LinkageTermInterface) j.next());
                    }
                    same = false;
                } else {
                    newTerms.addTerm((LinkageTermInterface) result);
                    same &= lti == result;
                }
            }
        }
        result = same ? terms : newTerms;
    }
    public void visitLinkageLoopTerm(final LinkageLoopTerm term) throws
        VisitorException
    {
        term.getTerm().accept(this);
        final LinkageTermInterface lterm = (LinkageTermInterface) result;

        final Range range = processRange(term.getRange());
        final ExpressionInterface min = range.getMinExpression();
        final ExpressionInterface max = range.getMaxExpression();

        if (min instanceof IntegerExpression &&
            max instanceof IntegerExpression) {
            processAbstractLoop((IntegerExpression) min,
                                (IntegerExpression) max,
                                term.getIndexVar(), new Looper() {
                LinkageTerms lt = new LinkageTerms();
                public void begin() throws VisitorException { lt.epr(term); }
                public void loop() throws VisitorException {
                    lterm.accept(ConstantEvaluator.this);
                    lt.addTerm((LinkageTermInterface) result);
                }
                public void end() throws VisitorException {
                    result = lt;
                }
            });
            return;
        }
        if (term.getRange() == range && term.getTerm() == lterm) {
            result = term;
        } else {
            result = new LinkageLoopTerm(term.getIndexVar(), range, lterm);
            result.epr(term);
        }
    }
    public void visitLinkageExpressionTerm(LinkageExpressionTerm term)
        throws VisitorException
    {
        term.getExpression().accept(this);
        if (term.getExpression() == result) {
            result = term;
        } else {
            result =
                new LinkageExpressionTerm((LinkageExpressionInterface) result,
                                          term.isInverted());
            result.epr(term);
        }
    }

    public void visitLinkageArrayAccessExpression(LinkageArrayAccessExpression
                                                  e) throws VisitorException
    {
        e.getArrayExpression().accept(this);
        final LinkageExpressionInterface arrayExpr =
            (LinkageExpressionInterface) result;
        e.getIndexExpression().accept(this);

        if (e.getArrayExpression() == arrayExpr &&
            e.getIndexExpression() == result) {
            result = e;
        } else {
            result =
                new LinkageArrayAccessExpression(arrayExpr,
                                                 (ExpressionInterface) result);
            result.epr(e);
        }
    }
    public void visitLinkageIdentifierExpression(LinkageIdentifierExpression
                                                 e) throws VisitorException
    {
        result = e;
    }
    public void
        visitLinkageStructureAccessExpression(LinkageStructureAccessExpression
                                              e) throws VisitorException
    {
        e.getStructureExpression().accept(this);
        final LinkageExpressionInterface structExpr =
            (LinkageExpressionInterface) result;
        if (e.getStructureExpression() == structExpr) {
            result = e;
        } else {
            result = new LinkageStructureAccessExpression(structExpr,
                                                          e.getFieldName());
            result.epr(e);
        }
    }
}
