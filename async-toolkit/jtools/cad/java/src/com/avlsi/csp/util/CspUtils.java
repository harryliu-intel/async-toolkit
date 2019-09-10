package com.avlsi.csp.util;

import java.math.BigInteger;
import java.util.Optional;
import java.util.function.Function;
import java.util.Iterator;

import com.avlsi.csp.ast.*;
import com.avlsi.csp.csp2java.runtime.CspInteger;

public class CspUtils {
    public static BigInteger getIntegerConstant(final ExpressionInterface e) {
        if (e instanceof IntegerExpression) {
            final IntegerExpression ie = (IntegerExpression) e;
            return new BigInteger(ie.getValue(), ie.getRadix());
        } else {
            return null;
        }
    }

    public static Optional<Boolean> getBooleanConstant(final ExpressionInterface e) {
        return Optional.ofNullable(getIntegerConstant(e))
                       .map(v -> new CspInteger(v).booleanValue());
    }

    public static int getWidth(final Interval x, final int def) {
        if (x == null || x == Interval.EXCEPTION) return def;
        else {
            final int lwidth = x.getLeftBound().bitLength();
            final int rwidth = x.getRightBound().bitLength();
            return Math.min(def, Math.max(lwidth, rwidth) + 1);
        }

    }

    public static Type getBaseType(Type ty) {
        while (ty instanceof ArrayType) {
            ty = ((ArrayType) ty).getElementType();
        }
        return ty;
    }

    public static ArrayType setBaseType(ArrayType aty, Type baseTy) {
        if (aty.getElementType() instanceof ArrayType) {
            ArrayType elTy = (ArrayType) aty.getElementType();
            return new ArrayType(aty.getRange(), setBaseType(elTy, baseTy));
        } else {
            return new ArrayType(aty.getRange(), baseTy);
        }
    }

    /**
     * Returns true if the argument is a packed structure.  A packed
     * structure is a structure that has a finite, declared width.  It
     * cannot contain strings.
     **/
    public static boolean isPacked(
            final Function<StructureType,StructureDeclaration> structDecl,
            final Type t) throws VisitorException {
        if (t instanceof BooleanType) {
            return true;
        } else if (t instanceof IntegerType) {
            final IntegerType it = (IntegerType) t;
            return it.getDeclaredWidth() != null;
        } else if (t instanceof ArrayType) {
            return isPacked(structDecl, ((ArrayType) t).getElementType());
        } else if (t instanceof StructureType) {
            final StructureDeclaration decl =
                structDecl.apply((StructureType) t);
            if (decl == null) {
                return false;
            } else {
                return isPacked(structDecl, decl);
            }
        } else {
            return false;
        }
    }

    private static boolean isPacked(
            final Function<StructureType,StructureDeclaration> structDecl,
            final StructureDeclaration decl)
        throws VisitorException {
        final boolean[] result = new boolean[] { true };
        (new DeclarationProcessor() {
            public void process(final Declarator d)
                throws VisitorException {
                final Type t = d.getTypeFragment();
                result[0] &= isPacked(structDecl, t);
            }
        }).process(decl.getDeclarations());
        return result[0];
    }

    public static int getPackSize(
            final Function<StructureType,StructureDeclaration> structDecl,
            final Type t) throws VisitorException {
        int s = -1;
        if (t instanceof BooleanType) {
            s = 1;
        } else if (t instanceof IntegerType) {
            final IntegerType it = (IntegerType) t;
            final BigInteger w =
                CspUtils.getIntegerConstant(it.getDeclaredWidth());
            s = w == null ? -1 : w.intValue();
        } else if (t instanceof ArrayType) {
            final ArrayType at = (ArrayType) t;
            final Range r = at.getRange();
            final BigInteger min =
                CspUtils.getIntegerConstant(r.getMinExpression());
            final BigInteger max =
                CspUtils.getIntegerConstant(r.getMaxExpression());
            if (min != null && max != null) {
                final int w = getPackSize(structDecl, at.getElementType());
                if (w != -1) {
                    BigInteger num = max.subtract(min).add(BigInteger.ONE);
                    s = num.intValue() * w;
                }
            }
        } else if (t instanceof StructureType) {
            final StructureDeclaration decl =
                structDecl.apply((StructureType) t);
            if (decl != null) {
                s = getPackSize(structDecl, decl);
            }
        }
        return s;
    }

    private static int getPackSize(
            final Function<StructureType,StructureDeclaration> structDecl,
            final StructureDeclaration decl)
        throws VisitorException {
        final int[] result = new int[] { 0 };
        (new DeclarationProcessor() {
            public void process(final Declarator d)
                throws VisitorException {
                if (result[0] != -1) {
                    final int w =
                        getPackSize(structDecl, d.getTypeFragment());
                    if (w == -1) result[0] = -1;
                    else result[0] += w;
                }
            }
        }).process(decl.getDeclarations());
        return result[0];
    }

    public static StatementInterface
    getInfiniteLoopBody(final StatementInterface si) {
        if (si instanceof DeterministicRepetitionStatement) {
            return getInfiniteLoopBody((DeterministicRepetitionStatement) si);
        } else {
            return null;
        }
    }

    private static StatementInterface
    getInfiniteLoopBody(final DeterministicRepetitionStatement si) {
        StatementInterface result = null;
        if (si.getElseStatement() != null) return null;
        for (Iterator<GuardedCommandInterface> i = si.getGuardedCommands();
             i.hasNext(); ) {
            final GuardedCommandInterface gci = i.next();
            if (!(gci instanceof GuardedCommand)) return null;
            final GuardedCommand gc = (GuardedCommand) gci;
            if (!(gc.getGuard() instanceof IntegerExpression)) return null;
            final IntegerExpression ie = (IntegerExpression) gc.getGuard();
            if (!ie.getValue().equals("-1")) return null;
            if (result == null) result = gc.getCommand();
            else return null;
        }
        return result;
    }

}
