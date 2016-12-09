package com.avlsi.csp.util;

import java.math.BigInteger;
import java.util.Optional;

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
}
