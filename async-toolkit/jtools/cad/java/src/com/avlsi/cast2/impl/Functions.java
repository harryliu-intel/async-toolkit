package com.avlsi.cast2.impl;

import java.math.BigInteger;
import java.util.Iterator;
import java.util.Map;

import com.avlsi.cast.impl.ArrayValue;
import com.avlsi.cast.impl.BoolValue;
import com.avlsi.cast.impl.FloatValue;
import com.avlsi.cast.impl.IntValue;
import com.avlsi.cast.impl.InvalidOperationException;
import com.avlsi.cast.impl.TupleValue;
import com.avlsi.cast.impl.Type;
import com.avlsi.cast.impl.Value;
import com.avlsi.util.math.BigIntegerUtil;
import com.avlsi.util.container.CollectionUtils;

public class Functions {
    private interface Function {
        Value invoke(final TupleValue args) throws InvalidOperationException;
    }

    private static void checkType(final Value v, final Type t, final int n)
        throws InvalidOperationException {
        if (v.getType() != t) {
            throw new InvalidOperationException("Expecting argument " + n + " to have type " + t.getString() + "; found type " + v.getType().getString());
        }
    }

    private static void checkLength(final TupleValue v, final int size)
        throws InvalidOperationException {
        if (v.getSize() != size) {
            throw new InvalidOperationException("Expecting " + size + " arguments; found " + v.getSize() + " arguments");
        }
    }

    private static class MinMax implements Function {
        private boolean min;
        public MinMax(boolean min) {
            this.min = min;
        }
        private Value op(final Value v1, final Value v2)
            throws InvalidOperationException {
            return ((BoolValue) (min ? v1.lt(v2) : v1.gt(v2))).getValue() ? v1
                                                                          : v2;
        }
        private Value invoke(final Iterator i, final String type)
            throws InvalidOperationException {
            Value result = null;
            int k = 1;
            while (i.hasNext()) {
                final Value v = (Value) i.next();
                if (v instanceof IntValue) {
                    result = result == null ? v : op(result, v);
                } else if (v instanceof FloatValue) {
                    result = result == null ? v 
                                            : FloatValue.valueOf(op(v, result));
                } else {
                    throw new InvalidOperationException("Expecting " + type + " " + k + " to be int or float; found " + v.getType().getString());
                }
                ++k;
            }
            return result;
        }
        public Value invoke(final TupleValue args)
            throws InvalidOperationException {
            final int size = args.getSize();
            if (size == 0) {
                throw new InvalidOperationException("At least one argument expected; found none");
            } else if (size == 1) {
                final Value v = args.accessTuple(0);
                if (v instanceof ArrayValue) {
                    return invoke(((ArrayValue) v).getIterator(),
                                  "array element");
                } else if (v instanceof IntValue || v instanceof FloatValue) {
                    return v;
                } else {
                    throw new InvalidOperationException("Expecting int or float; found " + v.getType().getString());
                }
            } else {
                return invoke(args.getIterator(), "argument");
            }
        }
    }

    private static Map table = CollectionUtils.mapify(new Object[] {
        "ceil", new Function() {
            public Value invoke(final TupleValue args)
                throws InvalidOperationException {
                checkLength(args, 1);
                final Value v = args.accessTuple(0);
                final Value result;
                if (v instanceof IntValue) {
                    result = v;
                } else if (v instanceof FloatValue) {
                    result = IntValue.valueOf(
                        String.format("%.0f",
                                      Math.ceil(((FloatValue) v).getValue())));
                } else {
                    throw new InvalidOperationException("Expecting int or float; found " + v.getType().getString());
                }
                return result;
            }
        },
        "log2", new Function() {
            public Value invoke(final TupleValue args)
                throws InvalidOperationException {
                checkLength(args, 1);
                final Value v = args.accessTuple(0);
                checkType(v, IntValue.TYPE, 1);
                return IntValue.valueOf(BigIntegerUtil.log2(((IntValue) v).getValue()));
            }
        },
        "log4", new Function() {
            public Value invoke(final TupleValue args)
                throws InvalidOperationException {
                checkLength(args, 1);
                final Value v = args.accessTuple(0);
                checkType(v, IntValue.TYPE, 1);
                return IntValue.valueOf(BigIntegerUtil.log4(((IntValue) v).getValue()));
            }
        },
        "log", new Function() {
            public Value invoke(final TupleValue args)
                throws InvalidOperationException {
                checkLength(args, 2);
                final Value base = args.accessTuple(0);
                checkType(base, IntValue.TYPE, 1);
                final Value v = args.accessTuple(1);
                checkType(v, IntValue.TYPE, 2);
                return IntValue.valueOf(BigIntegerUtil.log(((IntValue) base).getValue(), ((IntValue) v).getValue()));
            }
        },
        "min", new MinMax(true),
        "max", new MinMax(false),
        "exp", new Function() {
            public Value invoke(final TupleValue args)
                throws InvalidOperationException {
                checkLength(args, 1);
                final Value v = args.accessTuple(0);
                checkType(v, FloatValue.TYPE, 1);
                return FloatValue.valueOf(Math.exp(((FloatValue) v).getValue()));
            }
        },
        "fixed_point", new Function() {
            public Value invoke(final TupleValue args)
                throws InvalidOperationException {
                checkLength(args, 2);
                final Value x = args.accessTuple(0);
                checkType(x, FloatValue.TYPE, 1);
                final Value xpt = args.accessTuple(1);
                checkType(xpt, IntValue.TYPE, 2);

                int ipt = 0;
                try {
                    ipt = IntValue.valueOf(xpt).getValue().intValueExact();
                } catch (ArithmeticException e) {
                    throw new InvalidOperationException(
                            "Fixed point precision too large: " + xpt);
                }

                final double xscaled =
                    Math.scalb(FloatValue.valueOf(x).getValue(), ipt);
                if (Double.isInfinite(xscaled) || Double.isNaN(xscaled)) {
                    throw new InvalidOperationException(
                        "Fixed point representation too large for double: " +
                        xscaled);
                }

                final double xint = Math.floor(xscaled);
                if (xint > Long.MAX_VALUE || xint < Long.MIN_VALUE) {
                    throw new InvalidOperationException(
                        "Fixed point representation too large for long: " +
                        xint);
                }

                long result = Math.round(xint);
                if (xscaled - xint >= 0.5) {
                    result++;
                }

                return IntValue.valueOf(Long.toString(result));
            }
        }
    });

    public static Value invokeFunction(final String name, final TupleValue args)
        throws InvalidOperationException {
        final Function func = (Function) table.get(name);
        if (func == null) {
            throw new InvalidOperationException("Invalid function: " + name);
        } else {
            return func.invoke(args);
        }
    }
}
