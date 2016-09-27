package com.avlsi.csp.csp2java.runtime;

import java.util.function.Function;

public class RuntimeUtils {
    static class ArrayDuplicator implements Function<CspCloneableValue,CspValue> {
        final Function<CspCloneableValue,CspValue> func;
        public ArrayDuplicator(final Function<CspCloneableValue,CspValue> func) {
            this.func = func;
        }
        public CspValue apply(final CspCloneableValue t) {
            return t instanceof CspArray ? ((CspArray) t).duplicate(this)
                                         : func.apply(t);
        }
    }

    /** Helper function for passing an input integer */
    public static CspInteger copyInt(CspInteger val) {
        return new CspInteger(val);
    }

    /** Helper function for passing an input integer */
    public static CspInteger copyInt(int width, boolean isSigned, CspInteger val) {
        return new FiniteCspInteger(width, isSigned, val);
    }

    /** Helper function for passing an input integer array */
    public static CspArray copyIntArray(final CspArray val) {
        return (CspArray) val.duplicate(
                new ArrayDuplicator(v -> copyInt((CspInteger) v)));
    }

    /** Helper function for passing an input integer array */
    public static CspArray copyIntArray(final int width,
                                        final boolean isSigned,
                                        final CspArray val) {
        return (CspArray) val.duplicate(
                new ArrayDuplicator(
                    v -> copyInt(width, isSigned, (CspInteger) v)));
    }

    /** Helper function for passing an output integer */
    public static CspInteger shadowInt(CspInteger val) {
        return new ShadowCspInteger.Simple(val);
    }

    /** Helper function for passing an output integer */
    public static CspInteger shadowInt(int width, boolean isSigned, CspInteger val) {
        return new ShadowCspInteger.Simple(width, isSigned, val);
    }

    /** Helper function for passing an output integer array */
    public static CspArray shadowIntArray(final CspArray val) {
        return (CspArray) val.duplicate(
                new ArrayDuplicator(v -> shadowInt((CspInteger) v)));
    }

    /** Helper function for passing an output integer array */
    public static CspArray shadowIntArray(final int width,
                                          final boolean isSigned,
                                          final CspArray val) {
        return (CspArray) val.duplicate(
                new ArrayDuplicator(
                    v -> shadowInt(width, isSigned, (CspInteger) v)));
    }
}
