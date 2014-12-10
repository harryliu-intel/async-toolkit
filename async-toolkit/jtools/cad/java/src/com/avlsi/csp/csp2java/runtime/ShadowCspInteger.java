package com.avlsi.csp.csp2java.runtime;

import java.math.BigInteger;

public class ShadowCspInteger {
    public static class Simple extends CspInteger {
        private final CspInteger formal;
        private final CspInteger actual;
        public Simple(final CspInteger actual) {
            this(new CspInteger(), actual);
        }
        public Simple(final int width, final boolean isSigned,
                      final CspInteger actual) {
            this(new FiniteCspInteger(width, isSigned), actual);
        }
        public Simple(final CspInteger formal, final CspInteger actual) {
            this.formal = formal;
            this.actual = actual;
            formal.setValue(actual.getValue());
        }
        protected void setValue(final BigInteger val) {
            formal.setValue(val);
            actual.setValue(formal);
        }
        protected BigInteger getValue() {
            return formal.getValue();
        }
    }

    public static class BitRange extends CspInteger {
        private final CspInteger min, max;
        private final CspInteger formal;
        private final CspInteger actual;
        public BitRange(final CspInteger min, final CspInteger max,
                        final CspInteger actual) {
            this(new CspInteger(), min, max, actual);
        }
        public BitRange(final int width, final boolean isSigned,
                        final CspInteger min, final CspInteger max,
                        final CspInteger actual) {
            this(new FiniteCspInteger(width, isSigned), min, max, actual);
        }
        private BitRange(final CspInteger formal, final CspInteger min,
                         final CspInteger max, final CspInteger actual) {
            this.formal = formal;
            this.actual = actual;
            this.min = min;
            this.max = max;
            formal.setValue(actual.extractBits(min, max));
        }
        protected void setValue(final BigInteger val) {
            formal.setValue(val);
            actual.assignBits(min, max, formal);
        }
        protected BigInteger getValue() {
            return formal.getValue();
        }
    }
}
