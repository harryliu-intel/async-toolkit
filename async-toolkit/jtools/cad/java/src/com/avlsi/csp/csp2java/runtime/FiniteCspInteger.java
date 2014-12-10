package com.avlsi.csp.csp2java.runtime;

import java.math.BigInteger;

import com.avlsi.tools.sigscan.Sigscan;
import com.avlsi.util.math.BigIntegerUtil;

public class FiniteCspInteger extends CspInteger implements Packable {
    private final int width;
    private final boolean isSigned;
    public FiniteCspInteger(int width, boolean isSigned) {
        this(BigInteger.ZERO, width, isSigned);
    }
    public FiniteCspInteger(BigInteger val, int width, boolean isSigned) {
        super(BigIntegerUtil.truncate(val, width, isSigned));
        this.width = width;
        this.isSigned = isSigned;
    }
    public FiniteCspInteger(int width, boolean isSigned, CspInteger val) {
        this(val.toBigInteger(), width, isSigned);
    }
    public int pack(CspInteger packed, int start) {
        packed.assignBits(start, start + width - 1,
            new CspInteger(BigIntegerUtil.truncate(toBigInteger(), width)));
        return start + width;
    }
    public int unpack(CspInteger packed, int start) {
        setValue(packed.extractBits(start, start + width - 1));
        return start + width;
    }
    public CspCloneableValue duplicate() {
        return new FiniteCspInteger(getValue(), width, isSigned);
    }
    protected void setValue(final BigInteger val) {
        super.setValue(BigIntegerUtil.truncate(val, width, isSigned));
    }
    public byte[] getLogicArray() {
        final byte[] result = new byte[width];
        final BigInteger val = getValue();
        for (int i = 0, j = width - 1; i < width; ++i, --j) {
            result[j] = (byte) (val.testBit(i) ? Sigscan.LOGIC1
                                               : Sigscan.LOGIC0);
        }
        return result;
    }
}
