package com.fulcrummicro.hw.verification.lib.lang;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

public class IntegerBuffer extends ArrayList<Integer> {

    private static final long serialVersionUID = 5278116296566356265L;

    public IntegerBuffer() {
        super();
    }

    public IntegerBuffer(int initialCapacity) {
        super(initialCapacity);
    }

    public IntegerBuffer(Collection<? extends Integer> c) {
        super(c);
    }

    public IntegerBuffer(Integer ... intArray) {
        super(Arrays.asList(intArray));
    }

    public IntegerBuffer(byte[] byteArray) {
        super((byteArray.length + 3) / 4);

        for (int i = 0, value = 0; i < byteArray.length; value = 0) {
            for (int j = 0; i < byteArray.length && j < 4; i++, j++) {
                value |= (((int) byteArray[i]) & 0xFF) << (8 * j);
            }
            this.add(value);
        }
    }

    public byte[] toByteArray() {
        return this.toByteArray(0, 4 * this.size());
    }

    public byte[] toByteArray(int nBytes) {
        return this.toByteArray(0, nBytes);
    }

    public byte[] toByteArray(int fromIndex, int nBytes) {
        byte[] byteArray;

        if (fromIndex < 0 || (fromIndex + nBytes) > (4 * this.size())) {
            throw new IndexOutOfBoundsException();
        }

        byteArray = new byte[nBytes];
        for (int i = 0, j = fromIndex, k = j / 4, l = j % 4;
             i < nBytes;
             i++, j++, k = j / 4, l = j % 4) {
            byteArray[i] = (byte) ((this.get(k) >> (8 * l)) & 0xFF);
        }
        return byteArray;
    }

    public int[] toIntArray() {
        return this.toIntArray(0, this.size());
    }

    public int[] toIntArray(int nIntegers) {
        return this.toIntArray(0, nIntegers);
    }

    public int[] toIntArray(int fromIndex, int nIntegers) {
        int[] intArray;

        if (fromIndex < 0 || (fromIndex + nIntegers) > this.size()) {
            throw new IndexOutOfBoundsException();
        }

        intArray = new int[nIntegers];
        for (int i = 0, j = fromIndex; i < nIntegers; i++, j++) {
            intArray[i] = this.get(j);
        }
        return intArray;
    }

}
