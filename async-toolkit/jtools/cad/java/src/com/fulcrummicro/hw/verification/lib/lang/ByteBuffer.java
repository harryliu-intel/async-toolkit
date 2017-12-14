/**
 * INTEL TOP SECRET
 * Copyright 2004 - 2014 Intel Corporation
 * All Rights Reserved.
 */

package com.fulcrummicro.hw.verification.lib.lang;

import java.util.ArrayList;
import java.util.Collection;

/**
 * @author dandaly
 * @author mhesseli
 */
public class ByteBuffer extends ArrayList<Byte> implements Comparable<ByteBuffer> {

    private static final long serialVersionUID = -9026357512561982056L;

    public ByteBuffer() {
        super();
    }

    public ByteBuffer(int initialCapacity) {
        super(initialCapacity);
    }

    public ByteBuffer(Collection<Byte> c) {
        super(c);
    }

    public ByteBuffer(byte[] byteArray) {
        super(byteArray.length);
        this.addAll(byteArray);
    }

    public void addAll(byte[] byteArray) {
        this.ensureCapacity(this.size() + byteArray.length);
        for (int i = 0; i < byteArray.length; i++) {
            this.add(byteArray[i]);
        }
    }

    public int compareTo(ByteBuffer o2) {
        Byte b1;
        Byte b2;
        int size1;
        int size2;

        if (o2 == null) {
            return 1;
        }
        size1 = this.size();
        size2 = o2.size();
        if (size1 != size2) {
            return size1 < size2 ? -1 : 1;
        }
        for (int i = 0; i < size1; i++) {
            b1 = this.get(i);
            b2 = o2.get(i);
            if (!b1.equals(b2)) {
                return b1 < b2 ? -1 : 1;
            }
        }
        return 0;
    }

    @Override
    public String toString() {
        return this.toString(this.size());
    }

    public String toString(int nBytes) {
        StringBuilder buffer = new StringBuilder();
        int size = this.size();

        for (int i = 0, j = 1; i < size && i < nBytes; i++, j++) {
            buffer.append(String.format("%02x", this.get(i)));
            buffer.append((j % 16) == 0 ? "\n" : " ");
        }
        if (size > nBytes) {
            buffer.append("...");
        }
        return buffer.toString().trim();
    }

    public byte[] toByteArray() {
        return this.toByteArray(0, this.size());
    }

    public byte[] toByteArray(int nBytes) {
        return this.toByteArray(0, nBytes);
    }

    public byte[] toByteArray(int fromIndex, int nBytes) {
        byte[] byteArray;

        if (fromIndex < 0 || (fromIndex + nBytes) > this.size()) {
            throw new IndexOutOfBoundsException();
        }

        byteArray = new byte[nBytes];
        for (int i = 0, j = fromIndex; i < nBytes; i++, j++) {
            byteArray[i] = this.get(j);
        }
        return byteArray;
    }

    public void trim(int newLength) {
        int size = this.size();

        if (newLength < size) {
            this.subList(newLength, size).clear();
        }
    }

}
