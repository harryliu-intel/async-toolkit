/**
 * Copyright (C) 2008-2011 Fulcrum Microsystems, Inc.  All rights reserved.
 * Unauthorized disclosure prohibited.
 */

package com.fulcrummicro.hw.verification.tests.lib.bitarray;

import static java.lang.System.out;

import java.math.BigInteger;
import java.security.InvalidParameterException;
import java.util.Random;

import com.fulcrummicro.hw.verification.lib.bitarray.LittleEndianBitArray;

/**
 * @author mhesseli
 */
public class BitArrayTest {

    /**************************************************************************
     * Constants & Types
     **************************************************************************/

    private static final int L_SIZE = Long.SIZE;

    private static final int I_SIZE = Integer.SIZE;

    private static final int S_SIZE = Short.SIZE;

    private static final int B_SIZE = Byte.SIZE;

    private static class ExtendedClass extends LittleEndianBitArray {

        public ExtendedClass(int size) {
            super(size);
        }

        @Override
        public ExtendedClass and(LittleEndianBitArray array) {
            return (ExtendedClass) super.and(array);
        }

        @Override
        public ExtendedClass invert() {
            return (ExtendedClass) super.invert();
        }

        @Override
        public ExtendedClass invert(int fromIndex, int nBits) {
            return (ExtendedClass) super.invert(fromIndex, nBits);
        }

        @Override
        public ExtendedClass or(LittleEndianBitArray array) {
            return (ExtendedClass) super.or(array);
        }

        @Override
        public ExtendedClass shiftLeft(int nBits) {
            return (ExtendedClass) super.shiftLeft(nBits);
        }

        @Override
        public ExtendedClass shiftRight(int nBits) {
            return (ExtendedClass) super.shiftRight(nBits);
        }

        @Override
        public ExtendedClass xor(LittleEndianBitArray array) {
            return (ExtendedClass) super.xor(array);
        }
    }

    /**************************************************************************
     * Local Functions
     **************************************************************************/

    /**
     * Tests the logical <b>AND</b>, logical <b>NOT</b>, logical <b>OR</b>
     * and logical <b>XOR</b> operations for both big and little-endian bit
     * arrays.
     */
    private static void testBitwiseOperation() {
/*
        BigEndianBitArray beA = new BigEndianBitArray(L_SIZE);
        BigEndianBitArray beB = new BigEndianBitArray(I_SIZE);
*/
        LittleEndianBitArray leA = new LittleEndianBitArray(I_SIZE);
        LittleEndianBitArray leB = new LittleEndianBitArray(S_SIZE);
        Random r = new Random();
/*
        long lValue = r.nextLong();
*/
        // To prevent the upper 32-bits of iValue from being set after iValue
        // has been converted to a long, only allow random 31-bit unsigned
        // integers to be generated.
        int iValue = r.nextInt(0x7FFFFFFF);
        // To prevent the upper 16-bits of sValue from being set after sValue
        // has been converted to an int, only allow random 15-bit unsigned
        // integers to be generated.
        short sValue = (short) r.nextInt(0x7FFF);

/*
        // Perform a set of big-endian bitwise operations.
        beA.set(lValue);
        beB.set(iValue);
        beA = beA.and(beB);
        assert beA.getUnsignedLong(0) == (lValue & iValue) : beA.getUnsignedLong(0);

        beA.set(lValue);
        beA = beA.invert();
        assert beA.getUnsignedLong(0) == ~lValue : beA.getUnsignedLong(0);

        beA.set(lValue);
        beB.set(iValue);
        beA = beA.or(beB);
        assert beA.getUnsignedLong(0) == (lValue | iValue) : beA.getUnsignedLong(0);

        beA.set(lValue);
        beB.set(iValue);
        beA = beA.xor(beB);
        assert beA.getUnsignedLong(0) == (lValue ^ iValue) : beA.getUnsignedLong(0);
*/

        // Perform a set of little-endian bitwise operations.
        leA.set(iValue);
        leB.set(sValue);
        leA = leA.and(leB);
        assert leA.getUnsignedInt(0) == (iValue & sValue) : leA.getUnsignedInt(0);

        leA.set(iValue);
        leA = leA.invert();
        assert leA.getUnsignedInt(0) == ~iValue : leA.getUnsignedInt(0);

        leA.set(iValue);
        leB.set(sValue);
        leA = leA.or(leB);
        assert leA.getUnsignedInt(0) == (iValue | sValue) : leA.getUnsignedInt(0);

        leA.set(iValue);
        leB.set(sValue);
        leA = leA.xor(leB);
        assert leA.getUnsignedInt(0) == (iValue ^ sValue) : leA.getUnsignedInt(0);
    }

    /**
     * Tests the clone method for both big and little-endian bit arrays.
     */
    private static void testClone() {
/*
        BigEndianBitArray beA = new BigEndianBitArray(I_SIZE);
        BigEndianBitArray beB;
*/
        LittleEndianBitArray leA = new LittleEndianBitArray(I_SIZE);
        LittleEndianBitArray leB;
        Random r = new Random();
        int bit = r.nextInt(32);

/*
        // Perform a big-endian bit array clone test.
        beA.setBit(bit);
        beB = beA.clone();
        beB.clearBit(bit);
        assert beA.getUnsignedInt(0) == 1 << bit : beA.getUnsignedInt(0);
        assert beB.getUnsignedInt(0) == 0 : beB.getUnsignedInt(0);
*/

        // Perform a little-endian bit array clone test.
        leA.setBit(bit);
        leB = leA.clone();
        leB.clearBit(bit);
        assert leA.getUnsignedInt(0) == 1 << bit : leA.getUnsignedInt(0);
        assert leB.getUnsignedInt(0) == 0 : leB.getUnsignedInt(0);
    }

    /**
     * Tests bit array equality functionality for both big and little-endian bit
     * arrays.
     */
    private static void testEquality() {
/*
        BigEndianBitArray beA = new BigEndianBitArray(I_SIZE);
        BigEndianBitArray beB;
        BigEndianBitArray beC = new BigEndianBitArray(I_SIZE);
        BigEndianBitArray beD = new BigEndianBitArray(L_SIZE);
 */
        ExtendedClass eA = new ExtendedClass(I_SIZE);
        LittleEndianBitArray leA = new LittleEndianBitArray(I_SIZE);
        LittleEndianBitArray leB;
        LittleEndianBitArray leC = new LittleEndianBitArray(I_SIZE);
        LittleEndianBitArray leD = new LittleEndianBitArray(L_SIZE);
        Random r = new Random();

/*
        // Perform a set of big-endian equality tests.
        beA.set(r.nextInt());
        beB = beA.clone();
        beC.set(r.nextInt());
        beD.set(r.nextLong());
        assert beA.equals(null) == false;
        assert beA.equals(beA);
        assert beA.equals(beB);
        assert beA.equals(beC) == false;
        assert beA.equals(beD) == false;
        assert beA.equals(leA) == false;
*/

        // Perform a set of little-endian equality tests.
        leA.set(r.nextInt());
        leB = leA.clone();
        leC.set(r.nextInt());
        leD.set(r.nextLong());
        assert leA.equals(null) == false;
        assert leA.equals(leA);
        assert leA.equals(leB);
        assert leA.equals(leC) == false;
        assert leA.equals(leD) == false;
/*
        assert leA.equals(beA) == false;
*/

        // Perform a set of class extension equality tests.
        leA.set(r.nextInt());
        eA.set(leA.getUnsignedInt(0));
        assert leA.equals(eA) == false;

    }

    private static void testGet() {
        LittleEndianBitArray x = new LittleEndianBitArray(I_SIZE);
        Random r = new Random();
        int e;
        byte[] y = new byte[I_SIZE / B_SIZE];

        r.nextBytes(y);
        for (int i = 0; i < y.length; i++)
            x.set(y[i], i * B_SIZE);
        for (int i = 0; i < y.length; i++)
            assert (byte)(x.getUnsignedInt(i * B_SIZE, B_SIZE) & 0xFF) == y[i];

        e = r.nextInt() & 0x7FFFFFFF;
        x.set(e);
        assert x.getBigInteger(0).toString().equals(Integer.toString(e));
        x.set(0xFFFFFFFF);
        assert x.getBigInteger(0).toString().equals("4294967295");

        x = new LittleEndianBitArray(L_SIZE);
        x.set(0xFFFFFFFFFFFFFFFFL);
        assert x.getBigInteger(0).toString().equals("18446744073709551615");
    }

    /**
     * Tests the generation of exceptions as a result of invalid parameters for
     * both big and little-endian bit arrays.
     */
    private static void testInvalidParameter() {
/*
        BigEndianBitArray x = new BigEndianBitArray(I_SIZE);
*/
        LittleEndianBitArray y = new LittleEndianBitArray(I_SIZE);
        Random r = new Random();
        boolean exception = false;

/*
        // Perform a set of big-endian invalid parameter tests.
        try {
            exception = false;
            x.setBit(L_SIZE);
        }
        catch (InvalidParameterException e) {
            exception = true;
        }
        finally {
            assert exception;
        }

        try {
            exception = false;
            x.set(r.nextInt(), L_SIZE);
        }
        catch (InvalidParameterException e) {
            exception = true;
        }
        finally {
            assert exception;
        }

        try {
            exception = false;
            x.and(new BigEndianBitArray(L_SIZE));
        }
        catch (InvalidParameterException e) {
            exception = true;
        }
        finally {
            assert exception;
        }

        try {
            exception = false;
            x.copy(new BigEndianBitArray(S_SIZE), 0, 0, I_SIZE);
        }
        catch (InvalidParameterException e) {
            exception = true;
        }
        finally {
            assert exception;
        }
*/

        // Perform a set of little-endian invalid parameter tests.
        try {
            exception = false;
            y.setBit(L_SIZE);
        }
        catch (InvalidParameterException e) {
            exception = true;
        }
        finally {
            assert exception;
        }

        try {
            exception = false;
            y.set(r.nextInt(), L_SIZE);
        }
        catch (InvalidParameterException e) {
            exception = true;
        }
        finally {
            assert exception;
        }

        try {
            exception = false;
            y.and(new LittleEndianBitArray(L_SIZE));
        }
        catch (InvalidParameterException e) {
            exception = true;
        }
        finally {
            assert exception;
        }

/*
        try {
            exception = false;
            y.copy(new BigEndianBitArray(S_SIZE), 0, 0, I_SIZE);
        }
        catch (InvalidParameterException e) {
            exception = true;
        }
        finally {
            assert exception;
        }
*/
    }

    private static void testSet() {
        BigInteger x;
        LittleEndianBitArray y;
        Random r = new Random();
        int size = 33;

        x = new BigInteger(size - 1, r);
        y = new LittleEndianBitArray(size);
        y.set(x);
        assert y.toHexString().equalsIgnoreCase(x.toString(16));

        x = new BigInteger("-1");
        y = new LittleEndianBitArray(size);
        y.set(x);
        assert y.getSignedLong(0, size) == -1L;
    }

    /**
     * Tests sign extension for both big and little-endian bit arrays.
     */
    private static void testSignExtension() {
/*
        BigEndianBitArray x = new BigEndianBitArray(L_SIZE);
*/
        LittleEndianBitArray y = new LittleEndianBitArray(L_SIZE);
        int iValue = -1;
        short sValue = -1;
        byte bValue = -1;

/*
        // Perform a set of big-endian bit array sign extension tests.
        x.set(iValue);
        assert x.getSignedLong(0) == -1L : x.getSignedLong(0);
        x.clearBit(0, x.size());
        x.set(sValue);
        assert x.getSignedLong(0) == -1L : x.getSignedLong(0);
        assert x.getSignedInt(0) == -1 : x.getSignedInt(0);
        x.clearBit(0, x.size());
        x.set(bValue);
        assert x.getSignedLong(0) == -1L : x.getSignedLong(0);
        assert x.getSignedInt(0) == -1 : x.getSignedInt(0);
        assert x.getSignedShort(0) == (short) -1 : x.getSignedShort(0);
        x.clearBit(0, x.size());
*/

        // Perform a set of little-endian bit array sign extension tests.
        y.set(iValue);
        assert y.getSignedLong(0) == -1L : y.getSignedLong(0);
        y.clearBit(0, y.size());
        y.set(sValue);
        assert y.getSignedLong(0) == -1L : y.getSignedLong(0);
        assert y.getSignedInt(0) == -1 : y.getSignedInt(0);
        y.clearBit(0, y.size());
        y.set(bValue);
        assert y.getSignedLong(0) == -1L : y.getSignedLong(0);
        assert y.getSignedInt(0) == -1 : y.getSignedInt(0);
        assert y.getSignedShort(0) == (short) -1 : y.getSignedShort(0);
        y.clearBit(0, y.size());
    }

    /**
     * Tests conversion to string representation for both big and little-endian
     * bit arrays.
     */
    private static void testStringRepresentation() {
/*
        BigEndianBitArray x = new BigEndianBitArray(L_SIZE);
*/
        LittleEndianBitArray y = new LittleEndianBitArray(L_SIZE);
        Random r = new Random();
        long value = r.nextLong();

/*
        // Perform a set of big-endian string representation tests.
        x.set(value);
        assert x.toBinaryString().equals(Long.toBinaryString(value)) : x.toBinaryString();
        assert x.toHexString().equals(Long.toHexString(value)) : x.toHexString();
*/

        // Perform a set of little-endian string representation tests.
        y.set(value);
        assert y.toBinaryString().equals(Long.toBinaryString(value)) : y.toBinaryString();
        assert y.toHexString().equals(Long.toHexString(value)) : y.toHexString();
        y.clear();
        y.setBit(15);
        assert y.toBinaryString().equals("1000000000000000") : y.toBinaryString();
        assert y.toBinaryString("_").equals("10000000_00000000") : y.toBinaryString("_");
        assert y.toHexString("_", 16).equals("00000000_00008000") : y.toHexString("_", 16);
        y.clear();
        assert y.toHexString("_", 16).equals("00000000_00000000") : y.toHexString("_", 16);

        // Bugzilla #12462: "LittleEndianBitArray toHexString returns incorrect
        // value".
        // comment #0
        y = new LittleEndianBitArray(112);
        y.setBit(64, 2);
        assert y.toHexString().equals("30000000000000000") : y.toHexString();
        assert y.toHexString("_").equals("3_00000000_00000000") : y.toHexString("_");
        assert y.toHexString("_", 24).equals("00000003_00000000_00000000") : y.toHexString("_", 24);
        // comment #2
        y = new LittleEndianBitArray(128);
        y.setBit(48, 16);
        y.setBit(112, 16);
        assert y.toHexString().equals("ffff000000000000ffff000000000000") : y.toHexString();
        assert y.toHexString("_").equals("ffff0000_00000000_ffff0000_00000000") : y.toHexString("_");
        assert y.toHexString("_", 33).equals("0_ffff0000_00000000_ffff0000_00000000") : y.toHexString("_", 33);

        // Bugzilla #19376: "LittleEndianBitArray toHexString doesn't return
        // anything"
        y.clear();
        assert y.toHexString().equals("0") : y.toHexString();
        assert y.toHexString("_").equals("0") : y.toHexString("_");

        // Bugzilla #20025: "L2L_EVID1_TABLE_RW_MASK isn't correct"
        y = new LittleEndianBitArray(33);
        // comment #2
        y.setBit(0, y.size());
        assert y.toBinaryString().equals("111111111111111111111111111111111") : y.toBinaryString();
        assert y.toBinaryString("_").equals("1_11111111_11111111_11111111_11111111") : y.toBinaryString("_");
        assert y.toHexString().equals("1ffffffff") : y.toHexString();
        assert y.toHexString("_").equals("1_ffffffff") : y.toHexString("_");
        // extra
        y.clear();
        y.setBit(32);
        assert y.toBinaryString().equals("100000000000000000000000000000000") : y.toBinaryString();
        assert y.toBinaryString("_").equals("1_00000000_00000000_00000000_00000000") : y.toBinaryString("_");
        assert y.toHexString().equals("100000000") : y.toHexString();
        assert y.toHexString("_").equals("1_00000000") : y.toHexString("_");
        y = new LittleEndianBitArray(65);
        y.setBit(0);
        y.setBit(64);
        assert y.toHexString().equals("10000000000000001") : y.toHexString();
        assert y.toHexString("_").equals("1_00000000_00000001") : y.toHexString("_");
    }

/*
    private static void testEndianness() {
        BigEndianBitArray big = new BigEndianBitArray(L_SIZE);
        LittleEndianBitArray little = new LittleEndianBitArray(big.size());

        Random r = new Random();
        long value = r.nextLong();
        value = 0;

        big.set(value);
        little.set(value);

        System.out.println(big.toHexString());

        big.set(2,7,2);
        little.set(2,7,2);

        System.out.println(big.toHexString());
        assert big.toHexString().equals(little.toHexString());

        little.copy(big, 0, 0, L_SIZE);
        System.out.println(big.toHexString());
    }
*/

    /**************************************************************************
     * Public Functions
     **************************************************************************/

    public static void main(String[] args) {
        int i;
        int n = 1;

        for (i = 0; i < n; i++) {
            testGet();

            testSet();

            testBitwiseOperation();

            testClone();

            testEquality();

            testInvalidParameter();

            testSignExtension();

            testStringRepresentation();

/*
            testEndianness();
*/
        }

        out.println("PASS");
        System.exit(0);
    }

}
