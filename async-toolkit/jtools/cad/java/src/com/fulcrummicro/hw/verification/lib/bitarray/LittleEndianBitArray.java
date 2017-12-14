package com.fulcrummicro.hw.verification.lib.bitarray;

import java.util.ArrayList;
import java.math.BigInteger;

import com.fulcrummicro.hw.verification.util.gen.RandomNumberGenerator;

/**
 * This class implements a little-endian bit array.
 *
 * @author mhesseli
 *
 */
public class LittleEndianBitArray extends BitArray<LittleEndianBitArray> implements Cloneable {

    /**************************************************************************
     * Protected Functions
     **************************************************************************/

    protected LittleEndianBitArray Shrink(int fromIndex, int toIndex) {
        LittleEndianBitArray bitArray;
        int nBits;

        nBits = toIndex - fromIndex;
        bitArray = new LittleEndianBitArray(nBits);
        bitArray.copy(this, fromIndex, 0, nBits);
        return bitArray;
    }

    /**************************************************************************
     * Public Functions
     **************************************************************************/

    public LittleEndianBitArray(int size) {
        super(size);
    }
    
    /**
     * Create a LittleEndianBitArray of specified size and value.
     * 
     * @param size      The size of the LittleEndianBitArray
     * @param value     The value to set the LittleEndianBitArray to
     */
    public LittleEndianBitArray(int size, String value) {
        super(size);
        this.set(value);
    }

    /**
     * Create a LittleEndianBitArray of specified size and value.
     * 
     * @param size      The size of the LittleEndianBitArray
     * @param value     The value to set the LittleEndianBitArray to
     */
    public LittleEndianBitArray(int size, int value) {
        super(size);
        this.set(value);
    }
    
    /**
     * Create a LittleEndianBitArray of specified size and value.
     * 
     * @param size      The size of the LittleEndianBitArray
     * @param value     The value to set the LittleEndianBitArray to
     */
    public LittleEndianBitArray(int size, long value) {
        super(size);
        this.set(value);
    }
    
    /**
     * Create a LittleEndianBitArray of specified size and value.
     * 
     * @param size      The size of the LittleEndianBitArray
     * @param value     The value to set the LittleEndianBitArray to
     */
    public LittleEndianBitArray(int size, LittleEndianBitArray array) {
        super(size);
        this.set(array);
    }
    
    /**
     * Create a LittleEndianBitArray of specified size and value.
     * 
     * @param size      The size of the LittleEndianBitArray
     * @param value     The value to set the LittleEndianBitArray to
     */
    public LittleEndianBitArray(int size, BigInteger array) {
        super(size);
        this.set(array);
    }
        
    
    public LittleEndianBitArray clear() {
        this.Clear();
        return this;
    }

    public LittleEndianBitArray clone() {
        LittleEndianBitArray bitArray;

        bitArray = new LittleEndianBitArray(this.size());
        bitArray.copy(this, 0, 0, this.size());
        return bitArray;
    }


    /**
     * @return the BitArray as an ArrayList of Bytes in big endian order
     */
    public ArrayList<Byte> getBigEndianUnsignedByteArrayList() {
        ArrayList<Byte> bytes = new ArrayList<Byte>();
        int byteNum = 0;
        while(this.size() > byteNum * 8) {
            bytes.add(0, getUnsignedByte(byteNum * 8));
            byteNum++;
        }
        return bytes;
    }

    /**
     * @return the BitArray as an array of Bytes in big endian order
     */
    public Byte[] getBigEndianUnsignedByteArray() {
        return getBigEndianUnsignedByteArrayList().toArray(new Byte[0]);
    }


    /**
     * Copies a set of {@code nBits} bits from the specified little-endian bit
     * array to this bit array.
     *
     * @param bitArray
     *            is the little-endian bit array to copy the set of bits from.
     *
     * @param fromIndex
     *            is the position in the  little-endian bit array
     *            {@code bitArray} to start copying from.
     *
     * @param toIndex
     *            is the position in this little-endian bit array to start
     *            copying to.
     *
     * @param nBits
     *            is the number of bits to copy.
     */
    public void copy(LittleEndianBitArray bitArray,
                     int fromIndex,
                     int toIndex,
                     int nBits) {
        this.Copy(bitArray, fromIndex, toIndex, nBits);
    }

    /*
     * Copies a set of bits to a big-endian bit array.
     *
     * @param bitArray
     *            is the big-endian bit array to copy the set of bits to.
     * @param fromIndex
     *            is the position in this little-endian bit array to start
     *            copying from.
     * @param toIndex
     *            is the position in the big-endian bit array {@code bitArray}
     *            to start copying to.
     * @param nBits
     *            is the number of bits to copy.
    public void copy(BigEndianBitArray bitArray,
                     int fromIndex,
                     int toIndex,
                     int nBits) {
        this.Swab(bitArray, fromIndex, toIndex, nBits);
    }
     */

    /**
     * @param rng RandomNumberGenerator from which to pull random samples
     */
    public void randomize(RandomNumberGenerator rng) {
        int rem = size()%Integer.SIZE;
        int length = size()-rem;
        if (rem != 0) {
            set(rng.nextInt((int)Math.pow(2, rem)), size()-rem, rem);
        }
        while (length > 0) {
            length-=Integer.SIZE;
            set(rng.nextInt(), length, Integer.SIZE);
        }
    }

    /**
     * @return The original Bit Array but incremented by one, rollover to 0.
     */
    public LittleEndianBitArray incr(int x) {
        LittleEndianBitArray store = new LittleEndianBitArray(size()+2);
        store.copy(this, 0, 0, size());
        BigInteger num = new BigInteger(store.toHexString(), 16);
        BigInteger addend = BigInteger.valueOf(x);
        num = num.add(addend);
        store.set("0x" + num.toString(16));
        return store.getSlice(0, size());
    }

    /**
     * @return The original Bit Array but incremented by x, rollover to 0.
     */
    public LittleEndianBitArray incr() {
        return incr(1);
    }

    /**
     * In-place version of incr(), this affects the object it is called on.
     */
    public void add(int x) {
        int s = size();
        LittleEndianBitArray store = new LittleEndianBitArray(s+2);
        store.copy(this, 0, 0, s);
        BigInteger num = new BigInteger(store.toHexString(), 16);
        BigInteger addend = BigInteger.valueOf(x);
        num = num.add(addend);
        store.set("0x" + num.toString(16));
        // assign back to ourselves
        this.set(store.getSlice(0, s));
    }
    
    public void add(LittleEndianBitArray x) {
        int s = size();
        LittleEndianBitArray store = new LittleEndianBitArray(s+2);
        store.copy(this, 0, 0, s);
        
        BigInteger num      = new BigInteger(store.toHexString(), 16);
        BigInteger addend   = new BigInteger(x.toHexString(), 16);
        
        num = num.add(addend);
        
        store.set("0x" + num.toString(16));
        
        // assign back to ourselves
        this.set(store.getSlice(0, s));
    }

    public LittleEndianBitArray invert() {
        return this.Invert();
    }

    public LittleEndianBitArray invert(int index) {
        return this.Invert(index, 1);
    }

    public LittleEndianBitArray invert(int fromIndex, int nBits) {
        return this.Invert(fromIndex, nBits);
    }

    public LittleEndianBitArray and(LittleEndianBitArray array) {
        return this.And(array);
    }

    public LittleEndianBitArray or(LittleEndianBitArray array) {
        return this.Or(array);
    }

    public LittleEndianBitArray shiftLeft(int nBits) {
        return this.ShiftLeft(nBits);
    }

    public LittleEndianBitArray shiftLeftCyclic(int nBits) {
        LittleEndianBitArray bitArray = this.shiftLeft(nBits);
        bitArray.copy(this, this.size() - nBits, 0, nBits);
        return bitArray;
    }

    public LittleEndianBitArray shiftRight(int nBits) {
        return this.ShiftRight(nBits);
    }

    public LittleEndianBitArray shiftRightCyclic(int nBits) {
        LittleEndianBitArray bitArray = this.shiftRight(nBits);
        bitArray.copy(this, 0, this.size() - nBits, nBits);
        return bitArray;
    }

    public int parity() {
        int v = 0;
        for(int i = 0 ; i < this.size() ; i++)
        {
            v = v ^ (this.getBit(i) ? 1 : 0);
        }
        return v;
    }

    public static LittleEndianBitArray valueOf(int nBits, int[] value) {
        LittleEndianBitArray bitArray = new LittleEndianBitArray(nBits);
        bitArray.set(value);
        return bitArray;
    }

    public static LittleEndianBitArray valueOf(int nBits, int value) {
        LittleEndianBitArray bitArray = new LittleEndianBitArray(nBits);
        bitArray.set(value);
        return bitArray;
    }

    public LittleEndianBitArray xor(LittleEndianBitArray array) {
        return this.Xor(array);
    }
}
