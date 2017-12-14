package com.fulcrummicro.hw.verification.lib.bitarray;

/**
 * @author mhesseli
 */

import java.math.BigInteger;
import java.security.InvalidParameterException;
import java.util.BitSet;
import java.util.Iterator;

import com.fulcrummicro.hw.verification.lib.lang.ByteBuffer;

public abstract class BitArray<BitArrayT extends BitArray<BitArrayT>> implements Cloneable, Iterable<Integer> {

    /**************************************************************************
     * Constants & Types
     **************************************************************************/

    /**
     * The width of a long in bits.
     */
    private static final int L_SIZE = Long.SIZE;

    /**
     * The width of an int in bits.
     */
    private static final int I_SIZE = Integer.SIZE;

    /**
     * The width of a short in bits.
     */
    private static final int S_SIZE = Short.SIZE;

    /**
     * The width of a byte in bits.
     */
    private static final int B_SIZE = Byte.SIZE;

    protected class BooleanIterator implements Iterator<Boolean> {

        private int current;

        public BooleanIterator() {
            this.current = 0;
        }

        public boolean hasNext() {
            return this.current < BitArray.this.size();
        }

        public Boolean next() {
            return BitArray.this.bitSet.get(this.current++);
        }

        public void remove() {
            throw new UnsupportedOperationException();
        }
    }

    protected class IntegerIterator implements Iterator<Integer> {

        private int current;

        private final int toIndex;

        public IntegerIterator() {
            this(0, BitArray.this.size());
        }

        public IntegerIterator(int fromIndex, int toIndex) {
            this.current = BitArray.this.bitSet.nextSetBit(fromIndex);
            this.toIndex = toIndex;
        }

        public boolean hasNext() {
            return (this.current >= 0 && this.current < this.toIndex);
        }

        public Integer next() {
            int bit;

            bit = this.current;
            this.current = BitArray.this.bitSet.nextSetBit(++this.current);
            return bit;
        }

        public void remove() {
            throw new UnsupportedOperationException();
        }
    }

    /**************************************************************************
     * Private Variables
     **************************************************************************/

    /**
     * The backing store of the bit array.
     */
    protected final BitSet bitSet;

    /**
     * The size of the bit array.
     */
    protected final int size;

    /**************************************************************************
     * Private Functions
     **************************************************************************/

    /**
     * Checks whether the specified index is in {@code 0..this.size()-1}.
     *
     * @param index
     *        is the index to be checked.
     * @return  {@code true} if the index is in range; {@code false} otherwise.
     */
    private boolean CheckBounds(int index) {
        return (index >= 0 && index < this.size());
    }

    /**
     * Checks whether the specified {@code fromIndex} is in
     * {@code 0..this.size()-1} and the specified {@code toIndex} is in
     * {@code 1..this.size()}.
     *
     * @param fromIndex
     *        is the index of the first bit of the bit set to be checked.
     * @param toIndex
     *        is the index after the last bit of the bit set to be checked.
     * @return  {@code true} if the bit set is in range; {@code false} otherwise.
     */
    private boolean CheckBounds(int fromIndex, int toIndex) {
        return (fromIndex >= 0
                && fromIndex < this.size()
                && toIndex > 0
                && toIndex <= this.size());
    }

    private String FV_EARRAY(BitArrayT array) {
        return String.format("array.size() > this.size() (%d > %d)",
                             array.size(),
                             this.size());
    }

    private String FV_EFROM(int fromIndex) {
        return String.format("fromIndex(%d) not in 0..%d",
                             fromIndex,
                             this.size() - 1);
    }

    private String FV_EFROMNBITS(int fromIndex, int nBits) {
        return String.format("fromIndex(%d) not in 0..%d and/or nBits(%d) > %d",
                             fromIndex,
                             this.size() - 1,
                             nBits,
                             this.size() - fromIndex);
    }

    private String FV_EFROMTO(int fromIndex,
                                                       int toIndex) {
        return String.format(  "fromIndex(%d) not in 0..%d "
                             + "and/or toIndex(%d) not in 1..%d",
                             fromIndex,
                             this.size() - 1,
                             toIndex,
                             this.size());
    }

    private String FV_EINDEX(int index) {
        return String.format("index(%d) not in 0..%d",
                             index,
                             this.size() - 1);
    }

    private String FV_ENBITS(int nBits, int size) {
        return String.format("nBits(%d) not in 1..%d", nBits, size - 1);
    }

    private String FV_ETONBITS(int toIndex, int nBits) {
        return String.format("toIndex(%d) not in 0..%d and/or nBits(%d) > %d",
                             toIndex,
                             this.size() - 1,
                             nBits,
                             this.size() - toIndex);
    }

    private String FV_ETYPE(BitArrayT array) {
        return String.format("array(%s) not an instance of this(%s)",
                             array.getClass().getName(),
                             this.getClass().getName());
    }

    private String FV_EOVERFLOW(BitArrayT value, int nBits) {
        return String.format("value 0x%s does not fit in %d bits",
                             value.toHexString(),
                             nBits);
    }

    private String FV_EOVERFLOW(long value, int nBits) {
        return String.format("value %d does not fit in %d bits", value, nBits);
    }

    /**
     * Retrieves a {@code nBits}-bit signed value composed of bits starting at
     * {@code fromIndex}.
     *
     * @param fromIndex
     *            is the index of the first bit to include.
     * @param nBits
     *            is the width of the value to be retrieved.
     * @return the {@code nBits}-bit signed value.
     */
    private long GetSigned(int fromIndex, int nBits) {
        long value;

        value = this.GetUnsigned(fromIndex, nBits);

        // Perform sign extension.
        if (value >>> (nBits - 1) == 1) {
            value |= -1L & ~( (1L << nBits) - 1);
        }

        return value;
    }

    /**
     * Retrieves a {@code nBits}-bit unsigned value composed of bits starting
     * at {@code fromIndex}.
     *
     * @param fromIndex
     *            is the index of the first bit to include.
     * @param nBits
     *            is the width of the value to be retrieved.
     * @return the {@code nBits}-bit unsigned value.
     * @throws InvalidParameterException
     *             if {@code fromIndex} is not in {@code 0..this.size()-1}.
     */
    private long GetUnsigned(int fromIndex,
                             int nBits) throws InvalidParameterException {
        Iterator<Integer> it;
        int toIndex;
        long value = 0L;

        if (!this.CheckBounds(fromIndex)) {
            throw new InvalidParameterException(this.FV_EFROM(fromIndex));
        }

        toIndex = Math.min(this.size(), fromIndex + nBits);
        it = new IntegerIterator(fromIndex, toIndex);
        while (it.hasNext()) {
            value |= 1L << (it.next() - fromIndex);
        }

        return value;
    }

    /**
     * Sets the {@code nBits} bits following the specified {@code fromIndex} to
     * the specified value and sign extends the value to the specified
     * {@code extensionLimit}.
     *
     * @param value
     *            is the value to set the selected bits to.
     * @param fromIndex
     *            is the index of the first bit to be set.
     * @param nBits
     *            is the number of bits to set.
     * @param extensionLimit
     *            is the index after the last bit to include in sign extension.
     * @throws InvalidParameterException
     *             if {@code fromIndex} is not in {@code 0..this.size()-1} or
     *             {@code fromIndex+nBits} is not in {@code 1..this.size()}.
     */
    private void Set(long value,
                     int fromIndex,
                     int nBits,
                     int extensionLimit) throws InvalidParameterException {
        int i;
        int toIndex = fromIndex + nBits;

        if (!this.CheckBounds(fromIndex, toIndex)) {
            throw new InvalidParameterException(this.FV_EFROMTO(fromIndex,
                                                                toIndex));
        }

        if (// the right shift is modulo bit width, so >>> 64 is a no-op.
            (nBits < 64) && ((value >>> nBits) != 0)
            // if the value is < 0, but could be sign collapsed without losing
            // data, allow it.
            && ((value > 0) || (value <= -(1L << nBits)))) {
            throw new InvalidParameterException(this.FV_EOVERFLOW(value, nBits));
        }

        if (toIndex < this.size()) {
            // Perform sign extension.
            this.setBit(toIndex,
                        extensionLimit - toIndex,
                        ((value >>> (nBits - 1)) & 1L) == 1L);
        }

        for (i = 0; i < nBits; i++) {
            this.setBit(fromIndex + i, (value & 1L) == 1L);

            value >>>= 1;
        }
    }

    /**************************************************************************
     * Protected Functions
     **************************************************************************/

    /**
     * Performs a logical <b>AND</b> of this bit array and the specified bit
     * array.
     *
     * @param array
     *            is the bit array that is to be <b>AND</b>ed with this bit
     *            array.
     * @return the logical <b>AND</b> of this bit array and the specified bit
     *         array.
     * @throws InvalidParameterException
     *             if the specified bit array and this bit array are instances
     *             of different classes, or if the size of the specified bit
     *             array is greater than the size of this bit array.
     */
    protected BitArrayT And(BitArrayT array) throws InvalidParameterException {
        BitArrayT bitArray;

        if (!array.getClass().isInstance(this)) {
            throw new InvalidParameterException(this.FV_ETYPE(array));
        }
        if (array.size() > this.size()) {
            throw new InvalidParameterException(this.FV_EARRAY(array));
        }
        bitArray = this.clone();
        bitArray.bitSet.and(array.bitSet);
        return bitArray;
    }

    /**
     * Clears all bits in this bit array.
     */
    protected void Clear() {
        this.bitSet.clear();
    }

    /**
     * Copies a set of {@code nBits} bits from the specified bit array to this
     * bit array.
     *
     * @param array
     *            is the bit array to copy the set of bits from.
     * @param fromIndex
     *            is the index in the specified bit array array to start copying from.
     * @param toIndex
     *            is the index in this bit array to start copying to.
     * @param nBits
     *            is the number of bits to copy.
     * @throws InvalidParameterException
     *             if either {@code fromIndex} or {@code toIndex} is not in
     *             {@code 0..this.size()-1}, or if either
     *             {@code fromIndex+nBits} or {@code toIndex+nBits} is not in
     *             {@code 1..this.size()}.
     */
    protected void Copy(BitArray<BitArrayT> array,
                        int fromIndex,
                        int toIndex,
                        int nBits) throws InvalidParameterException {
        int i;

        if (!array.CheckBounds(fromIndex, fromIndex + nBits)) {
            throw new InvalidParameterException(array.FV_EFROMNBITS(fromIndex,
                                                                    nBits));
        }
        if (!this.CheckBounds(toIndex, toIndex + nBits)) {
            throw new InvalidParameterException(this.FV_ETONBITS(toIndex, nBits));
        }
        for (i = 0; i < nBits; i++) {
            this.setBit(toIndex + i, array.bitSet.get(fromIndex + i));
        }
    }

    /**
     * Inverts all bits.
     */
    protected BitArrayT Invert() {
        BitArrayT bitArray;

        bitArray = this.clone();
        bitArray.bitSet.flip(0, this.size());
        return bitArray;
    }

    /**
     * Inverts a set of bits from the specified {@code fromIndex} to the
     * specified {@code toIndex}.
     *
     * @param fromIndex
     *            is the index of the first bit to be inverted.
     * @param nBits
     *            is the number of bits to invert.
     * @return the (partly) inverted bit array.
     * @throws InvalidParameterException
     *             if {@code fromIndex} not in {@code 0..this.size()-1} or
     *             {@code toIndex} not in {@code 1..this.size()}
     */
    protected BitArrayT Invert(int fromIndex,
                               int nBits) throws InvalidParameterException {
        BitArrayT bitArray;
        int toIndex = fromIndex + nBits;

        if (!this.CheckBounds(fromIndex, toIndex)) {
            throw new InvalidParameterException(this.FV_EFROMTO(fromIndex,
                                                                toIndex));
        }
        bitArray = this.clone();
        bitArray.bitSet.flip(fromIndex, toIndex);
        return bitArray;
    }

    /**
     * Performs a logical <b>OR</b> of this bit array and the specified bit
     * array.
     *
     * @param array
     *            is the bit array that is to be <b>OR</b>ed with this bit
     *            array.
     * @return the logical <b>OR</b> of this bit array and the specified bit
     *         array.
     * @throws InvalidParameterException
     *             if the specified bit array and this bit array are instances
     *             of different classes, or if the size of the specified bit
     *             array is greater than the size of this bit array.
     */
    protected BitArrayT Or(BitArrayT array) throws InvalidParameterException {
        BitArrayT bitArray;

        if (!array.getClass().isInstance(this)) {
            throw new InvalidParameterException(this.FV_ETYPE(array));
        }
        if (array.size() > this.size()) {
            throw new InvalidParameterException(this.FV_EARRAY(array));
        }
        bitArray = this.clone();
        bitArray.bitSet.or(array.bitSet);
        return bitArray;
    }

    /**
     * Shifts the bit array {@code nBits} bit positions to the left.
     *
     * @param nBits
     *            is the number of bit positions to shift.
     * @return the shifted bit array.
     */
    protected BitArrayT ShiftLeft(int nBits) {
        BitArrayT bitArray;

        bitArray = this.clone();
        bitArray.clear();
        bitArray.Copy(this, 0, nBits, this.size() - nBits);
        return bitArray;
    }

    /**
     * Shifts the bit array {@code nBits} bit positions to the right.
     *
     * @param nBits
     *            is the number of bit positions to shift.
     * @return the shifted bit array.
     */
    protected BitArrayT ShiftRight(int nBits) {
        BitArrayT bitArray;

        bitArray = this.clone();
        bitArray.clear();
        bitArray.Copy(this, nBits, 0, this.size() - nBits);
        return bitArray;
    }

    /**
     * Returns a new bit array composed of bits from {@code fromIndex}
     * (inclusive) to {@code toIndex} (exclusive).
     *
     * @param fromIndex
     *            is the index of the first bit to include.
     * @param toIndex
     *            is the index after the last bit to include.
     * @return the bit array.
     */
    protected abstract BitArrayT Shrink(int fromIndex, int toIndex);

    /**
     * Copies a set of {@code nBits} bits from this bit array to the specified
     * bit array, simultaneously swapping the byte order.
     *
     * @param array
     *            is the bit array to copy the set of bits to.
     * @param fromIndex
     *            is the index in this bit array to start copying from.
     * @param toIndex
     *            is the index in the specified bit array to start copying to.
     * @param nBits
     *            is the number of bits to copy.
     * @throws InvalidParameterException
     *             if either {@code fromIndex} or {@code toIndex} is not in
     *             {@code 0..this.size()-1}, or if either
     *             {@code fromIndex+nBits} or {@code toIndex+nBits} is not in
     *             {@code 1..this.size()}.
     * @throws UnsupportedOperationException
     *             if the specified number of bits to copy is not a whole number
     *             of bytes.
     */
    protected void Swab(BitArray<?> array,
                        int fromIndex,
                        int toIndex,
                        int nBits) throws InvalidParameterException,
                                          UnsupportedOperationException {
        int i;
        int index;
        int mByte;

        if (!this.CheckBounds(fromIndex, fromIndex + nBits)) {
            throw new InvalidParameterException(this.FV_EFROMNBITS(fromIndex,
                                                                   nBits));
        }
        if (!array.CheckBounds(toIndex, toIndex + nBits)) {
            throw new InvalidParameterException(array.FV_ETONBITS(toIndex,
                                                                  nBits));
        }

        if (nBits == 0 || (nBits % B_SIZE) != 0) {
            throw new UnsupportedOperationException();
        }

        mByte = nBits / B_SIZE - 1;

        for (i = 0; i < nBits; i++) {
            index = toIndex + B_SIZE * (mByte - i / B_SIZE) + i % B_SIZE;

            array.setBit(index, this.bitSet.get(fromIndex + i));
        }
    }


    /**
     * Performs a logical <b>XOR</b> of this bit array and the specified bit
     * array.
     *
     * @param array
     *            is the bit array that is to be <b>XOR</b>ed with this bit
     *            array.
     * @return the logical <b>XOR</b> of this bit array and the specified bit
     *         array.
     * @throws InvalidParameterException
     *             if the specified bit array and this bit array are instances
     *             of different classes, or if the size of the specified bit
     *             array is greater than the size of this bit array.
     */
    protected BitArrayT Xor(BitArrayT array) throws InvalidParameterException {
        BitArrayT bitArray;

        if (!array.getClass().isInstance(this)) {
            throw new InvalidParameterException(this.FV_ETYPE(array));
        }
        if (array.size() > this.size()) {
            throw new InvalidParameterException(this.FV_EARRAY(array));
        }
        bitArray = this.clone();
        bitArray.bitSet.xor(array.bitSet);
        return bitArray;
    }

    /***************************************************************************
     * Public Functions
     **************************************************************************/

    /**
     * Creates a bit array whose initial size is large enough to explicitly
     * represent bits with indices in the range {@code 0} through
     * {@code nBits-1}.
     *
     * @param nBits
     *            is the initial size of the bit array.
     */
    public BitArray(int nBits) {
        this.size = nBits;
        this.bitSet = new BitSet(this.size());
    }

    /**
     * Returns an iterator over the bits in the bit array in ascending order.
     *
     * @return the bit iterator.
     */
    public Iterator<Boolean> booleanIterator() {
        return new BooleanIterator();
    }

    /**
     * Clones a bit array.
     *
     * @return the bit array clone.
     */
    @Override public abstract BitArrayT clone();

    /**
     * Compares whether some other object is "equal to" this bit array.
     *
     * Equality is defined as:
     * <ul>
     * <li> {@code obj} is non-null.</li>
     * <li> {@code obj} and this bit array are instances of the same runtime
     * class.</li>
     * <li> {@code obj} and this bit array are of equal size.</li>
     * <li> {@code obj} and this bit array have exactly the same set of bits set
     * to {@code true}.</li>
     * </ul>
     *
     * @param obj
     *        is the object to compare against this bit array.
     * @return {@code true} if the objects are the same; {@code false}
     *         otherwise.
     */
    @SuppressWarnings("unchecked")
    @Override public boolean equals(Object obj) {
        BitArrayT bitArray;

        if (obj != null) {
            if (obj.getClass().isInstance(this)) {
                bitArray = (BitArrayT) obj;
                if (bitArray.size() == this.size()) {
                    if (bitArray.bitSet.equals(this.bitSet)) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    /**
     * Returns an iterator over all set bits in the bit array in ascending
     * order.
     *
     * @return the bit iterator.
     */
    public Iterator<Integer> iterator() {
        return new IntegerIterator();
    }

    /**
     * Retrieves the number of bits set to {@code true} in this bit array.
     *
     * @return the number of bits set to {@code true}.
     */
    public int cardinality() {
        return this.bitSet.cardinality();
    }

    /**
     * Clears all bits in this bit array.
     */
    public abstract BitArrayT clear();

    /**
     * Clears the specified bit.
     *
     * @param index
     *            is the index of the bit to be cleared.
     * @throws InvalidParameterException
     *             if {@code index} is not in {@code 0..this.size()-1}.
     */
    public void clearBit(int index) throws InvalidParameterException {
        if (!this.CheckBounds(index)) {
            throw new InvalidParameterException(this.FV_EINDEX(index));
        }
        this.bitSet.clear(index);
    }

    /**
     * Clears the set of bits from the specified {@code fromIndex} (inclusive)
     * to the specified {@code toIndex} (exclusive).
     *
     * @param fromIndex
     *            is the index of the first bit to be cleared.
     * @param nBits
     *            is the number of bits to be cleared.
     * @throws InvalidParameterException
     *             if {@code fromIndex} is not in {@code 0..this.size()-1} or
     *             {@code toIndex} is not in {@code 1..this.size()}.
     */
    public void clearBit(int fromIndex,
                         int nBits) throws InvalidParameterException {
        int toIndex = fromIndex + nBits;
        if (!this.CheckBounds(fromIndex, toIndex)) {
            throw new InvalidParameterException(this.FV_EFROMTO(fromIndex,
                                                                toIndex));
        }
        this.bitSet.clear(fromIndex, toIndex+1);
    }

    public BigInteger getBigInteger(int fromIndex) {
        return this.getBigInteger(fromIndex, this.size());
    }

    public BigInteger getBigInteger(int fromIndex, int nBits) {
        Iterator<Integer> it;
        int bit;
        int m;
        int toIndex;
        byte[] byteArray;

        if (!this.CheckBounds(fromIndex)) {
            throw new InvalidParameterException(this.FV_EINDEX(fromIndex));
        }
        // Generate the BigInteger input array.
        // [Note: The input array must be in big-endian byte order.]
        byteArray = new byte[this.size() / B_SIZE + 1];
        m = byteArray.length - 1;
        toIndex = Math.min(this.size(), fromIndex + nBits);
        it = new IntegerIterator(fromIndex, toIndex);
        while (it.hasNext()) {
            bit = it.next();
            byteArray[m - (bit / B_SIZE)] |= (byte) (1 << (bit % B_SIZE));
        }
        return new BigInteger(byteArray);
    }


    /**
     * Retrieves the value of the bit with the specified index.
     *
     * @param index
     *            is the index of the bit whose value is to be retrieved.
     * @return the bit value.
     * @throws InvalidParameterException
     *             if {@code index} is not in {@code 0..this.size()-1}.
     */
    public boolean getBit(int index) throws InvalidParameterException {
        if (!this.CheckBounds(index)) {
            throw new InvalidParameterException(this.FV_EINDEX(index));
        }
        return this.bitSet.get(index);
    }

    /**
     * Retrieves an unsigned 64-bit value composed of bits starting at
     * {@code fromIndex}
     *
     * @param fromIndex
     *            is the index of the first bit in this bit array to include.
     * @return the unsigned 64-bit value.
     */
    public long getUnsignedLong(int fromIndex) {
        return this.GetUnsigned(fromIndex, L_SIZE);
    }

    /**
     * Retrieves an unsigned 64-bit value that is stored as an unsigned
     * {@code nBits}-bit value and composed of bits starting at
     * {@code fromIndex}
     *
     * @param fromIndex
     *            is the index of the first bit in this bit array to include.
     * @param nBits
     *            is the number of bits to include.
     * @return the unsigned 64-bit value.
     * @throws InvalidParameterException
     *             if {@code nBits} is not in {@code 1..64}.
     */
    public long getUnsignedLong(int fromIndex,
                                int nBits) throws InvalidParameterException {
        if (nBits <= 0 || nBits > L_SIZE) {
            throw new InvalidParameterException(this.FV_ENBITS(nBits, L_SIZE));
        }
        return this.GetUnsigned(fromIndex, nBits);
    }

    /**
     * Retrieves a signed 64-bit value composed of bits starting at
     * {@code fromIndex}
     *
     * @param fromIndex
     *            is the index of the first bit in this bit array to include.
     * @return the signed 64-bit value.
     */
    public long getSignedLong(int fromIndex) {
        return this.GetSigned(fromIndex, L_SIZE);
    }

    /**
     * Retrieves a signed 64-bit value that is stored as a signed {@code nBits}-bit
     * value and composed of bits starting at {@code fromIndex}.
     *
     * @param fromIndex
     *            is the index of the first bit in this bit array to include.
     * @param nBits
     *            is the number of bits to include.
     * @return the signed 64-bit value.
     * @throws InvalidParameterException
     *             if {@code nBits} is not in {@code 1..64}.
     */
    public long getSignedLong(int fromIndex,
                              int nBits) throws InvalidParameterException {
        if (nBits < 0 || nBits > L_SIZE) {
            throw new InvalidParameterException(this.FV_ENBITS(nBits, L_SIZE));
        }
        return this.GetSigned(fromIndex, nBits);
    }

    /**
     * Retrieves an unsigned 32-bit value composed of bits starting at
     * {@code fromIndex}
     *
     * @param fromIndex
     *            is the index of the first bit in this bit array to include.
     * @return the unsigned 32-bit value.
     */
    public int getUnsignedInt(int fromIndex) {
        return (int) this.GetUnsigned(fromIndex, I_SIZE);
    }

    /**
     * Retrieves an unsigned 32-bit value that is stored as an unsigned
     * {@code nBits}-bit value and composed of bits starting at
     * {@code fromIndex}
     *
     * @param fromIndex
     *            is the index of the first bit in this bit array to include.
     * @param nBits
     *            is the number of bits to include.
     * @return the unsigned 32-bit value.
     * @throws InvalidParameterException
     *             if {@code nBits} is not in {@code 0..32}.
     */
    public int getUnsignedInt(int fromIndex,
                              int nBits) throws InvalidParameterException {
        if (nBits < 0 || nBits > I_SIZE) {
            throw new InvalidParameterException(this.FV_ENBITS(nBits, I_SIZE));
        }
        return (int) this.GetUnsigned(fromIndex, nBits);
    }

    /**
     * Retrieves a signed 32-bit value composed of bits starting at
     * {@code fromIndex}
     *
     * @param fromIndex
     *            is the index of the first bit in this bit array to include.
     * @return the signed 32-bit value.
     */
    public int getSignedInt(int fromIndex) {
        return (int) this.GetSigned(fromIndex, I_SIZE);
    }

    /**
     * Retrieves a signed 32-bit value that is stored as a signed {@code nBits}-bit
     * value and composed of bits starting at {@code fromIndex}.
     *
     * @param fromIndex
     *            is the index of the first bit in this bit array to include.
     * @param nBits
     *            is the number of bits to include.
     * @return the signed 32-bit value.
     * @throws InvalidParameterException
     *             if {@code nBits} is not in {@code 0..32}.
     */
    public int getSignedInt(int fromIndex,
                            int nBits) throws InvalidParameterException {
        if (nBits < 0 || nBits > I_SIZE) {
            throw new InvalidParameterException(this.FV_ENBITS(nBits, I_SIZE));
        }
        return (int) this.GetSigned(fromIndex, nBits);
    }

    /**
     * Retrieves an unsigned 16-bit value composed of bits starting at
     * {@code fromIndex}
     *
     * @param fromIndex
     *            is the index of the first bit in this bit array to include.
     * @return the unsigned 16-bit value.
     */
    public short getUnsignedShort(int fromIndex) {
        return (short) this.GetUnsigned(fromIndex, S_SIZE);
    }

    /**
     * Retrieves an unsigned 16-bit value that is stored as an unsigned
     * {@code nBits}-bit value and composed of bits starting at
     * {@code fromIndex}
     *
     * @param fromIndex
     *            is the index of the first bit in this bit array to include.
     * @param nBits
     *            is the number of bits to include.
     * @return the unsigned 16-bit value.
     * @throws InvalidParameterException
     *             if {@code nBits} is not in {@code 0..16}.
     */
    public short getUnsignedShort(int fromIndex,
                                  int nBits) throws InvalidParameterException {
        if (nBits < 0 || nBits > S_SIZE) {
            throw new InvalidParameterException(this.FV_ENBITS(nBits, S_SIZE));
        }
        return (short) this.GetUnsigned(fromIndex, nBits);
    }

    /**
     * Retrieves a signed 16-bit value composed of bits starting at
     * {@code fromIndex}
     *
     * @param fromIndex
     *            is the index of the first bit in this bit array to include.
     * @return the signed 16-bit value.
     */
    public short getSignedShort(int fromIndex) {
        return (short) this.GetSigned(fromIndex, S_SIZE);
    }

    /**
     * Retrieves a signed 16-bit value that is stored as a signed {@code nBits}-bit
     * value and composed of bits starting at {@code fromIndex}.
     *
     * @param fromIndex
     *            is the index of the first bit in this bit array to include.
     * @param nBits
     *            is the number of bits to include.
     * @return the signed 16-bit value.
     * @throws InvalidParameterException
     *             if {@code nBits} is not in {@code 0..16}.
     */
    public short getSignedShort(int fromIndex,
                                int nBits) throws InvalidParameterException {
        if (nBits < 0 || nBits > S_SIZE) {
            throw new InvalidParameterException(this.FV_ENBITS(nBits, S_SIZE));
        }
        return (short) this.GetSigned(fromIndex, nBits);
    }

    /**
     * Retrieves an unsigned 8-bit value composed of bits starting at
     * {@code fromIndex}
     *
     * @param fromIndex
     *            is the index of the first bit in this bit array to include.
     * @return the unsigned 8-bit value.
     */
    public byte getUnsignedByte(int fromIndex) {
        return (byte) this.GetUnsigned(fromIndex, B_SIZE);
    }

    /**
     * Retrieves an unsigned 8-bit value that is stored as an unsigned
     * {@code nBits}-bit value and composed of bits starting at
     * {@code fromIndex}
     *
     * @param fromIndex
     *            is the index of the first bit in this bit array to include.
     * @param nBits
     *            is the number of bits to include.
     * @return the unsigned 8-bit value.
     * @throws InvalidParameterException
     *             if {@code nBits} is not in {@code 0..8}.
     */
    public byte getUnsignedByte(int fromIndex,
                                int nBits) throws InvalidParameterException {
        if (nBits < 0 || nBits > S_SIZE) {
            throw new InvalidParameterException(this.FV_ENBITS(nBits, S_SIZE));
        }
        return (byte) this.GetUnsigned(fromIndex, nBits);
    }

    /**
     * Retrieves a signed 8-bit value composed of bits starting at
     * {@code fromIndex}
     *
     * @param fromIndex
     *            is the index of the first bit in this bit array to include.
     * @return the signed 8-bit value.
     */
    public byte getSignedByte(int fromIndex) {
        return (byte) this.GetSigned(fromIndex, B_SIZE);
    }

    /**
     * Retrieves a signed 8-bit value that is stored as a signed {@code nBits}-bit
     * value and composed of bits starting at {@code fromIndex}.
     *
     * @param fromIndex
     *            is the index of the first bit in this bit array to include.
     * @param nBits
     *            is the number of bits to include.
     * @return the signed 8-bit value.
     * @throws InvalidParameterException
     *             if {@code nBits} is not in {@code 0..8}.
     */
    public byte getSignedByte(int fromIndex,
                              int nBits) throws InvalidParameterException {
        if (nBits < 0 || nBits > S_SIZE) {
            throw new InvalidParameterException(this.FV_ENBITS(nBits, S_SIZE));
        }
        return (byte) this.GetSigned(fromIndex, nBits);
    }

    public BitArrayT getSlice(int offset, int length) {
        return Shrink(offset, length + offset);
    }

    /**
     * Determines whether the bit array contains any bits set to {@code true}.
     *
     * @return  {@code true} if the bit array is empty, {@code false} otherwise.
     */
    public boolean isEmpty() {
        return this.bitSet.isEmpty();
    }

    /**
     * Reverses the bit order.
     *
     * @return the bit-reversed copy of this bit array.
     */
    public BitArrayT reverse() {
        BitArrayT bitArray = this.clone();

        for (int i = 0, j = bitArray.size - 1; i < this.size; i++, j--) {
            bitArray.setBit(j, this.getBit(i));
        }
        return bitArray;
    }

    /**
     * Retrieves the index of the first bit that is set to {@code false}
     * starting at {@code fromIndex}.
     *
     * @param fromIndex
     *            is the index of bit to start searching from.
     * @return the index of the first bit that is set to {@code false}. If no
     *         bits are found to be set to {@code false}, -1.
     */
    public int nextClearBit(int fromIndex) {
        return this.bitSet.nextClearBit(fromIndex);
    }

    /**
     * Retrieves the index of the first bit that is set to {@code true} starting
     * at {@code fromIndex}.
     *
     * @param fromIndex
     *            is the index of bit to start searching from.
     * @return the index of the first bit that is set to {@code false}. If no
     *         bits are found to be set to {@code true}, -1.
     */
    public int nextSetBit(int fromIndex) {
        return this.bitSet.nextSetBit(fromIndex);
    }

    /**
     * Sets this bit array to the specified bit array.
     *
     * @param array
     *            is the bit array to set this bit array to.
     * @throws InvalidParameterException
     *             if the specified bit array and this bit array are instances
     *             of different classes.
     */
    public void set(BitArrayT array) throws InvalidParameterException {
        if (!array.getClass().isInstance(this)) {
            throw new InvalidParameterException(this.FV_ETYPE(array));
        }
        this.Copy(array, 0, 0, array.size());
        if (this.size() > array.size()) {
            this.clearBit(array.size(), this.size() - array.size());
        }
    }

    /**
     * Sets the {@code nBits} bits following the specified {@code fromIndex}
     * (inclusive) to the specified value represented by a bit array.
     * <p>
     * Will not perform a sign extension.
     *
     * @param value
     *            is the value to the selected bits to.
     * @param fromIndex
     *            is the index of the first bit in this bit array to be set.
     * @param nBits
     *            is the number of bits to be set.
     * @throws InvalidParameterException
     *             if {@code fromIndex} is not in {@code 0..this.size-1}, or
     *             if {@code nBits} is out of the range of the size of the bit
     *             array, or if the value to set is larger than the size of the
     *             bit array.
     */
    public void set(BitArrayT value,
                    int fromIndex,
                    int nBits) throws InvalidParameterException {
        if (value.prevSetBit() >= nBits) {
            throw new InvalidParameterException(this.FV_EOVERFLOW(value, nBits));
        }
        this.Copy(value, 0, fromIndex, nBits);
    }

    /**
     * Sets the bit array to the specified {@link BigInteger} value.
     *
     * @param value
     *            is the value to set this bit array to.
     */
    public void set(BigInteger value) {
        this.set(value, 0);
    }

    /**
     * Sets the bit array to the specified {@link BigInteger} value.
     *
     * @param value
     *            is the value to set this bit array to.
     */
    public void set(BigInteger value, int fromIndex) {
        int nBits;

        nBits = value.bitLength() + 1;
        if (value.signum() == -1) {
            // The BigInteger.bitLength() method returns the minimum number of
            // bits needed to represent the absolute value of the supplied
            // BigInteger instance. For negative values this number is possibly
            // less than the width of the remainder of the bit array. To ensure
            // that negative values are encoded correctly, the number of bits to
            // set is increased to its maximum here.
            nBits = Math.max(nBits, this.size() - fromIndex);
        }
        this.set(value, fromIndex, nBits);
    }

    public void set(BigInteger value, int fromIndex, int nBits) {
        int bit = fromIndex;
        int toIndex = fromIndex + nBits;
        byte[] byteArray = value.toByteArray();

        if (!this.CheckBounds(fromIndex, toIndex)) {
            throw new InvalidParameterException(this.FV_EFROMTO(fromIndex,
                                                                toIndex));
        }

        for (int i = byteArray.length - 1; i >= 0; i--) {
            for (int j = 0; j < B_SIZE; j++) {
                if (((byteArray[i] >> j) & 0x1) > 0) {
                    this.setBit(bit);
                }
                if (++bit >= toIndex) {
                    return;
                }
            }
        }

        if (value.signum() == -1) {
            // Perform sign extension.
            do {
                this.setBit(bit);
            } while (++ bit < toIndex);
        }
    }

    /**
     * Sets the bit array to the value represented by the specified byte array.
     * <p>
     * Will perform a sign extension.
     *
     * @param value
     *            is the value to set this bit array to.
     * @throws InvalidParameterException
     *             if the value to set is larger than the size of the bit array.
     */
    public void set(byte[] value) throws InvalidParameterException {
        this.set(value, 0);
    }

    /**
     * Sets all bits following the specified {@code fromIndex} to the value
     * represented by the specified byte array.
     * <p>
     * Will perform a sign extension.
     *
     * @param value
     *            is the value to set the selected bits to.
     * @param fromIndex
     *            is the index of the first bit in this bit array to be set.
     * @throws InvalidParameterException
     *             if {@code fromIndex} is not in {@code 0..this.size-1}, or if
     *             the value to set is larger than the size of the bit array.
     */
    public void set(byte[] value, int fromIndex) throws InvalidParameterException {
        int extensionLimit;
        int toIndex = fromIndex + value.length * B_SIZE;

        if (!this.CheckBounds(fromIndex, toIndex)) {
            throw new InvalidParameterException(this.FV_EFROMTO(fromIndex, toIndex));
        }

        for (int i = 0, j = 1; i < value.length; i++, j++) {
            // Only perform sign extension for the last byte of the supplied
            // byte array.
            if (j == value.length) {
                extensionLimit = this.size();
            } else {
                extensionLimit = fromIndex + j * B_SIZE;
            }
            this.Set(value[i], fromIndex + i * B_SIZE, B_SIZE, extensionLimit);
        }
    }

    /**
     * Sets the {@code nBits} bits following the specified {@code fromIndex}
     * (inclusive) to the specified value represented by a byte array.
     * <p>
     * Will not perform a sign extension.
     *
     * @param value
     *            is the value to the selected bits to.
     * @param fromIndex
     *            is the index of the first bit in this bit array to be set.
     * @param nBits
     *            is the number of bits to be set.
     * @throws InvalidParameterException
     *             if {@code fromIndex} is not in {@code 0..this.size-1}, or
     *             if {@code nBits} is out of the range of the size of the byte
     *             array, or if the value to set is larger than the size of the
     *             bit array.
     */
    public void set(byte[] value,
                    int fromIndex,
                    int nBits) throws InvalidParameterException {
        int arraySize = value.length * B_SIZE;
        int toIndex = fromIndex + nBits;

        if (nBits < 0 || nBits > arraySize - 1) {
            throw new InvalidParameterException(
                this.FV_ENBITS(nBits, arraySize));
        }
        if (!this.CheckBounds(fromIndex, toIndex)) {
            throw new InvalidParameterException(this.FV_EFROMTO(fromIndex, toIndex));
        }

        int numBytes = nBits / B_SIZE; // number of bytes to copy
        if (nBits % B_SIZE > 0) // include last (incomplete byte)
            numBytes++;
        for (int i = 0; i < numBytes; i++) {
            // settings assuming it is not the last byte
            int bits = B_SIZE; // copy full byte
            int ext = fromIndex + ((i + 1) * B_SIZE); // don't sign extend
            if (i == (numBytes -1)) { // change settings if it is the last byte
                if ((nBits % B_SIZE) != 0)
                    bits = nBits % B_SIZE; // copy only partial
                ext = fromIndex + nBits; // don't sign extend
            }
            this.Set(value[i], fromIndex + (B_SIZE * i), bits, ext);
        }
    }

    /**
     * Sets the bit array to the specified 64-bit value.
     * <p>
     * Will perform a sign extension.
     *
     * @param value
     *            is the value to set this bit array to.
     */
    public void set(long value) {
        this.Set(value, 0, Math.min(this.size(), L_SIZE), this.size());
    }

    /**
     * Sets all bits following the specified {@code fromIndex} to the specified
     * 64-bit value.
     * <p>
     * Will perform a sign extension.
     *
     * @param value
     *            is the value to set the selected bits to.
     *
     * @param fromIndex
     *            is the index of the first bit in this bit array to be set.
     */
    public void set(long value, int fromIndex) {
        this.Set(value,
                 fromIndex,
                 Math.min(this.size() - fromIndex, L_SIZE),
                 this.size());
    }

    /**
     * Sets the {@code nBits} bits following the specified {@code fromIndex}
     * (inclusive) to the specified 64-bit value.
     * <p>
     * Will not perform a sign extension.
     *
     * @param value
     *            is the value to the selected bits to.
     * @param fromIndex
     *            is the index of the first bit in this bit array to be set.
     * @param nBits
     *            is the number of bits to be set.
     * @throws InvalidParameterException
     *             if {@code nBits} is not in {@code 0..64}.
     */
    public void set(long value,
                    int fromIndex,
                    int nBits) throws InvalidParameterException {
        if (nBits < 0 || nBits > L_SIZE) {
            throw new InvalidParameterException(this.FV_ENBITS(nBits, L_SIZE));
        }
        this.Set(value, fromIndex, nBits, fromIndex + nBits);
    }

    /**
     * Sets the bit array to the specified 32-bit value.
     * <p>
     * Will perform a sign extension.
     *
     * @param value
     *            is the value to set this bit array to.
     */
    public void set(int value) {
        this.Set(value, 0, Math.min(this.size(), I_SIZE), this.size());
    }

    /**
     * Sets all bits following the specified {@code fromIndex} to the specified
     * 32-bit value.
     * <p>
     * Will perform a sign extension.
     *
     * @param value
     *            is the value to set the selected bits to.
     * @param fromIndex
     *            is the index of the first bit in this bit array to be set.
     */
    public void set(int value, int fromIndex) {
        this.Set(value,
                 fromIndex,
                 Math.min(this.size() - fromIndex, I_SIZE),
                 this.size());
    }

    /**
     * Sets the {@code nBits} bits following the specified {@code fromIndex}
     * (inclusive) to the specified 32-bit value.
     * <p>
     * Will not perform a sign extension.
     *
     * @param value
     *            is the value to the selected bits to.
     * @param fromIndex
     *            is the index of the first bit in this bit array to be set.
     * @param nBits
     *            is the number of bits to be set.
     * @throws InvalidParameterException
     *             if {@code nBits} is not in {@code 0..32}.
     */
    public void set(int value,
                    int fromIndex,
                    int nBits) throws InvalidParameterException {
        if (nBits < 0 || nBits > I_SIZE) {
            throw new InvalidParameterException(this.FV_ENBITS(nBits, I_SIZE));
        }
        this.Set(value, fromIndex, nBits, fromIndex + nBits);
    }

    /**
     * Sets the bit array to the specified 16-bit value.
     * <p>
     * Will perform a sign extension.
     *
     * @param value
     *            is the value to set this bit array to.
     */
    public void set(short value) {
        this.Set(value, 0, Math.min(this.size(), S_SIZE), this.size());
    }

    /**
     * Sets all bits following the specified {@code fromIndex} to the specified
     * 16-bit value.
     * <p>
     * Will perform a sign extension.
     *
     * @param value
     *            is the value to set the selected bits to.
     * @param fromIndex
     *            is the index of the first bit in this bit array to be set.
     */
    public void set(short value, int fromIndex) {
        this.Set(value,
                 fromIndex,
                 Math.min(this.size() - fromIndex, S_SIZE),
                 this.size());
    }

    /**
     * Sets the {@code nBits} bits following the specified {@code fromIndex}
     * (inclusive) to the specified 16-bit value.
     * <p>
     * Will not perform a sign extension.
     *
     * @param value
     *            is the value to the selected bits to.
     * @param fromIndex
     *            is the index of the first bit in this bit array to be set.
     * @param nBits
     *            is the number of bits to be set.
     * @throws InvalidParameterException
     *             if {@code nBits} is not in {@code 0..16}.
     */
    public void set(short value,
                    int fromIndex,
                    int nBits) throws InvalidParameterException {
        if (nBits < 0 || nBits > S_SIZE) {
            throw new InvalidParameterException(this.FV_ENBITS(nBits, S_SIZE));
        }
        this.Set(value, fromIndex, nBits, fromIndex + nBits);
    }

    /**
     * Sets the bit array to the specified 8-bit value.
     * <p>
     * Will perform a sign extension.
     *
     * @param value
     *            is the value to set this bit array to.
     */
    public void set(byte value) {
        this.Set(value, 0, Math.min(this.size(), B_SIZE), this.size());
    }

    /**
     * Sets all bits following the specified {@code fromIndex} to the specified
     * 8-bit value.
     * <p>
     * Will perform a sign extension.
     *
     * @param value
     *            is the value to set the selected bits to.
     * @param fromIndex
     *            is the index of the first bit in this bit array to be set.
     */
    public void set(byte value, int fromIndex) {
        this.Set(value,
                 fromIndex,
                 Math.min(this.size() - fromIndex, B_SIZE),
                 this.size());
    }

    /**
     * Sets the {@code nBits} bits following the specified {@code fromIndex}
     * (inclusive) to the specified 8-bit value.
     * <p>
     * Will not perform a sign extension.
     *
     * @param value
     *            is the value to the selected bits to.
     * @param fromIndex
     *            is the index of the first bit in this bit array to be set.
     * @param nBits
     *            is the number of bits to be set.
     * @throws InvalidParameterException
     *             if {@code nBits} is not in {@code 0..16}.
     */
    public void set(byte value,
                    int fromIndex,
                    int nBits) throws InvalidParameterException {
        if (nBits < 0 || nBits > B_SIZE) {
            throw new InvalidParameterException(this.FV_ENBITS(nBits, B_SIZE));
        }
        this.Set(value, fromIndex, nBits, fromIndex + nBits);
    }

    public void set(short[] value) {
        for(int i=0; i<value.length; i++) {
            this.set(value[i], i * S_SIZE);
        }
    }

    public void set(int[] value) {
        for(int i=0; i<value.length; i++) {
            this.set(value[i], i * I_SIZE);
        }
    }


    public void set(Integer[] value) {
        for(int i=0; i<value.length; i++) {
            this.set(value[i], i * I_SIZE);
        }
    }

    public void set(int[] value, int fromIndex) {
        for(int i=0; i<value.length; i++) {
            this.set(value[i], i * I_SIZE + fromIndex);
        }
    }

    public void set(int[] value, int fromIndex, int nBits) {
        for(int i=0; i<value.length && nBits > 0; i++) {
            this.set(value[i], i * I_SIZE + fromIndex, Math.min(I_SIZE, nBits));
            nBits -= I_SIZE;
        }
    }

    public void set(Integer[] value, int fromIndex) {
        for(int i=0; i<value.length; i++) {
            this.set(value[i], i * I_SIZE + fromIndex);
        }
    }

    public void set(long[] value) {
        for(int i=0; i<value.length; i++) {
            this.set(value[i], i * L_SIZE);
        }
    }


    /**
     * Sets all bits following the specified {@code fromIndex} to the specified
     * string value.
     * <p>
     * Will perform a sign extension.
     *
     * @param value
     *            is the value to set the selected bits to. The format should be
     *               0xNNNN  for hex. TODO: support decimal, and possibly
     *               Mac address format AA:BB:CC:DD:EE:FF
     * @param fromIndex
     *            is the index of the first bit in this bit array to be set.
     */
    public void set(String value, int fromIndex) throws InvalidParameterException {
    	int[] x;

    	assert (value.matches("0x[0-9a-fA-F]+")) :
    		"Error expected hex string starting with 0x";

    	value = value.substring(2);

    	int len = value.length();


    	value = value.toLowerCase();

    	int numWords = (len % 8 == 0) ? len/8 : len/8 + 1;

    	x = new int[numWords];

    	for(int i = 0 ; i < numWords; i++)
    	{
    		int startIdx;
    		int endIdx;

    		if(len - 8*i - 8 < 2)
    		{
    			startIdx = 0;
    			endIdx = len - 8*i;
    		}else
    		{
    			startIdx = len - 8*i -8;
    			endIdx = len - 8*i;
    		}

    		//Note idx 0 is on the left side
    		String sub = value.substring(startIdx, endIdx);
    		Long d = Long.parseLong(sub, 16);
    		x[i] = d.intValue();
    	}

    	this.set(x, fromIndex);
    }

    /**
     * Sets all bits following to the specified
     * string value.
     * <p>
     * Will perform a sign extension.
     *
     * @param value
     *            is the value to set the selected bits to. The format should be
     *               0xNNNN  for hex. TODO: support decimal, and possibly
     *               Mac address format AA:BB:CC:DD:EE:FF
     */
    public void set(String value) throws InvalidParameterException{
    	this.set(value, 0);
    }

    /**
     * Sets the bit at the specified index.
     *
     * @param index
     *            is the index of the bit to be set.
     * @throws InvalidParameterException
     *             if {@code index} is not in {@code 0..this.size()-1}.
     */
    public void setBit(int index) throws InvalidParameterException {
        if (!this.CheckBounds(index)) {
            throw new InvalidParameterException(this.FV_EINDEX(index));
        }
        this.bitSet.set(index);
    }

    /**
     * Set the bit at the specified index to the specified value.
     *
     * @param index
     *            is the index of the bit to be set.
     * @param value
     *            is the value to set the bit to.
     * @throws InvalidParameterException
     *             if {@code index} is not in {@code 0..this.size()-1}.
     */
    public void setBit(int index, boolean value) {
        if (!this.CheckBounds(index)) {
            throw new InvalidParameterException(this.FV_EINDEX(index));
        }
        this.bitSet.set(index, value);
    }

    /**
     * Set the set of bits from the specified {@code fromIndex} (inclusive) to
     * the specified {@code toIndex} (exclusive).
     *
     * @param fromIndex
     *            is the index of the first bit to be set.
     * @param nBits
     *            is the number of bits to be set.
     * @throws InvalidParameterException
     *             if {@code fromIndex} is not in {@code 0..this.size()-1} or
     *             {@code toIndex} not in {@code 1..this.size()}.
     */
    public void setBit(int fromIndex,
                       int nBits) throws InvalidParameterException {
        int toIndex = fromIndex + nBits;
        if (!this.CheckBounds(fromIndex, toIndex)) {
            throw new InvalidParameterException(this.FV_EFROMTO(fromIndex,
                                                                toIndex));
        }
        this.bitSet.set(fromIndex, toIndex);
    }

    /**
     * Set the set of bits from the specified {@code fromIndex} (inclusive) to
     * the specified {@code toIndex} (exclusive) to the specified value.
     *
     * @param fromIndex
     *            is the index of the first bit to be set.
     * @param nBits
     *            is the number of bits to be set.
     * @param value
     *            is the value to set the bits to.
     * @throws InvalidParameterException
     *             if {@code fromIndex} is not in {@code 0..this.size()-1} or
     *             {@code toIndex} not in {@code 1..this.size()}.
     */
    public void setBit(int fromIndex, int nBits, boolean value) {
        int toIndex = fromIndex + nBits;
        if (!this.CheckBounds(fromIndex, toIndex)) {
            throw new InvalidParameterException(this.FV_EFROMTO(fromIndex,
                                                                toIndex));
        }
        this.bitSet.set(fromIndex, toIndex, value);
    }

    /**
     * Set all bits to the specified value.
     *
     * @param value
     *            is the value to set the bits to.
     */
    public void setAllTo(boolean value) {
        this.setBit(0, this.size(), value);
    }

    /**
     * Retrieves the size of the bit array.
     *
     * @return the bit array size in bits.
     */
    public int size() {
        return this.size;
    }

    /**
     * Converts the bit array into its binary string representation, with
     * optional byte separator and field width.
     *
     * @param separator
     *            is the string that separates bytes.
     * @param width
     *            is the desired width of the string excluding the separators in
     *            units of characters. Each character represents a single bit.
     * @return the binary string representation of this bit array.
     */
    public String toBinaryString(String separator, int width) {
        String s = new String();
        int numBits = 0;
        int i;

        // Iterator returns the index of the next non-zero bit.
        for (int bitNo : this) {
            for (i = numBits; i < bitNo; i++) {
                if ((numBits % 8) == 0 && numBits > 0) {
                    s = separator + s;
                }
                s = 0 + s;
                ++numBits;
            }
            if ((numBits % 8) == 0 && numBits > 0) {
                s = separator + s;
            }
            s = 1 + s;
            ++numBits;
        }

        while (numBits < width) {
            if ((numBits % 8) == 0 && numBits > 0) {
                s = separator + s;
            }
            s = 0 + s;
            ++numBits;
        }
        return s.length() > 0 ? s : "0";
    }

    /**
     * Converts the bit array into its binary string representation, with an
     * optional byte separator.
     *
     * @param separator
     *            is the string that separates bytes.
     * @return the binary string representation of this bit array.
     */
    public String toBinaryString(String separator) {
        return toBinaryString(separator, 0);
    }

    /**
     * Converts the bit array into its binary string representation.
     *
     * @return the binary string representation of this bit array.
     */
    public String toBinaryString() {
        return toBinaryString("", 0);
    }

    /**
     * Converts the bit array into its hexadecimal string representation.
     *
     * @param separator
     *            is the 32-bit word separator.
     * @param width
     *            is the desired width of the string excluding the separators in
     *            units of characters. Each character represents a 4-bit nibble.
     * @return the hexadecimal string representation of this bit array.
     */
    public String toHexString(String separator, int width) {
        Iterator<Integer> it;
        StringBuilder buffer = new StringBuilder();
        String s = null;
        int bitNo;
        int fraction;
        int mask;
        int numNibbles = 0;
        int value = 0;
        int word = 0;
        boolean hasNext;

        // Iterator returns the index of the next non-zero bit.
        it = this.iterator();
        hasNext = it.hasNext();
        while (hasNext) {
            bitNo = it.next();
            fraction = bitNo / I_SIZE;
            hasNext = it.hasNext();
            mask = 1 << (bitNo % I_SIZE);
            while (word < fraction || !hasNext) {
                if (word > 0) {
                    buffer.insert(0, separator);
                }
                if (word < fraction || hasNext) {
                    s = String.format("%08x", value);
                } else {
                    s = String.format("%x", value | mask);
                }
                buffer.insert(0, s);
                if (word == fraction && !hasNext) {
                    break;
                }
                s = null;
                value = 0;
                word++;
            }
            value |= mask;
        }
        numNibbles = 8 * word + (s == null ? 0 : s.length());
        while (numNibbles < width) {
            if ((numNibbles % 8) == 0 && numNibbles > 0) {
                buffer.insert(0, separator);
            }
            buffer.insert(0, "0");
            numNibbles++;
        }
        return buffer.length() > 0 ? buffer.toString() : "0";
    }

    /**
     * Converts the bit array into its hexadecimal string representation.
     *
     * @param separator
     *            is the 32-bit word separator.
     * @return the hexadecimal string representation of this bit array.
     */
    public String toHexString(String separator) {
        return toHexString(separator, 0);
    }

    /**
     * Converts the bit array into its hexadecimal string representation.
     *
     * @return the hexadecimal string representation of this bit array.
     */
    public String toHexString() {
        return toHexString("", 0);
    }

    @Override
    public String toString() {
        return toHexString();
    }

    public abstract void copy(BitArrayT bitArray,
                              int fromIndex,
                              int toIndex,
                              int nBits);
    public abstract BitArrayT invert();
    public abstract BitArrayT invert(int index);
    public abstract BitArrayT invert(int fromIndex, int nBits);
    public abstract BitArrayT shiftLeft(int nBits);
    public abstract BitArrayT shiftLeftCyclic(int nBits);
    public abstract BitArrayT shiftRight(int nBits);
    public abstract BitArrayT shiftRightCyclic(int nBits);
    public abstract BitArrayT and(BitArrayT array);
    public abstract BitArrayT or(BitArrayT array);
    public abstract BitArrayT xor(BitArrayT array);

    /**
     * Given {@code index} finds the next least significant bit that is set to
     * {@code true}.
     *
     * @param index
     *            is the bit position to start searching at.
     * @return the bit position of the next least significant bit set to {@code
     *         true} or -1 if no bit is set.
     * @throws InvalidParameterException
     *             if {@code index} is not in {@code 0..this.size()-1}.
     */
    public int prevSetBit(int index) throws InvalidParameterException {
        if (!this.CheckBounds(index)) {
            throw new InvalidParameterException(this.FV_EINDEX(index));
        }
        for (int i = index; i >= 0; i--) {
            if (this.getBit(i)) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Starting at the most significant bit, finds the next least significant
     * bit that is set to {@code true}.
     *
     * @return the bit position of the next least significant bit set to {@code
     *         true} or -1 if no bit is set.
     */
    public int prevSetBit() {
        return this.prevSetBit(this.size() - 1);
    }

    /**
     * Converts this bit array to an unsigned byte array.
     *
     * @return the unsigned byte array representation of this bit array.
     */
    public byte[] toUnsignedByteArray() {
        return this.toUnsignedByteArray(new byte[0]);
    }

    /**
     * Converts this bit array to an unsigned byte array. If the bit array fits
     * in the supplied byte array, it is returned therein, otherwise a new byte
     * array is allocated and returned.
     *
     * @param a
     *            is the byte array in which this bit array is to be stored if
     *            it fits.
     *
     * @return the unsigned byte array representation of this bit array.
     */
    public byte[] toUnsignedByteArray(byte[] a) {
        int nBytes;

        nBytes = (this.size() + (B_SIZE - 1)) / B_SIZE;
        if (a.length < nBytes) {
            a = new byte[nBytes];
        }
        for (int i = 0; i < nBytes; i++) {
            a[i] = this.getUnsignedByte(i * B_SIZE);
        }
        return a;
    }

    /**
     * Converts this bit array to a {@link ByteBuffer} instance.
     *
     * @return the {@code ByteBuffer} representation of this bit array.
     */
    public ByteBuffer toUnsignedByteBuffer() {
        ByteBuffer buffer;

        buffer = new ByteBuffer((this.size() + (B_SIZE - 1)) / B_SIZE);
        for (int i = 0; i < this.size(); i += B_SIZE) {
            buffer.add(this.getUnsignedByte(i));
        }
        return buffer;
    }

    /**
     * Converts this bit array to an unsigned integer array.
     *
     * @return the unsigned integer array representation of this bit array.
     */
    public int[] getUnsignedIntArray() {
        return this.toUnsignedIntArray(new int[0]);
    }

    /**
     * Converts this bit array to an unsigned integer array. If the bit array
     * fits in the supplied integer array, it is returned therein, otherwise a
     * new integer array is allocated and returned.
     *
     * @param a
     *            is the integer array in which this bit array is to be stored
     *            if it fits.
     * @return the unsigned integer array representation of this bit array.
     */
    public int[] toUnsignedIntArray(int[] a) {
        int nWords;

        nWords = (this.size() + (I_SIZE - 1)) / I_SIZE;
        if (a.length < nWords) {
            a = new int[nWords];
        }
        for (int i = 0; i < nWords; i++) {
            a[i] = this.getUnsignedInt(i * I_SIZE);
        }
        return a;
    }

}
