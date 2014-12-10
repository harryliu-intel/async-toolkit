/*
 * Copyright 2000, 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

import com.avlsi.tools.sigscan.Sigscan;
import java.math.BigInteger;

import java.util.Iterator;

/**
 * <p> Class for representing data on a bus. </p>
 *
 * @modifies MUTABLE
 * 
 * @author Dan Daly
 * @version $Revision$ $Date$
 *
 **/

public class Data implements Cloneable {
        
    public static final int LOGIC0 = Sigscan.LOGIC0;
    public static final int LOGIC1 = Sigscan.LOGIC1;
    public static final int LOGICX = Sigscan.LOGICX;
    public static final int LOGICZ = Sigscan.LOGICZ;

    private final int busSize;
    private final int busmask;
    private final int[] aval;
    private final int[] bval;

    public Data(int pinWidth) {
        this.busSize = pinWidth;
        int arrlen = (busSize -1)/32 + 1;
        this.aval = new int[arrlen];
        this.bval = new int[arrlen];
        busmask = (1<<(busSize%32))-1;
    }

    public Data(String values) {
        this(values.length());
        set(values);
    }
    
    public Data(BigInteger val) {
        this(val.bitLength());
        set(val);
    }

    public static Data valueOf(int val) {
        Data newdata = new Data(32);
        newdata.set(val);
        return newdata;
    }

    public static Data valueOf(long val) {
        Data newdata = new Data(64);
        newdata.set(val);
        return newdata;
    }

    public int getValueAt(int index) {
        assert(index< busSize);
        if (!testBit(bval,index)) {
            if (testBit(aval,index)) return Sigscan.LOGIC1;
            else return Sigscan.LOGIC0;
        } else {
            if (testBit(aval,index)) return Sigscan.LOGICX;
            else return Sigscan.LOGICZ;
        }
    }

    public int[] getAval() { return aval; }

    public int[] getBval() { return bval; }
    
    public String getStringData() { return getStringSection(0, busSize); }

    public String getStringSection(int startIndex, int endIndex) {
        if (endIndex > busSize) endIndex = busSize;
        StringBuffer buf = new StringBuffer();
        for (int loop=endIndex-1;loop>=startIndex;loop--) {
            if (!testBit(bval, loop)) {
                if (testBit(aval,loop)) buf.append('1');
                else buf.append('0');
            } else {
                if (testBit(aval,loop)) buf.append('X');
                else buf.append('Z');
            }
        }
        return buf.toString();
    }

    /**@return A long representation of the values on the bus.  Note
     * the char to bit conversion:
     * <pre>
     * '1' - 1
     * '0' - 0
     * 'Z' - 0
     * 'X' - 1
     * </pre>
     **/
    public long longValue() { return getLongData(); }

    /**@return A long representation of the values on the bus.  Note
     * the char to bit conversion:
     * <pre>
     * '1' - 1
     * '0' - 0
     * 'Z' - 0
     * 'X' - 1
     * </pre>
     **/
    public long getLongData() {
        if (busSize <=32) return (getIntData() & 0xFFFFFFFFL);
        else return (aval[1] &0xFFFFFFFFL)<<32 | aval[0]&0xFFFFFFFFL;
    }

    public boolean booleanValue() { return getBooleanData(); }
    
    public boolean getBooleanData() { 
        return (aval[0] & 0x1) > 0; 
    }

    public int intValue() { return getIntData(); }

    public int getIntData() { return aval[0]; }
        
    public int[] getaval() { return aval; }
    public int[] getbval() { return bval; }

    public final int setTo(Data newdata) {
        return setTo(newdata.getaval(), newdata.getbval());
    }
    
    public final int setTo(int[] newaval, int[] newbval) {
        int ret = 0;
        int numBits = busSize;
        for (int loop=0;loop<aval.length;loop++) {
            int newa = newaval[loop];
            int newb = newbval[loop];
            if (numBits < 32) {
                newa &= busmask;
                newb &= busmask;
            }
            if (aval[loop] != newa) {
                if (aval[loop] < newa) ret = 1;
                else ret = -1;
            }
            aval[loop] = newa;
            if (bval[loop] != newb) ret = -2;
            bval[loop] = newb;
            numBits -= 32;
        }
        return ret;
    }

    public BigInteger getBigIntegerData() { 
        byte[] bigData = new byte[busSize/8+1];//add an extra for sign
        //System.out.println("Getting BigInt of "+getStringData());
        int loop=0;
        for (loop=0;loop<busSize;loop++) {
            int bigEndian = loop;//busSize - loop - 1;
            if (testBit(aval,loop)) {
                //System.out.println(loop+" set or with "+(1<<(bigEndian%8)));
                bigData[bigData.length -1 -(bigEndian/8)] 
                    |= 1<<(bigEndian%8);
            } else {
                //System.out.println(loop+" clear and with "+(((1<<8)-1) ^ 1<<(bigEndian%8)));
                bigData[bigData.length -1 -(bigEndian/8)] &= 
                    ((1<<8)-1) ^ 1<<(bigEndian%8);
            }
        }
        bigData[0] &= ((1<<8)-1) ^ 1<<(loop%8); //unsigned
        BigInteger ret = new BigInteger(bigData);
        if ((busSize <64) && (ret.longValue() != getLongData())) {
            System.out.println("get BigInt failed: "+ret.longValue()+
                               " != "+getLongData()+" busSize= "+busSize);
        }
        return ret;
    }

    /** @return true if all pins are high Z **/
    public boolean isZ() {
        for (int loop=0;loop<busSize;loop++) {
            if (!testBit(bval,loop) || testBit(aval,loop)) return false;
        }
        return true;
    }
            
    /** @return true if all pins are X **/
    public boolean isX() {
        for (int loop=0;loop<busSize;loop++) {
            if (!testBit(bval,loop) || !testBit(aval,loop)) return false;
        }
        return true;
    }

    public boolean isX(int position) {
        if (testBit(bval,position) && testBit(aval,position)) return true;
        return false;
    }

    public boolean isZ(int position) {
        if (testBit(bval,position) && !testBit(aval,position)) return true;
        return false;
    }

    /** Justs tests the aval **/
    public boolean testBit(int n) { 
        if (n >= busSize) return false;
        return testBit(aval, n); 
    }

    public void setBval(int n) {
        if (n >= busSize) return;
        setBit(bval, n);
    }

    public void clearBval(int n) {
        if (n >= busSize) return;
        clearBit(bval, n);
    }

    /** Just sets the aval **/
    public void setBit(int n) { 
        if (n >= busSize) return;
        setBit(aval, n); 
    }

    /** Just flips the aval **/
    public void flipBit(int n) {
        if (n >= busSize) return;
        flipBit(aval, n); 
    }
    
    /** Just clears the aval **/
    public void clearBit(int n) { 
        if (n >= busSize) return;
        clearBit(aval, n); 
    }

    private static void setBit(int[] arr, int index) {
        arr[index/32] |= 1<<(index%32);
    }

    private static void clearBit(int[] arr, int index) {
        arr[index/32] &= 0xFFFFFFFF ^ 1<<(index%32);
    }

    private static boolean testBit(int[] arr, int index) {
        return (arr[index/32] & (1<<(index%32))) != 0;
    }

    /** Inverts the value of the bit at <code>pos</code> **/
    private static void flipBit(int[] arr, int pos) { 
        arr[pos/32] ^= 1<<(pos%32); 
    }

    public void setBitTo(int pos, int value) {
        switch(value) {
            case LOGIC0:
                clearBit(aval, pos); clearBit(bval, pos);
                break;
            case LOGIC1:
                setBit(aval, pos); clearBit(bval, pos);
                break;
            case LOGICZ:
                clearBit(aval, pos); setBit(bval, pos);
                break;
            case LOGICX:
                setBit(aval, pos); setBit(bval, pos);
                break;
        }
    }

    private static void fill(int[] arr, int num,int busSize) {
        int numBits = busSize;
        for (int loop=0;loop<arr.length;loop++) {
            if (numBits < 32) {
                arr[loop] = num & (1<<(busSize%32))-1;
            } else {
                arr[loop] = num;
            }
            numBits -= 32;
        }
    }
    
    public void setAllTo(char value) {
        switch(value) {
          case '0':
            fill(aval, 0, busSize); fill(bval, 0, busSize); break;
          case '1':
            fill(aval, -1, busSize); fill(bval, 0, busSize); break;
          case 'X':
          case 'x':
            fill(aval, -1, busSize); fill(bval, -1, busSize); break;
          case 'Z':
          case 'z':
            fill(aval, 0, busSize); fill(bval, -1, busSize); break;
          default: assert(false);
        }
    }

    public void set(String newval) {
        int count = 0;
        for (int loop=0;loop<aval.length;loop++) {
            if (newval.length() == loop) break;
            if (count == busSize) break;
            switch(newval.charAt(loop)) {
              case '0':
                clearBit(aval,loop); clearBit(bval,loop); break;
              case '1':
                setBit(aval,loop); clearBit(bval,loop); break;
              case 'X':
              case 'x':
                setBit(aval,loop); setBit(bval,loop); break;
              case 'Z':
              case 'z':
                clearBit(aval,loop); setBit(bval,loop); break;
              default: assert(false);
            }
            count++;
        }
    }

    public void set(int[] newaval, int[] newbval) {
        int numBits = busSize;
        for (int loop=0;loop<aval.length;loop++) {
            if (newaval.length == loop) break;
            if (newbval.length == loop) break;
            int newa = newaval[loop];
            int newb = newbval[loop];
            if (numBits < 32) {
                newa &= busmask;
                newb &= busmask;
            }
            this.aval[loop] = newa;
            this.bval[loop] = newb;
        }
    }

    public void set(int newval) {
        if (busSize < 32) aval[0] = newval & busmask;
        else aval[0] = newval;
        bval[0] = 0;
        for (int loop=1;loop<aval.length;loop++) {
            aval[loop] = 0;
            bval[loop] = 0;
        }
        assert((((long) newval) & 0xFFFFFFFFL) == getLongData());
    }

    public void set(long[] newval) {
        for (int loop=0;loop<aval.length;loop+=2) {
            if (newval.length <= loop/2) {
                aval[loop] = 0;
                bval[loop] = 0;
            } else {
                setLong(loop, newval[loop/2]);
            }
        }
    }

    public void set(long newval) {
        int loop = 1;
        if (busSize > 32) loop++;
        setLong(0, newval);
        for (;loop<aval.length;loop++) {
            aval[loop] = 0;
            bval[loop] = 0;
        }
    }

    public void setLong(int index, long newval) {
        int pinIndex = busSize - index*32;
        if (pinIndex < 0) return;
        if (pinIndex <= 32) 
            aval[index] = (int) (newval & busmask);
        else {
            aval[index] = (int)(newval & 0xFFFFFFFFL);
            aval[index+1] = (int)((newval >> 32) & 0xFFFFFFFFl); 
            if (pinIndex < 64)
                aval[index+1] &= busmask;
            bval[index+1] = 0;
        }
        bval[index] = 0;
        //System.out.println("newval= "+Long.toHexString(newval)+" aval= "+Integer.toHexString(aval[index]));
    }

    public void set(BigInteger newval) {
        for (int loop=0;loop<busSize;loop++) {
            if (newval.testBit(loop)) setBit(aval, loop);
            else clearBit(aval, loop);
        }
        assert (!((busSize <=64) && (newval.longValue() != getLongData()))) :
            "BigInt set failed: "+Long.toHexString(newval.longValue())+" != "+Long.toHexString(getLongData());
    }

    public static Data invert(Data d) {
        Data newdata = new Data(d.busSize);
        for (int i=0;i<d.busSize;i++) {
            if (!testBit(d.bval, i)) {
                if (testBit(d.aval,i)) clearBit(newdata.aval, i);
                else setBit(newdata.aval, i);
            }
        }
        return newdata;
    }

    /** Does not handle X and Z very well **/
    public static Data and(Data[] ds) {
        if ((ds == null) || (ds.length == 0)) return null;
        Data newdata = new Data(ds[0].busSize);
        for (int index=0;index<newdata.busSize;index++) {
            boolean alltrue = true;
            clearBit(newdata.bval, index);
            for (int loop=0;alltrue && (loop<ds.length);loop++) {
                int val = ds[loop].getValueAt(index);
                switch(val) {
                  case LOGIC0:  alltrue=false; ;break;
                  case LOGIC1:  break;
                  case LOGICZ:  alltrue = false; break;
                  default:
                    alltrue = false;
                    setBit(newdata.aval, index);
                    setBit(newdata.bval, index);
                    loop = ds.length;
                }
            }
            if (alltrue) setBit(newdata.aval, index);
            else clearBit(newdata.aval, index);
        }
        return newdata;
    }           
    
    public boolean equals(Data other) {
        for (int loop=0;loop<aval.length;loop++) {
            if (other.aval.length <= loop) return false;
            if (aval[loop] != other.aval[loop]) return false;
            if (bval[loop] != other.bval[loop]) return false;
        }
        return true;
    }

    public Object clone(){
        Data newdata = new Data(busSize);
        newdata.set(aval, bval);
        return newdata;
    }
    
    public String toString() { return getStringData(); }

    public String toString(int radix) {
        StringBuffer buf = new StringBuffer();
        for (int loop=0;loop<aval.length;loop++) {
            //use long for unsigned
            buf.insert(0,Long.toString(((long)aval[loop])&0xFFFFFFFFL, radix));
        }
        return buf.toString();
    }

    /** Calculates the Data value that results from the combination of these
     * values.  All Data objects in iterator must have the same length.
     * For each line in the bus, it does the following combination:
     *<pre>
     * for each bit in bus:
     *   for each driven value in bus:
     *     if all drivers are driving Z, return Z for that bit
     *     if all drivers EXCEPT one and only one are driving Z, then return the
     *       value on that non-Z bus.
     *     if more than one driver is not Z, return X for that bit
     *
     *     For Example:
     *     Driver 1:  01XZ
     *     Driver 2:  Z01X
     *     Result:    0XXX
     *
     *     Driver 1:  0ZZZ
     *     Driver 2:  Z1ZZ
     *     Driver 3:  ZZ11
     *     Result:    0111
     *
     *     Driver 1:  0011
     *     Driver 2:  1010
     *     Result:    XXXX
     *  
     *</pre>
     **/
    public static Data calculateCombinedValue(Iterator datas) {
        if (!datas.hasNext()) return null;
        Data newdata = (Data) ((Data)datas.next()).clone();
        while (datas.hasNext()) {
            Data nextdata = (Data) datas.next();
            doKarnaughMap(newdata, nextdata);
        }
        return newdata;
    }

    public int width() {
        return busSize;
    }

    // XXX: what exactly is this doing? mapping should be documented.
    // what about this method is a karnaugh map? --frederik
    // example: (is this correct?)
    // d1 = Z:=(0,1); d2 = 1:=(1,0)
    // y = 0. d1.b = 1, d1.a = 0. new d1 = (0,1):=Z
    public static void doKarnaughMap(Data d1, Data d2) {
        assert d1.width() == d2.width();
        for (int loop=0;loop<d1.aval.length;loop++) {
            // y covers the case when the result is X - either both
            // are 0 or 1, or one or the other is X.
            int y = (~d1.bval[loop] & ~d2.bval[loop]) | 
                     (d2.bval[loop] &  d2.aval[loop]) |
                     (d1.bval[loop] &  d1.aval[loop]);
            // if y is 0 then the remaining cases are Z$N -> N, N$Z ->
            // N (covered by ORing aval's), and Z$Z -> Z (covered by
            // ANDing bval's)
            d1.aval[loop] = y | d1.aval[loop] | d2.aval[loop];
            d1.bval[loop] = y | (d1.bval[loop] & d2.bval[loop]);
        }
    }
}
