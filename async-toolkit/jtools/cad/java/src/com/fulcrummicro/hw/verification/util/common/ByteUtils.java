/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.fulcrummicro.hw.verification.util.common;

import java.io.PrintStream;
import java.util.ArrayList;

/**
 * Byte array utilities:
 *
 *  String Formatting
 *  Comparison
 *  Printing a "diff"
 *
 * @author Dan Daly
 * @version $Date$
 **/
public class ByteUtils {

    /**
     * Does a byte-by-byte comparision of the byte arrays
     * and returns the length of the matching prefix
     * @param b1
     * @param b2
     */
    public static int matchingPrefixLength(byte[] b1, byte[] b2) {
        if ((b1==null) || (b2 == null)) return 0;
        for (int loop=0; loop<b1.length; loop++) {
            if (b2.length <= loop) return loop;
            if (b1[loop] != b2[loop]) return loop;
        }
        return b1.length;
    }

    /** Does a byte-by-byte comparison of the byte arrays
     * @param b1 Byte array 1
     * @param b2 Byte array 2
     * @return true if the arrays are equal
     **/
    public static boolean bytesEqual(byte[] b1, byte[] b2) {
        if ((b1 == null) && (b2 == null)) return true;
        if  (b1 == null) return false;
        if  (b2 == null) return false;
        for (int loop=0;loop<b1.length;loop++) {
            if (b2.length <= loop)   return false;
            if (b1[loop] != b2[loop]) return false;
        }
        return true;
    }

    /** Does a byte-by-byte comparison of the byte arrays.  Will only check the
     * bits starting at <code>start</code> and the bytes at
     * index less than start+numChecked.
     *
     * @param b1 Byte array 1
     * @param b2 Byte array 2
     * @param start The starting index to do the check
     * @param numChecked The number of bytes to check
     * @return true if the arrays are equal
     **/
    public static boolean bytesEqual(byte[] b1, byte[] b2, int start, int numChecked) {
        if ((b1 == null) && (b2 == null)) return true;
        if  (b1 == null) return false;
        if  (b2 == null) return false;
        for (int loop=start;loop<start+numChecked;loop++) {
            if ((b1.length <= loop) || (b2.length <= loop)) {
                //byte arrays finished, with no more bytes to check
                if (b1.length == b2.length) return true;
                else return false;
            }
            if (b1[loop] != b2[loop]) return false;
        }
        return true;
    }

    public static boolean bytesEqual(byte[] b1, int start1,
                                     byte[] b2, int start2,
                                     int numChecked) {
        if ((b1 == null) && (b2 == null)) return true;
        if  (b1 == null) return false;
        if  (b2 == null) return false;
        for (int loop=0;loop<numChecked;loop++) {
            int index1 = loop+start1;
            int index2 = loop+start2;
            if ((b1.length <= index1) || (b2.length <= index2)) {
                //Ran out of one buffer
                return false;
            }
            if (b1[index1] != b2[index2]) return false;
        }
        return true;
    }
    protected static final int default_bytesPerLine = 20;

    public static void printBytes(byte[] b, PrintStream out) {
        printBytes(b,System.out,default_bytesPerLine); }

    public static void printBytes(byte[] b,
                                  PrintStream out, int bytesPerLine) {
        out.print(printBytes(b, bytesPerLine));
    }

    public static String printBytes(byte[] b, int bytesPerLine) {
        if (bytesPerLine == 0) return "";
        StringBuilder buf = new StringBuilder();
        for (int loop=0;loop<b.length;loop+=bytesPerLine) {
            buf.append(printByteLine(b, loop,bytesPerLine));
            buf.append("\n");
        }
        return buf.toString();
    }

    public static String printBytes(byte[] b) {
        return printBytes(b, default_bytesPerLine);
    }


    public static void printByteDiff(byte[] b1, byte[] b2) {
        printByteDiff(b1, b2, System.out, default_bytesPerLine/2);
    }

    public static void printByteDiff(byte[] b1, byte[] b2,
                                     PrintStream out, int bytesPerLine) {
        out.print(byteDiff(b1, b2, bytesPerLine));
    }

    public static String byteDiff(byte[] b1, byte[] b2) {
        return byteDiff(b1, b2, default_bytesPerLine/2);
    }

    public static String byteDiff(byte[] b1, byte[] b2, int bytesPerLine) {
        StringBuilder buf = new StringBuilder();
        int maxBytes = Math.max(b1.length, b2.length);
        for (int loop=0;loop<maxBytes;loop+=bytesPerLine) {
            buf.append(printByteLine(b1, loop,bytesPerLine));
            if (bytesEqual(b1, b2, loop, bytesPerLine))
                buf.append("      ");
            else
                buf.append("  !=  ");
            buf.append(printByteLine(b2, loop,bytesPerLine));
            buf.append("\n");
        }
        return buf.toString();
    }

    public static String printBrief(byte[] data) {
        String output = ByteUtils
                        .toHexStringWithSpaces(data);
        return "<"+output.substring(0,Math.min(11, output.length()))
            +"...("+data.length+")>";
    }

    public static String toHexString(byte[] b) {
        StringBuilder sb=new StringBuilder();
        for(int i=0; i<b.length; i++) {
            sb.append(Integer.toHexString(b[i]-256).substring(6));
        }
        return sb.toString();
    }

    public static String toHexStringWithSpaces(byte[] b) {
        StringBuilder sb=new StringBuilder();
        for(int i=0; i<b.length; i++) {
            if(i>0) sb.append(" ");
            sb.append(Integer.toHexString(b[i]-256).substring(6));
        }
        return sb.toString();
    }

    public static String toHexString(int b) {
        return Integer.toHexString(b-256).substring(6);
    }

    /** Does BigEndian integer to byte conversion. **/
    public static short btos(byte[] arr, int offset) {
        return btos(arr, offset, true);
    }

    /** Breaks an integer into a 4 byte array
     * @param bigEndian set to true to have the msb in the first array index
     * (little endian)**/
    public static short btos(byte[] arr, int offset, boolean bigEndian) {
        short data = 0;
        for (int loop=0;loop<2;loop++) {
            int index = loop;
            if (bigEndian) index = 2-loop-1;
            if ((offset+index) < arr.length)
                data |= (arr[offset+index] & 0xff) << (8*loop);
        }
        return data;
    }

    /** Does BigEndian integer to byte conversion. **/
    public static int btoi(byte[] arr, int offset) {
        return btoi(arr, offset, true);
    }

    /** Breaks an integer into a 4 byte array
     * @param bigEndian set to true to have the msb in the first array index
     * (little endian)**/
    public static int btoi(byte[] arr, int offset, boolean bigEndian) {
        int data = 0;
        for (int loop=0;loop<4;loop++) {
            int index = loop;
            if (bigEndian) index = 4-loop-1;
            if ((offset+index) < arr.length)
                data |= ((int)arr[offset+index] & 0xff) << (8*loop);
        }
        return data;
    }

    /** Does BigEndian integer to byte conversion. **/
    public static long btol(byte[] arr, int offset) {
        return btol(arr, offset, true);
    }

    /** Breaks an integer into a 4 byte array
     * @param bigEndian set to true to have the msb in the first array index
     * (little endian)**/
    public static long btol(byte[] arr, int offset, boolean bigEndian) {
        long data = 0;
        for (int loop=0;loop<8;loop++) {
            int index = loop;
            if (bigEndian) index = 8-loop-1;
            if ((offset+index) < arr.length)
                data |= ((long)arr[offset+index] & 0xff) << (8*loop);
        }
        return data;
    }
    /** Does big-endian integer to byte array conversion. **/
    public static byte[] itob(long src) {
        return itob(src, true);
    }

    /** Breaks an integer into a 4 byte array
     * @param bigEndian set to true to have the lsb in the first array index
     * (little endian)**/
    public static byte[] itob(long src, boolean bigEndian) {
        byte[] data = new byte[4];
        for (int loop=0;loop<4;loop++) {
            int index = loop;
            if (bigEndian) index = 4-loop-1;
            data[index] =
                (byte) ((src >> (8*loop) & 0xff));
        }
        return data;
    }

    /** Does big endian long to byte conversion. **/
    public static byte[] ltob(long src) {
        return ltob(src, true);
    }

    /** Breaks an integer into a 8 byte array
     * @param bigEndian set to true to have the lsb in the first array index
     * (little endian)**/
    public static byte[] ltob(long src, boolean bigEndian) {
        byte[] data = new byte[8];
        for (int loop=0;loop<8;loop++) {
            int index = loop;
            if (bigEndian) index = 8-loop-1;
            data[index] = (byte) ((src >> (8*loop) & 0xff));
        }
        return data;
    }

    /** Does big-endian integer array to byte array conversion. **/
    public static byte[] iatob(ArrayList<Integer> src) {
        return iatob(src, true);
    }

    public static byte[] iatob(ArrayList<Integer> src, boolean bigEndian) {
        byte[] data = new byte[src.size()*4];
        for(int i=0;i<src.size();i++) {
            byte[] intData = ByteUtils.itob(src.get(i), bigEndian);
            System.arraycopy(intData,0,data,i*4,4);
        }
        return data;
    }

    /** Does big-endian integer array to byte array conversion. **/
    public static byte[] iatob(int[] src) {
        return iatob(src, true);
    }

    public static byte[] iatob(int[] src, boolean bigEndian) {
        byte[] data = new byte[src.length*4];
        for(int i=0;i<src.length;i++) {
            byte[] intData = ByteUtils.itob(src[i], bigEndian);
            System.arraycopy(intData,0,data,i*4,4);
        }
        return data;
    }

    public static int[] btoia(byte[] src) {
        return btoia(src, true);
    }

    public static int[] btoia(byte[] src, boolean bigEndian) {
        int[] data = new int[src.length/4 + (src.length%4==0 ? 0 : 1)];
        for(int i=0;i*4<src.length;i++) {
            data[i] = ByteUtils.btoi(src, i*4, bigEndian);
        }
        return data;
    }

    public static ArrayList<Integer> btoial(byte[] src) {
        return btoial(src, true);
    }

    public static ArrayList<Integer> btoial(byte[] src, boolean bigEndian) {
        ArrayList<Integer> data = new ArrayList<Integer>();
        for(int i=0;i*4<src.length;i++) {
            data.add(ByteUtils.btoi(src, i*4, bigEndian));
        }
        return data;
    }

    public static int[] reverse(int[] in) {
        int[] out = new int[in.length];
        for (int i = 0; i < out.length; i++) {
            out[i] = in[out.length- 1 - i];
        }
        return out;
    }

    public static byte[] reverse(byte[] in) {
        byte[] out = new byte[in.length];
        for (int i = 0; i < out.length; i++) {
            out[i] = in[out.length- 1 - i];
        }
        return out;
    }

    /*************************************************************************/

    /** Helper Output Functions **/

    public static String printByteLine(byte[] b, int start, int numPrint) {
        StringBuilder buf = new StringBuilder();
        for (int loop=start;loop<start+numPrint;loop++) {
            buf.append(" "+
                       ((loop>=b.length)?"  ":
                        Integer.toHexString(b[loop]-256).substring(6)));
        }
        return buf.toString();
    }

    /** convert a hex string into bytes array
     *  src: source hex string to be converted
     *  index: string index to start conversion
     *  len: length of byte[] array to be generated**/
    public static byte[] hexStringToBytes(String src, int startIndex, int len)
                                          throws RuntimeException{

        byte [] destBytes = new byte[len];

        int charVal;
        int byteHN=0, byteLN=0; //2 nibbles per byte

        if(src.substring(startIndex).startsWith("0x")) startIndex += 2;

        for(int index = startIndex; index<src.length(); index++) {
            char srcChar = src.charAt(index);

            if(srcChar >= '0' && srcChar <= '9') {
                charVal = srcChar-'0';
            }
            else if(srcChar >= 'a' && srcChar<= 'f') {
                charVal = srcChar-'a'+10;
            }
            else if(srcChar >= 'A' && srcChar<= 'F') {
                charVal = srcChar-'A'+10;
            }
            else
                throw new RuntimeException("string: " + src + " contains non-hex char");


            if((index-startIndex)%2 == 0)
                byteHN = charVal;
            else
                byteLN = charVal;


            if((index-startIndex)%2== 1)
                destBytes[(index-startIndex)/2] = ( (byte) (byteHN * 16 + byteLN));
            else  if(index == src.length()-1)
                destBytes[(index-startIndex)/2] = ( (byte) (byteHN * 16));
        }

        return destBytes;

    }

}

