/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.util.text;

import java.util.StringTokenizer;
import java.math.BigInteger;

/****************************************************************************
 * Bit Field Formatter utility class.
 *
 * Formats integers in a configurable manner, useful for presenting bit
 * fields that might be relevant for debugging digital simulation output.
 *
 * The format is defined by the String passed to the constructor.  It
 * specifies an arbitrary number of bit field ranges, plus a format (radix)
 * to represent each field.  The syntax is the following:
 *
 * <pre> max1:min1[format1],max2:min2[format2],...,maxN:minN[formatN]
 * </pre>
 *
 * Valid formats are "hex", "dec", and "bin".
 * Either ':min' or '[format]' may be omitted.  The former defaults to max,
 * the latter to "hex".
 *
 * When a BigInteger is passed to formatNumber, its bit fields will be
 * extracted and formatted into a String.  Example:
 *
 * <pre>
 *   Format:              "7:5[bin],4,3:0[hex]"
 *   BigInteger input:    255
 *   formatNumber output: "111 1 f"
 * </pre>
 *
 * @author Mike Davies
 ****************************************************************************/

public class BitFieldFormatter {

    public class InvalidFormatException extends Exception {
        public InvalidFormatException(String s) { super(s); }
        public InvalidFormatException(String s, Throwable cause) {
            super(s, cause);
        }
    }

    private static final byte BIN = 0;
    private static final byte DEC = 1;
    private static final byte HEX = 2;

    /*****************************************************
     * Helper class to hold "max:min[format]" range atom
     *****************************************************/

    private class RangeFormat {

        public int max;
        public int min;
        public byte format;

        /*************************************************
         * Parses string of the form "max:min[format]"
         *************************************************/
        public RangeFormat(String s) throws InvalidFormatException {
            int colon = s.indexOf(':');
            int lbracket = s.indexOf('[');
            if (lbracket < 0) {
                lbracket = s.length();
                format = HEX;
            }
            else {
                if (s.indexOf(']') != s.length()-1)
                    throw new InvalidFormatException("Garbled range: "+s);
                String fstr = s.substring(lbracket+1,s.length()-1);
                if (fstr.equals("bin")) format = BIN;
                else if (fstr.equals("dec")) format = DEC;
                else if (fstr.equals("hex")) format = HEX;
                else throw new InvalidFormatException("Unrecognized format: "+fstr);
            }
            if (colon >= 0) {
                if (s.lastIndexOf(':') != colon)
                    throw new InvalidFormatException("Garbled range: "+s);
                try {
                    max = Integer.parseInt(s.substring(0,colon));
                    min = Integer.parseInt(s.substring(colon+1,lbracket));
                }
                catch (NumberFormatException err) {
                    throw new InvalidFormatException("Garbled range: "+s, err);
                }
                if (max < min || max < 0 || min < 0)
                    throw new InvalidFormatException("Invalid range: "+
                                                     s.substring(0,lbracket));
            }
            else {
                try {
                    max = Integer.parseInt(s.substring(0,lbracket));
                    min = max;
                }
                catch (NumberFormatException err) {
                    throw (InvalidFormatException)
                        new InvalidFormatException("Invalid range: "+
                                                     s.substring(0,lbracket));
                }
                if (max < 0)
                    throw new InvalidFormatException("Invalid index: "+max);
            }
        }

        /******************************************************************
         * Formats a string from the range-specified bits of a BigInteger
         ******************************************************************/
        public String formatNumber(BigInteger x) {
            BigInteger sx = BigInteger.ZERO;
            String s = "";
            for (int i=min; i<=max; i++)
                if (x.testBit(i)) sx = sx.setBit(i-min);
            if (format==BIN) s = sx.toString(2);
            else if (format==DEC) s = sx.toString(10);
            else if (format==HEX) {
                int len = (max-min+1)/4+((max-min+1)%4==0?0:1);
                s = NumberFormatter.toHexString(sx,len);
            }
            return s;
        }
    }

    private int nbits;
    private RangeFormat[] ranges;

    /*************************************************************************
     * Constructor.
     * Parses a string of the form "maxN:minN[format1],maxM:minM[format2],..."
     *************************************************************************/

    public BitFieldFormatter(String format_string) throws 
                            InvalidFormatException {

        StringTokenizer st = new StringTokenizer(format_string,",");
        int nr = st.countTokens();
        ranges = new RangeFormat[nr];
        for (int i=0; i<nr; i++)
            ranges[i] = new RangeFormat(st.nextToken());
    }

    /**************************************
     * Formats a string from a BigInteger 
     **************************************/

    public String formatNumber(BigInteger x) {
        String s = "";
        for (int i=0; i<ranges.length; i++)
            s += ranges[i].formatNumber(x)+" ";
        return s;
    }
}
