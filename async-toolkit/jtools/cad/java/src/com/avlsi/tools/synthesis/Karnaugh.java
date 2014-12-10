/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.synthesis;

import java.io.*;
import java.util.*;

import com.avlsi.util.container.MultiSet;
import com.avlsi.util.text.NumberFormatter;

public class Karnaugh {

    /** Number of inputs for logic function. **/
    static int Inputs;

    /** Number of bits in Karnaugh map. **/
    static int Bits;

    /** Mask relevant bits in Karnaugh map. **/
    static long StateMask;

    /** Constructor. **/
    Karnaugh(int Inputs) {
        this.Inputs = Inputs;
        Bits = 1<<Inputs;
        if (Inputs >= 6) StateMask = -1L;
        else             StateMask = (1L<<Bits)-1;
    }

    /**
     * Find the most canonical equivalent gate over inversions of the
     * inputs and outputs and permutations of the inputs.
     **/
    static long canonicalGate(long T) {
        long NT, BT = T;
        for (int m = 0; m < 2*Bits; m++) {
            NT = invertWithMask(T, m);
            NT = bestOverPermutations(NT);
            if (compareUnsigned(NT,BT)<0) BT = NT;
        }
        return BT;
    }

    /**
     * Find the most canonical equivalent gate over inversions of the
     * inputs and outputs only.  Permutations are not tried.
     **/
    static long bestOverInversions(long T) {
        long NT, BT = T;
        for (int m = 0; m < 2*Bits; m++) {
            NT = invertWithMask(T, m);
            if (compareUnsigned(NT,BT)<0) BT = NT;
        }
        return BT;
    }

    /**
     * Check for a better T over all possible permutations of inputs.
     **/
    static private long bestOverPermutations(long T) {
        // initialize array.
        int [] permuteArray = new int[Inputs];
        for (int i = 0; i < Inputs; i++) permuteArray[i] = i;

        // variables
        long	NT, BT = T;
        double	bound = Math.pow(Inputs, Inputs);

        // search over all permutations
        for (int p = 0; p < bound; p++) {
            int q = p;
            for (int i = 0; i < Inputs; i++) {
                permuteArray[Inputs - 1 - i] = (q % Inputs);
                q = q / Inputs;
            }

            // continue if permutation has duplicate entries
            if (hasDuplicates(permuteArray)) continue;

            // look for more canonical Karnaugh pattern over inversions
            NT = permuteInputs(T,permuteArray);
            if (compareUnsigned(NT,BT)<0) BT = NT;
        }
        return BT;
    }

    /** Invert a mask of inputs and output. **/
    static private long invertWithMask(long T, int invertMask) {
        if (((invertMask>>Inputs)&1)==1) T = ~T&StateMask; // invert output
        for (int n = 0; n < Inputs; n++) {
            if (((invertMask>>n)&1) == 1)
                T = invertInput(T, n); // invert an input
        }
        return T;
    }

    /** Return T with an input inverted. **/
    static private long invertInput(long T, int n) {
        long NT = 0;
        for (int i = 0; i < Bits; i++) {
            long j;
            j = (i ^ (1<<n)) & (Bits - 1);
            NT |= ((T>>j)&1)<<i;
        }
        return NT;
    }

    /** Check if permutation array has duplicate entries. **/
    static private boolean hasDuplicates(int [] array) {
        for (int i = 0; i < Inputs; i++) {
            for (int j = i + 1; j < Inputs; j++) {
                if (array[i] == array[j])
                    return true;	// found a duplicate entry.
            }
        }
        return false;			// no duplicate entries
    }

    /** Permute T given a permutation array. **/
    static private long permuteInputs(long T, int [] permuteArray) {
        long NT	= 0;
        for (int i = 0; i < Bits; i++) {
            int k = 0;
            for (int m = 0; m < Inputs; m++) {
                int n = permuteArray[m];
                k |= ((i>>m)&1)<<n;
            }
            NT |= ((T>>k)&1)<<i;
        }
        return NT;
    }

    /**
     * Get the lowest numbered equivalent gate assuming long types are
     * unsigned.
     **/
    static private int compareUnsigned(long a, long b) {
        int result = 0;
        if (a == b)
            result = 0;
        if ((a < 0) && (b > 0))
            result = 1;			// a > b
        if ((a > 0) && (b < 0))
            result = -1;		// a < b
        if ((a < 0) && (b < 0))
            result = (a < b) ?  1 : -1;
        if ((a > 0) && (b > 0))
            result = (a < b) ? -1 :  1;
        return result;
    }

    /**
     * Check if a gate is degenerate.  That is, the Karnaugh map is
     * the same if you invert an input.
     **/
    static boolean degenerateGate(long T) {
        for (int i = 0; i < Inputs; i++) {
            if (invertInput(T, i) == T)
                return true;
        }
        return false;
    }

    /**
     * Generate non-canonical equivalent gates by inverting output and
     * inputs.  Set of gates will be unique over permutations.
     **/
    static MultiSet getInvertedEquivalentGates(long T) {
        MultiSet noncanonical = new MultiSet();
        MultiSet permutations = new MultiSet();
        for (int m = 0; m < 2*Bits; m++) {
            long NT = invertWithMask(T, m);
            long BT = bestOverPermutations(NT);
            Long LNT = new Long(NT);
            Long LBT = new Long(BT);
            if (permutations.find(LBT)==null) {
                permutations.add(LBT);
                noncanonical.addIfUnique(LNT);
            }
        }
        return noncanonical;
    }

    /**
     * Return the inversion mask necessary to turn this into a
     * canonical gate.
     **/
    static int getInversionMask(long T) {
        long BT = bestOverInversions(T);
        for (int m = 0; m < 2*Bits; m++) {
            long NT = invertWithMask(T,m);
            if (NT==BT) return m;
        }
        return -1;
    }

    /** Turn a hex string into a Karnaugh map. **/
    static long parseGateString(String s) {
        long T;
        try {
            T=Long.parseLong(s,16);
        } catch(Exception NumberFormatException) {
            T = 0;
            System.out.println("ERROR: " + s + " is not a valid hex string");
        }
        if ((T&StateMask) != T) {
            T = 0;
            System.out.println("ERROR: Karnaugh map " + s + " exceeds number of inputs");
        }
        return T;
    }

    /**
     * Get Karnaugh pattern from table format, where all terms that
     * produce a 1 are comma separated, and each term is a string of
     * 0/1/x for each input.
     **/
    static long getKarnaughFromTable(String s) {
        StringTokenizer t = new StringTokenizer(s,",");
        long T = 0;
        while(t.hasMoreTokens()) {
            String term = (String) t.nextToken();
            if (term.length() != Inputs) {
                System.err.println("ERROR: table entry " + s + " has wrong length");
                return 0;
            }
            T |= getTableTerm(term,0,0);
        }
        return T;
    }

    /**
     * Or up the 1's in the Karnaugh map of a table term, recursing to
     * expand X's
     **/
    private static long getTableTerm(String term, int input, int loc) {
        if (input>=Inputs) return 1L<<loc;
        char c = term.charAt(input);
        long T = 0;
        if (c=='x' | c == '0') T |= getTableTerm(term,input+1,2*loc);
        if (c=='x' | c == '1') T |= getTableTerm(term,input+1,2*loc+1);
        return T;
    }

    /** Return the Karnaugh map as a hex string **/
    static String getGateString(long T) {
        return NumberFormatter.toHexString(T, Bits/4);
    }

    /** Utility to get the name of a literal **/
    private static String inputLiteral(int i, int rail, boolean image, boolean bd) {
        if      (image && rail==0) return "!A" + i;
        else if (image && rail==1) return "A" + i;
        else if (bd && rail==0)    return "~A[" + i + "]";
        else if (bd && rail==1)    return "A[" + i + "]";
        return "A[" + i + "]." + rail;
    }

    /** Convert a Karnaugh map to an infix String describing the function. **/
    static String getFuncString(long T, boolean image, boolean bd, boolean trueFunc) {
        // create list of exclusives for inputs
        List exclusives = new ArrayList();
        for (int i = 0; i < Inputs; i++) {
            List ex = new ArrayList();
            ex.add(inputLiteral(i,0,image,bd));
            ex.add(inputLiteral(i,1,image,bd));
            exclusives.add(ex);
        }

        // create lists of rules for output logic
        List rules  = new ArrayList();
        for (int i = 0; i < Bits; i++) {
            List rule = new ArrayList();
            for (int j = 0; j < Inputs; j++) {
                int k= (i>>j)&1;
                rule.add(inputLiteral(j,k,image,bd));
            }
            boolean t = ((T>>i)&1)==1;
            if (t==trueFunc) rules.add(rule);
        }

        // optimize with espresso
        rules = (new Espresso(exclusives,rules)).optimizedRules();

        // create a string
        String s = "";
        for (int i = 0; i < rules.size(); i++) {
            List rule = (List) rules.get(i);
            if (i>0) s += " | ";
            for (int j = 0; j < rule.size(); j++) {
                if (j>0) s += " & ";
                s += (String) rule.get(j);
            }
        }
        return s;
    }
}
