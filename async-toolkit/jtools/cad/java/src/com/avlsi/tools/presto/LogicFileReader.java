/*
 * Copyright 2003-2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto;

import java.math.BigInteger;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.Reader;
import com.avlsi.util.text.StringUtil;

public class LogicFileReader {
    private static final String SEP = "=>";
    private static final BigInteger MAX_INT =
        new BigInteger(Integer.toString(Integer.MAX_VALUE));

    public static class TruthTableFormatException extends IOException {
	TruthTableFormatException(String msg) {
	    super(msg);
	}
    }

    private static class LineParser {
	private int lineSize = -1;
	private int sepPos = -1;
	private final LineNumberReader lnr;
	private String[] lastLine;

	LineParser(Reader r) throws IOException {
	    lnr = new LineNumberReader(r);
	}

	boolean parseLine() throws IOException {
	    String s = lnr.readLine();
	    if (s == null)
		return false;
	    lastLine = StringUtil.split(s.trim(), ' ');
	    if (lineSize == -1) {
		lineSize = lastLine.length;
	    } else if (lineSize != lastLine.length) {
		bad("line has too " +
		    (lastLine.length < lineSize ? "few" : "many") +
		    " items");
	    }
	    int pos = -1;
	    for (int i = 0; i < lastLine.length; i++)
		if (lastLine[i].equals(SEP)) {
		    if (pos == -1)
			pos = i;
		    else
			bad("more than one " + SEP + " encountered");
		}
	    if (pos == -1)
		bad("no " + SEP + " encountered");
	    if (sepPos == -1)
		sepPos = pos;
	    else if (sepPos != pos)
		bad(SEP + " in the wrong place");
	    return true;
	}

	String[] getInputs() {
	    String[] result = new String[sepPos];
	    System.arraycopy(lastLine, 0, result, 0, sepPos);
	    return result;
	}

	String[] getOutputs() {
	    String[] result = new String[lineSize - (sepPos + 1)];
	    System.arraycopy(lastLine, sepPos + 1, result, 0, result.length);
	    return result;
	}

	void bad(String msg) throws TruthTableFormatException {
	    throw new TruthTableFormatException("line " + lnr.getLineNumber() +
						": " + msg);
	}
    }

    public static TruthTable[] readLogicFile(Reader r) throws IOException {
	LineParser lp = new LineParser(r);
	if (!lp.parseLine())
	    throw new TruthTableFormatException("Empty file!");
	String[] inStrs = lp.getInputs();
	String[] outStrs = lp.getOutputs();

	if (inStrs.length == 0)
	    throw new TruthTableFormatException("no inputs!");

	if (outStrs.length == 0)
	    throw new TruthTableFormatException("no outputs!");

	BigInteger bigTableSize = BigInteger.ONE;
	ChannelName[] ins = new ChannelName[inStrs.length];
	for (int i = 0; i < ins.length; i++) {
	    ins[i] = new ChannelNameImpl(inStrs[i]);
            int of = ins[i].get1of();
            assert (of > 0) : inStrs[i];
            BigInteger bigOf = new BigInteger(Integer.toString(of));
	    bigTableSize = bigTableSize.multiply(bigOf);
	}

        if (bigTableSize.compareTo(MAX_INT) > 0) {
            System.err.println("Error: The truth table for your cell " +
                               "would require " + bigTableSize);
            System.err.println("entries.  That's more than the amount of " +
                               "memory in a 32-bit computer.");
            System.err.println("I'm not even going to try to do that.  " +
                               "Tough luck for you.");
            System.exit(32);
        }

        int tableSize = bigTableSize.intValue();
	
	ChannelName[] outs = new ChannelName[outStrs.length];
	for (int i = 0; i < outs.length; i++)
	    outs[i] = new ChannelNameImpl(outStrs[i]);
	
	byte[][] table = new byte[outs.length][tableSize];
	boolean[][] ack = new boolean[ins.length][tableSize];

        if (outs.length * tableSize > 10000) {
            System.err.println("Warning: " + outs.length +
                               " truth tables with " + tableSize +
                               " entries per table.");
            System.err.println("This could take a long time, or even " +
                               "fail due to lack of space.");
        }

        boolean[] acks = new boolean[inStrs.length];

        assert (acks.length == ins.length);
        assert (tableSize > 0);

	while (lp.parseLine()) {
	    inStrs = lp.getInputs();
	    outStrs = lp.getOutputs();
	    
	    int pos = 0;

	    for (int i = 0; i < inStrs.length; i++) {
                String s = inStrs[inStrs.length - (i+1)];
                if (s.endsWith("n")) {
                    acks[inStrs.length - (i+1)] = false;
                    s = s.substring(0, s.length() - 1);
                } else {
                    acks[inStrs.length - (i+1)] = true;
                }
		int x = Integer.parseInt(s);
		pos *= ins[inStrs.length - (i+1)].get1of();
		pos += x;
	    }

            for (int i = 0; i < acks.length; i++)
                ack[i][pos] = acks[i];

	    for (int i = 0; i < outStrs.length; i++) {
		int x = -1;
		if (!outStrs[i].equals("-"))
		    x = Integer.parseInt(outStrs[i]);
		table[i][pos] = (byte) x;
	    }
	}

	TruthTable[] result = new TruthTable[outs.length];
	for (int i = 0; i < outs.length; i++)
	    result[i] = new TruthTableImpl(ins, outs[i], table[i], ack);

	return result;
    }
}
