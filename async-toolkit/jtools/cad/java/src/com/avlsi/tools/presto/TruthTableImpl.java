/*
 * Copyright 2003-2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto;

import java.util.zip.CRC32;

public class TruthTableImpl implements TruthTable {
    private final ChannelName[] inputs;
    private final ChannelName output;
    private final byte[] table;
    private final boolean[][] ack;
    private CRC32 hc = null;

    public ChannelName[] getInputs() {
        return inputs;
    }

    public ChannelName getOutput() {
        return output;
    }

    public byte[] getTable() {
        return table;
    }

    public boolean[] getInputAcknowledge(int input) {
        return ack[input];
    }

    public TruthTableImpl(ChannelName[] inputs, ChannelName output,
                          byte[] table, boolean[][] ack) {
        this.inputs = inputs;
        this.output = output;
        this.table = table;
        this.ack = ack;
    }

    public String toString() {
        StringBuffer buf = new StringBuffer();
        for (int i = 0; i < inputs.length; i++) {
            buf.append(inputs[i].toString());
            buf.append(' ');
        }
        buf.append("=> ");
        buf.append(output.toString());
        buf.append('\n');

        int[] weights = new int[inputs.length];
        int weight = 1;

        for (int i = 0; i < inputs.length; i++) {
            weights[i] = weight;
            weight *= inputs[i].get1of();
        }

        for (int i = 0; i < table.length; i++) {
            for (int j = 0; j < inputs.length; j++) {
                int val = (i / weights[j]) % inputs[j].get1of();
                buf.append(Integer.toString(val));
                if (!ack[j][i])
                    buf.append('n');
                buf.append(' ');
            }
            buf.append("=> ");
            if (table[i] == -1)
                buf.append('-');
            else
                buf.append(Integer.toString(table[i]));
            buf.append('\n');
        }

        return buf.toString();
    }

    /**
     * Note: hashCode() and equals() only look at table; they don't
     * look at ack or the ChannelNames.
     */
    public int hashCode() {
        if (hc == null) {
            hc = new CRC32();
            hc.update(table);
        }
        return (int) hc.getValue();
    }

    public boolean equals(Object o) {
        boolean eq = (o instanceof TruthTableImpl);
        if (eq) {
            TruthTableImpl that = (TruthTableImpl) o;
            eq = (table.length == that.table.length);
            for (int i = 0; eq && i < table.length; i++)
                eq = (table[i] == that.table[i]);
        }
        return eq;
    }
}
