/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto;

import com.avlsi.util.text.StringUtil;

public class ChannelNameImpl implements ChannelName {
    private final int numRails;
    private final String name;
    private final int[] arrayIndices;

    public int get1of() {
        return numRails;
    }

    public String getName() {
        return name;
    }

    public int[] getArrayIndices() {
        return arrayIndices;
    }

    public ChannelNameImpl(String parseMe) {
        String[] parts = StringUtil.split(parseMe, ':');
        if (parts.length != 2)
            throw new IllegalArgumentException((parts.length-1) +
                                               " colons in " + parseMe);
        numRails = Integer.parseInt(parts[1]);
        int idx = parts[0].indexOf('[');
        if (idx < 0) {
            name = parts[0];
            arrayIndices = new int[0];
        } else {
            name = parts[0].substring(0, idx);
            String[] indices =
                StringUtil.split(parts[0].substring(idx + 1,
                                                    parts[0].length() - 1),
                                 ',');
            arrayIndices = new int[indices.length];
            for (int i = 0; i < indices.length; i++)
                arrayIndices[i] = Integer.parseInt(indices[i]);
        }
    }

    public String toString(boolean wantRails) {
        StringBuffer buf = new StringBuffer(name);
        if (arrayIndices.length != 0) {
            buf.append('[');
            for (int i = 0; i < arrayIndices.length; i++) {
                if (i > 0)
                    buf.append(',');
                buf.append(Integer.toString(arrayIndices[i]));
            }
            buf.append(']');
        }
        if (wantRails) {
            buf.append(':');
            buf.append(Integer.toString(numRails));
        }
        return buf.toString();
    }

    public String toString() {
        return toString(true);
    }

    public String getNameWithIndices() {
        return toString(false);
    }
}
