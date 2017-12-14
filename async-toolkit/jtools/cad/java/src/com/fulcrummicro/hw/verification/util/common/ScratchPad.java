package com.fulcrummicro.hw.verification.util.common;

import java.util.ArrayList;

public class ScratchPad {

    protected final ArrayList<StringBuilder> pad;

    protected final String spacing;

    protected int lineNumber;

    protected int offset;

    public ScratchPad(String spacing) {
        this.pad = new ArrayList<StringBuilder>();
        this.spacing = spacing;
        this.lineNumber = 0;
        this.offset = 0;
    }

    public void append(String line) {
        StringBuilder buffer;

        if (this.lineNumber >= this.pad.size()) {
            this.pad.add(new StringBuilder());
        }
        buffer = this.pad.get(this.lineNumber);
        while (buffer.length() < this.offset) {
            buffer.append(" ");
        }
        if (line != null) {
            buffer.append(line);
        }
        buffer.append(this.spacing);
        this.lineNumber += 1;
    }

    public void rewind() {
        for (StringBuilder buffer : this.pad) {
            this.offset = Math.max(this.offset, buffer.length());
        }
        this.lineNumber = 0;
    }

    @Override
    public String toString() {
        StringBuilder page = new StringBuilder();

        for (StringBuilder buffer : this.pad) {
            page.append(buffer);
            page.append("\n");
        }
        return page.toString();
    }
}