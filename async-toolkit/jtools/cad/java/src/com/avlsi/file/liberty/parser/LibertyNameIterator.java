package com.avlsi.file.liberty.parser;

import java.util.Iterator;
import java.util.NoSuchElementException;

import static com.avlsi.file.liberty.parser.libertyConstants.SI2DR_NO_ERROR;

public class LibertyNameIterator implements Iterator<String> {
    private final SWIGTYPE_p_void iter;
    private boolean gotNext = false;
    private String next = null;

    LibertyNameIterator(final SWIGTYPE_p_void iter) {
        this.iter = iter;
    }

    @Override
    public boolean hasNext() {
        if (!gotNext) {
            final int[] err = { SI2DR_NO_ERROR };
            next = liberty.si2drIterNextName(iter, err);
            LibertyUtil.checkErr("si2drIterNextName", err);
            gotNext = true;
        }
        return next != null;
    }

    @Override
    public String next() {
        if (hasNext()) {
            return next;
        } else {
            throw new NoSuchElementException();
        }
    }

    @Override
    public void finalize() {
        final int[] err = { SI2DR_NO_ERROR };
        liberty.si2drIterQuit(iter, err);
        LibertyUtil.checkErr("si2drIterQuit", err);
    }
}
