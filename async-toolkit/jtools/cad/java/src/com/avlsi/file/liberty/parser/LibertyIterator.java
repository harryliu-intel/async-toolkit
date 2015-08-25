package com.avlsi.file.liberty.parser;

import java.util.Iterator;
import java.util.NoSuchElementException;

import static com.avlsi.file.liberty.parser.libertyConstants.SI2DR_NO_ERROR;

abstract class LibertyIterator<T extends LibertyObject> implements Iterator<T> {
    protected final SWIGTYPE_p_void iter;

    private T next = null;

    protected LibertyIterator(final SWIGTYPE_p_void iter) {
        this.iter = iter;
    }

    protected abstract T getNext();

    @Override
    public boolean hasNext() {
        if (next == null) next = getNext();
        return !next.isNull();
    }

    @Override
    public T next() {
        if (hasNext()) {
            final T obj = next;
            next = null;
            return obj;
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
