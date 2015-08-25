package com.avlsi.file.liberty.parser;

import java.util.Iterator;
import java.util.NoSuchElementException;

import static com.avlsi.file.liberty.parser.libertyConstants.SI2DR_NO_ERROR;

public class LibertyGroupIterator extends LibertyIterator<LibertyGroup> {
    LibertyGroupIterator(final SWIGTYPE_p_void iter) {
        super(iter);
    }

    @Override
    protected LibertyGroup getNext() {
        final int[] err = { SI2DR_NO_ERROR };
        final si2ObjectIdT obj = liberty.si2drIterNextGroup(iter, err);
        LibertyUtil.checkErr("si2drIterNextGroup", err);
        return new LibertyGroup(obj);
    }
}
