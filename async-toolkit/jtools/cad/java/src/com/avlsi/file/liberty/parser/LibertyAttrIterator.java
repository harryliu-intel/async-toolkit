package com.avlsi.file.liberty.parser;

import java.util.Iterator;
import java.util.NoSuchElementException;

import static com.avlsi.file.liberty.parser.libertyConstants.SI2DR_NO_ERROR;

public class LibertyAttrIterator extends LibertyIterator<LibertyAttr> {
    LibertyAttrIterator(final SWIGTYPE_p_void iter) {
        super(iter);
    }

    @Override
    protected LibertyAttr getNext() {
        final int[] err = { SI2DR_NO_ERROR };
        final si2ObjectIdT obj = liberty.si2drIterNextAttr(iter, err);
        LibertyUtil.checkErr("si2drIterNextAttr", err);
        return LibertyAttr.makeAttr(obj);
    }
}
