package com.avlsi.file.liberty.parser;

import java.util.Iterator;
import java.util.NoSuchElementException;

import static com.avlsi.file.liberty.parser.libertyConstants.SI2DR_NO_ERROR;

public class LibertyDefineIterator extends LibertyIterator<LibertyDefine> {
    LibertyDefineIterator(final SWIGTYPE_p_void iter) {
        super(iter);
    }

    @Override
    protected LibertyDefine getNext() {
        final int[] err = { SI2DR_NO_ERROR };
        final si2ObjectIdT obj = liberty.si2drIterNextDefine(iter, err);
        LibertyUtil.checkErr("si2drIterNextDefine", err);
        return new LibertyDefine(obj);
    }
}
