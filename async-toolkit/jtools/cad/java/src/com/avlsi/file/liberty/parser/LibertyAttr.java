package com.avlsi.file.liberty.parser;

import static com.avlsi.file.liberty.parser.libertyConstants.SI2DR_COMPLEX;
import static com.avlsi.file.liberty.parser.libertyConstants.SI2DR_NO_ERROR;
import static com.avlsi.file.liberty.parser.libertyConstants.SI2DR_SIMPLE;

public class LibertyAttr extends LibertyObject {
    LibertyAttr(final si2ObjectIdT obj) {
        super(obj);
    }

    public int getAttrType() {
        return getAttrType(obj);
    }

    public String getName() {
        final int[] err = { SI2DR_NO_ERROR };
        final String result = liberty.si2drAttrGetName(obj, err);
        LibertyUtil.checkErr("si2drAttrGetName", err);
        return result;
    }

    public LibertySimpleAttr getSimpleAttr() {
        return null;
    }

    public LibertyComplexAttr getComplexAttr() {
        return null;
    }

    static int getAttrType(si2ObjectIdT obj) {
        final int[] err = { SI2DR_NO_ERROR };
        final int result = liberty.si2drAttrGetAttrType(obj, err);
        LibertyUtil.checkErr("si2drAttrGetAttrType", err);
        return result;
    }

    static LibertyAttr makeAttr(si2ObjectIdT obj) {
        if (LibertyObject.isNull(obj)) {
            return new LibertyAttr(obj);
        } else {
            switch (getAttrType(obj)) {
              case SI2DR_SIMPLE:
                return new LibertySimpleAttr(obj);
              case SI2DR_COMPLEX:
                return new LibertyComplexAttr(obj);
              default:
                assert false : "Invalid attribute type";
                return null;
            }
        }
    }
}
