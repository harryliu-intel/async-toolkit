package com.avlsi.file.liberty.parser;

import static com.avlsi.file.liberty.parser.libertyConstants.SI2DR_NO_ERROR;

public class LibertyComplexAttr extends LibertyAttr {
    LibertyComplexAttr(final si2ObjectIdT obj) {
        super(obj);
    }

    @Override
    public LibertyComplexAttr getComplexAttr() {
        return this;
    }

    public void addInt32Value(final int val) {
        final int[] err = { SI2DR_NO_ERROR };
        liberty.si2drComplexAttrAddInt32Value(obj, val, err);
        LibertyUtil.checkErr("si2drComplexAttrAddInt32Value", err);
    }

    public void addStringValue(final String val) {
        final int[] err = { SI2DR_NO_ERROR };
        liberty.si2drComplexAttrAddStringValue(obj, val, err);
        LibertyUtil.checkErr("si2drComplexAttrAddStringValue", err);
    }

    public void addBooleanValue(final boolean val) {
        final int[] err = { SI2DR_NO_ERROR };
        liberty.si2drComplexAttrAddBooleanValue(obj, val ? 1 : 0, err);
        LibertyUtil.checkErr("si2drComplexAttrAddBooleanValue", err);
    }

    public void addFloat64Value(final double val) {
        final int[] err = { SI2DR_NO_ERROR };
        liberty.si2drComplexAttrAddFloat64Value(obj, val, err);
        LibertyUtil.checkErr("si2drComplexAttrAddFloat64Value", err);
    }
}
