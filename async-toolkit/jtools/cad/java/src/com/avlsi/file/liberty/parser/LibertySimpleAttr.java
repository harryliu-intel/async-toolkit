package com.avlsi.file.liberty.parser;

import static com.avlsi.file.liberty.parser.libertyConstants.SI2DR_BOOLEAN;
import static com.avlsi.file.liberty.parser.libertyConstants.SI2DR_EXPR;
import static com.avlsi.file.liberty.parser.libertyConstants.SI2DR_FLOAT64;
import static com.avlsi.file.liberty.parser.libertyConstants.SI2DR_INT32;
import static com.avlsi.file.liberty.parser.libertyConstants.SI2DR_NO_ERROR;
import static com.avlsi.file.liberty.parser.libertyConstants.SI2DR_STRING;
import static com.avlsi.file.liberty.parser.libertyConstants.SI2DR_UNDEFINED_VALUETYPE;

public class LibertySimpleAttr extends LibertyAttr {
    LibertySimpleAttr(final si2ObjectIdT obj) {
        super(obj);
    }

    @Override
    public LibertySimpleAttr getSimpleAttr() {
        return this;
    }

    public int getValueType() {
        final int[] err = { SI2DR_NO_ERROR };
        final int result = liberty.si2drSimpleAttrGetValueType(obj, err);
        LibertyUtil.checkErr("si2drSimpleAttrGetValueType", err);
        return result;
    }

    public int getInt32Value() {
        final int[] err = { SI2DR_NO_ERROR };
        final int result = liberty.si2drSimpleAttrGetInt32Value(obj, err);
        LibertyUtil.checkErr("si2drSimpleAttrGetInt32Value", err);
        return result;
    }

    public void setInt32Value(final int val) {
        final int[] err = { SI2DR_NO_ERROR };
        liberty.si2drSimpleAttrSetInt32Value(obj, val, err);
        LibertyUtil.checkErr("si2drSimpleAttrSetInt32Value", err);
    }

    public double getFloat64Value() {
        final int[] err = { SI2DR_NO_ERROR };
        final double result = liberty.si2drSimpleAttrGetFloat64Value(obj, err);
        LibertyUtil.checkErr("si2drSimpleAttrGetFloat64Value", err);
        return result;
    }

    public void setFloat64Value(final double val) {
        final int[] err = { SI2DR_NO_ERROR };
        liberty.si2drSimpleAttrSetFloat64Value(obj, val, err);
        LibertyUtil.checkErr("si2drSimpleAttrSetFloat64Value", err);
    }

    public String getStringValue() {
        final int[] err = { SI2DR_NO_ERROR };
        final String result = liberty.si2drSimpleAttrGetStringValue(obj, err);
        LibertyUtil.checkErr("si2drSimpleAttrGetStringValue", err);
        return result;
    }

    public void setStringValue(final String val) {
        final int[] err = { SI2DR_NO_ERROR };
        liberty.si2drSimpleAttrSetStringValue(obj, val, err);
        LibertyUtil.checkErr("si2drSimpleAttrSetStringValue", err);
    }

    public boolean getBooleanValue() {
        final int[] err = { SI2DR_NO_ERROR };
        final int result = liberty.si2drSimpleAttrGetBooleanValue(obj, err);
        LibertyUtil.checkErr("si2drSimpleAttrGetBooleanValue", err);
        return result != 0;
    }

    public void setBooleanValue(final boolean val) {
        final int[] err = { SI2DR_NO_ERROR };
        liberty.si2drSimpleAttrSetBooleanValue(obj, val ? 1 : 0, err);
        LibertyUtil.checkErr("si2drSimpleAttrSetBooleanValue", err);
    }

    public Object getValue() {
        switch (getValueType()) {
          case SI2DR_UNDEFINED_VALUETYPE:
            return null;
          case SI2DR_INT32:
            return Integer.valueOf(getInt32Value());
          case SI2DR_STRING:
            return getStringValue();
          case SI2DR_FLOAT64:
            return Double.valueOf(getFloat64Value());
          case SI2DR_BOOLEAN:
            return Boolean.valueOf(getBooleanValue());
          case SI2DR_EXPR:
            throw new AssertionError("expr unsupported");
          default:
            assert false : "Invalid simple attribute type";
            return null;
        }
    }

    public void setValue(final Object o) {
        if (o instanceof Integer) {
            setInt32Value(((Integer) o).intValue());
        } else if (o instanceof String) {
            setStringValue((String) o);
        } else if (o instanceof Double) {
            setFloat64Value(((Double) o).doubleValue());
        } else if (o instanceof Boolean) {
            setBooleanValue(((Boolean) o).booleanValue());
        } else if (o instanceof LibertyExpr) {
            throw new AssertionError("expr unsupported");
        } else {
            throw new IllegalArgumentException("Invalid simple attribute value");
        }
    }
}
