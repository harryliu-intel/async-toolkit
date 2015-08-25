package com.avlsi.file.liberty.parser;

import static com.avlsi.file.liberty.parser.libertyConstants.SI2DR_NO_ERROR;

public class LibertyObject {
    protected final si2ObjectIdT obj;

    LibertyObject(final si2ObjectIdT obj) {
        this.obj = obj;
    }

    si2ObjectIdT getHandle() {
        return obj;
    }

    public static LibertyObject getNull() {
        final int[] err = { SI2DR_NO_ERROR };
        final si2ObjectIdT result = liberty.si2drPIGetNullId(err);
        LibertyUtil.checkErr("si2drPIGetNullId", err);
        return new LibertyObject(result);
    }

    public void delete() {
        final int[] err = { SI2DR_NO_ERROR };
        liberty.si2drObjectDelete(obj, err);
        LibertyUtil.checkErr("si2drObjectDelete", err);
    }

    public int getObjectType() {
        final int[] err = { SI2DR_NO_ERROR };
        final int result = liberty.si2drObjectGetObjectType(obj, err);
        LibertyUtil.checkErr("si2drObjectGetObjectType", err);
        return result;
    }

    public LibertyObject getOwner() {
        final int[] err = { SI2DR_NO_ERROR };
        final si2ObjectIdT result = liberty.si2drObjectGetOwner(obj, err);
        LibertyUtil.checkErr("si2drObjectGetOwner", err);
        return new LibertyObject(result);
    }

    public boolean isNull() {
        return isNull(obj);
    }

    static boolean isNull(si2ObjectIdT obj) {
        final int[] err = { SI2DR_NO_ERROR };
        final int result = liberty.si2drObjectIsNull(obj, err);
        LibertyUtil.checkErr("si2drObjectIsNull", err);
        return result != 0;
    }

    public boolean equals(LibertyObject lo) {
        final int[] err = { SI2DR_NO_ERROR };
        final int result = liberty.si2drObjectIsSame(obj, lo.obj, err);
        LibertyUtil.checkErr("si2drObjectIsSame", err);
        return result != 0;
    }

    public boolean equals(Object o) {
        if (o instanceof LibertyObject) {
            return equals((LibertyObject) o);
        } else {
            return false;
        }
    }

    public boolean isUsable() {
        final int[] err = { SI2DR_NO_ERROR };
        final int result = liberty.si2drObjectIsUsable(obj, err);
        LibertyUtil.checkErr("si2drObjectIsUsable", err);
        return result != 0;
    }

    public void setFileName(String filename) {
        final int[] err = { SI2DR_NO_ERROR };
        liberty.si2drObjectSetFileName(obj, filename, err);
        LibertyUtil.checkErr("si2drObjectSetFileName", err);
    }

    public void setLineNo(int lineno) {
        final int[] err = { SI2DR_NO_ERROR };
        liberty.si2drObjectSetLineNo(obj, lineno, err);
        LibertyUtil.checkErr("si2drObjectSetLineNo", err);
    }

    public int getLineNo() {
        final int[] err = { SI2DR_NO_ERROR };
        final int result = liberty.si2drObjectGetLineNo(obj, err);
        LibertyUtil.checkErr("si2drObjectGetLineNo", err);
        return result;
    }

    public String getFileName() {
        final int[] err = { SI2DR_NO_ERROR };
        final String result = liberty.si2drObjectGetFileName(obj, err);
        LibertyUtil.checkErr("si2drObjectGetFileName", err);
        return result;
    }
}
