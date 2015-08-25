package com.avlsi.file.liberty.parser;

import java.util.Iterator;

import static com.avlsi.file.liberty.parser.libertyConstants.SI2DR_NO_ERROR;
import static com.avlsi.file.liberty.parser.libertyConstants.SI2DR_OBJECT_NOT_FOUND;

public class LibertyGroup extends LibertyObject {
    LibertyGroup(final si2ObjectIdT obj) {
        super(obj);
    }

    public LibertyDefine createDefine(final String name,
                                      final String allowedGroupName,
                                      final int valtype) {
        final int[] err = { SI2DR_NO_ERROR };
        final si2ObjectIdT result = liberty.si2drGroupCreateDefine(
                obj, name, allowedGroupName, valtype, err);
        LibertyUtil.checkErr("si2drGroupCreateDefine", err);
        return new LibertyDefine(result);
    }

    public LibertyAttr createAttr(final String name, final int type) {
        final int[] err = { SI2DR_NO_ERROR };
        final si2ObjectIdT result = liberty.si2drGroupCreateAttr(
                obj, name, type, err);
        LibertyUtil.checkErr("si2drGroupCreateAttr", err);
        return LibertyAttr.makeAttr(result);
    }

    public Iterator<LibertyDefine> getDefines() {
        final int[] err = { SI2DR_NO_ERROR };
        final SWIGTYPE_p_void iter = liberty.si2drGroupGetDefines(obj, err);
        LibertyUtil.checkErr("si2drGroupGetDefines", err);
        return new LibertyDefineIterator(iter);
    }

    public Iterator<LibertyGroup> getGroups() {
        final int[] err = { SI2DR_NO_ERROR };
        final SWIGTYPE_p_void iter = liberty.si2drGroupGetGroups(obj, err);
        LibertyUtil.checkErr("si2drGroupGetGroups", err);
        return new LibertyGroupIterator(iter);
    }

    public Iterator<String> getNames() {
        final int[] err = { SI2DR_NO_ERROR };
        final SWIGTYPE_p_void iter = liberty.si2drGroupGetNames(obj, err);
        LibertyUtil.checkErr("si2drGroupGetNames", err);
        return new LibertyNameIterator(iter);
    }

    public String getGroupType() {
        final int[] err = { SI2DR_NO_ERROR };
        final String result = liberty.si2drGroupGetGroupType(obj, err);
        LibertyUtil.checkErr("si2drGroupGetGroupType", err);
        return result;
    }

    public Iterator<LibertyAttr> getAttrs() {
        final int[] err = { SI2DR_NO_ERROR };
        final SWIGTYPE_p_void iter = liberty.si2drGroupGetAttrs(obj, err);
        LibertyUtil.checkErr("si2drGroupGetAttrs", err);
        return new LibertyAttrIterator(iter);
    }

    public LibertyAttr findAttrByName(final String name) {
        final int[] err = { SI2DR_NO_ERROR };
        final si2ObjectIdT result =
            liberty.si2drGroupFindAttrByName(obj, name, err);
        if (err[0] == SI2DR_OBJECT_NOT_FOUND) {
            return null;
        } else {
            LibertyUtil.checkErr("si2drGroupFindAttrByName", err);
            return LibertyAttr.makeAttr(result);
        }
    }

    public void addName(final String name) {
        final int[] err = { SI2DR_NO_ERROR };
        liberty.si2drGroupAddName(obj, name, err);
        LibertyUtil.checkErr("si2drGroupAddName", err);
    }

    public void deleteName(final String name) {
        final int[] err = { SI2DR_NO_ERROR };
        liberty.si2drGroupDeleteName(obj, name, err);
        LibertyUtil.checkErr("si2drGroupDeleteName", err);
    }
}
