package com.avlsi.file.liberty.parser;

import java.util.Iterator;

public class LibertyParser implements libertyConstants {
    static {
        System.loadLibrary("Liberty");
    }

    public static class LibertyParserException extends RuntimeException {
        final int errorCode;
        public LibertyParserException(final int errorCode, final String message) {
            super(message);
            this.errorCode = errorCode;
        }
    }

    public LibertyParser() {
        final int[] err = { SI2DR_NO_ERROR };
        liberty.si2drPIInit(err);
        LibertyUtil.checkErr("si2drPIInit", err);
    }

    public LibertyParser(final String libName) {
        this();
        final int[] err = { SI2DR_NO_ERROR };
        liberty.si2drReadLibertyFile(libName, err);
        LibertyUtil.checkErr("si2drReadLibertyFile", err);
    }

    public Iterator<LibertyGroup> getGroups() {
        final int[] err = { SI2DR_NO_ERROR };
        final SWIGTYPE_p_void groups = liberty.si2drPIGetGroups(err);
        LibertyUtil.checkErr("si2drPIGetGroups", err);
        return new LibertyGroupIterator(groups);
    }

    public void writeLibertyFile(final String filename,
                                 final LibertyGroup group) {
        final int[] err = { SI2DR_NO_ERROR };
        liberty.si2drWriteLibertyFile(filename, group.getHandle(), err);
        LibertyUtil.checkErr("si2drWriteLibertyFile", err);
    }
}
