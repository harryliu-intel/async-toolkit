package com.avlsi.file.liberty.parser;

import static com.avlsi.file.liberty.parser.libertyConstants.*;

public class LibertyParser {
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

    private final int[] err = { -1 };
    private void clearErr() {
        err[0] = SI2DR_NO_ERROR;
    }
    private void checkErr(final String func) {
        if (err[0] != SI2DR_NO_ERROR) {
            throw new LibertyParserException(err[0], func);
        }
    }

    public LibertyParser() {
        clearErr();
        liberty.si2drPIInit(err);
        checkErr("si2drPIInit");
    }

    public LibertyParser(final String libName) {
        this();
        clearErr();
        liberty.si2drReadLibertyFile(libName, err);
        checkErr("si2drReadLibertyFile");
    }
}
