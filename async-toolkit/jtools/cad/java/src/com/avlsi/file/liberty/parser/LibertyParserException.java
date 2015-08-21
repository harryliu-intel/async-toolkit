package com.avlsi.file.liberty.parser;

public class LibertyParserException extends RuntimeException {
    public final int errorCode;
    public LibertyParserException(final int errorCode, final String message) {
        super(message);
        this.errorCode = errorCode;
    }
}
