package com.avlsi.tools.jauto;

public class SubprocessFailedException extends Exception {
    public SubprocessFailedException(String why) {
        super(why);
    }
    public SubprocessFailedException(String why, Exception cause) {
        super(why, cause);
    }
}
