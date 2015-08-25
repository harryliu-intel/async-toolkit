package com.avlsi.file.liberty.parser;

import static com.avlsi.file.liberty.parser.libertyConstants.*;

public class LibertyUtil {
    static void checkErr(final String func, final int[] err) {
        if (err[0] != SI2DR_NO_ERROR) {
            throw new LibertyParserException(err[0], func);
        }
    }
}
