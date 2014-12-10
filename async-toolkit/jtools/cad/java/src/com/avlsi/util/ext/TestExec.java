package com.avlsi.util.ext;

import java.io.*;

public class TestExec {
    public static void main(String args[]) throws Exception {
        StringBufferInputStream hi = new StringBufferInputStream("hi\n");
        int res = Exec.exec(new String[]{"bash","-c","echo hi >&2; cat; echo bye; echo $foo"},
                            null,
                            null,
                            hi,
                            System.out,
                            System.err);
        System.out.println("return value: "+res);

        res = Exec.exec("echo  hello",
                            System.out,
                            System.err);
        System.out.println("return value: "+res);
    }
}
