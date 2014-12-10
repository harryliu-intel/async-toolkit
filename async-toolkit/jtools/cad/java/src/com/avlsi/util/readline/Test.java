/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.readline;

public class Test {
	public static void main(String[] args) {
        System.out.println("Java System library path is: ");
        System.out.println(System.getProperties().get("java.library.path"));
        String abc = null;
        while (abc==null || !abc.equals("quit")) {
            abc = Readline.readline("test> ");
            System.err.println("Read in: " + abc);
        }
	}
}
