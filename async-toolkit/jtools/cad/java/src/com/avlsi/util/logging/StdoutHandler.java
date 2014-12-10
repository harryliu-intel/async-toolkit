package com.avlsi.util.logging;

public class StdoutHandler extends ConsoleHandler {
    public StdoutHandler() {
        super(System.out);
    }
}
