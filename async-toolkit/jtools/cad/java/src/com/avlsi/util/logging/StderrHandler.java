package com.avlsi.util.logging;

public class StderrHandler extends ConsoleHandler {
    public StderrHandler() {
        super(System.err);
    }
}
