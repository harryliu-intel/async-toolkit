package com.avlsi.util.logging;

import java.io.InputStream;
import java.util.logging.LogManager;

public class Configurator {
    private final static String CONFIG =
        "com/avlsi/util/logging/logging.properties";
    public Configurator() {
        final LogManager logManager = LogManager.getLogManager();
        final InputStream ins = ClassLoader.getSystemResourceAsStream(CONFIG);
        if (ins != null) {
            try {
                logManager.readConfiguration(ins);
            } catch (Exception e) {
                // silently ignore exceptions
            }
        }
    }
}
