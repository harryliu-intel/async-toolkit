package com.avlsi.util.logging;

import java.io.OutputStream;
import java.util.logging.Level;
import java.util.logging.LogManager;
import java.util.logging.LogRecord;
import java.util.logging.StreamHandler;

public class ConsoleHandler extends StreamHandler {
    private int max;

    public ConsoleHandler(OutputStream output) {
        LogManager manager = LogManager.getLogManager();
        String cname = getClass().getName();

        max = Level.OFF.intValue();
        String maxLevel = manager.getProperty(cname + ".level_max");
        if (maxLevel != null) {
            try {
                max = Level.parse(maxLevel).intValue();
            } catch (IllegalArgumentException e) {
            }
        }
        setOutputStream(output);
    }

    public boolean isLoggable(LogRecord record) {
        return super.isLoggable(record) && max > record.getLevel().intValue();
    }

    /**
     * Publish a <tt>LogRecord</tt>.
     * <p>
     * The logging request was made initially to a <tt>Logger</tt> object,
     * which initialized the <tt>LogRecord</tt> and forwarded it here.
     * <p>
     * @param  record  description of the log event. A null record is
     *                 silently ignored and is not published
     */
    public void publish(LogRecord record) {
        super.publish(record);        
        flush();
    }

    /**
     * Override <tt>StreamHandler.close</tt> to do a flush but not
     * to close the output stream.  That is, we do <b>not</b>
     * close <tt>System.err</tt>.
     */
    public void close() {
        flush();
    }
}
