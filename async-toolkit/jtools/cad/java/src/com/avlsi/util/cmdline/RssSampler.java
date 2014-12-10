/*
 * Copyright 2006 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

// vim:sw=4:expandtab:ai:cin:ts=4

package com.avlsi.util.cmdline;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * This is a background thread which periodically samples the proc
 * filesystem to get the current amount of resident memory for this
 * process, and keeps a maximum.  We are basically attempting to
 * emulate VmHWM for kernels that don't support it, such as the
 * FC3 kernel and the RHEL4 kernel.  See bug 7138 comment 14 and
 * comment 19.
 *
 * If for any reason we have trouble accessing the proc filesystem
 * (for example, if we are not running on Linux), then the thread will
 * simply terminate.
 */
public class RssSampler implements Runnable {
    private final long period; // in milliseconds
    private long hwm_kb = 0;

    /**
     * @param period   sampling period, in milliseconds
     */
    public RssSampler(long period) {
        this.period = period;
    }

    /** Returns high water mark, in kilobytes. */
    public synchronized long getHWM() {
        return hwm_kb;
    }

    private synchronized void updateHWM(long rss_kb) {
        if (rss_kb > hwm_kb)
            hwm_kb = rss_kb;
    }

    /**
     * Creates a new thread to run this RssSampler, starts it, and
     * returns.
     */
    public void start() {
        Thread t = new Thread(null, this, "RssSampler", 64 * 1024);
        t.setDaemon(true);
        t.setPriority(Thread.MAX_PRIORITY);
        t.start();
    }

    public void run() {
        try {
            Pattern p = Pattern.compile("VmRSS:\\s+(\\d+)\\s+kB");
            while (true) {
                long rss_kb = 0;
                BufferedReader br =
                    new BufferedReader(new FileReader("/proc/self/status"));
                String s;
                while ((s = br.readLine()) != null) {
                    Matcher m = p.matcher(s);
                    if (m.matches())
                        rss_kb = Long.parseLong(m.group(1));
                }
                br.close();
                updateHWM(rss_kb);
                Thread.sleep(period);
            }
        } catch (Exception e) {
            // if anything goes wrong, we just fall thru and return
        }
    }
}
