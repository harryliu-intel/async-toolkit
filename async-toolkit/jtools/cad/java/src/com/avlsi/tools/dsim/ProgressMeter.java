/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.dsim;

import java.io.PrintStream;

import EDU.oswego.cs.dl.util.concurrent.BrokenBarrierException;
import EDU.oswego.cs.dl.util.concurrent.CyclicBarrier;
import EDU.oswego.cs.dl.util.concurrent.SynchronizedBoolean;
import EDU.oswego.cs.dl.util.concurrent.TimeoutException;

/**
 * Spinner to indicate progress.  Cycles through / | - \.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
final class ProgressMeter {

    private final PrintStream writer;
    private final CyclicBarrier barrier;
    private SpinnerRunner runner = null;

    private static final char[] SPIN_CHARS = new char[]{'-', '\\', '|', '/'};

    ProgressMeter(final PrintStream writer) {
        this.writer = writer;
        this.barrier = new CyclicBarrier(2);
    }

    public void startSpinning() {
        assert runner == null;
        runner = new SpinnerRunner();
        runner.start();
    }

    public void stopSpinning() {
        try {
            assert runner != null;
            runner.done.set(true);
            barrier.barrier();
            runner = null;
        } catch (InterruptedException e) {
            throw new AssertionError(e);
        } catch (BrokenBarrierException e) {
            throw new AssertionError(e);
        }
    }

    private final class SpinnerRunner extends Thread {
        private final SynchronizedBoolean done =
            new SynchronizedBoolean(false);

        public void run() {
            try {
                int i = 0;
                while (!done.get()) {
                    writer.print(SPIN_CHARS[i]);
                    writer.flush();
                    Thread.sleep(250);
                    writer.print((char) 0x8);  // ^h
                    i = (i + 1) % SPIN_CHARS.length;
                }
                barrier.barrier();
            } catch (InterruptedException e) {
                throw new AssertionError(e);
            } catch (BrokenBarrierException e) {
                throw new AssertionError(e);
            }
        }
    }
}
