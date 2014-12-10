/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

import com.avlsi.util.debug.Debug;
import java.text.DecimalFormat;

/**
 * <p> A statistics gathering subsystem for TSim. </p>
 *
 * @see AbstractDevice#AbstractDevice(String, String, Sigscan, DebugOpts)
 * @see BufferedChannel#BufferedChannel(int, int, int, int, int, int, String, BigInteger)
 *
 * @see BufferedChannel#checklessSend
 * @see BufferedChannel#checklessReceive
 *
 * @see Wait#select
 * @see AccurateWait#select
 *
 *
 * @author <a href="mailto:kiniry@fulcrummicro.com">Joseph Kiniry</a>
 * @version $Revision$ $Date$
 **/

public class Statistics
{
    /**
     * Package-level flag to indicate that we should gather statistics.
     **/
    public static final boolean STATISTICS = false;
    public static final boolean HZ_STATISTICS = false;
    static final boolean BRIEF_VERSION = true;

    static final int REPORT_INTERVAL = 250000;

    static DecimalFormat formatter = new DecimalFormat("###,###,###.##");

    // Have we reported how many threads are in the system yet?
    static boolean reported_thread_count = false;

    // Various attributes used for statistics-gathering.

    /**
     * Total number of channels (system-wide).
     *
     * @design Average channels per device is partially derived from this value.
     * @design Average messages per channel is partially derived from this
     * value.
     *
     * <pre><jml>
     * invariant totalChannelCount >= 0;
     * </jml></pre>
     **/
    static long totalChannelCount;

    /**
     * Total number of devices (system-wide).
     *
     * @design Average channels per device is partially derived from this value.
     * @design Average messages per device is partially derived from this value.
     *
     * <pre><jml>
     * invariant totalDeviceCount >= 0;
     * </jml></pre>
     **/
    static long totalDeviceCount;

    /**
     * Total messages delivered (all channels).
     *
     * @design Total messages delivered per second (system-wide) is partially
     * derived from this value.
     * @design Average messages per channel is partially derived from this
     * value.
     * @design Average messages per device is partially derived from this value.
     *
     * <pre><jml>
     * invariant totalMessagesDelivered >= 0;
     * </jml></pre>
     **/
    static long totalMessagesDelivered;

    /**
     * Total time waiting on <code>BufferedChannel.receive()</code>.
     *
     * @design Average time waiting on receives is derived from this value.
     * @design Total time between blocked actions is partially derived from this
     * value.
     *
     * <pre><jml>
     * invariant totalReceiveWaitTime >= 0;
     * </jml></pre>
     **/
    static long totalReceiveWaitTime;

    /**
     * Total time waiting on <code>BufferedChannel.send()</code>.
     *
     * @design Average time waiting on sends is derived from this value.
     * @design Total time between blocked actions is partially derived from this
     * value.
     *
     * <pre><jml>
     * invariant totalSendWaitTime >= 0;
     * </jml></pre>
     **/
    static long totalSendWaitTime;

    /**
     * Total time waiting on <code>select()</code>.
     *
     * @design Total time waiting on actions is partially derived from this
     * value.
     *
     * <pre><jml>
     * invariant totalSelectWaitTime >= 0;
     * </jml></pre>
     **/
    static long totalSelectWaitTime;

    /**
     * The time at which the first message in the system was sent.
     *
     * @design We chose this particular time as the system start time to ignore
     * all non-simulation-related startup issues.  If thread creation time
     * really did impact simulation speed (i.e., our simulations were less than
     * a second long), then this choice might be different.
     *
     * <pre><jml>
     * invariant systemStartTime > 0;
     * </jml></pre>
     **/
    static long systemStartTime;


    // Dummy Constructor

    Statistics()
    {
        Debug.fail("The Statistics class is not meant to be instantiated.");
    }


    // Static Methods

    static synchronized long incrementChannelCount()
    {
        totalChannelCount++;
        return totalChannelCount;
    }

    static synchronized long incrementDeviceCount()
    {
        totalDeviceCount++;
        return totalDeviceCount;
    }

    static synchronized long incrementMessagesDelivered()
    {
        if (totalMessagesDelivered == 0)
            systemStartTime = System.currentTimeMillis();

        totalMessagesDelivered++;

        // For now, until we determine when/where to output data, we'll just do
        // it periodically here.
        if (totalMessagesDelivered % REPORT_INTERVAL == 0)
            summarizeSystemStatistics();

        return totalMessagesDelivered;
    }

    static synchronized long incrementReceiveWaitTime(long delta)
    {
        totalReceiveWaitTime += delta;
        return totalReceiveWaitTime;
    }

    static synchronized long incrementSendWaitTime(long delta)
    {
        totalSendWaitTime += delta;
        return totalSendWaitTime;
    }

    static synchronized long incrementSelectWaitTime(long delta)
    {
        totalSelectWaitTime += delta;
        return totalSelectWaitTime;
    }

    static synchronized void summarizeSystemStatistics()
    {
        long currentTime = System.currentTimeMillis();
        long runTime = currentTime - systemStartTime;

        // Run-time.

        double runTimeInSeconds = (double)runTime/1000.0;
        double runTimeInMinutes = runTimeInSeconds/60.0;
        double runTimeInHours = runTimeInMinutes/60.0;

        // Device and channel counts and ratio.

        double channelsPerDevice = (double)totalChannelCount/(double)totalDeviceCount;
        
        // Threads

        int activeThreadCount = Thread.activeCount();

        // Message throughput summaries.

        double messagesDeliveredPerDevice = 
            (double)totalMessagesDelivered/(double)totalDeviceCount;
        double messagesDeliveredPerChannel = 
            (double)totalMessagesDelivered/(double)totalChannelCount;
        double messagesDeliveredPerSecond =
            (double)totalMessagesDelivered/runTimeInSeconds;

        // Summary of wait times.

        double totalReceiveWaitTimeInSeconds = (double)totalReceiveWaitTime/1000.0;
        double totalSendWaitTimeInSeconds = (double)totalSendWaitTime/1000.0;
        double totalSelectWaitTimeInSeconds = (double)totalSelectWaitTime/1000.0;
        long totalWaitTime = 
            totalReceiveWaitTime + totalSendWaitTime + totalSelectWaitTime;
        double totalWaitTimeInSeconds = (double)totalWaitTime/1000.0;
        double totalWaitTimeInSecondsPerThread = 
            totalWaitTimeInSeconds/(double)activeThreadCount;
        double percentOfTimeWaiting = 
            totalWaitTimeInSecondsPerThread/runTimeInSeconds*100.0;

        if (!BRIEF_VERSION) {
            System.out.println("This system has been running for:");
            System.out.println(formatter.format(runTime) + "ms, " + 
                               formatter.format(runTimeInSeconds) + "s, " + 
                               formatter.format(runTimeInMinutes) + "m, " + 
                               formatter.format(runTimeInHours) + "h");
            System.out.println("This system has " + totalDeviceCount + 
                               " devices connected by " + totalChannelCount + 
                               " channels,\nfor an average of " + 
                               formatter.format(channelsPerDevice) + 
                               " channels per device.");
            System.out.println(formatter.format(totalMessagesDelivered) + 
                               " messages have been delivered.");
            System.out.println("This results in a total of " + 
                               formatter.format(messagesDeliveredPerDevice) + 
                               " messages delivered per device,");
            System.out.println("and " + formatter.format(messagesDeliveredPerChannel) + 
                               " messages delivered per channel.");
        }
        if (!reported_thread_count) {
            System.out.println(activeThreadCount + " threads are active in the system.");
            reported_thread_count = true;
        }
        System.out.println("STATISTICS REPORT: " + 
                           formatter.format(messagesDeliveredPerSecond) + 
                           " messages delivered per second.");


        // This performance is probably due to the amount of time this system is
        // waiting on events.
        if (!BRIEF_VERSION) {
            System.out.println("The total receive wait time is " + 
                               formatter.format(totalReceiveWaitTime) + "ms, or " + 
                               formatter.format(totalReceiveWaitTimeInSeconds) + "s.");
            System.out.println("The total send wait time is " + 
                               formatter.format(totalSendWaitTime) + "ms, or " + 
                               formatter.format(totalSendWaitTimeInSeconds) + "s.");
            System.out.println("The total select wait time is " + 
                               formatter.format(totalSelectWaitTime) + "ms, or " + 
                               formatter.format(totalSelectWaitTimeInSeconds) + "s.");
            System.out.println("The TOTAL wait time is " + 
                               formatter.format(totalWaitTime) + "ms, or " + 
                               formatter.format(totalWaitTimeInSeconds) + "s.");
            System.out.println("Finally, the total wait time for all threads is " + 
                               formatter.format(totalWaitTime) + "s,\nwhich is " + 
                               formatter.format(totalWaitTimeInSecondsPerThread) +
                               " seconds waiting per thread" +
                               ",\nwhich means we are waiting " + 
                               formatter.format(percentOfTimeWaiting) +
                               "% of the time waiting.");
            // Note that all of these numbers are approximations.  Our error bar,
            // after cross-checking these estimates against commercial profiling
            // tools are within 5%.
            System.out.println("\n\n");
        }
    }

} // end of class Statistics

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
