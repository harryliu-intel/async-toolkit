/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.benchmarks;

import EDU.oswego.cs.dl.util.concurrent.*;
import com.avlsi.util.debug.Debug;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.math.BigInteger;
import java.text.DecimalFormat;
import java.util.Random;

/**
 * <p> Measure the cost of context switching on various kinds of synchronization
 * constructs in Java. </p>
 *
 * @author <a href="mailto:kiniry@fulcrummicro.com">Joseph Kiniry</a>
 * @version $Revision$ $Date$
 **/

public class JavaSyncCost implements Runnable
{
    // Static Private Attributes

    private static final boolean DEBUG = false;

    private static Mutex mutex = new Mutex();
    private static CyclicBarrier barrier;
    private static Semaphore semaphore = new Semaphore(1);

    private static final byte NONE = -1, 
                              MUTEX = 0, BARRIER = 1, SEMAPHORE = 2, 
                              SYNCBLOCK = 3, SYNCMETHOD = 4, WAITNOTIFY = 5, PIPE = 6;
    private static final long COUNTS_PER_TEST = 10000;
    private static final long TOTAL_NUMBER_OF_TESTS = 10;
    private static final long MICROSECONDS_PER_MILLISECOND = 1000;
    private static byte testType;
    private static volatile boolean headerPrinted = false, resultPrinted = false;
    private static int numThreads = 2;
    private static Random random = new Random();


    // Private Attributes

    private long counter = 0;
    private long startTime = System.currentTimeMillis();
    private long runTime = 0;
    private DecimalFormat formatter = new DecimalFormat("###,###,###.##");
    private int threadNumber;

    private double cost = 0.0;
    private double totalCost = 0.0;
    private double costPerOperationInMicrosections = 0.0;


    // Constructors

    public JavaSyncCost(byte type, int threadNumber)
    {
        this.testType = type;
        this.threadNumber = threadNumber;
    }

    // Public Methods

    // Inner computation loop.

    public synchronized void synchronizedCompute()
    {
        compute();
    }

    public void compute()
    {
        counter++;
        if (counter % COUNTS_PER_TEST == 0) {
            runTime = System.currentTimeMillis() - startTime;
            costPerOperationInMicrosections = 
                (double)runTime / COUNTS_PER_TEST * MICROSECONDS_PER_MILLISECOND;
            totalCost += costPerOperationInMicrosections;
            if (DEBUG) {
                System.out.println(COUNTS_PER_TEST + " operations in " + 
                                   formatter.format(runTime) +
                                   "ms = cost of " + 
                                   formatter.format(costPerOperationInMicrosections) +
                                   "us");
                System.out.println("Total cost thus far: " +
                                   formatter.format(totalCost) + "us");
            }
            startTime = System.currentTimeMillis();
        }
        if (counter > TOTAL_NUMBER_OF_TESTS * COUNTS_PER_TEST)
            printTestResult();
    }

    synchronized void printTestResult()
    {
        if (!resultPrinted) {
            resultPrinted = true;
            System.out.println("Average cost: " + 
                   formatter.format(totalCost/(double)TOTAL_NUMBER_OF_TESTS) + "us");
            System.exit(0);
        }
    }        

    synchronized void printTestHeader(String testName)
    {
        if (!headerPrinted) {
            headerPrinted = true;
            System.out.println("Running " + testName + " test.");
        }
    }        

    public void mutexTest()
    {
        printTestHeader("mutex");

        while (true) {
            try {
                mutex.acquire();
                try {
                    compute();
                } finally {
                    mutex.release();
                }
            } catch (InterruptedException ie) {
                Debug.log("A thread was interrupted.");
            }
        }
    }

    public void barrierTest()
    {
        printTestHeader("barrier");

        while (true) {
            try {
                barrier.barrier();
                compute();
            } catch (BrokenBarrierException bbe) {
                Debug.log("A barrier was broken.");
            } catch (InterruptedException ie) {
                Debug.log("A thread was interrupted.");
            }
        }
    }

    public void semaphoreTest()
    {
        printTestHeader("semaphore");

        while (true) {
            try {
                semaphore.acquire();
                compute();
                semaphore.release();
            } catch (InterruptedException ie) {
                Debug.log("A thread was interrupted.");
            }
        }
    }

    public void synchronizedBlockTest()
    {
        printTestHeader("synchronized block");

        while (true) {
            synchronized(JavaSyncCost.class) {
                compute();
            }
        }
    }

    public void synchronizedMethodTest()
    {
        printTestHeader("synchronized method");

        while (true) {
            synchronizedCompute();
        }
    }

    // @todo kiniry 15 Aug 2002 - Not yet correct or working.

    public void waitNotifyTest()
    {
        printTestHeader("wait-notify");
    }

    public void pipeTest() throws IOException, ClassNotFoundException
    {
        printTestHeader("pipe");

        if (DEBUG)
            System.out.println("Current counter = " + counter + "\n" +
                               "Thread number = " + threadNumber);

        // Build two pipes through which the threads can communicate.
        PipedOutputStream posZero = new PipedOutputStream();
        PipedOutputStream posOne = new PipedOutputStream();
        PipedInputStream pisZero = new PipedInputStream(posZero);
        PipedInputStream pisOne = new PipedInputStream(posOne);
        ObjectOutputStream oosZero = new ObjectOutputStream(posZero);
        ObjectOutputStream oosOne = new ObjectOutputStream(posOne);
        ObjectInputStream oisZero = new ObjectInputStream(pisZero);
        ObjectInputStream oisOne = new ObjectInputStream(pisOne);

        if (DEBUG)
            System.out.println("Built channels.");

        // +-----------+                                           +-----------+
        // | Process 0 |-->oosZero-->posZero-->pisZero-->oisZero-->| Process 1 |
        // |           |<--oisOne<---pisOne<---posOne<---oosOne<---|           |
        // +-----------+                                           +-----------+

        BigInteger message = new BigInteger(64, new Random());
        if (threadNumber == 0) {
            if (DEBUG)
                System.out.println("Zero starts sending.");

            // Process/Thread 0 starts things off.
            while (true) {
                oosZero.writeObject(message);
                oosZero.flush();
                message = (BigInteger)oisOne.readObject();
            }
        } else {
            if (DEBUG)
                System.out.println("One starts receiving.");

            // Process/Thread One
            while (true) {
                message = (BigInteger)oisZero.readObject();
                oosOne.writeObject(message);
                oosOne.flush();
                compute();
            }
        }
    }

    /**
     * Run method of test.  Eternally attempt to acquire and release a mutex as
     * quickly as possible.
     **/

    public void run()
    {
        switch (testType) {
            case MUTEX: 
                mutexTest();
                break;
            case BARRIER:
                barrierTest();
                break;
            case SEMAPHORE:
                semaphoreTest();
                break;
            case SYNCBLOCK:
                synchronizedBlockTest();
                break;
            case SYNCMETHOD:
                synchronizedMethodTest();
                break;
            case WAITNOTIFY:
                waitNotifyTest();
                break;
            case PIPE:
                try {
                    pipeTest();
                } catch (Exception e) {
                    System.err.println("Pipe test failed: " + e.getMessage());
                    System.exit(-1);
                }
                break;
        }
    }


    // Static Methods

    /** 
     * Main method that sets up test.
     *
     * @param args command-line arguments.  The first argument is the test
     * name, the second is the number of threads that should be used in
     * the test.  If the arguments are not recognized, a usage message
     * is printed.
     * @exception InterruptedException if the main thread is interrupted.
     **/

    public static void main(String [] args) throws InterruptedException 
    {
        byte testType = NONE;

        if ((args == null) || (args.length == 0))
            testType = BARRIER;
        else if (args[0].compareToIgnoreCase("barrier") == 0)
            testType = BARRIER;
        else if (args[0].compareToIgnoreCase("mutex") == 0)
            testType = MUTEX;
        else if (args[0].compareToIgnoreCase("semaphore") == 0)
            testType = SEMAPHORE;
        else if (args[0].compareToIgnoreCase("syncblock") == 0)
            testType = SYNCBLOCK;
        else if (args[0].compareToIgnoreCase("syncmethod") == 0)
            testType = SYNCMETHOD;
        else if (args[0].compareToIgnoreCase("waitnotify") == 0)
            testType = WAITNOTIFY;
        else if (args[0].compareToIgnoreCase("pipe") == 0)
            testType = PIPE;
        else usage();

        if (args != null) {
            if (args.length >= 2) {
                try {
                    numThreads = Integer.parseInt(args[1]);
                } catch (NumberFormatException nfe) {
                    System.out.println(nfe.getMessage());
                    usage();
                }
                if (numThreads < 1)
                    numThreads = 1;
            }
        }

        if (DEBUG)
            System.out.println("Running with " + numThreads + " thread(s).");

        barrier = new CyclicBarrier(numThreads);
        Thread [] threads = new Thread [numThreads];
        for (int i = 0; i < numThreads; i++) {
            threads[i] = new Thread(new JavaSyncCost(testType, i));
        }
        for (int i = 0; i < numThreads; i++) {
            threads[i].start();
        }
    }

    static void usage() 
    {
        System.out.println("Usage: JavaSyncCost " +
                           "[barrier|mutex|semaphore|syncblock" + 
                           "|syncmethod|waitnotify|pipe] " +
                           "[# threads}");
        System.out.println("       No parameters indicates a barrier " +
                           "test with 2 threads.");
    }

} // end of class JavaSyncCost

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
