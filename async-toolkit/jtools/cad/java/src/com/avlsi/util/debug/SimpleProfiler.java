/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.util.debug;

import java.io.*;
import java.lang.reflect.*;
import java.util.*;

import com.avlsi.util.debug.GetTime;
import com.avlsi.util.debug.Debug;

/**
 * Gather general profiling statistics for the current java process
 * (currently memory usage only). Example use:
 *
 * SimpleProfiler sp=new SimpleProfiler;
 * sp.init();
 * // do stuff 
 * sp.finish();
 * sp.printStats(System.err);
 *
 * @author Frederik Eaton
 * @version $Revision$ $Date$
 **/
public class SimpleProfiler
{
    // ----------------------------------------------------------------
    // static shared instance

    private static SimpleProfiler shared=null;
    public static SimpleProfiler get() {
        if(shared==null) shared = new SimpleProfiler();
        return shared;
    }

    // ----------------------------------------------------------------
    // internals

    public long numSamples=0;
    public long memLast=0;
    public long memTotal=0;
    public long memAve=0;
    public long memMax=0;
    public Vector memData=new Vector();
    public long sampleInterval;
    public long timeStart=0;
    public long timeLast;
    public long timeEnd;
    public Vector sampleTimes=new Vector();
    public boolean profiling;

    private Thread monitorThread=null;

    // time units per second
    private static double timeScale=1000.0;

    public SimpleProfiler() {
        this(5000); // take samples every five seconds
    }

    public SimpleProfiler(long sampleInterval) {
        this.sampleInterval=sampleInterval;
    }

    private long userTime() {
        return GetTime.userCPUTime();
    }

    private long systemTime() {
        return GetTime.systemCPUTime();
    }

    private long realTime() {
        return System.currentTimeMillis();
    }

    private void sleepUntil(long target) {
        while (realTime() < target
               && profiling) {
            try {
//                 System.err.println("in SimpleProfiler.sleepUntil: calling "+
//                                    "Thread.sleep("+(target-realTime())+")");
                Thread.sleep(target-realTime());
            } catch (InterruptedException e) {}
        }
    }

    private void doSample() {
//         System.err.println("in SimpleProfiler.doSample");
        numSamples++;

        // record time
        timeLast=realTime();
        if(timeStart==0) timeStart=timeLast;
        sampleTimes.add(new Long(timeLast));

        // record memory usage data
        Runtime rt=Runtime.getRuntime();
//        rt.gc();
        memLast = (rt.totalMemory()-rt.freeMemory());
        memTotal += memLast;
        memAve = memTotal/numSamples;
        if(memLast > memMax) memMax=memLast;
        memData.add(new Long(memLast));
    }

    private long nextSampleTime() {
        //        return timeStart+numSamples*sampleInterval;
        return timeLast+sampleInterval;
    }

    private double lookupSampleTime(int i) {
        return (((Long)sampleTimes.elementAt(i)).longValue()-
                timeStart)/timeScale;
    }

    private String memString(long bytes) {
        return bytes/timeScale+"k";
    }

    // ----------------------------------------------------------------
    // probing (allows categorization of time spent in different parts
    // of the program)
    //
    // Usage:
    // SimpleProfiler.Probe p = SimpleProfiler.getProbe("the thing");
    // void foo() {
    //     p.enter();
    //     // do the thing
    //     p.exit();
    // }
    //
    // Why "probe", instead of "timer"? Because we might want to
    // collect other data with probes. We could for instance collect
    // data on memory usage.

    // TODO: collect probe time samples along with memory samples
    
    private static LinkedHashMap probeMap=new LinkedHashMap();

    public static Probe theEmptyProbe = new EmptyProbe();

    private static boolean useProbes=false;

    public static Probe getProbe(String name) {
        synchronized(probeMap) {
            if(!probeMap.containsKey(name))
                probeMap.put(name, get().new RealProbe());
            Probe p=(Probe)probeMap.get(name);
            return p;
        }
    }

    public interface Probe {
        void enter();
        void exit();
    }

    public class RealProbe implements Probe {
        public long timeTotalUser=0;
        public long timeTotalSystem=0;
        private ThreadLocal timeStartUser=new ThreadLocal() {
                protected synchronized Object initialValue() {
                    return new Long(0);
                }
            };
        private ThreadLocal timeStartSystem=new ThreadLocal() {
                protected synchronized Object initialValue() {
                    return new Long(0);
                }
            };
        
        public void enter() {
            if(!useProbes) return;
            timeStartUser.set(new Long(userTime()));
            timeStartSystem.set(new Long(systemTime()));
        }

        public void exit() {
            if(!useProbes) return;
            long endUser=userTime();
            long endSystem=systemTime();
            synchronized(this) {
                timeTotalUser+=endUser-((Long)timeStartUser.get()).longValue();
                timeTotalSystem+=endSystem-((Long)timeStartSystem.get()).longValue();
            }
        }
    }

    public static class EmptyProbe implements Probe {
        public void enter() {}
        public void exit() {}
    }

    // ----------------------------------------------------------------
    // general public interface

    public void start() {
        profiling = true;
        useProbes = true;
        monitorThread=
            new Thread(new Runnable() {
                    public void run() {
                        while(profiling) {
                            doSample();
                            sleepUntil(nextSampleTime());
                        }
                        doSample();
                    }
                }, "SimpleProfiler monitor thread");
        monitorThread.start();
    }

    public void stop() {
        timeEnd=realTime();
        profiling=false;
        monitorThread.interrupt();
        retry:
        try {
            monitorThread.join();
        } catch(InterruptedException e) {
            /* do nothing */
            break retry;
        }
        monitorThread=null;
    }

    public void printStats(PrintStream ps) {
        ps.println("Memory usage data (gathered in "+
                   sampleInterval/timeScale+" second intervals):");
        ps.println("SAMPLE_NUM TIME(sec) MEMORY_USED");
        for(int i=0; i<numSamples; i++) {
            ps.println(i+" "+lookupSampleTime(i)+" "+
                       memString(((Long)memData.elementAt(i)).longValue()));
        }
        ps.println("Average memory used: "+memString(memAve));
        ps.println("Maximum memory used: "+memString(memMax));
        ps.println("Probe time data:");
        ps.println("PROBE_NAME TIME_USER(sec) TIME_SYSTEM(sec)");
        String name; RealProbe p;
        for(Iterator it=probeMap.keySet().iterator(); it.hasNext(); ) {
            name=(String)it.next(); p=(RealProbe)probeMap.get(name);
            ps.println(name+" "+p.timeTotalUser/timeScale+" "+p.timeTotalSystem/timeScale);
        }
        ps.println("Total run time: "+(timeEnd-timeStart)/timeScale);
        // printStats must be called by the parent of all threads for
        // this to be useful
        ps.println("Total user cpu time: "+
                   (GetTime.userCPUTime()+
                    GetTime.childrenUserCPUTime())/timeScale);
        ps.println("Number of samples: "+numSamples);
    }

    // ----------------------------------------------------------------

    /** 
     * main
     *
     * This allows you to easily profile another class without
     * modifying its code - just insert com.avlsi.debug.SimpleProfiler
     * and an output file before the class name on the java command
     * line. Our main method will fetch the class with name of
     * args[0], and call its main method with the rest of the
     * arguments, while taking profiling data.
     **/
    // todo: better error checking
    public static void main(String args[]) throws Exception {
        final String className = args[1];
        File outputFile = new File(args[0]);
        int myArgCount = 2;
        final Class mainClass = Class.forName(className);
        final Method mainMethod = 
            mainClass.getMethod("main", 
                                new Class[] { String[].class });

        final String[] realArgs = new String[args.length - myArgCount];
        System.arraycopy(args, myArgCount, realArgs, 0, args.length - myArgCount);

        final SimpleProfiler sp = get();
        final PrintStream ps;
        if(outputFile.equals("-"))
            ps = System.err;
        else
            ps = new PrintStream
                (new FileOutputStream(outputFile));

        System.err.println("Starting profiler, will write output to "+
                   outputFile.getAbsolutePath());
        sp.start();
        System.err.print("Calling "+className+
                         ".main(");
        for(int i=0; i<realArgs.length; i++) {
            if(i>0) ps.print(", ");
            ps.print("\""+realArgs[i]+"\"");
        }
        ps.println(")");

        SimpleProfiler.Probe mainProbe = SimpleProfiler.getProbe("main");
        mainProbe.enter();
        mainMethod.invoke(null,new Object[] { realArgs });
        mainProbe.exit();
        sp.stop();
        sp.printStats(ps);
        System.out.println("children time: "+GetTime.childrenUserCPUTime());
        System.out.println("user time: "+GetTime.userCPUTime());
    }
}
