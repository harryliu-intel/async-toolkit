/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.tsim;

import java.util.HashMap;
import java.util.Iterator;
import java.util.ArrayList;
import java.util.Map;

import java.io.PrintWriter;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.math.BigInteger;

import com.avlsi.tools.dsim.DigitalScheduler;

/**
 * Class for logging sends and receives to a VCD file
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class DeviceLogger implements ChannelCreationInterface {

    private static long index = 0;
    private static final int numChars = 89;
    private static final int firstChar = 33;
    
    private PrintWriter writer;
    
    private static final HashMap tags = new HashMap();
    private final ArrayList unnamed_channels;
    private final String filename;
    private boolean closed = false;
    
    public DeviceLogger(String filename) throws IOException {
        this.filename = filename;
        writer = new PrintWriter( new BufferedWriter(
                    new FileWriter(filename)));
        unnamed_channels = new ArrayList();
        Runtime.getRuntime().addShutdownHook(
        new Thread(
            new Runnable() {
                public void run() { close(); }
            }
        ));
    }

    /** Write the value in characters in Base (numChars) **/
    private static String hashKey(long val) {
        long count = val;
        int power = 0;
        while (count >= numChars) {
            power++;
            count /= Math.pow(numChars,power);
        }
        char[] chhash =new char[power+1];
        long left = val;
        for (int loop=0;loop<chhash.length;loop++) {
            chhash[loop] = (char)(firstChar + (int)(left / 
                            (long) Math.pow(numChars,chhash.length-loop-1)));
            left = left % (long) Math.pow(numChars,chhash.length-loop-1);
        }
        String hashkey = new String(chhash);
        return hashkey;
        
    }

    public void initialize() {
        printHeader();
        printChannels();
        printInitialValues();
    }
    
    /** Called when a new buffered channel has been created **/
    public synchronized void newChannel(BufferedChannel chan) {
        String channame = chan.getName();
        String intchan = " internal channel";
        if (channame == null) {
            unnamed_channels.add(chan);
        } else {
            if (channame.endsWith(intchan))
                channame = 
                    channame.substring(0,channame.length()-intchan.length());
            tags.put(channame, hashKey(index));
            index++;
        }
    }

    public synchronized void printHeader() {
        writer.println("$timescale\n  1fs\n$end");
    }
    
    public synchronized void printChannels() {
        if (writer == null) return;
        for(Iterator i=tags.keySet().iterator();i.hasNext();) {
            String channame = (String)i.next();
            String tag = (String) tags.get(channame);
            writer.println("$scope module "+channame+" $end");
            writer.println("$var integer 32 "+
                        tag+"S sendval $end");
            writer.println("$var wire 1 "+
                        tag+"s send $end");
            writer.println("$var integer 32 "+
                        tag+"R recvval $end");
            writer.println("$var wire 1 "+
                        tag+"r recv $end");
            writer.println("$upscope $end");
        }
        writer.println("$enddefinitions $end");
        if (unnamed_channels.size() > 0) {
            System.err.println("DeviceLogger:  Unnamed channel(s):");
            for(Iterator i=unnamed_channels.iterator();i.hasNext();) {
                BufferedChannel unnamed = (BufferedChannel) i.next();
                System.out.println(unnamed);
            }
            System.out.println("Cannot log transactions from these channels");
        }
    }

    public synchronized void printInitialValues() {
        writer.println("$dumpvars");
        for(Iterator i=tags.entrySet().iterator();i.hasNext();) {
            String tag = (String)((Map.Entry)i.next()).getValue();
            writer.println("b0 "+tag+"S");
            writer.println("0"+tag+"s");
            writer.println("b0 "+tag+"R");
            writer.println("0"+tag+"r");
        }
        writer.println("$end");
    }

    public long getTime() { return DigitalScheduler.get().getTime(); }
    
    private void printTime() {
        writer.println("#"+
                    Math.round(SharedBus.getSecs(getTime())/SharedBus.fsec));
    }
    
    //
    //Methods Called in AbstractDevice to log sends/recvs
    //
    
    public synchronized void startedSending(AbstractDevice dev, 
            ChannelOutput out, Message m) {
        if (writer == null) return;
        printTime();
        String tag = (String) tags.get(out.getName());
        if (tag == null) {
        } else {
            writer.println("1"+tag+"s");
            BigInteger msg = m.getValue();
            if (msg.signum() != 1) {
                //message not positive
                msg = new BigInteger(1, msg.toByteArray());
            }
            writer.println("b"+msg.toString(2)+" "+tag+"S");
        }
    }
    
    public synchronized void endedSending(AbstractDevice dev, 
            ChannelOutput out,
                             Message m) {
        if (writer == null) return;
        printTime();
        String tag = (String) tags.get(out.getName());
        if (tag == null) {
        } else {
            writer.println("0"+tag+"s");
        }
    }

    public synchronized void startedReceiving(AbstractDevice dev, 
            ChannelInput in) {
        if (writer == null) return;
        printTime();
        String tag = (String) tags.get(in.getName());
        if (tag == null) {
        } else {
            writer.println("1"+tag+"r");
        }
    }
    
    public synchronized void endedReceiving(AbstractDevice dev, 
            ChannelInput in, Message m) {
        if (writer == null) return;
        printTime();
        String tag = (String) tags.get(in.getName());
        if (tag == null) {
        } else {
            writer.println("1"+tag+"s");
            BigInteger msg = m.getValue();
            if (msg.signum() != 1) {
                //message not positive
                msg = new BigInteger(1, msg.toByteArray());
            }
            writer.println("0"+tag+"r");
            writer.println("b"+msg.toString(2)+" "+tag+"R");
        }
    }

    //
    //Cleanup
    //
    
    public synchronized void close() {
        if (!closed) {
            printTime();
            writer.close();
            closed = true;
            writer=null;
            System.out.println("Finished Logging to "+filename);
        }
    }
}

