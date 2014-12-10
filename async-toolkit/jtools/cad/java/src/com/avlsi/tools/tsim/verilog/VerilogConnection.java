/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.tsim.verilog;

import com.avlsi.util.debug.Debug;

import java.net.Socket;
import java.net.ServerSocket;

import java.io.IOException;
import java.io.BufferedWriter;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.StringTokenizer;
import java.util.Hashtable;

import com.avlsi.tools.tsim.SharedBus;
import com.avlsi.tools.tsim.NativeBusDriver;

import com.avlsi.tools.dsim.DigitalScheduler;

/**
 * Class for ...
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class VerilogConnection implements Runnable, NativeBusDriver {

    public static final String DELIMITER = "@#$%";
    protected final ServerSocket socket;
    protected Socket sock=null;
    protected final String name;
    protected Thread thread=null;
    protected final Hashtable buses = new Hashtable();
    
    protected BufferedReader reader;
    protected BufferedWriter writer;

    private boolean stopped = false;
    private boolean init = false;

    /**
     * Constructor.
     **/
    public VerilogConnection(String name, ServerSocket socket) 
            throws IOException {
        this.name = name;
        this.socket = socket;
    }

    private synchronized void init() throws IOException{
        if (!init) {
            System.out.println("Ready for Verilog Connection.");
            sock = socket.accept();
            try {
                reader = new BufferedReader(
                         new InputStreamReader(sock.getInputStream()));
                writer = new BufferedWriter(
                         new OutputStreamWriter(sock.getOutputStream()));
            } catch (IOException e) {
                try { sock.close(); } catch (IOException ex) { 
                    Debug.assertTrue(false, "Error closing the socket after "+
                                        "reader & writer failed.");
                }
                throw e;
            }
            init = true;
            this.notifyAll();
        }
    }
    
    /**
     * Creates a new thread and starts it.
     **/
    public final void start() {
        if (thread != null) return;
        thread = new Thread(this, getName());
        thread.start();
    }

    public void run() {
        try {
            init();
            while (!stopped) {
                String input = reader.readLine();
                if (input != null) process(input);
            }
        } catch (IOException e) {
            System.err.println("I/O Error, exiting:\n\t"+e.getMessage());
            //e.printStackTrace();
            System.exit(1);
        }
        
    }

    public void process(String msg) {
        //System.out.println("Received: "+msg);
        if (msg.length() > 0) {
            switch(msg.charAt(0)) {
                case '0':   handleSetBus(msg);
                            break;
                case '1':   handleNextTime();
                            break;
                case '2':   handleCycleTo(msg);
                            break;
                default: System.out.println("Unknown: "+msg);
            }
        } //else Debug.assertTrue(false);
    }
    
    private void handleSetBus(String msg) {
        StringTokenizer st = new StringTokenizer(msg.substring(1),
                                                 DELIMITER);
        Debug.assertTrue(st.hasMoreTokens());
        String busname = st.nextToken();

        Debug.assertTrue(st.hasMoreTokens());
        //String value = st.nextToken();
        long value = 0;
        try {
            value = Long.parseLong(st.nextToken(),16);
        } catch (NumberFormatException e) {
            System.err.println("Verilog reported non-numeric signal value, "+
                               "msg= "+msg);
            return;
        }

        Debug.assertTrue(st.hasMoreTokens());
        long timeInFemtoSecs = 0, time=0;
        try {
            timeInFemtoSecs = Long.parseLong(st.nextToken(),16);
            time = SharedBus.convert(timeInFemtoSecs, SharedBus.fsec);
        } catch (NumberFormatException e) {
            System.err.println("Verilog reported non-numeric time, "+
                               "msg= "+msg);
            return;
        }

        SharedBus bus = (SharedBus) buses.get(busname);
        if (bus == null) {
            // XXX Hack, take out after testing
            try {
                Thread.sleep(3000); //3 sec
                //maybe the bus hasn't been created yet...
            } catch (InterruptedException e) { Debug.assertTrue(false); }
            bus = (SharedBus) buses.get(busname);
            if (bus == null) {
                System.err.println("Bus "+busname+" set in the Verilog "+
                                   "is not registered with VerilogConnection "+
                                   "or it does not exist, time = "+time+
                                   " buses.length= "+buses.size());
                return;
            }
        }
        
        if ((time - getTime()) == -1) {
            //This is caused by rounding in the conversion from femto
            //to dsim units.  In this particular case, round up so that
            //the event does not fire in the past.
            //One DSim unit should be negligible...
            bus.set(this, value, getTime());
        } else
            bus.set(this, value, time - getTime()); //computes delay
    }

    public long getTime() { return DigitalScheduler.get().getTime(); }

    private void handleNextTime() {
        long time = DigitalScheduler.get().getNextEventTime();
        long timeInFemtoSecs = 
            Math.round(SharedBus.getSecs(time)/SharedBus.fsec);
        //System.out.println("DSIM time= "+time+" = "+timeInFemtoSecs);
        try {
            if (time <= -1)
                sendMessage("1empty\n");
            else 
                sendMessage("1"+Long.toHexString(timeInFemtoSecs)+"\n");
        } catch (IOException e) {
            System.err.println("I/O Error while sending getNextTime: "+
                               e.getMessage());
        }
    }

    private void handleCycleTo(String msg) {
        long timeInFemtoSecs=0, time=0;
        try {
            timeInFemtoSecs = Long.parseLong(msg.substring(1),16);
            time = 
                Math.round(SharedBus.convert(timeInFemtoSecs, SharedBus.fsec));
        } catch (NumberFormatException e) {
            System.err.println("Verilog reported non-numeric time, "+
                               "msg= "+msg);
            return;
        }
        DigitalScheduler dsim = DigitalScheduler.get();
        dsim.cycleTo(time+1); //Include the time given
        dsim.waitUntilStable();
        try {
            sendMessage("2"+dsim.getTime()+"\n"); //cycle complete reply
        } catch (IOException e) {
            System.err.println("I/O Error while sending CycleTo: "+
                               e.getMessage());
        }

    }

    public synchronized void sendMessage(String msg) throws IOException {
        while (!init) {
            System.out.print("Waiting for Verilog Connection...");
            try {
                wait();
            } catch (InterruptedException e) {
                Debug.assertTrue(false);
            }
            if (init) System.out.println("done.");
        }
        writer.write(msg);
        writer.flush();
    }
    
    public void registerBus(SharedBus bus) {
        buses.put(bus.getFullname(), bus);
    }

    public void setBus(SharedBus bus, long newdata, long time) {
        StringBuffer buf = new StringBuffer("0");
        buf.append(bus.getFullname());
        buf.append(DELIMITER);
        buf.append(Long.toHexString(newdata));
        buf.append(DELIMITER);
        long timeInFemtoSecs = 
            Math.round(SharedBus.getSecs(time)/SharedBus.fsec);
        buf.append(Long.toHexString(timeInFemtoSecs));
        buf.append("\n");
        try {
            sendMessage(buf.toString());
        } catch (IOException e) {
            System.err.println("I/O Error during setBus: "+e.getMessage());
        }
    }

    public final String getName() { return name; }
}

