/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.tsim.verilog;

import java.net.Socket;

import java.io.IOException;
import java.io.BufferedWriter;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

import com.avlsi.util.debug.Debug;

/**
 * Class for ...
 *
 * @author Dan Daly
 * @version $Date$
 **/

class VerilogTestClient implements Runnable{

    protected Thread thread=null;
    protected BufferedWriter bw=null;
    protected BufferedReader br=null;
    private final String name;
    
    public VerilogTestClient(String name) throws IOException {
        this.name = name;
        Socket sock = new Socket("localhost", VerilogServer.verilog_port);
        br = new BufferedReader(
                            new InputStreamReader(sock.getInputStream()));
        bw = new BufferedWriter(
             new OutputStreamWriter(sock.getOutputStream()));
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
            System.out.println("Starting Client.");
            while (true) {
                String inp = br.readLine();
                if (inp != null) 
                    process(inp);
            }
                
        } catch (IOException e) {
            System.err.println("["+getName()+"] I/O Error in Client Run: "+
                                e.getMessage());
        }
    }

    private void process(String msg) {
        if (msg.length() > 0) {
            switch(msg.charAt(0)) {
                case '0':   handleSetBus(msg);
                            break;
                default: System.out.println("Unknown: "+msg);
            }
        } else Debug.assertTrue(false);
    }                    
                        
    private void handleSetBus(String msg) {
        //StringTokenizer st = new StringTokenizer(msg.substring(1),
        //                         VerilogConnection.DELIMITER);
        //Debug.assertTrue(st.hasMoreTokens());
        try {
            sendMessage(msg+"\n"); //Return it right back
        } catch (IOException e) {
            Debug.assertTrue(false);
        }
    }   
   
    public void sendMessage(String msg) throws IOException {
        bw.write(msg);
        bw.flush();
    }

    public final String getName() { return name; }

    public static void main(String[] args) {
   
        try {
            VerilogTestClient vtc = new VerilogTestClient("client1");
            vtc.start();
            //vtc.sendMessage("0top.bus1@#$%00001001@#$%11\n");
            vtc.sendMessage("1\n");
            vtc.sendMessage("220\n");
        } catch (IOException e) {
            System.out.println(e.getMessage());
        }

        while(true) {}
    }
}

