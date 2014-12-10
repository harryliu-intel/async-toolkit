/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.tsim.verilog;

import java.net.ServerSocket;
import java.net.Socket;

import java.io.IOException;

/**
 * Class for a Server in a Client-Server environment
 *
 * @author Dan Daly
 * @version $Date$
 **/

public abstract class Server implements Runnable {

    public static final int DEFAULT_PORT = 3333;
    public static final String DEFAULT_NAME = "Server";

    private final String name;
    
    protected Thread thread=null;
    private boolean stopped = false;
    
    protected final ServerSocket socket;
    /**
     * Constructor.
     **/
    public Server() throws IOException {
        this(DEFAULT_NAME, DEFAULT_PORT);
    }

    public Server(String name, int port) throws IOException {
        socket = new ServerSocket(port);
        this.name = name;
    }
    
    /**
     * Creates a new thread and starts it.
     **/
    public final void start() {
        if (thread != null) return;
        thread = new Thread(this, getName());
        thread.start();
    }

    /**The thread's run methods **/
    public void run() {
        try {
            while (!stopped) {
                Socket client = socket.accept();
                newConnection(client);
            }
        } catch (IOException e) {
            System.err.println("["+getName()+"] IOException while accepting "+
                               "socket connections:");
            System.err.println(e.getMessage());
        } finally {
            close();
        }
    }

    public abstract void newConnection(Socket sock) throws IOException;
    
    public void close() {
        try {
            socket.close();
        } catch (IOException e) {
            System.err.println(e.getMessage());
        }
    }  

    public final String getName() { return name; }
        
}

