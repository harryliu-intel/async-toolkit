/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.tsim.verilog;
import java.net.Socket;

import java.io.IOException;

import com.avlsi.tools.tsim.BusWatcher;
import com.avlsi.tools.tsim.SharedBus;
/**
 * Class for serving connections from Verilog clients
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class VerilogServer extends Server {

    public static final int verilog_port = 3345;
    
    /**
     * Constructor.
     **/
    public VerilogServer(String name) throws IOException {
        super(name, verilog_port);
    }

    private volatile int connectionCount = 0;
    public void newConnection(Socket sock) throws IOException {
        System.out.println("New Connection.");
        new VerilogConnection("VerilogSocket_on_"+getName()+(connectionCount++),
                              socket).start();
    }

    public VerilogConnection returnNewConnection() throws IOException {
        //Socket client = socket.accept();
        return new VerilogConnection(
                "VerilogConnect_on_"+getName()+(connectionCount++),socket);
    }

    public static void main(String[] args) {
        System.out.println("Starting Server.");
        try {
            VerilogServer vs = new VerilogServer("VerilogServer");
            VerilogConnection conn = vs.returnNewConnection();
            conn.start();
            conn.sendMessage("Send To Me\n");
        } catch (IOException e) {
            System.err.println("Could not start server: "+e.getMessage());
        }
        while (true) {}
    }
}

