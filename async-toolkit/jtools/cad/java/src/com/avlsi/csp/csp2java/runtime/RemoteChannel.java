/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 * Copyright 2002, 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id: //mrl/sw/intel/cad/java/src/com/avlsi/csp/csp2java/runtime/CspRuntimeAbstractDevice.java#10 $
 * $DateTime: 2017/09/13 22:18:34 $
 * $Author: amlines $
 */
package com.avlsi.csp.csp2java.runtime;

import java.math.BigInteger;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.Socket;

public class RemoteChannel {
    Socket socket;
    BufferedReader br;
    BufferedWriter bw;
    Exception last;
    public RemoteChannel(final String host, final int port) {
        try {
            socket = new Socket(host, port);
            socket.setTcpNoDelay(true);
            br = new BufferedReader(
                    new InputStreamReader(socket.getInputStream()));
            bw = new BufferedWriter(
                    new OutputStreamWriter(socket.getOutputStream()));
        } catch (IOException e) {
            System.err.println(e);
            last = e;
        }
    }
    protected void write(CspInteger x) {
        try {
            bw.write(x.toString(16) + "\n");
            bw.flush();
            //System.err.println("socket wrote: " + x);
        } catch (IOException e) {
            System.err.println(e);
            last = e;
        }
    }
    protected CspInteger read() {
        CspInteger result = CspInteger.ZERO;
        try {
            String line = br.readLine();
            result = new CspInteger(new BigInteger(line, 16));
            //System.err.println("socket read: " + result);
        } catch (IOException e) {
            System.err.println(e);
            last = e;
        }
        return result;
    }
    private CspInteger command(final String req) {
        CspInteger result = CspInteger.TRUE;
        try {
            bw.write(req + "\n");
            bw.flush();
            String line = br.readLine();
            result = new CspInteger(new BigInteger(line, 16));
        } catch (IOException e) {
            System.err.println(e);
            last = e;
        }
        return result;
    }
    private CspInteger command(String chan, String op, BigInteger arg) {
        final String req = chan + " " + op +
                           (arg == null ? "" : (" " + arg.toString(16)));
        return command(req);
    }

    private CspInteger command(String chan, String op) {
        return command(chan, op, null);
    }

    protected CspInteger probe(String chan) {
        return command(chan, "#");
    }

    protected CspInteger peek(String chan) {
        return command(chan, "#?");
    }

    protected CspInteger send(String chan, BigInteger val) {
        return command(chan, "!", val);
    }

    protected CspInteger receive(String chan) {
        return command(chan, "?");
    }

    protected CspInteger run(String cmd) {
        return command("RUN " + cmd);
    }

    protected synchronized void close() throws IOException {
        if (!socket.isClosed()) {
            socket.close();
        }
    }
}
