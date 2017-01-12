/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.cmdline;
import java.io.InputStream;
import java.net.Socket;
import java.net.ServerSocket;
//import sun.misc.*;

public class Signal implements sun.misc.SignalHandler {
    public interface Handler {
        void execute() ;
    }
    /// ** Interface to pass in to <code>setHandler</code> restore default handler. **/
    public static final Handler DEFAULT = new Handler() { public void execute() {} };
    //public static final sun.misc.SignalHandler DEFAULT = sun.misc.SignalHandler.SIG_DFL;
    /** Interface to pass in to <code>setHandler</code> ignore a signal. **/
    public static final Handler IGNORE = new Handler() { public void execute() {} };
    /** Valid signal types to pass into <code>setHandler</code> function**/
    private static sun.misc.Signal getSignal(String s) {
        try {
            return new sun.misc.Signal(s);
        } catch (IllegalArgumentException e) {
            // HUP is not a valid signal on Windows
            return null;
        }
    }
    public static final sun.misc.Signal SIGINT = getSignal("INT");
    public static final sun.misc.Signal SIGHUP = getSignal("HUP");
    static sun.misc.Signal signals[] = {
        SIGINT, SIGHUP
    };
    private Handler handlers[];
    private static final Signal singleton = new Signal();
    /** Returns singleton instance. **/    
    public static Signal get() { return singleton; }
    private Signal() {
        handlers = new Handler[signals.length];
        for (int i=0; i<signals.length; i++) {
            handlers[i]=IGNORE;
        }
    }

    private int GetSignalIndex(sun.misc.Signal sig) {
        int signum;
        for (signum=0; signum<signals.length; signum++) {
            if (sig==signals[signum]) { return signum; }
        }
        if (signum<signals.length) { return signum; }
        return -1;
    }
    public void handle(sun.misc.Signal sig) {
        int signum = GetSignalIndex(sig);
        if (signum>=0) { handlers[signum].execute(); }
    }
    /** Method that sets/restores handling of signal <code>sig</code> with 
        handler <code>h</code>. See above for special handlers to ignore
        or reset the default handler. WARNING: this function may be called
        multiple times per signal.
     **/
    public void setHandler(sun.misc.Signal sig, Handler h) {
        int signum = GetSignalIndex(sig);
        if (signum>=0) {
            handlers[signum]=h;
            if (h==DEFAULT) {
                sun.misc.Signal.handle(sig, sun.misc.SignalHandler.SIG_DFL);
            } else {
                sun.misc.Signal.handle(sig, this);
            }
        }
    }
}
