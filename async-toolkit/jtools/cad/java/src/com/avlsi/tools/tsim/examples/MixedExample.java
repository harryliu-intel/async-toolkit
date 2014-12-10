/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

// vim:sw=4:expandtab:ai:cin:ts=4

package com.avlsi.tools.tsim.examples;

import com.avlsi.tools.dsim.DigitalScheduler;
import com.avlsi.tools.tsim.*;
import com.avlsi.util.cmdline.CmdLine;
import com.avlsi.util.cmdline.CmdCommand;
import com.avlsi.util.cmdline.CmdModule;
import com.avlsi.util.debug.Debug;

/**
 * <p> Interface between the command-line interpreter and DSim simulator. </p>
 *
 * <p> This should be run with the following .cast file:
 * <pre>
 * import "add/add.cast";
 *
 * e1of4 a[0..15],b[0..15],out[0..15];
 * e1of2 cin,cout;
 * ADD_32() foo(a, b, out, cin, cout);
 * </pre> 
 * </p>
 *
 * @author Aaaron Denney
 * @author Kim Wallmar
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 *
 * @review kiniry - Under review beginning 23 July 2002.
 **/

public class MixedExample extends CmdLine {

    NodeWriteChannel a, b, cin;
    NodeReadChannel  s, cout;
    testadder d;

    public MixedExample() { 
        super(false); 
        addCommands(commands);
    }

    public String getName() { return "MixedExample"; }
    
    public String getHelpMsg() { return "\tMixedExample/\ttesting module"; }

    public void install(CmdModule parent, String args[]) {
    }

    CmdCommand commands[] = {
        new CmdCommand("attach", "attach", "connects up java channels to dsim nodes") {
            public void execute(String args[]) { 
                attach();
            }
        },
        new CmdCommand("go", "go", "starts sending on the two channels") {
            public void execute(String args[]) { 
                go();
            }
        },
        new CmdCommand("kill", "kill", "kill the java interacter") {
            public void execute(String args[]) { 
                kill();
            }
        },
    };

    private void go() {
        d.start();
    }

    private void kill() {
        d.interrupt();
        try {
            d.join();
        } catch (InterruptedException e) {
            System.err.println("Who interrupted us?");
        }
        a = b = cin = null;
        s = cout = null;
        d = null;
    }

    private void attach() {
        // Attach to all channels of adder.
        a = new NodeWriteChannel("a", 4, 16);
        b = new NodeWriteChannel("b", 4, 16);
        s = new NodeReadChannel("out", 4, 16);
        cin = new NodeWriteChannel("cin", 2);
        cout = new NodeReadChannel("cout", 2);
        d = new testadder();
    }

    private class testadder extends AbstractDevice {
        ChannelOutput [] in = new ChannelOutput [] { a, b, cin };
        ChannelInput [] out = new ChannelInput [] { s, cout };
        int [] values = new int [3];
        public void go() throws InterruptedException {
            values[2] = 0;
            for (int i = 0; i < 10; i++) {
                values[0] = i;
                for (int j = 0; j < i; j++) {
                    values[1] = j;
                    System.out.println("Sending: a="+values[0]+" b="+values[1]);
                    send(in, values); 
                    System.out.println("receiving");
                    Message [] rv = receive(out);
                    System.out.println("received:" + rv[0] + ", " + rv[1]);
                }
            }
        }

        public void cleanup() {
            a.destroy();
            b.destroy();
            s.destroy();
            cin.destroy();
            cout.destroy();
        }
    }

} // end of class MixedExample

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */

