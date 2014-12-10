/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim.test;

import com.avlsi.util.cmdline.CmdLine;
import com.avlsi.util.cmdline.CmdCommand;
import com.avlsi.tools.tsim.NodeReadChannel;
import com.avlsi.tools.tsim.NodeWriteChannel;
import com.avlsi.tools.tsim.Message;
import com.avlsi.tools.dsim.DSimUtil;
import com.avlsi.tools.dsim.DigitalScheduler;


public class NodeChannelTest extends CmdLine {
    public NodeChannelTest() {
	super(false);
	addCommands(commands);
    }

    public String getName() {
	return "NodeChannelTest";
    }

    CmdCommand commands[] = {
	new CmdCommand("test", "test chan_name (size)",
			"tests Node{Write,Read}Channels on chan") {
	    public void execute(String args[]) {
		int size;
		if (args != null) {
		    if (args.length == 1)
			size = 2;
		    else if (args.length == 2)
			size = Integer.parseInt(args[1]);
		    else {
			System.out.println("usage: test chan (size)");
			return;
		    }
		    runNodeTest(args[0], size);
		} else {
		    System.out.println("usage: test chan (size)");
		}
		return;
	    }
	}
    };

    private void runNodeTest(String chan_name, int size) {
        // Assuming e1of4 channels.
        NodeReadChannel nrc = new NodeReadChannel(chan_name, 4);
	NodeWriteChannel nwc = new NodeWriteChannel(chan_name, 4);

        System.out.println("about to reset");
        (new DSimUtil()).resetDSim(DSimUtil.STANDARD_RESET);
        System.out.println("reset");
        

	int i;

	DigitalScheduler.get().incThreadCount();  // this thread
        makeReadThread(size, nrc);
        startCycling();
	for (i = 0; i < size; i++) {
	    System.out.println("Test: Sending " + i);
	    send(i, nwc);
        }
	DigitalScheduler.get().decThreadCount();
    }

    class CycleThread implements Runnable {
        public void run() {
            System.out.println("one cycle");
            DigitalScheduler.get().cycle();
            System.out.println("done cycling");
        }
    }


    class ReadThread implements Runnable { 
	final int size;
	final NodeReadChannel nrc;

	ReadThread(int size, NodeReadChannel nrc) {
	    this.size = size;
	    this.nrc = nrc;
	}

	public void run() {
	    System.out.println("Starting Read Thread");
	    int i;
	    DigitalScheduler.get().incThreadCount();  // the read thread
	    for (i = 0; i < size; i++) {
		Message m = receive(i, nrc);
		System.out.println("Test:  Received " + m.intValue());
	    }
	    DigitalScheduler.get().decThreadCount();
	}
    }

    private void startCycling() {
        new Thread(new CycleThread()).start();
    }

    private void makeReadThread(int size, NodeReadChannel nrc) {
	new Thread(new ReadThread(size, nrc)).start();
    }

    private void send(int i, NodeWriteChannel nwc) {
	while (true) {
	    try {
		nwc.send(new Message(i), i);
		return;
	    } catch (InterruptedException e) {
		System.out.println("Test: Interrupted in send.  Retrying.");
	    }
	}
    }

    private static Message receive(int i, NodeReadChannel nrc) {
	while (true) {
	    try {
		final Message m = nrc.receive(i);
		return m;
	    } catch (InterruptedException e) {
		System.out.println("Test: Interrupted in receive.  Retrying.");
	    }
	}
    }
}
		
