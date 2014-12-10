/*
 * Copyright 2000 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim.test;

import com.avlsi.tools.tsim.*;

/**
 * @todo Undocumented.
 *
 * @author Dan Daly
 * @version $Revision$ $Date$
 *
 * @review kiniry - Under review beginning 29 July 2002.
 **/

public class TestDevice extends AbstractDevice {

    //private BufferedChannel input, output;
    //Using Waits for now

    private static int duration = 0xFFF000;
    
	public TestDevice(String name, boolean suppressOutput//,
			  //BufferedChannel input, BufferedChannel output) {
              ) {
		super(name, suppressOutput);
		//this.input = input;
		//this.output = output;
	}

	private void doSomeSending(int count) 
            throws InterruptedException {
		for(int loop=0;loop<count;loop++) {
			//send(output, in);
            System.out.print("l");
            wait(2);
		}
	} 

	public void go() throws InterruptedException {
		
		while (true) {
			//Message m = receive(input);
            wait(10);

			//System.out.println("I got "+message);
            System.out.print("M");
			//if (m.intValue() == 10) {
            if (false) {
				System.out.print(
				"Gee Not Sending at 10");
			} else {
				doSomeSending(3);
				//System.out.println(
				//"Sending some junk");
                System.out.print("J");
			}
			
			int a = 5;  //Computation
			a += 2;     //

			//send(output, a);
            wait(a);
			//System.out.println("Finished Sending "+a);
            System.out.print("a");
            if (com.avlsi.tools.dsim.DigitalScheduler.get()
                .getTime() > duration) {
                System.out.println("");
                System.exit(1);
            }
		}
	}

    public static void main(String[] args) {

        TestDevice t = new TestDevice("test1", false);

        t.start();
        com.avlsi.tools.dsim.SchedulerRunner.get().start();
    }

} // end of class TestDevice

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */

