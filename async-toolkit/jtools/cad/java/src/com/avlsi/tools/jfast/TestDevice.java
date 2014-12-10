/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id:$
 */

package com.avlsi.tools.jfast;

/**
 * Class for ...
 *
 * @author Dan Daly
 * @version $Date:$
 **/
public class TestDevice extends AbstractDevice {

	//private BufferedChannel input, output;
    
	private static int duration = 0xFFF000;
    

	public TestDevice(String name, boolean suppressOutput//,
			  //BufferedChannel input, BufferedChannel output) {
              ) {
		super(name, suppressOutput);
		//this.input = input;
		//this.output = output;
	}

	private int  __tempVal_doSomeSending_loop;
	private int  __tempParam_doSomeSending_count;
    private int doSomeSending_for_1_pc=0;
	private void doSomeSending(int count)
            throws InterruptedException {
		__tempParam_doSomeSending_count = count;
        switch(doSomeSending_for_1_pc) {
          case 0:
                __tempVal_doSomeSending_loop=0;
          case 1:
                if (doSomeSending_for_1_pc == 1)
                    __tempVal_doSomeSending_loop++;
                doSomeSending_for_1_pc = 1;
                if (!( __tempVal_doSomeSending_loop <
		             __tempParam_doSomeSending_count)) {
                    //System.out.println(__tempVal_doSomeSending_loop+"<"+
                    //                   __tempParam_doSomeSending_count);
                    break;
                } else pc--; //Return here next time pc
       			//send(output, in);
                //System.out.print("l");
                Wait(2);
		}
        doSomeSending_for_1_pc = 0;
	} 

	//private Message __tempVal_go_m;

	private int  __tempMeth_go_If_else_1_pc = 0;
	private void __tempMeth_go_If_else_1()
            throws InterruptedException {
		switch (__tempMeth_go_If_else_1_pc) {
			case 0:
				doSomeSending(3);
			case 1: __tempMeth_go_If_else_1_pc = 1;
				//System.out.println(
				//"Sending some junk");
                //System.out.print("J");
			default: __tempMeth_go_If_else_1_pc= 0;
		}
	}
	
	private int __tempVal_go_a;	
	public void go() throws InterruptedException {
		
		while (true) {
            //System.out.println("pc= "+pc);
			switch(pc) {
			case 0:
				//__tempVal_go_m = receive(input);
                Wait(10);
			case 1: pc = 1;
				//System.out.println("I got "+message);
                //System.out.print("M");
				//if (m.intValue() == 10) {
                if (false) {
					//System.out.println(
					//"Gee Not Sending at 10");
				} else {
					pc = 1*METHMASK+2;
					continue;
				}
				
			case 2: pc = 2;	
				__tempVal_go_a = 5;  //Computation
				__tempVal_go_a += 2;     //

				//send(output, a);
                Wait(__tempVal_go_a);
			case 3: pc = 3;
				//System.out.println("Finished Sending "+a);
                //System.out.print("a");
                if (com.avlsi.tools.dsim.DigitalScheduler.get()
                    .getTime() > duration) {
                    //System.out.println("");
                    System.exit(1);
                }
			default: pc = 0;
				break;
			case 1*METHMASK+2: 
				__tempMeth_go_If_else_1();
				pc = pc & (METHMASK-1);
				break;
            }
		}
	}

    public static void main(String[] args) {

        TestDevice t = new TestDevice("test1", false);

        t.start();
        com.avlsi.tools.dsim.SchedulerRunner.get().start();
    }

}

