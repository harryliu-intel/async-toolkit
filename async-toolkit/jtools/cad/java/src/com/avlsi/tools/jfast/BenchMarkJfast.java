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
public class BenchMarkJfast {
    /**
     * Constructor.
     **/

    static final int duration = 0xFFF000;
    static final int num_dev = 1;

    public static void main(String[] args) {
        
        AbstractDevice[] A = new AbstractDevice[num_dev];
        AbstractDevice[] B = new AbstractDevice[num_dev];
        for (int loop=0;loop<num_dev;loop++) {

            A[loop] = new AbstractDevice("A"+loop, false) {

                public void go() throws InterruptedException {
                    while (true) {
                        switch(pc) {
                          case 0:
                            double accum = 1;
                            double b = 39;
                            for ( int i=0; i<1 ; ++i ) {
                              accum = 1;
                              b = b + 1 ;
                              while ( b > 0 ) {
                                accum *= b;
                                --b;
                              }
                            }

                            //System.out.print("-");
                            wait(3);
                          case 1:   pc = 1;
                            //System.out.print(">");
                          default:  pc = 0;
                        }
                    }
                }
            };

            B[loop] = new AbstractDevice("B"+loop, false) {

                public void go() throws InterruptedException {

                    while (true) {
                        switch(pc) {
                          case 0:
                            if (com.avlsi.tools.dsim.DigitalScheduler
                                    .get().getTime() > duration) {
                                //System.out.println("");
                                System.exit(1);
                            }
                            wait(10);
                          case 1:   pc = 1;
                            //System.out.print(".");
                          default:  pc = 0;
                        }
                    }
                }
            };
            //com.avlsi.tools.dsim.DigitalScheduler.get().setVerbose(true);
            A[loop].start();
            B[loop].start();
        }
        com.avlsi.tools.dsim.SchedulerRunner.get().start();
    }
                    
}


