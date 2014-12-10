/*
 * Copyright 2000 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.tsim;

/**
 * <p> Initial infrastructure for benchmarking TSim. </p>
 *
 * @author Dan Daly
 * @version $Revision$ $Date$
 *
 * @review kiniry - Under review beginning 22 July 2002.
 **/

public class BenchMarkTsim {
    /**
     * This class should not be instantiated.
     **/
    private BenchMarkTsim() { }

    static final int duration = 0xFFF000;
    static final int num_dev = 1;
    public static void main(String[] args) {

        AbstractDevice[] A = new AbstractDevice[num_dev];
        AbstractDevice[] B = new AbstractDevice[num_dev];
        for (int loop=0;loop<num_dev;loop++) {
            A[loop] = new AbstractDevice("A"+loop, false) {

                public void go() throws InterruptedException {

                    while (true) {
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
                        //System.out.print(">");
                    }
                }
            };

            B[loop] = new AbstractDevice("B"+loop, false) {

                public void go() throws InterruptedException {

                    while (true) {
                        if (com.avlsi.tools.dsim.DigitalScheduler
                                .get().getTime() > duration) {
                            //System.out.println("");
                            System.exit(1);
                        }
                        wait(10);
                        //System.out.print(".");
                    }
                }
            };

            A[loop].start();
            B[loop].start();
        }
        com.avlsi.tools.dsim.SchedulerRunner.get().start();
    }
                    
} // end of class BenchMarkTsim

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
