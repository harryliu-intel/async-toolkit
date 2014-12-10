/*
 * Copyright 2000, 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim.examples;

import com.avlsi.tools.sigscan.ChannelSigscan;
import com.avlsi.tools.sigscan.DebugOpts;
import com.avlsi.tools.sigscan.IntAttribute;
import com.avlsi.tools.sigscan.Sigscan;
import com.avlsi.tools.sigscan.SigscanException;
import com.avlsi.tools.sigscan.StringAttribute;
import com.avlsi.tools.sigscan.TransactionType;
import com.avlsi.tools.tsim.*;
import java.math.BigInteger;

/**
 * <p> Class for testing <code>SharedBuses</code>. </p>
 *
 * @author Dan Daly
 * @version $Revision$ $Date$
 *
 * @review kiniry - Under review beginning 29 July 2002.
 **/

public class BusExample extends ClockedDevice 
    implements BusWatcher, BusDriver {

    protected final SharedBus data;
    
    /**
     * Constructor.
     **/
    public BusExample(String scopename, String name,
                      /*Clock ck, */SharedBus data,// SharedBus control,
                      Sigscan sigscan) {
        super(scopename, name, sigscan, new DebugOpts().logAllSST());
        this.data = data;
        data.addWatch(this);
    }

    public void go() throws InterruptedException {
        System.out.println("Should not be called.");
    }

    private int count=0;
    
    public void busChanged(SharedBus bus, long time) {
        if (count < 16) {
            bus.set(this, count++, 10);
        }
    }

    public static void main(String[] args) {
        ChannelSigscan sigscan=null;
        try {
            sigscan = new ChannelSigscan("bus");
            //SharedBus.setSigscanDefaults(sigscan,"bus");
            sigscan.start();
        } catch (SigscanException e) {
            System.out.println(e.getMessage());
            System.exit(1);
        }
        SharedBus data = new SharedBus("bus","data",8,sigscan);
        BusExample be = new BusExample("bus", "be", data, sigscan);
        data.set(be, 8, 0);
        new SchedulerRunner().start();
    }

//    public static int numRails = 0;
//
    
/*
    public static void main(String[] args) {
        ChannelSigscan sigscan=null;
        try {
            sigscan = new ChannelSigscan("bus");
            //SharedBus.setSigscanDefaults(sigscan,"bus");
            sigscan.start();
        } catch (SigscanException e) {
            System.out.println(e.getMessage());
            System.exit(1);
        }
        Clock clock = new Clock("bus","clock",10,sigscan);
        SharedBus data = new SharedBus("bus","data",8,sigscan);
        SharedBus control = new SharedBus("bus", "control",1,sigscan);
        BusExample left = new BusExample("bus", "left", clock, data, control,
                                         sigscan) {

            public void go() throws InterruptedException {
                int count = 0;
                int halfcycle = 10;
                int bit = 0;
                TransactionType send = 
                    newTransactionType(BEGINEND, "send");
                while (count < 10) {
                    //bus.set(this, 1, halfcycle);
                    //bus.set(this, 0, halfcycle*2);
                    ck.wait(SharedBus.NEGEDGE);
                    beginTransaction(send, "sending "+count,"count="+count);
                    data.set(this, count,0);
                    //updateTime(DigitalScheduler.get().getTime());
                    //output("Current Time = "+
                    //    com.avlsi.tools.dsim.DSim.get().getTime());
                    ck.wait(SharedBus.POSEDGE);
                    endTransaction();
                    count++;
                }
                System.exit(1);
            }
        };
        BusExample right = new BusExample("bus", "right", 
                                        clock, data, control,
                                        sigscan) {
            public void go() throws InterruptedException {
                int count = 0;
                int halfcycle = 10;
                int bit = 0;
                TransactionType receive = 
                    newTransactionType(BEGINEND, "receive");
                while (count < 10) {
                    //bus.set(this, 1, halfcycle);
                    //bus.set(this, 0, halfcycle*2);
                    ck.wait(SharedBus.POSEDGE);
                    BigInteger rec = data.getBigIntegerData();
                    beginTransaction(receive, "received "+rec,
                                     "count="+count);
                    wait(ck.getCycleTime() % 4);
                    control.set(this, count % 2, 0);
                    //updateTime(DigitalScheduler.get().getTime());
                    //output("Current Time = "+
                    //    com.avlsi.tools.dsim.DSim.get().getTime());
                    ck.wait(SharedBus.NEGEDGE);
                    endTransaction();
                    count++;
                }
            }
        };
        com.avlsi.tools.dsim.DigitalScheduler.get().setVerbose(true);
        left.start();
        right.start();
        clock.start();
        new SchedulerRunner().start();
    }
      */              
/*
    public static void mainold(String[] args) {
        try {
            ChannelSigscan sigscan = new ChannelSigscan("bus");
            AbstractDevice.setSigscanDefaults(sigscan, "bus",
                                              AbstractDevice.LOG_ALL_SST);
            SharedBus.setSigscanDefaults(sigscan,"bus");
            sigscan.start();
        } catch (SigscanException e) {
            System.out.println(e.getMessage());
            System.exit(1);
        }

        try {
            numRails = Integer.parseInt(args[0]);
        } catch (NumberFormatException e) {
            System.out.println("Num rails must be an integer.");
            System.exit(1);
        }

        System.out.println("Number of Rails= "+numRails);
        
        data = new Data[args.length-1];
        for (int loop=1;loop<args.length;loop++) {
            int num = 0;
            try {
                num = Integer.parseInt(args[loop]);
            } catch (NumberFormatException e) {
                System.out.println("Not a number ("+args[loop]+"), sending"+
                                   " zero instead.");
                num=0;
            }
            if ((num > numRails-1) || (num < 0)) num = 0;
            data[loop-1]= new Data(Integer.toBinaryString(1<<num)+"Z");
        }
        SharedBus bus1 = new SharedBus("bus",numRails+1);
        BusExample left = new BusExample("left", bus1) {
            
            public void go() throws InterruptedException {
                output("Starting "+name);
                super.bus.set(this, "0Z", 0);
                TransactionType sending = 
                    newTransactionType(BEGINEND,"sending");
                StringAttribute data_attr = 
                    new StringAttribute("data",true,sending);
                
                for (int loop=0;loop<data.length;loop++) {
                    nextCycle();
                    setState("sending "+BusExample.data[loop]);
                    beginTransaction(sending,
                                     "sending "+BusExample.data[loop],"");
                    data_attr.set(BusExample.data[loop].toString());
                    super.bus.set(this, data[loop], 0);
                    char bit = 'Z';
                    while (bit != '1') {
                        nextCycle();
                        bit = super.bus.get().getCharAt(0);
                        output("bit = "+bit+" data= "+super.bus.get());
                    }
                    endTransaction();
                    output("Received ack.");
                    setState("Lowering Rails");
                    super.bus.set(this,"0Z",0);
                    bit = 'Z';
                    while ( bit != '0') {
                        nextCycle();
                        bit = super.bus.get().getCharAt(0);
                        output("bit = "+bit+" data= "+super.bus.get());
                    }
                }
                System.exit(0);
            }
        };
        BusExample right = new BusExample("right", bus1) {

            public void go() throws InterruptedException {
                String Zrails = Data.pad(numRails,'Z');
                TransactionType receiving = 
                    newTransactionType(BEGINEND,"receiving");
                IntAttribute data_attr = 
                    new IntAttribute("data",true,receiving);
                while (true) {
                    int rails =0;
                    String railstr = "";
                    setState("Waiting for input");
                    while (rails == 0) {
                        nextCycle();
                        try {
                            railstr = super.bus.getStringSection(1,numRails);
                            rails = Integer.parseInt(railstr,2);
                        } catch (NumberFormatException e) {
                            outerr("Rails Weird: "+railstr);
                        }
                    }
                    output("Got data "+rails+" binary= "+railstr);
                    beginTransaction(receiving, "receiving "+railstr,"");
                    int rec = 0;
                    while (rails != 1) {
                        rec++;
                        rails = rails >> 1;
                    }
                    data_attr.set(rec);
                    super.bus.set(this,Zrails+"1",0);
                    setState("Acking");
                    while (rails != 0) {
                        nextCycle();
                        try {
                            railstr = super.bus.getStringSection(1,numRails);
                            rails = Integer.parseInt(railstr,2);
                            output("rails= "+rails);
                        } catch (NumberFormatException e) {
                            outerr("Rails Weird: "+railstr);
                        }
                    }
                    endTransaction();
                    super.bus.set(this,Zrails+"0",0);
                }
            }
        };
        left.start();
        right.start();
    }
                                 */

} // end of class BusExample

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
