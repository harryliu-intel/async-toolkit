/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */
package com.avlsi.tools.aspice;
import com.avlsi.file.common.HierName;
import java.util.ArrayList;
import java.util.Iterator;
import java.io.FileReader;
import java.io.BufferedReader;
/**
 * Class for Sinusoidal Voltage Source
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class PWLVoltageSource extends VoltageSource{

    /** Holds all of the events **/
    private SimpleQueue queue;
   
    /** Delay before starting up the queue **/
    private double td;

    
    /** Builds a sine voltage source device 
     *
     * @param name HierName of the device
     * @param nplus positive terminal node
     * @param nminus negative terminal node
     * @param vt Double array containing [ t1 v1 t2 v2 t3 v3 ... tn vn ]
     * @param repeat  time in units of seconds which specifies the start point
     * of the waveform which is to be repeated.  Set negative for no repeat.
     * @param td Time delay before beginning the piece-wise linear
     *           variation in seconds. 
     * **/
    public PWLVoltageSource(final HierName name,
                              final Node nplus,
                              final Node nminus,
                              final double[] tv,
                              final double repeat,
                              final double td) {
        super(name, nplus, nminus);
        this.td = td;
        this.queue = new SimpleQueue();
        int count = 0;
        double lastTime = 0;
        while (count < tv.length) {
            double currShift = 0, currValue=0;
            if (count == tv.length) {
                System.out.println("Warning: PWL voltage source found voltage"+
                                   " value without matching time value.");
            } else {
                if (tv[count] < lastTime) {
                    System.out.println("Warning: PWL voltage source expects"+
                                       " monotonic increasing time values,"+
                                       " results may be affected.");
                }
                currShift= tv[count] - lastTime;
                count++;
                currValue = tv[count++];
            }
            queue.add(new Event(currValue, currShift));
            lastTime += currShift;
        }
        if (repeat >= 0)
            queue.setRepeat(repeat);
    }
    
    public PWLVoltageSource(final HierName name,
                              final Node nplus,
                              final Node nminus,
                              final String filename,
                              final double repeat,
                              final double td)
            throws java.io.FileNotFoundException,java.io.IOException {
        this(name, nplus, nminus, getDoubles(filename), repeat, td);
    }

    private static int splitLine(double[] parts, String line) {
        if ((line == null) || (line.length()==0)) return -3;
        if (line.charAt(0) == '#') return -3;
        int start=-1,end=-1,count=0;
        for (int loop=0;loop<line.length();loop++) {
            char curr = line.charAt(loop);
            if (!(Character.isWhitespace(curr))) {
                if (start ==-1) start = loop;
            }
            if ((Character.isWhitespace(curr)) ||
                (loop == (line.length() -1)) ){ //end a double
                if ((start >=0) && (end == -1)) end =loop;
            }
            if ((start >=0) && (end >=0)) {
                if (start == -1) continue;
                try {
                    if (count == 2) return -2;
                    parts[count++] = 
                        Double.parseDouble(line.substring(start,loop+1));
                    start=-1; end=-1;
                } catch (NumberFormatException e) {
                    return -999;
                }
            }
        }
        if (count != 2) return -1;
        return 1;
    }

    private static final int initsize = 16;
    private static double[] getDoubles(String filename)
            throws java.io.FileNotFoundException,java.io.IOException {
        double[] vect = new double[initsize];
        FileReader fr = new FileReader(filename);
        BufferedReader br = new BufferedReader(fr);
        int linenum =0, count=0;
        double[] parts = new double[2];
        String line ="";
        while ( (line = br.readLine()) != null) {
            int result = splitLine(parts,line);
            if (result == -3) continue; //Comment case
            if (result ==-999)
                System.out.println("PWL: Number Format Error on line "+linenum+
                                   "\n\t"+line+"\nignoring line");
            if (result == -2)
                System.out.println("PWL: Too many arguments on line "+linenum+
                                   "\n\t"+line+"\nignoring line");
            else if (result == -1)
                System.out.println("PWL: Too few arguments on line "+linenum+
                                   "\n\t"+line+"\nignoring line");
            else {
                vect[count++] = parts[0];
                vect[count++] = parts[1];
                if (vect.length == count) { //Grow array
                    double[] temp = new double[2*vect.length];
                    System.arraycopy(vect,0,temp,0,vect.length);
                    vect = temp;
                }
            }                      
            linenum++;
        }
        double[] temp = new double[count];
        System.arraycopy(vect,0,temp,0,count);
        return temp;
    }
    
    public double ramp(Event curr, Event last, double time) {
        double run = curr.tshift;
        if (run == 0) return last.value;
        double rise = curr.value - last.value;
        double slope = rise/run;
        double shift = time - queue.total;
        return last.value + slope*(shift);
    }
    
    public double getDrivenVoltage(double time,double timestep) {
        if (time <= td) return 0;
        Event curr = queue.getCurrent();
        time -= td;
        if (time <= queue.total+curr.tshift) 
            return ramp(curr, queue.getLast(), time);
        Event newCurr = queue.getNext();
        if (newCurr.equals(curr)) return newCurr.value;
        return ramp(newCurr, curr,time);
    }

    private class SimpleQueue extends ArrayList {

        private int index;
        private int repeat;
        private Event last;
        private double total;
        public SimpleQueue() {
            this.index = 0;
            this.repeat = -1;
            this.last = null;
            total = 0;
        }


        /** This function assumes the queue has already been built **/
        public void setRepeat(double rtime) {
            Event last = null;
            int count = 0;
            double rtotal=0;
            for(Iterator i = iterator();i.hasNext();) {
                Event e = (Event) i.next();
                rtotal += e.tshift;
                if (rtotal > rtime) {
                    //add the new event
                    repeat = count;
                    if (last != null) {
                        //Insert a new repeat point
                        Event newEvent = 
                            new Event( ramp(e, last,rtime-(rtotal-e.tshift)),
                                       rtime-(rtotal-e.tshift));
                        add(repeat, newEvent);
                        e.tshift -= newEvent.tshift;
                    }
                    return;
                } else {
                    count++;
                    last = e;
                }
            }
            repeat = 0;
        } 

        public Event getCurrent() {
            return (Event) get(index);
        }

        public Event getLast() {
            return last;
        }

        public Event getNext() {
            if (size() == 0) return null;
            last = (Event) get(index);
            total += last.tshift;
            index++;
            if (index == size()) { 
                if (repeat == -1) {
                    index--;
                } else 
                    index = repeat;
            }
            return (Event) get(index);
        }
    }

    private class Event {
        public final double value;
        public double tshift;
        public Event(double v, double t) { value=v; tshift = t; }
    }
}

