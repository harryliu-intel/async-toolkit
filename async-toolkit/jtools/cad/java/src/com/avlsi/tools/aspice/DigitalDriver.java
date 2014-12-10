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
import java.util.ArrayList;
import com.avlsi.tools.dsim.NodeWatcher;
import com.avlsi.tools.dsim.DSim;
/**
 * Class for Driving a node with the activity of a node
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class DigitalDriver implements Node.NodeDriver,NodeWatcher {

    private ArrayList inputs;
    private double currV;
    private double prstau;
    /**
     * Constructor.
     **/
    public DigitalDriver(double prstau) {
        inputs = new ArrayList();
        currV = 0;
        this.prstau = prstau;
    }

    public void nodeChanged(com.avlsi.tools.dsim.Node node, long time) {
        double newtime = (time*5e-9d)/10000d;
        char val = node.toString().charAt(node.toString().length()-1);
        int num = 2;
        switch (val) {
          case '1': num = 1;
                   break;
          case '0': num = 0;
                   break;
        }
        inputs.add(new Event(newtime,num));
        System.out.println("Got new event: "+newtime+" "+node+" len= "+inputs.size());
    }   

    public double getDrivenVoltage(double time) {
        if (inputs.isEmpty()) return currV;
        double toptime = ((Event)inputs.get(0)).time;
        if (toptime <= time) {
            switch(((Event) inputs.remove(0)).num ) {
                case 0:
                    currV = 0.0;
                    break;
                case 1:
                    currV = 1.8;
                    break;
                default:
                    System.out.println("Error in D to A");
            }
            //System.out.println("Driving "+currV+" V at "+time);
        }
        return currV;
    }  

    public double getDrivenTau() {
        return prstau;
    }

    private class Event {
        public double time;
        public int num;

        public Event(double t, int num) {
            this.time = t;
            this.num = num;
        }
    }
}

