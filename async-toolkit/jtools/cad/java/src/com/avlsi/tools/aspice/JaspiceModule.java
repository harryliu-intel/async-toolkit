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

import com.avlsi.util.cmdline.CmdLine;
import com.avlsi.util.cmdline.CmdCommand;
import com.avlsi.util.cmdline.CmdModule;
import com.avlsi.util.cmdline.Signal;
import java.util.Iterator;
import com.avlsi.tools.dsim.DSim;
import com.avlsi.tools.dsim.DSimSynchronizer;
import com.avlsi.file.common.HierName;
/**
 * Class for running Jaspice throught the CmdLine interface.  See 
 * <a href="http://internal.avlsi.com/technical/docs/flow/tools/simulation/asim/jaspice_user_manual.shtml"
 * Owner's Manual</a> for more details on each command.
 *
 * @author Dan Daly
 * @version $Date$
 **/
public class JaspiceModule extends CmdLine{

    /** Jaspice Singleton Object **/
    private Jaspice ja = null;
    /** TraceFile Callback **/
    private DefaultCallback callback;
    
    /** Interrupt handler. Catches Ctrl-C and tells the simulator to stop. **/
    Signal.Handler intHandler = new Signal.Handler() {
        public void execute() {
            DSim dsim = DSim.get();
            if (dsim!=null) { dsim.interrupt(); }
        }
    };

    /** When run from the shell can also specify a setup file **/
    public void install(CmdModule parent, String args[]) {
         // process any arguments from the invocation command line
        if (args==null || args.length!=1) { 
            System.out.println("Usage: jaspice file.setup");
            return; 
        }
        routeCommand("/source " + args[0]);
    }

    /**
     * Constructor
     **/
    public JaspiceModule() {
        super(false);
        this.ja = Jaspice.get();
        callback = new DefaultCallback();
        ja.setCallback(callback);
        addIntHandler(intHandler);
        addCommands(commands);
    }

    public String getName() { return "Jaspice"; }
    public String getHelpMsg() { return "\tJaspice/\t\tanalog simulation"+
                                        " module"; }
    CmdCommand commands[] = {

        /*******************************************************************
        **  Loads the cdl or spice file into Jaspice with a specified
        **  isntance name
        ********************************************************************/

        new CmdCommand("attach", "attach <filename> <cell_type>"+
                                 " [instance_hierarchical_name]",
                       "Builds a Jaspice circuit from <filename> with"+
                       " instance name <instance_name>") {
            public void execute(String args[]) {
                if (args != null && (args.length==2 || args.length==3)) {
                    try {
                        String hiername = "";
                        if (args.length ==3) hiername = args[2];
                        
                        ja.buildCircuit(args[0],  //filename
                                        args[1],  //celltype
                                        hiername   //HierName+instName
                                        ); 
                    } catch (JaspiceException e) {
                        System.out.println("ERROR, JaspiceException: "+
                                           e.getMessage());
                    }
                } else
                    System.out.println("Usage: "+this.getUsage());
            }
        },

        /**********************************************************************
        **  Aliases a node name in Jaspice with a new name
        *********************************************************************/

        new CmdCommand("alias", "alias <hierarchical_node_name>"+
                                " <top_level_alias>",
                       "Connects the <node_name> with a new name <alias>") {
            public void execute(String args[]) {
                if (args != null && args.length==2) {
                    if (ja.isRunning()) {
                        System.out.println(
                                "Simulation started, aliases are frozen.");
                    } else {
                        Node node = ja.findNode(args[0]);
                        if (node == null) {
                            System.out.println("Node "+args[0]+" not found.");
                            return;
                        }
                        try {
                            ja.aliasNode(node,args[1]);
                        } catch (JaspiceException e) {
                            System.out.println(e.getMessage());
                        }
                    }
                    
                } else
                    System.out.println("Usage: "+this.getUsage());
            }
        },

        /**********************************************************************
        **  Sets static variables within Jaspice
        *********************************************************************/

        new CmdCommand("set", "set <param_name> <value>",
                       "Set a Jaspice property with name <param_name>"+
                       " to <value>") {
            public void execute(String args[]) {
                if (args != null && args.length==2) {
                    if (ja.isRunning()) {
                        System.out.println(
                                "Simulation started, parameters are frozen.");
                    } else {
                        try {
                            ja.set(args[0],args[1]);
                        } catch (Jaspice.PropertyException e) {
                            System.out.println(args[0]+" not found : "+
                                               e.getMessage());
                        }
                    }
                } else
                    System.out.println("Usage: "+this.getUsage());
            }
        },
        
        /**********************************************************************
        **  Lists all of the static variables in Jaspice and their values
        *********************************************************************/

        new CmdCommand("set?", "set? [param_name]",
                       "Print the value of Jaspice property [param_name],"+
                       " or print out all propertiesand their values") {
            public void execute(String args[]) {
                if ((args == null) || (args.length==1)){
                    if(args == null) {
                        for(final Iterator i = ja.getPropertyKeys().iterator();
                                           i.hasNext();) {
                            String key = (String)i.next();
                            try {
                                System.out.println(
                                    ja.printProperty(key));
                            } catch (Jaspice.PropertyException e) {}
                        }
                    } else {
                        try {
                            System.out.println(ja.printProperty(args[0]));
                        } catch (Jaspice.PropertyException e) {
                            System.out.println("Property with key "+args[0]+
                                               " not found.");
                        }
                    }
                } else
                    System.out.println("Usage: "+this.getUsage());
            }
        },

        /**********************************************************************
        **  Watches an analog node
        *********************************************************************/
/*
        new CmdCommand("watch", "watch <node_name>",
                       "Add node <node_name> to the watch list") {
            public void execute(String args[]) {
                if (args != null && args.length==1) {
                    Node node = ja.findNode(args[0]);
                    if (node != null) {
                        TempWatcher tw = new TempWatcher(args[0]);
                        node.addAnalogWatcher(tw);
                    } else
                        System.out.println("Node "+args[0]+" not found.");
                } else
                    System.out.println("Usage: "+this.getUsage());
            }
        },
  */      
        /**********************************************************************
        **  Unwatches an analog node
        *********************************************************************/
/*
        new CmdCommand("unwatch", "unwatch <node_name>",
                       "Remove a node <node_name> from the watch list") {
            public void execute(String args[]) {
                if (args != null && args.length==1) {
                    //Removes a node from the watch list
                    System.out.println("Sorry not implemented :(");
                } else
                    System.out.println("Usage: "+this.getUsage());
            }
        },
  */      
        /********************************************************************
         * Reports all nodes being traced cuurently by callback
         ********************************************************************/

        new CmdCommand("trace?", "trace? [type=V,I1,I2,I3,I4]",
                       "Displays the nodes and/or devices being traced,"+
                       " optionally provide a type to query only that type") {
            public void execute(String args[]) {
                if (args == null || args.length==1) {
                    Iterator strings = null;
                    int type = -1;
                    if (args != null) {
                        type = DefaultCallback.getType(args[0]);
                        if (type != -1) {
                            strings = callback.printElements(type);
                        } else System.out.println("Unknown type: "+args[0]);
                    } else strings = callback.printElements();
                    if (strings != null) {
                        while(strings.hasNext()) {
                            System.out.print((String)strings.next());
                            if (strings.hasNext())
                                System.out.print(", ");
                        }
                        System.out.println("");
                    }
                } else
                    System.out.println("Usage: "+this.getUsage());
            }
        },

        /********************************************************************
         * Prints out all of the nodes
         ********************************************************************/

        new CmdCommand("nodes?", "nodes?",
                       "Displays all of the nodes in the circuit") {
            public void execute(String args[]) {
                if (args == null) {
                    for(Iterator i = ja.getNodes().iterator(); i.hasNext();) {
                        Node node = (Node) i.next();
                        StringBuffer buf = new StringBuffer(" aka ");
                        for(Iterator j = ja.getAliases(node);j.hasNext();) {
                            HierName hname = (HierName) j.next();
                            if (hname.isPointer())
                                buf.insert(0,HierName.trim(hname).toString());
                            else {
                                buf.append(hname.toString());
                                if (j.hasNext())
                                    buf.append(" ");
                            }
                        }
                        System.out.println(buf.toString());
                    }
                } else
                    System.out.println("Usage: "+this.getUsage());
            }
        },
        /********************************************************************
        **  Traces an analog node
        *********************************************************************/

        new CmdCommand("trace", "trace <name> [type=V(default),I1,I2,I3,I4]",
                       "Add node or device <name> to the tracefile with"+
                       " type [type]") {
            public void execute(String args[]) {
                if (args != null && args.length>=1 && args.length <=2) {
                    if ( (args.length == 1) ||
                         (args[1].equals("V")) ) {
                        Node node = ja.findNode(args[0]);
                        if (node == null)
                            System.out.println("Node "+args[0]+
                                               " not found.");
                        else callback.trace(node,Node.VOLTAGE);
                    } else {
                        int type = DefaultCallback.getType(args[1]);
                        if (type != -1) {
                            AbstractDevice dev = ja.findDevice(args[0]);
                            if (dev == null)
                                System.out.println("Device "+args[0]+
                                                   " not found.");
                            else callback.trace(dev,type);
                        } else System.out.println("Unknown type: "+args[1]);
                    }
                } else
                    System.out.println("Usage: "+this.getUsage());
            }
        },
        
        /**********************************************************************
        **  Untraces an analog node
        *********************************************************************/

        new CmdCommand("untrace", "untrace <name>"+
                                  " [type=V(default),I1,I2,I3,I4]",
                       "Remove a node  or device with <name>"+
                       " and optionally [type] from the tracefile") {
            public void execute(String args[]) {
                if (args != null && args.length>=1 && args.length <=2) {
                    if ( (args.length == 1) ||
                         (args[1].equals("V")) ) {
                        Node node = ja.findNode(args[0]);
                        if (node == null)
                            System.out.println("Node "+args[0]+
                                               " not found.");
                        else callback.untrace(node,Node.VOLTAGE);
                    } else {
                        int type = DefaultCallback.getType(args[1]);
                        if (type != -1) {
                            AbstractDevice dev = ja.findDevice(args[0]);
                            if (dev == null)
                                System.out.println("Device "+args[0]+
                                                   " not found.");
                            else callback.untrace(dev,type);
                        } else System.out.println("Unknown type: "+args[1]);
                    }
                } else
                    System.out.println("Usage: "+this.getUsage());
            }
        },
        
        
        /**********************************************************************
        **  Traces all analog nodes
        *********************************************************************/

        new CmdCommand("traceall", "traceall",
                       "Adds all nodes to the tracefile") {
            public void execute(String args[]) {
                if (args == null) {
                        callback.addAllNodesToTrace();
                } else
                    System.out.println("Usage: "+this.getUsage());
            }
        },
        
        /**********************************************************************
        **  Untraces all analog nodes
        *********************************************************************/

        new CmdCommand("untraceall", "untraceall [nodes, devices]",
                       "Removes all nodes from the tracefile, add the keyword"+
                       " nodes to remove only nodes, add devices to remove"+
                       " only devices") {
            public void execute(String args[]) {
                if (args == null) {
                        callback.clearTraceList();
                        System.out.println("All nodes and devices removed"+
                                           " from tracelist.");
                } else if (args.length == 1) {
                    if (args[0].equals("nodes")) {
                        callback.removeAllNodesFromTrace();
                        System.out.println("All nodes removed from tracelist.");
                    } else if (args[0].equals("devices")) {
                        callback.removeAllDevicesFromTrace();
                        System.out.println("All devices removed from tracelist.");
                    }
                } else 
                    System.out.println("Usage: "+this.getUsage());
            }
        },
        
        /**********************************************************************
        **  Displays the Current Time in seconds
        *********************************************************************/

        new CmdCommand("time", "time",
                       "Returns current Jaspice time") {
            public void execute(String args[]) {
                if (args == null) {
                    System.out.println("Jaspice Time = "+ja.getTime()+" seconds");
                } else
                    System.out.println("Usage: "+this.getUsage());
            }
        },

        /**********************************************************************
        ** Synchronize Jaspice to run along-side DSim 
        *********************************************************************/
/*
        new CmdCommand("withDSim", "withDSim <dsim_unit> <time_in sec>",
                       "Run alongside DSim with <dism_unit> equal to"+
                       " <time_in_sec>") {
            public void execute(String args[]) {
                if (args != null && args.length==2) {
                   int dunits = Integer.parseInt(args[0]);
                   double tunits = Double.parseDouble(args[1]);
                   DSimSynchronizer ds = new DSimSynchronizer(dunits,tunits);
                   ds.addDevice(Jaspice.get());
                   ds.start();
                   System.out.println("Running Jaspice alongside DSim with"+
                                      " dunits= "+dunits+" which equals "+
                                      tunits+" seconds");
                } else
                    System.out.println("Usage: "+this.getUsage());
            }
        },
*/
        /**********************************************************************
        **  Runs the simulation
        *********************************************************************/
        
        new CmdCommand("run", "run [time_in_secs]",
                       "Run the analog simulation,"+
                       " optionaly providing a time to stop the run"+
                       " (which overrides timemax)") {
            public void execute(String args[]) {
                if ((args == null) || (args.length ==1)) {
                    try {
                        double newDur = 0;
                        if (args != null){
                            try {
                                newDur = Double.parseDouble(args[0]);
                            } catch(NumberFormatException e) {
                                System.out.println("Time provided: "+args[0]+
                                                   " should be a real number.");
                                return;
                            }
                        }
                        ja.run(newDur);
                    } catch (JaspiceException e) {
                        System.out.println("ERROR, JaspiceException: "+
                                           e.getMessage());
                    } catch (Jaspice.PropertyException e) {
                        System.out.println("PropertyException : "+
                                           e.getMessage());
                    }
                } else
                    System.out.println("Usage: "+this.getUsage());
            }
        },
        
        /**********************************************************************
        ** Drives an analog node with digital inout (requires DSim) 
        *********************************************************************/
/*
        new CmdCommand("drive", "drive <analog_node_name> <digital_node_name>",
                       "Drive an analognode with a digital input"+
                       " (requires DSim)") {
            public void execute(String args[]) {
                if (args != null && args.length==2) {
                    try {
                        ja.addDigitalDriver(args[0],args[1]);
                    } catch (JaspiceException e ) {
                        System.out.println(e.getMessage());
                    }
                } else
                    System.out.println("Usage: "+this.getUsage());

            }
        },
  */      
        /**********************************************************************
        * Add a voltage source
        *********************************************************************/

        new CmdCommand("vsource", "vsource name n+ n- type arg1 arg2 ... argN",
                       "Attach a voltage source device across two nodes") {
            public void execute(String args[]) {
                if (args != null && args.length>=4) {
                    String[] newargs = null;
                    if (args.length > 4) {
                        newargs = new String[args.length-4];
                        System.arraycopy(args,4,newargs,0,newargs.length);
                    }
                    try {
                        ja.addSource(args[0], args[3],args[1],args[2],newargs);
                    } catch (JaspiceException e) {
                        System.out.println(e.getMessage());
                    }
                } else
                    System.out.println("Usage: "+this.getUsage());
            }
        },
    };
/*
    private class TempWatcher implements AnalogWatcher {
     
        private byte value = -1;
        private String name;

        private double high = 1.2;
        private double low = .6;

        TempWatcher(String nodename) {
            this.name = nodename;
            DSim.get().setNodeValue("ckbar", (byte)0);
        }
        public void voltageChanged(double voltage) {
            if (voltage < low) {
                if ((value == -1) || (value == 1)) {
                    value = 0;
                    sendEvent();
                }
            } else if (voltage > high) {
                if (value <=0) {
                    value = 1;
                    sendEvent();
                }
            }
        }

        private void sendEvent() {
            System.out.println("The node "+name+" has changed to "+value);
            //byte ckbar = 1;
            //if (value == 1) ckbar = 0;
            DSim.get().setNodeValue("ckbar",value);
            System.out.println("Sending "+value+" to ckbar");
        }
    }
            
*/
            
}


