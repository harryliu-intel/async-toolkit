/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.cmdline;

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Vector;
import java.util.Enumeration;

import com.avlsi.tools.aspice.NativeAspiceUtil;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.io.FileSearchPath;
import com.avlsi.io.SearchPath;
import com.avlsi.util.container.StringContainerIterator;
import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.classloader.ConfigurableClassLoader;
import com.avlsi.cast.CastFileParser;

/** Interface between the command line interpreter and DSim simulator */
public class ADsimModule extends CmdLine {
    /** The underlying simulation object that does most of the work. **/
    NativeAspiceUtil adsim = null;
    
    /** Which version of cast are we using? **/
    String castVersion = null;
    
    /** Print out exception stack traces? **/
    boolean debug = false;
    
    /** Interrupt handler. Catches Ctrl-C and tells the simulator to stop. **/
    Signal.Handler intHandler = new Signal.Handler() {
            public void execute() {
                if (adsim!=null) { adsim.interrupt(); }
            }
        };
    
    /** Constructor */
    public ADsimModule() {
        super(false);
        addIntHandler(intHandler);
        addCommands(commands);
        adsim = new NativeAspiceUtil();
    }
    
    public String getName() { return "ADsim"; }

    public String getHelpMsg() { return "\tADsim/\t\t\tdigital simulation module"; }
    
    public String getExtendedHelpMsg() {
        return "\nModule ADsim\n\n"+
            "\tADsim simulates a mixed analog/digital circuit \n"+
            "\tspecified by CAST and/or CDL.\n\n";
    }

    /** All of the commands that make the simulator go. */
    CmdCommand commands[] = {
        new CmdCommand("model", 
                       "model file temp",
                       "Loads BSIM3 model from file at given temperature.",
                       "Loads BSIM3 model from file at given temperature.\n"+
                       "Model applies to all subsequent spice instantiations.") {
                public void execute(String args[]) {
                    if (args!=null && args.length==2) {
                        double temp = Double.parseDouble(args[1]);
                        adsim.selectModel(args[0],temp);
                    }
                }
            },
        new CmdCommand("spice", 
                       "spice file cell",
                       "Instantiates cell from file in spice/cdl syntax",
                       "Instantiate the analog circuit described by SUBCKT cell in\n"+
                       "a cdl/spice file.  Should load a BSIM3 model first.") {
                public void execute(String args[]) {
                    if (args!=null && args.length==2) {
                        adsim.instantiateSpice(args[0],args[1]);
                    }
                }
            },
        new CmdCommand("option", "option [type [value]]",
                       "Assigns or reports an aspice option.",
                       "Use option to set or get the value of a simulation option\n" +
                       "for analog simulation.") {
                public void execute(String args[]) {
                    if (args!=null && args.length==2) {
                        double value = Double.parseDouble(args[1]);
                        adsim.setOption(args[0],value);
                    } else if (args!=null && args.length==1) {
                        double value = adsim.getOption(args[0]);
                        System.out.println(args[0] + "=" + value);
                    } else {
                        adsim.printOptions();
                    }
                }
            },
        new CmdCommand("force", "force node voltage",
                       "Forces an analog node to a given voltage",
                       "Force an analog node to given voltage, used to create power\n"+
                       "supplies for analog simulation.") {
                public void execute(String args[]) {
                    if (args!=null && args.length==2) {
                        HierName name = HierName.makeHierName(args[0]);
                        double voltage = Double.parseDouble(args[1]);
                        adsim.forceAnalogNode(name,voltage);
                    }
                }
            },
        new CmdCommand("simulate", "simulate time", 
                       "Runs the simulator until for time seconds",
                       "The simulate command spices the current circuit.") {
                public void execute(String args[]) {
                    double time = Double.parseDouble(args[0]);
                    adsim.simulate(time);
                }
            },
        new CmdCommand("clear", "clear", "Clear circuit from simulator",
                       "Clears all nodes, rules, and devices from ADsim.\n"+
                       "Allows cells to be re-instantiated without needing\n" +
                       "to parse CAST files a second time.") {
                public void execute(String args[]) {
                    adsim.clear();
                }
            },
    };
}
