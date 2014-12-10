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

import java.util.Iterator;
import java.util.ArrayList;
import java.util.Collection;
/***************************************************************
 * Default callback that writes to a tracefile with name tracename.trace
 ***************************************************************/
public class DefaultCallback implements JaspiceCallbackInterface {
    private TraceFile tracer;
    private double timemax, poststep,timestep;
    private String tracename;
    private boolean traceon;

    /** Used to keep track of the poststep **/
    private double elapsed_time = 0;
    
    /** List of nodes to trace **/
    private TraceList tracelist = null; 
    
    /** Parses a string into the correct int (for tracing) **/
    public static int getType(String str) {
        int type = -1;
        if (str.equals("V"))
            type = Node.VOLTAGE;
        else if (str.equals("I1")) 
            type = AbstractDevice.I1;
        else if (str.equals("I2"))
            type = AbstractDevice.I2;
        else if (str.equals("I3"))
            type = AbstractDevice.I3;
        else if (str.equals("I4"))
            type = AbstractDevice.I4;
        return type;
    }

    /** Constructor, nothing is actually done until the init method **/
    public DefaultCallback() {
        timemax = 0;
        poststep = 0;
        timestep = 0;
        tracename = "a";
        boolean traceon = true;
        tracelist = null;
    }
    
    /** JaspiceCallbackInterface method.  Builds the tracefile, and adds all node and
     * device names to the tracefile .names **/
    public void init() throws JaspiceException{
        Jaspice ja = Jaspice.get();
        try {
            traceon = ja.getBooleanProperty("trace");
        } catch (Jaspice.PropertyException e) {
            System.out.println("Trace toggle not found.");
        }
        if (!traceon) {
            tracer = null;
            return; //trace disabled
        }
        try {
            timemax = ja.getDoubleProperty("timemax");
            poststep = ja.getDoubleProperty("poststep");
            timestep = ja.getDoubleProperty("timestep");
            tracename = ja.getStringProperty("tracename");
        } catch (Jaspice.PropertyException e) {
            throw new JaspiceException("In Callback: "+e.getMessage(), e);
        }
        try {
            tracer = new TraceFile(tracename);
            tracer.startNewTrace();
            for (Iterator list = getTraceList().iterator();
                    list.hasNext();) {
                TraceElement te = (TraceElement) list.next();
                //If a node then get all aliases as well
                if (te.getObject() instanceof Node) 
                    tracer.addNodeNames(
                            ja.getAliases((Node) te.getObject()));
                else
                    tracer.addNodeName(te.getName());
            }

            tracer.closeOutNodes();
        } catch( TraceFile.TraceFileException te) {
            throw new JaspiceException("TraceFileException: "+
                                       te.getMessage());
        }
    }
   
    /** Returns an iterator of strings that contains the print strings
     * of the elements in the trace list **/
    public Iterator printElements() {
        return printElements(-1);
    }

    /** Returns an iterator of strings that contains the print strings of the
     * elements of the trace list whose trace type is equal to <code>type</code>
     * **/
    public Iterator printElements(int type) {
        ArrayList strings = new ArrayList();
        if (tracelist == null) {
            strings.add("Trace List uninitialized, all nodes will be added to"+
                        " trace at runtime.");
        } else {
            for (Iterator i = tracelist.iterator();i.hasNext();) {
                TraceElement te = (TraceElement) i.next();
                if ((type == -1) || (te.getType() == type))
                    strings.add(te.printElement());
            }
        }
        return strings.iterator();
    }

    /** Gets the current trace list **/
    private Collection getTraceList() { 
        checkTrace();
        return tracelist;
    }
            
    /** Installs the default behavior before any trace functions can be used **/
    private void checkTrace() {
        if (tracelist == null) {
            tracelist = new TraceList();
            //default behavior
            addAllNodesToTrace();
        }
    }

    /** Removes all nodes in this circuit from the trace list **/
    public void removeAllNodesFromTrace() {
        if (tracelist == null) tracelist = new TraceList();
        else                    tracelist.removeAll(Node.VOLTAGE,Node.VOLTAGE);
    }

    /** Removes all devices in this circuit from the trace list **/
    public void removeAllDevicesFromTrace() {
        if (tracelist == null) tracelist = new TraceList();
        else                    tracelist.removeAll(AbstractDevice.I1,
                                                    AbstractDevice.I4);
    }

    /** Clears entire trace list **/
    public void clearTraceList() {
        if (tracelist == null) tracelist = new TraceList();
        else                   tracelist.clear();
    }
    
    /** Adds all of the nodes in this circuit to the node list **/
    public void addAllNodesToTrace() {
        if (tracelist == null) tracelist = new TraceList();
        else                    tracelist.clear();
        System.out.println("adding all nodes to trace");
        Jaspice ja = Jaspice.get();
        Collection c = ja.getNodes();
        if (c != null)
            tracelist.addAll(c, Node.VOLTAGE);
    }

    /** Trace a single node or object (depending on the type).**/
    public void trace(Object tracer, int type) {
        if (tracelist == null) tracelist = new TraceList();
        //Does not check for repeated nodes
        tracelist.add(new TraceElement(type,tracer));
    }
    
    /** Untrace a single trace type from node or device <code>untracer</code>. **/
    public void untrace(Object untracer,int type) {
        if (tracelist== null) tracelist = new TraceList();
        else tracelist.remove(untracer,type);
    }

    /** Removes all elements that trace the node or device untracer **/
    public void untrace(Object untracer) {
        if (tracelist== null) tracelist = new TraceList();
        else tracelist.removeAll(untracer);
    }

    /** Get all of the nodes being traced in the order they were printed
     * to the .names file **/
    private float[] traceNodes() {
        Collection c = getTraceList();
        float[] data = new float[c.size()];//all traced nodes, not inc time
        int index = 0;
        for(Iterator i = c.iterator();i.hasNext();) {
            TraceElement te = (TraceElement) i.next();
            //Node node = 
            //    (Node) i.next();
            data[index++] = (float) te.getData();//node.getVoltage();
        }
        return data;
    }

    /** JaspiceCallbackInterface method.  Outputs all node voltages and device
     * currents chosen in the init step in float form to the tracefile **/
    public void update() throws JaspiceException{
        if ((tracer==null) || (!traceon)) return;
        Jaspice ja = Jaspice.get();
        //JaspiceCircuit circuit = ja.getCircuit();
        /** Loop to check if poststep has elapased, under the assumption
         * that poststep is larger than timestep**/
        if (elapsed_time <= timestep*1e-6 /*zero*/) {
            try {
                tracer.writeFloat((float)ja.getTime());
                tracer.writeRecord( traceNodes());
             } catch (TraceFile.TraceFileException e) {
                throw new JaspiceException("TraceFileException: "+
                                           e.getMessage());
            }
            elapsed_time = poststep;
        }
        elapsed_time -= timestep;
    }
    
    /** JaspiceCallbackInterface method, closes the tracefile **/
    public void finish() throws JaspiceException {
        try {
            if (tracer != null) tracer.close();
        } catch (TraceFile.TraceFileException e) {
            throw new JaspiceException("TraceFileException: "+
                                       e.getMessage());
        }
    }

    /** Swanky arraylist that handles trace elements **/
    private class TraceList extends ArrayList {

        public void addAll(Collection src, int type) {
            for (Iterator i = src.iterator();i.hasNext();) {
                TraceElement te = new TraceElement(type,i.next());
                add(te);
            }
        }

        /** Remove the element o that has the trace type type **/
        public boolean remove(Object o,int type) {
            for( Iterator i = this.iterator();i.hasNext();) {
                TraceElement te = (TraceElement) i.next();
                if (te.getObject().equals(o) &&
                    (te.getType() == type)) {
                    tracelist.remove(te);
                    return true;
                }
            }
            return false;
        }
        
        /** Removes all elements whose object is the same as o.**/
        public boolean removeAll(Object o) {
            boolean found = false;
            for( Iterator i = this.iterator();i.hasNext();) {
                TraceElement te = (TraceElement) i.next();
                if (te.getObject().equals(o)) {
                    tracelist.remove(te);
                    found = true;
                }
            }
            return found;
        }
        /** Removes all elements whose types fall between a certain range **/
        public boolean removeAll(int type1, int type2) {
            boolean found = false;
            for( Iterator i = this.iterator();i.hasNext();) {
                TraceElement te = (TraceElement) i.next();
                int type = te.getType();
                if ((type >= type1) && (type <= type2)) {
                    i.remove();
                    found = true;
                }
            }
            return found;
        }
    }    

}
