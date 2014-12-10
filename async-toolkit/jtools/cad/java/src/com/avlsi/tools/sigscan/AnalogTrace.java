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

package com.avlsi.tools.sigscan;

import com.avlsi.tools.aspice.AnalogTraceInterface;
import com.avlsi.file.common.HierName;
import com.avlsi.tools.aspice.TraceFileException;
import java.util.Iterator;
/**
 * Class for Tracing Analog Nodes using an SST database (signalscan)<BR>
 * Note that this class uses native methods
 * @author Dan Daly
 * @version $Date$
 **/

public class AnalogTrace extends Sigscan implements AnalogTraceInterface {

    //
    //Time parameters
    //

    private final int timeScale;
    private final double timeUnit;
    
    //
    //Holds all of the variables created in an array
    //
    private final GrowingArray vars;
    
    /**
     * Constructor.
     **/
    public AnalogTrace(String tracename,
                       boolean useAlreadyOpenDatabase, int timeScale)
            throws SigscanException{
        super(tracename, useAlreadyOpenDatabase, timeScale);
        vars = new GrowingArray();
        this.timeScale = timeScale;
        this.timeUnit = Math.pow(10, timeScale);
        
    }
    
    /**
     * Constructor.
     **/
    public AnalogTrace(String tracename)
            throws SigscanException {
        //SDI defaults
        this(tracename, true, -9);
    }
    
    /** Set the current time to <code>time</code> in seconds.  Database
     * entries must be entered in chronological order **/
    public void setTime(double time) { 
        try {
            super.setTime(Math.round((time / timeUnit)) );
        } catch (SigscanException e) {
            System.err.println("AnalogTrace: "+e.getMessage());
        }
    }

    //
    //AnalogTraceInterface Methods
    //
    
    public void addAliasedElement(Iterator names) throws TraceFileException {
        if ((names == null) || (!names.hasNext()))
            throw new TraceFileException(
                    "Trace Element error, alias list error");
        Object o = names.next();
        String str;
        if (o instanceof String) str = (String) o;
        else if (o instanceof HierName) str = ((HierName) o).toString();
        else throw new TraceFileException("Unknown alias type");
        vars.add( newDoubleVariable("top", str));
    }

    public void addElement(String scope, String name) {
        vars.add( newDoubleVariable(scope, name) );
    }    

    public void addElement(String name) throws TraceFileException {
        addElement("top", name);
    }

/*    public void close() throws TraceFileException {
        super.close();
    }
  */      
    public void record(double time, double[] data)
            throws TraceFileException {
        if (vars.size() != data.length)
            throw new TraceFileException("Error while recording, too many data"+
                                         " points.");
        setTime(time);
        long[] varptrs = vars.getInts();
        for (int loop=0;loop<data.length;loop++) {
            doubleChange(varptrs[loop], data[loop]);
        }
    }
    
}

