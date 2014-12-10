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
/**
 * Interface for tracing analog nodes<BR>
 * An analog trace records all traced elements' values for all timesteps,
 * giving it a very simple interface.
 *
 * @author Dan Daly
 * @version $Date$
 **/

public interface AnalogTraceInterface {
    /** Adds an element to the tracefile, given an iterator of strings **/
   void addAliasedElement(Iterator names) throws TraceFileException;
   /** Adds a new element to the list **/
   void addElement(String name) throws TraceFileException;
   /** Sends a new record to the tracefile.  The order of the floats
    * should be the same as the order of the addElements.  If the number
    * of floats is not the same as the number of elements, the tracefile
    * may become corrupted **/
   void record(double time, double[] data) throws TraceFileException;
   /** Closes the tracefile **/
   void close() throws TraceFileException;
}

