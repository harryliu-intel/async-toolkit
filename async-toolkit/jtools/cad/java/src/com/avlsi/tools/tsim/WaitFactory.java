/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import com.avlsi.tools.dsim.Node;

/**
 * <p> A factory class for constructing subclasses of <code>Wait</code> at
 * runtime. </p>
 *
 * @author Patrick Pelletier
 * @version $Revision$ $Date$
 *
 * @review kiniry - Under review beginning 23 July 2002.
 **/

public class WaitFactory {
    /**
     * This class should not be instantiated.
     **/
    private WaitFactory() { }

    private static Class theClass = AccurateWait.class;

    /**
     * Returns the (ChannelInput[], ChannelOutput[], Node[], Node[])
     * constructor for the given class.
     */
    private static Constructor chooseNodeConstructor(Class clazz) {
	        Constructor[] cons = clazz.getConstructors();
        for (int i = 0; i < cons.length; i++) {
            Class[] params = cons[i].getParameterTypes();
            if (params.length == 4 &&
                params[0].getComponentType() == ChannelInput.class &&
                params[1].getComponentType() == ChannelOutput.class &&
                params[2].getComponentType() == Node.class &&
                params[3].getComponentType() == Node.class) {
                return cons[i];
            }
        }
        throw new RuntimeException(clazz + " does not have a (ChannelInput[]" +
                                   ", ChannelOutput[], Node[], Node[])" +
                                   " constructor");
    }

    /**
     * Returns the (ChannelInput[], ChannelOutput[])
     * constructor for the given class.
     */
    private static Constructor chooseChannelConstructor(Class clazz) {
	        Constructor[] cons = clazz.getConstructors();
        for (int i = 0; i < cons.length; i++) {
            Class[] params = cons[i].getParameterTypes();
            if (params.length == 2 &&
                params[0].getComponentType() == ChannelInput.class &&
                params[1].getComponentType() == ChannelOutput.class) {
                return cons[i];
            }
        }
        throw new RuntimeException(clazz + " does not have a (ChannelInput[]" +
                                   ", ChannelOutput[]) constructor");
    }

    /**
     * Set what subclass of Wait should be created in the future for
     * this program.
     * @param   whatKind   Class object for a subclass of Wait
     */
    public static void setWait(Class whatKind) {
        theClass = whatKind;
    }

    /**
     * Create a new Wait object.
     */
    public static Wait newWait(ChannelInput [] in, ChannelOutput [] out) {
	Object[] args = new Object[] {in, out};
	try {
	    return (Wait) chooseChannelConstructor(theClass).newInstance(args);
	        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        } catch (InstantiationException e) {
            throw new RuntimeException(e);
        } catch (InvocationTargetException e) {
            throw new RuntimeException(e);
        } 
    }

    /**
     * Create a new Wait object.
     */
    public static Wait newWait(ChannelInput [] in, ChannelOutput [] out,
                               Node [] up, Node [] down) {
        Object[] args = new Object[] {in, out, up, down};
        try {
            return (Wait) chooseNodeConstructor(theClass).newInstance(args);
        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        } catch (InstantiationException e) {
            throw new RuntimeException(e);
        } catch (InvocationTargetException e) {
            throw new RuntimeException(e);
        } 
    }

} // end of class WaitFactory

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
