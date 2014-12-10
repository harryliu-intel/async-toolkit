/*
 * Copyright 2000 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.tsim;

import com.avlsi.tools.dsim.DigitalScheduler;
import com.avlsi.tools.sigscan.Sigscan;
import com.avlsi.tools.sigscan.LoggingObject;
import com.avlsi.tools.sigscan.DebugOpts;

/**
 * <p> Class for single threaded objects that listen on other DSim or TSim
 * objects, such as nodes, <code>SharedBuse</code>s, etc.  Similar to a
 * <code>ClockedDevice</code>, except, of course, it has no thread of its own.
 * Note that this class doesn't do much until you implement some
 * <code>Watcher</code> interfaces.  Use the <code>init()</code> method to set
 * up all of your listeners, and to start your state machine running.
 *
 * @author Dan Daly
 * @version $Revision$ $Date$
 *
 * @review kiniry - Under review beginning 29 July 2002.
 **/

public class StateMachine extends LoggingObject {

    /**
     * Constructor that sets up the <code>LoggingObject</code>, and calls
     * <code>init()</code>.  Be sure that your name is fully qualified with
     * scope, or at least unique!
     *
     * @param fullname the fully scoped name of this object.
     * @param sigscan the database to place this in (can be null).
     * @param opts the database options.
     **/
    public StateMachine(String fullname,
                        Sigscan sigscan, DebugOpts opts) {
        super(fullname, sigscan, opts);
        init();
    }

    /**
     * Constructor that sets up the <code>LoggingObject</code>, and calls
     * <code>init()</code>.
     *
     * @param scopename the scope to place this object in.
     * @param name the name of this object.
     * @param sigscan the database to place this in (can be null).
     * @param opts the database options.
     **/
    public StateMachine(String scopename, String name,
                        Sigscan sigscan, DebugOpts opts) {
        super(scopename, name, sigscan, opts);
        init();
    }

    /**
     * The <code>init()</code> function, called from the constructor.  Currently
     * does nothing.
     **/
    protected void init() {}

    /**
     * @return the current time of the system <code>DigitalScheduler</code>.
     * @see DigitalScheduler#getTime
     **/
    public long getTime() { return DigitalScheduler.get().getTime(); }

} // end of class StateMachine

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
