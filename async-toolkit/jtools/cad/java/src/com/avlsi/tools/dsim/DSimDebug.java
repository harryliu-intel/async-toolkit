/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.dsim;

/**
 * <p> An interface for static debugging for the dsim package. </p>
 *
 * <p> Set this variable to "true" to compile in all debugging
 * code. </p>
 *
 * <p> Once we determine which other primary packages dsim depends
 * upon, we will augment this definition appropriately.  E.g., if dsim
 * depends upon the package foobar, then we will:
 * <enum>
 * <li> add <code>import ....foobar.FoobarDebug</code> to this class, and</li>
 * <li> rewrite
 * <pre>boolean DEBUG = false</pre>
 * as
 * <pre>boolean DEBUG = false || FoobarDebug.DEBUG</pre>
 * </li>
 * </p>
 *
 * @author <a href="mailto:kiniry@fulcrummicro.com">Joseph Kiniry</a>
 * @version $Revision$ $Date$
 **/

public interface DSimDebug {

    boolean DEBUG = false;

} // end of interface DSimDebug

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
